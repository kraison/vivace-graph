;;;; CALL-WITH-READ-SNAPSHOT leak regression (GH #181, #211).
;;;;
;;;; Before the fix, CREATE-TRANSACTION and PIN-READ-EPOCH ran as two
;;;; separate LET bindings BEFORE the UNWIND-PROTECT that releases them.
;;;; A signal from either one leaked whatever the prior binding had
;;;; already acquired: the tx stayed registered forever, pinning the
;;;; reaper's floor (GH #181's general shape; GH #211's concrete
;;;; trigger is a %QUIESCE-TRANSACTION-MANAGER flip landing between the
;;;; two calls).
(in-package #:graph-db/test)

(def-suite read-snapshot-leak-suite
  :description "CALL-WITH-READ-SNAPSHOT leaks nothing when an acquisition
signals (GH #181, #211)."
  :in graph-db-suite)

(in-suite read-snapshot-leak-suite)

(defmacro with-failing-function ((name error-call) &body body)
  "Run BODY with the function NAME replaced by one that runs ERROR-CALL
\(a form that signals, e.g. `(error 'some-condition ...)`) exactly once,
then falls through to the original definition, and finally restores it
(GH #181, #211) -- adapted from GRAPH-TESTS.LISP's WITH-FAILING-SNAPSHOT."
  (let ((orig (gensym "ORIG")) (fired (gensym "FIRED")))
    `(let ((,orig (fdefinition ',name))
           (,fired nil))
       (unwind-protect
            (progn
              (setf (fdefinition ',name)
                    (lambda (&rest args)
                      (if ,fired
                          (apply ,orig args)
                          (progn (setf ,fired t) ,error-call))))
              ,@body)
         (setf (fdefinition ',name) ,orig)))))

;; A dedicated pair of graph names for the cross-graph composition test
;; below -- distinct from every other suite's names so it never collides
;; on GH #169's one-store-id-per-name-per-image rule.
(def-vertex rsl-node-a () ((label :type string)) :rsl-graph-a)
(def-vertex rsl-node-b () ((label :type string)) :rsl-graph-b)

(defmacro with-two-graphs ((ga gb) &body body)
  "Bind GA/GB to freshly created graphs :RSL-GRAPH-A / :RSL-GRAPH-B, each in
its own scratch directory; close and clean up both regardless of how BODY
exits.  Mirrors MULTI-GRAPH-TESTS.LISP's WITH-THREE-GRAPHS."
  (let ((dir-a (gensym "DIR-A")) (dir-b (gensym "DIR-B")))
    `(with-temp-directory (,dir-a)
       (with-temp-directory (,dir-b)
         (let ((,ga (make-graph :rsl-graph-a (namestring ,dir-a)
                                :buffer-pool-size 1000))
               (,gb (make-graph :rsl-graph-b (namestring ,dir-b)
                                :buffer-pool-size 1000)))
           (unwind-protect
                (progn ,@body)
             (ignore-errors (close-graph ,ga :snapshot-p nil))
             (ignore-errors (close-graph ,gb :snapshot-p nil))
             (collect-garbage)))))))

(defun tx-table-count (tm)
  (hash-table-count (graph-db::transactions tm)))

(defun pin-table-count (tm)
  (hash-table-count (graph-db::read-pins tm)))

(test pin-read-epoch-signal-does-not-leak-the-tx-entry
  "GH #211's exact race: PIN-READ-EPOCH signals STORE-NOT-ACCEPTING-ERROR
after CREATE-TRANSACTION already registered the tx.  The signal must
propagate, but the manager's transactions table must have no leftover
entry -- the pre-fix failure mode (MINIMUM-START-TRANSACTION-ID wedged
forever) must be impossible."
  (with-test-graph (g)
    (let* ((tm (graph-db::transaction-manager g))
           (before (tx-table-count tm)))
      (with-failing-function
          (graph-db::pin-read-epoch
           (error 'graph-db::store-not-accepting-error
                  :name (graph-db:graph-name g) :reason :detaching))
        (signals graph-db::store-not-accepting-error
          (graph-db:with-read-snapshot (g) (error "thunk must not run"))))
      (is (= before (tx-table-count tm))
          "no leftover transactions-table entry after the leak window")
      (is (null (graph-db::minimum-start-transaction-id tm))
          "the reaper's floor is not wedged by a leaked entry")
      (is (= 0 (pin-table-count tm)) "no pin left held either"))))

(test create-transaction-signal-registers-and-pins-nothing
  "GH #181's other arm: CREATE-TRANSACTION itself signals, before
PIN-READ-EPOCH ever runs.  Nothing is registered and no pin is held."
  (with-test-graph (g)
    (let* ((tm (graph-db::transaction-manager g))
           (tx-before (tx-table-count tm))
           (pin-before (pin-table-count tm)))
      (with-failing-function
          (graph-db::create-transaction
           (error 'graph-db::store-not-accepting-error
                  :name (graph-db:graph-name g) :reason :detaching))
        (signals graph-db::store-not-accepting-error
          (graph-db:with-read-snapshot (g) (error "thunk must not run"))))
      (is (= tx-before (tx-table-count tm)) "no tx registered")
      (is (= pin-before (pin-table-count tm)) "no pin acquired"))))

(test remove-transaction-signal-still-releases-the-pin
  "Cleanup robustness: REMOVE-TRANSACTION signals once during teardown.
UNPIN-READ-EPOCH runs from a nested UNWIND-PROTECT around the pin
acquisition (GH #181) so it does not depend on REMOVE-TRANSACTION's
outcome at all -- the pin is released, and the cleanup signal from
REMOVE-TRANSACTION is what ultimately propagates."
  (with-test-graph (g)
    (let* ((tm (graph-db::transaction-manager g))
           (pin-before (pin-table-count tm)))
      (with-failing-function
          (graph-db::remove-transaction
           (error "simulated remove-transaction failure"))
        (signals error
          (graph-db:with-read-snapshot (g) nil)))
      (is (= pin-before (pin-table-count tm))
          "the read pin is still released despite the cleanup signal"))))

(test read-snapshot-happy-path-still-composes-across-graphs
  "Two graphs' snapshots still compose (GH #53), and the floor clears on
each graph's manager once WITH-READ-SNAPSHOT exits -- the restructure
(GH #181, #211) changes only the failure paths."
  (with-two-graphs (g1 g2)
    (let ((tm1 (graph-db::transaction-manager g1))
          (tm2 (graph-db::transaction-manager g2))
          id1 id2)
      (let ((*graph* g1))
        (with-transaction () (setq id1 (id (make-rsl-node-a :label "A")))))
      (let ((*graph* g2))
        (with-transaction () (setq id2 (id (make-rsl-node-b :label "B")))))
      (graph-db:with-read-snapshot (g1)
        (graph-db:with-read-snapshot (g2)
          (is (not (null (graph-db::minimum-start-transaction-id tm1))))
          (is (not (null (graph-db::minimum-start-transaction-id tm2))))
          (let ((*graph* g1))
            (is (equalp id1 (id (lookup-vertex id1)))))
          (let ((*graph* g2))
            (is (equalp id2 (id (lookup-vertex id2)))))))
      (is (null (graph-db::minimum-start-transaction-id tm1))
          "g1's floor clears after the nested snapshots exit")
      (is (null (graph-db::minimum-start-transaction-id tm2))
          "g2's floor clears after the nested snapshots exit"))))
