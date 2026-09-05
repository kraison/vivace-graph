;;;; The epoch axis for claim reads (GH #347): a version's commit
;;;; epoch, and :AS-OF-EPOCH on the two readers.  Facts in
;;;; docs/superpowers/notes/2026-09-05-epoch-axis-engine-api-facts.md.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *ep-a-name* :graph-db-ep-a)
(defparameter *ep-b-name* :graph-db-ep-b)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ep-a-name* graph-db::*schema-node-metadata*) nil)
  (setf (gethash *ep-b-name* graph-db::*schema-node-metadata*) nil))

;; Two stores need two families: DEF-CLAIM-CLASSES binds node types to
;; one graph name and class names are global (#347 recon F).
(def-claim-classes ea-claim :graph-db-ep-a)
(def-claim-classes eb-claim :graph-db-ep-b)
(def-claim-classes ek-claim :graph-db-ep-a :keep-revisions 1)

(defmacro with-clocked-stores ((a b) &body body)
  "Two fresh stores A and B on ONE system clock, all in scratch dirs.
The attach is asserted inside the fixture: a store that silently failed
to attach would let every epoch test pass for the wrong reason."
  (let ((cdir (gensym "CDIR")) (da (gensym "DA")) (db (gensym "DB"))
        (clock (gensym "CLOCK")))
    `(with-temp-directory (,cdir)
       (with-temp-directory (,da)
         (with-temp-directory (,db)
           (let ((,clock (graph-db:open-system-clock (namestring ,cdir))))
             (unwind-protect
                  (let ((,a (make-graph *ep-a-name* (namestring ,da)
                                        :buffer-pool-size 1000
                                        :system-clock ,clock))
                        (,b (make-graph *ep-b-name* (namestring ,db)
                                        :buffer-pool-size 1000
                                        :system-clock ,clock)))
                    (unwind-protect
                         (progn
                           (is (eq (graph-db:graph-system-clock ,a)
                                   (graph-db:graph-system-clock ,b))
                               "fixture: both stores on one clock")
                           ,@body)
                      (ignore-errors (close-graph ,a))
                      (ignore-errors (close-graph ,b))
                      (collect-garbage)))
               (graph-db:close-system-clock ,clock))))))))

(defun %tx (graph thunk)
  "Run THUNK in a transaction on GRAPH; return the committed epoch --
the transaction's id, readable after the commit (#347 recon E2)."
  (graph-db::transaction-id
   (with-transaction (:graph graph)
     (funcall thunk)
     graph-db:*transaction*)))

(defun %unary (graph maker key &key extent)
  "Make one unary claim with MAKER in GRAPH on (:region KEY)."
  (apply maker :graph graph
               :subject-namespace :region :subject-key key
               :relation "verified" :producer "audit" :standing :observed
               (and extent (list :extent extent))))

(defun %one (graph family key)
  "The live version of the one claim of FAMILY on (:region KEY)."
  (first (claims-touching graph family :region key :role :subject)))

(test epochs-form-one-sequence-across-two-stores
  "#347 part 1 (recon C1, E2): under one clock the commits A, B, A get
distinct increasing ids, and each claim's CLAIM-COMMIT-EPOCH read back
from the store is its own transaction's id."
  (with-clocked-stores (a b)
    (let ((e1 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1"))))
          (e2 (%tx b (lambda () (%unary b #'make-eb-claim-unary "r1"))))
          (e3 (%tx a (lambda () (%unary a #'make-ea-claim-unary "r2")))))
      (is (< e1 e2 e3))
      (is (= e1 (claim-commit-epoch (%one a 'ea-claim "r1"))))
      (is (= e2 (claim-commit-epoch (%one b 'eb-claim "r1"))))
      (is (= e3 (claim-commit-epoch (%one a 'ea-claim "r2")))))))

(test claim-commit-epoch-is-nil-for-a-reaped-claim
  "Part 1's reader must survive the REAPED-CLAIM structs :AS-OF mixes
into a result list."
  (with-clocked-stores (a b)
    (%tx a (lambda () (%unary a #'make-ea-claim-unary "r1")))
    (let ((c (%one a 'ea-claim "r1")))
      (is (integerp (claim-commit-epoch c)) "control: a live version")
      (is (null (claim-commit-epoch
                 (graph-db.spacetime::%make-reaped-claim (id c))))))))

(test claim-commit-epoch-reads-on-a-clockless-store
  "A store with no system clock still stamps versions from its own
counter, so part 1's reader answers an integer there; only the
:AS-OF-EPOCH reader refuses (recon E9).  *SYSTEM-CLOCK* is bound
explicitly so the premise does not depend on run order."
  (let ((graph-db:*system-clock* nil))
    (with-temp-directory (dir)
      (let ((g (make-graph *ep-a-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (unwind-protect
             (progn
               (is (null (graph-db:graph-system-clock g))
                   "control: no clock")
               (%tx g (lambda () (%unary g #'make-ea-claim-unary "r1")))
               (is (integerp (claim-commit-epoch (%one g 'ea-claim "r1")))))
          (ignore-errors (close-graph g))
          (collect-garbage))))))
