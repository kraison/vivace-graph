;;;; The claim class hierarchy and its macro (GH #131, design §3-§5).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *claim-graph-name* :graph-db-claim-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *claim-graph-name* graph-db::*schema-node-metadata*) nil))

(def-claim-classes ct-claim :graph-db-claim-test
  :extra-slots ((weight :initarg :weight :accessor ct-weight
                        :initform nil)))

(defmacro with-claim-graph ((g) &body body)
  "A fresh on-disk graph named *CLAIM-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *claim-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(test the-macro-defines-a-three-class-hierarchy
  (is-true (find-class 'ct-claim nil))
  (is-true (find-class 'ct-claim-unary nil))
  (is-true (find-class 'ct-claim-binary nil))
  (is-true (subtypep 'ct-claim-unary 'ct-claim))
  (is-true (subtypep 'ct-claim-binary 'ct-claim))
  (is-false (subtypep 'ct-claim-binary 'ct-claim-unary)))

(test the-parent-gets-no-constructor
  "Design §3.3: non-instantiability is signalled by not generating a
constructor.  MAKE-INSTANCE still works; nothing invites it."
  (is-true (fboundp 'make-ct-claim-unary))
  (is-true (fboundp 'make-ct-claim-binary))
  (is-false (fboundp 'make-ct-claim)))

(test object-slots-exist-only-on-the-binary-class
  "This IS the arity-as-a-type property (design §3.1): a unary claim cannot
carry an object because the slot does not exist.

Uses CL's SLOT-EXISTS-P on instances rather than a MOP call on classes:
graph-db :USEs SB-MOP on SBCL and CLOSER-MOP only on CCL/LispWorks, so
CLOSER-MOP is not loaded here and this test package sees neither."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((u (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                                    :relation :r :producer :p
                                    :standing :inferred))
            (b (make-ct-claim-binary :subject-namespace :ns :subject-key "s"
                                     :relation :r :object-namespace :ns
                                     :object-key "o" :producer :p
                                     :standing :inferred)))
        (is-true (slot-exists-p b 'graph-db.spacetime::object-key))
        (is-false (slot-exists-p u 'graph-db.spacetime::object-key))))))

(test extra-slots-land-on-the-parent-so-both-arities-inherit-them
  (with-claim-graph (g)
    (declare (ignorable g))
    (let (u b)
      (with-transaction ()
        (setq u (make-ct-claim-unary :subject-namespace :ns
                                     :subject-key "s1" :relation :r
                                     :producer :rule-a :standing :inferred
                                     :weight 1.5d0))
        (setq b (make-ct-claim-binary :subject-namespace :ns
                                      :subject-key "s1" :relation :r
                                      :object-namespace :ns :object-key "o1"
                                      :producer :rule-a :standing :inferred
                                      :weight 2.5d0)))
      (is (= 1.5d0 (ct-weight u)))
      (is (= 2.5d0 (ct-weight b))))))

(test the-registry-maps-a-parent-to-its-arity-subclasses
  (let ((f (claim-family 'ct-claim)))
    (is (eq 'ct-claim (claim-family-parent f)))
    (is (eq 'ct-claim-unary (claim-family-unary f)))
    (is (eq 'ct-claim-binary (claim-family-binary f)))
    (signals unknown-claim-family (claim-family 'no-such-claim))))

(test standing-is-validated-at-construction
  "A claim cannot be built with a standing outside the vocabulary."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals invalid-standing
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                             :relation :r :producer :p :standing :probably)))))

(test a-graph-holding-claims-closes-and-reopens-cleanly
  "Regression test for GH #131: an earlier STANDING check hooked
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, which also fires while an existing node
is deserialized (its DATA alist not populated yet) -- CLOSE-GRAPH's snapshot
silently failed for any graph holding claim data.  The wrapped-constructor
approach must not reintroduce that (design §5)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) uid bid)
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (setq uid (id (make-ct-claim-unary
                           :subject-namespace :ns :subject-key "s"
                           :relation :r :producer :p :standing :observed)))
            (setq bid (id (make-ct-claim-binary
                           :subject-namespace :ns :subject-key "s"
                           :relation :r :object-namespace :ns
                           :object-key "o" :producer :p
                           :standing :observed))))
          (close-graph g)))              ; :snapshot-p t (default)
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (let ((u (lookup-ct-claim-unary uid))
                     (b (lookup-ct-claim-binary bid)))
                 (is-true u)
                 (is-true b)
                 (is (eq :observed (claim-standing u)))
                 (is (eq :observed (claim-standing b)))))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))

(test a-claim-carries-registration-outputs-with-defaults
  "On the SHARED slots, not a tenant's :EXTRA-SLOTS: unit 3's traversal
weights by fraction without knowing which tenant wrote the claim, so it
must read one accessor (design §2, cl-llm#13)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((c (make-ct-claim-binary :subject-namespace "s" :subject-key "k"
                                   :object-namespace "o" :object-key "ok"
                                   :relation "r" :producer "p"
                                   :standing :asserted)))
      (is (null (claim-precision-m c)))
      (is (= 1.0d0 (claim-fraction c))))))

(test registration-outputs-survive-a-round-trip
  (with-claim-graph (g)
    (let ((key "rt-1"))
      (make-ct-claim-binary :subject-namespace "s" :subject-key key
                            :object-namespace "o" :object-key "ok"
                            :relation "r" :producer "p"
                            :standing :asserted
                            :precision-m 12.5d0 :fraction 0.25d0)
      (let ((c (first (graph-db:index-lookup
                        g 'ct-claim
                        '(graph-db.spacetime::subject-namespace
                          graph-db.spacetime::subject-key)
                        (list "s" key)))))
        (is (= 12.5d0 (claim-precision-m c)))
        (is (= 0.25d0 (claim-fraction c)))))))
