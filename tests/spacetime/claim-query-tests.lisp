;;;; The inverse query (design §8).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test claims-touching-finds-a-subject-across-both-arities
  (with-claim-graph (g)
    (with-transaction ()
      (make-u :subject "alpha")
      (make-b :subject "alpha" :object "beta"))
    (is (= 2 (length (claims-touching g 'ct-claim :ns "alpha"))))))

(test claims-touching-finds-an-object
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha" :object "beta"))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "beta"))))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "beta"
                                      :role :object))))
    (is (= 0 (length (claims-touching g 'ct-claim :ns "beta"
                                      :role :subject))))))

(test claims-touching-does-not-cross-namespaces
  "The namespace is part of the key, so the same string in another namespace
is a different endpoint."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha"))
    (is (= 0 (length (claims-touching g 'ct-claim :other "alpha"))))))

(test claims-touching-returns-each-claim-once
  "A claim naming the same endpoint as BOTH subject and object must not be
returned twice by the :EITHER union.  The node cache would hand back one EQ
instance for both lookups, which is deduplicated by EQL alone and would let
a wrong (plain REMOVE-DUPLICATES) implementation pass; disabling it forces
two distinct instances for the same node, so the :KEY/:TEST choice is what
is actually under test."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "self" :object "self"))
    (let ((graph-db::*cache-enabled* nil))
      (is (= 1 (length (claims-touching g 'ct-claim :ns "self")))))))

(test claims-touching-signals-on-an-unregistered-parent
  (with-claim-graph (g)
    (signals unknown-claim-family
      (claims-touching g 'no-such-claim :ns "x"))))

(test claims-touching-signals-on-a-bad-role
  "An out-of-range ROLE must signal, not silently return NIL -- NIL is also
the correct answer for \"nothing touches this endpoint\"."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha"))
    (signals error
      (claims-touching g 'ct-claim :ns "alpha" :role :subjet))))

(test a-claim-carries-a-temporal-extent-across-a-reopen
  "Design §7: the slot holds the sexp, the accessor decodes.  The reopen is
the point -- an in-memory round trip would not exercise serialization."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (id nil))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (let ((c (make-u)))
                   (setf (claim-extent c)
                         (make-granule-instant (ts 2026 3 15) :month
                                               :standing :observed))
                   (setq id (id c)))))
          (close-graph g)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let* ((graph-db:*graph* g2)
                    (e (claim-extent (lookup-vertex id))))
               (is (eq :instant (extent-kind e)))
               (is (eq :month (extent-precision e)))
               (is (eq :observed (extent-standing e)))
               (is (eq (extent-start e) (extent-end e))
                   "the instant coupling survives storage"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

(test a-claim-without-an-extent-reads-as-nil-not-as-an-error
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (is (null (claim-extent (make-u)))))))
