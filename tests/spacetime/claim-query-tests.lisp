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
the point -- an in-memory round trip would not exercise serialization.

EXTENT arrives via the constructor, not a post-construction SETF: a
not-yet-committed node's bytes are cached at construction and a plain
SETF before commit never reaches them (GH #135) -- see
SETF-CLAIM-EXTENT-DOES-NOT-PERSIST-ON-AN-UNCOMMITTED-CLAIM below, which
pins that failure mode so it stays a known contract, not a regression."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (id nil))
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((graph-db:*graph* g))
               (with-transaction ()
                 (setq id (id (make-u
                               :extent
                               (make-granule-instant (ts 2026 3 15) :month
                                                     :standing :observed))))))
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

(test the-sweep-removes-only-the-named-producers-claims
  (with-claim-graph (g)
    (with-transaction ()
      (make-b :producer :rule-a :object "o1")
      (make-b :producer :rule-a :object "o2")
      (make-u :producer :rule-a)
      (make-b :producer :rule-b :object "o1"))
    (is (= 3 (with-transaction ()
               (delete-claims-by-producer g 'ct-claim :rule-a))))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "s1"))))))

(test regeneration-leaves-no-orphan-when-a-rule-stops-producing-a-claim
  "Design §6.4 -- the case the constraint alone cannot fix.  v1 produces two
claims, v2 produces one; without the sweep the dropped claim would survive
forever, because no upsert ever touches it.

Sweep and reinsert are two SEPARATE transactions, not one: the unique
constraint's release (APPLY-TX-WRITES-TO-UNIQUE-INDEXES) is post-durability,
but its check (VALIDATE-UNIQUE-CONSTRAINTS) is pre-durability and runs first
within the same commit, so a same-transaction reinsert of an unchanged claim
would never see its own sweep's release (design §6.4) -- see
SWEEP-THEN-INSERT-OF-AN-UNCHANGED-CLAIM-COLLIDES-WITHIN-ONE-TRANSACTION
below, which pins that as the enforced boundary."
  (with-claim-graph (g)
    (with-transaction ()
      (make-b :producer :rule-a :object "kept")
      (make-b :producer :rule-a :object "dropped"))
    (with-transaction ()
      (delete-claims-by-producer g 'ct-claim :rule-a))
    (with-transaction ()
      (make-b :producer :rule-a :object "kept"))
    (let ((live (claims-touching g 'ct-claim :ns "s1")))
      (is (= 1 (length live)))
      (is (string= "kept" (claim-object-key (first live)))))))

(test sweep-then-insert-of-an-unchanged-claim-collides-within-one-transaction
  "Pins #131: MARK-DELETED's release lands in
APPLY-TX-WRITES-TO-UNIQUE-INDEXES, which runs post-durability, after
VALIDATE-UNIQUE-CONSTRAINTS's pre-durability check -- both inside the same
commit.  So a sweep and a reinsert of the identical claim in ONE transaction
always collide; the split into two transactions above is required, not
stylistic (design §6.4)."
  (with-claim-graph (g)
    (with-transaction () (make-b))
    (signals graph-db:unique-constraint-violation
      (with-transaction ()
        (delete-claims-by-producer g 'ct-claim :rule-a)
        (make-b)))))

(test the-sweep-makes-a-claim-re-insertable
  "After a sweep the constraint must not still be holding the old key."
  (with-claim-graph (g)
    (with-transaction () (make-b))
    (with-transaction () (delete-claims-by-producer g 'ct-claim :rule-a))
    (finishes (with-transaction () (make-b)))))

(test the-sweep-signals-on-an-unregistered-parent
  (with-claim-graph (g)
    (signals unknown-claim-family
      (delete-claims-by-producer g 'no-such-claim :rule-a))))

(test setf-claim-extent-does-not-persist-on-an-uncommitted-claim
  "Pins GH #135: a not-yet-committed node's bytes are cached at
construction, so a plain (SETF (CLAIM-EXTENT ...)) between MAKE-<ARITY>
and commit is visible in-memory but silently absent after a
close/reopen.  This is the documented contract, not a bug in this
subsystem -- MAKE-<ARITY>'s :EXTENT initarg is the correct way to give a
claim an extent at construction (see the reopen test above); SETF is for
mutating an already-committed claim (COPY, mutate, SAVE)."
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
                   (is-true (claim-extent c)
                            "visible in-memory, before commit")
                   (setq id (id c)))))
          (close-graph g)))
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (is (null (claim-extent (lookup-vertex id)))
                   "silently absent once read back from disk"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))
