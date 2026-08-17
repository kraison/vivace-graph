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

EXTENT arrives via the constructor here; a post-construction SETF works
too, now that GH #135 is fixed -- see
SETF-CLAIM-EXTENT-PERSISTS-ON-AN-UNCOMMITTED-CLAIM below.  The initarg
remains the preferred form for ergonomics and validation placement, not
because SETF fails to persist."
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

(test setf-claim-extent-persists-on-an-uncommitted-claim
  "GH #135 is fixed: SETF on a node created in the current transaction now
persists correctly, because the create path re-serializes BYTES from DATA
just as the update path always did.  So (SETF (CLAIM-EXTENT ...)) on a
just-created, not-yet-committed claim survives a close/reopen, exactly
like the :EXTENT initarg does (see the reopen test above).  The reopen
here is the point -- an in-memory assertion would pass whether or not the
fix landed."
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
             (let* ((graph-db:*graph* g2)
                    (e (claim-extent (lookup-vertex id))))
               (is (eq :instant (extent-kind e))
                   "the SETF extent survived the reopen")
               (is (eq :month (extent-precision e)))
               (is (eq :observed (extent-standing e))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; Reading by producer -- the audit counterpart of the sweep (GH #145).

(test claims-by-producer-finds-both-arities
  "CLAIM-CLASS is the PARENT, so one call covers unary and binary, exactly
as DELETE-CLAIMS-BY-PRODUCER does."
  (with-claim-graph (g)
    (with-transaction ()
      (make-b :producer :rule-a :object "o1")
      (make-u :producer :rule-a)
      (make-b :producer :rule-b :object "o2"))
    (is (= 2 (length (claims-by-producer g 'ct-claim :rule-a))))
    (is (= 1 (length (claims-by-producer g 'ct-claim :rule-b))))))

(test claims-by-producer-returns-nil-for-a-producer-that-wrote-nothing
  "NIL is a real answer here -- 'that rule has produced nothing' -- and must
not be an error."
  (with-claim-graph (g)
    (with-transaction () (make-b :producer :rule-a))
    (is (null (claims-by-producer g 'ct-claim :rule-never-ran)))))

(test claims-by-producer-does-not-return-swept-claims
  "⚠ The sweep MARKS DELETED rather than removing, so whether the producer
index still surfaces those nodes decides whether an auditing caller sees
ghosts.  A reconciliation built on this would report a stale projection as
current -- the failure it exists to catch."
  (with-claim-graph (g)
    (with-transaction ()
      (make-b :producer :rule-a :object "o1")
      (make-u :producer :rule-a))
    (with-transaction () (delete-claims-by-producer g 'ct-claim :rule-a))
    (is (null (claims-by-producer g 'ct-claim :rule-a)))))

(test claims-by-producer-signals-on-an-unregistered-parent
  "Same contract as the sweep: an unknown family signals rather than
answering NIL, which is also the correct answer for 'wrote nothing'."
  (with-claim-graph (g)
    (signals unknown-claim-family
      (claims-by-producer g 'no-such-claim :rule-a))))
