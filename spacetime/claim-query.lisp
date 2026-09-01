;;;; Reading claims back: the inverse query, the extent codec, and the
;;;; regeneration sweep (GH #131, design §7-§8).

(in-package #:graph-db.spacetime)

(defun extents-disjoint-p (a b)
  "True when extents A and B certainly share no instant: every Allen
relation possible between them is :BEFORE or :AFTER.  :MEETS is NOT
disjoint -- intervals are closed, so meeting extents share their boundary
instant -- and an ambiguous pair is not disjoint either.  A NIL extent
overlaps everything, so the predicate is total (GH #296, design §2.3)."
  (and a b
       (every (lambda (r) (member r '(:before :after)))
              (temporal-relation-relations (allen-relations a b)))
       t))

(defun %claim-validity-touches-p (claim probe)
  "True when CLAIM's extent possibly shares an instant with PROBE.  A
claim with no extent makes no validity statement and never matches."
  (let ((e (claim-extent claim)))
    (and e (not (extents-disjoint-p e probe)))))

(defun claims-touching (graph claim-class namespace key
                        &key (role :either) current at during)
  "Claims in GRAPH naming (NAMESPACE, KEY) as subject, object, or either.
CLAIM-CLASS is the PARENT class name; one call covers both arities.  Answers
from the claim graph's own indexes -- no cross-graph read, no snapshot, which
is what makes it implementable in this unit (design §8).

With :CURRENT, only claims still believed -- CLAIM-CURRENT-P -- so a
retracted registration does not read as live (GH #162).  The default
returns retracted claims too: they are the record of what was believed.

:AT (a TIMESTAMP) keeps claims whose validity extent possibly contains
that instant; :DURING (a TEMPORAL-EXTENT) keeps claims whose extent
possibly shares an instant with it -- the runs INTERSECTING the window,
not Allen's stricter :DURING.  One or the other, not both.  Both are
orthogonal to :CURRENT (validity vs transaction time, GH #148), both
exclude a claim with no extent, and both filter the candidates the
endpoint index already bounded (GH #296, design §2.5).

An out-of-range ROLE signals rather than silently returning NIL -- NIL is
also the correct answer for \"no claims touch this endpoint\", and this
subsystem exists to keep those two cases from being confused."
  (check-type role (member :subject :object :either))
  (check-type at (or null local-time:timestamp))
  (check-type during (or null temporal-extent))
  (when (and at during)
    (error "Pass only one of :AT or :DURING, not both."))
  (let* ((probe (cond (at (make-instant (exact-bound at)))
                      (during during)))
         (family (claim-family claim-class))
         (want (list namespace key))
         (subjects (when (member role '(:subject :either))
                     (graph-db:index-lookup
                      graph (claim-family-parent family)
                      '(subject-namespace subject-key) want)))
         (objects (when (member role '(:object :either))
                    (graph-db:index-lookup
                     graph (claim-family-binary family)
                     '(object-namespace object-key) want))))
    ;; A claim naming one endpoint as BOTH subject and object appears in both
    ;; lookups; the union must still return it once.
    (let ((all (if (and subjects objects)
                   (remove-duplicates (append subjects objects)
                                      :key #'graph-db:id :test #'equalp)
                   (or subjects objects))))
      (when current
        (setf all (remove-if-not #'claim-current-p all)))
      (when probe
        (setf all (remove-if-not (lambda (c)
                                   (%claim-validity-touches-p c probe))
                                 all)))
      all)))

(defun claim-extent (claim)
  "CLAIM's TEMPORAL-EXTENT, decoded from the stored sexp, or NIL.  The stored
form is EXTENT-SEXP; the two never share a name so neither is mistaken for
the other (design §7)."
  (let ((s (claim-extent-sexp claim)))
    (when s (sexp->extent s))))

(defun (setf claim-extent) (extent claim)
  "Store EXTENT on CLAIM as its sexp.  Only values GRAPH-DB:SERIALIZE
already handles reach the heap, so no core type byte is reserved.

Legal on any CLAIM this transaction is entitled to mutate (engine
slot-mutation contract, GH #135, now fixed): a claim created in this
transaction -- SETF its slots directly, no COPY needed, since COPY of an
uncommitted node signals COPYING-UNCOMMITTED-NODE -- or a COPY of a claim
looked up from the graph, mutated, then SAVEd.  MAKE-<ARITY>'s :EXTENT
initarg is still preferred for a brand-new claim, for ergonomics and
validation placement, not because this SETF would fail to persist."
  (setf (claim-extent-sexp claim) (and extent (extent->sexp extent)))
  extent)

(defun claim-transaction-extent (claim)
  "CLAIM's transaction-time TEMPORAL-EXTENT, decoded, or NIL when the claim
predates the axis (GH #148).  NIL is INDETERMINATE, never the epoch."
  (let ((s (claim-transaction-extent-sexp claim)))
    (when s (sexp->extent s))))

(defun (setf claim-transaction-extent) (extent claim)
  "Store EXTENT as CLAIM's transaction extent, once.  Signals
TRANSACTION-EXTENT-IMMUTABLE if CLAIM already has one -- an audit field is
written at creation and not revised (GH #148).  The one sanctioned change
after that is CLOSING the period, and RETRACT-CLAIM is its only writer
(GH #162).  Writing CLAIM-TRANSACTION-EXTENT-SEXP bypasses this guard
but not the commit: TRANSACTION-EXTENT-STEP refuses the same changes on
every write path, REST included (GH #158)."
  (when (claim-transaction-extent-sexp claim)
    (error 'transaction-extent-immutable))
  (setf (claim-transaction-extent-sexp claim)
        (and extent (extent->sexp extent)))
  extent)

(defun claim-recorded-at (claim)
  "Two values: when CLAIM was recorded, and that extent's STANDING.  A claim
predating the axis returns (VALUES NIL :INDETERMINATE) -- we do not know
when it was recorded, and that is not the same as the epoch (GH #148).  The
first value is the raw BOUND-EARLIEST of the extent's start: :UNBOUNDED
when that start is itself unbounded, and only the earliest edge of a
fuzzy (non-exact) start otherwise -- a caller must not assume it is a
TIMESTAMP without checking (GH #148)."
  (let ((e (claim-transaction-extent claim)))
    (if (null e)
        (values nil :indeterminate)
        (values (bound-earliest (extent-start e)) (extent-standing e)))))

(defun claim-current-p (claim)
  "True while CLAIM is still believed: its transaction period is open, or
absent -- a claim predating the axis was never retracted.  NIL once
RETRACT-CLAIM has closed the period (GH #162)."
  (let ((e (claim-transaction-extent claim)))
    (or (null e) (bound-unknown-p (extent-end e)))))

(defun retract-claim (claim &key (at (local-time:now)))
  "Close CLAIM's transaction period at AT: it was believed until now and no
longer is, and the record of that belief stays -- the bitemporal
[recorded, superseded) the #148 design left as a seam (GH #162).

NOT a deletion.  A retracted claim still occupies its identity tuple, so a
later assertion of the same fact re-opens it (REGISTER-NODE does this),
and CLAIMS-TOUCHING still returns it unless :CURRENT filters it;
CLAIM-CURRENT-P tells the two apart.  A claim predating the axis closes as
[unknown, AT).  Already-retracted claims are left as they are.  JOINS an
ambient transaction when one is open -- so retract-then-assert inside one
WITH-TRANSACTION commits or fails as a unit, which membership
disjointness depends on (GH #157 4b) -- and opens its own otherwise.
Returns the saved copy, or CLAIM itself when nothing was written."
  (flet ((%retract ()
           (let* ((c (graph-db:copy claim))
                  (e (claim-transaction-extent c))
                  (start (if e (extent-start e) (unknown-bound))))
             (setf (claim-transaction-extent-sexp c)
                   (extent->sexp (make-interval start (exact-bound at)
                                                :semantics :transaction
                                                :standing :asserted)))
             (graph-db:save c)
             c)))
    (cond ((not (claim-current-p claim)) claim)
          (graph-db::*transaction* (%retract))
          (t (graph-db:with-transaction () (%retract))))))

(defun claims-by-producer (graph claim-class producer)
  "Every live claim PRODUCER wrote, both arities.  CLAIM-CLASS is the PARENT,
so one call covers unary and binary -- the same contract as this function's
destructive twin, DELETE-CLAIMS-BY-PRODUCER.

This is the audit direction CLAIMS-TOUCHING cannot serve: that one answers
only for an endpoint the caller already thinks of, so it structurally cannot
find a claim nothing justifies -- the orphan case §6.4 says the uniqueness
constraint cannot catch either (GH #145).

NIL means PRODUCER has written nothing, which is a real answer.  An
unregistered CLAIM-CLASS signals UNKNOWN-CLAIM-FAMILY instead, so \"no such
family\" and \"that family, nothing produced\" stay distinguishable.

Uses the PRODUCER index, so this is O(matching) rather than a scan of every
claim."
  (let ((family (claim-family claim-class)))
    (graph-db:index-lookup graph (claim-family-parent family)
                           '(producer) producer)))

(defun delete-claims-by-producer (graph claim-class producer)
  "Mark every claim PRODUCER wrote as deleted; return how many.  CLAIM-CLASS
is the PARENT, so one call sweeps both arities.

Regeneration is sweep-then-insert, and the uniqueness constraint is NOT what
makes it work: a rule that stops producing a claim leaves an orphan no upsert
can remove (design §6.4).  Uses the PRODUCER index, so this is O(matching)
rather than a scan of every claim."
  (let ((family (claim-family claim-class))
        (n 0))
    (dolist (c (graph-db:index-lookup graph (claim-family-parent family)
                                      '(producer) producer)
             n)
      (graph-db:mark-deleted c)
      (incf n))))
