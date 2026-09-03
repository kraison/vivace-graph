;;;; Reading claims back: the inverse query, the extent codec, and the
;;;; regeneration sweep (GH #131, design §7-§8).

(in-package #:graph-db.spacetime)

(defun %claim-validity-touches-p (claim probe)
  "True when CLAIM's extent possibly shares an instant with PROBE.  A
claim with no extent makes no validity statement and never matches."
  (let ((e (claim-extent claim)))
    (and e (not (extents-disjoint-p e probe)))))

(defun %identity-key-field (x)
  "One identity-tuple field rendered for CLAIM-IDENTITY-KEY: strings pass
escaped (\\ then |), keywords/symbols as :lowercase-name, integers as
decimals, anything else via PRIN1.  Keys are canonically strings here;
the escapes make the join injective for them."
  (flet ((esc (str)
           (with-output-to-string (out)
             (loop for ch across str do
               (when (or (char= ch #\\) (char= ch #\|))
                 (write-char #\\ out))
               (write-char ch out)))))
    (etypecase x
      (string (esc x))
      (symbol (format nil ":~(~a~)" (symbol-name x)))
      (integer (format nil "~d" x))
      (t (esc (prin1-to-string x))))))

(defun claim-identity-key (claim)
  "CLAIM's identity tuple as one canonical string: producer, subject
namespace and key, relation, the object pair for a binary claim, and --
for a temporal family -- the extent START exactly as the identity tuple
canonicalises it (EXTENT-SEXP-START-KEY).  Equal identity tuples render
STRING= keys; the key survives retraction, re-assertion and
regeneration, which node ids do not (GH #303).  Fields join on |, with
| and \\ escaped inside string fields."
  (let* ((family (or (find-if (lambda (f)
                                (typep claim (claim-family-parent f)))
                              (alexandria:hash-table-values
                               *claim-families*))
                     (error 'unknown-claim-family
                            :name (class-name (class-of claim)))))
         (binary-p (typep claim (claim-family-binary family)))
         (fields
           (append
            (list (%identity-key-field (claim-producer claim))
                  (%identity-key-field (claim-subject-namespace claim))
                  (%identity-key-field (claim-subject-key claim)))
            (when binary-p
              (list (%identity-key-field (claim-object-namespace claim))
                    (%identity-key-field (claim-object-key claim))))
            (list (%identity-key-field (claim-relation claim)))
            (when (claim-family-temporal-p family)
              (list (let ((*print-case* :downcase))
                      (prin1-to-string
                       (extent-sexp-start-key
                        (claim-extent-sexp claim)))))))))
    (format nil "~{~a~^|~}" fields)))

(defun %split-identity-key-fields (key)
  "KEY's fields: split on unescaped |, with \\| and \\\\ unescaped."
  (let ((fields '()) (buf (make-string-output-stream))
        (i 0) (n (length key)))
    (loop while (< i n) do
      (let ((ch (char key i)))
        (cond ((char= ch #\\)
               (when (= (1+ i) n)
                 (error 'malformed-claim-identity-key :key key))
               (write-char (char key (1+ i)) buf)
               (incf i 2))
              ((char= ch #\|)
               (push (get-output-stream-string buf) fields)
               (incf i))
              (t (write-char ch buf) (incf i)))))
    (push (get-output-stream-string buf) fields)
    (nreverse fields)))

(defun %identity-key-namespace (field key)
  "FIELD as the keyword %IDENTITY-KEY-FIELD rendered, else signal."
  (if (and (> (length field) 1) (char= (char field 0) #\:))
      (intern (string-upcase (subseq field 1)) :keyword)
      (error 'malformed-claim-identity-key :key key)))

(defun %identity-key-extent-start (field key)
  "FIELD read back as EXTENT-SEXP-START-KEY data, *READ-EVAL* off."
  (handler-case
      (with-standard-io-syntax
        (let ((*read-eval* nil)
              (*package* (find-package :graph-db.spacetime)))
          (read-from-string field)))
    (error () (error 'malformed-claim-identity-key :key key))))

(defun split-claim-identity-key (key)
  "The inverse of CLAIM-IDENTITY-KEY (GH #321): (VALUES PRODUCER
SUBJECT-NAMESPACE SUBJECT-KEY RELATION OBJECT-NAMESPACE OBJECT-KEY
EXTENT-START), NIL for the fields a unary or non-temporal key lacks.
Arity and temporality follow from the field count -- 4, 5, 6 or 7 --
so the string alone suffices.  Namespaces come back as keywords, keys
and relations as strings (an integer key encodes as its decimal string
and decodes as one), EXTENT-START as EXTENT-SEXP-START-KEY's data.  The
escape rule lives here and in %IDENTITY-KEY-FIELD, nowhere else.
Signals MALFORMED-CLAIM-IDENTITY-KEY for any other shape."
  (let* ((fields (%split-identity-key-fields key))
         (n (length fields)))
    (unless (<= 4 n 7)
      (error 'malformed-claim-identity-key :key key))
    (let* ((binary-p (>= n 6))
           (temporal-p (oddp n))
           (after-subject (cdddr fields))
           (object-namespace (and binary-p
                                  (%identity-key-namespace
                                   (first after-subject) key)))
           (object-key (and binary-p (second after-subject)))
           (tail (if binary-p (cddr after-subject) after-subject)))
      (values (first fields)
              (%identity-key-namespace (second fields) key)
              (third fields)
              (first tail)
              object-namespace
              object-key
              (and temporal-p
                   (%identity-key-extent-start (second tail) key))))))

(defstruct (reaped-claim (:constructor %make-reaped-claim (id)))
  "An :AS-OF answer the store can no longer give: the claim existed at
the asked instant, but every version stamped then is past the family's
:KEEP-REVISIONS window and reaped.  Reported, never silently substituted
(GH #300)."
  id)

(defun %claim-effective-stamp (version)
  "VERSION's place on the wall clock: its :AS-OF stamp, else the start of
its (immutable) transaction extent, else NIL for a claim predating both
axes -- treated as arbitrarily old."
  (or (claim-version-stamp version)
      (let ((te (claim-transaction-extent version)))
        (when te
          (let ((b (extent-start te)))
            (let ((e (bound-earliest b)))
              (unless (eq e :unbounded) e)))))))

(defun %claim-as-of (graph claim at)
  "The version of CLAIM believed AT (a TIMESTAMP), or NIL when the claim
was not believed then (not yet created, or already retracted), or a
REAPED-CLAIM when it existed but every version of that age is reaped.
Walks VERTEX-HISTORY newest-first over the family's retained chain."
  (let* ((history (graph-db:vertex-history graph (graph-db:id claim)))
         (instant (make-instant (exact-bound at)))
         (resolved
           (loop for (version . nil) in history
                 for stamp = (%claim-effective-stamp version)
                 when (or (null stamp)
                          (not (local-time:timestamp< at stamp)))
                   return version)))
    (cond
      (resolved
       ;; Believed at AT only while AT falls inside that version's
       ;; transaction period: a retraction closes it, so an instant
       ;; after the close resolves to the retracted version and drops
       ;; here.  A NIL extent predates the axis: indeterminate, kept.
       (let ((te (claim-transaction-extent resolved)))
         (if (or (null te) (not (extents-disjoint-p te instant)))
             resolved
             nil)))
      ((null history) nil)
      (t
       ;; No retained version is old enough.  The immutable transaction
       ;; start on ANY version says whether the claim existed at AT.
       (let ((te (claim-transaction-extent (car (first history)))))
         (cond ((null te) (%make-reaped-claim (graph-db:id claim)))
               ((let ((e (bound-earliest (extent-start te))))
                  (and (not (eq e :unbounded))
                       (local-time:timestamp< at e)))
                nil)                    ; did not exist yet
               (t (%make-reaped-claim (graph-db:id claim)))))))))

(defun %paginate (list limit offset)
  "LIST cut to OFFSET/LIMIT; second value T when entries existed past the
cut (the REST envelope's one-past-the-cap rule, GH #302)."
  (let* ((start (min (or offset 0) (length list)))
         (rest (nthcdr start list)))
    (if limit
        (values (subseq rest 0 (min limit (length rest)))
                (> (length rest) limit))
        (values rest nil))))

(defun claims-touching (graph claim-class namespace key
                        &key (role :either) current at during
                             relation limit offset as-of)
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

:AS-OF (a TIMESTAMP) answers on the TRANSACTION axis (GH #300): each
claim is returned AS THE VERSION believed at that instant -- an in-place
update after AS-OF is unwound to the earlier version, a claim retracted
before AS-OF drops out, one not yet created drops out, and one whose
versions of that age are reaped past the family's :KEEP-REVISIONS window
appears as a REAPED-CLAIM, never as a silently-substituted newer
version.  :AT/:DURING then filter the RESOLVED version's validity.  No
argument or result is an epoch; the mapping is the per-version stamp in
the claim's own data, so replicas answer from their own applied history.

:RELATION (a canonical string) restricts to one relation; on the subject
side it rides the (subject-namespace subject-key relation) index (GH
#302), on the object side it filters the endpoint candidates.  :LIMIT /
:OFFSET cut the FINAL filtered result; the second return value is T when
more claims existed past the cut (NIL without :LIMIT).

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
                     (if relation
                         (graph-db:index-lookup
                          graph (claim-family-parent family)
                          '(subject-namespace subject-key relation)
                          (list namespace key relation))
                         (graph-db:index-lookup
                          graph (claim-family-parent family)
                          '(subject-namespace subject-key) want))))
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
      (when as-of
        (setf all (loop for c in all
                        for v = (%claim-as-of graph c as-of)
                        when v collect v)))
      (when current
        (setf all (remove-if-not (lambda (c)
                                   (or (reaped-claim-p c)
                                       (claim-current-p c)))
                                 all)))
      (when probe
        (setf all (remove-if-not (lambda (c)
                                   (or (reaped-claim-p c)
                                       (%claim-validity-touches-p c probe)))
                                 all)))
      (when (and relation (member role '(:object :either)))
        ;; The object side has no relation index; filter what the endpoint
        ;; index already bounded (GH #302).
        (setf all (remove-if-not (lambda (c)
                                   (or (reaped-claim-p c)
                                       (equal relation (claim-relation c))))
                                 all)))
      (%paginate all limit offset))))

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

(defun retract-claim (claim &key (at (%st-now)))
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

(defun claims-by-producer (graph claim-class producer
                           &key limit offset as-of)
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
claim.  :LIMIT / :OFFSET cut the result; the second return value is T
when more claims existed past the cut (NIL without :LIMIT) (GH #302)."
  (let* ((family (claim-family claim-class))
         (all (graph-db:index-lookup graph (claim-family-parent family)
                                     '(producer) producer)))
    (when as-of
      (setf all (loop for c in all
                      for v = (%claim-as-of graph c as-of)
                      when v collect v)))
    (%paginate all limit offset)))

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
