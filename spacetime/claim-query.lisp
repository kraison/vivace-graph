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
                     ;; :PARENT is the condition's only initarg; :NAME left
                     ;; it unbound, so the report signalled (GH #335).
                     (error 'unknown-claim-family
                            :parent (class-name (class-of claim)))))
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
  "FIELD as the keyword %IDENTITY-KEY-FIELD rendered, else signal.
Validated before interning: only a canonical name ([a-z0-9-]+, the
RELATION rule) is interned, so a caller-supplied string cannot mint an
arbitrary keyword through this path -- KEYWORD symbols are never
collected.  The encoder downcases symbol names, so a non-canonical
namespace never round-trips anyway (GH #321)."
  (let ((name (and (> (length field) 1) (char= (char field 0) #\:)
                   (subseq field 1))))
    (if (and name (canonical-relation-p name))
        (intern (string-upcase name) :keyword)
        (error 'malformed-claim-identity-key :key key))))

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

(defun claim-commit-epoch (claim)
  "The epoch of the transaction that committed CLAIM's version, or NIL
for a REAPED-CLAIM (a version the store no longer holds) and for a
version not yet committed -- an epoch is assigned at commit, so a claim
read inside its own open transaction has none.  The number is the
writer's own TRANSACTION-ID, comparable across stores only while they
share one SYSTEM-CLOCK -- see GRAPH-DB:GRAPH-SYSTEM-CLOCK (GH #347)."
  (unless (reaped-claim-p claim)
    (let ((e (graph-db:commit-epoch claim)))
      (and (plusp e) e))))

(define-condition epoch-axis-unavailable
    (graph-db:query-precondition-error)
  ((graph-name :initarg :graph-name
               :reader epoch-axis-unavailable-graph-name))
  (:documentation "An :AS-OF-EPOCH read of a store with no system clock.
Its epochs are a private counter, so an answer would look like the
attached case and mean something unrelated (GH #347 recon E9).  The
parent's REASON is filled at the signal site.  The rules subsystem
handles QUERY-PRECONDITION-ERROR as \"no facts\"; a reader there that
ever takes the epoch axis must let this subtype through."))

(defun %refuse-epoch-axis (graph)
  "Signal EPOCH-AXIS-UNAVAILABLE unless GRAPH is attached to a clock.
Whether two stores share ONE clock is the consumer's precondition; a
single-store reader cannot see the other store."
  (unless (graph-db:graph-system-clock graph)
    (let ((name (graph-db:graph-name graph)))
      (error 'epoch-axis-unavailable
             :graph-name name
             :reason (format nil "~(~s~) has no system clock; ~
                                  :as-of-epoch needs one"
                             name)))))

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

(defun %claim-as-of-epoch (graph claim epoch)
  "The version of CLAIM live at EPOCH -- committed at or before it and
not retracted at or before it -- or NIL when there is none (created
after EPOCH, or retracted by then), or a REAPED-CLAIM when versions of
that age existed but are past the family's :KEEP-REVISIONS window.
Walks VERTEX-HISTORY newest-first comparing each version's own commit
epoch with <=; RESOLVE-VERSION-AT-EPOCH is a strict snapshot-start
predicate and would drop the commit made AT EPOCH (#347 recon C2, C3).
A retraction is a version, so CLAIM-CURRENT-P on the selected version
is the whole retraction test (recon E4).  A claim created with an
already-closed transaction period -- a replicated or restored belief
retracted upstream -- reads as retracted at every epoch; there is no
epoch-to-instant map to probe its period with."
  (let* ((history (graph-db:vertex-history graph (graph-db:id claim)))
         (resolved (loop for (version . e) in history
                         when (<= e epoch) return version)))
    (cond (resolved (and (claim-current-p resolved) resolved))
          ((null history) nil)
          ;; Nothing old enough.  Reaping severs the chain, so only the
          ;; oldest retained REVISION tells created-after-EPOCH (0: it
          ;; is the create) from reaped (> 0) -- recon C4.  REVISION is
          ;; 32-bit and wraps; after 2^32 updates to one claim the
          ;; discriminator reads a wrapped 0 as the create.
          ((zerop (graph-db:revision (car (first (last history))))) nil)
          (t (%make-reaped-claim (graph-db:id claim))))))

(defun %paginate (list limit offset)
  "LIST cut to OFFSET/LIMIT; second value T when entries existed past the
cut (the REST envelope's one-past-the-cap rule, GH #302)."
  (let* ((start (min (or offset 0) (length list)))
         (rest (nthcdr start list)))
    (if limit
        (values (subseq rest 0 (min limit (length rest)))
                (> (length rest) limit))
        (values rest nil))))

(defun %overlay-transaction (graph all family admit)
  "ALL as the open transaction will commit it (GH #324): a candidate the
transaction updated is replaced by its written version, one it deleted
drops out, and a claim of FAMILY it created that satisfies ADMIT is
added.  ALL itself outside a transaction.  The commit view is the same
\"store with this transaction's writes on top\" that validation reads."
  (let ((tx graph-db::*transaction*))
    (if (null tx)
        all
        (let* ((view (graph-db:make-commit-view graph tx))
               (out '()))
          (dolist (c all)
            (let ((v (graph-db:view-node view (graph-db:id c))))
              (when v (push v out))))
          (dolist (w (graph-db:view-writes view))
            (let ((n (graph-db:view-node view (graph-db:id w))))
              (when (and n
                         (typep n (claim-family-parent family))
                         (null (graph-db:view-old-node view n))
                         (funcall admit n)
                         (not (find (graph-db:id n) out
                                    :key #'graph-db:id :test #'equalp)))
                (push n out))))
          (nreverse out)))))

(defun %narrow-claims (graph claims &key current probe relation
                                         as-of as-of-epoch limit offset)
  "The shared tail of CLAIMS-TOUCHING and NODE-CLAIMS (GH #369): resolve
the transaction axis (:AS-OF / :AS-OF-EPOCH), then filter currency
(:CURRENT), validity (PROBE, an extent), and RELATION, then paginate.
A REAPED-CLAIM survives every filter -- it is the record that a version
existed, not a candidate to judge (GH #300)."
  (let ((all claims))
    (cond (as-of
           (setf all (loop for c in all
                           for v = (%claim-as-of graph c as-of)
                           when v collect v)))
          (as-of-epoch
           (setf all (loop for c in all
                           for v = (%claim-as-of-epoch graph c as-of-epoch)
                           when v collect v))))
    (when current
      (setf all (remove-if-not (lambda (c)
                                 (or (reaped-claim-p c) (claim-current-p c)))
                               all)))
    (when probe
      (setf all (remove-if-not (lambda (c)
                                 (or (reaped-claim-p c)
                                     (%claim-validity-touches-p c probe)))
                               all)))
    (when relation
      (setf all (remove-if-not (lambda (c)
                                 (or (reaped-claim-p c)
                                     (equal relation (claim-relation c))))
                               all)))
    (%paginate all limit offset)))

(defun claims-touching (graph claim-class namespace key
                        &key (role :either) current at during
                             relation limit offset as-of as-of-epoch)
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

:AS-OF-EPOCH (an integer) answers on the same axis by commit epoch (GH
#347): each claim is the version whose committing transaction id is the
newest at or below it, dropped when that version is retracted, and a
REAPED-CLAIM when older versions existed but are past :KEEP-REVISIONS
-- told from \"created after\" by the oldest retained REVISION.  Epochs
compare across stores only while the stores share one SYSTEM-CLOCK; a
clockless store signals EPOCH-AXIS-UNAVAILABLE.  One of :AS-OF or
:AS-OF-EPOCH, not both.  :CURRENT is redundant on this axis: only
versions still believed are ever selected.

:RELATION (a canonical string) restricts to one relation; on the subject
side it rides the (subject-namespace subject-key relation) index (GH
#302), on the object side it filters the endpoint candidates.  :LIMIT /
:OFFSET cut the FINAL filtered result; the second return value is T when
more claims existed past the cut (NIL without :LIMIT).

Inside an open transaction the answer is what THAT transaction will
commit (GH #324): its own retractions, updates and new claims are
visible, so retract-then-assert on one series reads correctly before
the commit.  :AS-OF and :AS-OF-EPOCH are the exceptions -- they answer
committed history only; an uncommitted change is not yet history and
has no epoch at all.

An out-of-range ROLE signals rather than silently returning NIL -- NIL is
also the correct answer for \"no claims touch this endpoint\", and this
subsystem exists to keep those two cases from being confused."
  (check-type role (member :subject :object :either))
  (check-type at (or null local-time:timestamp))
  (check-type during (or null temporal-extent))
  (check-type as-of-epoch (or null unsigned-byte))
  (when (and at during)
    (error "Pass only one of :AT or :DURING, not both."))
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
  (when as-of-epoch (%refuse-epoch-axis graph))
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
      ;; The overlay is for the neither-axis arm only: an uncommitted
      ;; write has no epoch (#347 recon C6).
      (unless (or as-of as-of-epoch)
        (setf all (%overlay-transaction
                   graph all family
                   (lambda (c)
                     (or (and (member role '(:subject :either))
                              (equal namespace (claim-subject-namespace c))
                              (equal key (claim-subject-key c))
                              (or (null relation)
                                  (equal relation (claim-relation c))))
                         (and (member role '(:object :either))
                              (typep c (claim-family-binary family))
                              (equal namespace (claim-object-namespace c))
                              (equal key (claim-object-key c))))))))
      ;; The subject side already rode the relation index (GH #302); only
      ;; the object side still needs the filter.
      (%narrow-claims graph all
                      :current current :probe probe
                      :relation (and (member role '(:object :either))
                                     relation)
                      :as-of as-of :as-of-epoch as-of-epoch
                      :limit limit :offset offset))))

;;; Edges under claims: the adjacency reads (GH #369, spec sec.6.1).
;;; Both see LINKED claims only; CLAIMS-TOUCHING is the complete read.

(defun claim-endpoints (claim &key (graph (graph-db::node-graph claim)))
  "CLAIM's linked endpoint nodes: (VALUES SUBJECT-NODE OBJECT-NODE), from
its outgoing SUBJECT-OF / OBJECT-OF edges in GRAPH (its own store).  NIL
for an endpoint that is not linked -- key-only, or a unary claim's
object.  A cross-store endpoint is read through LOOKUP-VERTEX-ANYWHERE,
so it may be an UNRESOLVED-NODE marker while that store is detached.
Edges created in a still-open transaction are not visible until it
commits (adjacency is indexed at commit apply)."
  (flet ((endpoint (type)
           (let ((e (first (graph-db:outgoing-edges claim :graph graph
                                                          :edge-type type))))
             (when e
               (graph-db:lookup-vertex-anywhere (graph-db:to e))))))
    (values (endpoint 'subject-of) (endpoint 'object-of))))

(defun node-claims (node &key (graph (graph-db::node-graph node))
                              family (role :either) current relation
                              at during as-of as-of-epoch limit offset)
  "Claims linked to NODE, from its incoming SUBJECT-OF / OBJECT-OF edges
in GRAPH -- the adjacency twin of CLAIMS-TOUCHING, with the same
filters and the same meaning for each (GH #369, spec sec.6.1).  Linked
claims only: a key-only claim is not here.  FAMILY (a parent class
name) restricts to one family; default every family.

GRAPH is the store holding the CLAIMS, not necessarily NODE's own --
adjacency is indexed in the edge's store (edge.lisp, ADD-TO-VE-INDEX) --
and defaults to NODE's store, which is the answer in a one-store
deployment; pass the claim store when the two differ.  Inside an open
transaction, only committed adjacency is visible; CLAIMS-TOUCHING is the
read that sees the transaction's own writes (GH #324)."
  (check-type role (member :subject :object :either))
  (check-type at (or null local-time:timestamp))
  (check-type during (or null temporal-extent))
  (check-type as-of-epoch (or null unsigned-byte))
  (when (and at during)
    (error "Pass only one of :AT or :DURING, not both."))
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
  (when as-of-epoch (%refuse-epoch-axis graph))
  (let ((probe (cond (at (make-instant (exact-bound at)))
                     (during during)))
        (parent (and family (claim-family-parent (claim-family family))))
        (claims '())
        (seen (make-hash-table :test 'equalp)))
    (flet ((collect (type)
             (graph-db:map-edges
              (lambda (e)
                (let ((c (graph-db:lookup-vertex (graph-db:from e)
                                                 :graph graph)))
                  (when (and c
                             (or (null parent) (typep c parent))
                             (not (gethash (graph-db:id c) seen)))
                    (setf (gethash (graph-db:id c) seen) t)
                    (push c claims))))
              graph :vertex node :direction :in :edge-type type)))
      (when (member role '(:subject :either)) (collect 'subject-of))
      (when (member role '(:object :either)) (collect 'object-of)))
    (%narrow-claims graph (nreverse claims)
                    :current current :probe probe :relation relation
                    :as-of as-of :as-of-epoch as-of-epoch
                    :limit limit :offset offset)))

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
                           &key limit offset as-of as-of-epoch)
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

:AS-OF and :AS-OF-EPOCH resolve each claim to the version believed at
a wall-clock instant or live at a commit epoch, exactly as
CLAIMS-TOUCHING does (GH #300, GH #347); one or the other, not both.

Uses the PRODUCER index, so this is O(matching) rather than a scan of every
claim.  :LIMIT / :OFFSET cut the result; the second return value is T
when more claims existed past the cut (NIL without :LIMIT) (GH #302)."
  (check-type as-of-epoch (or null unsigned-byte))
  (when (and as-of as-of-epoch)
    (error "Pass only one of :AS-OF or :AS-OF-EPOCH, not both."))
  (when as-of-epoch (%refuse-epoch-axis graph))
  (let* ((family (claim-family claim-class))
         (all (graph-db:index-lookup graph (claim-family-parent family)
                                     '(producer) producer)))
    (cond (as-of
           (setf all (loop for c in all
                           for v = (%claim-as-of graph c as-of)
                           when v collect v)))
          (as-of-epoch
           (setf all (loop for c in all
                           for v = (%claim-as-of-epoch graph c as-of-epoch)
                           when v collect v)))
          (t
           (setf all (%overlay-transaction
                      graph all family
                      (lambda (c) (equal producer (claim-producer c)))))))
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

;;; ---------------------------------------------------------------------------
;;; Vocabulary: what a family names (GH #350, spec 2026-09-07 §4).
;;;
;;; Names come from the family's ordered indexes by MAP-INDEX-PREFIXES;
;;; every name is confirmed by resolving one live node under it (R7),
;;; counts are index-range sizes (R2), and :CURRENT resolves the range
;;; (R4).  Membership is live: an open WITH-AS-OF extent changes only
;;; what a name's nodes resolve to (R6).  Inside an open transaction
;;; every answer is what it will commit: committed entries resolve
;;; through the commit view, the transaction's own creates are added
;;; (R5, §4.4).
;;;
;;; GH #361 §3 (spec 2026-09-08): outside an as-of extent the answer is
;;; a read of the family's three count indexes plus the open
;;; transaction's delta, and no node is resolved.  The walk below stays
;;; -- it is the only mechanism that answers at an epoch (R7).
;;; ---------------------------------------------------------------------------

(defun %refuse-vocabulary-axis (as-of as-of-epoch)
  "The listing answers live membership only (GH #350 R6)."
  (when (or as-of as-of-epoch)
    (error 'graph-db:query-precondition-error
           :reason (format nil "The vocabulary listing has no :AS-OF / ~
:AS-OF-EPOCH axis: index membership is live (GH #350, ~
docs/time-travel.md Bounds)."))))

(defun %vocabulary-sources (family role)
  "The (CLASS SLOTS) pairs the walk reads for ROLE: the subject index on
the parent, the object index on the binary class -- declared on
different classes, and the parent signals for the object slots."
  (ecase role
    (:subject (list (list (claim-family-parent family)
                          '(subject-namespace subject-key))))
    (:object (list (list (claim-family-binary family)
                         '(object-namespace object-key))))
    (:either (append (%vocabulary-sources family :subject)
                     (%vocabulary-sources family :object)))))

(defun %vocabulary-key (slots prefix)
  "PREFIX as MAP-INDEX and INDEX-COUNT take it: a scalar on a
single-slot index, the tuple otherwise.  Trap: a null name on a
single-slot index gives NIL, which MAP-INDEX reads as an unbounded
bound -- do not pass one.  Claim identity makes a null relation
unreachable, so the walk never builds such a prefix."
  (if (= 1 (length slots)) (first prefix) prefix))

(defun %vocabulary-view (graph)
  "The commit view of the open transaction on GRAPH, or NIL outside one
\(the GH #324 rule, R5)."
  (let ((tx graph-db::*transaction*))
    (and tx (graph-db:make-commit-view graph tx))))

(defun %view-resolve (view node)
  "NODE as the transaction will commit it: NODE itself outside a
transaction, its written version inside one, NIL if that write deletes
it."
  (if view (graph-db:view-node view (graph-db:id node)) node))

(defun %claim-tuple (claim slots)
  "CLAIM's values for the index SLOTS, in order.  Trap: the object
accessors live on the binary class only, so CLAIM must be of the
source's own class -- check TYPEP before calling."
  (loop for slot in slots
        collect (ecase slot
                  (subject-namespace (claim-subject-namespace claim))
                  (subject-key (claim-subject-key claim))
                  (object-namespace (claim-object-namespace claim))
                  (object-key (claim-object-key claim))
                  (relation (claim-relation claim)))))

(defun %created-under (view class slots prefix current)
  "The claims of CLASS the open transaction created whose SLOTS tuple
starts with PREFIX (NIL for any prefix) -- current ones with CURRENT.
NIL outside a transaction: the index already holds every committed
claim, and holds nothing of this transaction until it applies."
  (when view
    (let ((out '()))
      (dolist (w (graph-db:view-writes view) (nreverse out))
        (let ((n (graph-db:view-node view (graph-db:id w))))
          (when (and n
                     (typep n class)
                     (null (graph-db:view-old-node view n))
                     (or (null prefix)
                         (every #'equal prefix
                                (subseq (%claim-tuple n slots)
                                        0 (length prefix))))
                     (or (not current) (claim-current-p n)))
            (push n out)))))))

(defun %name-admitted-p (graph class slots prefix current view)
  "T when a claim under PREFIX resolves live through VIEW -- current,
with CURRENT (R7, R4); stops at the first.  Committed entries only:
add %CREATED-UNDER for the open transaction's own claims."
  (let ((key (%vocabulary-key slots prefix)))
    (block found
      (graph-db:map-index
       (lambda (node)
         (let ((n (%view-resolve view node)))
           (when (and n (or (not current) (claim-current-p n)))
             (return-from found t))))
       graph class slots :start key :end key)
      nil)))

(defun %name-count (graph class slots prefix current view)
  "Claims under PREFIX as the transaction will commit them: outside a
transaction and without CURRENT the index range's size; otherwise each
committed entry resolved through VIEW, plus the claims the transaction
created under PREFIX (R2, R4, spec §4.4).  Trap: the fast path counts
entries, not live nodes."
  (let ((key (%vocabulary-key slots prefix)))
    (if (and (null view) (not current))
        (graph-db:index-count graph class slots key :prefix t)
        (let ((n (length (%created-under view class slots prefix
                                         current))))
          (graph-db:map-index
           (lambda (node)
             (let ((c (%view-resolve view node)))
               (when (and c (or (not current) (claim-current-p c)))
                 (incf n))))
           graph class slots :start key :end key)
          n))))

(defun %walk-names (graph class slots arity start position current counts)
  "The admitted names under (CLASS SLOTS) at ARITY from START, in index
order, plus the names the open transaction's created claims introduce:
the component at POSITION of each prefix, or (NAME . COUNT) with
COUNTS.  With START the walk stops at the first prefix whose leading
component leaves START's.  Not sorted -- %MERGE-NAMES sorts, so a
created name lands in index order."
  (let* ((view (%vocabulary-view graph))
         (seen '())
         (names '()))
    (labels ((tally (prefix)
               (and counts
                    (%name-count graph class slots prefix current view)))
             (note (prefix count)
               (push prefix seen)
               (let ((name (nth position prefix)))
                 (push (if counts (cons name count) name) names)))
             (admit (prefix)
               ;; With COUNTS the count decides admission -- 0 is
               ;; exactly "nothing live under the name" -- so the range
               ;; resolves once, not twice.  INDEX-COUNT counts entries
               ;; rather than live nodes, so R7's confirmation still
               ;; runs on that path (GH #350, spec §4.4).
               (if counts
                   (let ((n (tally prefix)))
                     (when (and (plusp n)
                                (or view current
                                    (%name-admitted-p graph class slots
                                                      prefix current
                                                      view)))
                       (note prefix n)))
                   (when (%name-admitted-p graph class slots prefix
                                           current view)
                     (note prefix nil)))))
      (block walk
        (graph-db:map-index-prefixes
         (lambda (prefix)
           (when (and start (not (equal (first prefix) (first start))))
             (return-from walk))
           (admit prefix))
         graph class slots :arity arity :start start))
      ;; Names only the transaction's own creates hold (GH #324).
      (dolist (c (%created-under view class slots start current))
        (let ((prefix (subseq (%claim-tuple c slots) 0 arity)))
          (unless (member prefix seen :test #'equal)
            (note prefix (tally prefix))))))
    (nreverse names)))

(defun %name-lessp (a b)
  "Index order for two names: the engine's per-component collation.
Total for the non-null names claim identity guarantees; LESS-THAN
orders NIL against a symbol in one direction only, so a null name
would not sort stably."
  (graph-db::less-than a b))

(defun %merge-names (lists counts)
  "LISTS, each in index order, as one list in index order without
duplicates; with COUNTS the entries are (NAME . COUNT) and a name in
several lists sums its counts."
  (let ((all (stable-sort (apply #'append lists) #'%name-lessp
                          :key (if counts #'car #'identity)))
        (out '()))
    (dolist (e all (nreverse out))
      (let ((name (if counts (car e) e)))
        (if (and out (equal name (if counts (car (first out)) (first out))))
            (when counts (incf (cdr (first out)) (cdr e)))
            (push (if counts (cons name (cdr e)) name) out))))))

(defun %names-at (graph class slots depth prefix)
  "The count index on (CLASS SLOTS) at DEPTH under PREFIX as
\(NAME ALL CURRENT) triples -- NAME the last component, CURRENT 0 for
an index with no predicate -- in index order (GH #361 §3.2)."
  (let ((out '()))
    (graph-db:map-count-index
     (lambda (components all cur)
       (push (list (car (last components)) all (or cur 0)) out))
     graph class slots :depth depth :prefix prefix)
    (nreverse out)))

(defun %vocabulary-delta (tx class slots depth prefix)
  "Per-name (ALL . CURRENT) adjustments TX will commit for the names at
DEPTH under PREFIX of (CLASS SLOTS): an EQUAL table, NIL outside a
transaction.  Bounded by the write set (GH #361 §3.3).  Trap:
GRAPH-DB:WRITES, not the commit view -- the view keys writes by id and
so hides the TX-CREATE of a claim created and deleted in one
transaction, while this must predict the apply pass write for write."
  (when tx
    (let ((delta (make-hash-table :test 'equal)))
      (flet ((bump (node d-all d-current)
               (when (and node (typep node class))
                 (let ((tuple (%claim-tuple node slots)))
                   ;; An all-null tuple is not counted, as %COUNT-TUPLE
                   ;; has it (count-index.lisp).
                   (when (and (notevery #'null tuple)
                              (every #'equal prefix tuple))
                     (let* ((name (nth (1- depth) tuple))
                            (cell (or (gethash name delta)
                                      (setf (gethash name delta)
                                            (cons 0 0)))))
                       (incf (car cell) d-all)
                       (incf (cdr cell) d-current)))))))
        (dolist (w (graph-db:writes tx))
          ;; The apply pass's arithmetic: the new version arrives, the
          ;; old one leaves, and a deleted new version only leaves.
          (let ((new (graph-db::node w))
                (old (and (typep w 'graph-db::tx-update)
                          (graph-db::old-node w))))
            (unless (or (typep w 'graph-db::tx-delete)
                        (graph-db:deleted-p new))
              (bump new 1 (if (claim-current-p new) 1 0)))
            (when old
              (bump old -1 (if (claim-current-p old) -1 0))))))
      delta)))

(defun %counted-names (graph class slots depth prefix current counts)
  "One source's names from its count index, the open transaction's
delta applied: names, or (NAME . COUNT); a name whose adjusted ALL is 0
is dropped, and under CURRENT one whose CURRENT is 0.  Not sorted --
%MERGE-NAMES sorts, so a name the transaction introduces lands in index
order."
  (let ((delta (%vocabulary-delta graph-db::*transaction* class slots
                                  depth prefix))
        (out '()))
    (flet ((emit (name all cur)
             (let ((n (if current cur all)))
               (when (plusp n)
                 (push (if counts (cons name n) name) out)))))
      (dolist (e (%names-at graph class slots depth prefix))
        (destructuring-bind (name all cur) e
          (let ((d (and delta (gethash name delta))))
            (when d (remhash name delta))
            (emit name (+ all (if d (car d) 0))
                  (+ cur (if d (cdr d) 0))))))
      ;; What is left holds only names the transaction introduces.
      (when delta
        (maphash (lambda (name d) (emit name (car d) (cdr d))) delta)))
    (nreverse out)))

(defun %vocabulary-1 (graph class slots depth prefix current counts)
  "One source's names at DEPTH under PREFIX: the count path, or #350's
walk inside an as-of extent -- the only mechanism that answers at an
epoch (GH #361 R7)."
  (if (graph-db::%as-of-snapshot graph)
      (%walk-names graph class slots depth prefix (1- depth) current
                   counts)
      (%counted-names graph class slots depth prefix current counts)))

(defun %vocabulary (graph family role depth prefix current counts)
  "The merged names of FAMILY for ROLE at DEPTH under PREFIX, in index
order."
  (%merge-names
   (loop for (class slots) in (%vocabulary-sources family role)
         collect (%vocabulary-1 graph class slots depth prefix current
                                counts))
   counts))

(defun claim-namespaces (graph claim-class
                         &key (role :either) current counts
                              as-of as-of-epoch)
  "The namespaces CLAIM-CLASS's family names as subject, object or
either, in index order, one entry per name; with COUNTS each is
\(NAME . COUNT), the claims under it in ROLE, summed under :EITHER.
The default lists every name the indexes hold, retracted claims
included; :CURRENT keeps a name only if a claim under it is current
and counts only those.  Inside an open transaction the answer is what
that transaction will commit (GH #324).  Trap: membership is live --
:AS-OF and :AS-OF-EPOCH are refused, and an open WITH-AS-OF extent
changes only what a name's claims resolve to (GH #350)."
  (check-type role (member :subject :object :either))
  (%refuse-vocabulary-axis as-of as-of-epoch)
  (let ((family (claim-family claim-class)))
    (%vocabulary graph family role 1 nil current counts)))

(defun claim-relations (graph claim-class
                        &key current counts as-of as-of-epoch)
  "The relations CLAIM-CLASS's family uses, in index order, from its
CLAIM-RELATION-COUNT index -- from CLAIM-RELATION inside an as-of
extent (GH #361); with COUNTS, (NAME . COUNT).  :CURRENT and the
refusals as CLAIM-NAMESPACES (GH #350)."
  (%refuse-vocabulary-axis as-of as-of-epoch)
  (let ((family (claim-family claim-class)))
    ;; One source, so no merge to do -- %MERGE-NAMES is here for the
    ;; sort alone: a relation the open transaction introduces must land
    ;; in index order, not after the counted ones.
    (%merge-names
     (list (%vocabulary-1 graph (claim-family-parent family) '(relation)
                          1 nil current counts))
     counts)))

(defun claim-keys (graph claim-class namespace
                   &key (role :either) current counts limit offset
                        as-of as-of-epoch)
  "The keys filed under NAMESPACE by CLAIM-CLASS's family in ROLE, in
index order, one entry per key, with COUNTS as (KEY . COUNT); NIL when
nothing is filed there.  :LIMIT / :OFFSET page the merged list; the
second value is T when entries existed past the cut.  :CURRENT and the
refusals as CLAIM-NAMESPACES (GH #350)."
  (check-type role (member :subject :object :either))
  (%refuse-vocabulary-axis as-of as-of-epoch)
  (let ((family (claim-family claim-class)))
    (%paginate (%vocabulary graph family role 2 (list namespace)
                            current counts)
               limit offset)))
