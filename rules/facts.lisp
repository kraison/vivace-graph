;;;; rules/facts.lisp -- claims as Prolog facts (spec §4, GH #330).
;;;;
;;;; Homed in GRAPH-DB, not GRAPH-DB.RULES: DEF-GLOBAL-PROLOG-FUNCTOR
;;;; exports from *PACKAGE*, and MAKE-FUNCTOR-SYMBOL resolves a goal head
;;;; in its own package and then in GRAPH-DB -- so a raw SELECT written in
;;;; any package resolves these.  Declared deviation from spec §3;
;;;; docs/rules.md.  Generators follow FIND-BY-SLOT/4's trail discipline
;;;; (index.lisp).

(in-package #:graph-db)

;;; The claim indexes DEF-CLAIM-CLASSES declares (spacetime/claim.lisp).
;;; INDEX-LOOKUP matches a slot-name list with EQUAL, i.e. EQ per symbol,
;;; so these must be GRAPH-DB.SPACETIME's symbols and not same-named ones
;;; read here; the slots are not exported, hence "::".

(defparameter +claim-subject-index-slots+
  '(graph-db.spacetime::subject-namespace graph-db.spacetime::subject-key))

(defparameter +claim-subject-relation-index-slots+
  '(graph-db.spacetime::subject-namespace graph-db.spacetime::subject-key
    graph-db.spacetime::relation))

(defparameter +claim-object-index-slots+
  '(graph-db.spacetime::object-namespace graph-db.spacetime::object-key))

(defparameter +claim-producer-index-slots+
  '(graph-db.spacetime::producer))

(defvar *claim-scope* nil
  "The stores CLAIM/7 and CLAIM-PRODUCER/2 read, own store first, or NIL
for *GRAPH* alone (spec §10, GH #332).  RUN-RULE binds it for a body's
evaluation; a Lisp caller may bind it around a SELECT.  Trap: inside a
read-write transaction every read of another store is the engine's
CROSS-GRAPH-TRANSACTION-ERROR (GH #53) -- bind it outside one.")

(defvar *rule-delta* nil
  "NIL, or an EQUAL hash table relation -> list of claim nodes: the
claims a fixpoint round derived new, which RULE-DELTA/2 generates for
the next round (GH #333).  Bound by RUN-RULES around a recursive
stratum; never by a query.")

(defvar *claim-exclude-producers* nil
  "NIL, or a list of (PRODUCER . RELATION) pairs CLAIM/7's index and
scan routes must not answer from: a fixpoint round reads base facts
plus the delta, never the stratum's own prior output, since that
output is exactly what this round's reconcile may still sweep
(GH #333).  Bound by %RUN-STRATUM around every round, round 0
included.  Never applies to a bound ?C -- that is how the delta
itself, and any other caller holding a node already, keeps working.
Scoped by RELATION as well as producer, so a body reading a stratum
producer's DERIVATION records -- another family, another relation --
still sees them (M3).")

(defstruct (derived-index (:constructor make-derived-index))
  "One fixpoint round's own derivation of one relation, indexed like
CLAIM/7's routes so a rule with more than one recursive goal can see
what its other goal already derived this run (GH #333, C1): BY-SUBJECT
and BY-OBJECT are EQUAL hashes (namespace-keyword . key) -> claim
nodes; ALL is every one of them, for the unindexed scan route."
  (by-subject (make-hash-table :test 'equal))
  (by-object (make-hash-table :test 'equal))
  (all '()))

(defvar *claim-derived-this-run* nil
  "NIL, or an EQUAL hash table relation-string -> DERIVED-INDEX: one
fixpoint run's own derivation so far, unioned into CLAIM/7's index and
scan candidates (never the bound-?C route) so a rule with more than
one recursive goal still sees what its OTHER goal derived earlier this
run -- a plain read otherwise only sees state committed before the
run started (GH #333).  Bound by %RUN-STRATUM around every round.")

(defun %derived-by-subject (rel key)
  "Claims *CLAIM-DERIVED-THIS-RUN* holds with subject KEY -- a
(namespace . key) pair -- and relation REL, or across every relation
when REL is NIL (the subject-only route does not know the relation
either): CLAIM/7's index sees only committed claims, so this unions in
what this run has itself derived (GH #333)."
  (when *claim-derived-this-run*
    (if rel
        (let ((idx (gethash rel *claim-derived-this-run*)))
          (and idx (gethash key (derived-index-by-subject idx))))
        (loop for idx being the hash-values of *claim-derived-this-run*
              append (gethash key (derived-index-by-subject idx))))))

(defun %derived-by-object (key)
  "Claims derived this run with object KEY, across every relation --
the object index CLAIM/7 substitutes for is not relation-scoped either
(GH #333)."
  (when *claim-derived-this-run*
    (loop for idx being the hash-values of *claim-derived-this-run*
          append (gethash key (derived-index-by-object idx)))))

(defun %derived-all ()
  "Every claim derived this run, across every relation: the unindexed
scan route's own union (GH #333)."
  (when *claim-derived-this-run*
    (loop for idx being the hash-values of *claim-derived-this-run*
          append (derived-index-all idx))))

(defun %scope-graphs ()
  "The stores in scope, own store first: *CLAIM-SCOPE*, or *GRAPH*
alone when it is NIL."
  (or *claim-scope* (list *graph*)))

(defun %scope-lookup (class-name slots value)
  "INDEX-LOOKUP over every store in scope, in scope order.  A FOREIGN
store whose schema does not carry CLASS-NAME contributes nothing --
QUERY-PRECONDITION-ERROR read as %PRODUCER-CANDIDATES reads it -- while
the own store (first) still signals, so a single-store goal keeps S1's
ill-typed refusal (ruling S3-R1, recon C1).  Same caveat as its sibling:
a wrong component count signals that condition too (%INDEX-BOUNDS,
index.lisp), so the swallow is safe only at these indexes' known
arities."
  (let ((graphs (%scope-graphs)))
    (append (index-lookup (first graphs) class-name slots value)
            (loop for g in (rest graphs)
                  append (handler-case
                             (index-lookup g class-name slots value)
                           (query-precondition-error () '()))))))

(defun %exclude-producers (candidates)
  "CANDIDATES with every claim matching a (PRODUCER . RELATION) pair of
*CLAIM-EXCLUDE-PRODUCERS* removed -- both halves, so the producer's
claims of any OTHER relation stay.  Applied only at CLAIM/7's index and
scan routes, never to a bound ?C (GH #333).  ASSOC is enough: one
producer writes one relation, so a producer names at most one pair."
  (if *claim-exclude-producers*
      (remove-if (lambda (claim)
                   (let ((pair (assoc (graph-db.spacetime:claim-producer
                                       claim)
                                      *claim-exclude-producers*
                                      :test #'string=)))
                     (and pair
                          (string= (cdr pair)
                                   (graph-db.spacetime:claim-relation
                                    claim)))))
                 candidates)
      candidates))

(defun %excluded-producer-p (producer)
  "PRODUCER is excluded for its own relation this round, so the index
cannot answer for it and *CLAIM-DERIVED-THIS-RUN* must (GH #333)."
  (and (assoc producer *claim-exclude-producers* :test #'string=) t))

(defun %derived-of-producer (producer)
  "This run's own derivation under PRODUCER: what CLAIM-PRODUCER/2
generates for an excluded producer in place of the index (GH #333)."
  (remove-if-not (lambda (c)
                   (string= producer
                            (graph-db.spacetime:claim-producer c)))
                 (%derived-all)))

(defun %keyword-string (keyword)
  "A keyword as the lowercase string the wire uses (spec §4)."
  (string-downcase (symbol-name keyword)))

(defun %namespace-keyword (x)
  "The keyword a namespace argument names, or NIL: a keyword passes; a
string resolves with FIND-SYMBOL, so a query cannot grow KEYWORD, and
only when it is that keyword's exact wire form -- \"HOST\" names nothing,
since lookup and unification must agree on the spelling; anything else is
NIL (spec §4)."
  (let ((v (var-deref x)))
    (cond ((keywordp v) v)
          ((stringp v)
           (let ((kw (find-symbol (string-upcase v) :keyword)))
             (and kw (string= v (%keyword-string kw)) kw)))
          (t nil))))

(defun %namespace-value (arg keyword)
  "The value a namespace argument unifies against for a claim whose
namespace is KEYWORD: KEYWORD when ARG already carries a keyword, else
the lowercase wire string; NIL for a unary claim's absent object
namespace.  Without the keyword case such an argument selects candidates
through the index and then unifies against nothing (spec §4)."
  (cond ((null keyword) nil)
        ((keywordp (var-deref arg)) keyword)
        (t (%keyword-string keyword))))

(defun %family-or-ill-typed (x)
  "The CLAIM-FAMILY the family argument names.  UNKNOWN-CLAIM-FAMILY is
what the runner reports as ill-typed client input, so it passes."
  (let ((v (%prolog-index-bound x)))
    (unless (symbolp v)
      (error 'graph-db.spacetime:unknown-claim-family :parent v))
    (graph-db.spacetime:claim-family v)))

(defmacro %yield ((var value) &body body)
  "Unify VAR with VALUE, run BODY (the continuation), undo."
  (let ((mark (gensym "TRAIL")))
    `(let ((,mark (fill-pointer *trail*)))
       (when (unify ,var ,value) ,@body)
       (undo-bindings ,mark))))

(defun %unify-claim (claim ?c ?sns ?skey ?rel ?ons ?okey family cont)
  "Bind every argument to CLAIM's fields and continue.  A claim outside
FAMILY yields nothing; a unary claim binds the object pair to NIL,
which is also why the object accessors are read under TYPEP -- only the
binary class has those slots.  Namespaces answer in the shape they were
asked in; see %NAMESPACE-VALUE."
  (when (typep claim (graph-db.spacetime:claim-family-parent family))
    (let* ((binary (typep claim
                          (graph-db.spacetime:claim-family-binary family)))
           (ons-key (and binary
                         (graph-db.spacetime:claim-object-namespace claim)))
           (sns (%namespace-value
                 ?sns (graph-db.spacetime:claim-subject-namespace claim)))
           (ons (%namespace-value ?ons ons-key))
           (okey (and binary
                      (graph-db.spacetime:claim-object-key claim))))
      (%yield (?c claim)
        (%yield (?sns sns)
          (%yield (?skey (graph-db.spacetime:claim-subject-key claim))
            (%yield (?rel (graph-db.spacetime:claim-relation claim))
              (%yield (?ons ons)
                (%yield (?okey okey)
                  (funcall cont))))))))))

;; CLAIM/7 is cost-unbounded only for a goal that reaches the COND's
;; last clause -- a per-goal property, so not
;; DECLARE-FUNCTOR-COST-UNBOUNDED, which classifies the whole functor
;; and would withhold CLAIM from free text entirely (GH #285).
(defun %unbound-claim-scan (family)
  "Every claim of FAMILY in every store in scope -- CLAIM/7's fallback,
the COND's last clause, not a nothing-bound special case.  Refused as
cost-unbounded when a resource bound is in effect, since %TICK cannot
preempt inside a family walk (GH #285) -- unless the caller accepted
that with :ALLOW-COST-UNBOUNDED, which SELECT now carries into a functor
body (GH #334).  Same test as %REFUSE-COST-UNBOUNDED's, so the static
refusal and this one answer to one value."
  (when (and (not *allow-cost-unbounded*)
             (or *inference-budget* *query-deadline*))
    (error 'prolog-cost-unbounded-error :functor 'claim/7))
  ;; :INCLUDE-SUBCLASSES-P defaults to T, so the parent covers unary and
  ;; binary.  :COLLECT-P is what materialises node bytes before a node
  ;; escapes the scan's read pin (vertex.lisp) -- not a style choice.
  ;; :VERTEX-TYPE keeps the scan typed, so a store lacking the family
  ;; visits nothing rather than reading live versions (recon B8).
  (let ((parent (graph-db.spacetime:claim-family-parent family)))
    (loop for g in (%scope-graphs)
          append (map-vertices #'identity g :vertex-type parent
                                            :collect-p t))))

(defun %scope-keys (family role namespace)
  "The keys filed under NAMESPACE in ROLE by FAMILY, over every store in
scope, merged in index order (the engine's collation, ascending).  Read
from the vocabulary index (CLAIM-KEYS, GH #350), so the cost is the
number of keys, never the number of claims.  Same store rule as
%SCOPE-LOOKUP: a foreign store lacking the family contributes nothing,
the own store still signals."
  (let ((graphs (%scope-graphs))
        (class (graph-db.spacetime:claim-family-parent family)))
    (flet ((keys (g) (values (graph-db.spacetime:claim-keys
                              g class namespace :role role))))
      (graph-db.spacetime::%merge-names
       (cons (keys (first graphs))
             (loop for g in (rest graphs)
                   collect (handler-case (keys g)
                             (query-precondition-error () '()))))
       nil))))

(defun %namespace-route (family role namespace rel emit)
  "GH #389: every claim whose ROLE endpoint sits in NAMESPACE, key by
key -- the keys from the vocabulary index, each key's claims from the
same subject or object index the bound-key routes read, in ascending
key order.  Bounded, unlike the walk: the work between two %TICKs is
one key's claims, so a budget preempts between keys.  This run's own
derivation follows, unfiltered; %UNIFY-CLAIM re-filters (GH #333)."
  (let ((parent (graph-db.spacetime:claim-family-parent family))
        (binary (graph-db.spacetime:claim-family-binary family)))
    (dolist (key (%scope-keys family role namespace))
      (mapc emit
            (%exclude-producers
             (ecase role
               (:subject
                (if rel
                    (%scope-lookup parent
                                   +claim-subject-relation-index-slots+
                                   (list namespace key rel))
                    (%scope-lookup parent +claim-subject-index-slots+
                                   (list namespace key))))
               (:object
                (%scope-lookup binary +claim-object-index-slots+
                               (list namespace key)))))))
    (mapc emit (%derived-all))))

(def-global-prolog-functor claim/7
    (?c ?family ?sns ?skey ?rel ?ons ?okey cont)
  "Claims of ?FAMILY (a parent class name) as facts: subject namespace
and key, relation, object namespace and key.  A namespace answers as the
lowercase wire string, or as the keyword when the argument was already
bound to one; a unary claim's object pair is NIL.  Generates from the
subject index when the subject is bound, the object index when the object
is, the producer index through CLAIM-PRODUCER/2 in the same body; a
namespace bound without its key generates key by key from the
vocabulary index (GH #389).  A bound namespace naming no keyword answers
empty; every other goal the routes miss reaches the COND's last clause,
which walks the family or is refused as cost-unbounded under a resource
bound (GH #285, spec §4).  The indexed routes and the walk read every
store in *CLAIM-SCOPE*, own store first, or *GRAPH* alone when it is
NIL; a bound ?C and the empty fast paths read no store at all (spec
§10, GH #332).  Solution order: docs/rules.md \"Solution order\"."
  (let* ((family (%family-or-ill-typed ?family))
         (parent (graph-db.spacetime:claim-family-parent family))
         (binary (graph-db.spacetime:claim-family-binary family))
         (c (%prolog-index-bound ?c))
         (sns-arg (%prolog-index-bound ?sns))
         (ons-arg (%prolog-index-bound ?ons))
         (sns (%namespace-keyword sns-arg))
         (ons (%namespace-keyword ons-arg))
         (skey (%prolog-index-bound ?skey))
         (okey (%prolog-index-bound ?okey))
         (rel (%prolog-index-bound ?rel)))
    ;; Each indexed/scan route unions in this run's own derivation,
    ;; keyed like the index it substitutes for (GH #333, C1);
    ;; over-inclusion is safe, %UNIFY-CLAIM re-filters every candidate.
    (flet ((emit (claim)
             (%unify-claim claim ?c ?sns ?skey ?rel ?ons ?okey family
                           cont)))
      (cond ((node-p c) (emit c))
            ((and sns skey rel)
             (mapc #'emit
                   (append (%exclude-producers
                            (%scope-lookup
                             parent +claim-subject-relation-index-slots+
                             (list sns skey rel)))
                           (%derived-by-subject rel (cons sns skey)))))
            ((and sns skey)
             (mapc #'emit
                   (append (%exclude-producers
                            (%scope-lookup parent
                                           +claim-subject-index-slots+
                                           (list sns skey)))
                           (%derived-by-subject nil (cons sns skey)))))
            ((and ons okey)
             (mapc #'emit
                   (append (%exclude-producers
                            (%scope-lookup binary
                                           +claim-object-index-slots+
                                           (list ons okey)))
                           (%derived-by-object (cons ons okey)))))
            ;; A bound namespace argument naming no keyword of this
            ;; image -- a name no claim was recorded under, a non-wire
            ;; spelling, a non-string: no solutions, and nothing
            ;; interned.  Not the walk below, which under the guard's
            ;; budget refuses instead (spec §4).
            ((and sns-arg (null sns)) nil)
            ((and ons-arg (null ons)) nil)
            ;; A namespace without its key: the vocabulary routes
            ;; (GH #389).  Subject first, so a goal binding both
            ;; namespaces and neither key enumerates subjects.
            (sns (%namespace-route family :subject sns rel #'emit))
            (ons (%namespace-route family :object ons nil #'emit))
            (t (mapc #'emit
                     (append (%exclude-producers
                              (%unbound-claim-scan family))
                             (%derived-all))))))))

(defun %unbound-p (x)
  "X is an unbound Prolog variable -- not a bound NIL, which
%PROLOG-INDEX-BOUND folds into the same value (S1's deferred gate,
docs/superpowers/decisions/2026-09-04-rules-s1-rulings.md)."
  ;; VAR-DEREF writes the place it is given; here that place is this
  ;; function's own parameter binding, as in PRINT-VAR (prologc.lisp).
  (var-p (var-deref x)))

(defun %claim-arg (x)
  "X's value when it is a node, else NIL -- every CLAIM-* filter fails on
NIL rather than signalling.  A node of another type is out of contract:
the accessors signal NO-APPLICABLE-METHOD, which the runner already
classifies as ill-typed input (spec §4)."
  (let ((v (%prolog-index-bound x)))
    (and v (node-p v) v)))

(def-global-prolog-functor claim-current/1 (?c cont)
  "True while ?C's transaction period is open -- a claim RETRACT-CLAIM has
closed is filtered out.  Claims are generated retracted-and-all, matching
CLAIMS-TOUCHING's default, so this is the goal that says \"still
believed\" (spec §4).  Trap: not \"currently valid\" -- a claim whose
validity a successor closed still passes; that is CLAIM-VALID-AT/2 with
the present instant (GH #388)."
  (let ((c (%claim-arg ?c)))
    (when (and c (graph-db.spacetime:claim-current-p c))
      (funcall cont))))

(defun %instant-arg (x)
  "X as a LOCAL-TIME timestamp: a timestamp passes, an RFC 3339 /
ISO-8601 string is parsed, an unbound or NIL X is NIL and fails the
goal.  Anything else -- a string that does not parse, a number -- is
QUERY-PRECONDITION-ERROR, which the guarded runner answers as ill-typed:
a bad instant must be visible, not an empty answer (GH #388)."
  (let ((v (%prolog-index-bound x)))
    (cond ((null v) nil)
          ((typep v 'local-time:timestamp) v)
          ((and (stringp v)
                (ignore-errors (local-time:parse-timestring v))))
          (t (error 'query-precondition-error
                    :reason (format nil "~S is not an RFC 3339 instant"
                                    v))))))

(defparameter +instant-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\T
    (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:nsec 9) #\Z)
  "How the extent functors spell an instant: RFC 3339, UTC, nanosecond
precision, 30 characters -- one spelling, so two answers order
lexically in time order (GH #388).")

(defun %instant-string (timestamp)
  (local-time:format-timestring nil timestamp :format +instant-format+
                                              :timezone local-time:+utc-zone+))

(defun %bound-edge (bound edge)
  "BOUND's :EARLIEST or :LATEST edge as an instant string, or NIL when
that edge is :UNBOUNDED."
  (let ((ts (ecase edge
              (:earliest (graph-db.spacetime:bound-earliest bound))
              (:latest (graph-db.spacetime:bound-latest bound)))))
    (and (typep ts 'local-time:timestamp) (%instant-string ts))))

(def-global-prolog-functor claim-valid-from/2 (?c ?from cont)
  "?C's validity start as an instant string (+INSTANT-FORMAT+) -- the
EARLIEST edge of a fuzzy start, NIL when that edge is unbounded; a claim
with no extent fails.  The outer envelope, as CLAIM-VALID-AT/2's
\"possibly contains\" is (GH #388)."
  (let* ((c (%claim-arg ?c))
         (e (and c (graph-db.spacetime:claim-extent c))))
    (when e
      (%yield (?from (%bound-edge (graph-db.spacetime:extent-start e)
                                  :earliest))
        (funcall cont)))))

(def-global-prolog-functor claim-valid-to/2 (?c ?to cont)
  "?C's validity end as an instant string -- the LATEST edge of a fuzzy
end, NIL for an open end; a claim with no extent fails (GH #388)."
  (let* ((c (%claim-arg ?c))
         (e (and c (graph-db.spacetime:claim-extent c))))
    (when e
      (%yield (?to (%bound-edge (graph-db.spacetime:extent-end e)
                                :latest))
        (funcall cont)))))

(def-global-prolog-functor claim-recorded-at/2 (?c ?at cont)
  "When ?C was recorded, as an instant string: the start of its
transaction extent.  NIL -- a solution -- when the claim predates the
transaction-time axis or that start is unbounded, so a change feed can
still list it (GH #388, #148)."
  (let ((c (%claim-arg ?c)))
    (when c
      (let ((at (graph-db.spacetime:claim-recorded-at c)))
        (%yield (?at (and (typep at 'local-time:timestamp)
                          (%instant-string at)))
          (funcall cont))))))

;; INSTANT</2 and friends compare two instants -- strings in any RFC
;; 3339 spelling, or timestamps -- by value, where </2 on two strings is
;; lexical and so only right for one fixed spelling (GH #388).  An
;; unbound side fails; a malformed one signals, as %INSTANT-ARG says.
(defmacro %def-instant-compare (name op doc)
  `(def-global-prolog-functor ,name (?a ?b cont)
     ,doc
     (let ((a (%instant-arg ?a))
           (b (%instant-arg ?b)))
       (when (and a b (,op a b))
         (funcall cont)))))

(%def-instant-compare instant</2 local-time:timestamp<
  "True when instant ?A is before ?B (GH #388).")
(%def-instant-compare instant>/2 local-time:timestamp>
  "True when instant ?A is after ?B (GH #388).")
(%def-instant-compare instant<=/2 local-time:timestamp<=
  "True when instant ?A is not after ?B (GH #388).")
(%def-instant-compare instant>=/2 local-time:timestamp>=
  "True when instant ?A is not before ?B (GH #388).")
(%def-instant-compare instant=/2 local-time:timestamp=
  "True when ?A and ?B name the same instant, whatever their spelling
\(GH #388).")

(def-global-prolog-functor claim-valid-at/2 (?c ?at cont)
  "True when ?C's validity extent possibly contains ?AT (an ISO-8601
string or a timestamp); a claim with no extent makes no validity
statement and never matches.  Shares CLAIMS-TOUCHING's predicate and
probe shape, so the two cannot diverge (spec §11)."
  (let ((c (%claim-arg ?c))
        (at (%instant-arg ?at)))
    (when (and c at
               (graph-db.spacetime::%claim-validity-touches-p
                c (graph-db.spacetime:make-instant
                   (graph-db.spacetime:exact-bound at))))
      (funcall cont))))

(def-global-prolog-functor claim-standing/2 (?c ?s cont)
  "?C's standing as the lowercase wire string (\"inferred\"), the same
shape a namespace answers in; a bound ?S filters instead (spec §4)."
  (let ((c (%claim-arg ?c)))
    (when c
      (%yield (?s (%keyword-string (graph-db.spacetime:claim-standing c)))
        (funcall cont)))))

(def-global-prolog-functor claim-relation/2 (?c ?r cont)
  "?C's relation, a canonical string; a bound ?R filters instead."
  (let ((c (%claim-arg ?c)))
    (when c
      (%yield (?r (graph-db.spacetime:claim-relation c))
        (funcall cont)))))

(def-global-prolog-functor claim-rule-version/2 (?c ?v cont)
  "?C's rule version, NIL when it has none -- NIL is a solution here, not
a failure, so a claim no rule wrote still answers."
  (let ((c (%claim-arg ?c)))
    (when c
      (%yield (?v (graph-db.spacetime:claim-rule-version c))
        (funcall cont)))))

(defun %producer-candidates (producer)
  "Every claim PRODUCER wrote in every store in scope, from the producer
index of each family registered in this image.  *CLAIM-FAMILIES* is
image-wide, not per graph, so a family a store's schema does not carry is
skipped -- own store included, unlike %SCOPE-LOOKUP, since the family
loop was always cross-family: that is what QUERY-PRECONDITION-ERROR means
here, not a fault to report (ruling S3-R1)."
  (let ((out '())
        (graphs (%scope-graphs)))
    (dolist (family (alexandria:hash-table-values
                     graph-db.spacetime::*claim-families*)
                    (nreverse out))
      (let ((parent (graph-db.spacetime:claim-family-parent family)))
        (dolist (graph graphs)
          (handler-case
              (dolist (c (index-lookup graph parent
                                       +claim-producer-index-slots+
                                       producer))
                (push c out))
            ;; Also the condition a wrong component count signals
            ;; (%INDEX-BOUNDS, index.lisp) -- safe only while this index
            ;; is arity 1 and PRODUCER a bare scalar; a multi-slot one
            ;; would read a shape error as "no candidates".
            (query-precondition-error () nil)))))))

(def-global-prolog-functor claim-producer/2 (?c ?p cont)
  "?C's producer.  With ?C unbound and ?P a producer name it generates
instead: every claim ?P wrote, across every family each store in
*CLAIM-SCOPE* indexes (spec §10, GH #332), or *GRAPH*'s when it is NIL --
write that goal BEFORE the CLAIM/7 goal it feeds, or ?C is bound by then
and this filters.  With neither bound there is no index to generate from
and no walk to fall back to, so the goal is refused as cost-unbounded
under a resource bound and answers nothing without one -- or with
:ALLOW-COST-UNBOUNDED, which buys that same silence rather than a walk,
there being none to buy.  Unbound means unbound: a ?C bound to NIL is a
bound non-node, so it filters and fails (spec §4, GH #334,
docs/rules.md)."
  (let ((c (%claim-arg ?c))
        (unbound (%unbound-p ?c))
        (p (%prolog-index-bound ?p)))
    (cond (c (%yield (?p (graph-db.spacetime:claim-producer c))
               (funcall cont)))
          ;; %CLAIM-ARG is NIL for a bound non-node too -- an explicit
          ;; NIL included -- and generating there is a whole
          ;; cross-family lookup that then unifies with nothing, past
          ;; %TICK's reach.  The index's answers pass the same
          ;; (producer . relation) exclusion CLAIM/7's routes apply,
          ;; and an excluded producer's own relation is answered from
          ;; this run's derivation instead (GH #333, C1, M3).
          ((and unbound (stringp p))
           (dolist (claim
                    (append (%exclude-producers (%producer-candidates p))
                            (when (%excluded-producer-p p)
                              (%derived-of-producer p))))
             (%yield (?c claim) (funcall cont))))
          ;; Nothing bound routes nowhere, so CLAIM/7's refusal rather
          ;; than silence.  A bound ?P that is a string naming no
          ;; producer still answers empty, as an unresolvable namespace
          ;; does; a ?P bound to NIL is not a producer name and lands
          ;; here.
          ((and unbound (null p)
                (not *allow-cost-unbounded*)
                (or *inference-budget* *query-deadline*))
           (error 'prolog-cost-unbounded-error
                  :functor 'claim-producer/2)))))

(def-global-prolog-functor rule-delta/2 (?c ?rel cont)
  "?C over *RULE-DELTA*'s claims of relation ?REL (a string); a bound
?C succeeds only when it is one of them.  Nothing without a delta
bound.  Withheld from free text (*PROLOG-EXCLUDED-PREDICATES*): a rule
body cannot name it, the fixpoint loop injects it (GH #333)."
  (let ((c (%claim-arg ?c))
        (unbound (%unbound-p ?c))
        (rel (%prolog-index-bound ?rel)))
    (when (and *rule-delta* (stringp rel))
      (let ((claims (gethash rel *rule-delta*)))
        (cond (c (when (member c claims) (funcall cont)))
              (unbound (dolist (claim claims)
                         (%yield (?c claim) (funcall cont)))))))))
