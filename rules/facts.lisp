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

;; CLAIM/7 is cost-unbounded only for a goal no index route covers -- a
;; per-goal property, so not DECLARE-FUNCTOR-COST-UNBOUNDED, which
;; classifies the whole functor and would withhold CLAIM from free text
;; entirely (GH #285).
(defun %unbound-claim-scan (graph family)
  "Every claim of FAMILY in GRAPH -- CLAIM/7's fallback for a goal no
index route covers, not a nothing-bound special case.  Refused as
cost-unbounded when a resource bound is in effect, since %TICK cannot
preempt inside a family walk (GH #285).  The refusal is unconditional:
:ALLOW-COST-UNBOUNDED is threaded through SELECT at query-compile time
and no special variable carries it into a functor body, so it cannot
reach here."
  (when (or *inference-budget* *query-deadline*)
    (error 'prolog-cost-unbounded-error :functor 'claim/7))
  ;; :INCLUDE-SUBCLASSES-P defaults to T, so the parent covers unary and
  ;; binary.  :COLLECT-P is what materialises node bytes before a node
  ;; escapes the scan's read pin (vertex.lisp) -- not a style choice.
  (let ((parent (graph-db.spacetime:claim-family-parent family)))
    (map-vertices #'identity graph :vertex-type parent :collect-p t)))

(def-global-prolog-functor claim/7
    (?c ?family ?sns ?skey ?rel ?ons ?okey cont)
  "Claims of ?FAMILY (a parent class name) as facts: subject namespace
and key, relation, object namespace and key.  A namespace answers as the
lowercase wire string, or as the keyword when the argument was already
bound to one; a unary claim's object pair is NIL.  Generates from the
subject index when the subject is bound, the object index when the object
is, the producer index through CLAIM-PRODUCER/2 in the same body.  A
goal no route covers walks the family, or is refused as cost-unbounded
when a resource bound is in effect (GH #285, spec §4)."
  (let* ((family (%family-or-ill-typed ?family))
         (parent (graph-db.spacetime:claim-family-parent family))
         (binary (graph-db.spacetime:claim-family-binary family))
         (g *graph*)
         (c (%prolog-index-bound ?c))
         (sns-arg (%prolog-index-bound ?sns))
         (ons-arg (%prolog-index-bound ?ons))
         (sns (%namespace-keyword sns-arg))
         (ons (%namespace-keyword ons-arg))
         (skey (%prolog-index-bound ?skey))
         (okey (%prolog-index-bound ?okey))
         (rel (%prolog-index-bound ?rel))
         (candidates
           (cond ((node-p c) (list c))
                 ((and sns skey rel)
                  (index-lookup g parent
                                +claim-subject-relation-index-slots+
                                (list sns skey rel)))
                 ((and sns skey)
                  (index-lookup g parent +claim-subject-index-slots+
                                (list sns skey)))
                 ((and ons okey)
                  (index-lookup g binary +claim-object-index-slots+
                                (list ons okey)))
                 ;; A bound namespace argument naming no keyword of this
                 ;; image -- a name no claim was recorded under, a
                 ;; non-wire spelling, a non-string: no solutions, and
                 ;; nothing interned.  Not the walk below, which under
                 ;; the guard's budget refuses instead (spec §4).
                 ((and sns-arg (null sns)) '())
                 ((and ons-arg (null ons)) '())
                 (t (%unbound-claim-scan g family)))))
    (dolist (claim candidates)
      (%unify-claim claim ?c ?sns ?skey ?rel ?ons ?okey family cont))))

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
believed\" (spec §4)."
  (let ((c (%claim-arg ?c)))
    (when (and c (graph-db.spacetime:claim-current-p c))
      (funcall cont))))

(defun %instant-arg (x)
  "X as a LOCAL-TIME timestamp: a timestamp passes, an ISO-8601 string is
parsed, everything else -- an unparsable string included -- is NIL, so a
malformed instant fails the goal instead of signalling."
  (let ((v (%prolog-index-bound x)))
    (cond ((typep v 'local-time:timestamp) v)
          ((stringp v) (ignore-errors (local-time:parse-timestring v)))
          (t nil))))

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

(defun %producer-candidates (graph producer)
  "Every claim PRODUCER wrote in GRAPH, from the producer index of each
family registered in this image.  *CLAIM-FAMILIES* is image-wide, not per
graph, so a family GRAPH's schema does not carry is skipped: that is what
QUERY-PRECONDITION-ERROR means here, not a fault to report."
  (let ((out '()))
    (dolist (family (alexandria:hash-table-values
                     graph-db.spacetime::*claim-families*)
                    (nreverse out))
      (let ((parent (graph-db.spacetime:claim-family-parent family)))
        (handler-case
            (dolist (c (index-lookup graph parent
                                     +claim-producer-index-slots+
                                     producer))
              (push c out))
          ;; Also the condition a wrong component count signals
          ;; (%INDEX-BOUNDS, index.lisp) -- safe only while this index
          ;; is arity 1 and PRODUCER a bare scalar; a multi-slot one
          ;; would read a shape error as "no candidates".
          (query-precondition-error () nil))))))

(def-global-prolog-functor claim-producer/2 (?c ?p cont)
  "?C's producer.  With ?C unbound and ?P a producer name it generates
instead: every claim ?P wrote, across every family this graph indexes.
With neither bound the goal fails: there is no index to generate from,
and the only alternative is a whole-store walk (spec §4)."
  (let ((c (%claim-arg ?c))
        (p (%prolog-index-bound ?p)))
    (cond (c (%yield (?p (graph-db.spacetime:claim-producer c))
               (funcall cont)))
          ;; %CLAIM-ARG is NIL for a bound non-node too, and generating
          ;; there is a whole cross-family lookup that then unifies with
          ;; nothing, past %TICK's reach.
          ((and (null (%prolog-index-bound ?c)) (stringp p))
           (dolist (claim (%producer-candidates *graph* p))
             (%yield (?c claim) (funcall cont)))))))
