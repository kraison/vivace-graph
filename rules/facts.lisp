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

(defun %namespace-keyword (x)
  "The keyword a namespace argument names, or NIL: a string resolves
with FIND-SYMBOL so a query cannot grow KEYWORD; a keyword passes; an
unbound variable or anything else is NIL (spec §4)."
  (let ((v (var-deref x)))
    (cond ((keywordp v) v)
          ((stringp v) (find-symbol (string-upcase v) :keyword))
          (t nil))))

(defun %keyword-string (keyword)
  "A keyword as the lowercase string the wire uses (spec §4)."
  (string-downcase (symbol-name keyword)))

(defun %bound (x)
  "X's value when it is bound to a non-variable, else NIL.  An explicit
NIL argument and an unbound one are one case here, as they are for an
index range bound."
  (%prolog-index-bound x))

(defun %family-or-ill-typed (x)
  "The CLAIM-FAMILY the family argument names.  UNKNOWN-CLAIM-FAMILY is
what the runner reports as ill-typed client input, so it passes."
  (let ((v (%bound x)))
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
binary class has those slots."
  (when (typep claim (graph-db.spacetime:claim-family-parent family))
    (let* ((binary (typep claim
                          (graph-db.spacetime:claim-family-binary family)))
           (ons (and binary
                     (%keyword-string
                      (graph-db.spacetime:claim-object-namespace claim))))
           (okey (and binary
                      (graph-db.spacetime:claim-object-key claim))))
      (%yield (?c claim)
        (%yield (?sns (%keyword-string
                       (graph-db.spacetime:claim-subject-namespace claim)))
          (%yield (?skey (graph-db.spacetime:claim-subject-key claim))
            (%yield (?rel (graph-db.spacetime:claim-relation claim))
              (%yield (?ons ons)
                (%yield (?okey okey)
                  (funcall cont))))))))))

;; CLAIM/7 is cost-unbounded only in the nothing-bound case -- a per-goal
;; property, so not DECLARE-FUNCTOR-COST-UNBOUNDED, which classifies the
;; whole functor and would withhold CLAIM from free text entirely
;; (GH #285).  The walk that is legal without a resource bound lands in
;; S1 task 2.
(defun %unbound-claim-scan (family)
  "Refuse a nothing-bound CLAIM/7 as cost-unbounded: %TICK cannot
preempt inside a family walk (GH #285)."
  (declare (ignore family))
  (error 'prolog-cost-unbounded-error :functor 'claim/7))

(def-global-prolog-functor claim/7
    (?c ?family ?sns ?skey ?rel ?ons ?okey cont)
  "Claims of ?FAMILY (a parent class name) as facts: subject namespace
and key, relation, object namespace and key -- namespaces as strings,
NIL object pair for a unary claim.  Generates from the subject index
when the subject is bound, the object index when the object is, the
producer index through CLAIM-PRODUCER/2 in the same body; with nothing
bound under a resource bound it is refused as cost-unbounded (GH #285),
and without a bound it walks the family (spec §4)."
  (let* ((family (%family-or-ill-typed ?family))
         (parent (graph-db.spacetime:claim-family-parent family))
         (binary (graph-db.spacetime:claim-family-binary family))
         (g *graph*)
         (c (%bound ?c))
         (sns-arg (%bound ?sns))
         (ons-arg (%bound ?ons))
         (sns (%namespace-keyword sns-arg))
         (ons (%namespace-keyword ons-arg))
         (skey (%bound ?skey))
         (okey (%bound ?okey))
         (rel (%bound ?rel))
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
                 ;; A namespace no claim was ever recorded under names no
                 ;; keyword: no solutions, and nothing interned (spec §4).
                 ((and (stringp sns-arg) (null sns)) '())
                 ((and (stringp ons-arg) (null ons)) '())
                 (t (%unbound-claim-scan family)))))
    (dolist (claim candidates)
      (%unify-claim claim ?c ?sns ?skey ?rel ?ons ?okey family cont))))
