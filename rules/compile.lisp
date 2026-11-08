;;;; rules/compile.lisp -- a rule as text, and text as a compiled rule
;;;; (spec §5-§6, GH #331).  Every spacetime and engine symbol is written
;;;; qualified: see package.lisp's header for why nothing is imported.
;;;;
;;;; The RULE class and its accessors are made per store by
;;;; DEF-RULES-SCHEMA (schema.lisp), so this file cold-compiles with
;;;; forward-reference style warnings; they go away once a store has
;;;; declared the schema.

(in-package #:graph-db.rules)

(defstruct (rule-spec (:constructor %make-rule-spec))
  "One rule as text: the slots of a RULE record, whichever way it came
in.  FAMILY is the parent class's name, lowercase; SOURCE is :STORED or
:DEF-RULE."
  name version family head body (extent-policy :premises) (enabled t)
  source)

(defstruct (production-spec (:include rule-spec)
                            (:constructor %make-production-spec))
  "A production as text (forward-rules design §7): a RULE-SPEC whose
HEAD is empty and whose BODY is the author's TEXT -- the FIRING head and
the INSTANTIATION-KEY goal are built from fresh symbols at compile
(%GUARD-PRODUCTION, #466) -- with SALIENCE, the relations the action
WRITES, ACTION the name its function is registered under
(*PRODUCTION-ACTIONS*) and SOURCE-FAMILY the family the author named.
SOURCE is :STORED or :DEF-PRODUCTION."
  (salience 0) (writes '()) action source-family text)

(defvar *def-productions* (make-hash-table :test 'equal)
  "DEF-PRODUCTION's registry, name -> PRODUCTION-SPEC (unsynthesised).")

(defvar *production-actions* (make-hash-table :test 'equal)
  "Action name -> function of (EVENT MATCH); DEF-PRODUCTION registers
its action under the production's name, a stored record names one.")

(defvar *def-rules* (make-hash-table :test 'equal)
  "DEF-RULE's registry, name -> RULE-SPEC: rules that live in the image
rather than in a store (spec §5).  Image-wide, so RUN-RULES filters it
by the store's families (ruling P8).")

(defvar *def-rules-stamp* 0
  "Bumped by every DEF-RULE and UNDEF-RULE; a forward state's compiled
rules are current while their stamp matches (rules/forward.lisp).")

(defun %family-string (family)
  "FAMILY -- a symbol or a string -- as the lowercase name a RULE stores."
  (string-downcase (string family)))

(defun %register-def-rule (name &key version family head body
                                     (extent-policy :premises)
                                     (enabled t))
  (check-type name string)
  (check-type version string)
  (check-type head string)
  (check-type body string)
  (incf *def-rules-stamp*)
  (setf (gethash name *def-rules*)
        (%make-rule-spec :name name :version version
                         :family (%family-string family)
                         :head head :body body
                         :extent-policy extent-policy :enabled enabled
                         :source :def-rule))
  name)

(defmacro def-rule (name &key version family head body
                              (extent-policy :premises) (enabled t))
  "Register a rule in the image, not in a store (spec §5): the same
producer rule/NAME, the same compile and RUN-RULE as a stored rule,
without a record.  FAMILY is the parent class symbol, unevaluated.
Compiled per store when run, not here -- the cycle check needs the
store's other rules.  Returns NAME."
  `(%register-def-rule ,name :version ,version :family ',family
                       :head ,head :body ,body
                       :extent-policy ,extent-policy :enabled ,enabled))

(defun undef-rule (name)
  "Forget the DEF-RULE NAME; T when there was one."
  (incf *def-rules-stamp*)
  (and (remhash name *def-rules*) t))

(defun %register-def-production (name &key version family body
                                           (salience 0) writes action)
  (check-type name string)
  (check-type version string)
  (check-type body string)
  (check-type salience real)
  (check-type writes list)
  (incf *def-rules-stamp*)
  (setf (gethash name *production-actions*)
        (etypecase action
          (function action)
          (symbol action)))
  (setf (gethash name *def-productions*)
        (%make-production-spec :name name :version version
                               :family "firing" :source-family
                               (%family-string family)
                               :head "" :body body :text body
                               :extent-policy :none :enabled t
                               :salience salience :writes writes
                               :action name :source :def-production))
  name)

(defmacro def-production (name &key version family body (salience 0)
                                    writes action)
  "Register a production in the image (forward-rules design §7): BODY
is a rule body over FAMILY's claims (the parent class symbol,
unevaluated); when it matches, a FIRING claim keyed by the premises'
identities is derived and ACTION, a function of (EVENT MATCH) with
EVENT :ADDED or :LOST and MATCH a plist (:KEY :PREMISES :BINDINGS
:FIRING; MATCH-ROW renders it), runs from the agenda in SALIENCE order
(higher first).
WRITES lists the relations the action may write; one it reads too is
a cycle, refused.  Returns NAME."
  `(%register-def-production ,name :version ,version :family ',family
                             :body ,body :salience ,salience
                             :writes ,writes :action ,action))

(defun undef-production (name)
  "Forget the DEF-PRODUCTION NAME and its action; T when there was one."
  (incf *def-rules-stamp*)
  (remhash name *production-actions*)
  (and (remhash name *def-productions*) t))

(defun find-def-production (name)
  "The DEF-PRODUCTION NAME's PRODUCTION-SPEC, or NIL."
  (values (gethash name *def-productions*)))

(defun find-def-rule (name)
  "The DEF-RULE NAME's RULE-SPEC, or NIL."
  (values (gethash name *def-rules*)))

(defun rule-spec-of (thing &optional graph)
  "THING as a RULE-SPEC: a RULE record is read into one, a spec passes,
and a string names a rule of GRAPH's store, else a DEF-RULE, in that
order -- the same order %RESOLVE-RULE (run.lisp) uses -- so a caller
with only the name a prior COMPILE-RULE stored can re-compile it
(GH #333)."
  (etypecase thing
    (rule-spec thing)
    (production (%production-spec-of-record thing))
    (rule (%make-rule-spec :name (rule-name thing)
                           :version (rule-version thing)
                           :family (%family-string (rule-family thing))
                           :head (rule-head thing) :body (rule-body thing)
                           :extent-policy (rule-extent-policy thing)
                           :enabled (rule-enabled thing)
                           :source :stored))
    (string
     (let ((stored (and graph (%graph-declares-p graph 'rule)
                        (first (graph-db:index-lookup
                                graph 'rule '(name) thing)))))
       (cond ((and stored (not (graph-db:deleted-p stored)))
              (rule-spec-of stored))
             ((find-def-rule thing))
             ((%stored-production graph thing))
             ((find-def-production thing))
             (t (error "No rule named ~S in ~(~S~) or the image."
                       thing
                       (and graph (graph-db:graph-name graph)))))))))

(define-condition rule-compile-error (graph-db:constraint-violation)
  ((rule :initarg :rule :reader rule-compile-error-rule)
   (reason :initarg :reason :reader rule-compile-error-reason))
  (:report (lambda (c s)
             (format s "Rule ~A does not compile: ~A"
                     (rule-compile-error-rule c)
                     (rule-compile-error-reason c))))
  (:documentation "A rule the compiler refused (spec §6).  A
CONSTRAINT-VIOLATION, because a RULE write is refused at commit with it
(ruling P3)."))

(defun %refuse (spec format-string &rest args)
  (error 'rule-compile-error
         :rule (rule-spec-name spec)
         :reason (apply #'format nil format-string args)))

(defstruct (compiled-rule (:constructor %make-compiled-rule))
  "A rule ready to run: the guarded goals and what the head derives.
HEAD-* are the head's argument terms -- a keyword (namespace), a string
(key or relation), NIL, or a body variable.  VARS is SELECT's variable
list, BODY-VARS the variables the body text names in first-appearance
order, none synthesised (a match's :BINDINGS, #466), PREMISE-VARS the
?c of every body CLAIM/7 goal, READS the
relations the body reads -- never :ANY, which COMPILE-RULE refuses
before this struct is built -- FAMILY-READS the families it reads
wide, every relation of a literal family under a literal namespace
(#469), and NEGATIVE-READS the subset of READS read under a NOT,
which orders this rule's stratum after the one deriving them
(%STRATUM-READS, run.lisp, GH #333).  STRATUM is the sorted names of
the rules whose head relations are mutually reachable with this one's
(its own name always), STRATUM-RELATIONS the sorted relations they
derive; a rule is recursive when a body CLAIM/7 goal has
an unbound ?c and a relation in STRATUM-RELATIONS (%RECURSIVE-GOAL-P,
run.lisp, GH #333).  THRESHOLDS is one entry per comparison of NOW/1's
instant: (GOAL DIRECTION CLAIM-VAR SOURCE OFFSET BOUND-VAR), DIRECTION
:GAIN or :LOSS, SOURCE the claim-instant functor's name, OFFSET its
ISO duration or NIL, BOUND-VAR the variable the comparison reads
(forward-rules design §5.2; %THRESHOLDS)."
  spec family relation
  head-c head-sns head-skey head-ons head-okey unary-p
  vars body-vars premise-vars goals reads (family-reads '())
  negative-reads stratum stratum-relations (thresholds '()))

;;; Reading the text

(defun %variable-p (x)
  (graph-db::variable-p x))

(defun %term-label (x)
  "X as the rule's author wrote it, for a refusal: a symbol bare and
downcased, since ~S would spell out the guard's scratch package.  The
guard's own %TERM-LABEL (query/guard.lisp) reads the same."
  (if (and x (symbolp x))
      (string-downcase (symbol-name x))
      (format nil "~S" x)))

(defun %engine-goal-p (goal name arity)
  "GOAL is a call of the functor NAME/ARITY, by the canonical symbol the
guard rebuilds a head into: NAME interned where S1 homed CLAIM/7
(rules/facts.lisp), so this follows the functors if they ever move
(recon C3)."
  (and (consp goal)
       (symbolp (first goal))
       (eq (symbol-package (first goal))
           (symbol-package 'graph-db:claim/7))
       (string= (symbol-name (first goal)) name)
       (= arity (1- (length goal)))))

(defun %guard-text (spec text graph)
  (handler-case
      (let ((graph-db.query:*guarding-rule-text* t))
        (graph-db.query:guard-query-text text graph))
    (graph-db.query:prolog-guard-error (c)
      (%refuse spec "~A" (graph-db.query:prolog-guard-error-reason c)))))

(defun %guard (spec graph)
  "HEAD then BODY through the guard as one text, so a variable shared
between them reads as one symbol: (VALUES VARS GOALS SYNTHESISED).
Rule text may say NOW (*GUARDING-RULE-TEXT*).  A production's head and
key goal are not text: %GUARD-PRODUCTION builds them from fresh
symbols, listed as SYNTHESISED, so its body may name any variable
(#466)."
  (if (production-spec-p spec)
      (%guard-production spec graph)
      (multiple-value-bind (vars goals)
          (%guard-text spec (format nil "~A~%~A" (rule-spec-head spec)
                                    (rule-spec-body spec))
                       graph)
        (values vars goals '()))))

(defun %guard-production (spec graph)
  "SPEC's body text guarded, then the FIRING head -- subject key the
instantiation key, object the key in the clear (#471) -- and the
INSTANTIATION-KEY goal over the premises, built on fresh symbols
(forward-rules design §7).  A body with no top-level claim/7 goal or
with more than eight premises is refused here, in those words."
  (multiple-value-bind (vars goals)
      (%guard-text spec (production-spec-text spec) graph)
    (let ((premises (%premise-vars goals)))
      (when (null premises)
        (%refuse spec "a production body needs a claim/7 goal"))
      (when (> (length premises) 8)
        (%refuse spec "a production body has ~D premises; at most eight"
                 (length premises)))
      (let ((?c (make-symbol "?%C"))
            (?key (make-symbol "?%KEY"))
            (?clear (make-symbol "?%CLEAR")))
        (values vars
                (cons (list 'graph-db::claim ?c 'firing "production" ?key
                            (rule-spec-name spec) "instantiation" ?clear)
                      (append goals
                              (list (list* 'graph-db::instantiation-key
                                           ?key ?clear premises))))
                (list ?c ?key ?clear))))))

(defun %head-goal-count (spec graph)
  "How many goals the HEAD text alone is -- the guard refuses an empty
text, which here is a refusal in its own words."
  (handler-case
      (length (nth-value 1 (graph-db.query:guard-query-text
                            (rule-spec-head spec) graph)))
    (graph-db.query:prolog-guard-error (c)
      (%refuse spec "head: ~A"
               (graph-db.query:prolog-guard-error-reason c)))))

(defun %check-no-bare-var (spec form)
  "Refuse a bare ? anywhere in FORM: read into the guard's scratch
package it is one NAMED variable shared by every goal that writes it,
not the engine's anonymous one (recon A10).  The head is scanned by
this on its own -- passing it to %BODY-VARIABLES would make its
variables \"bound by the body\"."
  (labels ((walk (x)
             (cond ((and (%variable-p x) (string= (symbol-name x) "?"))
                    (%refuse spec "a bare ? is one shared variable ~
here, not an anonymous one: name it"))
                   ((consp x) (walk (car x)) (walk (cdr x))))))
    (walk form)))

(defun %body-variables (spec goals)
  "Every ?variable in GOALS, once each; a bare ? is refused."
  (%check-no-bare-var spec goals)
  (let ((vars '()))
    (labels ((walk (x)
               (cond ((%variable-p x) (pushnew x vars))
                     ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk goals))
    vars))

(defun %head-namespace (spec term body-vars what)
  "A head namespace term: a canonical string is interned as its keyword
now (rules are validated content, so this growth is bounded by the
rules that compile); a body variable passes; anything else refuses."
  (cond ((and (stringp term)
              (graph-db.spacetime:canonical-relation-p term))
         (intern (string-upcase term) :keyword))
        ((stringp term)
         (%refuse spec "~A namespace ~S is not canonical ([a-z0-9-]+)"
                  what term))
        ((and (%variable-p term) (member term body-vars)) term)
        ((%variable-p term)
         (%refuse spec "~A namespace ~(~A~) is not bound by the body"
                  what term))
        (t (%refuse spec "~A namespace must be a string or a body ~
variable, not ~(~A~)" what term))))

(defun %head-key (spec term body-vars what)
  (cond ((stringp term) term)
        ((and (%variable-p term) (member term body-vars)) term)
        ((%variable-p term)
         (%refuse spec "~A key ~(~A~) is not bound by the body"
                  what term))
        (t (%refuse spec "~A key must be a string or a body variable, ~
not ~(~A~)" what term))))

(defun %check-not-premise (spec term premise-vars what)
  "Refuse a head namespace or key bound to a body premise's ?c: that
variable names a claim NODE, which is never a namespace or a key, so
the rule is refused at compile rather than at run (#331)."
  (when (and (%variable-p term) (member term premise-vars))
    (%refuse spec "the head's ~A ~A is a body premise, and a premise ~
names a claim, not a namespace or a key" what (%term-label term))))

(defun %parse-head (spec head body-vars premise-vars)
  "The head's seven arguments checked against spec §6; returns a plist
of the COMPILED-RULE head slots."
  (unless (%engine-goal-p head "CLAIM" 7)
    (%refuse spec "the head must be a claim/7 pattern, not ~S"
             (if (consp head)
                 (string-downcase (string (first head)))
                 head)))
  (destructuring-bind (?c fam sns skey rel ons okey) (rest head)
    (unless (%variable-p ?c)
      (%refuse spec "the head's ?c must be an unbound variable, not ~A"
               (%term-label ?c)))
    (when (member ?c body-vars)
      (%refuse spec "the head's ?c ~(~A~) must not appear in the body"
               ?c))
    (let ((family (handler-case (graph-db.spacetime:claim-family fam)
                    (graph-db.spacetime:unknown-claim-family ()
                      (%refuse spec "~(~A~) is not a claim family"
                               fam)))))
      (unless (string-equal (symbol-name fam) (rule-spec-family spec))
        (%refuse spec "the head's family ~(~A~) is not the rule's ~
family ~A" fam (rule-spec-family spec)))
      (unless (and (stringp rel)
                   (graph-db.spacetime:canonical-relation-p rel))
        (%refuse spec "the head's relation must be a canonical string, ~
not ~A" (%term-label rel)))
      (%check-not-premise spec sns premise-vars "subject namespace")
      (%check-not-premise spec skey premise-vars "subject key")
      (%check-not-premise spec ons premise-vars "object namespace")
      (%check-not-premise spec okey premise-vars "object key")
      (let ((unary (and (null ons) (null okey))))
        (when (and (not unary) (or (null ons) (null okey)))
          (%refuse spec "the head's object pair must be both NIL ~
(a unary claim) or both given"))
        (list :family family :relation rel :head-c ?c
              :head-sns (%head-namespace spec sns body-vars "subject")
              :head-skey (%head-key spec skey body-vars "subject")
              :head-ons (and (not unary)
                             (%head-namespace spec ons body-vars "object"))
              :head-okey (and (not unary)
                              (%head-key spec okey body-vars "object"))
              :unary-p unary)))))

(defun %claim-read-goal-p (goal)
  "GOAL reads a relation: a CLAIM/7 or a CLAIM-ABSENT/7 call, the two
share argument positions (D2)."
  (or (%engine-goal-p goal "CLAIM" 7)
      (%engine-goal-p goal "CLAIM-ABSENT" 7)))

(defun %generator-goal-p (goal)
  "(claim-producer ?v \"p\"): a generator, to run first (ruling P5)."
  (and (%engine-goal-p goal "CLAIM-PRODUCER" 2)
       (%variable-p (second goal))
       (stringp (third goal))))

(defun %order-body (goals)
  (append (remove-if-not #'%generator-goal-p goals)
          (remove-if #'%generator-goal-p goals)))

(defun %family-wide-goal-p (goal)
  "GOAL reads a family wide (#469): a CLAIM/7 or CLAIM-ABSENT/7 goal
whose relation is a variable under a literal family and a literal
subject namespace -- the vocabulary route, bounded by the namespace's
keys (GH #389)."
  (and (%claim-read-goal-p goal)
       (%variable-p (sixth goal))
       (symbolp (third goal)) (third goal) (not (%variable-p (third goal)))
       (stringp (fourth goal))))

(defun %goal-family (goal)
  "GOAL's literal family as the lowercase string a RULE-SPEC's FAMILY
holds."
  (%family-string (third goal)))

(defun %body-reads (goals)
  "(VALUES RELATIONS FAMILIES): the relations the body's CLAIM/7 and
CLAIM-ABSENT/7 goals read, and the families its family-wide goals read
(#469); :ANY when a goal leaves the relation unbound any other way
(ruling P6)."
  (let ((reads '())
        (families '()))
    (dolist (goal goals (values (nreverse reads) (nreverse families)))
      (when (%claim-read-goal-p goal)
        ;; The relation is CLAIM/7's fifth ARGUMENT, i.e. (sixth goal):
        ;; (claim ?c family sns skey rel ons okey).
        (let ((rel (sixth goal)))
          (cond ((stringp rel) (pushnew rel reads :test #'string=))
                ((%family-wide-goal-p goal)
                 (pushnew (%goal-family goal) families :test #'string=))
                (t (return-from %body-reads :any))))))))

(defun %write-relation (write)
  "A production write's relation: WRITE is a relation string or
(\"family\" \"relation\") (#469)."
  (if (consp write) (second write) write))

(defun %write-family (write)
  "A production write's declared family, or NIL for a bare relation."
  (and (consp write) (first write)))

(defun %family-reads-marker-p (x)
  (and (consp x) (eq (car x) :family)))

(defun %with-family-markers (reads families)
  "READS plus one (:FAMILY . name) marker per family read wide, the
form %STRATA expands once every head in scope is known."
  (append reads (mapcar (lambda (f) (cons :family f)) families)))

(defun %mentions-claim-after-p (goals)
  "A CLAIM-AFTER/9 goal anywhere in GOALS, at any nesting: the reads
and strata analyses recognise CLAIM/7 by name, so a rule writes that
and the comparison (#451)."
  (and (consp goals)
       (or (%engine-goal-p goals "CLAIM-AFTER" 9)
           (some #'%mentions-claim-after-p goals))))

(defun %not-goal-p (goal)
  "(not G): the guard admits only a full goal as G."
  (and (consp goal) (symbolp (first goal))
       (string= (symbol-name (first goal)) "NOT")
       (= 2 (length goal))))

(defun %negative-reads (goals)
  "The relations read under a NOT, at any nesting; :ANY when one is
unbound there (GH #333)."
  (let ((reads '()))
    (labels ((walk (goal negated)
               (cond ((%not-goal-p goal) (walk (second goal) t))
                     ((and negated (%claim-read-goal-p goal))
                      (let ((rel (sixth goal)))
                        (if (stringp rel)
                            (pushnew rel reads :test #'string=)
                            (return-from %negative-reads :any))))
                     ((and (consp goal) (rest goal))
                      (dolist (sub (rest goal))
                        (when (consp sub) (walk sub negated)))))))
      (dolist (goal goals) (walk goal nil)))
    (nreverse reads)))

(defun %premise-vars (goals)
  "The ?c of every top-level CLAIM/7 and CLAIM-ABSENT/7 goal, once
each, in first-appearance order."
  (loop for goal in goals
        when (and (%claim-read-goal-p goal)
                  (%variable-p (second goal)))
          collect (second goal) into vars
        finally (return (remove-duplicates vars :from-end t))))

;;; Thresholds (forward-rules design §5.2)

(defparameter *instant-source-functors*
  '("CLAIM-VALID-FROM" "CLAIM-VALID-TO" "CLAIM-RECORDED-AT"
    "CLAIM-TOUCHED-AT" "CLAIM-RETRACTED-AT")
  "The functors whose second argument is an instant of a claim: what
NOW/1's instant may be compared with, directly or through
INSTANT-OFFSET/3.")

(defun %mentions-p (var form)
  (or (eq var form)
      (and (consp form)
           (or (%mentions-p var (car form)) (%mentions-p var (cdr form))))))

(defun %instant-binder (var goals)
  "The goal binding VAR to a claim instant: an INSTANT-OFFSET/3 whose
third argument is VAR, or a source functor whose second is."
  (find-if (lambda (g)
             (or (and (%engine-goal-p g "INSTANT-OFFSET" 3)
                      (eq (fourth g) var))
                 (and (some (lambda (n) (%engine-goal-p g n 2))
                            *instant-source-functors*)
                      (eq (third g) var))))
           goals))

(defun %threshold-source (spec goal var goals premise-vars)
  "(CLAIM-VAR SOURCE OFFSET) for VAR, the instant GOAL compares NOW
with; refused when it is not a premise claim's instant."
  (flet ((refuse ()
           (%refuse spec "~A compares now with ~A, which is not a ~
premise claim's instant: now may be compared only with ~
claim-valid-from, claim-valid-to, claim-recorded-at, claim-touched-at ~
or claim-retracted-at of a premise, or instant-offset over one of them"
                    (%term-label goal) (%term-label var))))
    (let ((b (%instant-binder var goals)))
      (when (null b) (refuse))
      (multiple-value-bind (source offset)
          (if (%engine-goal-p b "INSTANT-OFFSET" 3)
              (let ((src (%instant-binder (second b) goals)))
                (when (or (null src)
                          (%engine-goal-p src "INSTANT-OFFSET" 3)
                          (not (stringp (third b))))
                  (refuse))
                (values src (third b)))
              (values b nil))
        (unless (member (second source) premise-vars)
          (refuse))
        (list (second source) (symbol-name (first source)) offset)))))

(defun %thresholds (spec body premise-vars)
  "BODY's threshold entries (COMPILED-RULE-THRESHOLDS), or NIL when it
never says NOW.  Every other use of NOW's variable is refused."
  (let ((nows (remove-if-not (lambda (g) (%engine-goal-p g "NOW" 1))
                             body)))
    (when (null nows)
      (return-from %thresholds '()))
    (when (rest nows)
      (%refuse spec "a body says now once"))
    (let ((?t (second (first nows)))
          (entries '()))
      (unless (%variable-p ?t)
        (%refuse spec "now takes a variable"))
      (dolist (goal body (nreverse entries))
        (unless (eq goal (first nows))
          (when (%mentions-p ?t goal)
            (let* ((op (and (consp goal) (symbolp (first goal))
                            (= 3 (length goal))
                            (symbol-name (first goal))))
                   (a (second goal)) (b (third goal))
                   (direction
                     (cond ((not (member op '("INSTANT<" "INSTANT<="
                                             "INSTANT>" "INSTANT>=")
                                         :test #'equal))
                            nil)
                           ((and (eq a ?t) (%variable-p b) (not (eq b ?t)))
                            (if (member op '("INSTANT>" "INSTANT>=")
                                        :test #'equal)
                                :gain :loss))
                           ((and (eq b ?t) (%variable-p a) (not (eq a ?t)))
                            (if (member op '("INSTANT<" "INSTANT<=")
                                        :test #'equal)
                                :gain :loss))
                           (t nil))))
              (unless direction
                (%refuse spec "~A: now's instant may only be ordered ~
against a premise claim's instant with instant<, instant<=, instant> ~
or instant>=" (%term-label goal)))
              (let ((var (if (eq a ?t) b a)))
                (push (append (list goal direction)
                              (%threshold-source spec goal var body
                                                 premise-vars)
                              (list var))
                      entries)))))))))

;;; The rule set a compile is checked against

(defun %graph-declares-p (graph type-name)
  "GRAPH's schema declares the vertex type TYPE-NAME, compared by name so
a family written as a lowercase string matches its class symbol."
  (and (member type-name
               (graph-db.query:schema-type-names graph :vertex)
               :test #'string-equal)
       t))

(defun %stored-rules (graph &key view)
  "Every RULE record in GRAPH, as specs.  With VIEW (a commit view),
records the transaction writes replace or remove their committed
version, so the set is the store as it will be after the commit.  A
store that never ran DEF-RULES-SCHEMA has no RULE type at all and
answers NIL rather than erring in MAP-VERTICES (T4-R2)."
  (unless (%graph-declares-p graph 'rule)
    (return-from %stored-rules '()))
  (let* ((committed (graph-db:map-vertices #'identity graph
                                           :vertex-type 'rule
                                           :collect-p t))
         (nodes (if view
                    (loop for r in committed
                          for n = (graph-db:view-node view (graph-db:id r))
                          when n collect n)
                    committed)))
    (when view
      (dolist (w (graph-db:view-writes view))
        (let ((n (graph-db:view-node view (graph-db:id w))))
          (when (and n (typep n 'rule)
                     (null (graph-db:view-old-node view n))
                     (not (find (graph-db:id n) nodes
                                :key #'graph-db:id :test #'equalp)))
            (push n nodes)))))
    (mapcar #'rule-spec-of
            (remove-if #'graph-db:deleted-p nodes))))

(defun %production-spec-of-record (record)
  "A stored PRODUCTION record as an unsynthesised PRODUCTION-SPEC."
  (%make-production-spec :name (production-name record)
                         :version (production-version record)
                         :family "firing"
                         :source-family (%family-string
                                         (production-family record))
                         :head "" :body (production-body record)
                         :text (production-body record)
                         :extent-policy :none :enabled t
                         :salience (production-salience record)
                         :writes (production-writes record)
                         :action (production-action record)
                         :source :stored))

(defun find-production (graph name)
  "GRAPH's stored PRODUCTION record named NAME, or NIL -- a deleted
record is NIL (#466).  A DEF-PRODUCTION lives in the image:
FIND-DEF-PRODUCTION."
  (let ((stored (and graph (%graph-declares-p graph 'production)
                     (first (graph-db:index-lookup
                             graph 'production '(name) name)))))
    (and stored (not (graph-db:deleted-p stored)) stored)))

(defun %stored-production (graph name)
  "GRAPH's stored production NAME as a spec, or NIL."
  (let ((stored (find-production graph name)))
    (and stored (%production-spec-of-record stored))))

(defun %stored-productions (graph &key view)
  "Every PRODUCTION record in GRAPH as specs, as %STORED-RULES reads
rules."
  (unless (%graph-declares-p graph 'production)
    (return-from %stored-productions '()))
  (let* ((committed (graph-db:map-vertices #'identity graph
                                           :vertex-type 'production
                                           :collect-p t))
         (nodes (if view
                    (loop for r in committed
                          for n = (graph-db:view-node view (graph-db:id r))
                          when n collect n)
                    committed)))
    (when view
      (dolist (w (graph-db:view-writes view))
        (let ((n (graph-db:view-node view (graph-db:id w))))
          (when (and n (typep n 'production)
                     (null (graph-db:view-old-node view n))
                     (not (find (graph-db:id n) nodes
                                :key #'graph-db:id :test #'equalp)))
            (push n nodes)))))
    (mapcar #'%production-spec-of-record
            (remove-if #'graph-db:deleted-p nodes))))

(defun rules-in-scope (graph &key view)
  "The specs a compile checks a rule against (spec §6): every enabled
stored rule of GRAPH -- through VIEW when a commit is in flight -- plus
every enabled DEF-RULE (ruling T3-R5: ENABLED reads the same on both),
plus every production, stored or DEF-PRODUCTION, a rule over FIRING
once compiled (%GUARD-PRODUCTION).  A def-rule the store cannot run
still constrains the cycle graph; RUN-RULES is what filters by family
(ruling P8)."
  (append (remove-if-not #'rule-spec-enabled
                         (%stored-rules graph :view view))
          (loop for spec being the hash-values of *def-rules*
                when (rule-spec-enabled spec) collect spec)
          (%stored-productions graph :view view)
          (alexandria:hash-table-values *def-productions*)))

(defvar *edges-cache* nil
  "NIL, or an EQ hash spec -> %EDGES result for the length of one
recompile (%FORWARD-COMPILED, RUN-RULES, the write validators): every
rule in scope is compiled against the same spec objects, so each text
is guarded once for its edges rather than once per other rule (#474).
Bound per call, so nothing outlives *DEF-RULES-STAMP*.")

(defmacro with-edges-cache (() &body body)
  "BODY with a fresh *EDGES-CACHE*, unless one is already bound."
  `(let ((*edges-cache* (or *edges-cache* (make-hash-table :test 'eq))))
     ,@body))

(defun %edges (spec graph)
  "SPEC's edges, memoised in *EDGES-CACHE* when one is bound (#474);
%COMPUTE-EDGES does the work."
  (if *edges-cache*
      (multiple-value-bind (edges found) (gethash spec *edges-cache*)
        (if found
            edges
            (setf (gethash spec *edges-cache*)
                  (%compute-edges spec graph))))
      (%compute-edges spec graph)))

(defun %compute-edges (spec graph)
  "SPEC's edges for the dependency graph, a list of (relation family
reads) -- the head's, plus one per write for a production, FAMILY the
head's family or the write's declared one (NIL for a bare write) -- or
NIL when the spec's text does not guard: such a rule cannot run and
constrains nothing.  READS is what the body reads positively AND under
a NOT: a negated read is a dependency like any other, so a cycle
another rule closes only through negation reaches the SCC search here
exactly as this rule's own does (COMPILE-RULE, GH #333); a family read
wide is a (:FAMILY . name) marker %STRATA expands (#469).  :ANY on
either side makes the whole edge :ANY, which %STRATA reads as an
isolated node.  Only the FIRST goal is read as the head: a DEF-RULE is
never validated at registration, so a two-goal head there contributes
its second goal as a read, which over-constrains rather than under-."
  (handler-case
      (multiple-value-bind (vars goals) (%guard spec graph)
        (declare (ignore vars))
        (let ((head (first goals)))
          (when (%engine-goal-p head "CLAIM" 7)
            (multiple-value-bind (reads families) (%body-reads (rest goals))
              (let* ((negative (%negative-reads (rest goals)))
                     (all (if (or (eq reads :any) (eq negative :any))
                              :any
                              (%with-family-markers
                               (union reads negative :test #'string=)
                               families))))
                ;; A production's writes are relations its inputs feed
                ;; (forward-rules design §7): one edge per write too.
                (cons (list (sixth head) (rule-spec-family spec) all)
                      (when (production-spec-p spec)
                        (mapcar (lambda (w)
                                  (list (%write-relation w)
                                        (%write-family w) all))
                                (production-spec-writes spec)))))))))
    (rule-compile-error () nil)))

(defun %expand-family-markers (reads triples)
  "READS with each (:FAMILY . f) marker replaced by the relations
TRIPLES derive or write in family F -- a bare production write, family
unknown, counts for every family -- so a family-wide read is ordered
after everything that may feed it (#469)."
  (if (or (eq reads :any) (notany #'%family-reads-marker-p reads))
      reads
      (remove-duplicates
       (loop for r in reads
             if (%family-reads-marker-p r)
               append (loop for (rel fam) in triples
                            when (or (null fam) (string-equal fam (cdr r)))
                              collect rel)
             else collect r)
       :test #'string=)))

(defun %strata (relation family reads graph others)
  "The stratum RELATION, derived in FAMILY, belongs to over READS and
OTHERS' edges (spec §2): (values RULE-NAMES RELATIONS EXPANDED-READS),
the first two sorted, the third READS with its markers expanded.
The strongly connected component of the relation graph, by a DFS from
RELATION forward and one over the reversed edges; the stratum's rules
are every rule in OTHERS deriving a component relation, plus this
one.  READS may carry (:FAMILY . f) markers (%WITH-FAMILY-MARKERS),
expanded here against every head and write in scope (#469)."
  (let ((triples (list (list relation family reads)))
        (derivers (list (cons relation nil))))
    (dolist (o others)
      (dolist (e (%edges o graph))
        (push e triples)
        (push (cons (first e) (rule-spec-name o)) derivers)))
    (let ((edges (mapcar (lambda (tr)
                           (cons (first tr)
                                 (%expand-family-markers (third tr)
                                                         triples)))
                         triples)))
    (labels (;; An :ANY edge contributes none here: an unvalidated
             ;; other rule is an isolated node for stratification and
             ;; is refused on its own compile, not here (GH #333).
             (succ (rel)
               (loop for e in edges when (string= (car e) rel)
                     append (if (eq (cdr e) :any) '() (cdr e))))
             (pred (rel)
               (loop for e in edges
                     when (and (listp (cdr e))
                               (member rel (cdr e) :test #'string=))
                       collect (car e)))
             (reach (start next)
               (let ((seen (list start)) (stack (list start)))
                 (loop while stack do
                   (dolist (n (funcall next (pop stack)))
                     (unless (member n seen :test #'string=)
                       (push n seen) (push n stack))))
                 seen)))
      (let* ((component (intersection (reach relation #'succ)
                                      (reach relation #'pred)
                                      :test #'string=))
             (names (loop for (rel . name) in derivers
                          when (and name
                                    (member rel component :test #'string=))
                            collect name)))
        (values (sort (copy-list names) #'string<)
                (sort (copy-list component) #'string<)
                ;; The subject's own reads, expanded -- not the first
                ;; edge of the list, which is another rule's.
                (%expand-family-markers reads triples)))))))

(defun %production-relations (spec reads families negative relations
                              graph others)
  "A production's stratum relations: its firing relation's component
plus its writes, after the §7 checks -- a write it reads, a write into
a family it reads wide or one that leaves its family unnamed beside
such a read (#469), or a cycle from a write back to a read, is refused
(a body with no premise was, by %GUARD-PRODUCTION)."
  (let* ((writes (production-spec-writes spec))
         (all-reads (union reads negative :test #'string=))
         (marked (%with-family-markers all-reads families)))
    (let ((bad (intersection (mapcar #'%write-relation writes) all-reads
                             :test #'string=)))
      (when bad
        (%refuse spec "the production reads what it writes: ~{~A~^, ~}"
                 bad)))
    (dolist (w writes)
      (let ((fam (%write-family w)))
        (cond ((and fam (member fam families :test #'string-equal))
               (%refuse spec "the production reads what it writes: every ~
relation of ~A, its write ~A included" fam (%write-relation w)))
              ((and (null fam) families)
               (%refuse spec "the body reads ~{~A~^, ~} wide, so name the ~
family of the write ~S as (\"family\" ~S)"
                        families w w)))))
    (dolist (w writes)
      (multiple-value-bind (names component expanded)
          (%strata (%write-relation w) (%write-family w) marked graph
                   others)
        (declare (ignore names))
        ;; EXPANDED: the reads with a family-wide read spelled out, so
        ;; a cycle through a rule deriving into that family is seen.
        (let ((cycle (intersection component expanded :test #'string=)))
          (when cycle
            (%refuse spec "a cycle runs from the production's write ~A ~
back to its read ~{~A~^, ~}" (%write-relation w) cycle)))))
    (sort (remove-duplicates (append (mapcar #'%write-relation writes)
                                     relations)
                             :test #'string=)
          #'string<)))

(defun compile-production (graph production &key (others nil others-p))
  "PRODUCTION (a PRODUCTION record, a PRODUCTION-SPEC or a name)
compiled through COMPILE-RULE as the rule over FIRING it is
(%GUARD-PRODUCTION).  => COMPILED-RULE whose spec is a
PRODUCTION-SPEC; signals RULE-COMPILE-ERROR."
  (let ((spec (etypecase production
                (production-spec production)
                (production (%production-spec-of-record production))
                (string (or (%stored-production graph production)
                            (find-def-production production)
                            (error "No production named ~S in ~(~S~) ~
or the image." production (graph-db:graph-name graph)))))))
    (if others-p
        (compile-rule graph spec :others others)
        (compile-rule graph spec))))

(defun compiled-production-p (compiled)
  "COMPILED is a production's COMPILED-RULE."
  (and (compiled-rule-p compiled)
       (production-spec-p (compiled-rule-spec compiled))))

(declaim (notinline compile-rule))

(defun compile-rule (graph rule &key (others nil others-p))
  "RULE (a RULE record, a RULE-SPEC, or a stored rule's name) compiled
against GRAPH's schema and the rules in scope (spec §6): head and body
through the guard, the head checked as one claim/7 pattern,
claim-producer generators moved to the front (P5), STRATUM computed
over every rule in OTHERS -- default RULES-IN-SCOPE minus this one
(GH #333).  => COMPILED-RULE; signals RULE-COMPILE-ERROR.  A name held
by both a stored rule and a DEF-RULE is a collision, refused."
  (let* ((spec (rule-spec-of rule graph))
         (name (rule-spec-name spec))
         (others (remove name
                         (if others-p others (rules-in-scope graph))
                         :key #'rule-spec-name :test #'string=)))
    (when (and (eq (rule-spec-source spec) :stored) (find-def-rule name))
      (%refuse spec "a def-rule of the same name exists in the image"))
    (when (and (eq (rule-spec-source spec) :def-rule)
               (find name (%stored-rules graph)
                     :key #'rule-spec-name :test #'string=))
      (%refuse spec "a stored rule of the same name exists in the store"))
    (unless (or (production-spec-p spec)
                (= 1 (%head-goal-count spec graph)))
      (%refuse spec "the head must be exactly one claim/7 pattern"))
    (multiple-value-bind (vars goals synthesised) (%guard spec graph)
      (when (%mentions-claim-after-p goals)
        (%refuse spec "claim-after/9 is not a rule premise: write ~
claim/7 and the comparison, the compiler fuses them (#451)"))
      (let* ((head (first goals))
             (body (%order-body (rest goals)))
             (body-vars (%body-variables spec body)))
        (%check-no-bare-var spec head)
        (when (null body)
          (%refuse spec "the body is empty"))
        (let* ((premise-vars (%premise-vars body))
               (thresholds (%thresholds spec body premise-vars))
               (parsed (%parse-head spec head body-vars premise-vars))
               ;; The guard's order is first appearance; the head's
               ;; own variables and the synthesised ones drop out.
               (ordered-body-vars
                 (remove-if-not (lambda (v)
                                  (and (member v body-vars)
                                       (not (member v synthesised))))
                                vars))
               (head-vars (remove-if-not #'%variable-p
                                         (list (getf parsed :head-sns)
                                               (getf parsed :head-skey)
                                               (getf parsed :head-ons)
                                               (getf parsed :head-okey)))))
          (multiple-value-bind (reads families) (%body-reads body)
            (when (eq reads :any)
              (%refuse spec "a body claim/7 goal leaves its relation ~
unbound, so the rule reads every relation, its own ~S included: bind ~
the relation" (getf parsed :relation)))
            ;; A family read wide (#469): not the head's own family --
            ;; that is reading its own relation -- and not the rules
            ;; layer's own families.
            (when (member (rule-spec-family spec) families
                          :test #'string-equal)
              (%refuse spec "the body reads every relation of ~A, the ~
head's own family, its own ~S included: bind the relation"
                       (rule-spec-family spec) (getf parsed :relation)))
            (dolist (f '("firing" "derivation"))
              (when (member f families :test #'string-equal)
                (%refuse spec "a family-wide read of ~A is refused: ~
bind the relation" f)))
            (let ((negative (%negative-reads body)))
              (when (eq negative :any)
                (%refuse spec "a negated claim/7 goal leaves its ~
relation unbound: bind the relation"))
              (multiple-value-bind (stratum relations)
                  ;; A NOT's target joins the SCC search, here for this
                  ;; rule and in %EDGES for every other, so a cycle
                  ;; closed only through negation is detected wherever
                  ;; it runs (GH #333: that is what makes it
                  ;; unstratifiable, and the refusal below names it).
                  (%strata (getf parsed :relation) (rule-spec-family spec)
                           (%with-family-markers
                            (union reads negative :test #'string=)
                            families)
                           graph others)
                (let ((bad (intersection negative relations
                                         :test #'string=)))
                  (when bad
                    (%refuse spec "negation over the rule's own stratum ~
~{~A~^, ~}: a NOT may read only an earlier stratum" bad)))
                (when (production-spec-p spec)
                  (setf relations (%production-relations
                                   spec reads families negative relations
                                   graph others)))
                (apply #'%make-compiled-rule
                       :spec spec
                       :vars (remove-duplicates
                              (append head-vars premise-vars
                                      (mapcar #'sixth thresholds)))
                       :body-vars ordered-body-vars
                       :premise-vars premise-vars
                       :thresholds thresholds
                       :goals body :reads reads :family-reads families
                       :negative-reads negative
                       :stratum (sort (cons name (copy-list stratum))
                                      #'string<)
                       :stratum-relations relations
                       parsed)))))))))

;;; The write validator (ruling P3)

(defun %validate-rule-writes (tx graph)
  "GRAPH-DB:*COMMIT-VALIDATORS*: every RULE record this transaction
writes compiles against the store as it will be after the commit, so
a rule that cannot run is never stored (spec §6, ruling P3).  Inert
until some store has run DEF-RULES-SCHEMA -- that is what makes the
RULE class, and this validator is image-wide."
  (let ((class (find-class 'rule nil)))
    (when class
      (let ((written (loop for w in (graph-db:writes tx)
                           for node = (graph-db::node w)
                           when (and (typep node class)
                                     (not (graph-db:deleted-p node)))
                             collect node)))
        (when written
          (let ((scope (rules-in-scope
                        graph
                        :view (graph-db:make-commit-view graph tx))))
            (with-edges-cache ()
              (dolist (r written)
                (compile-rule graph r :others scope)))))))))

(pushnew '%validate-rule-writes graph-db:*commit-validators*)

(defun %validate-production-writes (tx graph)
  "GRAPH-DB:*COMMIT-VALIDATORS*: every PRODUCTION record this
transaction writes names a registered action and compiles against
the store as it will be after the commit (forward-rules design §7)."
  (let ((class (find-class 'production nil)))
    (when class
      (let ((written (loop for w in (graph-db:writes tx)
                           for node = (graph-db::node w)
                           when (and (typep node class)
                                     (not (graph-db:deleted-p node)))
                             collect node)))
        (when written
          (let ((scope (rules-in-scope
                        graph
                        :view (graph-db:make-commit-view graph tx))))
            (with-edges-cache ()
              (dolist (p written)
                (unless (gethash (production-action p) *production-actions*)
                  (error 'rule-compile-error
                         :rule (production-name p)
                         :reason (format nil "action ~S is not registered"
                                         (production-action p))))
                (compile-production graph p :others scope)))))))))

(pushnew '%validate-production-writes graph-db:*commit-validators*)
