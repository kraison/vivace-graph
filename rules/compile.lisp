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

(defvar *def-rules* (make-hash-table :test 'equal)
  "DEF-RULE's registry, name -> RULE-SPEC: rules that live in the image
rather than in a store (spec §5).  Image-wide, so RUN-RULES filters it
by the store's families (ruling P8).")

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
  (and (remhash name *def-rules*) t))

(defun find-def-rule (name)
  "The DEF-RULE NAME's RULE-SPEC, or NIL."
  (values (gethash name *def-rules*)))

(defun rule-spec-of (thing)
  "THING as a RULE-SPEC: a RULE record is read into one, a spec passes."
  (etypecase thing
    (rule-spec thing)
    (rule (%make-rule-spec :name (rule-name thing)
                           :version (rule-version thing)
                           :family (%family-string (rule-family thing))
                           :head (rule-head thing) :body (rule-body thing)
                           :extent-policy (rule-extent-policy thing)
                           :enabled (rule-enabled thing)
                           :source :stored))))

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
list, PREMISE-VARS the ?c of every body CLAIM/7 goal, READS the
relations the body reads or :ANY."
  spec family relation
  head-c head-sns head-skey head-ons head-okey unary-p
  vars premise-vars goals reads)

;;; Reading the text

(defun %variable-p (x)
  (graph-db::variable-p x))

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

(defun %guard (spec graph)
  "HEAD then BODY through the guard as one text, so a variable shared
between them reads as one symbol: (VALUES VARS GOALS)."
  (handler-case
      (graph-db.query:guard-query-text
       (format nil "~A~%~A" (rule-spec-head spec) (rule-spec-body spec))
       graph)
    (graph-db.query:prolog-guard-error (c)
      (%refuse spec "~A" (graph-db.query:prolog-guard-error-reason c)))))

(defun %head-goal-count (spec graph)
  "How many goals the HEAD text alone is -- the guard refuses an empty
text, which here is a refusal in its own words."
  (handler-case
      (length (nth-value 1 (graph-db.query:guard-query-text
                            (rule-spec-head spec) graph)))
    (graph-db.query:prolog-guard-error (c)
      (%refuse spec "head: ~A"
               (graph-db.query:prolog-guard-error-reason c)))))

(defun %body-variables (spec goals)
  "Every ?variable in GOALS, once each.  A bare ? is refused: read into
the guard's scratch package it is one NAMED variable shared by every
goal that writes it, not the engine's anonymous one (recon A10)."
  (let ((vars '()))
    (labels ((walk (x)
               (cond ((and (%variable-p x) (string= (symbol-name x) "?"))
                      (%refuse spec "a bare ? is one shared variable ~
here, not an anonymous one: name it"))
                     ((%variable-p x) (pushnew x vars))
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
         (%refuse spec "~A namespace ~A is not bound by the body"
                  what (symbol-name term)))
        (t (%refuse spec "~A namespace must be a string or a body ~
variable, not ~S" what term))))

(defun %head-key (spec term body-vars what)
  (cond ((stringp term) term)
        ((and (%variable-p term) (member term body-vars)) term)
        ((%variable-p term)
         (%refuse spec "~A key ~A is not bound by the body"
                  what (symbol-name term)))
        (t (%refuse spec "~A key must be a string or a body variable, ~
not ~S" what term))))

(defun %parse-head (spec head body-vars)
  "The head's seven arguments checked against spec §6; returns a plist
of the COMPILED-RULE head slots."
  (unless (%engine-goal-p head "CLAIM" 7)
    (%refuse spec "the head must be a claim/7 pattern, not ~S"
             (if (consp head)
                 (string-downcase (string (first head)))
                 head)))
  (destructuring-bind (?c fam sns skey rel ons okey) (rest head)
    (unless (%variable-p ?c)
      (%refuse spec "the head's ?c must be an unbound variable, not ~S" ?c))
    (when (member ?c body-vars)
      (%refuse spec "the head's ?c ~A must not appear in the body"
               (symbol-name ?c)))
    (let ((family (handler-case (graph-db.spacetime:claim-family fam)
                    (graph-db.spacetime:unknown-claim-family ()
                      (%refuse spec "~S is not a claim family" fam)))))
      (unless (string-equal (symbol-name fam) (rule-spec-family spec))
        (%refuse spec "the head's family ~(~A~) is not the rule's ~
family ~A" fam (rule-spec-family spec)))
      (unless (and (stringp rel)
                   (graph-db.spacetime:canonical-relation-p rel))
        (%refuse spec "the head's relation must be a canonical string, ~
not ~S" rel))
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

(defun %generator-goal-p (goal)
  "(claim-producer ?v \"p\"): a generator, to run first (ruling P5)."
  (and (%engine-goal-p goal "CLAIM-PRODUCER" 2)
       (%variable-p (second goal))
       (stringp (third goal))))

(defun %order-body (goals)
  (append (remove-if-not #'%generator-goal-p goals)
          (remove-if #'%generator-goal-p goals)))

(defun %body-reads (goals)
  "The relations the body's CLAIM/7 goals read, or :ANY when one of
them leaves the relation unbound (ruling P6)."
  (let ((reads '()))
    (dolist (goal goals (nreverse reads))
      (when (%engine-goal-p goal "CLAIM" 7)
        ;; The relation is CLAIM/7's fifth ARGUMENT, i.e. (sixth goal):
        ;; (claim ?c family sns skey rel ons okey).
        (let ((rel (sixth goal)))
          (if (stringp rel)
              (pushnew rel reads :test #'string=)
              (return-from %body-reads :any)))))))

(defun %premise-vars (goals)
  (loop for goal in goals
        when (and (%engine-goal-p goal "CLAIM" 7)
                  (%variable-p (second goal)))
          collect (second goal) into vars
        finally (return (remove-duplicates vars))))

;;; The rule set a compile is checked against

(defun %stored-rules (graph &key view)
  "Every RULE record in GRAPH, as specs.  With VIEW (a commit view),
records the transaction writes replace or remove their committed
version, so the set is the store as it will be after the commit."
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

(defun rules-in-scope (graph &key view)
  "The specs a compile checks a rule against (spec §6): every enabled
stored rule of GRAPH -- through VIEW when a commit is in flight -- plus
every DEF-RULE.  A def-rule the store cannot run still constrains the
cycle graph; RUN-RULES is what filters by family (ruling P8)."
  (append (remove-if-not #'rule-spec-enabled
                         (%stored-rules graph :view view))
          (loop for spec being the hash-values of *def-rules*
                when (rule-spec-enabled spec) collect spec)))

(defun %edges (spec graph)
  "SPEC's (head-relation . reads) for the cycle graph, or NIL when the
spec's text does not guard -- such a rule cannot run and constrains
nothing."
  (handler-case
      (multiple-value-bind (vars goals) (%guard spec graph)
        (declare (ignore vars))
        (let ((head (first goals)))
          (when (%engine-goal-p head "CLAIM" 7)
            (cons (sixth head) (%body-reads (rest goals))))))
    (rule-compile-error () nil)))

(defun %check-cycle (spec relation reads graph others)
  "Refuse when SPEC's head RELATION reaches itself through READS and
OTHERS' edges (spec §6), naming the path.  :ANY reads every head
relation in scope, the rule's own included (ruling P6)."
  (let* ((edges (list (cons relation reads)))
         (heads (list relation)))
    (dolist (o others)
      (let ((e (%edges o graph)))
        (when e
          (push e edges)
          (pushnew (car e) heads :test #'string=))))
    (labels ((successors (rel)
               (let ((out '()))
                 (dolist (e edges out)
                   (when (string= (car e) rel)
                     (setf out (union out (if (eq (cdr e) :any)
                                              heads
                                              (cdr e))
                                      :test #'string=))))))
             (path-to (target from seen)
               (dolist (next (successors from))
                 (cond ((string= next target)
                        (return (list next)))
                       ((not (member next seen :test #'string=))
                        (let ((p (path-to target next (cons next seen))))
                          (when p (return (cons next p)))))))))
      (when (eq reads :any)
        (%refuse spec "a body claim/7 goal leaves its relation unbound, ~
so the rule reads every relation, its own ~S included: bind the relation"
                 relation))
      (let ((path (path-to relation relation (list relation))))
        (when path
          (%refuse spec "deriving ~S closes a cycle: ~{~A~^ -> ~}"
                   relation (cons relation path)))))))

(defun compile-rule (graph rule &key (others nil others-p))
  "RULE (a RULE record or a RULE-SPEC) compiled against GRAPH's schema
and the rules in scope (spec §6): head and body through the guard, the
head checked as one claim/7 pattern, claim-producer generators moved
to the front (P5), recursion refused over every rule in OTHERS --
default RULES-IN-SCOPE minus this one -- with the cycle named.
=> COMPILED-RULE; signals RULE-COMPILE-ERROR.  A name held by both a
stored rule and a DEF-RULE is a collision, refused."
  (let* ((spec (rule-spec-of rule))
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
    (unless (= 1 (%head-goal-count spec graph))
      (%refuse spec "the head must be exactly one claim/7 pattern"))
    (multiple-value-bind (vars goals) (%guard spec graph)
      (declare (ignore vars))
      (let* ((head (first goals))
             (body (%order-body (rest goals)))
             (body-vars (%body-variables spec body)))
        (when (null body)
          (%refuse spec "the body is empty"))
        (let* ((parsed (%parse-head spec head body-vars))
               (reads (%body-reads body))
               (premise-vars (%premise-vars body))
               (head-vars (remove-if-not #'%variable-p
                                         (list (getf parsed :head-sns)
                                               (getf parsed :head-skey)
                                               (getf parsed :head-ons)
                                               (getf parsed :head-okey)))))
          (%check-cycle spec (getf parsed :relation) reads graph others)
          (apply #'%make-compiled-rule
                 :spec spec
                 :vars (remove-duplicates (append head-vars premise-vars))
                 :premise-vars premise-vars
                 :goals body :reads reads
                 parsed))))))

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
            (dolist (r written)
              (compile-rule graph r :others scope))))))))

(pushnew '%validate-rule-writes graph-db:*commit-validators*)
