;;;; Free-text Prolog, behind START-GUI's :ALLOW-PROLOG flag (GH #279).
;;;;
;;;; The builder (GH #278) reads no user text at all.  This surface
;;;; does, so the whole risk of the workbench concentrates here and the
;;;; guard -- not the editor -- is the deliverable.
;;;;
;;;; Order of operations, all of it before one character reaches the
;;;; Prolog compiler:
;;;;
;;;;   1. *ALLOW-PROLOG*     -- the flag, stays in the GUI handler
;;;;      (gui/prolog.lisp), checked before the graph is resolved.
;;;;   2. %SCAN-QUERY-TEXT   -- a character screen that runs BEFORE the
;;;;      reader: length, paren depth/balance, and an outright refusal
;;;;      of #, `, , and the package marker.  Refusing ':' textually is
;;;;      what makes package-qualified input safe -- READ would intern
;;;;      GRAPH-DB::ANYTHING before any walker could object.
;;;;   3. %READ-QUERY-FORMS  -- *READ-EVAL* NIL, a readtable with #, `
;;;;      and , disabled, standard IO syntax otherwise, and *PACKAGE*
;;;;      bound to a per-request scratch package that USES NOTHING.
;;;;   4. %GUARD-QUERY       -- walks the form and rebuilds it out of
;;;;      canonical symbols.  A symbol survives only as a registered
;;;;      functor of that arity, a control construct, a schema type or
;;;;      slot of THIS graph, a ?variable, or T/NIL.
;;;;   5. RUN-QUERY-GOALS    -- query/dsl.lisp's own runner: :EFFECTS
;;;;      NIL, one snapshot, the inference/time/row bounds.  Reached
;;;;      through %RUN-GUARDED-GOALS in this file.  Goal heads resolve
;;;;      each in its own package now, not one shared :PACKAGE
;;;;      (GH #322).
;;;;   6. DELETE-PACKAGE     -- in an UNWIND-PROTECT, so a hostile
;;;;      query's symbols leave with the request that made them.
;;;;      RUN-GUARDED-PROLOG, which does this, is also in this file;
;;;;      only step 1's flag check stays in gui/prolog.lisp.
;;;;
;;;; The interning subtlety (issue text, utilities.lisp:155):
;;;; MAKE-FUNCTOR-SYMBOL now looks up an existing functor before
;;;; interning (GH #322), but a miss still falls through to INTERN --
;;;; so probing it with a client's own NAME/ARITY would create exactly
;;;; the symbol it was asking about.  The whitelist therefore ENUMERATES
;;;; the two live registries and compares NAMES AS STRINGS; nothing
;;;; derived from the request is ever interned into GRAPH-DB or the
;;;; schema package.
;;;;
;;;; Home: graph-db/query (GH #322) -- steps 2 through 6, including
;;;; RUN-GUARDED-PROLOG.  Only step 1's flag and the HTTP envelope
;;;; around a call into this file stay in gui/prolog.lisp, which
;;;; calls this file's exports qualified.

(in-package #:graph-db.query)

(defparameter *prolog-max-query-length* 4096
  "Longest free-text query accepted, in characters.")

(defparameter *prolog-max-depth* 32
  "Deepest parenthesis nesting accepted.  Bounds the reader's own
recursion, so a nesting bomb is refused before READ sees it.")

(define-condition prolog-guard-error (error)
  ((reason :initarg :reason :reader prolog-guard-error-reason))
  (:report (lambda (c s)
             (format s "~A" (prolog-guard-error-reason c))))
  (:documentation "A free-text query the guard refused.  Carries the
client-facing reason, which names the offending token."))

(define-condition prolog-ill-typed-error (error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "ill-typed query")))
  (:documentation "A guarded query whose goals were given arguments a
predicate cannot use -- the CLIENT's error, answered 400.  Deliberately
CARRIES NO DETAIL: the conditions it stands in for report engine
internals -- a store's keyword name, a generic-function name, an ANSI
section reference -- and the client that provoked them is
unauthenticated.  The detail is logged (GH #279)."))

(define-condition prolog-server-fault (error) ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "internal error")))
  (:documentation "A condition raised while running a guarded query
that is NOT a shape client input is known to produce -- i.e. an engine
defect, answered 500.  Separate from PROLOG-ILL-TYPED-ERROR so a
genuine fault is never labelled the client's, in the response or in the
log.  Carries no detail either: the leak rule does not relax because
the fault is ours (GH #279)."))

(defun %refuse (format-string &rest args)
  (error 'prolog-guard-error
         :reason (apply #'format nil format-string args)))

(defun %term-label (x)
  "X as the client wrote it.  A symbol prints bare and downcased -- ~S
would spell out the request's scratch package, which is our bookkeeping
and not something to explain in an error message."
  (if (and x (symbolp x))
      (string-downcase (symbol-name x))
      (format nil "~S" x)))

;;; ---------------------------------------------------------------------
;;; Step 2: the pre-read character screen
;;; ---------------------------------------------------------------------

(defun %token-around (text pos)
  "The whitespace/paren-delimited token of TEXT containing POS -- what
to name in a refusal message."
  (flet ((delim-p (ch)
           (or (member ch '(#\( #\) #\" #\; #\')) (char<= ch #\Space))))
    (let ((start pos) (end pos) (n (length text)))
      (loop while (and (plusp start) (not (delim-p (char text (1- start)))))
            do (decf start))
      (loop while (and (< end n) (not (delim-p (char text end))))
            do (incf end))
      (subseq text start end))))

(defun %skip-delimited (text start close what)
  "Index just past the CLOSE that ends the region starting at START,
honouring backslash escapes.  Refuses an unterminated region."
  (let ((i start) (n (length text)))
    (loop
      (when (>= i n) (%refuse "unterminated ~A" what))
      (let ((ch (char text i)))
        (cond ((char= ch #\\) (incf i 2))
              ((char= ch close) (return (1+ i)))
              (t (incf i)))))))

(defun %scan-query-text (text)
  "Screen TEXT before the reader touches it (GH #279).

Refuses, by name: the package marker (so no READ can intern into
GRAPH-DB or a schema package), every # reader macro (#. above all),
backquote and comma, unbalanced or over-deep parentheses, and an
unterminated string or |...| name.  Inside a string or a |...| name
none of those characters mean anything, so a literal \"#.(...)\" is
data and passes -- the guard refuses reader syntax, not text."
  (let ((depth 0) (i 0) (n (length text)))
    (loop while (< i n) do
      (let ((ch (char text i)))
        (cond
          ((char= ch #\\)
           (when (> (+ i 2) n)
             (%refuse "the query ends in a backslash escape"))
           (incf i 2))
          ((char= ch #\;)
           (setq i (or (position #\Newline text :start i) n)))
          ((char= ch #\") (setq i (%skip-delimited text (1+ i) #\" "string")))
          ((char= ch #\|)
           (setq i (%skip-delimited text (1+ i) #\| "|...| name")))
          ((char= ch #\()
           (incf depth)
           (when (> depth *prolog-max-depth*)
             (%refuse "nesting deeper than ~D parentheses is not ~
permitted" *prolog-max-depth*))
           (incf i))
          ((char= ch #\))
           (when (zerop depth)
             (%refuse "unbalanced parentheses: a ')' closes nothing"))
           (decf depth)
           (incf i))
          ((char= ch #\:)
           (%refuse "package-qualified name ~S is not permitted: a ~
query may name only this graph's schema and the registered Prolog ~
functors" (%token-around text i)))
          ((char= ch #\#)
           (%refuse "reader macro ~S is not permitted; #. (read-time ~
evaluation) least of all"
                    (subseq text i (min n (+ i 2)))))
          ((or (char= ch #\`) (char= ch #\,))
           (%refuse "~S is not permitted in a query" (string ch)))
          (t (incf i)))))
    (unless (zerop depth)
      (%refuse "unbalanced parentheses: ~D form~:P left open" depth))
    t))

;;; ---------------------------------------------------------------------
;;; Step 3: the read
;;; ---------------------------------------------------------------------

(defvar *prolog-readtable* nil
  "Lazily built readtable for free-text queries; see %PROLOG-READTABLE.")

(defun %refuse-reader-macro (stream char)
  (declare (ignore stream))
  (%refuse "the ~S reader macro is not permitted" (string char)))

(defun %prolog-readtable ()
  "A copy of the standard readtable with #, ` and , disabled.  Second
line of defence: %SCAN-QUERY-TEXT already refuses these characters, so
this only fires if that screen is ever wrong."
  (or *prolog-readtable*
      (setq *prolog-readtable*
            (let ((rt (copy-readtable nil)))
              (dolist (ch '(#\# #\` #\,) rt)
                (set-macro-character ch #'%refuse-reader-macro nil rt))))))

(defvar *prolog-scratch-counter* 0)

(defun %make-scratch-package ()
  "A fresh empty package for one request's read.  :USE '() -- so a bare
name the client types can intern nowhere but here, and DELETE-PACKAGE
takes every one of them with it.  Retries on the (benign) name race
between concurrent requests."
  (loop repeat 64
        for name = (format nil "GRAPH-DB.GUI.SCRATCH-~D"
                           (incf *prolog-scratch-counter*))
        for pkg = (and (not (find-package name))
                       (ignore-errors (make-package name :use '())))
        when pkg return pkg
          finally (error "Cannot create a GUI Prolog scratch package.")))

(defun %read-query-forms (text package)
  "TEXT read into PACKAGE as a list of forms.  *READ-EVAL* NIL, the
restricted readtable, standard IO syntax otherwise.  A string stream
cannot block, and unbalanced input signals END-OF-FILE rather than
hanging -- so this terminates for every input."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* package)
          (*readtable* (%prolog-readtable))
          (*read-suppress* nil))
      (with-input-from-string (in text)
        ;; IN is its own EOF marker: no readable object can be EQ to it.
        (loop for form = (read in nil in)
              until (eq form in)
              collect form)))))

;;; ---------------------------------------------------------------------
;;; Step 4: the whitelist, derived from the live image
;;; ---------------------------------------------------------------------

(defun %split-functor-name (symbol)
  "SYMBOL named NAME/ARITY as (values name-string arity), else NIL.
Splits at the LAST slash, so /=/2 reads as (\"/=\" . 2)."
  (let* ((name (symbol-name symbol))
         (slash (position #\/ name :from-end t)))
    (when (and slash (plusp slash) (< (1+ slash) (length name)))
      (let ((arity (ignore-errors (parse-integer name :start (1+ slash)))))
        (when (and arity (>= arity 0))
          (values (subseq name 0 slash) arity))))))

;; ⚠ The three lists below are the ONLY hand-maintained part of the
;; guard: the whitelist grows with the registries automatically, these
;; do not.  PROLOG-FUNCTOR-INVENTORY-IS-PINNED (tests/gui/gui-tests.lisp)
;; fails when a functor is added or removed anywhere, and its message
;; says how to classify the new one.  Read it before editing any of them.

(defparameter *prolog-excluded-predicates*
  '("%COMMIT" "CALL" "CATCH" "FINDALL" "BAGOF" "SETOF" "MAP-QUERY"
    "SELECT" "SHOW-PROLOG-VARS")
  "Predicate names withheld from free text at EVERY arity, whatever the
registries say.

All but the first are the runtime meta-call family: each hands a term to
%SOLVE, which builds a functor symbol from that term's head at run time
(prolog-functors.lisp:229).  For a STRING head that is an INTERN of
unvetted data -- graph content, reached through a variable -- into the
live schema package, which is the one way a fully whitelisted query
could still grow the image.  SELECT/2 and SHOW-PROLOG-VARS/2 are
result-collection machinery and a REPL printer, neither a predicate.
%COMMIT is an internal cut barrier taking compiler gensyms (GH #279).")

(defparameter *prolog-goal-argument-control*
  '("AND" "OR" "NOT" "ONCE" "IF" "FORALL")
  "Control constructs whose arguments are GOALS.  Every such argument
must be a parenthesised goal here, never a variable: a variable makes
STATIC-GOAL-P (prologc.lisp:475) fail, the compiler macro declines, and
the runtime functor behind it meta-calls whatever the variable holds --
the %SOLVE path *PROLOG-EXCLUDED-PREDICATES* exists to close.  Demanding
a static goal keeps all six on the compile-time path (GH #279).  Same
tripwire as above: PROLOG-FUNCTOR-INVENTORY-IS-PINNED.")

(defparameter *prolog-cost-unbounded-predicates*
  (graph-db:cost-unbounded-predicate-names)   ; single source: GH #285
  "Predicate names withheld because their worst-case cost is bounded
neither by the graph nor by the length of the query.

The query rails are enforced by %TICK (prologc.lisp:890), which runs at
inference and goal boundaries -- NEVER inside a functor that is already
running.  A predicate that can burn arbitrary time in one atomic Lisp
call is therefore not preemptible by *QUERY-DEFAULT-TIMEOUT* or
*QUERY-DEFAULT-MAX-INFERENCES*, and a watchdog is not an option here:
interrupting a worker mid-call could unwind holding the GUI's rw lock
or with an mmap operation in flight, which is worse than the hazard it
would close.  Exclusion is the sound fix (GH #279).

REGEX-MATCH/2 takes BOTH the pattern and the subject from the client,
so a catastrophically backtracking pattern -- (a+)+$ against a run of
a's -- costs ~2^n in a payload of a few dozen characters, well inside
the 4096-character cap, and runs past the 30 s deadline unaborted.
VALID-DATE-P/1 also runs a regex and STAYS: its pattern is a fixed,
anchored, quantifier-free constant, so its cost is linear in a subject
the length cap already bounds -- which is the line this category draws.

Graph-bounded scans (IS-A/2, FIND-SLOT-RANGE/5, the spatial FIND-*
family, whose window cover is capped at +SPATIAL-QUERY-MAX-CELLS+) are
NOT in this category: their cost tracks the operator's own data, and
the un-flagged structured builder already reaches all of it.")

(defun %excluded-predicate-p (name)
  "NAME is withheld from free text, for either reason."
  (or (member name *prolog-excluded-predicates* :test #'string=)
      (member name *prolog-cost-unbounded-predicates* :test #'string=)))

(defun %functor-whitelist ()
  "(name-string . arity) -> home package, ENUMERATED from the two live
registries: *PROLOG-GLOBAL-FUNCTORS* (globals.lisp:420, which also
carries the per-schema edge functors) and *USER-FUNCTORS*.

Enumerated, never probed: MAKE-FUNCTOR-SYMBOL interns, so asking the
registry whether a client's NAME/ARITY exists would create it.
Uninterned keys are dropped -- SELECT registers a transient gensym
functor per running query, which is nobody's predicate."
  (let ((table (make-hash-table :test 'equal))
        (engine (find-package :graph-db)))
    (flet ((add (key)
             (when (and (symbolp key) (symbol-package key))
               (multiple-value-bind (name arity) (%split-functor-name key)
                 ;; The exclusions name ENGINE predicates, so they only
                 ;; apply to GRAPH-DB-homed keys.  A schema that happens
                 ;; to declare an edge type called REGEX-MATCH (or CALL,
                 ;; or SELECT) owns that name in its own package, and its
                 ;; auto-installed NAME/2 and NAME/3 must not be silently
                 ;; dropped -- the inventory tripwire watches the
                 ;; registry, not the schema, so it would not catch it
                 ;; (GH #279).
                 (when (and name
                            (not (and (eq (symbol-package key) engine)
                                      (%excluded-predicate-p name))))
                   (setf (gethash (cons name arity) table)
                         (symbol-package key)))))))
      (maphash (lambda (k v) (declare (ignore v)) (add k))
               graph-db::*prolog-global-functors*)
      (maphash (lambda (k v) (declare (ignore v)) (add k))
               graph-db::*user-functors*))
    table))

(defun %control-word-table ()
  "NAME-STRING -> canonical symbol for the Prolog control constructs,
enumerated from the image: every GRAPH-DB symbol carrying a
PROLOG-COMPILER-MACRO property, plus the two cut spellings COMPILE-BODY
compares by EQ.  Derived from the image, not hand-listed, minus
*PROLOG-EXCLUDED-PREDICATES*."
  (let ((table (make-hash-table :test 'equal))
        (pkg (find-package :graph-db)))
    (do-symbols (s pkg)
      (when (and (eq (symbol-package s) pkg)
                 (get s 'graph-db::prolog-compiler-macro)
                 (not (%excluded-predicate-p (symbol-name s))))
        (setf (gethash (symbol-name s) table) s)))
    (dolist (name '("!" "CUT") table)
      (let ((s (find-symbol name pkg)))
        (when s (setf (gethash name table) s))))))

(defun schema-type-names (graph parent)
  "Node-type names (class symbols) registered for PARENT (:vertex or
:edge) in GRAPH's schema, sorted by name."
  (let ((names '()))
    (maphash (lambda (key meta)
               (when (numberp key)
                 (push (graph-db::node-type-name meta) names)))
             (gethash parent (graph-db::schema-type-table
                             (graph-db::schema graph))))
    (sort names #'string< :key #'symbol-name)))

(defun %schema-name-table (graph)
  "NAME-STRING -> canonical symbol for every vertex/edge type and every
declared slot of GRAPH's schema.  Together with the functors and the
?variables this is the complete set of symbols a query may name; the
names are the engine's own kebab spelling, so what /types shows is what
a query types (GH #277)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (parent '(:vertex :edge) table)
      (dolist (type-name (schema-type-names graph parent))
        (setf (gethash (symbol-name type-name) table) type-name)
        (let ((meta (graph-db::lookup-node-type-by-name
                     (intern (symbol-name type-name) :keyword)
                     parent :graph graph)))
          (dolist (slot (and meta (graph-db::node-type-slots meta)))
            (let ((s (if (consp slot) (first slot) slot)))
              (when (symbolp s)
                (setf (gethash (symbol-name s) table) s)))))))))

(defstruct (guard-ctx (:conc-name gc-))
  "One request's whitelist plus the scratch package its symbols came
from.  VARS accumulates the query variables in reverse first-appearance
order."
  package functors control schema (vars '()))

(defun %guard-context (graph package)
  (make-guard-ctx :package package
                  :functors (%functor-whitelist)
                  :control (%control-word-table)
                  :schema (%schema-name-table graph)))

;;; The walk.  It does not merely validate: it REBUILDS the form out of
;;; canonical symbols, so what reaches EVAL is made of the engine's own
;;; symbols and the request's scratch variables -- never a symbol the
;;; client's text interned.

(defun %routes-to-engine-control-p (name)
  "True when a goal head spelled NAME will be compiled by GRAPH-DB's own
control macro, whatever package the head itself lives in.

Ask the question the COMPILER asks, not the one the registry answers.
PROLOG-COMPILER-MACRO (prologc.lisp:224) canonicalizes a foreign-package
head BY NAME back into GRAPH-DB, so a head's HOME PACKAGE does not
decide what compiles it -- its NAME does.  Scoping the exclusions to
GRAPH-DB-homed registry keys therefore excluded by home while the
compiler routed by name, and a schema-package CALL/2 (the GH #172
runtime-schema shape) was admitted by the whitelist and then handed
straight to the engine's CALL macro, re-opening %SOLVE-CALL -- the one
path by which a fully whitelisted query can still intern a functor
symbol from graph data.

Only CALL and %COMMIT are both excluded and compiler-macro-backed, so
this refuses exactly those two and leaves a schema's own FINDALL,
SELECT or REGEX-MATCH working, which is what the home-scoping was for
(GH #279)."
  (let ((sym (find-symbol name (find-package :graph-db))))
    (and sym (get sym 'graph-db::prolog-compiler-macro) t)))

(defun %cut-symbol-p (x ctx)
  "X is one of the two cut spellings -- the only bare symbol that is a
goal on its own."
  (and x (symbolp x)
       (member (symbol-name x) '("!" "CUT") :test #'string=)
       (gethash (symbol-name x) (gc-control ctx))))

(defun %guard-symbol (sym ctx)
  "SYM validated and translated, or a refusal naming it."
  (let ((name (symbol-name sym))
        (pkg (symbol-package sym)))
    (cond
      ((null pkg)
       (%refuse "uninterned symbol ~A is not permitted" name))
      ;; The scratch package uses nothing, so a symbol resolved
      ;; anywhere else was package-qualified or inherited.
      ((not (eq pkg (gc-package ctx)))
       (%refuse "package-qualified symbol ~A::~A is not permitted"
                (package-name pkg) name))
      ((zerop (length name)) (%refuse "the empty symbol || is not a term"))
      ((string= name "NIL") nil)
      ((string= name "T") t)
      ((char= (char name 0) #\?)
       (pushnew sym (gc-vars ctx))
       sym)
      ((gethash name (gc-schema ctx)))
      ((gethash name (gc-control ctx)))
      (t
       (%refuse "~A is not a Prolog functor, a schema name of this ~
graph, or a ?variable" (string-downcase name))))))

(defun %guard-goal (form ctx)
  "FORM (a cons) validated and translated as a goal."
  (let ((n (ignore-errors (list-length form))))
    (unless n
      (%refuse "a dotted or improper list is not a goal"))
    (let ((head (first form))
          (arity (1- n)))
      (unless (and head (symbolp head))
        ;; A string head is the sharp case: COMPILE-BODY hands it to
        ;; PROLOG-COMPILER-MACRO, which INTERNS it into GRAPH-DB
        ;; (prologc.lisp:225).  Symbols only.
        (%refuse "a goal's head must be a symbol, not ~A"
                 (%term-label head)))
      (let ((name (symbol-name head))
            (pkg (symbol-package head)))
        (when (and pkg (not (eq pkg (gc-package ctx))))
          (%refuse "package-qualified goal ~A::~A is not permitted"
                   (package-name pkg) name))
        ;; Routing, not home: an excluded name is refused however it got
        ;; into the registry, because the compiler will route it to the
        ;; engine's macro by name anyway (GH #279).
        (when (and (%excluded-predicate-p name)
                   (%routes-to-engine-control-p name))
          (%refuse "~A/~D is not available in free text: a goal spelled ~
~A is compiled by the engine's own control macro whatever package it ~
comes from"
                   (string-downcase name) arity (string-downcase name)))
        (let* ((home (gethash (cons name arity) (gc-functors ctx)))
               (canonical (or (gethash name (gc-control ctx))
                              ;; Bounded: NAME came out of the registry
                              ;; by string match, so this interns only
                              ;; names the image already registered.
                              (and home (intern name home)))))
          (unless canonical
            (%refuse "~A/~D is not a registered Prolog functor"
                     (string-downcase name) arity))
          (when (member name *prolog-goal-argument-control* :test #'string=)
            (dolist (arg (rest form))
              (unless (or (consp arg) (%cut-symbol-p arg ctx))
                (%refuse "~A takes goals, not ~A: write the goal out ~
in full -- a variable there would be meta-called at run time"
                         (string-downcase name) (%term-label arg)))))
          (cons canonical
                (mapcar (lambda (arg) (%guard-term arg ctx))
                        (rest form))))))))

(defun %guard-term (x ctx)
  "X validated and translated.  Every list is validated AS A GOAL:
free-standing list data is not expressible here, which is what keeps an
unvetted symbol or string out of head position at run time."
  (cond ((null x) nil)
        ((consp x) (%guard-goal x ctx))
        ((symbolp x) (%guard-symbol x ctx))
        ((or (numberp x) (stringp x) (characterp x)) x)
        (t (%refuse "~A is not a permitted literal" (%term-label x)))))

(defun %guard-query (forms ctx)
  "FORMS validated and translated.  Returns (values vars goals): VARS
in first-appearance order, which is the column order of the answer."
  (unless forms (%refuse "the query is empty"))
  (dolist (f forms)
    (unless (or (consp f) (%cut-symbol-p f ctx))
      (%refuse "~A is not a goal; a goal is a parenthesised form"
               (%term-label f))))
  (let ((goals (mapcar (lambda (f) (%guard-term f ctx)) forms)))
    (let ((vars (reverse (gc-vars ctx))))
      (unless vars
        (%refuse "the query binds no ?variables, so it can return no ~
columns"))
      (values vars goals))))

(defun %prolog-functor-names ()
  "Every predicate a free-text query may name, in the wire's kebab
spelling: the registered functors plus the control constructs.  Feeds
the editor's overlay mode, which dims a head it does not know here."
  (let ((names '()))
    (maphash (lambda (key pkg)
               (declare (ignore pkg))
               (pushnew (string-downcase (car key)) names :test #'string=))
             (%functor-whitelist))
    (maphash (lambda (name sym)
               (declare (ignore sym))
               (pushnew (string-downcase name) names :test #'string=))
             (%control-word-table))
    (sort names #'string<)))

(defun %non-finite-p (x)
  "True for a float that is not a finite number.  A comparison that
itself signals counts too: this feeds a refusal screen (GH #309)."
  (and (floatp x)
       (handler-case
           (let ((most (etypecase x
                         (single-float most-positive-single-float)
                         (double-float most-positive-double-float)
                         (short-float most-positive-short-float)
                         (long-float most-positive-long-float))))
             (or (/= x x) (> x most) (< x (- most))))
         (arithmetic-error () t))))

(defun %screen-non-finite (form)
  "Refuse FORM if any float in it is non-finite.  Not every reader
rejects an overflowing literal: ECL reads 1e999999 as infinity, which
would sail past the read guard and 500 at JSON encoding (GH #309)."
  (cond ((consp form)
         (%screen-non-finite (car form))
         (%screen-non-finite (cdr form)))
        ((%non-finite-p form)
         (%refuse "the query could not be read: check the quoting and ~
the numeric literals"))))

(defun %read-guarded-forms (text scratch ctx)
  "TEXT screened, read into SCRATCH, and guarded through CTX -- steps 2,
3 and 4 in order.  The screen runs FIRST and on the raw characters:
once READ has resolved a package-qualified name the interning it was
meant to prevent has already happened.  Every way READ can fail is a
refusal, so a malformed query is a 400 and never a 500."
  (%scan-query-text text)
  (let ((forms (handler-case (%read-query-forms text scratch)
                 (prolog-guard-error (c) (error c))
                 (end-of-file ()
                   (%refuse "the query ends mid-form: unbalanced ~
parentheses or an unterminated string"))
                 ;; A reader condition's report is raw implementation
                 ;; text (SBCL spells out its internal reader-error
                 ;; classes), so it is logged, not echoed.  1/0 and (on
                 ;; SBCL) 1e999999 land here; a reader that instead
                 ;; produces infinity is caught by %SCREEN-NON-FINITE
                 ;; below (GH #309).
                 (error (c)
                   (log:error "GUI prolog: unreadable query: ~A" c)
                   (%refuse "the query could not be read: check the ~
quoting and the numeric literals")))))
    (%screen-non-finite forms)
    (%guard-query forms ctx)))

(defvar *no-applicable-method-type*
  (or #+sbcl (find-symbol "NO-APPLICABLE-METHOD-ERROR" "SB-PCL")
      #+ccl (find-symbol "NO-APPLICABLE-METHOD-EXISTS" "CCL")
      nil)
  "The implementation's condition class for a generic function called
with arguments no method matches, or NIL where it has none.  ANSI
defines the NO-APPLICABLE-METHOD generic but no condition class, so
this is looked up by name once at load.  Belt-and-suspenders since GH
#309: the three whitelisted read generics define their own
NO-APPLICABLE-METHOD methods signalling QUERY-PRECONDITION-ERROR, which
%ILL-TYPED-CONDITION-P admits on every implementation -- necessary
because ECL has no distinct class here (it signals a bare SIMPLE-ERROR,
which is deliberately a 500).")

(defvar *unknown-claim-family-type* nil
  "GRAPH-DB.SPACETIME:UNKNOWN-CLAIM-FAMILY once %UNKNOWN-CLAIM-FAMILY-TYPE
has resolved it, else NIL.")

(defun %unknown-claim-family-type ()
  "The condition class a claim-family name no DEF-CLAIM-CLASSES
registered signals, or NIL where GRAPH-DB.SPACETIME is not loaded;
cached on the first success.  Deferred to CALL time, unlike
*NO-APPLICABLE-METHOD-TYPE*: GRAPH-DB/QUERY depends on GRAPH-DB/CORE
alone and must load standalone, so a load-time FIND-SYMBOL would cache
NIL for the life of the image and CLAIM/7's ill-typed family name would
report as an engine fault (GH #330)."
  (or *unknown-claim-family-type*
      (let ((pkg (find-package "GRAPH-DB.SPACETIME")))
        (and pkg
             (setf *unknown-claim-family-type*
                   (find-symbol "UNKNOWN-CLAIM-FAMILY" pkg))))))

(defun %ill-typed-condition-p (c)
  "True when C is a shape that CLIENT INPUT is known to produce, as
opposed to an engine defect.  The two were MEASURED against the
whitelisted read functors, not assumed (GH #279):

  NO-APPLICABLE-METHOD -- an unbound Prolog variable reaching a generic
    that dispatches on node classes: (outgoing-edges ?a ?b),
    (incoming-edges ?a ?b), (invoke-view ?a ?b ?c ?d).

  QUERY-PRECONDITION-ERROR -- how the query layer reports a failed
    precondition it checked on purpose: \"No secondary index on ?2.?3
    in :G\" for (find-by-slot ?a ?b ?c ?d), and the not-spatially-indexed
    report from %RESOLVE-SPATIAL-SCOPE for (find-nearest ?n some-type
    0 0 5).  Both were SIMPLE-ERRORs before GH #286.

  UNKNOWN-CLAIM-FAMILY -- a claim family name graph-db/rules' CLAIM/7
    was handed that DEF-CLAIM-CLASSES never registered, e.g. an arity
    subclass where the parent was meant.  The name is a schema type the
    guard admits, so only the goal can reject it (GH #330).

TYPE-ERROR and UNBOUND-VARIABLE are deliberately NOT here, though they
are the obvious guesses.  No client input produced either: the read
functors guard with NUMBERP / NODE-P and simply fail instead of
signalling.  A TYPE-ERROR is instead the classic shape of a real defect
-- a NIL where a struct was expected -- so admitting it would relabel
exactly the fault this split exists to surface.

Since GH #286 the engine signals those preconditions as
QUERY-PRECONDITION-ERROR, so that class is what is admitted here; a
plain SIMPLE-ERROR -- an internal (error \"...\") or a failed ASSERT --
is a defect again and answers 500, which closes the residual the first
cut recorded."
  (let ((family-type (%unknown-claim-family-type)))
    (or (typep c 'graph-db:query-precondition-error)
        (and *no-applicable-method-type*
             (typep c *no-applicable-method-type*))
        (and family-type (typep c family-type)))))

;;; ---------------------------------------------------------------------
;;; Step 5-6: the runner, exported (spec SS4, GH #322)
;;; ---------------------------------------------------------------------

(defun %clamp-cap (limit)
  (if (and (integerp limit) (plusp limit))
      (min limit graph-db::*query-default-limit*)
      graph-db::*query-default-limit*))

(defun %probe (cap)
  "One past CAP tells truncated from exactly full; at the ceiling there
is no room, so an exactly-full page reads as truncated (GH #278)."
  (if (< cap graph-db::*query-default-limit*) (1+ cap) cap))

(defun %run-guarded-goals (vars goals graph probe)
  "The already-guarded query's rows, RAW, at most PROBE of them, with
the GUI's three-way condition contract (GH #279).  No :PACKAGE is
passed -- each head resolves in its own package (GH #322)."
  (handler-case
      (let ((rows '()))
        (graph-db::run-query-goals
         vars goals graph :limit probe :format :raw
         :callback (lambda (row) (push row rows)))
        (nreverse rows))
    (graph-db:prolog-error (c) (error c))
    (graph-db:query-param-error (c) (error c))
    (error (c)
      (cond ((%ill-typed-condition-p c)
             (log:error "query guard: ill-typed query (~S): ~A"
                        (type-of c) c)
             (error 'prolog-ill-typed-error))
            (t
             (log:error "query guard: UNEXPECTED SERVER FAULT (~S): ~A"
                        (type-of c) c)
             (error 'prolog-server-fault))))))

(defun run-guarded-prolog (text graph &key limit max-inferences timeout
                                            (format :data))
  "Screen, read, guard and run TEXT against GRAPH; (VALUES COLUMNS ROWS
TRUNCATED-P).  COLUMNS are the variables in first-appearance order as
camelCase wire spelling; ROWS one list per solution, cells JSON-shaped
under :DATA (a node is its id string; strings, numbers, T, NIL pass) or
as bound under :RAW.  LIMIT is clamped to *QUERY-DEFAULT-LIMIT*;
MAX-INFERENCES and TIMEOUT bind the DSL's budgets for this call.
Refusals signal PROLOG-GUARD-ERROR; see the header for the rest of the
condition contract (spec SS4, GH #322)."
  (check-type format (member :data :raw))
  (let* ((scratch (%make-scratch-package))
         (cap (%clamp-cap limit))
         (probe (%probe cap))
         (graph-db::*query-default-max-inferences*
           (or max-inferences graph-db::*query-default-max-inferences*))
         (graph-db::*query-default-timeout*
           (or timeout graph-db::*query-default-timeout*)))
    (unwind-protect
         (multiple-value-bind (vars goals)
             (%read-guarded-forms text scratch (%guard-context graph scratch))
           (let* ((rows (%run-guarded-goals vars goals graph probe))
                  (n (length rows))
                  (truncated (if (> probe cap) (> n cap) (>= n cap)))
                  (shown (if (> n cap) (subseq rows 0 cap) rows)))
             (values (mapcar #'graph-db::%query-var-field vars)
                     (if (eq format :data)
                         (mapcar (lambda (row)
                                   (mapcar #'graph-db::%query-value->json row))
                                 shown)
                         shown)
                     (and truncated t))))
      (delete-package scratch))))
