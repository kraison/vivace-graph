;;;; Structured JSON query DSL: compiler + runner (GH #44, extracted in
;;;; GH #278).
;;;;
;;;; This cluster used to live in rest.lisp but is not REST-specific --
;;;; the GUI workbench (POST /api/graphs/:name/query) compiles through
;;;; the same implementation.  rest.lisp keeps only its HTTP wrappers
;;;; (%REQUEST-QUERY-DSL, CALL-REST-PATTERN-QUERY, DEF-QUERY, the
;;;; routes).  Moved verbatim: bodies here are byte-identical to the
;;;; rest.lisp originals, including their pre-80-column line widths.
;;;;
;;;; Home: the graph-db/query subsystem (GH #322), which depends on
;;;; graph-db/core only.  The one web-bound line this file had -- the
;;;; :NDJSON arm setting a content type on NINGLE:*RESPONSE* -- moved to
;;;; rest.lisp's %SET-NDJSON-CONTENT-TYPE, called by both the /query
;;;; route and DEF-QUERY when they ask for ndjson.

(in-package :graph-db)

(defvar *query-default-limit* 1000
  "Maximum solutions a DEF-QUERY returns unless it overrides :LIMIT.")
(defvar *query-default-max-inferences* 1000000
  "Inference budget for a DEF-QUERY unless it overrides :MAX-INFERENCES.")
(defvar *query-default-timeout* 30
  "Wall-clock seconds a DEF-QUERY may run unless it overrides :TIMEOUT.")

(defvar *pattern-query-callback* nil
  "Per-row callback for an EVAL'd ad-hoc pattern query (which has no lexical
environment to capture one); RUN-PATTERN-QUERY binds it around the SELECT.")

(define-condition query-param-error (query-precondition-error)
  ((reason :initarg :reason :reader query-param-error-reason))
  (:report (lambda (c s)
             (format s "Query parameter error: ~A" (query-param-error-reason c)))))

(defun %query-var-name (var)
  "The bare name of a query variable: ?min-age -> \"MIN-AGE\"."
  (string-left-trim "?" (symbol-name var)))

(defun %query-var-key (var)
  "The *QUERY-PARAMS* key for VAR: ?min-age -> :MIN-AGE."
  (intern (%query-var-name var) :keyword))

(defun %query-var-field (var)
  "The JSON field name for VAR: ?min-age -> \"minAge\"."
  (json:lisp-to-camel-case (%query-var-name var)))

(defun %query-value->json (v)
  "Render a query result value as a JSON-encodable datum: a node becomes its id
string, a non-keyword symbol its name, an UNBOUND query variable JSON null;
scalars pass through.

An unbound variable is a legitimate ANSWER, not a client error: var/1 and atom/1
exist to be called on one, and unifying two fresh variables succeeds with both
still unbound.  It arrives here as a VAR STRUCT (prologc.lisp:97) whose
:PRINT-FUNCTION renders it \"?1\" -- which is why it looks like a symbol in a
backtrace but is not one, so it matched neither NODE-P nor SYMBOLP, fell through
to the identity branch and reached cl-json as a raw struct
(JSON:UNENCODABLE-VALUE-ERROR, a 500).  A BOUND variable is dereferenced first,
so only a genuinely unbound one becomes null (GH #279).

A slot whose stored value is NIL is JSON null too, and T is JSON true: both
are symbols, and the SYMBOLP branch used to render them as the strings
\"NIL\" and \"T\" (GH #282).  Other symbols are still their names."
  (setq v (var-deref v))
  (cond ((var-p v) nil)                 ; unbound -> JSON null
        ((null v) nil)                  ; an empty slot -> JSON null
        ((eq v t) t)                    ; -> JSON true
        ((node-p v) (string-id v))
        ((keywordp v) v)
        ((symbolp v) (symbol-name v))
        (t v)))

(defun query-row->alist (return-vars row)
  "One result ROW (a list of values aligned with RETURN-VARS) as a JSON object
alist keyed by the camelCase result-variable names."
  (mapcar (lambda (var val) (cons (%query-var-field var) (%query-value->json val)))
          return-vars row))

(defun query-results->json (return-vars tuples)
  "Encode TUPLES (one row per solution) as a JSON array of objects.

Rows go through ENCODE-JSON-ALIST rather than cl-json's guessing encoder.
(CONS key NIL) is not a dotted pair, so a row whose values are ALL null -- which
(= ?x ?y) and (var ?x) legitimately produce -- was guessed to be a plain list
and came out as [[\"x\"],[\"y\"]] instead of {\"x\":null,\"y\":null}.
ENCODE-JSON-ALIST forces the object and still encodes each VALUE with the
ordinary encoder, so a list-valued slot is still a JSON array -- which the
explicit encoder would NOT be: it would read the list as its own markup
(GH #279).

An empty result stays \"null\", exactly as before; widening it to [] would be a
second, unrelated change to the wire."
  (if (null tuples)
      (json:encode-json-to-string nil)
      (with-output-to-string (out)
        (json:with-array (out)
          (dolist (row tuples)
            (json:as-array-member (out)
              (json:encode-json-alist
               (query-row->alist return-vars row) out)))))))

(defun emit-query-results (return-vars format run)
  "Render a query's results.  RUN is a function of one argument -- a per-row
callback -- that runs the query, invoking the callback for each result row as it
is produced (via SELECT :callback, so no intermediate result list is built).
With FORMAT :JSON returns a JSON array; with :NDJSON returns each row as its own
JSON line; the caller sets the content type."
  (ecase format
    (:json
     (let ((rows '()))
       (funcall run (lambda (row) (push row rows)))
       (query-results->json return-vars (nreverse rows))))
    (:ndjson
     (with-output-to-string (out)
       (funcall run
                (lambda (row)
                  ;; ENCODE-JSON-ALIST, not ENCODE-JSON: same all-null-row
                  ;; hazard as QUERY-RESULTS->JSON (GH #279).
                  (json:encode-json-alist
                   (query-row->alist return-vars row) out)
                  (terpri out)))))))

;;; ---------------------------------------------------------------------------
;;; Constrained JSON pattern queries (#44, tier 2).
;;;
;;; A client may POST an ad-hoc, read-only query as a JSON object compiled to a
;;; bounded SELECT -- no server-authored template, no client Lisp.  The shape:
;;;
;;;   {"match":  [ {"vertex":"?p","type":"gPerson"},
;;;                {"edge":"gKnows","from":"?p","to":"?f"} ],
;;;    "where":  [ {"slot":"?f","name":"name","bind":"?fname"},
;;;                {"slot":"?p","name":"name","value":"Alice"},
;;;                {"compare":"<","args":["?age",30]} ],
;;;    "select": ["?fname"],
;;;    "limit":  50, "skip": 0}
;;;
;;; Each goal is built from a fixed set of safe pattern kinds (no arbitrary
;;; predicate naming).  Type/edge names are resolved against the live schema (an
;;; unknown one is a 400), which also yields the schema package the query is
;;; compiled in.  The query runs read-only (:effects nil), under one MVCC
;;; snapshot, capped by the *QUERY-DEFAULT-* bounds; a breach is a 400.
;;; ---------------------------------------------------------------------------

(defun %dsl-var-or-literal (v)
  "Map a decoded JSON value to a Prolog term: a \"?x\" string becomes a query
variable symbol (variables are matched by name, so the package is irrelevant);
any other value is a literal (string/number/boolean) used as-is."
  (if (and (stringp v) (plusp (length v)) (char= (char v 0) #\?))
      (intern (string-upcase v) :graph-db)
      v))

;;; JSON bodies are decoded with STRING keys and only the DSL's own
;;; vocabulary is turned into keywords (GH #284).  cl-json's default
;;; decoder interned every object key it met, so an unauthenticated
;;; caller could grow the image's KEYWORD package one bogus field per
;;; request; the free-text Prolog endpoint (GH #279) already decoded by
;;; string, this makes the builder and REST pattern-query bodies do the
;;; same.  An unknown key stays a string, and (ASSOC :FOO ...) simply
;;; misses it.

(defparameter +dsl-json-keys+
  '("match" "where" "select" "limit" "skip" "format"
    "vertex" "edge" "type" "from" "to"
    "slot" "name" "bind" "value" "compare" "args")
  "Every object key the pattern-query DSL reads (RUN-PATTERN-QUERY and its
helpers).  The whole vocabulary a decoded body may intern.")

(defun %dsl-intern-known-keys (value)
  "VALUE (a string-keyed decode) with every DSL key replaced by its keyword,
recursively through nested objects and arrays; unknown keys stay strings."
  (cond ((and (consp value) (consp (car value)) (stringp (caar value)))
         ;; a JSON object: alist of (string . value)
         (mapcar (lambda (pair)
                   (cons (if (member (car pair) +dsl-json-keys+ :test #'string=)
                             (intern (string-upcase (car pair)) :keyword)
                             (car pair))
                         (%dsl-intern-known-keys (cdr pair))))
                 value))
        ((consp value) (mapcar #'%dsl-intern-known-keys value))
        (t value)))

(defun decode-json-string-keys (string)
  "Decode JSON STRING with every object key left a STRING -- nothing is
interned, whatever a client sends (GH #279, #284)."
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string string)))

(defun decode-dsl-json (string)
  "Decode a pattern-query JSON body STRING into the alist RUN-PATTERN-QUERY
reads, interning only the DSL's own keys (GH #284).  The one decoder for
the REST /query route and the GUI builder."
  (%dsl-intern-known-keys (decode-json-string-keys string)))

(defun %dsl-keyword (name)
  "A client type or slot NAME as a keyword.  Two spellings arrive: the
engine's own wire form -- lowercase kebab, exactly what the GUI's /types
and /stats emit -- goes in verbatim, upcased; legacy camelCase (any
uppercase letter present) folds through CAMEL-CASE-TO-LISP: \"minAge\" ->
:MIN-AGE.  Never the latter for a kebab name: it inserts a hyphen before
every digit, so \"foo-bar2\" became :FOO-BAR-2 and the schema's own
spelling was refused (GH #281)."
  (intern (if (some #'upper-case-p name)
              (string-upcase (json:camel-case-to-lisp name))
              (string-upcase name))
          :keyword))

(defun %dsl-resolve-type (name parent graph)
  "Resolve a vertex/edge type NAME to its canonical class symbol via GRAPH's
schema (PARENT is :vertex or :edge).  Returns (values symbol schema-package).
Signals QUERY-PARAM-ERROR for an unknown type."
  (unless (stringp name)
    (error 'query-param-error :reason (format nil "~(~A~) type must be a string" parent)))
  (let ((meta (handler-case
                  (lookup-node-type-by-name (%dsl-keyword name) parent
                                            :graph graph)
                (ambiguous-node-type-name (c)
                  (error 'query-param-error
                         :reason (format nil "ambiguous ~(~A~) type '~A': ~
one of ~{~A~^, ~}"
                                         parent name
                                         (mapcar
                                          #'%qualified-type-name-string
                                          (ambiguous-type-candidates
                                           c))))))))
    (unless meta
      (error 'query-param-error
             :reason (format nil "unknown ~(~A~) type '~A'" parent name)))
    (values (node-type-name meta)
            (or (find-package (node-type-package meta)) (find-package :graph-db)))))

(defparameter *dsl-compare-ops*
  '(("<" . <) (">" . >) ("<=" . <=) (">=" . >=) ("=" . =) ("==" . ==) ("/=" . /=))
  "Comparison operators a pattern query may use, mapped to their Prolog functors.")

(defun %compile-match-pattern (pat graph)
  "Compile one MATCH pattern object (an alist) to a goal; second value is the
schema package (or NIL)."
  (cond
    ((assoc :vertex pat)
     (multiple-value-bind (sym pkg)
         (%dsl-resolve-type (cdr (assoc :type pat)) :vertex graph)
       (values (list 'is-a (%dsl-var-or-literal (cdr (assoc :vertex pat))) sym) pkg)))
    ((assoc :edge pat)
     (multiple-value-bind (sym pkg)
         (%dsl-resolve-type (cdr (assoc :edge pat)) :edge graph)
       (values (list sym
                     (%dsl-var-or-literal (cdr (assoc :from pat)))
                     (%dsl-var-or-literal (cdr (assoc :to pat))))
               pkg)))
    (t (error 'query-param-error
              :reason (format nil "unrecognized match pattern ~S" pat)))))

(defun %compile-where-constraint (con)
  "Compile one WHERE constraint object (an alist) to a goal."
  (cond
    ((assoc :slot con)
     (let ((var (%dsl-var-or-literal (cdr (assoc :slot con))))
           (name (cdr (assoc :name con)))
           (bind (assoc :bind con))
           (value (assoc :value con)))
       (unless (stringp name)
         (error 'query-param-error :reason "slot constraint needs a string \"name\""))
       (cond (bind  (list 'node-slot-value var (%dsl-keyword name)
                          (%dsl-var-or-literal (cdr bind))))
             (value (list 'node-slot-value var (%dsl-keyword name) (cdr value)))
             (t (error 'query-param-error
                       :reason "slot constraint needs \"bind\" or \"value\"")))))
    ((assoc :compare con)
     (let ((op (cdr (assoc (cdr (assoc :compare con)) *dsl-compare-ops* :test #'equal)))
           (args (cdr (assoc :args con))))
       (unless op
         (error 'query-param-error
                :reason (format nil "unsupported comparison '~A'" (cdr (assoc :compare con)))))
       (unless (and (listp args) (= 2 (length args)))
         (error 'query-param-error :reason "compare needs exactly two \"args\""))
       (cons op (mapcar #'%dsl-var-or-literal args))))
    (t (error 'query-param-error
              :reason (format nil "unrecognized where constraint ~S" con)))))

(defun compile-pattern-query (dsl graph)
  "Compile a decoded JSON pattern query DSL (an alist) for GRAPH.  Returns
(values select-vars goals limit skip schema-package).  Signals
QUERY-PARAM-ERROR on malformed input."
  (let ((pkg nil) (goals nil))
    (dolist (pat (cdr (assoc :match dsl)))
      (multiple-value-bind (goal p) (%compile-match-pattern pat graph)
        (when (and p (null pkg)) (setf pkg p))
        (push goal goals)))
    (dolist (con (cdr (assoc :where dsl)))
      (push (%compile-where-constraint con) goals))
    (let ((select (cdr (assoc :select dsl))))
      (unless (and (listp select) select)
        (error 'query-param-error
               :reason "query must specify a non-empty \"select\" list"))
      (values (mapcar #'%dsl-var-or-literal select)
              (nreverse goals)
              (cdr (assoc :limit dsl))
              (cdr (assoc :skip dsl))
              (or pkg (find-package :graph-db))))))

(defun run-query-goals (vars goals graph
                        &key (package (find-package :graph-db))
                             limit skip (format :json) callback)
  "Run the already-compiled query GOALS against GRAPH, collecting VARS.

This is THE runner: every client-supplied query -- the JSON pattern DSL and
the GUI's free-text Prolog alike -- reaches SELECT through here, so the rails
are stated once.  Read-only (:EFFECTS NIL), snapshot-isolated, bounded by
*QUERY-DEFAULT-MAX-INFERENCES* / *QUERY-DEFAULT-TIMEOUT*, and LIMIT capped at
*QUERY-DEFAULT-LIMIT*.  PACKAGE no longer routes functor resolution: since
GH #322, MAKE-FUNCTOR-SYMBOL resolves each goal head in its own home
package (falling back to GRAPH-DB), so a goal list spanning the engine
and a schema resolves correctly regardless of *PACKAGE*.  PACKAGE stays
advisory -- it is what COMPILE-PATTERN-QUERY resolved the DSL's own
match/where types against, and is still bound around the EVAL, but
nothing here depends on it for a goal head to be found any more.
Callers built from untrusted text must whitelist every symbol BEFORE
calling (see gui/prolog.lisp, GH #279).  Returns the result string
under :JSON/:NDJSON.

:RAW is a fourth arm (GH #322): CALLBACK (required) receives each raw
row -- no JSON rendering -- and the function returns NIL, for a caller
that wants the bound values themselves (graph-db.query:run-guarded-
prolog's :DATA format still converts them; :RAW does not)."
  (when (and (eq format :raw) (not callback))
    (error "RUN-QUERY-GOALS :FORMAT :RAW requires :CALLBACK."))
  ;; The eval'd SELECT / node-slot-value goals key off *GRAPH*.
  (let* ((*graph* graph)
         (*package* package)
         (cap (if (and (integerp limit) (plusp limit))
                  (min limit *query-default-limit*)
                  *query-default-limit*))
         (run (lambda (cb)
                ;; the select form is EVAL'd (null lexenv), so pass the
                ;; callback through a special the form references
                ;; rather than a lexical.
                (let ((*pattern-query-callback* cb))
                  (eval `(select (:effects nil :snapshot t
                                  :limit ,cap
                                  :skip ,(when (integerp skip) skip)
                                  :max-inferences
                                  ,*query-default-max-inferences*
                                  :timeout ,*query-default-timeout*
                                  :callback *pattern-query-callback*)
                                 ,vars ,@goals))))))
    (if (eq format :raw)
        (progn (funcall run callback) nil)
        (emit-query-results vars format run))))

(defun %dsl-ndjson-p (dsl)
  "T when the decoded JSON pattern query DSL asks for \"format\":\"ndjson\"."
  (string-equal "ndjson" (princ-to-string (or (cdr (assoc :format dsl)) ""))))

(defun run-pattern-query (dsl graph)
  "Compile and run a decoded JSON pattern query DSL against GRAPH, returning the
result string.  Read-only, snapshot-isolated, and bounded; the client :limit is
capped at *QUERY-DEFAULT-LIMIT*.  A \"format\":\"ndjson\" field streams the rows
as newline-delimited JSON instead of an array."
  (multiple-value-bind (vars goals limit skip pkg)
      (compile-pattern-query dsl graph)
    (run-query-goals vars goals graph
                     :package pkg :limit limit :skip skip
                     :format (if (%dsl-ndjson-p dsl) :ndjson :json))))
