;;;; Free-text Prolog HTTP surface, behind START-GUI's :ALLOW-PROLOG
;;;; flag.  The guard and the runner live in GRAPH-DB.QUERY; only the
;;;; flag, the body helpers and the envelope stay here (GH #279, #322).

(in-package #:graph-db.gui)

;;; ---------------------------------------------------------------------
;;; The flag
;;; ---------------------------------------------------------------------

(defvar *allow-prolog* nil
  "True when the running GUI accepts free-text Prolog.  START-GUI sets
it from :ALLOW-PROLOG (default NIL) each time it actually starts a
server; calling START-GUI against an already-running GUI changes
nothing, this included.  Restart to change the flag.")

(defparameter *prolog-internal-error-message*
  "An internal error occurred while running the query."
  "The ONLY thing a server fault tells the client.  Fixed text, never
the condition's report: this endpoint answers untrusted text, and a
fault's report names engine internals just as an ill-typed goal's does.
The detail goes to the log under an UNEXPECTED label (GH #279).")

;;; ---------------------------------------------------------------------
;;; The endpoint
;;; ---------------------------------------------------------------------

;; What this server offers the frontend.  Server-level, not per-graph:
;; the roster is a list of graphs and has nowhere to hang a server-wide
;; flag, and the page needs the answer at boot, before any graph is
;; selected.  The functor inventory ships only when the flag is on; it
;; is what the editor dims unknown heads against.
(def-gui-handler api-capabilities (params)
  (%json-response
   (list (cons :allow-prolog (%bool *allow-prolog*))
         (cons :prolog
               (if *allow-prolog*
                   (%obj
                    (list
                     (cons :max-query-length
                           graph-db.query:*prolog-max-query-length*)
                     (cons :max-depth
                           graph-db.query:*prolog-max-depth*)
                     (cons :functors
                           (%arr
                            (graph-db.query::%prolog-functor-names)))))
                   (%maybe nil))))))

(defun %prolog-request-body ()
  "The request body decoded as JSON with its object KEYS LEFT AS
STRINGS, or :MALFORMED.  cl-json's default decoder INTERNS every key it
meets as a keyword, so a client could grow the KEYWORD package one
bogus field per request; nothing on this path needs a symbol (GH #279).
Since GH #284 the shared decoder interns the DSL's own keys -- \"limit\"
among them -- so this endpoint, which reads its two fields by string,
asks for none."
  (%request-json-body :intern-dsl-keys nil))

(defun %prolog-field (body name)
  (cdr (assoc name body :test #'equal)))

(defun %prolog-request-text (body)
  "The \"query\" string of a decoded request BODY, or a refusal."
  (let ((text (%prolog-field body "query")))
    (unless (stringp text)
      (graph-db.query::%refuse "the request needs a \"query\" string"))
    (when (> (length text) graph-db.query:*prolog-max-query-length*)
      (graph-db.query::%refuse "query is ~D characters; the limit is ~D"
                               (length text)
                               graph-db.query:*prolog-max-query-length*))
    text))

(defun %run-guarded-prolog (text limit graph)
  "The workbench envelope over GRAPH-DB.QUERY:RUN-GUARDED-PROLOG (GH
#322).  The runner clamps and probes exactly as the DSL endpoint does,
so the envelope's CAP/TRUNCATED come straight from its own answer."
  (let ((cap (%clamp-row-cap limit)))
    (multiple-value-bind (columns rows truncated)
        (graph-db.query:run-guarded-prolog text graph :limit cap)
      (%query-envelope-from-rows columns rows cap truncated))))

;; The flag is checked FIRST -- before the graph is resolved -- so a GUI
;; started without :ALLOW-PROLOG answers identically whatever graph is
;; named, and the UI hiding the tab is decoration, not the control.
(def-gui-handler api-graph-prolog (params)
  (if (not *allow-prolog*)
      (gui-error 403 "prolog-disabled"
                 (format nil "Free-text Prolog is disabled on this ~
server; start the GUI with :ALLOW-PROLOG T to enable it"))
      (with-gui-graph (graph params)
        (let ((body (%prolog-request-body)))
          (cond
            ((eq body :malformed)
             (gui-error 400 "malformed-json"
                        "Request body is not valid JSON"))
            ((not (and (listp body) (every #'consp body)))
             (gui-error 400 "malformed-query"
                        "Request body must be a JSON object"))
            (t
             (handler-case
                 (%run-guarded-prolog (%prolog-request-text body)
                                      (%prolog-field body "limit")
                                      graph)
               (graph-db.query:prolog-guard-error (c)
                 (gui-error 400 "refused-query"
                            (graph-db.query:prolog-guard-error-reason c)))
               ;; Same mapping unit B's endpoint uses for the same
               ;; conditions.  The two resource/permission subtypes must
               ;; precede PROLOG-ERROR: HANDLER-CASE takes the first
               ;; matching clause, not the most specific.
               (graph-db:query-param-error (c)
                 (gui-error 400 "bad-query"
                            (graph-db::query-param-error-reason c)))
               (graph-db:prolog-resource-error (c)
                 (declare (ignore c))
                 (gui-error 400 "query-too-expensive"
                            "Query exceeded its resource limits"))
               (graph-db:prolog-permission-error (c)
                 (declare (ignore c))
                 (gui-error 403 "forbidden-operation"
                            "Query attempted a forbidden operation"))
               (graph-db:prolog-cost-unbounded-error (c)
                 ;; The engine's #285 rail: admitted by the whitelist
                 ;; (home-scoped), refused by the engine because the
                 ;; resource bounds cannot cover one atomic call.  Its
                 ;; own code, so the layers stay distinguishable.
                 (gui-error 400 "cost-unbounded-goal"
                            (princ-to-string c)))
               (graph-db:prolog-error (c)
                 (declare (ignore c))
                 (gui-error 400 "bad-query"
                            "Not a well-formed Prolog goal sequence"))
               (graph-db.query:prolog-ill-typed-error (c)
                 (declare (ignore c))
                 (gui-error 400 "ill-typed-query"
                            (format nil "A goal was given arguments ~
its predicate cannot use.  Check that each goal's arguments are bound ~
the way that predicate needs.")))
               (graph-db.query:prolog-server-fault (c)
                 (declare (ignore c))
                 (gui-error 500 "internal-error"
                            *prolog-internal-error-message*))
               ;; Anything from OUTSIDE the runner (scratch package,
               ;; JSON encoding).  DEF-GUI-HANDLER's own 500 arm would
               ;; PRINC-TO-STRING it; this endpoint is the one that
               ;; answers untrusted text, so it does not.  DELETE-PACKAGE
               ;; has already run -- a leak cannot hide behind a 500.
               (error (c)
                 (log:error "GUI prolog: UNEXPECTED HANDLER FAULT ~
(~S): ~A" (type-of c) c)
                 (gui-error 500 "internal-error"
                            *prolog-internal-error-message*)))))))))
