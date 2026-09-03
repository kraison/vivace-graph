(in-package :graph-db)

(defvar *rest-port* 8080)
(defvar *rest-app* nil)
(defvar *clack-app* nil)
(defvar *rest-procedures*
  #+sbcl (make-hash-table :synchronized t :test 'equalp)
  #+lispworks (make-hash-table :single-thread nil :test 'equalp)
  #+ccl (make-hash-table :shared t :test 'equalp)
  #+ecl (make-hash-table :test 'equalp
                          #+graph-db-ecl-sync-hash :synchronized
                          #+graph-db-ecl-sync-hash t))
(defvar *rest-passwd-file* "rpasswd")
(defvar *htpasswd-bin* "/usr/bin/htpasswd")

;;; ---------------------------------------------------------------------------
;;; Named parameterized queries (#44).
;;;
;;; DEF-QUERY registers a server-authored, read-only graph query as a REST
;;; endpoint (POST /graph/:graph/query/<name>).  The server author owns the query
;;; shape and its safety bounds; the client supplies only typed, named
;;; parameters.  Each query runs through SELECT with safe defaults -- read-only
;;; (:effects nil), a single MVCC snapshot, a result limit, an inference budget,
;;; and a wall-clock timeout -- which the author may override per query.
;;; ---------------------------------------------------------------------------

(defvar *rest-queries*
  #+sbcl (make-hash-table :synchronized t :test 'equalp)
  #+lispworks (make-hash-table :single-thread nil :test 'equalp)
  #+ccl (make-hash-table :shared t :test 'equalp)
  #+ecl (make-hash-table :test 'equalp
                          #+graph-db-ecl-sync-hash :synchronized
                          #+graph-db-ecl-sync-hash t))

(defun coerce-query-param (raw type field)
  "Validate/convert request value RAW to TYPE (:string :integer :number :boolean
:keyword), signaling QUERY-PARAM-ERROR on a missing or malformed value.  FIELD is
the client-facing parameter name, used in error messages."
  (when (null raw)
    (error 'query-param-error
           :reason (format nil "missing required parameter '~A'" field)))
  (flet ((bad ()
           (error 'query-param-error
                  :reason (format nil "parameter '~A' is not a valid ~(~A~)"
                                  field type))))
    (ecase type
      (:string  (if (stringp raw) raw (princ-to-string raw)))
      (:integer (typecase raw
                  (integer raw)
                  (string (or (ignore-errors (parse-integer raw)) (bad)))
                  (t (bad))))
      (:number  (typecase raw
                  (number raw)
                  (string (let ((v (ignore-errors
                                     (let ((*read-eval* nil)) (read-from-string raw)))))
                            (if (numberp v) v (bad))))
                  (t (bad))))
      (:boolean (cond ((member raw '("true" "t" t) :test #'equalp) t)
                      ((member raw '("false" "f" "nil") :test #'equalp) nil)
                      ((null raw) nil)
                      (t (bad))))
      (:keyword (typecase raw
                  (string (intern (string-upcase raw) :keyword))
                  (symbol raw)
                  (t (bad)))))))

(defun coerce-query-params (specs req-params)
  "Build the *QUERY-PARAMS* alist for parameter SPECS ((?var type)...) from the
request alist REQ-PARAMS, coercing each value to its declared type."
  (mapcar (lambda (spec)
            (destructuring-bind (var type) spec
              (cons (%query-var-key var)
                    (coerce-query-param (get-param req-params (%query-var-field var))
                                        type (%query-var-field var)))))
          specs))

(defun query-format (params)
  "The requested response format: :NDJSON when the \"format\" parameter is
\"ndjson\" (newline-delimited JSON, one object per line), else :JSON (an array)."
  (if (string-equal "ndjson" (or (get-param params "format") ""))
      :ndjson
      :json))

(defun register-query (name fn)
  "Register query handler FN under NAME (both its symbol name and camelCase)."
  (setf (gethash (string name) *rest-queries*) fn)
  (setf (gethash (json:lisp-to-camel-case (string name)) *rest-queries*) fn)
  name)

(defun add-rest-user (username password)
  (trivial-shell:shell-command
   (format nil "~A -b ~A ~A ~A ~A"
           *htpasswd-bin*
           (if (probe-file *rest-passwd-file*) "" "-c")
           *rest-passwd-file*
           username
           password)))

(defun delete-rest-user (username)
  (trivial-shell:shell-command
   (format nil "~A -D ~A ~A"
           *htpasswd-bin*
           *rest-passwd-file*
           username)))

(defun auth-rest-user (username password)
  (with-open-file (stream *rest-passwd-file*)
    (do ((line (read-line stream nil :eof)
               (read-line stream nil :eof)))
        ((eql line :eof))
      (let ((pair (cl-ppcre:split "\\:" line)))
        (when (equalp (first pair) username)
          (destructuring-bind (_ hash salt enc)
              (cl-ppcre:split "\\$" (second pair))
            (declare (ignore _ enc))
            (multiple-value-bind (out err exit-code)
                (trivial-shell:shell-command
                 (format nil "openssl passwd -~A -salt ~A ~A"
                         hash salt password))
              (declare (ignore exit-code err))
              (setq out (cl-ppcre:regex-replace-all "\\s+$" out ""))
              (log:warn "GOT ~A FOR ~A" out (second pair))
              (return-from auth-rest-user
                (equalp out (second pair))))))))))

#|
  ;; This only works with newer versions of htpasswd;  see openssl version above
  (multiple-value-bind (out err exit-code)
      (trivial-shell:shell-command
       (format nil "~A -vb ~A ~A ~A"
               *htpasswd-bin*
               *rest-passwd-file*
               username
               password))
    (declare (ignore out err))
    (eq 0 exit-code)))
|#

(defmacro with-rest-auth ((username password) &body body)
  `(if (auth-rest-user ,username ,password)
       (progn
         ,@body)
       (progn
         (setf (lack.response:response-status ningle:*response*) 401)
         (json:encode-json-to-string
          (list
           (cons :error "Invalid credentials"))))))

(defmacro with-rest-graph ((graph-name) &body body)
  `(let ((*graph* (lookup-graph (intern (json:camel-case-to-lisp ,graph-name) :keyword))))
     (if (graph-p *graph*)
         (progn
           ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list
             (cons :error
                   (format nil "Unknown graph ~A" ,graph-name))))))))

(defmacro with-rest-vertex ((id) &body body)
  `(let ((vertex (lookup-vertex ,id)))
     (if vertex
         (progn
           ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list
             (cons :error
                   (format nil "Unknown vertex ~A" ,id))))))))

(defmacro with-rest-edge ((id) &body body)
  `(let ((edge (lookup-edge ,id)))
     (if edge
         (progn
           ,@body)
         (progn
           (setf (lack.response:response-status ningle:*response*) 404)
           (json:encode-json-to-string
            (list
             (cons :error
                   (format nil "Unknown edge ~A" ,id))))))))

(defmethod json-encode ((edge edge))
  (let* ((class (class-of edge))
         (class-name (class-name class)))
    (json-rpc::encode-json-to-string
     (nconc
      (list (cons :id (string-id edge))
            (cons :type (json:lisp-to-camel-case
                         (symbol-name class-name)))
            (cons :from (string-id (from edge)))
            (cons :to (string-id (to edge))))
      (mapcar (lambda (slot-name)
                (cons slot-name (slot-value edge slot-name)))
              (data-slots (find-class class-name)))))))

(defmethod json-encode-edge-list (seq)
  (json-rpc::encode-json-to-string
   (map 'list
        (lambda (edge)
          (let* ((class (class-of edge))
                 (class-name (class-name class)))
            (nconc
             (list (cons :id (string-id edge))
                   (cons :type (json:lisp-to-camel-case
                                (symbol-name class-name)))
                   (cons :from (string-id (from edge)))
                   (cons :to (string-id (to edge))))
             (mapcar (lambda (slot-name)
                       (cons slot-name (slot-value edge slot-name)))
                     (data-slots (find-class class-name))))))
        seq)))

(defmethod json-encode ((vertex vertex))
  (let* ((class (class-of vertex))
         (class-name (class-name class)))
    (json-rpc::encode-json-to-string
     (nconc
      (list (cons :id (string-id vertex))
            (cons :type (json:lisp-to-camel-case
                         (symbol-name class-name))))
      (mapcar (lambda (slot-name)
                (cons slot-name (slot-value vertex slot-name)))
              (data-slots (find-class class-name)))))))

(defmethod json-encode ((graph graph))
  (json-rpc::encode-json-to-string
   (list
    (cons :name (graph-name graph))
    (cons :type :graph)
    (cons :mode (if (typep graph 'slave-graph)
                    :read-only
                    :read-write))
    (cons :vertex-types
          (loop
             for key being the hash-keys
             in (gethash :vertex (schema-type-table (schema graph)))
             using (hash-value type-definition)
             if (numberp key)
             collecting
               (list (cons :name (node-type-name type-definition))
                     (cons :slots
                           (mapcar
                            (lambda (slot-def)
                              (list (cons :name (first slot-def))
                                    (cons :type
                                          (let ((type-pos (position :type slot-def)))
                                            (if type-pos
                                                (nth (1+ type-pos) slot-def)
                                                :any)))))
                            (node-type-slots type-definition))))))
    (cons :edge-types
          (loop
             for key being the hash-keys
             in (gethash :edge (schema-type-table (schema graph)))
             using (hash-value type-definition)
             if (numberp key)
             collecting
               (list (cons :name (node-type-name type-definition))
                     (cons :slots
                           (mapcar
                            (lambda (slot-def)
                              (list (cons :name (first slot-def))
                                    (cons :type
                                          (let ((type-pos (position :type slot-def)))
                                            (if type-pos
                                                (nth (1+ type-pos) slot-def)
                                                :any)))))
                            (node-type-slots type-definition)))))))))

(defun get-param (params name)
  (cdr (assoc name params :test 'equalp)))

(defmacro def-rest-procedure (name lambda-list &body body)
  (with-gensyms (params)
    `(let ((fn (lambda (,params)
                 (let (,@(mapcar (lambda (var)
                                   (list var
                                         `(get-param ,params
                                                     ,(json:lisp-to-camel-case
                                                       (symbol-name var)))))
                                 lambda-list))
                   (progn
                     ,@body)))))
       (setf (gethash ,name *rest-procedures*) fn)
       (setf (gethash ,(json:lisp-to-camel-case (symbol-name name)) *rest-procedures*) fn))))

(defun call-rest-procedure (name params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (let ((fn (gethash name *rest-procedures*)))
      (if (functionp fn)
          (with-rest-graph ((get-param params :graph-name))
            (funcall fn params))
          (progn
            (setf (lack.response:response-status ningle:*response*) 404)
            (json:encode-json-to-string
             (list
              (cons :error
                    (format nil "Unknown procedure '~A'" name)))))))))

(defmacro def-query (name &key params return where
                              limit max-inferences timeout
                              (effects nil) (snapshot t))
  "Define and register a parameterized, read-only graph query NAME, exposed at
POST /graph/:graph/query/<name> (camelCase).

  :PARAMS  a list of (?var TYPE) declarations; TYPE is :string, :integer,
           :number, :boolean or :keyword.  The client supplies each by the
           camelCase of its name (?min-age -> \"minAge\"); the value is coerced
           and unified with ?var before the query runs.  A missing or malformed
           parameter yields a 400.
  :RETURN  the result variables; the response is a JSON array of objects keyed by
           their camelCase names (a node value is rendered as its id).
  :WHERE   the query goals (a list), which may reference the parameter vars.
  :LIMIT / :MAX-INFERENCES / :TIMEOUT  per-query safety bounds (default to
           *QUERY-DEFAULT-LIMIT* / *-MAX-INFERENCES* / *-TIMEOUT*); exceeding a
           bound yields a 400.
  :EFFECTS the side-effect policy (default NIL = read-only; a forbidden
           write/eval/io attempt yields a 403).  A read-only query runs under a
           lightweight MVCC read snapshot (:SNAPSHOT, default T -- consistent,
           lock-free reads).  A query whose :EFFECTS permit side effects instead
           runs inside a WITH-TRANSACTION: all its writes flatten into that one
           transaction, which provides the same read snapshot and is committed
           on success (or rolled back if a bound/permission error aborts it).
  :SNAPSHOT whether a read-only query uses the read snapshot (default T); ignored
           for an effecting query, which always runs in a transaction.

The server author owns the query shape and its bounds; the client supplies only
the declared parameters."
  (let* ((cb (gensym "CB"))
         (param-goals (mapcar (lambda (spec)
                                (list 'param (first spec) (%query-var-key (first spec))))
                              params))
         ;; A strictly read-only query gets the lightweight read snapshot; any
         ;; effecting query is wrapped in WITH-TRANSACTION instead (commits its
         ;; writes; the write ops flatten into it via ensure-transaction).
         (read-only-p (null effects))
         (opts (list :effects effects
                    :snapshot (and read-only-p snapshot)
                    :limit (or limit '*query-default-limit*)
                    :max-inferences (or max-inferences '*query-default-max-inferences*)
                    :timeout (or timeout '*query-default-timeout*)
                    :callback cb))
         (query-form `(select ,opts ,return ,@param-goals ,@where))
         (run-form (if read-only-p query-form `(with-transaction () ,query-form))))
    `(register-query ',name
       (lambda (req-params)
         (handler-case
             (let ((*query-params* (coerce-query-params ',params req-params)))
               ;; stream rows through the callback; :json buffers an array,
               ;; :ndjson writes one JSON object per line
               (emit-query-results ',return (query-format req-params)
                                   (lambda (,cb) ,run-form)))
           ;; QUERY-PARAM-ERROR is a subclass, so one clause covers the
           ;; DSL's own refusals and the engine's typed preconditions --
           ;; an unindexed slot, an unscoped spatial class (GH #286).
           (query-precondition-error (c)
             (setf (lack.response:response-status ningle:*response*) 400)
             (json:encode-json-to-string
              (list (cons :error (query-precondition-error-reason c)))))
           (prolog-resource-error (c)
             (declare (ignore c))
             (setf (lack.response:response-status ningle:*response*) 400)
             (json:encode-json-to-string
              (list (cons :error "query exceeded its resource limits"))))
           (prolog-permission-error (c)
             (declare (ignore c))
             (setf (lack.response:response-status ningle:*response*) 403)
             (json:encode-json-to-string
              (list (cons :error "query attempted a forbidden operation")))))))))

(defun call-rest-query (name params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (let ((fn (gethash name *rest-queries*)))
      (if (functionp fn)
          (with-rest-graph ((get-param params :graph-name))
            (funcall fn params))
          (progn
            (setf (lack.response:response-status ningle:*response*) 404)
            (json:encode-json-to-string
             (list
              (cons :error
                    (format nil "Unknown query '~A'" name)))))))))

(defun %rest-resolve-post-type (type-name parent)
  "TYPE-NAME (a client camelCase string) resolved to a node-type of PARENT
in *GRAPH*.  (values META ERROR-STRING), exactly one non-NIL: the POST
handlers turn ERROR-STRING into their :error JSON (GH #190)."
  (handler-case
      (let ((meta (lookup-node-type-by-name
                   (intern (json:camel-case-to-lisp type-name) :keyword)
                   parent :graph *graph*)))
        (if meta
            (values meta nil)
            (values nil (format nil "Unknown ~(~A~) type ~A"
                                parent type-name))))
    (ambiguous-node-type-name (c)
      (values nil
              (format nil "Ambiguous ~(~A~) type ~A: one of ~{~A~^, ~}"
                      parent type-name
                      (mapcar #'%qualified-type-name-string
                              (ambiguous-type-candidates c)))))))

;;; ---------------------------------------------------------------------------
;;; Ad-hoc JSON pattern queries (#44, tier 2): the HTTP wrappers only.  The DSL
;;; itself -- COMPILE-PATTERN-QUERY / RUN-PATTERN-QUERY / EMIT-QUERY-RESULTS and
;;; the shape it accepts -- moved to query-dsl.lisp, which the GUI workbench
;;; compiles through as well (GH #278).
;;; ---------------------------------------------------------------------------

(defun %request-query-dsl ()
  "Decode the JSON request body of an ad-hoc pattern query into a DSL alist.
The HTTP integration seam for the /query route.  DECODE-DSL-JSON interns
only the DSL's own keys, so a client cannot grow the KEYWORD package one
bogus field per request (GH #284)."
  (decode-dsl-json
   (flexi-streams:octets-to-string
    (lack/request:request-content ningle:*request*)
    :external-format :utf-8)))

(defun call-rest-pattern-query (dsl params)
  "Auth + graph-scope an ad-hoc pattern query (DSL = the decoded JSON body).
A malformed query or a resource-bound breach is a 400, a forbidden effect a 403."
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      ;; The DSL renders ndjson; the wire header is HTTP's (GH #322).
      (when (%dsl-ndjson-p dsl)
        (setf (lack.response:response-headers ningle:*response*)
              (list* :content-type "application/x-ndjson"
                     (lack.response:response-headers ningle:*response*))))
      (handler-case (run-pattern-query dsl *graph*)
        ;; Includes QUERY-PARAM-ERROR, its subclass (GH #286).
        (query-precondition-error (c)
          (setf (lack.response:response-status ningle:*response*) 400)
          (json:encode-json-to-string
           (list (cons :error (query-precondition-error-reason c)))))
        (prolog-resource-error (c)
          (declare (ignore c))
          (setf (lack.response:response-status ningle:*response*) 400)
          (json:encode-json-to-string
           (list (cons :error "query exceeded its resource limits"))))
        (prolog-permission-error (c)
          (declare (ignore c))
          (setf (lack.response:response-status ningle:*response*) 403)
          (json:encode-json-to-string
           (list (cons :error "query attempted a forbidden operation"))))))))

(defun rest-get-graph (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (json-encode *graph*))))

(defun rest-get-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-vertex ((get-param params :node-id))
        (json-encode vertex)))))

(defun rest-post-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (let ((type-name (get-param params :type)))
        (multiple-value-bind (type error-string)
            (%rest-resolve-post-type type-name :vertex)
          (if type
              (let* ((class (find-class (node-type-name type)))
                     (slots (mapcar (lambda (slot)
                                      (intern (symbol-name slot) :keyword))
                                    (data-slots class)))
                     (constructor (node-type-constructor type))
                     ;; MAPCAN (not FLATTEN): FLATTEN drops nil slot values,
                     ;; leaving an odd plist -> "odd number of &KEY args".
                     (slot-args
                       (mapcan (lambda (slot)
                                 (list slot
                                       (cdr (assoc (json:lisp-to-camel-case
                                                    (symbol-name slot))
                                                   params
                                                   :test 'string-equal))))
                               slots))
                     ;; The URL's graph IS the explicit :graph -- without
                     ;; it the node lands in the class's declared store,
                     ;; not the store the request named (GH #167).
                     (node (apply constructor :graph *graph* slot-args)))
                (json-encode node))
              (json:encode-json-to-string
               (list (cons :error error-string)))))))))

(defun rest-put-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-vertex ((get-param params :node-id))
        (with-transaction ()
          (let ((slots (data-slots (class-of vertex)))
                (new-vertex (copy vertex)))
            (dolist (slot slots)
              (let ((kv (assoc (json:lisp-to-camel-case
                                (symbol-name slot))
                               params
                               :test 'string-equal)))
                (when kv
                  (setf (slot-value new-vertex slot)
                        (cdr kv)))))
            (json-encode (save new-vertex))))))))

(defun rest-delete-vertex (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-vertex ((get-param params :node-id))
        (mark-deleted vertex)))))

(defun rest-get-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-edge ((get-param params :node-id))
        (json-encode edge)))))

(defun rest-post-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (let ((type-name (get-param params :type)))
        (multiple-value-bind (type error-string)
            (%rest-resolve-post-type type-name :edge)
          (if type
              (let* ((class (find-class (node-type-name type)))
                     (slots (mapcar (lambda (slot)
                                      (intern (symbol-name slot) :keyword))
                                    (data-slots class)))
                     (constructor (node-type-constructor type))
                     (from (lookup-vertex
                            (cdr (assoc "from" params :test 'string-equal))))
                     (to (lookup-vertex
                          (cdr (assoc "to" params :test 'string-equal))))
                     ;; MAPCAN (not FLATTEN): FLATTEN drops nil slot values,
                     ;; leaving an odd plist -> "odd number of &KEY args".
                     (slot-args
                       (mapcan (lambda (slot)
                                 (list slot
                                       (cdr (assoc (json:lisp-to-camel-case
                                                    (symbol-name slot))
                                                   params
                                                   :test 'string-equal))))
                               slots)))
                (if (and from to)
                    ;; :graph *graph* -- see rest-post-vertex (GH #167).
                    (let ((node (apply constructor :graph *graph*
                                       :from from :to to
                                       slot-args)))
                      (json-encode node))
                    (json:encode-json-to-string
                     (list
                      (cons :error
                            "You must provide both FROM and TO vertices")))))
              (json:encode-json-to-string
               (list (cons :error error-string)))))))))

(defun rest-put-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-edge ((get-param params :node-id))
        (with-transaction ()
          (let ((slots (data-slots (class-of edge)))
                (new-edge (copy edge)))
            (dolist (slot slots)
              (let ((kv (assoc (json:lisp-to-camel-case
                                (symbol-name slot))
                               params
                               :test 'string-equal)))
                (when kv
                  (setf (slot-value new-edge slot)
                        (cdr kv)))))
            (json-encode (save new-edge))))))))

(defun rest-delete-edge (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-edge ((get-param params :node-id))
        (mark-deleted edge)))))

(defun rest-list-edges (params)
  (with-rest-auth ((get-param params "username") (get-param params "password"))
    (with-rest-graph ((get-param params :graph-name))
      (with-rest-vertex ((get-param params :node-id))
        (json-encode-edge-list
         (nconc (map-edges 'identity *graph*
                           :direction :out
                           :collect-p t
                           :vertex vertex)
                (map-edges 'identity *graph*
                           :direction :in
                           :collect-p t
                           :vertex vertex)))))))

(defun start-rest (&optional (port *rest-port*))
  (prog1
      (setq *rest-app* (make-instance 'ningle:<app>))
    (setf (ningle:route *rest-app* "/graph/:graph-name" :method :get)
          'rest-get-graph

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :get)
          'rest-get-vertex

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id/edges" :method :get)
          'rest-list-edges

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :delete)
          'rest-delete-vertex

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:type" :method :post)
          'rest-post-vertex

          (ningle:route *rest-app* "/graph/:graph-name/vertex/:node-id" :method :put)
          'rest-put-vertex

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :get)
          'rest-get-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :delete)
          'rest-delete-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:type" :method :post)
          'rest-post-edge

          (ningle:route *rest-app* "/graph/:graph-name/edge/:node-id" :method :put)
          'rest-put-edge

          ;; NB: ningle route handlers must be FUNCTIONS (or symbols naming one);
          ;; a quoted (lambda ...) is a list, not a function -- ningle returns it
          ;; verbatim and the response breaks.  Route captures (:procedure-name,
          ;; :query-name) arrive in PARAMS as keyword keys, like :node-id above.
          (ningle:route *rest-app* "/graph/:graph-name/procedure/:procedure-name" :method :post)
          (lambda (params)
            (call-rest-procedure (get-param params :procedure-name) params))

          (ningle:route *rest-app* "/graph/:graph-name/query/:query-name" :method :post)
          (lambda (params)
            (call-rest-query (get-param params :query-name) params))

          (ningle:route *rest-app* "/graph/:graph-name/query" :method :post)
          (lambda (params)
            (let ((dsl (handler-case (%request-query-dsl) (error () :%malformed))))
              (if (eq dsl :%malformed)
                  (progn
                    (setf (lack.response:response-status ningle:*response*) 400)
                    (json:encode-json-to-string
                     (list (cons :error "malformed JSON request body"))))
                  (call-rest-pattern-query dsl params)))))

    (setq *clack-app* (clack:clackup *rest-app* :port port))))

(defun stop-rest (&optional (app *clack-app*))
  (clack:stop app)
  (setq *rest-app* nil))
