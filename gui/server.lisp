;;;; GUI server lifecycle + static file serving (GH #269).
;;;;
;;;; Mirrors START-REST's ningle/clack shape.  The GUI holds NO graph
;;;; state: every request resolves its graph by name at request time
;;;; (see api.lisp), so a graph closed mid-session yields a clean JSON
;;;; error, never a stale handle.

(in-package #:graph-db.gui)

(defvar *gui-port* 4270)
(defvar *gui-app* nil
  "The clack app START-GUI is serving (MAKE-GUI-APP's), or NIL.")
(defvar *gui-handler* nil
  "The running clack handler, or NIL when the GUI is stopped.")

(defun gui-static-root ()
  "The gui/static/ directory of the loaded graph-db source tree.
The binary future swaps this serving strategy, not the layout."
  (asdf:system-relative-pathname :graph-db/gui "gui/static/"))

(defparameter *static-content-types*
  '(("html" . "text/html; charset=utf-8")
    ("css"  . "text/css; charset=utf-8")
    ("js"   . "application/javascript; charset=utf-8")
    ("mjs"  . "application/javascript; charset=utf-8")
    ("json" . "application/json; charset=utf-8")
    ("svg"  . "image/svg+xml")
    ("png"  . "image/png")
    ("ico"  . "image/x-icon")
    ("map"  . "application/json"))
  "File extension -> Content-Type for static assets.")

(defun %static-content-type (path)
  (or (cdr (assoc (string-downcase (or (pathname-type path) ""))
                  *static-content-types* :test #'equal))
      "application/octet-stream"))

(defun %pathname-under-p (path root)
  "True when truename PATH sits inside truename directory ROOT."
  (let ((pd (pathname-directory path))
        (rd (pathname-directory root)))
    (and (>= (length pd) (length rd))
         (equal (subseq pd 0 (length rd)) rd))))

(defun %safe-static-path (path-info root)
  "PATH-INFO resolved to an existing file strictly under ROOT, or NIL.
Component screen (.., empty, leading ~) plus a TRUENAME containment
check: PROBE-FILE resolves ~ to the home directory and follows
symlinks, so the request string alone proves nothing -- only the
resolved truename's prefix against ROOT's does (GH #269)."
  (let* ((rel (string-left-trim "/" (if (string= path-info "/")
                                        "index.html"
                                        path-info)))
         (parts (uiop:split-string rel :separator "/")))
    (when (and (plusp (length rel))
               (notany (lambda (p)
                         (or (string= p "..") (string= p "")
                             (char= (char p 0) #\~)))
                       parts))
      (let* ((merged (ignore-errors (merge-pathnames rel root)))
             (file (and merged
                        (not (wild-pathname-p merged))
                        (ignore-errors (probe-file merged))))
             (true-root (ignore-errors (truename root))))
        (when (and file true-root (%pathname-under-p file true-root))
          file)))))

(defun %static-response (path-info root)
  "A clack response serving PATH-INFO from ROOT, or NIL when no file
matches (a pathname body is streamed by the handler)."
  (let ((file (%safe-static-path path-info root)))
    (when (and file (not (uiop:directory-pathname-p file)))
      (list 200
            (list :content-type (%static-content-type file))
            file))))

(defvar *max-request-body-bytes* 65536
  "Largest request body the API accepts, in bytes.  A query document is
a few hundred bytes and free-text Prolog is capped at 4096 characters,
so 64 KB is generous for anything legitimate.

The check is on CONTENT-LENGTH and happens in %GUI-DISPATCH, before the
request reaches ningle -- which is the only place it CAN happen: lack
parses an application/json body while building the request, so by the
time a handler runs the bytes are already read and decoded.  A 32 MB
body cost ~7.3 s of CPU and 32 MB of transient allocation on an
unauthenticated endpoint before any handler could object (GH #279).

Refusing on the header means the body is never drained, so a client
still uploading a very large one sees the connection close rather than
a tidy 413 -- which is the point, and is what nginx does too.  A body
that fits the socket buffer gets the 413 itself.")

(defun %too-large-response (length)
  "413 for an over-large request, in the GUI's own {error, message}
shape.  Built by hand because NINGLE:*RESPONSE* does not exist yet:
this runs before the ningle app is entered at all."
  (list 413
        (list :content-type "application/json; charset=utf-8")
        (list (json:with-explicit-encoder
                (json:encode-json-to-string
                 (list :object
                       (cons :error "request-too-large")
                       (cons :message
                             (format nil "Request body is ~D bytes; ~
the limit is ~D" length *max-request-body-bytes*))))))))

(defun %request-path (env)
  "The URI path the client sent, before any mount stripped its prefix:
:REQUEST-URI minus the query string and minus an absolute-form scheme
and authority.  hunchentoot strips those from its script name but hands
the request URI over raw, so GET http://evil.example/gui used to put
that host in the redirect's Location: an open redirect behind any proxy
hop (GH #386).  The mount middleware drops the prefix from :PATH-INFO
without recording it, so the path cannot be rebuilt from there."
  (let* ((uri (or (getf env :request-uri) ""))
         (path (subseq uri 0 (or (position #\? uri) (length uri))))
         (scheme-end (search "://" path)))
    (if (and scheme-end
             (< 0 scheme-end 6)
             (every #'alpha-char-p (subseq path 0 scheme-end)))
        (let ((slash (position #\/ path :start (+ scheme-end 3))))
          (if slash (subseq path slash) "/"))
        path)))

(defun %read-only-response ()
  "403 read-only in the GUI's own {error, message} shape, built by hand
because this answers from the dispatcher, before ningle (GH #386)."
  (list 403
        (list :content-type "application/json; charset=utf-8")
        (list (json:with-explicit-encoder
                (json:encode-json-to-string
                 (list :object
                       (cons :error "read-only")
                       (cons :message "This GUI is read-only; only ~
reads are served")))))))

(defun %drain-request-body (env)
  "Read and drop ENV's request body.  An answer from the dispatcher
leaves it unread: clack hands it to the app as a stream, which
hunchentoot's own drain before the headers skips, and a close with
unread bytes is a TCP reset that a macOS client sees in place of the
reply (GH #446).  Bounded by the length the 413 rail already checked."
  (let ((in (getf env :raw-body))
        (length (getf env :content-length)))
    (when (and in (integerp length) (plusp length))
      (let ((buffer (make-array (min length 4096)
                                :element-type '(unsigned-byte 8))))
        (loop while (plusp (read-sequence buffer in)))))))

(defparameter *read-only-post-routes* '("query" "prolog")
  "The /api/graphs/:name/<verb> POST routes a read-only GUI still
serves: JSON-bodied reads that never write (GH #278, #279).")

(defun %read-route-p (path)
  "True when PATH is one of *READ-ONLY-POST-ROUTES* under /api/graphs/."
  (let ((parts (split-sequence:split-sequence #\/ path
                                              :remove-empty-subseqs t)))
    (and (= 4 (length parts))
         (string= "api" (first parts))
         (string= "graphs" (second parts))
         (member (fourth parts) *read-only-post-routes* :test #'string=)
         t)))

(defun %slash-redirect (env)
  "301 to the request path plus a trailing slash, or NIL when it has
one.  index.html names its assets and the API by RELATIVE URL, so a
mount under a host listener resolves them under the mount (GH #384) --
which holds only while the page's own URL ends in a slash, so the bare
mount root (/gui) is sent to /gui/ first.  A standalone GUI's / always
has its slash and never takes this branch."
  (let ((path (%request-path env))
        (query (getf env :query-string)))
    (unless (or (zerop (length path))
                (char= (char path (1- (length path))) #\/))
      (list 301
            (list :location (if (and query (plusp (length query)))
                                (format nil "~A/?~A" path query)
                                (format nil "~A/" path))
                  :content-type "text/plain")
            (list "")))))

(defun %gui-dispatch (env ningle-fn root)
  "Route ENV: /api/* to the ningle app, everything else (GET/HEAD) to
the static tree, with index.html at / (a slashless mount root is
redirected there first, see %SLASH-REDIRECT).

An over-large body is refused here, ahead of both.  413 rather than 400
because for once the request ENTITY really is what is too large -- the
DSL's resource-budget breach stays a 400, where the request is tiny and
it is the query that is expensive.

A chunked body has no CONTENT-LENGTH and so cannot be pre-checked here.
Deliberately not special-cased: hunchentoot never hands one to a
handler at all -- it answers its own 400 after a read timeout -- and 40
concurrent held chunked connections were measured not to degrade
service, so a 411 here would add a branch on every request to change an
outcome that is already safe."
  (let ((path (getf env :path-info))
        (length (getf env :content-length)))
    (cond
      ((and (integerp length) (> length *max-request-body-bytes*))
       (%too-large-response length))
      ;; :READ-ONLY is a dispatch rail, not a per-handler courtesy
      ;; (GH #386): nothing but GET, HEAD and the two JSON-bodied
      ;; reads gets past here, so a mutating route added without
      ;; WITH-WRITE-VERB is refused all the same.
      ((and *read-only*
            (not (member (getf env :request-method) '(:get :head)))
            (not (%read-route-p path)))
       (%drain-request-body env)
       (%read-only-response))
      ((and (member (getf env :request-method) '(:get :head))
            (not (eql 0 (search "/api/" path))))
       (or (and (string= path "/") (%slash-redirect env))
           (%static-response path root)
           '(404 (:content-type "text/plain") ("not found"))))
      (t (funcall ningle-fn env)))))

(defun %make-gui-app ()
  (let ((app (make-instance 'ningle:<app>)))
    (setf (ningle:route app "/api/capabilities" :method :get)
          'api-capabilities

          (ningle:route app "/api/graphs" :method :get)
          'api-graphs

          (ningle:route app "/api/graphs/:name/open" :method :post)
          'api-open-graph

          (ningle:route app "/api/graphs/:name/close" :method :post)
          'api-close-graph

          (ningle:route app "/api/graphs/:name/stats" :method :get)
          'api-graph-stats

          (ningle:route app "/api/graphs/:name/types" :method :get)
          'api-graph-types

          (ningle:route app "/api/graphs/:name/nodes" :method :get)
          'api-graph-nodes

          (ningle:route app "/api/graphs/:name/node/:id" :method :get)
          'api-graph-node

          (ningle:route app "/api/graphs/:name/neighborhood/:id"
                        :method :get)
          'api-graph-neighborhood

          ;; Query workbench (GH #278): a POST because the structured
          ;; DSL is a JSON document, not query parameters.  Read-only
          ;; all the same -- run-pattern-query never writes.
          (ningle:route app "/api/graphs/:name/query" :method :post)
          'api-graph-query

          ;; Free-text Prolog (GH #279).  Routed unconditionally; the
          ;; handler itself refuses when :ALLOW-PROLOG is off, so the
          ;; flag is enforced at the endpoint and not by the UI hiding
          ;; a tab.
          (ningle:route app "/api/graphs/:name/prolog" :method :post)
          'api-graph-prolog)
    app))

(defun %allow-prolog-p (allow-prolog env)
  "ALLOW-PROLOG decided for this request: a function is called on the
clack ENV; anything else is a generalized boolean."
  (and (if (functionp allow-prolog)
           (funcall allow-prolog env)
           allow-prolog)
       t))

(defun make-gui-app (&key (static-root (gui-static-root))
                          allow-prolog read-only)
  "A clack application -- a function of ENV -- serving what START-GUI
serves: /api/* through the ningle routes, everything else through the
static tree under STATIC-ROOT with the body-size refusal, index.html
at the root.  Every path is relative to the app's own root, so a host
running its own listener mounts it under a prefix (GH #384):

  (lack:builder (:mount \"/gui\" (graph-db.gui:make-gui-app)) host-app)

ALLOW-PROLOG opens the free-text Prolog endpoint: T for every request,
or a function of the clack ENV, decided per request -- how a host
admits the workbench for some visitors only.  READ-ONLY refuses every
request but GET, HEAD and the query and Prolog POSTs with 403 read-only
at dispatch (GH #386), and says so in /api/capabilities.
Both are bound around each request, so an outer binding of
*ALLOW-PROLOG* is shadowed: pass a function instead."
  (let ((ningle-fn (lack.component:to-app (%make-gui-app))))
    (lambda (env)
      (let ((*allow-prolog* (%allow-prolog-p allow-prolog env))
            (*read-only* (and read-only t)))
        (%gui-dispatch env ningle-fn static-root)))))

(defun start-gui (&key (port *gui-port*) (bind "127.0.0.1")
                       allow-prolog read-only)
  "Start the GUI HTTP server on PORT, bound to BIND (loopback by
default -- localhost is the v1 security boundary).  A non-loopback
BIND serves the UNAUTHENTICATED API and open/close verbs to that
network: bind only interfaces whose peers you trust.  Returns the
clack handler.  Idempotent: a second call while running returns the
running handler unchanged.

ALLOW-PROLOG (default NIL) opens POST /api/graphs/:name/prolog, which
accepts a free-text Prolog query.  Everything that text can name is
whitelisted against the live functor registries and this graph's
schema before it compiles (gui/prolog.lisp), and it runs on the same
read-only, bounded rails as the structured builder -- but it is still
the only surface that reads client text, so it is off unless asked
for.  READ-ONLY refuses the open/close verbs.  Both are MAKE-GUI-APP's
and, START-GUI being idempotent, take effect only when it actually
starts a server: restart to change them."
  (or *gui-handler*
      (let ((app (make-gui-app :allow-prolog allow-prolog
                               :read-only read-only)))
        (setq *gui-app* app)
        (setq *gui-port* port)
        (setq *gui-handler*
              ;; :debug nil -- a handler bug must never put a backtrace
              ;; in the browser; api.lisp logs details via log4cl.
              (clack:clackup app :port port :address bind
                             :debug nil :silent t)))))

(defun stop-gui ()
  "Stop the GUI server.  Idempotent: a no-op when not running.
Returns T when a server was stopped, NIL otherwise.  The handle is
nulled only after CLACK:STOP returns -- if the stop signals, the
handle survives so a retry can still stop it."
  (let ((handler *gui-handler*))
    (when handler
      (clack:stop handler)
      (setq *gui-handler* nil *gui-app* nil)
      t)))
