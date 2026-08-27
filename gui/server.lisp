;;;; GUI server lifecycle + static file serving (GH #269).
;;;;
;;;; Mirrors START-REST's ningle/clack shape.  The GUI holds NO graph
;;;; state: every request resolves its graph by name at request time
;;;; (see api.lisp), so a graph closed mid-session yields a clean JSON
;;;; error, never a stale handle.

(in-package #:graph-db.gui)

(defvar *gui-port* 4270)
(defvar *gui-app* nil)
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

(defun %gui-dispatch (env ningle-fn root)
  "Route ENV: /api/* to the ningle app, everything else (GET/HEAD) to
the static tree, with index.html at /."
  (let ((path (getf env :path-info)))
    (if (and (member (getf env :request-method) '(:get :head))
             (not (eql 0 (search "/api/" path))))
        (or (%static-response path root)
            '(404 (:content-type "text/plain") ("not found")))
        (funcall ningle-fn env))))

(defun %make-gui-app ()
  (let ((app (make-instance 'ningle:<app>)))
    (setf (ningle:route app "/api/graphs" :method :get)
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
          'api-graph-neighborhood)
    app))

(defun start-gui (&key (port *gui-port*) (bind "127.0.0.1"))
  "Start the GUI HTTP server on PORT, bound to BIND (loopback by
default -- localhost is the v1 security boundary).  Returns the clack
handler.  Idempotent: a second call while running returns the running
handler unchanged."
  (or *gui-handler*
      (let* ((app (%make-gui-app))
             (root (gui-static-root))
             (ningle-fn (lack.component:to-app app)))
        (setq *gui-app* app)
        (setq *gui-port* port)
        (setq *gui-handler*
              ;; :debug nil -- a handler bug must never put a backtrace
              ;; in the browser; api.lisp logs details via log4cl.
              (clack:clackup
               (lambda (env) (%gui-dispatch env ningle-fn root))
               :port port :address bind :debug nil :silent t)))))

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
