;;;; GUI suite runner + fixtures (GH #269).
;;;;
;;;; Standalone suite -- NOT :in graph-db-suite, so the main
;;;; (asdf:test-system :graph-db) run does not pick it up.
;;;;
;;;; ⚠ The HTTP handlers run in server threads, which see the GLOBAL
;;;; values of *SYSTEM-DIRECTORY* / *SYSTEM-CLOCK*, not this thread's
;;;; LET bindings -- so the runner SETFs the globals for the run and
;;;; restores them after (a fresh test image, so nothing else owns
;;;; them).

(in-package #:graph-db/gui-test)

(def-suite gui-suite
  :description "graph-db/gui backend over real HTTP.")

(defun run-gui-tests ()
  "Run the GUI suite.  Returns T when every check passed.  Invoked by
(asdf:test-system :graph-db/gui-test)."
  (log:config :error)
  (let ((old-dir graph-db::*system-directory*)
        (old-reg graph-db::*store-registry*)
        (old-type-reg graph-db::*type-registry*)
        (system-dir (graph-db-test-scratch:make-scratch-directory
                     "graph-db-gui-sys")))
    (setf graph-db::*system-directory* (namestring system-dir)
          graph-db::*store-registry* nil
          graph-db::*type-registry* nil)
    (unwind-protect
         (let ((results (run 'gui-suite)))
           (explain! results)
           (results-status results))
      (ignore-errors (stop-gui))
      (setf graph-db::*system-directory* old-dir
            graph-db::*store-registry* old-reg
            graph-db::*type-registry* old-type-reg)
      (graph-db-test-scratch:cleanup-scratch-run))))

;;; ---------------------------------------------------------------------
;;; Schema (domain-neutral, per repo policy #197)
;;; ---------------------------------------------------------------------

(defparameter *gui-graph-name* :gui-test-graph)

;; Clean slate so reloading this file doesn't double-register.
(eval-when (:load-toplevel :execute)
  (setf (gethash *gui-graph-name* graph-db::*schema-node-metadata*) nil))

;; HOME-CITY is deliberately multi-word: it is the only slot whose
;; wire spelling can tell kebab from camelCase (GH #277).
(def-vertex gui-person ()
  ((name :type string)
   (age)
   (home-city))
  :gui-test-graph)

(def-vertex gui-city ()
  ((name :type string))
  :gui-test-graph)

(def-edge gui-visited ()
  ((year))
  :gui-test-graph)

(defun define-gui-views ()
  "One map view on the live *graph*, for the stats view inventory."
  (def-view people-by-name :lessp (gui-person :gui-test-graph)
    (:map (lambda (p)
            (when (slot-value p 'name)
              (yield (slot-value p 'name) nil))))))

;;; ---------------------------------------------------------------------
;;; Graph fixture: a populated store attached to a system clock, so the
;;; clock journal records its :ATTACH location (the roster's source for
;;; closed stores).
;;; ---------------------------------------------------------------------

(defvar *fixture* nil
  "Plist for the current fixture: :graph :location :alice :bob :paris
:tokyo, set by WITH-GUI-FIXTURE for the tests' convenience.")

(defun populate-fixture (graph)
  (let ((*graph* graph) alice bob paris tokyo)
    (define-gui-views)
    (with-transaction ()
      (setq alice (make-gui-person :name "Alice" :age 34
                                   :home-city "Paris")
            bob (make-gui-person :name "Bob" :age 41)
            paris (make-gui-city :name "Paris")
            tokyo (make-gui-city :name "Tokyo"))
      (make-gui-visited :from alice :to paris :year 2019)
      (make-gui-visited :from alice :to tokyo :year 2023)
      (make-gui-visited :from bob :to paris :year 2021))
    (list :graph graph :location (namestring (graph-db::location graph))
          :alice alice :bob bob :paris paris :tokyo tokyo)))

(defmacro with-gui-fixture (() &body body)
  "Open a system clock on the run's system directory, build a fresh
populated :GUI-TEST-GRAPH attached to it, run BODY, then tear down
graph and clock (globals, not LET -- see the file header)."
  `(let ((dir (graph-db-test-scratch:make-scratch-directory
               "graph-db-gui")))
     (setf graph-db::*system-clock*
           (graph-db::open-system-clock graph-db::*system-directory*))
     (let ((graph (make-graph *gui-graph-name* (namestring dir)
                              :buffer-pool-size 1000)))
       (unwind-protect
            (let ((*fixture* (populate-fixture graph)))
              ,@body)
         (let ((live (lookup-graph *gui-graph-name*)))
           (when live
             (ignore-errors (close-graph live :snapshot-p nil))))
         (ignore-errors
          (graph-db::close-system-clock graph-db::*system-clock*))
         (setf graph-db::*system-clock* nil)
         (ignore-errors
          (uiop:delete-directory-tree dir :validate t
                                          :if-does-not-exist :ignore))))))

;;; ---------------------------------------------------------------------
;;; HTTP plumbing
;;; ---------------------------------------------------------------------

(defparameter *gui-test-port* nil)

(defun free-tcp-port ()
  "An unused loopback port: bind port 0, read the OS's pick, release."
  (let ((s (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect (usocket:get-local-port s)
      (usocket:socket-close s))))

(defmacro with-gui-server ((&key allow-prolog) &body body)
  "Run BODY against a GUI on a fresh ephemeral port.  ALLOW-PROLOG
opens the free-text Prolog endpoint; the default NIL is the shipped
default, and the suite proves the flag in both states (GH #279)."
  `(let ((*gui-test-port* (free-tcp-port)))
     (unwind-protect
          (progn
            (start-gui :port *gui-test-port* :bind "127.0.0.1"
                       :allow-prolog ,allow-prolog)
            (sleep 0.3)                 ; let the listener bind
            ,@body)
       (ignore-errors (stop-gui)))))

(defun gui-url (path)
  (format nil "http://127.0.0.1:~D~A" *gui-test-port* path))

(defun gui-request (path &key (method :get) preserve-uri content
                             (content-type "application/json"))
  "Request PATH; (values decoded-json status content-type raw-body).
DRAKMA returns 4xx/5xx without signaling, so status is always there.
:PRESERVE-URI T sends PATH byte-for-byte (no client-side dot-segment
or percent normalization) -- for the traversal tests.  :CONTENT is a
UTF-8 request body sent under :CONTENT-TYPE (GH #278)."
  (multiple-value-bind (body status headers)
      (apply #'drakma:http-request (gui-url path)
             :method method
             :preserve-uri preserve-uri
             (when content
               (list :content (flexi-streams:string-to-octets
                               content :external-format :utf-8)
                     :content-type content-type)))
    (let ((string (cond ((null body) "")
                        ((stringp body) body)
                        (t (flexi-streams:octets-to-string
                            body :external-format :utf-8)))))
      (values (when (plusp (length string))
                (ignore-errors (json:decode-json-from-string string)))
              status
              (cdr (assoc :content-type headers))
              string))))

(defun jref (alist key)
  "Decoded-body lookup.  ⚠ cl-json's decoder folds BOTH \"guiPerson\"
and \"gui-person\" to :GUI-PERSON, so a JREF assertion cannot pin the
wire spelling -- assert on GUI-REQUEST's raw fourth value for that
\(GH #277)."
  (cdr (assoc key alist)))
