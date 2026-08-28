;;;; graph-db/gui backend tests over real HTTP (GH #269).
;;;;
;;;; Each test stands up the full stack: a populated on-disk graph
;;;; attached to a system clock, START-GUI on an ephemeral loopback
;;;; port, and drakma as the client.  Covers the spec's contract:
;;;; roster (incl. the no-system-dir fallback), open/close (incl. the
;;;; dirty 409), stats, types/nodes sampling, node inspection
;;;; (alist -> JSON fidelity), neighborhood shape/caps/truncated, the
;;;; error contract, and start/stop idempotence.

(in-package #:graph-db/gui-test)

(in-suite gui-suite)

;;; ---------------------------------------------------------------------
;;; Lifecycle + static serving
;;; ---------------------------------------------------------------------

(test start-stop-idempotent
  "start-gui twice returns the same handler; stop-gui is a no-op when
already stopped."
  (let ((port (free-tcp-port)))
    (unwind-protect
         (let ((h1 (start-gui :port port :bind "127.0.0.1"))
               (h2 (start-gui :port port :bind "127.0.0.1")))
           (is (eq h1 h2))
           (is-true (stop-gui))
           (is-false (stop-gui)))
      (ignore-errors (stop-gui)))))

(test static-index-at-root
  "GET / serves gui/static/index.html with a text/html content type."
  (with-gui-server ()
    (multiple-value-bind (json status ctype raw)
        (gui-request "/")
      (declare (ignore json))
      (is (= 200 status))
      (is (eql 0 (search "text/html" ctype)))
      (is (search "VivaceGraph GUI" raw)))))

(test static-frame-assets-serve
  "Every asset the frame page loads serves 200 with the right content
type -- pins the file layout index.html depends on (GH #270)."
  (with-gui-server ()
    (flet ((asset (path ctype)
             (multiple-value-bind (json status actual)
                 (gui-request path)
               (declare (ignore json))
               (is (= 200 status) "~A did not serve 200" path)
               (is (eql 0 (search ctype actual))
                   "~A served content type ~A, wanted ~A"
                   path actual ctype))))
      (asset "/" "text/html")
      (asset "/css/gui.css" "text/css")
      (asset "/js/api.js" "application/javascript")
      (asset "/js/roster.js" "application/javascript")
      (asset "/js/stats.js" "application/javascript")
      (asset "/js/main.js" "application/javascript"))))

(test static-explorer-assets-serve
  "The explorer's assets serve 200 with the right content types, and
the vendored cytoscape build is non-trivially large (GH #271)."
  (with-gui-server ()
    (flet ((asset (path ctype)
             (multiple-value-bind (json status actual raw)
                 (gui-request path)
               (declare (ignore json))
               (is (= 200 status) "~A did not serve 200" path)
               (is (eql 0 (search ctype actual))
                   "~A served content type ~A, wanted ~A"
                   path actual ctype)
               raw)))
      (let ((cyto (asset "/vendor/cytoscape.min.js"
                         "application/javascript")))
        (is (> (length cyto) 100000)
            "cytoscape.min.js is suspiciously small (~A bytes)"
            (length cyto)))
      (asset "/js/explorer.js" "application/javascript")
      (asset "/js/inspector.js" "application/javascript"))))

(test static-workbench-asset-serves
  "The query workbench's modules serve 200 as javascript (GH #278)."
  (with-gui-server ()
    (dolist (path '("/js/workbench.js" "/js/wb-splitter.js"))
      (multiple-value-bind (json status ctype)
          (gui-request path)
        (declare (ignore json))
        (is (= 200 status) "~A did not serve 200" path)
        (is (eql 0 (search "application/javascript" ctype))
            "~A served content type ~A" path ctype)))))

(test static-missing-file-404
  "A missing static path yields 404."
  (with-gui-server ()
    (multiple-value-bind (json status) (gui-request "/no-such.css")
      (declare (ignore json))
      (is (= 404 status)))))

(test static-head-request
  "HEAD on a static asset answers 200 with no meaningful body."
  (with-gui-server ()
    (multiple-value-bind (json status) (gui-request "/" :method :head)
      (declare (ignore json))
      (is (= 200 status)))))

(test static-traversal-rejected
  "Escape attempts never leave the static root: ~ home-dir expansion,
~user, dot-dot (plain and percent-encoded), and absolute-ish paths all
answer 4xx, and never file contents (GH #269)."
  (with-gui-server ()
    (flet ((refused-p (path &key preserve-uri)
             (multiple-value-bind (json status)
                 (gui-request path :preserve-uri preserve-uri)
               (declare (ignore json))
               (member status '(400 404)))))
      ;; SBCL's probe-file expands a leading ~ to $HOME.
      (is-true (refused-p "/~/.bashrc"))
      (is-true (refused-p "/~root/.bashrc"))
      (is-true (refused-p "/../graph-db.asd" :preserve-uri t))
      (is-true (refused-p "/%2e%2e/graph-db.asd" :preserve-uri t))
      (is-true (refused-p "/etc/passwd"))
      ;; the legitimate file still serves
      (multiple-value-bind (json status) (gui-request "/index.html")
        (declare (ignore json))
        (is (= 200 status))))))

#+sbcl
(test static-symlink-escape-rejected
  "A symlink inside the static root that points outside it is refused
by the truename containment check -- the one escape the component
screen cannot catch (GH #269)."
  (with-gui-server ()
    (let* ((root (asdf:system-relative-pathname :graph-db "gui/static/"))
           (link (merge-pathnames "esc-test.css" root))
           (target (asdf:system-relative-pathname :graph-db
                                                  "CHANGELOG.md")))
      (ignore-errors (delete-file link))
      (sb-posix:symlink (namestring target) (namestring link))
      (unwind-protect
           (multiple-value-bind (json status)
               (gui-request "/esc-test.css")
             (declare (ignore json))
             (is (= 404 status)))
        (ignore-errors (delete-file link))))))

;;; ---------------------------------------------------------------------
;;; Roster
;;; ---------------------------------------------------------------------

(defun roster-entry (name)
  (multiple-value-bind (json status) (gui-request "/api/graphs")
    (values (find name (jref json :graphs)
                  :key (lambda (e) (jref e :name))
                  :test #'string=)
            status)))

(test roster-lists-open-graph
  "GET /api/graphs lists the open fixture graph: name, location, open."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (entry status) (roster-entry "gui-test-graph")
        (is (= 200 status))
        (is-true entry)
        (is-true (jref entry :open))
        (is (string= (getf *fixture* :location)
                     (jref entry :location)))))))

(test roster-fallback-without-system-dir
  "With no *SYSTEM-DIRECTORY* bound the roster falls back to *GRAPHS*."
  (with-gui-fixture ()
    (with-gui-server ()
      (let ((saved-dir graph-db::*system-directory*)
            (saved-reg graph-db::*store-registry*))
        (unwind-protect
             (progn
               ;; Globals, because the handler thread reads them.
               (setf graph-db::*system-directory* nil
                     graph-db::*store-registry* nil)
               (multiple-value-bind (entry status)
                   (roster-entry "gui-test-graph")
                 (is (= 200 status))
                 (is-true entry)
                 (is-true (jref entry :open))))
          (setf graph-db::*system-directory* saved-dir
                graph-db::*store-registry* saved-reg))))))

;;; ---------------------------------------------------------------------
;;; Open / close management verbs
;;; ---------------------------------------------------------------------

(test close-then-reopen-cycle
  "Close and reopen through the API; roster tracks state; a second
open is idempotent; a closed graph 404s on reads (no stale handle)."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/close" :method :post)
        (is (= 200 status))
        (is (string= "closed" (jref json :status))))
      (let ((entry (roster-entry "gui-test-graph")))
        (is-true entry)
        (is-false (jref entry :open))
        ;; The journal's :ATTACH record still knows the location.
        (is (string= (getf *fixture* :location)
                     (jref entry :location))))
      ;; Closed mid-session: reads answer 404, never a stale handle.
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/stats")
        (is (= 404 status))
        (is (string= "unknown-graph" (jref json :error))))
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/open" :method :post)
        (is (= 200 status))
        (is (string= "opened" (jref json :status))))
      (is-true (jref (roster-entry "gui-test-graph") :open))
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/open" :method :post)
        (is (= 200 status))
        (is (string= "already-open" (jref json :status)))))))

(test dirty-store-open-409
  "A .dirty marker turns open into a 409 carrying the condition's
report verbatim; removing it lets the open succeed."
  (with-gui-fixture ()
    (with-gui-server ()
      (gui-request "/api/graphs/gui-test-graph/close" :method :post)
      (let ((dirty (format nil "~A.dirty" (getf *fixture* :location))))
        (with-open-file (s dirty :direction :output
                                 :if-does-not-exist :create)
          (write-string "test" s))
        (unwind-protect
             (multiple-value-bind (json status)
                 (gui-request "/api/graphs/gui-test-graph/open"
                              :method :post)
               (is (= 409 status))
               (is (string= "dirty-store" (jref json :error)))
               (is (search "not closed properly" (jref json :message))))
          (ignore-errors (delete-file dirty))))
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/open" :method :post)
        (declare (ignore json))
        (is (= 200 status))))))

(test open-location-missing-404
  "Opening a roster entry whose recorded directory has vanished is a
clean 404 (location-missing), not a 500."
  (with-gui-fixture ()
    (with-gui-server ()
      (gui-request "/api/graphs/gui-test-graph/close" :method :post)
      (let* ((loc (getf *fixture* :location))
             (aside (format nil "~Aaside/"
                            (namestring
                             (uiop:pathname-parent-directory-pathname
                              loc)))))
        (rename-file (uiop:ensure-directory-pathname loc)
                     (uiop:ensure-directory-pathname aside))
        (unwind-protect
             (multiple-value-bind (json status)
                 (gui-request "/api/graphs/gui-test-graph/open"
                              :method :post)
               (is (= 404 status))
               (is (string= "location-missing" (jref json :error))))
          (rename-file (uiop:ensure-directory-pathname aside)
                       (uiop:ensure-directory-pathname loc)))))))

(test reads-race-close-cleanly
  "Reads racing in-flight close/open cycles never crash or fault: with
the GUI rw-lock every response is a clean 200 or 404 (GH #269)."
  (with-gui-fixture ()
    (with-gui-server ()
      (let* (;; Full URL captured lexically: the reader thread does not
             ;; inherit this thread's *GUI-TEST-PORT* binding.
             (url (gui-url (node-path (getf *fixture* :alice)
                                      "neighborhood")))
             (statuses '())
             (lock (bordeaux-threads:make-lock "race-statuses"))
             (reader
               (bordeaux-threads:make-thread
                (lambda ()
                  (dotimes (i 40)
                    (let ((status (nth-value
                                   1 (drakma:http-request url))))
                      (bordeaux-threads:with-lock-held (lock)
                        (push status statuses)))))
                :name "gui race reader")))
        (dotimes (i 3)
          (gui-request "/api/graphs/gui-test-graph/close" :method :post)
          (gui-request "/api/graphs/gui-test-graph/open" :method :post))
        (bordeaux-threads:join-thread reader)
        (is (= 40 (length statuses)))
        (is (every (lambda (s) (member s '(200 404))) statuses))))))

(test open-unknown-graph-404
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/no-such-graph/open" :method :post)
        (is (= 404 status))
        (is (string= "unknown-graph" (jref json :error)))))))

(test close-not-open-409
  (with-gui-fixture ()
    (with-gui-server ()
      (gui-request "/api/graphs/gui-test-graph/close" :method :post)
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/close" :method :post)
        (is (= 409 status))
        (is (string= "not-open" (jref json :error)))))))

;;; ---------------------------------------------------------------------
;;; Stats + types
;;; ---------------------------------------------------------------------

(test stats-against-known-fixture
  "Totals, per-type counts, view + index inventories, on-disk size and
schema summary for the known fixture."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/stats")
        (is (= 200 status))
        (is (= 4 (jref json :vertex-count)))
        (is (= 3 (jref json :edge-count)))
        (let ((by-type (jref json :vertex-counts-by-type)))
          (is (= 2 (jref by-type :gui-person)))
          (is (= 2 (jref by-type :gui-city))))
        (is (= 3 (jref (jref json :edge-counts-by-type) :gui-visited)))
        (is (find "people-by-name" (jref json :views)
                  :key (lambda (v) (jref v :name)) :test #'string=))
        (is (listp (jref json :indexes)))
        (is (plusp (jref json :on-disk-bytes)))
        (let* ((schema (jref json :schema))
               (person (find "gui-person" (jref schema :vertex-types)
                             :key (lambda (v) (jref v :name))
                             :test #'string=)))
          (is-true person)
          (is (find "name" (jref person :slots) :test #'string=))
          (is (find "age" (jref person :slots) :test #'string=))
          (is (find "home-city" (jref person :slots) :test #'string=))
          (is (find "gui-visited" (jref schema :edge-types)
                    :key (lambda (v) (jref v :name))
                    :test #'string=)))))))

(test wire-names-are-kebab
  "Schema names ship as the engine spells them and JSON keys stay
camelCase; a type value the API emits is accepted verbatim by ?type=
\(GH #277).  Asserted on the RAW body -- the decoder folds both
spellings together, so JREF cannot see the difference."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status ctype raw)
          (gui-request "/api/graphs/gui-test-graph/stats")
        (declare (ignore json ctype))
        (is (= 200 status))
        ;; Values naming schema entities: the engine's own spelling.
        (dolist (kebab '("\"gui-person\"" "\"gui-city\""
                         "\"gui-visited\"" "\"people-by-name\""
                         "\"home-city\""))
          (is-true (search kebab raw) "~A missing from stats" kebab))
        (dolist (camel '("guiPerson" "guiCity" "guiVisited"
                         "peopleByName" "homeCity"))
          (is-false (search camel raw) "~A still on the wire" camel))
        ;; Keys are protocol and stay camelCase.
        (dolist (key '("\"vertexCount\"" "\"edgeCount\""
                       "\"onDiskBytes\"" "\"vertexCountsByType\""
                       "\"vertexTypes\""))
          (is-true (search key raw) "key ~A is not camelCase" key)))
      ;; Round trip: take a type value verbatim, feed it to ?type=.
      (let ((type (multiple-value-bind (json status)
                      (gui-request "/api/graphs/gui-test-graph/types")
                    (is (= 200 status))
                    (first (jref json :vertex-types)))))
        (is (string= "gui-city" type))
        (multiple-value-bind (json status)
            (gui-request
             (format nil "/api/graphs/gui-test-graph/nodes?type=~A"
                     type))
          (is (= 200 status))
          (is (string= type (jref json :type)))
          (is (plusp (length (jref json :nodes))))))
      ;; Inspector slot names are dynamic KEYS built from domain
      ;; identifiers -- kebab by decision (GH #277).
      (multiple-value-bind (json status ctype raw)
          (gui-request
           (format nil "/api/graphs/gui-test-graph/node/~A"
                   (string-id (getf *fixture* :alice))))
        (declare (ignore ctype))
        (is (= 200 status))
        (is-true (search "\"home-city\"" raw))
        (is-false (search "homeCity" raw))
        (is (string= "Paris" (jref (jref json :slots) :home-city)))
        ;; ...while the body's own keys stay camelCase.
        (is-true (search "\"inEdgeCount\"" raw))
        (is-true (search "\"outEdgeCount\"" raw))))))

(test types-inventory
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/types")
        (is (= 200 status))
        (is (find "gui-person" (jref json :vertex-types)
                  :test #'string=))
        (is (find "gui-city" (jref json :vertex-types) :test #'string=))
        (is (find "gui-visited" (jref json :edge-types)
                  :test #'string=))))))

;;; ---------------------------------------------------------------------
;;; Node sample
;;; ---------------------------------------------------------------------

(test nodes-sample-with-limit
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request
           "/api/graphs/gui-test-graph/nodes?type=gui-person")
        (is (= 200 status))
        (is (= 2 (length (jref json :nodes))))
        (is-false (jref json :truncated))
        (is (every (lambda (n)
                     (and (stringp (jref n :id))
                          (string= "gui-person" (jref n :type))))
                   (jref json :nodes))))
      (multiple-value-bind (json status)
          (gui-request
           "/api/graphs/gui-test-graph/nodes?type=gui-person&limit=1")
        (is (= 200 status))
        (is (= 1 (length (jref json :nodes))))
        (is-true (jref json :truncated))))))

(test nodes-unknown-type-404
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/nodes?type=no-such")
        (is (= 404 status))
        (is (string= "unknown-type" (jref json :error)))))))

(test nodes-missing-type-400
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/nodes")
        (is (= 400 status))
        (is (string= "missing-type" (jref json :error)))))))

;;; ---------------------------------------------------------------------
;;; Node inspection
;;; ---------------------------------------------------------------------

(defun node-path (node &optional (endpoint "node"))
  (format nil "/api/graphs/gui-test-graph/~A/~A"
          endpoint (string-id node)))

(test node-inspection-alist-fidelity
  "A node's data (an alist of (:SLOT . value) conses) serializes as a
JSON object with the slot values intact; in/out edge counts are right."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request (node-path (getf *fixture* :alice)))
        (is (= 200 status))
        (is (string= (string-id (getf *fixture* :alice))
                     (jref json :id)))
        (is (string= "gui-person" (jref json :type)))
        (let ((slots (jref json :slots)))
          (is (string= "Alice" (jref slots :name)))
          (is (= 34 (jref slots :age)))
          (is (string= "Paris" (jref slots :home-city))))
        (is (= 2 (jref json :out-edge-count)))
        (is (= 0 (jref json :in-edge-count))))
      (multiple-value-bind (json status)
          (gui-request (node-path (getf *fixture* :paris)))
        (is (= 200 status))
        (is (= 0 (jref json :out-edge-count)))
        (is (= 2 (jref json :in-edge-count)))))))

(test node-malformed-id-400
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/node/not-hex")
        (is (= 400 status))
        (is (string= "malformed-id" (jref json :error)))))))

(test node-unknown-id-404
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request
           (format nil "/api/graphs/gui-test-graph/node/~A"
                   (make-string 32 :initial-element #\f)))
        (is (= 404 status))
        (is (string= "unknown-node" (jref json :error)))))))

(test edge-inspection-after-neighborhood-fetch
  "GET /node/:id for an EDGE id renders the edge card.  Exercised
AFTER a neighborhood fetch: that warms the id-keyed node cache, which
makes LOOKUP-VERTEX return the cached edge -- the path that used to
500 (GH #271)."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (nbhd status)
          (gui-request (node-path (getf *fixture* :alice)
                                  "neighborhood"))
        (is (= 200 status))
        (let ((edge-id (jref (first (jref nbhd :edges)) :id)))
          (multiple-value-bind (json status)
              (gui-request
               (format nil "/api/graphs/gui-test-graph/node/~A"
                       edge-id))
            (is (= 200 status))
            (is (string= "gui-visited" (jref json :type)))
            (is (stringp (jref json :from)))
            (is (stringp (jref json :to)))
            (is (numberp (jref (jref json :slots) :year))))))
      ;; The frontend discriminates vertex vs edge by the presence of
      ;; FROM: a vertex body must not carry the key at all.
      (multiple-value-bind (vjson vstatus)
          (gui-request (node-path (getf *fixture* :alice)))
        (is (= 200 vstatus))
        (is (null (assoc :from vjson)))))))

(test neighborhood-edge-center-404
  "An edge id as neighborhood center answers a clean 404
unknown-node, even with the edge sitting in the id-keyed node cache
(GH #271): a neighborhood is defined on vertices only."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (nbhd status)
          (gui-request (node-path (getf *fixture* :alice)
                                  "neighborhood"))
        (is (= 200 status))
        (let ((edge-id (jref (first (jref nbhd :edges)) :id)))
          (multiple-value-bind (json status)
              (gui-request
               (format nil
                       "/api/graphs/gui-test-graph/neighborhood/~A"
                       edge-id))
            (is (= 404 status))
            (is (string= "unknown-node" (jref json :error)))))))))

;;; ---------------------------------------------------------------------
;;; Neighborhood
;;; ---------------------------------------------------------------------

(test neighborhood-shape-both-directions
  "One round trip returns viz-shaped {nodes, edges} covering both
directions, type-labeled."
  (with-gui-fixture ()
    (with-gui-server ()
      ;; Alice: two outgoing edges.
      (multiple-value-bind (json status)
          (gui-request (node-path (getf *fixture* :alice)
                                  "neighborhood"))
        (is (= 200 status))
        (is (= 3 (length (jref json :nodes))))
        (is (= 2 (length (jref json :edges))))
        (is-false (jref json :truncated))
        (is (every (lambda (e)
                     (and (string= "gui-visited" (jref e :type))
                          (stringp (jref e :from))
                          (stringp (jref e :to))))
                   (jref json :edges))))
      ;; Paris: two INCOMING edges -- both directions are covered.
      (multiple-value-bind (json status)
          (gui-request (node-path (getf *fixture* :paris)
                                  "neighborhood"))
        (is (= 200 status))
        (is (= 3 (length (jref json :nodes))))
        (is (= 2 (length (jref json :edges))))))))

(test neighborhood-cap-sets-truncated
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request (format nil "~A?limit=1"
                               (node-path (getf *fixture* :alice)
                                          "neighborhood")))
        (is (= 200 status))
        (is (= 1 (length (jref json :edges))))
        (is-true (jref json :truncated))))))

(test neighborhood-error-contract
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/neighborhood/zzz")
        (is (= 400 status))
        (is (string= "malformed-id" (jref json :error))))
      (multiple-value-bind (json status)
          (gui-request
           (format nil "/api/graphs/gui-test-graph/neighborhood/~A"
                   (make-string 32 :initial-element #\f)))
        (is (= 404 status))
        (is (string= "unknown-node" (jref json :error))))
      (multiple-value-bind (json status)
          (gui-request
           (format nil "/api/graphs/no-such/neighborhood/~A"
                   (string-id (getf *fixture* :alice))))
        (is (= 404 status))
        (is (string= "unknown-graph" (jref json :error)))))))

;;; ---------------------------------------------------------------------
;;; Query workbench: POST /api/graphs/:name/query (GH #278)
;;; ---------------------------------------------------------------------

(defun run-query (body &key (content-type "application/json"))
  "POST BODY (a JSON string) to the fixture graph's query endpoint."
  (gui-request "/api/graphs/gui-test-graph/query"
               :method :post :content body
               :content-type content-type))

(defun rows-of (json key)
  "The KEY column of every row in a query response, sorted."
  (sort (mapcar (lambda (row) (jref row key)) (jref json :rows))
        #'string< :key #'princ-to-string))

(test query-vertex-pattern-and-slot-bind
  "A vertex pattern plus a slot bind returns one row per match, keyed
by the result variable, with the shape the frontend renders."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status ctype raw)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"name\",
                         \"bind\":\"?n\"}],
             \"select\":[\"?n\"],\"limit\":10}")
        (declare (ignore ctype))
        (is (= 200 status))
        (is (equal '("n") (jref json :columns)))
        (is (= 2 (jref json :row-count)))
        (is (equal '("Alice" "Bob") (rows-of json :n)))
        (is (= 10 (jref json :limit)))
        (is-false (jref json :truncated))
        ;; Envelope keys are protocol and stay camelCase (GH #277).
        (dolist (key '("\"rowCount\"" "\"truncated\"" "\"columns\""
                       "\"rows\"" "\"limit\""))
          (is-true (search key raw) "key ~A is not camelCase" key))))))

(test query-accepts-kebab-schema-names
  "Type and slot names go in exactly as /types and /stats emit them --
kebab, multi-word included -- and the result key is the variable the
query named (GH #277, #278)."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status ctype raw)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"home-city\",
                         \"bind\":\"?hc\"}],
             \"select\":[\"?hc\"]}")
        (declare (ignore ctype))
        (is (= 200 status))
        (is (equal '("hc") (jref json :columns)))
        ;; Both persons match: NODE-SLOT-VALUE binds an unset slot to
        ;; NIL rather than failing, so Bob contributes a null row.
        (is (= 2 (jref json :row-count)))
        (is (member "Paris" (rows-of json :hc) :test #'equal))
        (is-true (search "\"hc\"" raw))))))

(test query-edge-pattern-joins-vertices
  "An edge pattern joins two variables; a slot value filters one end."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"},
                        {\"vertex\":\"?c\",\"type\":\"gui-city\"},
                        {\"edge\":\"gui-visited\",\"from\":\"?p\",
                         \"to\":\"?c\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"name\",
                         \"value\":\"Alice\"},
                        {\"slot\":\"?c\",\"name\":\"name\",
                         \"bind\":\"?cn\"}],
             \"select\":[\"?cn\"]}")
        (is (= 200 status))
        (is (equal '("Paris" "Tokyo") (rows-of json :cn)))))))

(test query-compare-constraint
  "A compare constraint filters on a bound numeric slot."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"age\",
                         \"bind\":\"?age\"},
                        {\"compare\":\">\",\"args\":[\"?age\",40]},
                        {\"slot\":\"?p\",\"name\":\"name\",
                         \"bind\":\"?n\"}],
             \"select\":[\"?n\"]}")
        (is (= 200 status))
        (is (equal '("Bob") (rows-of json :n)))))))

(test query-selects-node-ids-for-canvas-handoff
  "Selecting a pattern variable yields the engine's 32-hex string ids,
which is what the workbench hands to the explorer canvas (GH #278)."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"select\":[\"?p\"]}")
        (is (= 200 status))
        (let ((ids (rows-of json :p)))
          (is (= 2 (length ids)))
          (is (every (lambda (id)
                       (and (stringp id) (= 32 (length id))))
                     ids))
          (is (equal (sort (list (string-id (getf *fixture* :alice))
                                 (string-id (getf *fixture* :bob)))
                           #'string<)
                     ids)))))))

(test query-limit-is-enforced-and-reported
  "A client limit caps the rows and comes back as the bound the query
ran under.  TRUNCATED distinguishes an exactly-full page from a cut
one: the endpoint asks the runner for one row past the cap and never
shows it (GH #278)."
  (with-gui-fixture ()
    (with-gui-server ()
      ;; 2 persons, limit 1 -> cut.
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"name\",
                         \"bind\":\"?n\"}],
             \"select\":[\"?n\"],\"limit\":1}")
        (is (= 200 status))
        (is (= 1 (jref json :row-count)))
        (is (= 1 (length (jref json :rows))))
        (is (= 1 (jref json :limit)))
        (is-true (jref json :truncated)))
      ;; 2 persons, limit exactly 2 -> complete, NOT truncated.
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"name\",
                         \"bind\":\"?n\"}],
             \"select\":[\"?n\"],\"limit\":2}")
        (is (= 200 status))
        (is (= 2 (jref json :row-count)))
        (is (= 2 (length (jref json :rows))))
        (is (= 2 (jref json :limit)))
        (is-false (jref json :truncated)))
      ;; Limit well above the 2 matching cities: short of the cap, and
      ;; the probe row must not leak into the answer either way.
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?c\",\"type\":\"gui-city\"}],
             \"select\":[\"?c\"],\"limit\":4}")
        (is (= 200 status))
        (is (= 2 (jref json :row-count)))
        (is-false (jref json :truncated))))))

(test query-literal-with-leading-question-mark
  "A \"?\"-leading LITERAL must not become a variable.  The slot
\"value\" arm is already safe -- it passes the datum raw, so nothing
matches.  The DSL's COMPARE arm is asymmetric: it maps
%DSL-VAR-OR-LITERAL over its args and interns such a string as a query
variable, which makes the row vacuous.  The workbench therefore
refuses a \"?\"-leading comparison literal in the builder and offers an
explicit variable-vs-variable mode instead (GH #278).

⚠ The second assertion pins the DSL defect, NOT desired behaviour: it
is the tripwire for the follow-up that fixes %COMPILE-WHERE-CONSTRAINT.
When that lands, this assertion is what tells you to update it."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"name\",
                         \"value\":\"?zz\"}],
             \"select\":[\"?p\"]}")
        (is (= 200 status))
        (is (= 0 (jref json :row-count))
            "a \"?\"-leading slot value is a literal and matches none"))
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"age\",
                         \"bind\":\"?age\"},
                        {\"compare\":\"=\",
                         \"args\":[\"?age\",\"?zz\"]}],
             \"select\":[\"?p\"]}")
        (is (= 200 status))
        (is (= 2 (jref json :row-count))
            "DSL defect pinned: compare interns \"?zz\", so the row is ~
vacuous and every person matches.  The builder never emits this.")))))

(test query-error-contract
  "Malformed JSON, a non-object body, an unrecognized pattern, a
missing select and an unknown type are each a clean 400 carrying the
DSL's own reason; a closed graph is a 404."
  (with-gui-fixture ()
    (with-gui-server ()
      ;; Under application/json the lack layer parses the body before
      ;; ningle dispatches, so a syntax error is ITS plain-text 400 --
      ;; the handler never runs.  Same for rest.lisp's /query route.
      (multiple-value-bind (json status) (run-query "{not json")
        (declare (ignore json))
        (is (= 400 status)))
      ;; Under any other content type the body reaches the handler,
      ;; which answers the GUI's own {error, message} 400.
      (multiple-value-bind (json status)
          (run-query "{not json" :content-type "text/plain")
        (is (= 400 status))
        (is (string= "malformed-json" (jref json :error))))
      (multiple-value-bind (json status) (run-query "[1,2]")
        (is (= 400 status))
        (is (string= "malformed-query" (jref json :error))))
      (multiple-value-bind (json status)
          (run-query "{\"match\":[{\"bogus\":\"?p\"}],
                       \"select\":[\"?p\"]}")
        (is (= 400 status))
        (is (string= "bad-query" (jref json :error)))
        (is (search "unrecognized match pattern" (jref json :message))))
      (multiple-value-bind (json status)
          (run-query "{\"match\":[{\"vertex\":\"?p\",
                                   \"type\":\"gui-person\"}]}")
        (is (= 400 status))
        (is (string= "bad-query" (jref json :error)))
        (is (search "select" (jref json :message))))
      ;; An unknown type is a field value inside the query document, not
      ;; the addressed resource, so it is a 400 (like REST) -- unlike
      ;; /nodes?type=, where the type IS what is being addressed (404).
      (multiple-value-bind (json status)
          (run-query "{\"match\":[{\"vertex\":\"?p\",
                                   \"type\":\"no-such-type\"}],
                       \"select\":[\"?p\"]}")
        (is (= 400 status))
        (is (string= "bad-query" (jref json :error)))
        (is (search "unknown vertex type" (jref json :message))))
      (gui-request "/api/graphs/gui-test-graph/close" :method :post)
      (multiple-value-bind (json status)
          (run-query "{\"match\":[{\"vertex\":\"?p\",
                                   \"type\":\"gui-person\"}],
                       \"select\":[\"?p\"]}")
        (is (= 404 status))
        (is (string= "unknown-graph" (jref json :error))))
      (gui-request "/api/graphs/gui-test-graph/open" :method :post))))

(test query-ignores-ndjson-format-field
  "The DSL's REST-only NDJSON arm is not offered here: a \"format\"
field is dropped, so the answer is still one JSON object (GH #278)."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status ctype)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"name\",
                         \"bind\":\"?n\"}],
             \"select\":[\"?n\"],\"format\":\"ndjson\"}")
        (is (= 200 status))
        (is (eql 0 (search "application/json" ctype)))
        (is (= 2 (jref json :row-count)))))))
