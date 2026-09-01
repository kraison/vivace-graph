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

;;; ---------------------------------------------------------------------
;;; Free-text Prolog: POST /api/graphs/:name/prolog (GH #279)
;;;
;;; Mostly adversarial by design -- the guard is the unit, the editor is
;;; the accessory.  Every case here goes over real HTTP against a real
;;; graph, because that is the only place the whole chain (flag ->
;;; character screen -> restricted read -> whitelist walk -> bounded
;;; runner -> scratch teardown) is actually in series.
;;; ---------------------------------------------------------------------

(defun prolog-body (text &optional (limit 50))
  "A request body naming TEXT, with the keys spelled as the wire does."
  (json:with-explicit-encoder
    (json:encode-json-to-string
     (list :object (cons "query" text) (cons "limit" limit)))))

(defun run-prolog (text &key (limit 50) (graph "gui-test-graph"))
  "POST TEXT to GRAPH's free-text Prolog endpoint."
  (gui-request (format nil "/api/graphs/~A/prolog" graph)
               :method :post :content (prolog-body text limit)))

(defun refused-p (text &key (status 400) (code "refused-query"))
  "TEXT refused with STATUS and CODE; returns the server's message so a
caller can assert on what it named."
  (multiple-value-bind (json actual) (run-prolog text)
    (values (and (= status actual)
                 (string= code (jref json :error)))
            (jref json :message)
            actual)))

(defparameter *legit-query*
  "(is-a ?p gui-person) (node-slot-value ?p name ?n)"
  "A query that uses real functors and this graph's kebab schema names.")

;;; --- the flag ---------------------------------------------------------

(test prolog-refused-when-flag-off
  "Default off: a legitimate free-text query is a 403 from the ENDPOINT,
not from the UI hiding a tab.  /api/capabilities says so too, and ships
no functor inventory (GH #279)."
  (with-gui-fixture ()
    (with-gui-server ()
      (multiple-value-bind (json status) (run-prolog *legit-query*)
        (is (= 403 status))
        (is (string= "prolog-disabled" (jref json :error))))
      ;; The flag is checked before the graph is resolved, so an unknown
      ;; graph gets the same answer -- the endpoint leaks nothing.
      (multiple-value-bind (json status)
          (run-prolog *legit-query* :graph "no-such-graph")
        (is (= 403 status))
        (is (string= "prolog-disabled" (jref json :error))))
      (multiple-value-bind (json status ctype raw)
          (gui-request "/api/capabilities")
        (declare (ignore ctype))
        (is (= 200 status))
        (is-false (jref json :allow-prolog))
        (is-false (jref json :prolog))
        (is-true (search "\"allowProlog\":false" raw)
                 "capabilities did not spell allowProlog camelCase")))))

(test prolog-capability-advertised-when-flag-on
  "With :ALLOW-PROLOG the capability flips and the functor inventory
appears -- derived from the live registries, so is-a and
node-slot-value are in it and the withheld meta-call family is not."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (json status) (gui-request "/api/capabilities")
        (is (= 200 status))
        (is-true (jref json :allow-prolog))
        (let ((functors (jref (jref json :prolog) :functors)))
          (is-true (member "is-a" functors :test #'string=))
          (is-true (member "node-slot-value" functors :test #'string=))
          (is-true (member "not" functors :test #'string=))
          ;; The runtime meta-call family is withheld: it is the only
          ;; path left that interns from graph data.
          (dolist (name '("call" "findall" "bagof" "setof" "catch"))
            (is-false (member name functors :test #'string=)
                      "~A is advertised but must be withheld" name)))
        (is (= 4096 (jref (jref json :prolog) :max-query-length)))))))

;;; --- the legitimate path ---------------------------------------------

(test prolog-legitimate-query-shares-the-envelope
  "Real functors plus kebab schema names answer in unit B's envelope --
the same {columns, rows, rowCount, limit, truncated} the builder's
results table already renders (GH #278, #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (json status ctype raw)
          (run-prolog *legit-query*)
        (is (= 200 status))
        (is (eql 0 (search "application/json" ctype)))
        ;; Columns are the ?variables in first-appearance order.
        (is (equal '("p" "n") (jref json :columns)))
        (is (= 2 (jref json :row-count)))
        (is (equal '("Alice" "Bob") (rows-of json :n)))
        (is (= 50 (jref json :limit)))
        (is-false (jref json :truncated))
        (dolist (key '("\"rowCount\"" "\"truncated\"" "\"columns\""
                       "\"rows\"" "\"limit\""))
          (is-true (search key raw) "key ~A is not camelCase" key))))))

(test prolog-multi-word-schema-names-and-edges
  "Multi-word kebab slot names and an edge functor both go in exactly as
/types and /stats spell them (GH #277)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person)
                       (node-slot-value ?p home-city ?hc)")
        (is (= 200 status))
        (is (equal '("p" "hc") (jref json :columns)))
        (is-true (member "Paris" (rows-of json :hc) :test #'equal)))
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person) (is-a ?c gui-city)
                       (gui-visited ?p ?c)
                       (node-slot-value ?c name ?cn)")
        (is (= 200 status))
        (is (equal '("Paris" "Paris" "Tokyo") (rows-of json :cn)))))))

(test prolog-limit-is-enforced-and-reported
  "The client limit caps the rows and comes back as the bound the query
ran under, with the same probe-row rule unit B uses."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (json status)
          (run-prolog *legit-query* :limit 1)
        (is (= 200 status))
        (is (= 1 (jref json :row-count)))
        (is (= 1 (jref json :limit)))
        (is-true (jref json :truncated)))
      (multiple-value-bind (json status)
          (run-prolog *legit-query* :limit 2)
        (is (= 200 status))
        (is (= 2 (jref json :row-count)))
        (is-false (jref json :truncated))))))

;;; --- reader-level attacks ---------------------------------------------

(test prolog-reader-eval-refused
  "#. and every other # reader macro are refused BEFORE the read, along
with backquote and comma.  Read-time evaluation is the attack the
character screen exists for (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (dolist (text '("#.(cl:print 1)"
                      "(is-a ?p #.(cl:error \"x\"))"
                      "(is-a ?p gui-person) #.(cl:list 1)"
                      "(is-a ?p #(1 2 3))"
                      "(is-a ?p #1=(?x))"
                      "(is-a ?p #'car)"
                      "(is-a ?p `(,x))"))
        (multiple-value-bind (ok message status) (refused-p text)
          (is-true ok "~S was not refused (status ~A)" text status)
          (is-true (or (search "reader macro" message)
                       (search "not permitted" message))
                   "~S refused with an unhelpful message: ~A"
                   text message))))))

(test prolog-hash-dot-inside-a-string-is-data
  "#. inside a string literal is DATA, not reader syntax, and must not
be refused -- the screen refuses syntax, not text (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person)
                       (node-slot-value ?p name \"#.(cl:print 1)\")")
        (is (= 200 status))
        (is (= 0 (jref json :row-count))
            "the literal matched something; it should match nothing"))
      ;; ...and one that DOES match, so the literal really is compared.
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person)
                       (node-slot-value ?p name \"Alice\")")
        (is (= 200 status))
        (is (= 1 (jref json :row-count)))))))

(test prolog-package-qualified-refused
  "A package-qualified name is refused by NAME, before READ -- which is
the point: READ would intern GRAPH-DB::ANYTHING on its way to telling
us about it (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (dolist (case '(("(cl:load \"/etc/passwd\")" . "cl:load")
                      ("(graph-db::close-graph ?g)"
                       . "graph-db::close-graph")
                      ("(sb-ext:run-program \"/bin/sh\" ?a)"
                       . "sb-ext:run-program")
                      ("(is-a ?p cl-user::gui-person)"
                       . "cl-user::gui-person")
                      ("(is-a ?p :gui-person)" . ":gui-person")))
        (multiple-value-bind (ok message status) (refused-p (car case))
          (is-true ok "~S was not refused (status ~A)" (car case) status)
          (is-true (search (cdr case) message)
                   "the refusal did not name ~S: ~A"
                   (cdr case) message))))))

(test prolog-unbalanced-input-is-a-clean-400
  "Unbalanced or truncated input is a 400 naming the imbalance, never a
hang and never a 500."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (ok message) (refused-p "(is-a ?p gui-person")
        (is-true ok)
        (is-true (search "unbalanced" message)))
      (multiple-value-bind (ok message) (refused-p "(is-a ?p) )")
        (is-true ok)
        (is-true (search "unbalanced" message)))
      (multiple-value-bind (ok message)
          (refused-p "(node-slot-value ?p name \"unterminated)")
        (is-true ok)
        (is-true (search "unterminated" message)))
      ;; An empty query is a refusal, not an empty answer.
      (is-true (refused-p "")))))

(test prolog-input-caps-refuse
  "The length cap and the nesting cap both refuse, and both do it
before the reader runs."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (let ((long (concatenate 'string *legit-query*
                               (make-string 5000
                                            :initial-element #\Space))))
        (multiple-value-bind (ok message) (refused-p long)
          (is-true ok)
          (is-true (search "the limit is 4096" message))))
      (let ((deep (concatenate 'string
                               (make-string 40 :initial-element #\()
                               (make-string 40 :initial-element #\)))))
        (multiple-value-bind (ok message) (refused-p deep)
          (is-true ok)
          (is-true (search "nesting deeper" message)))))))

;;; --- whitelist attacks -------------------------------------------------

(test prolog-unknown-functor-refused
  "An unregistered head is refused with its own name and arity, before
anything compiles."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (ok message) (refused-p "(no-such-functor ?x)")
        (is-true ok)
        (is-true (search "no-such-functor/1" message)))
      ;; Right name, wrong arity: the whitelist is keyed by both.
      (multiple-value-bind (ok message) (refused-p "(is-a ?p)")
        (is-true ok)
        (is-true (search "is-a/1" message)))
      ;; A schema name is not a predicate, and a slot name is not a type.
      (is-true (refused-p "(gui-person ?p)"))
      (is-true (refused-p "(is-a ?p not-a-type)")))))

(test prolog-non-symbol-head-refused
  "A STRING head is the sharp case: COMPILE-BODY hands it to
PROLOG-COMPILER-MACRO, which INTERNS it into GRAPH-DB.  Heads must be
symbols (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (ok message)
          (refused-p "(\"is-a\" ?p gui-person)")
        (is-true ok)
        (is-true (search "must be a symbol" message)))
      (is-true (refused-p "(42 ?p)"))
      (is-true (refused-p "((is-a ?p gui-person) ?x)"))
      ;; A dotted form cannot be a goal either.
      (multiple-value-bind (ok message) (refused-p "(is-a ?p . ?q)")
        (is-true ok)
        (is-true (search "improper" message))))))

(test prolog-runtime-meta-call-refused
  "The meta-call family is withheld and a control construct will not
take a variable where a goal belongs -- together that closes %SOLVE,
the last place a functor symbol is interned from run-time data."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (dolist (text '("(call ?g) (is-a ?p gui-person)"
                      "(findall ?x (is-a ?x gui-person) ?r)"
                      "(bagof ?x (is-a ?x gui-person) ?r)"
                      "(setof ?x (is-a ?x gui-person) ?r)"
                      "(catch (is-a ?p gui-person) ?b (fail))"
                      "(select (?p) (?p))"))
        (is-true (refused-p text) "~S was not refused" text))
      (multiple-value-bind (ok message)
          (refused-p "(is-a ?p gui-person) (not ?g)")
        (is-true ok)
        (is-true (search "takes goals" message)))
      ;; ...while the same construct with a real goal still works.
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person)
                       (not (node-slot-value ?p name \"Alice\"))")
        (is (= 200 status))
        (is (= 1 (jref json :row-count)))))))

;;; --- the rails behind the guard ---------------------------------------

(test prolog-effectful-goals-are-403
  "A whitelisted but effectful predicate passes the guard and is stopped
by :EFFECTS NIL at run time -- the 403 path, exactly as the issue
specifies."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (dolist (text '("(nl) (is-a ?p gui-person)"
                      "(is-a ?p gui-person) (write ?p)"
                      "(is-a ?p gui-person) (lisp ?r ?p)"
                      "(is-a ?p gui-person) (retract ?p)"))
        (multiple-value-bind (json status) (run-prolog text)
          (is (= 403 status) "~S was not a 403 (got ~A)" text status)
          (is (string= "forbidden-operation" (jref json :error))))))))

(test prolog-resource-limits-stop-a-runaway
  "A deliberately unbounded query hits the inference cap and answers
400, rather than hanging the handler.  The cap is dropped for the test
so the answer is fast; the plumbing is the same at the default."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (let ((saved graph-db::*query-default-max-inferences*))
        (unwind-protect
             (progn
               ;; Global, not a LET: the handler runs in a server thread.
               (setf graph-db::*query-default-max-inferences* 5000)
               (multiple-value-bind (json status)
                   (run-prolog "(repeat) (fail) (is-a ?p gui-person)")
                 (is (= 400 status))
                 (is (string= "query-too-expensive"
                              (jref json :error)))))
          (setf graph-db::*query-default-max-inferences* saved))))))

(test prolog-error-contract
  "Malformed JSON, a non-object body, a missing \"query\" and a closed
graph each answer the GUI's own {error, message}."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/prolog"
                       :method :post :content "{not json"
                       :content-type "text/plain")
        (is (= 400 status))
        (is (string= "malformed-json" (jref json :error))))
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/prolog"
                       :method :post :content "[1,2]")
        (is (= 400 status))
        (is (string= "malformed-query" (jref json :error))))
      (multiple-value-bind (json status)
          (gui-request "/api/graphs/gui-test-graph/prolog"
                       :method :post :content "{\"limit\":10}")
        (is (= 400 status))
        (is (string= "refused-query" (jref json :error)))
        (is (search "\"query\" string" (jref json :message))))
      (gui-request "/api/graphs/gui-test-graph/close" :method :post)
      (multiple-value-bind (json status) (run-prolog *legit-query*)
        (is (= 404 status))
        (is (string= "unknown-graph" (jref json :error))))
      (gui-request "/api/graphs/gui-test-graph/open" :method :post))))

;;; --- the reclaim ------------------------------------------------------

(defun package-symbol-count (package)
  (let ((n 0))
    (do-symbols (s package) (declare (ignore s)) (incf n))
    n))

(defun scratch-packages ()
  (remove-if-not (lambda (p)
                   (eql 0 (search "GRAPH-DB.GUI.SCRATCH-"
                                  (package-name p))))
                 (list-all-packages)))

(test prolog-hostile-queries-do-not-grow-the-image
  "Repeated hostile queries leave nothing behind: no scratch package, no
new symbol in GRAPH-DB, none in KEYWORD, and no new package at all.
This is the reclaim step -- everything a query's text interns is
interned in a package that dies with the request (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      ;; One of each shape first: the bounded, one-time interning (a
      ;; canonical head, a functor NAME/ARITY in the schema package) has
      ;; to have happened before the baseline is taken.
      (run-prolog *legit-query*)
      (run-prolog "(zzz-warm ?x)")
      (run-prolog "(graph-db::zzz-warm ?x)")
      (is (null (scratch-packages))
          "a scratch package survived its request")
      (let ((packages (length (list-all-packages)))
            (gdb (package-symbol-count :graph-db))
            (kw (package-symbol-count :keyword)))
        (dotimes (i 50)
          ;; Distinct names every time -- the whole point of the attack
          ;; is that each one would be a NEW symbol if it were interned.
          (run-prolog (format nil "(hostile-~D ?v-~D)" i i))
          (run-prolog (format nil "(graph-db::hostile-~D ?x)" i))
          (run-prolog (format nil "(is-a ?p hostile-type-~D)" i))
          (run-prolog (format nil "#.(cl:intern \"HOSTILE-~D\")" i))
          (run-prolog (format nil "(is-a ?p |hostile-bar-~D|)" i))
          ;; A bogus JSON FIELD, not a bogus goal: cl-json's default
          ;; decoder interns every object key it meets as a keyword,
          ;; which is the same attack one layer up (GH #279).
          (gui-request "/api/graphs/gui-test-graph/prolog"
                       :method :post
                       :content
                       (format nil "{\"query\":\"(is-a ?p gui-person)\",
                                     \"zzHostileField~D\":~D}" i i)))
        (is (null (scratch-packages))
            "~D scratch package(s) survived"
            (length (scratch-packages)))
        (is (= packages (length (list-all-packages)))
            "the image gained ~D package(s)"
            (- (length (list-all-packages)) packages))
        (is (= gdb (package-symbol-count :graph-db))
            "GRAPH-DB gained ~D symbol(s)"
            (- (package-symbol-count :graph-db) gdb))
        (is (= kw (package-symbol-count :keyword))
            "KEYWORD gained ~D symbol(s)"
            (- (package-symbol-count :keyword) kw))))))

;;; --- the editor's assets ----------------------------------------------

(test static-codemirror-assets-serve
  "The vendored CodeMirror 5 build, its stylesheet and the Common Lisp
mode all serve with the right content types, and the bundle is
non-trivially large (GH #279)."
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
      (let ((cm (asset "/vendor/codemirror.js"
                       "application/javascript")))
        (is (> (length cm) 100000)
            "codemirror.js is suspiciously small (~A bytes)"
            (length cm))
        (is-true (search "MIT license" cm)
                 "the MIT notice is missing from codemirror.js"))
      (asset "/vendor/codemirror.css" "text/css")
      (asset "/vendor/codemirror-commonlisp.js"
             "application/javascript")
      (asset "/js/prolog.js" "application/javascript")
      (asset "/js/wb-results.js" "application/javascript"))))

;;; ---------------------------------------------------------------------
;;; Drift tripwire for the guard's two hand-maintained exclusion lists
;;; (GH #279).
;;;
;;; The whitelist is ENUMERATED from the live registries, so a functor
;;; added to the engine tomorrow is admitted to free text
;;; automatically.  The exclusions are matched by hand-written string,
;;; so they do NOT grow with it -- and nothing else in this suite would
;;; notice.  PROLOG-RUNTIME-META-CALL-REFUSED pins today's family by
;;; name; it cannot see a new arrival.  This test can.
;;;
;;; Same shape as QUERY-LITERAL-WITH-LEADING-QUESTION-MARK above: an
;;; assertion whose job is to fail loudly and tell the next person what
;;; to do about it.
;;; ---------------------------------------------------------------------

(defun registered-functor-inventory ()
  "Every registered Prolog functor in this image, as sorted lowercase
\"name/arity\" strings.  Enumerated from *PROLOG-GLOBAL-FUNCTORS* and
*USER-FUNCTORS* exactly as GRAPH-DB.GUI::%FUNCTOR-WHITELIST does --
interned keys only, so SELECT's transient per-query gensym functor is
skipped -- but WITHOUT the exclusions, because the exclusions are the
thing under test."
  (let ((names '()))
    (flet ((collect (table)
             (maphash (lambda (key value)
                        (declare (ignore value))
                        (when (and (symbolp key) (symbol-package key))
                          (pushnew (string-downcase (symbol-name key))
                                   names :test #'string=)))
                      table)))
      (collect graph-db::*prolog-global-functors*)
      (collect graph-db::*user-functors*))
    (sort names #'string<)))

(defparameter *reviewed-functor-inventory*
  '("/=/2"
    "</2"
    "<=/2"
    "=/2"
    "==/2"
    ">/2"
    ">=/2"
    "atom/1"
    "bagof/3"
    "call/1"
    "catch/3"
    "fail/0"
    "find-by-slot/4"
    "find-intersects/3"
    "find-near/5"
    "find-nearest/5"
    "find-slot-range/5"
    "find-within/3"
    "findall/3"
    "forall/2"
    "geo-distance/5"
    "geo-near/5"
    "geo-within/3"
    "gui-visited/2"
    "gui-visited/3"
    "if/2"
    "if/3"
    "incoming-edges/2"
    "incoming-edges/3"
    "incoming-edges/4"
    "invoke-reduced-view/4"
    "invoke-view/4"
    "invoke-view/5"
    "is-a/2"
    "is/2"
    "lisp/2"
    "lispp/1"
    "map-query/5"
    "nl/0"
    "node-slot-value/3"
    "not-in-list/2"
    "not/1"
    "numberp/1"
    "once/1"
    "or/2"
    "outgoing-edges/2"
    "outgoing-edges/3"
    "outgoing-edges/4"
    "param/2"
    "read/1"
    "regex-match/2"
    "repeat/0"
    "retract/1"
    "retract/3"
    "select/2"
    "setof/3"
    "show-prolog-vars/2"
    "throw/1"
    "trigger/1"
    "unique/1"
    "valid-date-p/1"
    "var/1"
    "weight/2"
    "write/1")
  "The Prolog functors this image is REVIEWED to register, one per
line, sorted.  Reviewed means each was read and answered for BOTH
questions the failure message asks: does it meta-call %SOLVE, and is
its worst-case cost bounded by the graph or by the query's length.
The two GUI-VISITED entries are the fixture schema's own edge functors,
which DEF-EDGE installs into *PROLOG-GLOBAL-FUNCTORS* -- their presence
is the record that a graph's edge predicates reach free text too.

Do not update this list to make the suite pass.  Read
PROLOG-FUNCTOR-INVENTORY-IS-PINNED's failure message first.")

(defun %inventory-guidance ()
  "The standing explanation both drift messages end with."
  (format nil "~
Why this test exists: the free-text Prolog guard (gui/prolog.lisp, GH ~
#279) derives its whitelist BY ENUMERATING these registries, so any ~
functor in them is already reachable from client text on a GUI started ~
with :ALLOW-PROLOG T.  Its three hand-maintained lists -- ~
GRAPH-DB.GUI::*PROLOG-EXCLUDED-PREDICATES*, ~
...::*PROLOG-GOAL-ARGUMENT-CONTROL* and ~
...::*PROLOG-COST-UNBOUNDED-PREDICATES* -- are matched by hand-written ~
string and do NOT grow with the registries.  That gap is what this ~
list closes."))

(defun %added-functor-message (added)
  (format nil "~
The image registers ~D Prolog functor(s) the reviewed inventory does ~
not know about:~%~%    ~{~A~^ ~}~%~%~
Before adding them to *REVIEWED-FUNCTOR-INVENTORY* in this file, read ~
each one's definition and answer BOTH questions below.  Answering only ~
the first is how a hole gets shipped.~%~%~
QUESTION 1 -- can a caller hand it a GOAL?  That is, does it reach ~
%SOLVE at run time, directly or through CALL/1, %SOLVE-CALL or ~
%SOLVE-AGGREGATE?~%~
  * Yes, and the goal is an ordinary argument that could be bound to a ~
variable (an apply/2, an aggregate-all/3): add its NAME to ~
*PROLOG-EXCLUDED-PREDICATES* in gui/prolog.lisp.~%~
  * Yes, and it is a control construct whose arguments ARE goals (a ~
new sibling of not/once/if/forall): add its NAME to ~
*PROLOG-GOAL-ARGUMENT-CONTROL* there instead -- that forces its ~
arguments to be written-out goals, which keeps it on the compile-time ~
path.~%~
  * No: nothing to do for this question.~%~%~
Note on HOW an exclusion is enforced, if you add one: it is matched ~
against GRAPH-DB-homed registry keys, so a schema owning the same name ~
keeps its own predicate -- AND, independently, against ROUTING, so a ~
head spelled like a GRAPH-DB compiler macro is refused whatever ~
package it came from.  You do not need to add anything for the second: ~
%ROUTES-TO-ENGINE-CONTROL-P asks the image.  You DO need to know it ~
exists, because it is why excluding CALL actually holds.~%~%~
Why: %SOLVE builds a functor symbol from its goal's head AT RUN TIME ~
(prolog-functors.lisp:229).  A head that turns out to be a STRING -- ~
reachable through a variable bound to graph data -- is then an INTERN ~
of unvetted content into the live schema package: unbounded symbol ~
growth driven by a client's query.~%~%~
QUESTION 2 -- is its worst-case cost bounded?  Bounded means bounded ~
by the GRAPH (a scan, an index range) or by the LENGTH of the query ~
(which the 4096-character cap already bounds).~%~
  * Bounded either way: nothing to do for this question.~%~
  * Unbounded by both -- it can burn arbitrary time in ONE Lisp call ~
driven by client literals (a regex over client-supplied pattern AND ~
subject, an unbounded string or sequence operation, a loop over a ~
client-supplied number): add its NAME to ~
*PROLOG-COST-UNBOUNDED-PREDICATES* in gui/prolog.lisp.~%~%~
Why: the query rails are enforced by %TICK at inference and goal ~
boundaries, never inside a functor that is already running, so ~
*QUERY-DEFAULT-TIMEOUT* and *QUERY-DEFAULT-MAX-INFERENCES* cannot ~
preempt it.  REGEX-MATCH/2 is the worked example: (a+)+$ against a run ~
of a's ran ~~2^n and blew a 30 s deadline from a payload of a few ~
dozen characters.  A watchdog is NOT the alternative -- interrupting a ~
worker mid-call can unwind holding the GUI rw lock or with an mmap ~
operation in flight.~%~%~A"
          (length added) added (%inventory-guidance)))

(defun %removed-functor-message (removed)
  (format nil "~
The reviewed inventory names ~D Prolog functor(s) this image does not ~
register:~%~%    ~{~A~^ ~}~%~%~
Usually a rename or a deleted predicate.  Drop them from ~
*REVIEWED-FUNCTOR-INVENTORY* in this file -- and if any of those names ~
also appears in *PROLOG-EXCLUDED-PREDICATES*, ~
*PROLOG-GOAL-ARGUMENT-CONTROL* or *PROLOG-COST-UNBOUNDED-PREDICATES* ~
in gui/prolog.lisp, remove it there too, so those lists do not ~
accumulate names that no longer exist and quietly stop meaning ~
anything.~%~%~A"
          (length removed) removed (%inventory-guidance)))

(test prolog-functor-inventory-is-pinned
  "⚠ TRIPWIRE, not a behaviour test.  The guard's whitelist grows with
the engine automatically; its exclusion lists do not.  This fails the
moment the set of registered functors changes, so a newly added
meta-calling predicate cannot reach free text unreviewed (GH #279)."
  (let* ((actual (registered-functor-inventory))
         (added (sort (set-difference actual *reviewed-functor-inventory*
                                      :test #'string=)
                      #'string<))
         (removed (sort (set-difference *reviewed-functor-inventory*
                                        actual :test #'string=)
                        #'string<)))
    (is (null added) "~A" (%added-functor-message added))
    (is (null removed) "~A" (%removed-functor-message removed))
    ;; A passing run still says what it checked.
    (is (= (length *reviewed-functor-inventory*) (length actual)))))

;;; ---------------------------------------------------------------------
;;; Cost-unbounded predicates, and the error contract's own leaks
;;; (GH #279, second security review).
;;; ---------------------------------------------------------------------

(defun redos-payload (n)
  "The classic catastrophic-backtracking pair: a nested quantifier and
a run of N a's that cannot match.  cl-ppcre:scan on this is ~2^n."
  (format nil "(= ?x 1) (regex-match \"(a+)+$\" \"~AX\")"
          (make-string n :initial-element #\a)))

(test prolog-cost-unbounded-predicate-refused
  "REGEX-MATCH/2 is refused outright.  It takes BOTH the pattern and
the subject from the client, and %TICK cannot preempt a functor that is
already running -- so a catastrophically backtracking pattern runs past
*QUERY-DEFAULT-TIMEOUT* unaborted.  Measured before the exclusion: 26
a's took 6.0s, 29 took 51.2s and still answered 200 (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      ;; The payload the exclusion exists for, refused before it runs.
      (let ((start (get-internal-real-time)))
        (multiple-value-bind (ok message) (refused-p (redos-payload 30))
          (is-true ok "the ReDoS payload was not refused")
          (is-true (search "regex-match/2" message)
                   "the refusal did not name the predicate: ~A" message))
        (is (< (/ (- (get-internal-real-time) start)
                  internal-time-units-per-second)
               5)
            "refusing the ReDoS payload should be instant"))
      ;; Refused at every arity and in every position, not just as a
      ;; leading goal.
      (is-true (refused-p "(is-a ?p gui-person) (regex-match \"a\" \"b\")"))
      (is-true (refused-p "(is-a ?p gui-person) (not (regex-match ?a ?b))"))
      ;; ...and the exclusion is SCOPED: the sibling that also runs a
      ;; regex stays, because its pattern is a fixed anchored constant
      ;; and its cost is linear in a subject the length cap bounds.
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person)
                       (valid-date-p \"2026-08-28\")")
        (is (= 200 status) "valid-date-p/1 must remain usable")
        (is (= 2 (jref json :row-count))))
      ;; ...and an ordinary query is untouched.
      (multiple-value-bind (json status) (run-prolog *legit-query*)
        (is (= 200 status))
        (is (= 2 (jref json :row-count)))))))

(test prolog-cost-unbounded-predicate-not-advertised
  "A withheld predicate is absent from the capability inventory too, so
the editor does not offer what the guard refuses."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (multiple-value-bind (json status) (gui-request "/api/capabilities")
        (is (= 200 status))
        (let ((functors (jref (jref json :prolog) :functors)))
          (is-false (member "regex-match" functors :test #'string=))
          ;; The scoped sibling is still offered.
          (is-true (member "valid-date-p" functors :test #'string=)))))))

(defparameter *internal-leak-markers*
  '("no applicable method" "ANSI Standard" "secondary index"
    "GRAPH-DB" "graph-db::" "GUI-TEST-GRAPH" "#<" "SB-" "sb-pcl")
  "Substrings that betray engine internals.  None may appear in any
response body from the free-text endpoint, on any path (GH #279).")

(defun body-has-no-internals (raw label)
  (dolist (leak *internal-leak-markers* t)
    (is-false (search leak raw) "~A leaked ~S: ~A" label leak raw)))

(test prolog-ill-typed-goals-are-400-without-internals
  "A well-formed query whose goals are ill-typed is the CLIENT's error:
400, with a generic message.  Several whitelisted read functors raise
conditions that are not PROLOG-ERROR subtypes and whose reports name
engine internals -- a store's keyword name, a generic-function name, an
ANSI section number.  None of that may reach the browser (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      ;; (incoming-edges ?a ?b ?c) is NOT here, and the reason is a
      ;; degenerate path rather than tolerance: an unbound EDGE TYPE
      ;; short-circuits MAP-EDGES before the vertex is dereferenced, so
      ;; it answers 200 having done nothing.  Bind the type and the
      ;; natural "all visits" query below is a 400 like the rest.
      (dolist (text '("(find-by-slot ?a ?b ?c ?d)"
                      "(invoke-view ?a ?b ?c ?d)"
                      "(outgoing-edges ?a ?b)"
                      "(incoming-edges ?v gui-visited ?e)"))
        (multiple-value-bind (json status ctype raw) (run-prolog text)
          (declare (ignore ctype))
          (is (= 400 status) "~S answered ~A, not 400" text status)
          (is (string= "ill-typed-query" (jref json :error))
              "~S answered error code ~A" text (jref json :error))
          (body-has-no-internals raw text)))
      ;; The reviewed Prolog conditions keep their own messages, which
      ;; are actionable and were written to be client-facing.
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person) (lisp ?r ?p)")
        (is (= 403 status))
        (is (string= "forbidden-operation" (jref json :error)))))))

(test prolog-reader-errors-do-not-echo-implementation-text
  "A numeric literal the reader rejects is a clean 400 that says the
query could not be read -- not SBCL's own reader-error report."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (dolist (text '("(= ?x 1/0)" "(= ?x 1e999999)"))
        (multiple-value-bind (json status ctype raw) (run-prolog text)
          (declare (ignore ctype))
          (is (= 400 status) "~S answered ~A" text status)
          (is (string= "refused-query" (jref json :error)))
          (is-true (search "could not be read" (jref json :message))
                   "~S: ~A" text (jref json :message))
          (body-has-no-internals raw text)
          (dolist (leak '("READER-ERROR" "DIVISION-BY-ZERO"
                          "FLOATING-POINT"))
            (is-false (search leak raw)
                      "~S leaked ~S: ~A" text leak raw)))))))

(defmacro with-broken-functor ((functor-name) &body body)
  "Run BODY with the global functor named by FUNCTOR-NAME (a
\"name/arity\" string) replaced by one that signals a plain error, then
restore it.

Provoking a GENUINE engine fault cheaply and without contriving one:
swapping an EXISTING registry entry's implementation leaves the
registry's key set untouched, so PROLOG-FUNCTOR-INVENTORY-IS-PINNED is
unaffected, and the swap is visible to the server thread because
COMPILE-CALL looks the entry up at run time (GH #279)."
  (let ((key (gensym "KEY")) (saved (gensym "SAVED")))
    `(let* ((,key (find-symbol (string-upcase ,functor-name) :graph-db))
            (,saved (gethash ,key graph-db::*prolog-global-functors*)))
       (assert ,saved () "~A is not a registered global functor"
               ,functor-name)
       (unwind-protect
            (progn
              (setf (gethash ,key graph-db::*prolog-global-functors*)
                    (lambda (&rest args)
                      (declare (ignore args))
                      ;; A TYPE-ERROR: the classic shape of a real
                      ;; defect (a NIL where a struct was expected), and
                      ;; deliberately NOT in the ill-typed family.
                      (car 42)))
              ,@body)
         (setf (gethash ,key graph-db::*prolog-global-functors*)
               ,saved)))))

(test prolog-genuine-fault-is-500-and-still-leaks-nothing
  "A real engine defect during execution must NOT be labelled the
client's.  A TYPE-ERROR -- a NIL dereference, the classic defect shape
-- is deliberately outside the ill-typed family, so it answers 500 with
a fixed generic body.  Both properties at once: no leak AND
diagnosable, since the two cases log under distinct labels (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      ;; Baseline: the functor works, so the 500 below is the swap.
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person) (valid-date-p \"2026-08-28\")")
        (is (= 200 status))
        (is (= 2 (jref json :row-count))))
      (with-broken-functor ("valid-date-p/1")
        (multiple-value-bind (json status ctype raw)
            (run-prolog
             "(is-a ?p gui-person) (valid-date-p \"2026-08-28\")")
          (declare (ignore ctype))
          (is (= 500 status)
              "a genuine fault answered ~A, not 500" status)
          (is (string= "internal-error" (jref json :error))
              "a genuine fault must not be labelled the client's ~
(got ~A)" (jref json :error))
          ;; Against the server's own parameter, so the wire text and
          ;; the source cannot drift apart.
          (is (string= graph-db.gui::*prolog-internal-error-message*
                       (jref json :message)))
          (body-has-no-internals raw "genuine fault")
          ;; The condition's own report must not survive either.
          (is-false (search "42" raw))
          (is-false (search "TYPE-ERROR" raw))))
      ;; Restored: the swap did not outlive the test.
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person) (valid-date-p \"2026-08-28\")")
        (is (= 200 status))
        (is (= 2 (jref json :row-count))))
      ;; ...and an ill-typed goal is still the client's 400, so the
      ;; narrowing did not simply reclassify everything as a fault.
      (multiple-value-bind (json status raw)
          (run-prolog "(find-by-slot ?a ?b ?c ?d)")
        (declare (ignore raw))
        (is (= 400 status))
        (is (string= "ill-typed-query" (jref json :error)))))))

;;; ---------------------------------------------------------------------
;;; Request-size gate, and the exclusions' scope (GH #279, third review).
;;; ---------------------------------------------------------------------

(defun big-body (bytes)
  "A syntactically valid JSON body of at least BYTES, whose oversize is
in a STRING VALUE -- so nothing but the size can be what refuses it."
  (format nil "{\"query\":\"(is-a ?p gui-person)\",\"pad\":\"~A\"}"
          (make-string bytes :initial-element #\x)))

(test oversize-request-body-is-refused-before-it-is-decoded
  "A body past the cap is a 413 from the DISPATCHER, before ningle --
which is the only place it can be caught, because lack parses an
application/json body while building the request.  Both POST endpoints,
since the builder's (GH #278) shares the hazard: a 32 MB body cost
~7.3 s of CPU on each before any handler could object."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (let ((body (big-body (* 2 1024 1024))))   ; 2 MB, 32x the cap
        (dolist (path '("/api/graphs/gui-test-graph/prolog"
                        "/api/graphs/gui-test-graph/query"))
          (let ((start (get-internal-real-time)))
            (multiple-value-bind (json status)
                (gui-request path :method :post :content body)
              (is (= 413 status) "~A answered ~A, not 413" path status)
              (is (string= "request-too-large" (jref json :error))
                  "~A answered error code ~A" path (jref json :error)))
            (is (< (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second)
                   5)
                "~A took too long to refuse an oversize body" path))))
      ;; The gate is on SIZE only: a body under the cap still runs, and
      ;; an over-long QUERY STRING inside a legal body is still the
      ;; guard's own 400, not a 413.
      (multiple-value-bind (json status) (run-prolog *legit-query*)
        (is (= 200 status))
        (is (= 2 (jref json :row-count))))
      (multiple-value-bind (ok message)
          (refused-p (concatenate 'string *legit-query*
                                  (make-string 5000
                                               :initial-element #\Space)))
        (is-true ok)
        (is-true (search "the limit is 4096" message))))))

(test excluded-names-are-scoped-to-engine-predicates
  "The exclusions name ENGINE predicates and must key off GRAPH-DB-homed
registry entries.  The fixture's own edge functor proves a schema's
predicates survive the filter; if the exclusions matched by bare name
across every origin, a schema declaring an edge type called REGEX-MATCH
would find its auto-installed functors silently dropped, and the
inventory tripwire -- which watches the registry, not the schema --
would not catch it (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      ;; The schema's edge functor is advertised and usable.
      (multiple-value-bind (json status) (gui-request "/api/capabilities")
        (is (= 200 status))
        (is-true (member "gui-visited"
                         (jref (jref json :prolog) :functors)
                         :test #'string=)))
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person) (is-a ?c gui-city)
                       (gui-visited ?p ?c)")
        (is (= 200 status))
        ;; The fixture's three visits: Alice->Paris, Alice->Tokyo,
        ;; Bob->Paris.
        (is (= 3 (jref json :row-count))))
      ;; ...while the engine predicate of the same category is still out.
      (is-true (refused-p "(regex-match \"a\" \"b\")")))))

;;; ---------------------------------------------------------------------
;;; Exclusion by ROUTING, not by home package (GH #279, third review).
;;; ---------------------------------------------------------------------

(defparameter *foreign-schema-package*
  (or (find-package '#:graph-db/gui-test-foreign-schema)
      (make-package '#:graph-db/gui-test-foreign-schema :use '()))
  "Stand-in for a schema defined in its OWN package -- the runtime-schema
shape of GH #172, where DEF-EDGE installs NAME/2 and NAME/3 into
*PROLOG-GLOBAL-FUNCTORS* homed there rather than in GRAPH-DB.  It USES
nothing, so a symbol interned here is genuinely foreign, exactly as a
real operator schema's would be.")

(defmacro with-foreign-schema-functor ((name arity) &body body)
  "Register NAME/ARITY in *PROLOG-GLOBAL-FUNCTORS* homed in
*FOREIGN-SCHEMA-PACKAGE*, run BODY, then remove it.

Registered and removed inside one test, so
PROLOG-FUNCTOR-INVENTORY-IS-PINNED -- which pins the registry's key set
-- is unaffected."
  (let ((key (gensym "KEY")))
    `(let ((,key (intern (format nil "~A/~D" (string-upcase ,name) ,arity)
                         *foreign-schema-package*)))
       (unwind-protect
            (progn
              (setf (gethash ,key graph-db::*prolog-global-functors*)
                    (lambda (&rest args) (declare (ignore args)) nil))
              ,@body)
         (remhash ,key graph-db::*prolog-global-functors*)))))

(test excluded-name-is-refused-by-routing-not-by-home-package
  "Scoping the exclusions to GRAPH-DB-homed registry keys excluded by
HOME while the compiler routes by NAME: PROLOG-COMPILER-MACRO
canonicalizes a foreign-package head back into GRAPH-DB, so a
schema-package CALL/2 was admitted by the whitelist and then compiled by
the engine's CALL macro, re-opening the runtime meta-call.  The guard
now asks the same question the compiler will (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      ;; The exact scenario: a schema-package-homed CALL/2.
      (with-foreign-schema-functor ("CALL" 2)
        (multiple-value-bind (ok message) (refused-p "(call ?g ?x)")
          (is-true ok "a schema-homed CALL/2 re-opened the meta-call")
          (is-true (search "control macro" message)
                   "the refusal did not explain the routing: ~A" message))
        ;; The data-driven shape the exclusion exists for, refused too.
        (is-true
         (refused-p "(is-a ?p gui-person) (node-slot-value ?p name ?g)
                     (call ?g ?x)")))
      ;; ...and the home-scoping it must NOT undo.  REGEX-MATCH is on the
      ;; same exclusion list but is not compiler-macro-backed, so a
      ;; schema that owns the name keeps it: admitted, not refused.
      (with-foreign-schema-functor ("REGEX-MATCH" 2)
        (multiple-value-bind (json status)
            (run-prolog "(is-a ?p gui-person) (regex-match \"a\" \"b\")")
          (is (= 200 status)
              "a schema-homed REGEX-MATCH/2 was refused; the routing ~
test undid the home-scoping (got ~A)" status)
          (is (= 0 (jref json :row-count)))))
      ;; Without that registration the engine's own one is still out.
      (is-true (refused-p "(regex-match \"a\" \"b\")"))
      ;; ...and the fixture's real edge functor is untouched.
      (multiple-value-bind (json status)
          (run-prolog "(is-a ?p gui-person) (is-a ?c gui-city)
                       (gui-visited ?p ?c)")
        (is (= 200 status))
        (is (= 3 (jref json :row-count)))))))

;;; ---------------------------------------------------------------------
;;; An unbound result variable is an ANSWER, not a fault (GH #279).
;;; ---------------------------------------------------------------------

(test prolog-unbound-result-variable-is-null
  "An unbound variable in a result column renders as JSON null and
answers 200.  These are idiomatic Prolog -- var/1 and atom/1 exist to be
called on an unbound variable, and unifying two fresh variables
legitimately succeeds -- but the variable reached the encoder as a raw
VAR STRUCT (prologc.lisp:97, printed \"?1\"), which cl-json could not
encode: an 11-character request produced a 500 and an UNEXPECTED SERVER
FAULT log line, poisoning the very alarm the fault split exists to keep
trustworthy (GH #279)."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (dolist (text '("(= ?x ?y)" "(var ?x)" "(atom ?x)"))
        (multiple-value-bind (json status ctype raw) (run-prolog text)
          (declare (ignore ctype))
          (is (= 200 status) "~S answered ~A, not 200" text status)
          (is (= 1 (jref json :row-count)) "~S" text)
          ;; Every column is null -- and the ROW is still an object.
          ;; (cons key NIL) is not a dotted pair, so the guessing
          ;; encoder used to turn an all-null row into the ARRAY
          ;; [["x"],["y"]] and break the envelope's shape.
          (let ((row (first (jref json :rows))))
            (is-true (consp row) "~S: row is not an object: ~A" text raw)
            (dolist (cell row)
              (is-false (cdr cell)
                        "~S: column ~A is not null: ~A"
                        text (car cell) raw)))
          (is-true (search "null" raw) "~S: no null on the wire: ~A"
                   text raw)
          (body-has-no-internals raw text)))
      ;; Positive control: a BOUND variable in the SAME query still
      ;; carries its value, so the encoder is not blanket-nulling.
      (multiple-value-bind (json status raw)
          (run-prolog "(is-a ?p gui-person)
                       (node-slot-value ?p name ?n) (var ?u)")
        (declare (ignore raw))
        (is (= 200 status))
        (is (equal '("p" "n" "u") (jref json :columns)))
        (is (= 2 (jref json :row-count)))
        (is (equal '("Alice" "Bob") (rows-of json :n))
            "a bound variable lost its value")
        (dolist (row (jref json :rows))
          (is-true (jref row :p) "the node id went missing")
          (is-false (jref row :u) "the unbound variable is not null"))))))

;;; ---------------------------------------------------------------------
;;; Every CodeMirror entry point the editor calls must be VENDORED and
;;; LOADED (GH #279).
;;;
;;; The bug this exists for: CodeMirror.overlayMode is not part of CM5's
;;; lib/codemirror.js -- it is addon/mode/overlay.js.  It was never
;;; vendored, so building the vg-prolog mode threw during editor
;;; construction, ENABLE-PROLOG's exception was swallowed into a
;;; console.warn, and the Prolog sub-tab simply never appeared.  Asset
;;; serving, node --check and the pure PAREN-BALANCE function all passed
;;; throughout: nothing had ever CONSTRUCTED the pane.
;;;
;;; ⚠ What this checks and what it does not.  It is a static presence
;;; check: for each CodeMirror entry point prolog.js calls, is that name
;;; defined somewhere in the vendor files main.js actually loads?  That
;;; catches calling an API that lives in an unvendored addon, and
;;; vendoring a file without loading it -- the two ways this class of
;;; bug happens.  It does NOT execute anything, so it cannot catch a
;;; wrong argument or a misused return.  Executing the editor needs a
;;; browser, which v1 does not have.
;;; ---------------------------------------------------------------------

(defun %static-file (relative)
  "Text of a file under gui/static/."
  (with-open-file (in (merge-pathnames
                       relative
                       (asdf:system-relative-pathname :graph-db/gui
                                                      "gui/static/"))
                      :external-format :utf-8)
    (let ((s (make-string (file-length in))))
      (subseq s 0 (read-sequence s in)))))

(defun %matches (pattern text &key (group 0))
  "Every distinct GROUP capture of PATTERN in TEXT, in order."
  (let ((acc '()))
    (cl-ppcre:do-matches-as-strings (m pattern text)
      (multiple-value-bind (whole groups)
          (cl-ppcre:scan-to-strings pattern m)
        (declare (ignore whole))
        (pushnew (if (zerop group) m (aref groups (1- group))) acc
                 :test #'string=)))
    (nreverse acc)))

(defun %vendor-scripts-main-loads ()
  "The /vendor/*.js files main.js loads, as bare filenames."
  (mapcar (lambda (path) (subseq path (1+ (position #\/ path :from-end t))))
          (%matches "/vendor/[A-Za-z0-9._-]+\\.js"
                    (%static-file "js/main.js"))))

(defun %codemirror-entry-points ()
  "Every CodeMirror entry point prolog.js calls, as bare names -- read
out of the source, so this cannot drift from what the code does."
  (%matches "\\bCM\\.([A-Za-z_][A-Za-z0-9_]*)"
            (%static-file "js/prolog.js") :group 1))

(test codemirror-entry-points-are-vendored-and-loaded
  "Every CodeMirror API the editor calls is defined in a vendor file that
main.js loads.  This is the check that was missing when
CodeMirror.overlayMode -- an ADDON, not core -- was called but never
vendored, and the Prolog tab silently never appeared (GH #279)."
  (let* ((scripts (%vendor-scripts-main-loads))
         (entry-points (%codemirror-entry-points))
         (bundle (format nil "~{~A~^~%~}"
                         (mapcar (lambda (f)
                                   (%static-file
                                    (concatenate 'string "vendor/" f)))
                                 scripts))))
    (is-true (member "codemirror.js" scripts :test #'string=)
             "main.js does not load the CodeMirror core: ~S" scripts)
    ;; The addon this test was written for, named explicitly so a
    ;; regression reads as itself rather than as a generic miss.
    (is-true (member "codemirror-overlay.js" scripts :test #'string=)
             "main.js does not load the overlay addon; ~
CodeMirror.overlayMode will be undefined (~S)" scripts)
    (is-true entry-points "no CM.* calls found -- the scan is broken")
    (dolist (name entry-points)
      (is-true (search name bundle)
               "prolog.js calls CodeMirror.~A, but no vendor file ~
main.js loads defines it.  If it is a CodeMirror ADDON, vendor it ~
under gui/static/vendor/ and load it in enableProlog (GH #279). ~
Loaded: ~S" name scripts))))

(test codemirror-addon-assets-serve
  "The overlay addon serves like the rest of the vendored set."
  (with-gui-server ()
    (multiple-value-bind (json status ctype raw)
        (gui-request "/vendor/codemirror-overlay.js")
      (declare (ignore json))
      (is (= 200 status))
      (is (eql 0 (search "application/javascript" ctype)))
      (is-true (search "overlayMode" raw)
               "the vendored addon does not define overlayMode"))))

;;; ---------------------------------------------------------------------
;;; Value fidelity, name resolution, and the error contract
;;; (GH #282, #281, #286).
;;; ---------------------------------------------------------------------

(test query-null-and-true-slot-values-render-as-json
  "An empty slot is JSON null and T is JSON true -- not the strings
\"NIL\" and \"T\" (GH #282).  Bob has no home-city; a third person's
age is T."
  (with-gui-fixture ()
    (let ((*graph* (getf *fixture* :graph)))
      (with-transaction () (make-gui-person :name "Cy" :age t)))
    (with-gui-server ()
      (multiple-value-bind (json status ctype raw)
          (run-query
           "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
             \"where\":[{\"slot\":\"?p\",\"name\":\"home-city\",
                         \"bind\":\"?hc\"},
                        {\"slot\":\"?p\",\"name\":\"age\",
                         \"bind\":\"?a\"}],
             \"select\":[\"?hc\",\"?a\"]}")
        (declare (ignore ctype))
        (is (= 200 status))
        (is (= 3 (jref json :row-count)))
        (is-false (search "\"NIL\"" raw)
                  "an empty slot must not be the string NIL")
        (is-false (search "\"T\"" raw) "T must not be the string T")
        (let ((rows (jref json :rows)))
          (is (member nil (mapcar (lambda (r) (jref r :hc)) rows))
              "Bob's missing home-city is null")
          (is (member t (mapcar (lambda (r) (jref r :a)) rows))
              "Cy's age T is true"))))))

(test node-inspection-renders-a-stored-object-as-an-object
  "A slot holding a JSON object -- stored as an alist -- comes back from
the inspector as a JSON object, not as printed Lisp (GH #282)."
  (with-gui-fixture ()
    (let (node)
      (let ((*graph* (getf *fixture* :graph)))
        (with-transaction ()
          (setq node (make-gui-person :name "Di"
                                      :age '((:nested . 1)
                                             (:label . "x"))))))
      (with-gui-server ()
        (multiple-value-bind (json status ctype raw)
            (gui-request (node-path node))
          (declare (ignore ctype))
          (is (= 200 status))
          (let ((age (jref (jref json :slots) :age)))
            (is (= 1 (jref age :nested)))
            (is (string= "x" (jref age :label))))
          (is-false (search "(nested" raw) "no printed Lisp on the wire"))))))

(test query-resolves-digit-bearing-names
  "Type and slot names with digits resolve exactly as /types spells them
(GH #281): CAMEL-CASE-TO-LISP used to turn gui-zone3 into GUI-ZONE-3 and
the query endpoint refused the schema's own name."
  (with-gui-fixture ()
    (let ((*graph* (getf *fixture* :graph)))
      (with-transaction () (make-gui-zone3 :x1-y2 7)))
    (with-gui-server ()
      (multiple-value-bind (json status)
          (run-query
           "{\"match\":[{\"vertex\":\"?z\",\"type\":\"gui-zone3\"}],
             \"where\":[{\"slot\":\"?z\",\"name\":\"x1-y2\",
                         \"bind\":\"?v\"}],
             \"select\":[\"?v\"]}")
        (is (= 200 status) "digit-bearing names must resolve: ~A"
            (jref json :message))
        (is (equal '(7) (rows-of json :v)))))))

(test prolog-plain-simple-error-is-an-engine-fault
  "A bare (ERROR \"...\") in engine code is a defect, not the client's:
500 internal-error.  Before GH #286 every SIMPLE-ERROR was labelled
ill-typed, because the engine's own checked preconditions were
SIMPLE-ERRORs; they are QUERY-PRECONDITION-ERRORs now, so the label can
be exact in both directions."
  (with-gui-fixture ()
    (with-gui-server (:allow-prolog t)
      (let* ((key (find-symbol "VALID-DATE-P/1" :graph-db))
             (saved (gethash key graph-db::*prolog-global-functors*)))
        (assert saved)
        (unwind-protect
             (progn
               (setf (gethash key graph-db::*prolog-global-functors*)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (error "boom, an engine defect")))
               (multiple-value-bind (json status ctype raw)
                   (run-prolog
                    "(is-a ?p gui-person) (valid-date-p \"2026-08-28\")")
                 (declare (ignore ctype))
                 (is (= 500 status) "a SIMPLE-ERROR answered ~A" status)
                 (is (string= "internal-error" (jref json :error)))
                 (is-false (search "boom" raw))))
          (setf (gethash key graph-db::*prolog-global-functors*) saved)))
      ;; ...and a checked precondition is still the client's 400.
      (multiple-value-bind (json status)
          (run-prolog "(find-by-slot ?a ?b ?c ?d)")
        (is (= 400 status))
        (is (string= "ill-typed-query" (jref json :error)))))))

(test builder-query-bodies-do-not-intern-bogus-keys
  "GH #284: fifty builder requests each carrying a never-seen JSON field
leave the KEYWORD package exactly as large as before -- the same guard
the Prolog endpoint has had since GH #279, now on the DSL decoder both
surfaces share."
  (with-gui-fixture ()
    (with-gui-server ()
      (run-query "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gui-person\"}],
                   \"select\":[\"?p\"]}")
      (let ((kw (package-symbol-count :keyword)))
        (dotimes (i 50)
          (multiple-value-bind (json status)
              (run-query
               (format nil "{\"match\":[{\"vertex\":\"?p\",
                                      \"type\":\"gui-person\",
                                      \"zzHostile~D\":~D}],
                             \"select\":[\"?p\"],\"zzTop~D\":true}" i i i))
            (is (= 200 status) "request ~D answered ~A: ~A" i status
                (jref json :message))))
        (is (= kw (package-symbol-count :keyword))
            "KEYWORD gained ~D symbol(s)"
            (- (package-symbol-count :keyword) kw))))))
