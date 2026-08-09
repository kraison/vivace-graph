;;;; End-to-end tests for the REST layer (rest.lisp) over REAL HTTP.
;;;;
;;;; Unlike rest-tests.lisp -- which calls the request handlers in-process with a
;;;; hand-built params alist -- these start an actual server with START-REST and
;;;; drive it with DRAKMA, so they exercise the full path: routing, the
;;;; auth/graph wrappers, query- and form-parameter parsing, and (crucially) the
;;;; JSON request-body reader for ad-hoc pattern queries, none of which the
;;;; in-process tests touch.
;;;;
;;;; AUTH-REST-USER normally shells out to htpasswd/openssl; WITH-HTTP-REST stubs
;;;; it to a fixed user/password verdict so both the 200 and 401 paths run.
;;;;
;;;; Reuses the g-person / g-knows schema and the friendsOf def-query + the
;;;; make-a-knows-b-and-c helper from rest-tests.lisp.

(in-package #:graph-db/test)

(def-suite rest-http-suite
  :description "REST endpoints exercised end-to-end over real HTTP."
  :in graph-db-suite)

(in-suite rest-http-suite)

;; A FIXED port is a cross-run contaminator on a shared host exactly like a fixed
;; temp path, but fails worse: a second concurrent run either cannot bind, or its
;; requests reach the FIRST run's server and read that run's graph.  Ask the OS
;; for a free port per server instead.
(defparameter *http-test-port* 17893)

(defun free-tcp-port ()
  "An unused loopback port: bind port 0, read what the OS assigned, release it."
  (let ((s (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect (usocket:get-local-port s)
      (usocket:socket-close s))))

;; A trivial procedure to exercise the /procedure route over HTTP.
(def-rest-procedure :echo (msg)
  (json:encode-json-to-string (list (cons :echo msg))))

(defmacro with-http-rest ((&key (user "u") (pass "p")) &body body)
  "Start a real REST server on *HTTP-TEST-PORT* with AUTH-REST-USER stubbed to
accept USER/PASS, run BODY, then stop the server and restore auth."
  (let ((saved (gensym "AUTH")))
    `(let ((,saved (symbol-function 'graph-db::auth-rest-user))
           (*http-test-port* (free-tcp-port)))
       (unwind-protect
            (progn
              (setf (symbol-function 'graph-db::auth-rest-user)
                    (lambda (u p) (and (string= u ,user) (string= p ,pass))))
              (graph-db:start-rest *http-test-port*)
              (sleep 0.3)               ; let the listener bind the port
              ,@body)
         (ignore-errors (graph-db:stop-rest))
         (setf (symbol-function 'graph-db::auth-rest-user) ,saved)))))

(defun http-url (path)
  (format nil "http://127.0.0.1:~D~A" *http-test-port* path))

(defun graph-path (suffix)
  "A path under the test graph, e.g. (graph-path \"/query/friendsOf\")."
  (format nil "/graph/~A~A" (rest-graph-name) suffix))

(defun http-json (path &key (method :get) parameters content)
  "Make an HTTP request and return (values decoded-json status-code).  DRAKMA
returns 4xx/5xx without signaling, so the status is always available."
  (multiple-value-bind (body status)
      (drakma:http-request (http-url path)
                           :method method
                           :parameters parameters
                           :content content
                           :content-type (when content "application/json"))
    (let ((string (cond ((null body) "")
                        ((stringp body) body)
                        (t (flexi-streams:octets-to-string
                            body :external-format :utf-8)))))
      (values (when (plusp (length string))
                (ignore-errors (json:decode-json-from-string string)))
              status))))

(defun auth-params (&rest extra)
  (append '(("username" . "u") ("password" . "p")) extra))

(test http-get-graph-returns-schema
  "GET /graph/<g> returns the schema JSON over HTTP."
  (with-test-graph (g)
    (declare (ignore g))
    (with-http-rest ()
      (multiple-value-bind (j status) (http-json (graph-path "") :parameters (auth-params))
        (is (= 200 status))
        (is (string-equal "graph" (princ-to-string (cdr (assoc :type j)))))))))

(test http-post-then-get-vertex
  "POST a vertex and GET it back over HTTP."
  (with-test-graph (g)
    (declare (ignore g))
    (with-http-rest ()
      (multiple-value-bind (j status)
          (http-json (graph-path "/vertex/gPerson")
                     :method :post :parameters (auth-params '("name" . "Alice")))
        (is (= 200 status))
        (let ((id (cdr (assoc :id j))))
          (is-true id)
          (multiple-value-bind (j2 s2)
              (http-json (graph-path (format nil "/vertex/~A" id)) :parameters (auth-params))
            (is (= 200 s2))
            (is (string= "Alice" (cdr (assoc :name j2))))))))))

(test http-procedure-echo
  "POST /graph/<g>/procedure/echo runs a def-rest-procedure over HTTP."
  (with-test-graph (g)
    (declare (ignore g))
    (with-http-rest ()
      (multiple-value-bind (j status)
          (http-json (graph-path "/procedure/echo")
                     :method :post :parameters (auth-params '("msg" . "hi")))
        (is (= 200 status))
        (is (string= "hi" (cdr (assoc :echo j))))))))

(test http-def-query-friends
  "POST /graph/<g>/query/friendsOf runs a named def-query over HTTP."
  (with-test-graph (g)
    (declare (ignore g))
    (make-a-knows-b-and-c)
    (with-http-rest ()
      (multiple-value-bind (j status)
          (http-json (graph-path "/query/friendsOf")
                     :method :post :parameters (auth-params '("name" . "A")))
        (is (= 200 status))
        (is (equal '("B" "C")
                   (sort (mapcar (lambda (row) (cdr (assoc :friend-name row))) j)
                         #'string<)))))))

(test http-pattern-query-json-body
  "POST /graph/<g>/query with a JSON body runs an ad-hoc pattern query --
exercises the request-body reader the in-process tests bypass."
  (with-test-graph (g)
    (declare (ignore g))
    (make-a-knows-b-and-c)
    (with-http-rest ()
      ;; auth in the query string, since the body carries the query DSL
      (multiple-value-bind (j status)
          (http-json (graph-path "/query?username=u&password=p")
                     :method :post
                     :content "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"},
                                          {\"edge\":\"gKnows\",\"from\":\"?p\",\"to\":\"?f\"}],
                                \"where\":[{\"slot\":\"?p\",\"name\":\"name\",\"value\":\"A\"},
                                           {\"slot\":\"?f\",\"name\":\"name\",\"bind\":\"?fn\"}],
                                \"select\":[\"?fn\"]}")
        (is (= 200 status))
        (is (equal '("B" "C")
                   (sort (mapcar (lambda (row) (cdr (assoc :fn row))) j) #'string<)))))))

(test http-pattern-query-ndjson
  "A pattern query with format=ndjson streams newline-delimited JSON over HTTP."
  (with-test-graph (g)
    (declare (ignore g))
    (make-a-knows-b-and-c)              ; A, B, C
    (with-http-rest ()
      (multiple-value-bind (body status)
          (drakma:http-request
           (http-url (graph-path "/query?username=u&password=p"))
           :method :post :content-type "application/json"
           :content "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                      \"where\":[{\"slot\":\"?p\",\"name\":\"name\",\"bind\":\"?n\"}],
                      \"select\":[\"?n\"],\"format\":\"ndjson\"}")
        (is (= 200 status))
        (let* ((string (if (stringp body) body
                           (flexi-streams:octets-to-string body :external-format :utf-8)))
               (lines (remove "" (uiop:split-string string :separator '(#\Newline))
                              :test #'string=)))
          (is (= 3 (length lines)) "one JSON object per result row")
          (is (every (lambda (l) (assoc :n (json:decode-json-from-string l))) lines)))))))

(test http-bad-credentials-401
  "Wrong credentials yield 401 over HTTP."
  (with-test-graph (g)
    (declare (ignore g))
    (with-http-rest ()
      (multiple-value-bind (j status)
          (http-json (graph-path "")
                     :parameters '(("username" . "u") ("password" . "wrong")))
        (declare (ignore j))
        (is (= 401 status))))))

(test http-unknown-graph-404
  "An unknown graph name yields 404 over HTTP."
  (with-http-rest ()
    (multiple-value-bind (j status)
        (http-json "/graph/noSuchGraph" :parameters (auth-params))
      (declare (ignore j))
      (is (= 404 status)))))
