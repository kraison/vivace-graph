;;;; Tests for the REST layer (rest.lisp): the json-encode serialization
;;;; contract and the request handlers (rest-get/post/put/delete-vertex|edge,
;;;; rest-list-edges, rest-get-graph) exercised IN-PROCESS.
;;;;
;;;; We do not stand up a real HTTP server or the htpasswd/openssl-backed
;;;; auth-rest-user (which shells out to external binaries).  Instead each
;;;; handler is called directly with a params alist shaped exactly as ningle
;;;; delivers it -- route captures (:graph-name, :node-id, :type) as KEYWORD
;;;; keys, query/body params ("username", "name", "from", ...) as STRING keys.
;;;; WITH-REST-ENV stubs auth-rest-user to a fixed verdict and binds a fresh
;;;; ningle:*response* so the 401/404 branches can set a status.
;;;;
;;;; Reuses the g-person / g-knows schema from graph-tests.lisp and the
;;;; WITH-TEST-GRAPH fixture (which registers the graph under
;;;; *integration-graph-name*, so with-rest-graph's lookup-graph resolves it).

(in-package #:graph-db/test)

(def-suite rest-suite
  :description "REST layer: json-encode contract + in-process request handlers."
  :in graph-db-suite)

(in-suite rest-suite)

(defun rest-graph-name ()
  "The graphName route param that with-rest-graph maps back to the test graph."
  (json:lisp-to-camel-case (symbol-name *integration-graph-name*)))

(defun rest-decode (json)
  "Decode a handler's JSON string into a cl-json alist (object keys -> keywords)."
  (json:decode-json-from-string json))

(defmacro with-rest-env ((&key (auth t)) &body body)
  "Run BODY with graph-db::auth-rest-user stubbed to always return AUTH and a
fresh ningle:*response* bound (status 200).  Restores auth-rest-user after."
  (let ((saved (gensym "AUTH")))
    `(let ((,saved (symbol-function 'graph-db::auth-rest-user)))
       (unwind-protect
            (progn
              (setf (symbol-function 'graph-db::auth-rest-user)
                    (lambda (u p) (declare (ignore u p)) ,auth))
              (let ((ningle:*response* (lack/response:make-response 200)))
                ,@body))
         (setf (symbol-function 'graph-db::auth-rest-user) ,saved)))))

(defun rest-params (&rest extra)
  "An auth+graph params alist (the shape ningle hands a handler), with EXTRA
conses appended.  Route-capture keys must be keywords; the rest strings."
  (append (list (cons "username" "u")
                (cons "password" "p")
                (cons :graph-name (rest-graph-name)))
          extra))

(defun rest-status ()
  (lack/response:response-status ningle:*response*))

;;; ---------------------------------------------------------------------------
;;; json-encode -- the serialization contract (no auth / server needed)
;;; ---------------------------------------------------------------------------

(test json-encode-vertex-has-id-type-and-slots
  "json-encode of a vertex emits its string id, camelCased type, and data slots."
  (with-test-graph (g)
    (let (v)
      (with-transaction () (setq v (make-g-person :name "Alice" :age 30)))
      (let ((j (rest-decode (graph-db::json-encode v))))
        (is (string= (string-id v) (cdr (assoc :id j))))
        (is (string= "gPerson" (cdr (assoc :type j))))
        (is (string= "Alice" (cdr (assoc :name j))))
        (is (= 30 (cdr (assoc :age j))))))))

(test json-encode-edge-has-endpoints-and-slots
  "json-encode of an edge emits id, type, from, to and its data slots."
  (with-test-graph (g)
    (let (a b e)
      (with-transaction ()
        (setq a (make-g-person :name "A") b (make-g-person :name "B"))
        (make-g-knows :from a :to b :since "2020"))
      (setq e (first (outgoing-edges a)))
      (let ((j (rest-decode (graph-db::json-encode e))))
        (is (string= (string-id e) (cdr (assoc :id j))))
        (is (string= "gKnows" (cdr (assoc :type j))))
        (is (string= (string-id a) (cdr (assoc :from j))))
        (is (string= (string-id b) (cdr (assoc :to j))))
        (is (string= "2020" (cdr (assoc :since j))))))))

(test json-encode-edge-list-is-an-array
  "json-encode-edge-list emits a JSON array with one object per edge."
  (with-test-graph (g)
    (let (a b c)
      (with-transaction ()
        (setq a (make-g-person :name "A") b (make-g-person :name "B")
              c (make-g-person :name "C"))
        (make-g-knows :from a :to b)
        (make-g-knows :from a :to c))
      (let ((arr (rest-decode (graph-db::json-encode-edge-list (outgoing-edges a)))))
        (is (listp arr))
        (is (= 2 (length arr)))
        (is (every (lambda (o) (string= "gKnows" (cdr (assoc :type o)))) arr))))))

(test json-encode-graph-describes-schema
  "json-encode of a graph emits its name, read/write mode and the vertex/edge
type schema."
  (with-test-graph (g)
    (let ((j (rest-decode (graph-db::json-encode g))))
      (is (string= (rest-graph-name) (cdr (assoc :name j))))
      (is (string= "graph" (cdr (assoc :type j))))
      (is (string= "readWrite" (cdr (assoc :mode j))))
      (let* ((vtypes (cdr (assoc :vertex-types j)))
             (names (mapcar (lambda (vt) (cdr (assoc :name vt))) vtypes)))
        (is-true (member "gPerson" names :test #'string=)
                 "graph schema should list the gPerson vertex type")
        ;; gPerson lists its declared slots
        (let* ((gp (find "gPerson" vtypes
                         :key (lambda (vt) (cdr (assoc :name vt))) :test #'string=))
               (slot-names (mapcar (lambda (s) (cdr (assoc :name s)))
                                   (cdr (assoc :slots gp)))))
          (is-true (member "name" slot-names :test #'string=))
          (is-true (member "age" slot-names :test #'string=))))
      (let ((etypes (mapcar (lambda (et) (cdr (assoc :name et)))
                            (cdr (assoc :edge-types j)))))
        (is-true (member "gKnows" etypes :test #'string=)
                 "graph schema should list the gKnows edge type")))))

;;; ---------------------------------------------------------------------------
;;; Request handlers -- CRUD round trips (auth stubbed, in-process)
;;; ---------------------------------------------------------------------------

(test rest-get-graph-returns-schema-json
  (with-test-graph (g)
    (with-rest-env ()
      (let ((j (rest-decode (graph-db::rest-get-graph (rest-params)))))
        (is (string= (rest-graph-name) (cdr (assoc :name j))))
        (is (string= "readWrite" (cdr (assoc :mode j))))))))

(test rest-post-vertex-creates-and-persists
  "POST vertex creates the vertex in the store and echoes it as JSON."
  (with-test-graph (g)
    (with-rest-env ()
      (let* ((out (graph-db::rest-post-vertex
                   (rest-params (cons :type "gPerson")
                                (cons "name" "Zoe") (cons "age" 99))))
             (j (rest-decode out))
             (id (cdr (assoc :id j))))
        (is (string= "gPerson" (cdr (assoc :type j))))
        (is (string= "Zoe" (cdr (assoc :name j))))
        (is (= 99 (cdr (assoc :age j))))
        ;; the vertex is actually in the store
        (is (= 1 (length (map-vertices #'identity g :collect-p t
                                                 :vertex-type 'g-person))))
        (let ((v (lookup-vertex id)))
          (is-true v "posted vertex should be retrievable by its returned id")
          (is (string= "Zoe" (slot-value v 'name))))))))

(test rest-get-vertex-returns-the-vertex
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (string-id (make-g-person :name "Gettable" :age 7))))
      (with-rest-env ()
        (let ((j (rest-decode
                  (graph-db::rest-get-vertex (rest-params (cons :node-id id))))))
          (is (string= id (cdr (assoc :id j))))
          (is (string= "Gettable" (cdr (assoc :name j))))
          (is (= 7 (cdr (assoc :age j)))))))))

(test rest-put-vertex-updates-slots
  "PUT vertex updates the named slots and persists the change."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (string-id (make-g-person :name "Before" :age 1))))
      (with-rest-env ()
        (let ((j (rest-decode
                  (graph-db::rest-put-vertex
                   (rest-params (cons :node-id id) (cons "name" "After"))))))
          (is (string= "After" (cdr (assoc :name j))))))
      ;; persisted
      (is (string= "After" (slot-value (lookup-vertex id) 'name))))))

(test rest-delete-vertex-soft-deletes
  "DELETE vertex marks it deleted and removes it from live type scans.  (NB:
lookup-vertex still resolves a soft-deleted node, so a GET by id returns it
flagged rather than 404 -- this asserts the documented current behavior.)"
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (string-id (make-g-person :name "Doomed"))))
      (with-rest-env ()
        (graph-db::rest-delete-vertex (rest-params (cons :node-id id)))
        (let ((v (lookup-vertex id)))
          (is-true (or (null v) (deleted-p v))
                   "deleted vertex must be nil or deleted-p")))
      ;; gone from a live type scan
      (is (null (map-vertices #'identity g :collect-p t :vertex-type 'g-person))
          "soft-deleted vertex should not appear in a live scan"))))

(test rest-post-get-and-list-edges
  "POST edge between two vertices, GET it, and list a vertex's edges."
  (with-test-graph (g)
    (let (aid bid)
      (with-transaction ()
        (setq aid (string-id (make-g-person :name "From"))
              bid (string-id (make-g-person :name "To"))))
      (with-rest-env ()
        (let* ((out (graph-db::rest-post-edge
                     (rest-params (cons :type "gKnows")
                                  (cons "from" aid) (cons "to" bid))))
               (j (rest-decode out))
               (eid (cdr (assoc :id j))))
          (is (string= "gKnows" (cdr (assoc :type j))))
          (is (string= aid (cdr (assoc :from j))))
          (is (string= bid (cdr (assoc :to j))))
          ;; GET the edge back
          (let ((gj (rest-decode
                     (graph-db::rest-get-edge (rest-params (cons :node-id eid))))))
            (is (string= eid (cdr (assoc :id gj)))))
          ;; list edges for the source vertex
          (let ((arr (rest-decode
                      (graph-db::rest-list-edges (rest-params (cons :node-id aid))))))
            (is (= 1 (length arr)))
            (is (string= eid (cdr (assoc :id (first arr)))))))))))

;;; ---------------------------------------------------------------------------
;;; Error paths -- auth, unknown graph / vertex / type
;;; ---------------------------------------------------------------------------

(test rest-auth-failure-returns-401
  "A failed credential check yields HTTP 401 and an error body."
  (with-test-graph (g)
    (with-rest-env (:auth nil)
      (let ((j (rest-decode (graph-db::rest-get-graph (rest-params)))))
        (is (= 401 (rest-status)))
        (is-true (assoc :error j) "401 body should carry an :error")))))

(test rest-unknown-graph-returns-404
  (with-test-graph (g)
    (with-rest-env ()
      (let ((j (rest-decode
                (graph-db::rest-get-graph
                 (list (cons "username" "u") (cons "password" "p")
                       (cons :graph-name "noSuchGraph"))))))
        (is (= 404 (rest-status)))
        (is-true (assoc :error j))))))

(test rest-unknown-vertex-returns-404
  (with-test-graph (g)
    (with-rest-env ()
      (graph-db::rest-get-vertex
       (rest-params (cons :node-id "00000000000000000000000000000000")))
      (is (= 404 (rest-status))))))

(test rest-post-vertex-unknown-type-errors
  "POSTing an unknown vertex type returns an error body (not a crash)."
  (with-test-graph (g)
    (with-rest-env ()
      (let ((j (rest-decode
                (graph-db::rest-post-vertex
                 (rest-params (cons :type "noSuchType") (cons "name" "x"))))))
        (is-true (assoc :error j)
                 "unknown vertex type should yield an :error body")))))

;;; ---------------------------------------------------------------------------
;;; def-query (#44): named, parameterized, read-only queries over /query/.
;;; ---------------------------------------------------------------------------

;; A friends-by-name query: read-only and snapshot-isolated by default.
(def-query friends-of
  :params ((?name :string))
  :return (?friend-name)
  :where ((is-a ?person g-person)
          (node-slot-value ?person name ?name)
          (g-knows ?person ?friend)
          (node-slot-value ?friend name ?friend-name)))

;; Same query, capped at a single result, to exercise an author-set bound.
(def-query friends-of-capped
  :params ((?name :string))
  :return (?friend-name)
  :limit 1
  :where ((is-a ?person g-person)
          (node-slot-value ?person name ?name)
          (g-knows ?person ?friend)
          (node-slot-value ?friend name ?friend-name)))

;; A query that attempts a write -- must be refused under the read-only default.
(def-query try-retract
  :params ((?name :string))
  :return (?name)
  :where ((is-a ?p g-person)
          (node-slot-value ?p name ?name)
          (retract ?p)))

;; The same write, explicitly write-enabled: runs in a transaction and commits.
(def-query delete-person
  :params ((?name :string))
  :effects (:write)
  :return (?name)
  :where ((is-a ?p g-person)
          (node-slot-value ?p name ?name)
          (retract ?p)))

(defun make-a-knows-b-and-c ()
  "A knows B and C; returns nothing."
  (with-transaction ()
    (let ((a (make-g-person :name "A")))
      (make-g-knows :from a :to (make-g-person :name "B"))
      (make-g-knows :from a :to (make-g-person :name "C")))))

(test def-query-returns-json-objects
  "A def-query endpoint returns a JSON array of objects keyed by the camelCase
result-variable names."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (make-a-knows-b-and-c)
      (let* ((j (rest-decode
                 (graph-db::call-rest-query "friendsOf"
                                            (rest-params (cons "name" "A")))))
             (names (sort (mapcar (lambda (row) (cdr (assoc :friend-name row))) j)
                          #'string<)))
        (is (= 2 (length j)))
        (is (equal '("B" "C") names))))))

(test def-query-honors-author-set-limit
  "An author-set :limit caps the number of results."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (make-a-knows-b-and-c)
      (is (= 1 (length (rest-decode
                        (graph-db::call-rest-query
                         "friendsOfCapped" (rest-params (cons "name" "A"))))))))))

(test def-query-missing-parameter-is-400
  "A missing required parameter yields a 400 with an :error body."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (let ((j (rest-decode
                (graph-db::call-rest-query "friendsOf" (rest-params)))))
        (is (= 400 (rest-status)))
        (is-true (assoc :error j))))))

(test def-query-write-attempt-is-403
  "The read-only default refuses a query that tries to mutate the graph (403),
and nothing is deleted."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction () (make-g-person :name "A") (make-g-person :name "B"))
      (let ((j (rest-decode
                (graph-db::call-rest-query "tryRetract"
                                           (rest-params (cons "name" "A"))))))
        (is (= 403 (rest-status)))
        (is-true (assoc :error j)))
      (is (= 2 (length (select-flat (?p) (is-a ?p g-person))))
          "the write was refused, so both persons survive"))))

(test def-query-write-enabled-commits
  "A query whose :effects permit writes runs in a transaction and its mutation
persists (the retract flattens into the wrapping with-transaction)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction () (make-g-person :name "A") (make-g-person :name "B"))
      (graph-db::call-rest-query "deletePerson" (rest-params (cons "name" "A")))
      (is (equal '("B")
                 (sort (select-flat (?n) (is-a ?p g-person) (node-slot-value ?p name ?n))
                       #'string<))
          "the write-enabled query committed its retract"))))

;;; ---------------------------------------------------------------------------
;;; Ad-hoc JSON pattern queries (#44, tier 2): POST /graph/:g/query with a
;;; {match, where, select, limit} body, compiled to a bounded read-only select.
;;; ---------------------------------------------------------------------------

(defun pattern-query (json-string)
  "Run an ad-hoc pattern query given its JSON body (decoded as the route would),
returning the decoded JSON response."
  (rest-decode
   (graph-db::call-rest-pattern-query (json:decode-json-from-string json-string)
                                      (rest-params))))

(test pattern-query-vertex-and-slot
  "A vertex pattern + slot bind returns each match as an object keyed by the
result var."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction ()
        (make-g-person :name "A") (make-g-person :name "B") (make-g-person :name "C"))
      (let* ((j (pattern-query
                 "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                   \"where\":[{\"slot\":\"?p\",\"name\":\"name\",\"bind\":\"?n\"}],
                   \"select\":[\"?n\"]}"))
             (names (sort (mapcar (lambda (row) (cdr (assoc :n row))) j) #'string<)))
        (is (= 3 (length j)))
        (is (equal '("A" "B" "C") names))))))

(test pattern-query-edge-join-and-value-filter
  "An edge pattern joins vertices; a slot value filters the source."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (make-a-knows-b-and-c)            ; A knows B, A knows C
      (let* ((j (pattern-query
                 "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"},
                              {\"edge\":\"gKnows\",\"from\":\"?p\",\"to\":\"?f\"}],
                   \"where\":[{\"slot\":\"?p\",\"name\":\"name\",\"value\":\"A\"},
                              {\"slot\":\"?f\",\"name\":\"name\",\"bind\":\"?fn\"}],
                   \"select\":[\"?fn\"]}"))
             (names (sort (mapcar (lambda (row) (cdr (assoc :fn row))) j) #'string<)))
        (is (equal '("B" "C") names))))))

(test pattern-query-compare-constraint
  "A compare constraint filters by a numeric slot."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction ()
        (make-g-person :name "young" :age 20)
        (make-g-person :name "old" :age 40))
      (let ((j (pattern-query
                "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                  \"where\":[{\"slot\":\"?p\",\"name\":\"age\",\"bind\":\"?age\"},
                             {\"compare\":\">\",\"args\":[\"?age\",30]},
                             {\"slot\":\"?p\",\"name\":\"name\",\"bind\":\"?n\"}],
                  \"select\":[\"?n\"]}")))
        (is (= 1 (length j)))
        (is (string= "old" (cdr (assoc :n (first j)))))))))

(test pattern-query-limit-caps-results
  "A client-supplied :limit bounds the result count."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction ()
        (dotimes (i 5) (make-g-person :name (format nil "n~d" i))))
      (is (= 2 (length (pattern-query
                        "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                          \"where\":[{\"slot\":\"?p\",\"name\":\"name\",\"bind\":\"?n\"}],
                          \"select\":[\"?n\"],\"limit\":2}")))))))

(defun ndjson-lines (body)
  "Split an NDJSON response BODY into decoded objects (one per non-blank line)."
  (mapcar #'json:decode-json-from-string
          (remove "" (uiop:split-string body :separator '(#\Newline)) :test #'string=)))

(test def-query-ndjson-streams-one-object-per-line
  "format=ndjson streams each result row as its own JSON line with the
application/x-ndjson content type."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (make-a-knows-b-and-c)
      (let* ((body (graph-db::call-rest-query
                    "friendsOf"
                    (rest-params (cons "name" "A") (cons "format" "ndjson"))))
             (objs (ndjson-lines body)))
        (is (string= "application/x-ndjson"
                     (getf (lack/response:response-headers ningle:*response*) :content-type)))
        (is (= 2 (length objs)))
        (is (equal '("B" "C")
                   (sort (mapcar (lambda (o) (cdr (assoc :friend-name o))) objs)
                         #'string<)))))))

(test pattern-query-ndjson-format
  "An ad-hoc pattern query with \"format\":\"ndjson\" streams NDJSON rows."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (make-a-knows-b-and-c)              ; A, B, C
      (let* ((body (graph-db::call-rest-pattern-query
                    (json:decode-json-from-string
                     "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                       \"where\":[{\"slot\":\"?p\",\"name\":\"name\",\"bind\":\"?n\"}],
                       \"select\":[\"?n\"],\"format\":\"ndjson\"}")
                    (rest-params)))
             (objs (ndjson-lines body)))
        (is (= 3 (length objs)))
        (is (equal '("A" "B" "C")
                   (sort (mapcar (lambda (o) (cdr (assoc :n o))) objs) #'string<)))))))

(test pattern-query-unknown-type-is-400
  "Referencing an unknown vertex/edge type is a 400."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (let ((j (pattern-query
                "{\"match\":[{\"vertex\":\"?p\",\"type\":\"noSuchType\"}],
                  \"select\":[\"?p\"]}")))
        (is (= 400 (rest-status)))
        (is-true (assoc :error j))))))

(test pattern-query-malformed-pattern-is-400
  "A pattern object that isn't a recognized kind is a 400."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (let ((j (pattern-query
                "{\"match\":[{\"bogus\":\"?p\"}],\"select\":[\"?p\"]}")))
        (is (= 400 (rest-status)))
        (is-true (assoc :error j))))))

(test def-query-unknown-name-is-404
  "An unknown query name yields a 404."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (let ((j (rest-decode
                (graph-db::call-rest-query "noSuchQuery" (rest-params)))))
        (is (= 404 (rest-status)))
        (is-true (assoc :error j))))))

(test dsl-ambiguous-type-name-is-a-query-param-error
  "An ambiguous bare type name from a REST client must come back as the
DSL's own client-error surface, not a raw internal condition.  Nearest
wrong implementation: let AMBIGUOUS-NODE-TYPE-NAME propagate raw (the
test would then see the wrong condition class)."
  (with-alias-test-graph (g :alias-two-store)
    (signals graph-db::query-param-error
      (graph-db::%dsl-resolve-type "aliasSpecies" :vertex g))))

(test rest-post-type-resolution-reports-ambiguity
  "The POST vertex/edge path returns (values NIL message) for an ambiguous
name and (values meta NIL) for a unique one."
  (with-alias-test-graph (g :alias-two-store)
    (let ((graph-db::*graph* g))
      (multiple-value-bind (meta msg)
          (graph-db::%rest-resolve-post-type "aliasSpecies" :vertex)
        (is (null meta))
        (is (search "mbiguous" msg)))))
  (with-alias-test-graph (g :alias-solo-store)
    (let ((graph-db::*graph* g))
      (multiple-value-bind (meta msg)
          (graph-db::%rest-resolve-post-type "aliasUnique" :vertex)
        (is (graph-db::node-type-p meta))
        (is (null msg))))))

;;; ---------------------------------------------------------------------------
;;; POST places the node in the URL's graph, not the class's declared
;;; default store (GH #167).
;;; ---------------------------------------------------------------------------

;; Declared default is store A; a POST to store B must still land there.
(def-vertex rest-dual-item () ((label :type string)) :rest-dual-store-a)

(defmacro with-rest-dual-stores ((ga gb) &body body)
  "Two open stores, :REST-DUAL-STORE-A (REST-DUAL-ITEM's declared
default) and :REST-DUAL-STORE-B (foreign to it), under a fresh system
directory."
  (let ((sys (gensym)) (da (gensym)) (db (gensym)))
    `(with-temp-directory (,sys)
       (with-temp-directory (,da)
         (with-temp-directory (,db)
           (let ((graph-db::*system-directory* (namestring ,sys)))
             (let ((,ga (make-graph :rest-dual-store-a (namestring ,da)
                                    :buffer-pool-size 1000))
                   (,gb nil))
               (unwind-protect
                    (progn
                      (setq ,gb (make-graph :rest-dual-store-b
                                            (namestring ,db)
                                            :buffer-pool-size 1000))
                      ,@body)
                 (ignore-errors (close-graph ,ga :snapshot-p nil))
                 (when ,gb
                   (ignore-errors (close-graph ,gb :snapshot-p nil)))
                 (collect-garbage)))))))))

(test rest-post-vertex-honors-the-url-graph-over-the-class-default
  "POST to store B for a class whose declared default is store A: the
node must be created IN B (the URL's graph), not silently redirected
to A.  %REST-RESOLVE-POST-TYPE only sees types already known to
*GRAPH*'s own schema, so B first adopts REST-DUAL-ITEM the ordinary
way (one direct write with an explicit :GRAPH); the REST POST that
follows is the thing under test (GH #167)."
  (with-rest-dual-stores (ga gb)
    (with-transaction ((graph-db::transaction-manager gb))
      (make-rest-dual-item :label "seed" :graph gb))
    (with-rest-env ()
      (let* ((out (graph-db::rest-post-vertex
                   (list (cons "username" "u") (cons "password" "p")
                         (cons :graph-name
                               (json:lisp-to-camel-case
                                (symbol-name :rest-dual-store-b)))
                         (cons :type "restDualItem")
                         (cons "label" "in-b"))))
             (j (rest-decode out))
             (id (cdr (assoc :id j))))
        (is (string= "in-b" (cdr (assoc :label j))))
        (is-true (lookup-vertex id :graph gb))
        (is (null (lookup-vertex id :graph ga)))))))

;;; ---------------------------------------------------------------------
;;; An unbound result variable is an ANSWER, not a fault (GH #279).
;;;
;;; Reachable from the structured DSL too: a "select" variable that
;;; appears nowhere in "match" or "where" is never bound, so it reaches
;;; the encoder as a raw VAR STRUCT (prologc.lisp:97).  The GUI's
;;; free-text surface hits the same defect with (= ?x ?y); the fix is in
;;; the shared encoder, so both are covered by one change.
;;; ---------------------------------------------------------------------

(test pattern-query-unbound-select-var-is-null
  "A selected variable that nothing binds comes back JSON null, and a
bound variable in the SAME query keeps its value."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction () (make-g-person :name "A"))
      (let* ((j (pattern-query
                 "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                   \"where\":[{\"slot\":\"?p\",\"name\":\"name\",
                               \"bind\":\"?n\"}],
                   \"select\":[\"?n\",\"?unbound\"]}"))
             (row (first j)))
        (is (= 1 (length j)))
        (is (equal "A" (cdr (assoc :n row))) "the bound var lost its value")
        (is-true (assoc :unbound row) "the unbound column is missing")
        (is-false (cdr (assoc :unbound row))
                  "the unbound var did not render as null")))))

(test pattern-query-all-null-row-is-still-an-object
  "A row whose values are ALL null stays a JSON OBJECT.  (cons key NIL)
is not a dotted pair, so cl-json's guessing encoder used to render such
a row as the array [[\"unbound\"]] and silently change the response's
shape -- hence ENCODE-JSON-ALIST in QUERY-RESULTS->JSON (GH #279)."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction () (make-g-person :name "A"))
      (let ((body (graph-db::call-rest-pattern-query
                   (json:decode-json-from-string
                    "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                      \"select\":[\"?unbound\"]}")
                   (rest-params))))
        ;; Assert on the RAW body: the decoded form cannot tell an
        ;; object from an array of one-element lists.
        (is-true (search "{\"unbound\":null}" body)
                 "an all-null row is not an object: ~A" body)
        (is-false (search "[[" body)
                  "an all-null row came out as an array: ~A" body)))))

;;; ---------------------------------------------------------------------------
;;; Value fidelity, name resolution, and the error contract (GH #282, #281,
;;; #286) on the REST surface.
;;; ---------------------------------------------------------------------------

(test dsl-keyword-accepts-both-spellings
  "GH #281: the engine's kebab spelling goes in verbatim -- digits and all
-- and legacy camelCase still folds."
  (is (eq :foo-bar2 (graph-db::%dsl-keyword "foo-bar2")))
  (is (eq :node-3d-point (graph-db::%dsl-keyword "node-3d-point")))
  (is (eq :x1-y2 (graph-db::%dsl-keyword "x1-y2")))
  (is (eq :gui-person (graph-db::%dsl-keyword "gui-person")))
  (is (eq :min-age (graph-db::%dsl-keyword "minAge"))))

(test pattern-query-nil-slot-is-null-and-t-is-true
  "GH #282: an empty slot is JSON null and T is JSON true on the pattern-
query surface -- not the strings \"NIL\" / \"T\"."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (with-transaction ()
        (make-g-person :name "A")
        (make-g-person :name "B" :age t))
      (let* ((raw (graph-db::call-rest-pattern-query
                   (json:decode-json-from-string
                    "{\"match\":[{\"vertex\":\"?p\",\"type\":\"gPerson\"}],
                      \"where\":[{\"slot\":\"?p\",\"name\":\"age\",
                                  \"bind\":\"?a\"}],
                      \"select\":[\"?a\"]}")
                   (rest-params)))
             (j (rest-decode raw))
             (ages (mapcar (lambda (row) (cdr (assoc :a row))) j)))
        (is (= 2 (length j)))
        (is-false (search "\"NIL\"" raw))
        (is-false (search "\"T\"" raw))
        (is (member nil ages) "A's missing age is null")
        (is (member t ages) "B's age T is true")))))

;; A query over an UNINDEXED slot through the index functor: the engine's
;; checked precondition, a typed condition since GH #286.
(def-query people-by-age-index
  :params ((?age :integer))
  :return (?n)
  :where ((find-by-slot ?p g-person age ?age)
          (node-slot-value ?p name ?n)))

(test def-query-checked-precondition-is-400-with-the-reason
  "GH #286: an engine precondition the caller failed -- no index on
G-PERSON.AGE -- is the client's 400 with the reason, not a 500."
  (with-test-graph (g)
    (declare (ignore g))
    (with-rest-env ()
      (let ((j (rest-decode
                (graph-db::call-rest-query "peopleByAgeIndex"
                                           (rest-params (cons "age" "3"))))))
        (is (= 400 (rest-status)))
        (is-true (search "No secondary index" (cdr (assoc :error j)))
                 "the reason names the precondition: ~S"
                 (cdr (assoc :error j)))))))
