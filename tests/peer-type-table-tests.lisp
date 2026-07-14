;;;; The peer type table: (kind, type-id, name, direct-super) shipped to devices.
;;;;
;;;; A type-id is meaningless on its own.  Ids are handed out by DEF-VERTEX/DEF-EDGE
;;;; evaluation order (GET-NEXT-TYPE-ID), persisted per graph, and NO type NAME ever
;;;; crosses the wire -- a receiver resolves the raw uint16 against its OWN schema.  A
;;;; Lisp device gets away with that only because it evaluates the same schema.lisp; a
;;;; non-Lisp peer (the Kotlin/SQLite device) cannot.  So the hub ships the mapping.
;;;;
;;;; The load-bearing detail these tests pin down: NEXT-VERTEX-ID and NEXT-EDGE-ID are
;;;; SEPARATE counters that BOTH start at 1, so a vertex type and an edge type routinely
;;;; share a numeric id.  Any consumer keying on the id ALONE silently confuses them.

(in-package #:graph-db/test)

(def-suite peer-type-table-suite
  :description "The (kind,id,name,super) type table shipped in the peer auth-ok plist."
  :in graph-db-suite)

(in-suite peer-type-table-suite)

;;; No new schema: the g-* schema from graph-tests (loaded earlier in the system) is
;;; exactly the shape this needs -- two vertex types where G-EMPLOYEE subclasses
;;; G-PERSON, and two edge types.  Both id spaces therefore start at 1, which is what
;;; makes the collision assertion below meaningful.  WITH-TEST-GRAPH (suite.lisp:113)
;;; builds a graph of *INTEGRATION-GRAPH-NAME* carrying it.

(test type-table-round-trips
  "PEER-PARSE-TYPE-TABLE inverts PEER-TYPE-TABLE-STRING."
  (with-test-graph (g)
    (let* ((s (graph-db::peer-type-table-string g))
           (parsed (graph-db::peer-parse-type-table s)))
      (is (stringp s))
      (is (plusp (length parsed)))
      (dolist (row parsed)
        (is (member (first row) '(:vertex :edge)))
        (is (integerp (second row)))
        (is (stringp (third row)))
        (is (or (null (fourth row)) (stringp (fourth row))))))))

(test type-table-carries-kind-because-ids-collide
  "REGRESSION GUARD.  NEXT-VERTEX-ID and NEXT-EDGE-ID both start at 1 (schema.lisp:15-16),
so a vertex type and an edge type share a numeric id.  The table must therefore be keyed
on (KIND . ID).  If this ever fails because the ids no longer collide, the KIND field is
STILL required -- do not 'simplify' it away."
  (with-test-graph (g)
    (let* ((parsed (graph-db::peer-parse-type-table (graph-db::peer-type-table-string g)))
           (vertex-ids (loop for row in parsed when (eq (first row) :vertex) collect (second row)))
           (edge-ids (loop for row in parsed when (eq (first row) :edge) collect (second row))))
      (is (plusp (length vertex-ids)))
      (is (plusp (length edge-ids)))
      (is (intersection vertex-ids edge-ids))
      (let ((keys (loop for row in parsed collect (cons (first row) (second row)))))
        (is (= (length keys) (length (remove-duplicates keys :test #'equal))))))))

(test type-table-reports-direct-superclass-only
  "A subclass reports its DIRECT parent; a root type reports NIL (not VERTEX/EDGE)."
  (with-test-graph (g)
    (let ((parsed (graph-db::peer-parse-type-table (graph-db::peer-type-table-string g))))
      (flet ((super-of (name)
               (fourth (find name parsed :key #'third :test #'string=))))
        (is (string= "g-person" (super-of "g-employee")))
        (is (null (super-of "g-person")))
        (is (null (super-of "g-knows")))))))

(test type-table-survives-the-plist-channel
  "The whole point of encoding the table as a STRING: a nested list would trip
PLIST-TOO-FANCY-ERROR.  Serialize the actual auth-ok plist and read it back."
  (with-test-graph (g)
    (let* ((table (graph-db::peer-type-table-string g))
           (plist (list :peer-control :auth-ok :type-table table))
           (bytes (graph-db::serialize-packet-plist plist))
           (back (graph-db::deserialize-packet-plist bytes)))
      (is (eq :auth-ok (getf back :peer-control)))
      (is (string= table (getf back :type-table)))
      (is (equal (graph-db::peer-parse-type-table table)
                 (graph-db::peer-parse-type-table (getf back :type-table)))))))

(test type-table-absent-parses-to-nil
  "An OLD hub sends no :type-table.  (getf plist :type-table) -> NIL must parse to NIL,
not signal -- that is the device's back-compat fallback path."
  (is (null (graph-db::peer-parse-type-table nil)))
  (is (null (graph-db::peer-parse-type-table ""))))
