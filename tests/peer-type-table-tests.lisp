;;;; The peer type table: (kind, type-id, name, supers) shipped to devices.
;;;;
;;;; A type-id is meaningless on its own.  Ids are handed out by DEF-VERTEX/DEF-EDGE
;;;; evaluation order (GET-NEXT-TYPE-ID), persisted per graph, and NO type NAME ever
;;;; crosses the wire -- a receiver resolves the raw uint16 against its OWN schema.  A
;;;; Lisp device gets away with that only because it evaluates the same schema.lisp; a
;;;; non-Lisp peer (the Kotlin/SQLite device) cannot.  So the hub ships the mapping.
;;;;
;;;; The load-bearing details these tests pin down:
;;;;
;;;;   - NEXT-VERTEX-ID and NEXT-EDGE-ID are SEPARATE counters that BOTH start at 1, so
;;;;     a vertex type and an edge type routinely share a numeric id.  Any consumer
;;;;     keying on the id ALONE silently confuses them.
;;;;   - DEF-NODE-TYPE does not ENFORCE single inheritance -- it splices PARENT-TYPES
;;;;     straight into DEFCLASS -- so multiple inheritance really works on the hub.  The
;;;;     supers field is therefore a LIST, not a scalar; dropping the second parent
;;;;     would make a device's subclass closure silently disagree with the hub's.
;;;;   - The format is a FROZEN EXTERNAL CONTRACT (a Kotlin/SQLite device parses it), so
;;;;     the encoder validates what it emits and the reference parser is strict: both
;;;;     are the spec.

(in-package #:graph-db/test)

(def-suite peer-type-table-suite
  :description "The (kind,id,name,supers) type table shipped in the peer auth-ok plist."
  :in graph-db-suite)

(in-suite peer-type-table-suite)

;;; The happy-path tests need no new schema: the g-* schema from graph-tests (loaded
;;; earlier in the system) is exactly the shape they need -- two vertex types where
;;; G-EMPLOYEE subclasses G-PERSON, and two edge types.  Both id spaces therefore start
;;; at 1, which is what makes the collision assertion below meaningful.  WITH-TEST-GRAPH
;;; (suite.lisp:113) builds a graph of *INTEGRATION-GRAPH-NAME* carrying it.
;;;
;;; The multiple-inheritance and encoder-rejection tests each need their OWN graph name:
;;; the type table is graph-WIDE, and *INTEGRATION-GRAPH-NAME*'s schema is shared with
;;; every other suite -- a deliberately unrepresentable type added there would break them
;;; all.  Hence the dedicated schemas below, and WITH-NAMED-TEST-GRAPH.

(defmacro with-named-test-graph ((g name) &body body)
  "Like WITH-TEST-GRAPH but for a graph of a name OTHER than *INTEGRATION-GRAPH-NAME*."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph ,name (namestring ,dir) :buffer-pool-size 1000)))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; --- A multiple-inheritance schema. --------------------------------------------
;;; M-UAV is both an M-HAZARD and an M-ASSET.  The hub agrees: (typep uav 'm-asset) is
;;; true, and "all m-assets" includes it.  A wire format carrying only the FIRST parent
;;; would make the device disagree.

(eval-when (:load-toplevel :execute)
  (setf (gethash :peer-type-table-mi-test *schema-node-metadata*) nil))

(def-vertex m-hazard ()
  ((severity))
  :peer-type-table-mi-test)

(def-vertex m-asset ()
  ((serial))
  :peer-type-table-mi-test)

(def-vertex m-uav (m-hazard m-asset)
  ((range))
  :peer-type-table-mi-test)

(def-edge m-find-of-type ()
  ()
  :peer-type-table-mi-test)

;;; --- A schema no wire format can represent: a name containing a delimiter. -------
;;; CL symbol names may contain ANY character via |escaped| syntax, and DEF-VERTEX
;;; constrains nothing.  The ENCODER is the only possible defense: a table corrupted at
;;; the source is unrecoverable on the Kotlin side.

(eval-when (:load-toplevel :execute)
  (setf (gethash :peer-type-table-badname-test *schema-node-metadata*) nil))

(def-vertex |odd,name| ()
  ()
  :peer-type-table-badname-test)

;;; --- A schema whose names COLLIDE once downcased. --------------------------------
;;; STRING-DOWNCASE is not injective.

(eval-when (:load-toplevel :execute)
  (setf (gethash :peer-type-table-dupname-test *schema-node-metadata*) nil))

(def-vertex p-person ()
  ()
  :peer-type-table-dupname-test)

(def-vertex |P-Person| ()
  ()
  :peer-type-table-dupname-test)

;;; ---------------------------------------------------------------------------
;;; Round-trip
;;; ---------------------------------------------------------------------------

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
        ;; SUPERS is a LIST of strings (possibly empty), never a bare string.
        (is (listp (fourth row)))
        (is (every #'stringp (fourth row)))))))

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

(test type-table-reports-direct-superclasses-only
  "A subclass reports its DIRECT parents; a root type reports NIL (not VERTEX/EDGE)."
  (with-test-graph (g)
    (let ((parsed (graph-db::peer-parse-type-table (graph-db::peer-type-table-string g))))
      (flet ((supers-of (name)
               (fourth (find name parsed :key #'third :test #'string=))))
        (is (equal '("g-person") (supers-of "g-employee")))
        (is (null (supers-of "g-person")))
        (is (null (supers-of "g-knows")))))))

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

;;; ---------------------------------------------------------------------------
;;; Multiple inheritance: the supers field is a LIST
;;; ---------------------------------------------------------------------------

(test type-table-carries-every-super-of-a-multiple-inheritance-type
  "DEF-NODE-TYPE's docstring claims single inheritance but does NOT enforce it -- it
splices PARENT-TYPES straight into DEFCLASS.  So M-UAV really IS both an M-HAZARD and an
M-ASSET on the hub.  Emitting only the FIRST parent would silently drop M-UAV from a
device's \"all m-assets\" closure while the hub kept it: per-peer divergent wrong answers.
The supers field is therefore a SPACE-SEPARATED LIST."
  (with-named-test-graph (g :peer-type-table-mi-test)
    ;; The hub genuinely believes in the second parent.
    (is (subtypep 'm-uav 'm-asset))
    (let* ((s (graph-db::peer-type-table-string g))
           (parsed (graph-db::peer-parse-type-table s)))
      (is (search ",m-uav,m-hazard m-asset" s))
      (flet ((supers-of (name)
               (fourth (find name parsed :key #'third :test #'string=))))
        (is (equal '("m-hazard" "m-asset") (supers-of "m-uav")))
        (is (null (supers-of "m-hazard")))
        (is (null (supers-of "m-asset")))
        (is (null (supers-of "m-find-of-type")))))))

;;; ---------------------------------------------------------------------------
;;; Encode-time validation: a loud hub error beats silent device corruption
;;; ---------------------------------------------------------------------------

(test type-table-encoder-rejects-a-name-carrying-a-delimiter
  "|odd,name| is a legal CL symbol and a legal DEF-VERTEX name, but it cannot be
represented on the wire: the comma splits it into two fields.  (A |semi;colon| is worse
-- it splits into two RECORDS, the first of which parses SILENTLY.)  The encoder must
refuse, naming the type, rather than ship a corrupt table to a device that cannot
possibly recover from it."
  (with-named-test-graph (g :peer-type-table-badname-test)
    (signals error (graph-db::peer-type-table-string g))))

(test type-table-encoder-rejects-names-that-collide-when-downcased
  "STRING-DOWNCASE is not injective: P-PERSON and |P-Person| are distinct CLOS classes
that emit the SAME name.  A device would resolve one type-id's name to the other type."
  (with-named-test-graph (g :peer-type-table-dupname-test)
    (signals error (graph-db::peer-type-table-string g))))

;;; ---------------------------------------------------------------------------
;;; The reference parser IS the spec: it must be strict
;;; ---------------------------------------------------------------------------

(test type-table-parser-rejects-malformed-records
  "Every laxness here is a place the Kotlin parser will silently diverge.  A record has
EXACTLY 4 fields; KIND is exactly \"v\" or \"e\" (anything else used to become an EDGE);
ID is an integer in 0..65535 (type-ids are (UNSIGNED-BYTE 16)); no record may be empty."
  ;; Wrong arity.
  (signals error (graph-db::peer-parse-type-table "v,1,foo"))
  (signals error (graph-db::peer-parse-type-table "v,1,foo,,extra"))
  ;; Bad kind -- the real trap: this used to parse SILENTLY as an edge.
  (signals error (graph-db::peer-parse-type-table "x,1,foo,"))
  (signals error (graph-db::peer-parse-type-table "V,1,foo,"))
  ;; Bad id.
  (signals error (graph-db::peer-parse-type-table "v,abc,foo,"))
  (signals error (graph-db::peer-parse-type-table "v,1x,foo,"))
  (signals error (graph-db::peer-parse-type-table "v,65536,foo,"))
  (signals error (graph-db::peer-parse-type-table "v,-1,foo,"))
  ;; Empty records: leading, trailing, consecutive separators.
  (signals error (graph-db::peer-parse-type-table ";v,1,foo,"))
  (signals error (graph-db::peer-parse-type-table "v,1,foo,;"))
  (signals error (graph-db::peer-parse-type-table "v,1,foo,;;v,2,bar,"))
  ;; An empty NAME is not a name.
  (signals error (graph-db::peer-parse-type-table "v,1,,"))
  ;; ...and the boundary that must still WORK.
  (is (equal '((:vertex 65535 "foo" nil))
             (graph-db::peer-parse-type-table "v,65535,foo,")))
  (is (equal '((:edge 0 "foo" ("bar" "baz")))
             (graph-db::peer-parse-type-table "e,0,foo,bar baz"))))

(test type-table-tolerates-forward-references
  "Type-ids are STABLE across schema evolution, so the table is sorted by ID, NOT
topologically.  Adding a superclass ABOVE an existing type therefore produces a FORWARD
reference -- T-B (id 1) names T-A (id 2).  Parsing must not depend on declaration order;
consumers MUST two-pass (read all rows, then resolve names).  A single-pass consumer that
resolves supers as it reads breaks on the first hierarchy refactor."
  (let ((parsed (graph-db::peer-parse-type-table "v,1,t-b,t-a;v,2,t-a,")))
    (is (= 2 (length parsed)))
    (is (equal '(:vertex 1 "t-b" ("t-a")) (first parsed)))
    (is (equal '(:vertex 2 "t-a" nil) (second parsed)))))
