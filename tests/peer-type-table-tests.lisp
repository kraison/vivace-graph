;;;; The peer type table: (kind, type-id, name, supers) shipped to devices.
;;;;
;;;; A type-id is meaningless on its own.  Ids come from the IMAGE's type
;;;; registry (GH #186; before that, per-graph counters), and NO type NAME ever
;;;; crosses the wire -- a receiver resolves the raw id against its OWN schema.
;;;; A Lisp device gets away with that only because it evaluates the same
;;;; schema.lisp; a non-Lisp peer (the Kotlin/SQLite device) cannot.  So the hub
;;;; ships the mapping.
;;;;
;;;; The load-bearing details these tests pin down:
;;;;
;;;;   - The registry's vertex and edge counters are SEPARATE and BOTH start at
;;;;     1, so a vertex type and an edge type routinely share a numeric id.  Any
;;;;     consumer keying on the id ALONE silently confuses them.
;;;;   - DEF-NODE-TYPE does not ENFORCE single inheritance -- it splices
;;;;     PARENT-TYPES straight into DEFCLASS -- so multiple inheritance really
;;;;     works on the hub.  The supers field is therefore a LIST, not a scalar;
;;;;     dropping the second parent would make a device's subclass closure
;;;;     silently disagree with the hub's.
;;;;   - The format is a FROZEN EXTERNAL CONTRACT (a Kotlin/SQLite device parses
;;;;     it), so the encoder validates what it emits and the reference parser is
;;;;     strict: both are the spec.
;;;;   - The table is the IMAGE's registry, so the hub's own graph does not
;;;;     bound it, and the handshake refuses a peer whose registry disagrees
;;;;     (D15).

(in-package #:graph-db/test)

(def-suite peer-type-table-suite
  :description
  "The (kind,id,name,supers) type table shipped in the peer auth-ok plist."
  :in graph-db-suite)

(in-suite peer-type-table-suite)

;;; Every test here needs its OWN system directory, and that is not tidiness.
;;; The table is the IMAGE's registry (GH #186) while RUN-TESTS gives the whole
;;; suite ONE system directory, so against the shared registry these tests would
;;; encode every type any other file has ever declared -- including this file's
;;; deliberately unrepresentable ones, which would then make every test here
;;; signal.  WITH-OWN-REGISTRY* is what keeps each test's table its own.

(defmacro with-ptt-registry ((registry) &body body)
  "Run BODY under a system directory -- hence a type registry -- of its own,
with REGISTRY bound to that registry."
  (let ((dir (gensym "SYSDIR")))
    `(with-temp-directory (,dir)
       (let* ((graph-db::*system-directory* (namestring ,dir))
              (graph-db::*type-registry* nil)
              (,registry (graph-db::ensure-type-registry)))
         (declare (ignorable ,registry))
         ,@body))))

(defmacro with-ptt-registry-graph ((g name &optional (registry (gensym "R")))
                                   &body body)
  "A graph of NAME in its own scratch directory AND its own system directory,
so REGISTRY holds NAME's types and nothing else."
  (let ((dir (gensym "DIR")))
    `(with-ptt-registry (,registry)
       (with-temp-directory (,dir)
         (let ((,g (make-graph ,name (namestring ,dir) :buffer-pool-size 1000)))
           (unwind-protect
                (let ((*graph* ,g))
                  ,@body)
             (ignore-errors (close-graph ,g :snapshot-p nil))
             (collect-garbage)))))))

(defun %ptt-qname (symbol)
  "SYMBOL as it now appears in the wire table's NAME/SUPERS fields: downcased
package-qualified (GH #201).  Tests below assert against this instead of the
bare symbol-name so they track the encoder's actual contract."
  (graph-db::%peer-qualified-wire-name symbol))

(defun %ptt-adopt (registry symbol parent id)
  "Record SYMBOL at exactly ID in REGISTRY, no graph involved.  The tests below
need registries holding ids and symbols DEF-VERTEX cannot produce -- an id
above the wire's range, two packages' same-named symbols -- and a store's
history can produce all of them."
  (graph-db::with-registry-append-lock (registry)
    (graph-db::%registry-adopt registry symbol parent id)))

(defun %ptt-refusal-text (thunk)
  "The report of the PEER-TYPE-REGISTRY-CONFLICT-ERROR THUNK signals, or NIL if
it signals none.  NIL is the ablated implementation's answer, so every caller
asserts STRINGP first."
  (handler-case (progn (funcall thunk) nil)
    (graph-db::peer-type-registry-conflict-error (c) (princ-to-string c))))

;;; --- A multiple-inheritance schema. ------------------------------------
;;; M-UAV is both an M-HAZARD and an M-ASSET.  The hub agrees:
;;; (typep uav 'm-asset) is true, and "all m-assets" includes it.  A wire
;;; format carrying only the FIRST parent would make the device disagree.

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

;;; --- Two stores, disjoint types plus one shared type (Task 3, GH #206). --
;;; PTS-A-ONLY lives only in store A's schema, PTS-B-ONLY only in store B's,
;;; and PTS-SHARED is registered under BOTH graph-names -- the same symbol,
;;; identical slots, so %WARN-IF-DIVERGENT-ACROSS-STORES stays silent (that
;;; is the documented multi-store feature, not a redefinition warning).

(eval-when (:load-toplevel :execute)
  (setf (gethash :peer-type-table-scope-a *schema-node-metadata*) nil)
  (setf (gethash :peer-type-table-scope-b *schema-node-metadata*) nil))

(def-vertex pts-a-only ()
  ()
  :peer-type-table-scope-a)

(def-vertex pts-b-only ()
  ()
  :peer-type-table-scope-b)

(def-vertex pts-shared ()
  ()
  :peer-type-table-scope-a)

(def-vertex pts-shared ()
  ()
  :peer-type-table-scope-b)

;;; --- A CLOS parent registered under a DIFFERENT graph-name (Task 3 fix,
;;; GH #206).  PTS-CROSS-PARENT lives only in store A; PTS-CROSS-CHILD names
;;; it as a direct superclass but is registered only in store B.  Scoping B
;;; naively would keep the child and drop the parent's row -- a dangling
;;; SUPERS reference.

(def-vertex pts-cross-parent ()
  ()
  :peer-type-table-scope-a)

(def-vertex pts-cross-child (pts-cross-parent)
  ()
  :peer-type-table-scope-b)

;;; --- A schema no wire format can represent: a name with a delimiter. -----
;;; CL symbol names may contain ANY character via |escaped| syntax, and
;;; DEF-VERTEX constrains nothing.  The ENCODER is the only possible defense:
;;; a table corrupted at the source is unrecoverable on the Kotlin side.

(eval-when (:load-toplevel :execute)
  (setf (gethash :peer-type-table-badname-test *schema-node-metadata*) nil))

(def-vertex |odd,name| ()
  ()
  :peer-type-table-badname-test)

;;; --- A schema whose names COLLIDE once downcased. ------------------------
;;; STRING-DOWNCASE is not injective.

(eval-when (:load-toplevel :execute)
  (setf (gethash :peer-type-table-dupname-test *schema-node-metadata*) nil))

(def-vertex p-person ()
  ()
  :peer-type-table-dupname-test)

(def-vertex |P-Person| ()
  ()
  :peer-type-table-dupname-test)

;;; --- Unregistered CLOS mixins in the inheritance chain (GH #216). --------
;;; PTM-MIXIN and PTM-TAGGED are plain DEFCLASS -- never DEF-VERTEXed, so in
;;; no registry.  PTM-CHILD inherits from registered PTM-BASE THROUGH
;;; PTM-MIXIN (which needs :METACLASS NODE-CLASS only because it inherits
;;; FROM a node class -- the default VALIDATE-SUPERCLASS rejects a
;;; STANDARD-CLASS subclass of a NODE-CLASS; PTM-TAGGED shows plain
;;; STANDARD-CLASS works fine ABOVE a node type.  DEF-NODE-TYPE checks
;;; nothing, so both shapes are legal).  PTM-LEAF mixes in PTM-TAGGED, which
;;; has no registered ancestor at all.  PTM-DIAMOND and PTM-DIRECT-AND-VIA
;;; pin dedup: two spliced paths to one registered ancestor must emit it
;;; once.

(eval-when (:load-toplevel :execute)
  (setf (gethash :peer-type-table-mixin-test *schema-node-metadata*) nil))

(def-vertex ptm-base ()
  ((label))
  :peer-type-table-mixin-test)

(defclass ptm-mixin (ptm-base) ()
  (:metaclass graph-db::node-class))

(defclass ptm-tagged () ())

(def-vertex ptm-child (ptm-mixin)
  ((extra))
  :peer-type-table-mixin-test)

(def-vertex ptm-leaf (ptm-tagged)
  ((leafish))
  :peer-type-table-mixin-test)

(defclass ptm-mixin-2 (ptm-base) ()
  (:metaclass graph-db::node-class))

(def-vertex ptm-diamond (ptm-mixin ptm-mixin-2)
  ()
  :peer-type-table-mixin-test)

;; PTM-MIXIN precedes PTM-BASE: it is PTM-BASE's subclass, so the other
;; order is an illegal class precedence list.
(def-vertex ptm-direct-and-via (ptm-mixin ptm-base)
  ()
  :peer-type-table-mixin-test)

;;; ---------------------------------------------------------------------------
;;; Round-trip
;;; ---------------------------------------------------------------------------

(test type-table-round-trips
  "PEER-PARSE-TYPE-TABLE inverts PEER-TYPE-TABLE-STRING."
  (with-ptt-registry-graph (g *integration-graph-name*)
    (declare (ignorable g))
    (let* ((s (graph-db::peer-type-table-string))
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
  "REGRESSION GUARD.  The registry's vertex and edge counters both start at 1
(type-registry.lisp), so a vertex type and an edge type share a numeric id. The
table must therefore be keyed on (KIND . ID).  If this ever fails because the
ids no longer collide, the KIND field is STILL required -- do not 'simplify' it
away."
  (with-ptt-registry-graph (g *integration-graph-name*)
    (declare (ignorable g))
    (let* ((parsed (graph-db::peer-parse-type-table
                    (graph-db::peer-type-table-string)))
           (vertex-ids (loop for row in parsed
                             when (eq (first row) :vertex)
                               collect (second row)))
           (edge-ids (loop for row in parsed
                           when (eq (first row) :edge)
                             collect (second row))))
      (is (plusp (length vertex-ids)))
      (is (plusp (length edge-ids)))
      (is (intersection vertex-ids edge-ids))
      (let ((keys (loop for row in parsed
                        collect (cons (first row) (second row)))))
        (is (= (length keys)
               (length (remove-duplicates keys :test #'equal))))))))

(test type-table-reports-direct-superclasses-only
  "A subclass reports its DIRECT parents; a root type reports NIL (not
VERTEX/EDGE).  NAME and SUPERS are package-qualified since #201 -- updated
from the pre-#201 bare-name assertions."
  (with-ptt-registry-graph (g *integration-graph-name*)
    (declare (ignorable g))
    (let ((parsed (graph-db::peer-parse-type-table
                   (graph-db::peer-type-table-string))))
      (flet ((supers-of (name)
               (fourth (find name parsed :key #'third :test #'string=))))
        (is (equal (list (%ptt-qname 'g-person))
                   (supers-of (%ptt-qname 'g-employee))))
        (is (null (supers-of (%ptt-qname 'g-person))))
        (is (null (supers-of (%ptt-qname 'g-knows))))))))

(test type-table-survives-the-plist-channel
  "The whole point of encoding the table as a STRING: a nested list would trip
PLIST-TOO-FANCY-ERROR.  Serialize the actual auth-ok plist and read it back."
  (with-ptt-registry-graph (g *integration-graph-name*)
    (declare (ignorable g))
    (let* ((table (graph-db::peer-type-table-string))
           (plist (list :peer-control :auth-ok :type-table table))
           (bytes (graph-db::serialize-packet-plist plist))
           (back (graph-db::deserialize-packet-plist bytes)))
      (is (eq :auth-ok (getf back :peer-control)))
      (is (string= table (getf back :type-table)))
      (is (equal (graph-db::peer-parse-type-table table)
                 (graph-db::peer-parse-type-table (getf back :type-table)))))))

(test type-table-absent-parses-to-nil
  "An OLD hub sends no :type-table.  (getf plist :type-table) -> NIL must parse
to NIL, not signal -- that is the device's back-compat fallback path."
  (is (null (graph-db::peer-parse-type-table nil)))
  (is (null (graph-db::peer-parse-type-table ""))))

;;; ---------------------------------------------------------------------------
;;; The table is the IMAGE's registry, not one graph's schema (D14)
;;; ---------------------------------------------------------------------------

(test type-table-is-the-image-registry-not-one-graph-s-schema
  "Two stores, ONE image, ONE table.  Under the per-graph schema the table
could only ever name the hub graph's own types; under the registry it names
every type the SYSTEM has assigned, because a device may be sent an id from any
of them.

Non-vacuous by construction: each store's schema is asserted NOT to know the
other's type, so no single SCHEMA-TYPE-TABLE could produce this table -- an
implementation that still read (SCHEMA GRAPH) fails here whichever graph it
read.  ROW is looked up by the package-qualified name (GH #201) -- updated
from the pre-#201 bare-name lookup."
  (with-ptt-registry (r)
    (with-temp-directory (d1)
      (with-temp-directory (d2)
        (let ((g1 (make-graph *integration-graph-name* (namestring d1)
                              :buffer-pool-size 1000))
              (g2 (make-graph :peer-type-table-mi-test (namestring d2)
                              :buffer-pool-size 1000)))
          (unwind-protect
               (let* ((parsed (graph-db::peer-parse-type-table
                               (graph-db::peer-type-table-string r))))
                 (flet ((row (name)
                          (find name parsed :key #'third :test #'string=))
                        (knows-p (g sym parent)
                          (and (graph-db::lookup-node-type-by-name
                                sym parent :graph g)
                               t)))
                   (is (row (%ptt-qname 'g-person))
                       "the first store's type is in the table")
                   (is (row (%ptt-qname 'm-uav)) "and so is the second store's")
                   (is (not (knows-p g1 'm-uav :vertex))
                       "store 1's schema does not know M-UAV")
                   (is (not (knows-p g2 'g-person :vertex))
                       "store 2's schema does not know G-PERSON")
                   ;; The ids are the registry's, not either store's counter.
                   (is (eql (second (row (%ptt-qname 'm-uav)))
                            (graph-db::registry-id-for r 'm-uav :vertex)))
                   (is (eql (second (row (%ptt-qname 'g-knows)))
                            (graph-db::registry-id-for r 'g-knows :edge)))))
            (ignore-errors (close-graph g1 :snapshot-p nil))
            (ignore-errors (close-graph g2 :snapshot-p nil))
            (collect-garbage)))))))

;;; ---------------------------------------------------------------------------
;;; Multiple inheritance: the supers field is a LIST
;;; ---------------------------------------------------------------------------

(test type-table-carries-every-super-of-a-multiple-inheritance-type
  "DEF-NODE-TYPE's docstring claims single inheritance but does NOT enforce it
-- it splices PARENT-TYPES straight into DEFCLASS.  So M-UAV really IS both an
M-HAZARD and an M-ASSET on the hub.  Emitting only the FIRST parent would
silently drop M-UAV from a device's \"all m-assets\" closure while the hub kept
it: per-peer divergent wrong answers. The supers field is therefore a SPACE-
SEPARATED LIST.  NAME/SUPERS are package-qualified since #201 -- updated from
the pre-#201 bare-name assertions."
  (with-ptt-registry-graph (g :peer-type-table-mi-test)
    (declare (ignorable g))
    ;; The hub genuinely believes in the second parent.
    (is (subtypep 'm-uav 'm-asset))
    (let* ((s (graph-db::peer-type-table-string))
           (parsed (graph-db::peer-parse-type-table s)))
      (is (search (format nil ",~A,~A ~A" (%ptt-qname 'm-uav)
                          (%ptt-qname 'm-hazard) (%ptt-qname 'm-asset))
                  s))
      (flet ((supers-of (name)
               (fourth (find name parsed :key #'third :test #'string=))))
        (is (equal (list (%ptt-qname 'm-hazard) (%ptt-qname 'm-asset))
                   (supers-of (%ptt-qname 'm-uav))))
        (is (null (supers-of (%ptt-qname 'm-hazard))))
        (is (null (supers-of (%ptt-qname 'm-asset))))
        (is (null (supers-of (%ptt-qname 'm-find-of-type))))))))

;;; ---------------------------------------------------------------------------
;;; Unregistered CLOS mixins carry no wire meaning (GH #216)
;;; ---------------------------------------------------------------------------

(test type-table-splices-out-an-unregistered-mixin
  "A plain CLOS mixin used as a node type's direct superclass has no row --
DEF-VERTEX never saw it -- so emitting it into SUPERS made
%PEER-VALIDATE-TYPE-TABLE-ROWS refuse the whole table (GH #216).  The mixin
must be filtered out of SUPERS, and a registered ancestor reached THROUGH it
must be spliced in, or the child would drop out of its ancestor's closure on
the device while the hub kept it.  The validator itself stays strict (see
TYPE-TABLE-VALIDATE-ROWS-SIGNALS-ON-DANGLING-SUPER)."
  (with-ptt-registry-graph (g :peer-type-table-mixin-test r)
    ;; The hub genuinely believes PTM-CHILD is a PTM-BASE, via the mixin.
    (is (subtypep 'ptm-child 'ptm-mixin))
    (is (subtypep 'ptm-child 'ptm-base))
    (let* ((s (graph-db::peer-type-table-string))
           (parsed (graph-db::peer-parse-type-table s)))
      ;; (a) The table encodes at all -- the bug was a refusal here.
      (is (stringp s))
      ;; The unregistered mixins appear NOWHERE: no row, no SUPERS entry.
      (is (not (search "ptm-mixin" s)))
      (is (not (search "ptm-tagged" s)))
      (flet ((supers-of (name)
               (fourth (find name parsed :key #'third :test #'string=))))
        ;; (b) The registered ancestor reached THROUGH the mixin is spliced
        ;; into SUPERS in the mixin's place.
        (is (equal (list (%ptt-qname 'ptm-base))
                   (supers-of (%ptt-qname 'ptm-child))))
        ;; A mixin with no registered ancestor simply vanishes.
        (is (null (supers-of (%ptt-qname 'ptm-leaf))))
        ;; Dedup pins: two mixins sharing one registered ancestor
        ;; (diamond), and an ancestor both direct AND via a mixin, each
        ;; emit it exactly ONCE.  EQUAL against the singleton also pins
        ;; the order stable.
        (is (equal (list (%ptt-qname 'ptm-base))
                   (supers-of (%ptt-qname 'ptm-diamond))))
        (is (equal (list (%ptt-qname 'ptm-base))
                   (supers-of (%ptt-qname 'ptm-direct-and-via)))))
      ;; The hub's auth-ok path scopes to the graph; it must encode too,
      ;; and the spliced SUPERS keeps its closure resolvable.
      (let ((scoped (graph-db::peer-parse-type-table
                     (graph-db::peer-type-table-string r g))))
        (is (find (%ptt-qname 'ptm-base) scoped
                  :key #'third :test #'string=))
        (dolist (row scoped)
          (dolist (super (fourth row))
            (is (find super scoped :key #'third :test #'string=)
                "scoped SUPERS entry ~S must resolve" super)))))))

;;; ---------------------------------------------------------------------------
;;; Encode-time validation: a loud hub error beats silent device corruption
;;; ---------------------------------------------------------------------------

(test type-table-encoder-rejects-a-name-carrying-a-delimiter
  "|odd,name| is a legal CL symbol and a legal DEF-VERTEX name, but it cannot
be represented on the wire: the comma splits it into two fields.  (A
|semi;colon| is worse -- it splits into two RECORDS, the first of which parses
SILENTLY.)  The encoder must refuse, naming the type, rather than ship a
corrupt table to a device that cannot possibly recover from it."
  (with-ptt-registry-graph (g :peer-type-table-badname-test)
    (declare (ignorable g))
    (signals error (graph-db::peer-type-table-string))))

(test type-table-encoder-rejects-names-that-collide-when-downcased
  "A RESIDUAL collision (#201): P-PERSON and |P-Person| are distinct CLOS
classes in the SAME package, so package-qualifying does not separate them --
their qualified names still downcase alike.  STRING-DOWNCASE is not
injective, and a device would resolve one type-id's name to the other type.
Contrast TYPE-TABLE-ENCODES-TWO-SAME-NAMED-TYPES-FROM-DIFFERENT-PACKAGES,
where qualifying the DIFFERENT packages is exactly what avoids this."
  (with-ptt-registry-graph (g :peer-type-table-dupname-test)
    (declare (ignorable g))
    (signals error (graph-db::peer-type-table-string))))

(test type-table-encoder-rejects-packages-that-collide-when-downcased
  "The OTHER residual-collision shape (#201): two DIFFERENT packages whose
own names downcase alike -- \"Wv2-Case-Pkg\" and \"WV2-CASE-PKG\" -- each
holding a symbol of the SAME name.  Package-qualifying does not help here
because the PACKAGE half of the qualified string is what collides."
  (with-ptt-registry (r)
    (let* ((p1 (or (find-package "Wv2-Case-Pkg")
                   (make-package "Wv2-Case-Pkg")))
           (p2 (or (find-package "WV2-CASE-PKG") (make-package "WV2-CASE-PKG")))
           (s1 (intern "TWIN" p1))
           (s2 (intern "TWIN" p2)))
      (unwind-protect
           (progn
             (%ptt-adopt r s1 :vertex 1)
             (%ptt-adopt r s2 :vertex 2)
             (let ((text (handler-case
                             (progn (graph-db::peer-type-table-string r) nil)
                           (error (c) (princ-to-string c)))))
               (is (stringp text) "the encoder must refuse the collision")
               (is (search "Wv2-Case-Pkg" text)
                   "the first package must be named package-qualified")
               (is (search "WV2-CASE-PKG::TWIN" text)
                   "the second type must be named package-qualified")))
        (delete-package p1)
        (delete-package p2)))))

(test type-table-encodes-two-same-named-types-from-different-packages
  "THE #201 ACCEPTANCE.  Two symbols with the SAME name in DIFFERENT
packages, registered from different stores (GH #186), used to be
UNREPRESENTABLE (see the retired TYPE-TABLE-COLLISION-ERROR-NAMES-THE-
PACKAGES): the pre-#201 encoder emitted the bare symbol-name, so both rows
collided under STRING-DOWNCASE and PEER-TYPE-TABLE-STRING signalled.  Since
#201's package-qualified NAME (%PEER-QUALIFIED-WIRE-NAME) the two packages
themselves disambiguate the rows, so this now encodes cleanly with both
rows present, present under DISTINCT qualified names, and round-trips.
DEF-VERTEX cannot build this case -- a schema file defines its types in one
package -- so it is registered directly."
  (with-ptt-registry (r)
    (let* ((p1 (or (find-package "PTT-PKG-ONE") (make-package "PTT-PKG-ONE")))
           (p2 (or (find-package "PTT-PKG-TWO") (make-package "PTT-PKG-TWO")))
           (s1 (intern "PTT-TWIN" p1))
           (s2 (intern "PTT-TWIN" p2)))
      (%ptt-adopt r s1 :vertex 1)
      (%ptt-adopt r s2 :vertex 2)
      (let* ((table (graph-db::peer-type-table-string r))
             (parsed (graph-db::peer-parse-type-table table)))
        (is (stringp table) "the encoder no longer refuses this pair")
        (is (search "ptt-pkg-one:ptt-twin" table))
        (is (search "ptt-pkg-two:ptt-twin" table))
        (is (find "ptt-pkg-one:ptt-twin" parsed :key #'third :test #'string=)
            "the first type has its own row")
        (is (find "ptt-pkg-two:ptt-twin" parsed :key #'third :test #'string=)
            "and the second, DISTINCT from the first")
        (is (/= (second (find "ptt-pkg-one:ptt-twin" parsed
                              :key #'third :test #'string=))
                (second (find "ptt-pkg-two:ptt-twin" parsed
                              :key #'third :test #'string=)))
            "distinct rows carry their own ids")))))

(test type-table-encoder-rejects-a-package-name-carrying-a-reserved-char
  "Package names are UNCONSTRAINED too (MAKE-PACKAGE takes any string), and
since #201 the package half is now part of what hits the wire.
%PEER-CHECK-WIRE-NAME runs on the FULL qualified string, so a package name
with a space -- the cheapest honest reserved-char case -- must refuse exactly
as a bad symbol-name would, not just get silently mangled into the SUPERS
field's own space-separator convention."
  (with-ptt-registry (r)
    (let* ((p (or (find-package "PTT BAD PKG") (make-package "PTT BAD PKG")))
           (s (intern "OK-NAME" p)))
      (unwind-protect
           (progn
             (%ptt-adopt r s :vertex 1)
             (let ((text (handler-case
                             (progn (graph-db::peer-type-table-string r) nil)
                           (error (c) (princ-to-string c)))))
               (is (stringp text)
                   "the encoder must refuse a reserved char in the package ~
name")
               (is (search "reserved character" text))))
        (delete-package p)))))

(test type-table-encoder-rejects-an-id-the-wire-cannot-carry
  "The registry assigns 32-bit type-ids; the wire's ID field is frozen at
(UNSIGNED-BYTE 16).  Before this the ENCODER emitted the row anyway and only
the reference PARSER refused, so the failure landed on the device rather than
on the hub that produced it.  The check is on PEER-TYPE-TABLE-STRING alone
here, so a parser-only implementation cannot pass: nothing in this test parses
anything.  Widening the field is GH #199."
  (with-ptt-registry (r)
    (%ptt-adopt r 'ptt-too-wide :vertex 70000)
    (let ((text (handler-case
                    (progn (graph-db::peer-type-table-string r) nil)
                  (error (c) (princ-to-string c)))))
      (is (stringp text) "the encoder must refuse an out-of-range type-id")
      (is (search "70000" text) "naming the id")
      (is (search "PTT-TOO-WIDE" text) "and the type"))))

;;; ---------------------------------------------------------------------------
;;; D15: the handshake refuses a peer whose registry disagrees
;;; ---------------------------------------------------------------------------

(test handshake-accepts-a-hub-whose-registry-agrees
  "The control for the two refusal tests below: an implementation that refused
everything would pass those and fail this one."
  (with-ptt-registry (r)
    (%ptt-adopt r 'ptt-agree-a :vertex 1)
    (%ptt-adopt r 'ptt-agree-b :edge 1)
    (let* ((table (graph-db::peer-type-table-string r))
           (rows (graph-db::peer-device-accept-auth-ok
                  (list :peer-control :auth-ok :type-table table) r)))
      (is (equal (graph-db::peer-parse-type-table table) rows)
          "an agreeing table is accepted and returned parsed"))))

(test handshake-refuses-a-hub-that-gives-one-symbol-another-id
  "D15.  An image with no hub is its own authority, so two of them can hand the
same symbol different ids; a node arriving under the hub's id would then
materialise as the wrong class here.  Refuse, and NAME the symbol -- an
operator has to find it in two stores, so the package has to be in the message.

Reconciling instead would mean rewriting every node of that type because a
network handshake said so, which is why this is a refusal and not a merge.

The hand-built hub table's NAME field is package-qualified (#201) -- updated
from the pre-#201 bare \"ptt-conflict-type\", which no longer matches this
image's now-qualified row and so no longer triggers the conflict at all."
  (with-ptt-registry (r)
    (%ptt-adopt r 'ptt-conflict-type :vertex 7)
    (let ((text (%ptt-refusal-text
                 (lambda ()
                   (graph-db::peer-device-accept-auth-ok
                    (list :peer-control :auth-ok
                          :type-table (format nil "v,4,~A,"
                                              (%ptt-qname 'ptt-conflict-type)))
                    r)))))
      (is (stringp text) "the handshake must REFUSE a disagreeing registry")
      (is (search "GRAPH-DB/TEST::PTT-CONFLICT-TYPE" text)
          "naming the conflicting symbol, package-qualified")
      (is (search "7" text) "and both ids it is caught between")
      (is (search "4" text)))))

(test handshake-refuses-a-hub-whose-id-means-another-type-here
  "The direction a name-keyed comparison MISSES.  The hub's type is unknown in
this image, so there is no shared name to compare -- but its id already means a
local type, and a node arriving under it materialises as that local type.  An
implementation checking only name -> id passes the test above and fails this
one."
  (with-ptt-registry (r)
    (%ptt-adopt r 'ptt-local-only :vertex 3)
    (let ((text (%ptt-refusal-text
                 (lambda ()
                   (graph-db::peer-device-accept-auth-ok
                    (list :peer-control :auth-ok
                          :type-table "v,3,ptt-hub-only,")
                    r)))))
      (is (stringp text) "an id meaning two types must be refused")
      (is (search "GRAPH-DB/TEST::PTT-LOCAL-ONLY" text)
          "naming what the id means HERE")
      (is (search "ptt-hub-only" text)
          "and what the hub says it means"))))

(test handshake-refuses-a-conflict-carried-over-the-real-wire
  "The same refusal, but the table is the one the HUB actually builds (PEER-
TYPE-TABLE-STRING) and it crosses the real plist codec on the way.  Covers hub
encode + wire + device refusal in one; only the socket itself is stubbed."
  (let (hub-table)
    ;; The hub's image: one symbol, its own registry, its own ids.
    (with-ptt-registry (hub)
      (%ptt-adopt hub 'ptt-wire-type :vertex 11)
      (setf hub-table (graph-db::peer-type-table-string hub)))
    ;; The device's image: the same symbol, a different id.
    (with-ptt-registry (device)
      (%ptt-adopt device 'ptt-wire-type :vertex 12)
      (let* ((bytes (graph-db::serialize-packet-plist
                     (list :peer-control :auth-ok :type-table hub-table)))
             (ctrl (graph-db::deserialize-packet-plist bytes))
             (text (%ptt-refusal-text
                    (lambda ()
                      (graph-db::peer-device-accept-auth-ok ctrl device)))))
        (is (stringp text)
            "a conflict that crosses the real wire must still be refused")
        (is (search "GRAPH-DB/TEST::PTT-WIRE-TYPE" text))))))

(test handshake-still-accepts-a-hub-too-old-to-ship-a-table
  "The one hole in the guard, kept deliberately and recorded here so it is not
mistaken for coverage: a hub that ships no :type-table cannot be compared at
all, so it is trusted.  Removing this back-compat path would break every
pre-#186 hub."
  (with-ptt-registry (r)
    (%ptt-adopt r 'ptt-oldhub-type :vertex 1)
    (is (null (graph-db::peer-device-accept-auth-ok
               (list :peer-control :auth-ok) r)))
    (is (null (graph-db::peer-device-accept-auth-ok
               (list :peer-control :auth-ok :type-table "") r)))))

(test handshake-still-refuses-an-auth-rejection
  "The check D15 was added alongside must not have been displaced by it."
  (with-ptt-registry (r)
    (signals error
      (graph-db::peer-device-accept-auth-ok
       (list :peer-control :auth-failed) r))))

;;; ---------------------------------------------------------------------------
;;; The reference parser IS the spec: it must be strict
;;; ---------------------------------------------------------------------------

(test type-table-parser-rejects-malformed-records
  "Every laxness here is a place the Kotlin parser will silently diverge.  A
record has EXACTLY 4 fields; KIND is exactly \"v\" or \"e\" (anything else used
to become an EDGE); ID is an integer in 0..65535 (type-ids are (UNSIGNED-BYTE
16)); no record may be empty."
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
  "Type-ids are STABLE across schema evolution, so the table is sorted by ID,
NOT topologically.  Adding a superclass ABOVE an existing type therefore
produces a FORWARD reference -- T-B (id 1) names T-A (id 2).  Parsing must not
depend on declaration order; consumers MUST two-pass (read all rows, then
resolve names).  A single-pass consumer that resolves supers as it reads breaks
on the first hierarchy refactor."
  (let ((parsed (graph-db::peer-parse-type-table "v,1,t-b,t-a;v,2,t-a,")))
    (is (= 2 (length parsed)))
    (is (equal '(:vertex 1 "t-b" ("t-a")) (first parsed)))
    (is (equal '(:vertex 2 "t-a" nil) (second parsed)))))

;;; ---------------------------------------------------------------------------
;;; The call site, over a real socket
;;;
;;; Everything above drives PEER-DEVICE-ACCEPT-AUTH-OK directly, which cannot
;;; tell whether PEER-SYNC still calls it.  The peer harnesses that would --
;;; tests/peer-replication*/ -- are separate OS processes and are not in this
;;; suite, so deleting that one line would cost nothing anywhere.
;;;
;;; A full hub cannot run in this image (it would share *GRAPHS*, the schema
;;; registry and *GRAPH* with the device; see run-peer-test.sh).  Nothing here
;;; needs one: the refusal happens at auth-ok, before a single node moves, so
;;; a socket that speaks the three plists up to that point is a faithful hub
;;; for exactly this question.
;;; ---------------------------------------------------------------------------

(defparameter *ptt-sync-origin*
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 23))

(defun %ptt-serve-auth-ok (listener table version)
  "Accept ONE connection on LISTENER and play hub as far as auth-ok: the
handshake plist (VERSION is the device's own, so the same-major gate passes),
read the device's auth, answer :AUTH-OK carrying TABLE.  Then close, which is
what the device sees if it gets past the guard."
  (bordeaux-threads:make-thread
   (lambda ()
     (ignore-errors
      (unwind-protect
           (when (usocket:wait-for-input listener :timeout 20 :ready-only t)
             (let ((socket (usocket:socket-accept
                            listener :element-type '(unsigned-byte 8))))
               (unwind-protect
                    (progn
                      (graph-db::peer-write-plist
                       (list :peer-protocol-version
                             graph-db::*peer-protocol-version*
                             :name "PTT-FAKE-HUB"
                             :schema-major (first version)
                             :schema-minor (second version)
                             :schema-digest 0)
                       socket)
                      (graph-db::read-plist-packet socket)
                      (graph-db::peer-write-plist
                       (list :peer-control :auth-ok :type-table table)
                       socket))
                 (ignore-errors (usocket:socket-close socket)))))
        (ignore-errors (usocket:socket-close listener)))))
   :name "ptt fake hub"))

(defmacro with-ptt-device ((g registry) &body body)
  "A device peer-graph under its own system directory, with REGISTRY bound to
that directory's registry -- the one PEER-SYNC will compare the hub's table
against."
  (let ((dir (gensym "DIR")))
    `(with-ptt-registry (,registry)
       (with-temp-directory (,dir)
         (let ((,g (make-graph *integration-graph-name* (namestring ,dir)
                               :peer-role :device
                               :origin-id *ptt-sync-origin*
                               :peer-host "127.0.0.1" :replication-port 0
                               :buffer-pool-size 1000)))
           (unwind-protect (let ((*graph* ,g)) ,@body)
             (ignore-errors (close-graph ,g :snapshot-p nil))
             (collect-garbage)))))))

(defun %ptt-sync-against (g table)
  "Point device G at a fake hub answering auth-ok with TABLE, run PEER-SYNC,
and return the condition it signalled (never NIL: the fake hub closes right
after auth-ok, so a device that gets past the guard fails on the pull)."
  (let ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                         :element-type '(unsigned-byte 8))))
    (setf (graph-db::replication-port g) (usocket:get-local-port listener))
    (%ptt-serve-auth-ok listener table (graph-db::peer-schema-version g))
    (handler-case (progn (graph-db::peer-sync g :attempts 20) nil)
      (error (c) c))))

(test peer-sync-refuses-a-hub-whose-registry-disagrees
  "PEER-SYNC itself must run the guard, over a real socket.  The three tests
above call PEER-DEVICE-ACCEPT-AUTH-OK directly and would all still pass if
PEER-SYNC stopped calling it.

The hand-built hub table's NAME field is package-qualified (#201) -- updated
from the pre-#201 bare \"g-person\", which would no longer match this
image's now-qualified row."
  (with-ptt-device (g r)
    (let* ((mine (graph-db::registry-id-for r 'g-person :vertex))
           (c (%ptt-sync-against
               g (format nil "v,~D,~A," (+ 100 mine) (%ptt-qname 'g-person)))))
      (is (typep c 'graph-db::peer-type-registry-conflict-error)
          "PEER-SYNC must refuse at auth-ok; it signalled ~S" c)
      (when (typep c 'graph-db::peer-type-registry-conflict-error)
        (is (search "G-PERSON" (princ-to-string c))
            "naming the conflicting symbol")))))

(test peer-sync-does-not-refuse-a-hub-whose-registry-agrees
  "The control: the same socket, the same fake hub, a table built from the
device's OWN registry.  PEER-SYNC still fails -- the fake hub closes after
auth-ok and the pull never comes -- but NOT with a registry conflict.  Without
this, a guard that refused every table would pass the test above."
  (with-ptt-device (g r)
    (let ((c (%ptt-sync-against g (graph-db::peer-type-table-string r))))
      (is (not (typep c 'graph-db::peer-type-registry-conflict-error))
          "an agreeing table must get past the guard; it signalled ~S" c))))

;;; ---------------------------------------------------------------------------
;;; The escape hatch may READ a contradicted store; it may not SERVE one.
;;; ---------------------------------------------------------------------------

(test a-frozen-open-refuses-to-start-replication
  "WITH-SCHEMA-FROZEN exists so an operator can read a store whose ids the
registry contradicts.  OPEN-GRAPH calls START-REPLICATION unconditionally, so
without this the same hatch would let them SERVE one -- a hub shipping a type
table built from the REGISTRY while its node heads carry the STORE's
contradicted ids.  That corrupts a REMOTE peer, which no local guard sees.

Checked for both peer roles, since a device pushes authored ops to a hub and
is therefore just as able to send an id that means something else there."
  (dolist (role '(:hub :device))
    (with-ptt-registry (r)
      (with-temp-directory (dir)
        (let ((c (handler-case
                     (let ((g (with-schema-frozen ()
                                (make-graph *integration-graph-name*
                                            (namestring dir)
                                            :peer-role role
                                            :origin-id *ptt-sync-origin*
                                            :peer-host "127.0.0.1"
                                            :replication-port 0
                                            :buffer-pool-size 1000))))
                       (ignore-errors (close-graph g :snapshot-p nil))
                       nil)
                   (frozen-graph-cannot-replicate (e) e))))
          (is (typep c 'frozen-graph-cannot-replicate)
              "a frozen ~(~A~) must refuse to start replication; got ~S"
              role c)
          (when (typep c 'frozen-graph-cannot-replicate)
            (is (search "WITH-SCHEMA-FROZEN" (princ-to-string c))
                "and say why")))
        (collect-garbage)))))

(test an-ordinary-open-still-starts-replication
  "The control: the same graph, opened normally, must still come up.  A guard
that refused every peer-graph would pass the test above."
  (with-ptt-registry (r)
    (with-temp-directory (dir)
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :device
                           :origin-id *ptt-sync-origin*
                           :peer-host "127.0.0.1" :replication-port 0
                           :buffer-pool-size 1000)))
        (unwind-protect
             (progn
               (is (not (graph-db::schema-frozen-p g))
                   "an ordinary open is not frozen")
               (is (graph-db::peer-writer-mailbox g)
                   "and its writer funnel is running"))
          (ignore-errors (close-graph g :snapshot-p nil))))
      (collect-garbage))))

;;; ---------------------------------------------------------------------------
;;; Task 3 (GH #206): store-scoped table.  PEER-TYPE-TABLE-STRING's optional
;;; GRAPH filters rows to types actually registered in GRAPH's own schema
;;; (LOOKUP-NODE-TYPE-BY-NAME, direct symbol path, GH #190) -- the hub's
;;; auth-ok call site passes the session's graph so a device only ever learns
;;; about the store it is actually replicating.
;;; ---------------------------------------------------------------------------

(defmacro with-ptt-two-stores ((r ga gb) &body body)
  "One registry R, two stores under it: GA is
:PEER-TYPE-TABLE-SCOPE-A (PTS-A-ONLY, PTS-SHARED), GB is
:PEER-TYPE-TABLE-SCOPE-B (PTS-B-ONLY, PTS-SHARED)."
  (let ((da (gensym "DA")) (db (gensym "DB")))
    `(with-ptt-registry (,r)
       (with-temp-directory (,da)
         (with-temp-directory (,db)
           (let ((,ga (make-graph :peer-type-table-scope-a (namestring ,da)
                                  :buffer-pool-size 1000))
                 (,gb (make-graph :peer-type-table-scope-b (namestring ,db)
                                  :buffer-pool-size 1000)))
             (unwind-protect (let () ,@body)
               (ignore-errors (close-graph ,ga :snapshot-p nil))
               (ignore-errors (close-graph ,gb :snapshot-p nil))
               (collect-garbage))))))))

(test type-table-scoped-to-graph-excludes-the-other-store-s-type
  "THE DISCLOSURE PIN.  Store A's session table contains A's own type and the
SHARED type, and explicitly does NOT contain B-only's row.  Ablation: an
implementation that drops the GRAPH filter (the nearest wrong
implementation is calling PEER-TYPE-TABLE-STRING with no GRAPH, i.e. the
whole-image table) makes the absence assertion below fail, because the
whole-image control table (asserted first) DOES carry all three."
  (with-ptt-two-stores (r ga gb)
    (declare (ignorable gb))
    (let* ((whole-image (graph-db::peer-parse-type-table
                         (graph-db::peer-type-table-string r)))
           (scoped-a (graph-db::peer-parse-type-table
                      (graph-db::peer-type-table-string r ga))))
      (flet ((row (parsed name)
               (find name parsed :key #'third :test #'string=)))
        ;; Control: the unscoped table is the whole image, so it is NOT
        ;; vacuously missing B-only -- proving the absence below is the
        ;; filter's doing, not an empty registry.
        (is (row whole-image (%ptt-qname 'pts-a-only)))
        (is (row whole-image (%ptt-qname 'pts-b-only)))
        (is (row whole-image (%ptt-qname 'pts-shared)))
        ;; The scoped table for store A.
        (is (row scoped-a (%ptt-qname 'pts-a-only))
            "A's own type is in A's session table")
        (is (row scoped-a (%ptt-qname 'pts-shared))
            "the shared type is in A's session table too")
        (is (not (row scoped-a (%ptt-qname 'pts-b-only)))
            "B-only's row must NOT be disclosed to a session scoped to A")))))

(test type-table-scoped-to-graph-is-symmetric
  "The mirror of the pin above: B's session table carries B's own type and the
shared type, and not A-only's."
  (with-ptt-two-stores (r ga gb)
    (declare (ignorable ga))
    (let* ((scoped-b (graph-db::peer-parse-type-table
                      (graph-db::peer-type-table-string r gb))))
      (flet ((row (name) (find name scoped-b :key #'third :test #'string=)))
        (is (row (%ptt-qname 'pts-b-only)))
        (is (row (%ptt-qname 'pts-shared)))
        (is (not (row (%ptt-qname 'pts-a-only))))))))

(test type-table-no-graph-argument-stays-whole-image
  "Every existing direct caller (and the tests above this section) calls
PEER-TYPE-TABLE-STRING with no GRAPH and must keep seeing every type in the
registry -- the opt-in is additive, not a behaviour change for callers that
do not pass one."
  (with-ptt-two-stores (r ga gb)
    (declare (ignorable ga gb))
    (let ((parsed (graph-db::peer-parse-type-table
                   (graph-db::peer-type-table-string r))))
      (dolist (name (list 'pts-a-only 'pts-b-only 'pts-shared))
        (is (find (%ptt-qname name) parsed :key #'third :test #'string=)
            "no GRAPH argument must still disclose ~A" name)))))

(test type-table-scoped-still-round-trips
  "The scoped table is still exactly what PEER-PARSE-TYPE-TABLE expects: same
grammar, fewer rows."
  (with-ptt-two-stores (r ga gb)
    (declare (ignorable gb))
    (let* ((s (graph-db::peer-type-table-string r ga))
           (parsed (graph-db::peer-parse-type-table s)))
      (is (stringp s))
      (is (plusp (length parsed)))
      (dolist (row parsed)
        (is (member (first row) '(:vertex :edge)))
        (is (integerp (second row)))
        (is (stringp (third row)))
        (is (listp (fourth row)))))))

(test type-table-scoped-graph-survives-an-unrepresentable-type-elsewhere
  "The blast-radius improvement %PEER-VALIDATE-TYPE-TABLE-ROWS's docstring now
documents: an unrepresentable type living in a store UNRELATED to the
session's graph used to fail every device connection in the image (GH #186);
scoped to a graph that does not instantiate that type, the session's table
encodes cleanly."
  (with-ptt-registry (r)
    (with-temp-directory (da)
      (with-temp-directory (dbad)
        (let ((ga (make-graph :peer-type-table-scope-a (namestring da)
                              :buffer-pool-size 1000))
              (gbad (make-graph :peer-type-table-badname-test
                                (namestring dbad) :buffer-pool-size 1000)))
          (unwind-protect
               (progn
                 ;; The whole-image table still fails: |odd,name| is
                 ;; unrepresentable and it IS in the registry.
                 (signals error (graph-db::peer-type-table-string r))
                 ;; But A's session, which does not instantiate |odd,name|,
                 ;; is unaffected.
                 (is (stringp (graph-db::peer-type-table-string r ga))))
            (ignore-errors (close-graph ga :snapshot-p nil))
            (ignore-errors (close-graph gbad :snapshot-p nil))
            (collect-garbage)))))))

(test d15-scoped-hub-table-is-a-subset-the-device-does-not-conflict-on
  "D15 restated for a scoped table: a device whose OWN registry knows MORE
types than the hub's (scoped) table -- because the hub only disclosed one
store's worth -- must NOT see that as a conflict.  %PEER-REGISTRY-CONFLICTS
keys conflicts on HUB rows, so a row present locally but absent from
HUB-ROWS is silently not-a-conflict; this pins that behaviour rather than
changing the function."
  (with-ptt-two-stores (r ga gb)
    (declare (ignorable gb))
    (let ((hub-table (graph-db::peer-type-table-string r ga)))
      ;; The device's own registry (R) knows PTS-B-ONLY too -- strictly more
      ;; than the scoped hub table -- and must still be accepted.
      (is (equal (graph-db::peer-parse-type-table hub-table)
                 (graph-db::peer-check-type-registry-agreement hub-table r))
          "a scoped table that is a subset of the device's own registry's
world must be accepted, not refused"))))

;;; ---------------------------------------------------------------------------
;;; Fix round (GH #206): scoping closure-completes SUPERS across stores.
;;; ---------------------------------------------------------------------------

(test type-table-scoped-graph-closure-completes-a-cross-store-parent
  "PTS-CROSS-PARENT lives only in store A; PTS-CROSS-CHILD names it as a
direct superclass but is registered only in store B.  B's scoped table must
carry BOTH rows -- the child (direct) and the parent (closure-completed) --
and every SUPERS entry in the scoped table must resolve to some row's NAME,
so a device's closure walk never dangles.  ABLATION: disabling closure
completion (reverting %PEER-GRAPH-SCOPED-ROWS to the direct-only filter)
makes the \"parent row present\" assertion below fail -- the parent is
dropped while the child's SUPERS still names it."
  (with-ptt-two-stores (r ga gb)
    (declare (ignorable ga))
    (let* ((scoped-b (graph-db::peer-parse-type-table
                      (graph-db::peer-type-table-string r gb))))
      (flet ((row (name) (find name scoped-b :key #'third :test #'string=)))
        (is (row (%ptt-qname 'pts-cross-child))
            "the direct type is in B's session table")
        (is (row (%ptt-qname 'pts-cross-parent))
            "the cross-store parent must be closure-completed into B's table")
        (dolist (row scoped-b)
          (dolist (super (fourth row))
            (is (find super scoped-b :key #'third :test #'string=)
                "SUPERS entry ~S must resolve within the scoped table" super))))
      (is (equal (graph-db::peer-parse-type-table
                  (graph-db::peer-type-table-string r gb))
                 scoped-b)
          "the closure-completed scoped table still round-trips"))))

(test type-table-scoped-to-graph-excludes-the-other-store-s-type-still-holds
  "The original disclosure pin must not be weakened by closure completion:
B-only's row (PTS-B-ONLY, no one's parent) is still excluded from A's
scoped table."
  (with-ptt-two-stores (r ga gb)
    (declare (ignorable gb))
    (let* ((scoped-a (graph-db::peer-parse-type-table
                      (graph-db::peer-type-table-string r ga))))
      (is (not (find (%ptt-qname 'pts-b-only) scoped-a
                     :key #'third :test #'string=))
          "B-only is no one's parent, so closure completion must not pull
it in"))))

(test type-table-validate-rows-signals-on-dangling-super
  "%PEER-VALIDATE-TYPE-TABLE-ROWS's closure-integrity check: a synthetic row
set with a SUPERS entry naming no row must signal.  ABLATION: removing this
check makes this test fail, since %PEER-GRAPH-SCOPED-ROWS's own closure
completion can never produce a dangling reference on its own."
  (let ((dangling (list (list "v" 1 "pkg:child" '("pkg:missing-parent")
                              'pts-a-only))))
    (signals error (graph-db::%peer-validate-type-table-rows dangling))))

(test type-table-validate-rows-does-not-false-positive-on-empty-supers
  "A top-level type (empty SUPERS, rooted at VERTEX/EDGE) must not trip the
closure-integrity check, and neither must a whole-image table."
  (with-ptt-registry-graph (g *integration-graph-name*)
    (declare (ignorable g))
    (is (stringp (graph-db::peer-type-table-string)))))
