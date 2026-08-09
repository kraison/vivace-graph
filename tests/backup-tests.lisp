;;;; Manual snapshot + replay (restore-from-snapshot) round trip.
;;;;
;;;; Exercises the high-level backup path: SNAPSHOT writes a snap-<ts> file (one
;;;; s-expression per node) under the graph's txn-log/ dir, and REPLAY rebuilds a
;;;; brand-new empty graph from the newest snapshot in a directory -- restoring
;;;; nodes, slot values, edge endpoints/weight and (preserved) ids, then
;;;; regenerating views.  Reuses the g-person / g-knows schema from
;;;; graph-tests.lisp.

(in-package #:graph-db/test)

(def-suite backup-suite
  :description "Manual snapshot and replay (restore-from-snapshot)."
  :in graph-db-suite)

(in-suite backup-suite)

;;; ---------------------------------------------------------------------------
;;; Issue #56: specialized (non-byte) vector slots must survive snapshot/replay.
;;;
;;; The snapshot file is text, so a vector's ELEMENT TYPE has to be written down:
;;; BACKUP emits #V(<element-type> ...) and the restore readtable reads it back.
;;; Before the fix, the restore reader coerced every #(...) to (unsigned-byte 8)
;;; -- correct for ids, fatal for a SINGLE-FLOAT embedding.
;;; ---------------------------------------------------------------------------

(def-vertex bk-vec ()
  ((label :type string)
   (fvec))                              ; a (simple-array single-float (*))
  :graph-db-integration-test)

(def-edge bk-link ()
  ((payload))                           ; a specialized vector on an EDGE
  :graph-db-integration-test)

(defun %bk-floats (&rest values)
  "A (simple-array single-float (*)) holding VALUES."
  (make-array (length values) :element-type 'single-float
                              :initial-contents (mapcar (lambda (x)
                                                          (coerce x 'single-float))
                                                        values)))

(defun %bk-bytes (&rest values)
  (coerce values '(simple-array (unsigned-byte 8) (*))))

(test snapshot-replay-preserves-single-float-vector-slot
  "REGRESSION for issue #56.  A node slot holding a (simple-array single-float
(*)) round-trips through snapshot + replay with both its CONTENTS and its
ELEMENT TYPE intact.  Before the fix the replay signalled
  The value 1.0 is not of type (UNSIGNED-BYTE 8)."
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let* ((p1 (namestring dir1)) (p2 (namestring dir2))
             (original (%bk-floats 1.0 1.25 1.5 -2.75))
             (id nil))
        (let ((g (make-graph *integration-graph-name* p1 :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq id (id (make-bk-vec :label "floats" :fvec original))))
            (is (= 1 (graph-db:snapshot g))
                "expected exactly 1 node snapshotted"))
          (close-graph g :snapshot-p nil))
        (let ((g2 (make-graph *integration-graph-name* p2 :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1) :graph-db/test)
                 (let ((v (lookup-vertex id)))
                   (is-true v "the float-vector vertex ~A was not restored at all" id)
                   (when v
                     (is (equalp id (id v)) "restored vertex id differs from ~S" id)
                     (is (typep (id v) '(simple-array (unsigned-byte 8) (*)))
                         "restored vertex id is not an id byte vector: ~S" (type-of (id v)))
                     (let ((back (slot-value v 'fvec)))
                       (is (vectorp back)
                           "restored fvec should be a vector, got ~S" back)
                       (is (= 4 (length back))
                           "restored fvec should have 4 elements, got ~S" back)
                       (is (equalp original back)
                           "restored fvec ~S differs from the original ~S" back original)
                       (is (equal 'single-float (array-element-type back))
                           "restored fvec lost its element type: ~S (type-of ~S)"
                           (array-element-type back) (type-of back))
                       (is (typep back '(simple-array single-float (*)))
                           "restored fvec is not a specialized single-float vector: ~S"
                           (type-of back))
                       ;; element-by-element, so an all-zero or truncated vector
                       ;; cannot slip past the assertions above
                       (is (and (= 1.0 (aref back 0)) (= 1.25 (aref back 1))
                                (= 1.5 (aref back 2)) (= -2.75 (aref back 3)))
                           "restored fvec has the wrong values: ~S" back)))))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))

(defun %backup-text-round-trip (plist)
  "Print PLIST with the real snapshot writer, read it back with the real restore
readtable, and return what came out."
  (let ((text (with-output-to-string (out)
                (graph-db::write-backup-plist plist out))))
    (let ((*readtable* graph-db::*restore-readtable*)
          (*package* (find-package :graph-db/test)))
      (values (read-from-string text) text))))

(test snapshot-text-preserves-byte-vector-data
  "The case the old blanket #(...) coercion existed to serve: UNSIGNED-BYTE 8
data still comes back as a byte vector, not as a simple-vector of integers.

This is exercised at the snapshot TEXT layer (real writer + real restore
readtable) rather than through a node slot, because a raw byte vector is not a
storable slot value in the first place: SERIALIZE treats an (unsigned-byte 8)
vector as ALREADY-serialized bytes and splices it in raw, so the surrounding
alist no longer deserializes.  That is a pre-existing property of the binary
codec, unrelated to the snapshot format.  Byte vectors reach a snapshot as node
IDS and as an edge's FROM / TO, and those paths are covered end to end by the
other tests here."
  (let* ((bytes (%bk-bytes 0 1 127 128 255))
         (plist (list :v 'g-person (list (cons :name "b") (cons :blob bytes))
                      :id bytes :revision 0 :deleted-p nil)))
    (multiple-value-bind (back text) (%backup-text-round-trip plist)
      (is (search "#V((UNSIGNED-BYTE 8) 0 1 127 128 255)" text)
          "writer should emit an element-typed #V literal; wrote: ~A" text)
      (let ((blob (cdr (assoc :blob (third back))))
            (id (getf (nthcdr 3 back) :id)))
        (is-true blob "the :BLOB entry did not survive the round trip: ~S" back)
        (is (equalp bytes blob) "round-tripped blob ~S differs from ~S" blob bytes)
        (is (typep blob '(simple-array (unsigned-byte 8) (*)))
            "round-tripped blob is no longer a byte vector: ~S" (type-of blob))
        (is (= 255 (aref blob 4)) "round-tripped blob has wrong values: ~S" blob)
        (is (typep id '(simple-array (unsigned-byte 8) (*)))
            "round-tripped :ID is no longer an id byte vector: ~S" (type-of id))
        (is (equalp bytes id) "round-tripped :ID ~S differs from ~S" id bytes)))))

(test snapshot-vector-element-types-are-implementation-independent
  "A snapshot must be readable by an implementation OTHER than the one that
wrote it -- snapshot+replay is the documented way to move a graph between
implementations, and the ECL field devices back up to an SBCL hub.

ARRAY-ELEMENT-TYPE returns the implementation's own name for the upgraded type,
so printing it directly made snapshots implementation-specific: ECL emitted
#V(EXT:BYTE8 ...), which SBCL cannot even READ (\"Package EXT does not exist\"),
so an ECL-written snapshot could not be restored on SBCL at all.

Asserts the printed specifier is the STANDARD one for each element type
graph-db actually stores -- byte vectors (ids, blobs), single-float embeddings
and double-float packed coordinates -- and, as the property that really
matters, that it carries no package-qualified symbol."
  (flet ((spec-of (vector)
           (graph-db::%backup-element-type-spec vector))
         (printed (vector)
           (with-output-to-string (s)
             (let ((*print-pretty* nil))
               (princ (graph-db::make-backup-vector-literal vector) s)))))
    (let ((bytes   (make-array 3 :element-type (quote (unsigned-byte 8))
                                 :initial-contents (list 0 1 255)))
          (singles (make-array 2 :element-type (quote single-float)
                                 :initial-contents (list 1.0 2.0)))
          (doubles (make-array 2 :element-type (quote double-float)
                                 :initial-contents (list 1d0 2d0))))
      (is (equal (quote (unsigned-byte 8)) (spec-of bytes))
          "byte vector element type printed as ~S, not the standard specifier"
          (spec-of bytes))
      (is (equal (quote single-float) (spec-of singles))
          "single-float element type printed as ~S" (spec-of singles))
      (is (equal (quote double-float) (spec-of doubles))
          "double-float element type printed as ~S" (spec-of doubles))
      ;; THE portability property: no symbol in the literal may be
      ;; package-qualified, or another implementation cannot read it.
      (dolist (v (list bytes singles doubles))
        (let ((text (printed v)))
          (is (not (find #\: text))
              "printed #V literal ~S contains a package marker, so another ~
implementation cannot read this snapshot" text))))))

(test snapshot-replay-preserves-edge-endpoints-and-vector-payload
  "Edges carry TWO bare id byte vectors positionally (FROM and TO) plus their own
data, so they are the case most likely to break.  Round-trip an edge with a
single-float payload: endpoints, weight, payload and traversability all survive."
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let* ((p1 (namestring dir1)) (p2 (namestring dir2))
             (payload (%bk-floats 0.5 -0.25 4.0))
             (aid nil) (bid nil) (eid nil))
        (let ((g (make-graph *integration-graph-name* p1 :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (let* ((a (make-bk-vec :label "from"))
                     (b (make-bk-vec :label "to"))
                     (e (make-bk-link :from a :to b :weight 3.25 :payload payload)))
                (setq aid (id a) bid (id b) eid (id e))))
            (is (= 3 (graph-db:snapshot g))
                "expected 3 nodes snapshotted (2 vertices + 1 edge)"))
          (close-graph g :snapshot-p nil))
        (let ((g2 (make-graph *integration-graph-name* p2 :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1) :graph-db/test)
                 (let ((a (lookup-vertex aid)))
                   (is-true a "source vertex ~A was not restored" aid)
                   (is-true (lookup-vertex bid) "target vertex ~A was not restored" bid)
                   (when a
                     (let ((outs (outgoing-edges a)))
                       (is (= 1 (length outs))
                           "expected exactly 1 restored outgoing edge, got ~S" outs)
                       (when (= 1 (length outs))
                         (let ((e (first outs)))
                           (is (equalp eid (id e)) "restored edge lost its id")
                           (is (equalp aid (from e))
                               "restored edge FROM is ~S, expected ~S" (from e) aid)
                           (is (equalp bid (to e))
                               "restored edge TO is ~S, expected ~S" (to e) bid)
                           (is (typep (from e) '(simple-array (unsigned-byte 8) (*)))
                               "restored edge FROM is not an id byte vector: ~S"
                               (type-of (from e)))
                           (is (typep (to e) '(simple-array (unsigned-byte 8) (*)))
                               "restored edge TO is not an id byte vector: ~S"
                               (type-of (to e)))
                           (is (= 3.25 (weight e))
                               "restored edge weight is ~S, expected 3.25" (weight e))
                           (let ((back (slot-value e 'payload)))
                             (is (equalp payload back)
                                 "restored edge payload ~S differs from ~S" back payload)
                             (is (equal 'single-float (array-element-type back))
                                 "restored edge payload lost its element type: ~S"
                                 (type-of back))
                             (is (= 4.0 (aref back 2))
                                 "restored edge payload has the wrong values: ~S" back))))))))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))

(test snapshot-replay-repopulates-vector-index-segment
  "End-to-end for a :VECTOR-INDEX slot (SI-DOC/EMBEDDING, from
segment-integration-tests): after snapshot + replay into a fresh graph, the
restoring transactions repopulate the vector segment, so VECTOR-SEARCH on the
NEW graph finds the node.  This is #56 at the feature level -- an embedding is a
single-float vector, so before the fix the replay could not even parse it."
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let* ((p1 (namestring dir1)) (p2 (namestring dir2))
             (near-vec (%si-embedding 8 1.0))
             (far-vec (%si-embedding 8 50.0))
             (near nil) (far nil))
        (let ((g (make-graph *integration-graph-name* p1 :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq near (id (make-si-doc :title "near" :embedding near-vec)))
              (setq far (id (make-si-doc :title "far" :embedding far-vec))))
            (graph-db:snapshot g))
          (close-graph g :snapshot-p nil))
        (let ((g2 (make-graph *integration-graph-name* p2 :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 ;; the fresh graph has no segment at all before replay
                 (is (null (vector-search g2 'si-doc 'embedding near-vec 5))
                     "fresh graph must have nothing indexed before replay")
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1) :graph-db/test)
                 ;; the slot value itself came back with its element type
                 (let ((v (lookup-vertex near)))
                   (is-true v "the embedded vertex ~A was not restored" near)
                   (when v
                     (let ((back (slot-value v 'embedding)))
                       (is (equalp near-vec back)
                           "restored embedding ~S differs from ~S" back near-vec)
                       (is (equal 'single-float (array-element-type back))
                           "restored embedding lost its element type: ~S" (type-of back)))))
                 ;; and the segment was repopulated by the restoring transactions
                 (let ((hits (vector-search g2 'si-doc 'embedding near-vec 5)))
                   (is (= 2 (length hits))
                       "expected both restored docs in the rebuilt segment, got ~S" hits)
                   (when hits
                     (is (equalp near (cdr (first hits)))
                         "the nearest restored doc should rank first; got ~S"
                         (mapcar #'cdr hits))
                     (is (member far (mapcar #'cdr hits) :test #'equalp)
                         "the far doc ~A is missing from the rebuilt segment" far))))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))

(defun %write-old-format-snapshot (path a-id b-id e-id)
  "Hand-write a snapshot in the PRE-#V format: every vector, including the ids
and an edge's FROM / TO, printed as a bare #(...).  This is exactly what
graph-db wrote before the issue-#56 fix, and it must still restore."
  (flet ((bare (id) (format nil "#(~{~D~^ ~})" (coerce id 'list))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (format out "(:V G-PERSON ((:NAME . \"Old\") (:AGE . 51)) :ID ~A :REVISION 0 :DELETED-P NIL)~%"
              (bare a-id))
      (format out "(:V G-PERSON ((:NAME . \"Format\") (:AGE . 8)) :ID ~A :REVISION 0 :DELETED-P NIL)~%"
              (bare b-id))
      (format out "(:E G-KNOWS ~A ~A 1.5 ((:SINCE . 1999)) :ID ~A :REVISION 0 :DELETED-P NIL)~%"
              (bare a-id) (bare b-id) (bare e-id)))))

(test old-format-snapshot-still-replays
  "BACKWARD COMPATIBILITY GATE.  A snapshot written by a pre-fix graph-db --
bare #(...) for the vertex ids and for the edge's FROM / TO -- still replays:
those values are repaired back into id byte vectors at the consumption site.
Without that repair the ids come back as SIMPLE-VECTORs and nothing resolves."
  (with-temp-directory (snapdir)
    (let ((a-id (graph-db::gen-vertex-id))
          (b-id (graph-db::gen-vertex-id))
          (e-id (graph-db::gen-edge-id)))
      (%write-old-format-snapshot (merge-pathnames "snap-1" snapdir) a-id b-id e-id)
      (with-temp-directory (dir2)
        (let ((g2 (make-graph *integration-graph-name* (namestring dir2)
                              :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (graph-db:replay g2 snapdir :graph-db/test)
                 (let ((a (lookup-vertex a-id))
                       (b (lookup-vertex b-id)))
                   (is-true a "old-format vertex ~A did not restore" a-id)
                   (is-true b "old-format vertex ~A did not restore" b-id)
                   (when (and a b)
                     (is (typep (id a) '(simple-array (unsigned-byte 8) (*)))
                         "old-format id restored as ~S, not an id byte vector"
                         (type-of (id a)))
                     (is (string= "Old" (slot-value a 'name))
                         "old-format slot value lost: ~S" (slot-value a 'name))
                     (is (= 51 (slot-value a 'age)))
                     (is (string= "Format" (slot-value b 'name)))
                     (let ((outs (outgoing-edges a)))
                       (is (= 1 (length outs))
                           "old-format edge did not restore into the adjacency index: ~S"
                           outs)
                       (when (= 1 (length outs))
                         (let ((e (first outs)))
                           (is (equalp e-id (id e)) "old-format edge id lost")
                           (is (equalp a-id (from e))
                               "old-format edge FROM is ~S, expected ~S" (from e) a-id)
                           (is (equalp b-id (to e))
                               "old-format edge TO is ~S, expected ~S" (to e) b-id)
                           (is (typep (from e) '(simple-array (unsigned-byte 8) (*)))
                               "old-format edge FROM restored as ~S, not an id byte vector"
                               (type-of (from e)))
                           (is (= 1.5 (weight e))
                               "old-format edge weight is ~S, expected 1.5" (weight e))
                           (is (= 1999 (slot-value e 'since))))))))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage))))))))

(test snapshot-and-replay-round-trip
  "Snapshot a populated graph, then replay it into a fresh empty graph in a new
directory: vertex count, slot values, edge endpoints/weight and node ids all
survive the round trip."
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let ((p1 (namestring dir1)) (p2 (namestring dir2)) aid bid)
        ;; --- populate the source graph and take a MANUAL snapshot ---
        (let ((g (make-graph *integration-graph-name* p1 :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (let ((a (make-g-person :name "Snap" :age 42))
                    (b (make-g-person :name "Shot" :age 7)))
                (setq aid (id a) bid (id b))
                (make-g-knows :from a :to b :weight 2.5)))
            ;; SNAPSHOT writes <p1>/txn-log/snap-<ts>; it relies on *graph* (the
            ;; backup walk uses map-vertices/map-edges over *graph*).
            (let ((result (graph-db:snapshot g)))
              (is (integerp result)
                  "snapshot should return a node count, not ~S" result)
              (is (= 3 result)
                  "expected 3 nodes snapshotted (2 vertices + 1 edge); got ~A"
                  result)))
          (close-graph g :snapshot-p nil))   ; manual snapshot already on disk
        ;; --- replay into a brand-new EMPTY graph in a different directory ---
        (let ((g2 (make-graph *integration-graph-name* p2 :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1)
                                  :graph-db/test)
                 ;; both vertices restored
                 (is (= 2 (length (map-vertices #'identity g2 :collect-p t
                                                          :vertex-type 'g-person)))
                     "expected 2 restored g-person vertices")
                 ;; ids preserved + slot values intact
                 (let ((a (lookup-vertex aid)))
                   (is-true a "source vertex ~A was not restored" aid)
                   (is (string= "Snap" (slot-value a 'name)))
                   (is (= 42 (slot-value a 'age)))
                   ;; edge + adjacency restored, with endpoints and weight
                   (let ((outs (outgoing-edges a)))
                     (is (= 1 (length outs)) "expected 1 outgoing edge")
                     (is (equalp bid (to (first outs)))
                         "restored edge points to the wrong vertex")
                     (is (= 2.5 (weight (first outs))))))
                 (is-true (lookup-vertex bid) "target vertex ~A was not restored" bid))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))

(test snapshot-replay-restores-spatial-index
  "After snapshot + replay into a fresh graph, geometry-bearing nodes are
spatially queryable: replay re-applies them through the transaction path, so the
write-path hook repopulates the spatial index.  (GEO-PLACE / NODE-GEOMETRY come
from spatial-hook-tests.)"
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let ((p1 (namestring dir1)) (p2 (namestring dir2)) kh-id lv-id)
        ;; populate the source graph with a Kharkiv and a Lviv place, then snapshot
        (let ((g (make-graph *integration-graph-name* p1 :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq kh-id (id (make-geo-place :loc (make-point 37.1724d0 49.2020d0)))
                    lv-id (id (make-geo-place :loc (make-point 23.7183d0 50.0263d0)))))
            (graph-db:snapshot g))
          (close-graph g :snapshot-p nil))
        ;; replay into a brand-new empty graph
        (let ((g2 (make-graph *integration-graph-name* p2 :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 ;; the fresh graph has no spatial index at all before replay
                 ;; (indexes are created lazily, on the first geometry write)
                 (is (null (all-spatial-indexes g2))
                     "fresh graph has no spatial index yet")
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1) :graph-db/test)
                 ;; after replay the Kharkiv place is spatially indexed; Lviv is elsewhere
                 (let ((cands (spatial-index-query-bbox (spatial-index-for g2 'geo-place nil)
                                                        37.16d0 49.19d0 37.19d0 49.21d0)))
                   (is (member kh-id cands :test 'equalp) "Kharkiv place re-indexed by replay")
                   (is (not (member lv-id cands :test 'equalp)) "Lviv place outside the window"))
                 ;; and through the high-level query
                 (is (= 1 (length (find-nodes-near 'geo-place 49.2020d0 37.1724d0 500d0 :graph g2)))))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))

(test recovery-from-dirty-marker
  "A leftover .dirty marker (unclean prior shutdown) blocks open-graph; the
recovery procedure -- clear the marker, then reopen -- lets open-graph run
recover-transactions and reopen with all committed data intact."
  (with-temp-directory (dir)
    (let* ((path (namestring dir))
           (dirty (format nil "~A/.dirty" path))
           id)
      ;; commit data, then close cleanly (flushes data, removes .dirty)
      (let ((g (make-graph *integration-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (setq id (id (make-g-person :name "Survivor" :age 99)))))
        (close-graph g :snapshot-p nil))
      ;; simulate an unclean prior shutdown by re-creating the .dirty marker
      (with-open-file (out dirty :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
        (format out "~S" (get-universal-time)))
      (is-true (probe-file dirty) ".dirty marker should be present")
      ;; open-graph must refuse a dirty graph
      (signals error (open-graph *integration-graph-name* path))
      ;; recovery: clear the marker, then reopen (open-graph runs recover-transactions)
      (delete-file dirty)
      (let ((g2 (open-graph *integration-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((v (lookup-vertex id)))
                 (is-true v "committed vertex lost after dirty-marker recovery")
                 (is (string= "Survivor" (slot-value v 'name)))
                 (is (= 99 (slot-value v 'age))))
               (is (= 1 (length (map-vertices #'identity g2 :collect-p t
                                                        :vertex-type 'g-person)))))
          (close-graph g2 :snapshot-p nil)
          (collect-garbage))))))

(test snapshot-excludes-deleted-nodes
  "Snapshot omits deleted nodes by default (backup honors deleted-p); replay
restores only the live nodes."
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let ((p1 (namestring dir1)) (p2 (namestring dir2)) gone-id keep-id)
        (let ((g (make-graph *integration-graph-name* p1 :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq gone-id (id (make-g-person :name "Gone" :age 1)))
              (setq keep-id (id (make-g-person :name "Kept" :age 2))))
            (with-transaction ()
              (mark-deleted (lookup-vertex gone-id)))
            (let ((result (graph-db:snapshot g)))
              (is (= 1 result)
                  "snapshot should write only the 1 live vertex; got ~A" result)))
          (close-graph g :snapshot-p nil))
        (let ((g2 (make-graph *integration-graph-name* p2 :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1)
                                  :graph-db/test)
                 (is (= 1 (length (map-vertices #'identity g2 :collect-p t
                                                          :vertex-type 'g-person)))
                     "only the live vertex should be restored")
                 (is-true (lookup-vertex keep-id) "kept vertex missing after replay")
                 (is (null (lookup-vertex gone-id))
                     "deleted vertex should not have been restored"))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))

;;; ---------------------------------------------------------------------------
;;; GH #100: snapshot filenames must not come from the clock
;;; ---------------------------------------------------------------------------

(test gettimeofday-returns-two-integer-values
  "GH #100: every branch must return (VALUES SECONDS MICROSECONDS).  ECL had no
branch at all, so the whole body was empty and the call returned NIL -- which
then formatted into a snapshot filename as the literal \"NIL\"."
  (multiple-value-bind (sec usec) (graph-db::gettimeofday)
    (is (integerp sec) "seconds must be an integer, got ~S" sec)
    (is (integerp usec) "microseconds must be an integer, got ~S" usec)
    ;; Sanity: seconds since the epoch, not a fraction and not a nanosecond count.
    (is (> sec 1700000000) "seconds looks wrong for a Unix epoch time: ~S" sec)
    (is (<= 0 usec 999999) "microseconds out of range: ~S" usec)))

(test repeated-snapshots-do-not-overwrite-each-other
  "GH #100: the txn-log snapshot name was built from GETTIMEOFDAY, which returns
NIL on ECL -- so every snapshot of a graph wrote to ONE constant filename and
silently replaced its predecessor (ECL's :IF-EXISTS default overwrites).  Two
snapshots must leave two files."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (make-bk-vec :label "a"))
             (graph-db::snapshot g :check-data-integrity-p nil)
             (with-transaction () (make-bk-vec :label "b"))
             (graph-db::snapshot g :check-data-integrity-p nil)
             (let ((snaps (directory
                           (merge-pathnames "txn-log/snap-*"
                                            (uiop:ensure-directory-pathname
                                             (namestring dir))))))
               (is (= 2 (length snaps))
                   "expected 2 distinct snapshot files, got ~D: ~S"
                   (length snaps) (mapcar #'file-namestring snaps))))
        (ignore-errors (close-graph g :snapshot-p nil))
        (collect-garbage)))))

;;; ---------------------------------------------------------------------------
;;; Struct slot values must be literalized too (issue #56, second half)
;;;
;;; BACKUP-LITERALIZE walked conses and vectors but returned a STRUCT untouched,
;;; so a GEOMETRY in a node's data alist printed via #S(...) and its packed
;;; coordinate rings printed as bare #(...).  On restore those come back as
;;; element-type T vectors, GEOMETRY-BBOX's WALK-RING matches nothing, and every
;;; caller does arithmetic on the resulting NILs.  Measured on production
;;; snapshots this was 301,569 of the forensics graph's lines.
;;;
;;; The assertions below are on ARRAY-ELEMENT-TYPE, not on the values: EQUALP
;;; compares the two vectors equal either way, which is exactly why the rest of
;;; this file never caught it.
;;; ---------------------------------------------------------------------------

(defun %bk-polygon ()
  "A one-ring :POLYGON whose ring is a (simple-array double-float (*))."
  (make-polygon (list (list '(30.0d0 50.0d0) '(31.0d0 50.0d0)
                            '(31.0d0 51.0d0) '(30.0d0 50.0d0)))))

(defstruct (bk-frozen (:constructor make-bk-frozen (label)))
  (label "" :read-only t))                ; a struct slot with NO setf writer

(test backup-literalize-passes-unchanged-structs-through
  "A struct no slot of which needs literalizing must come back untouched.
SBCL has no (SETF SLOT-VALUE) writer for a :READ-ONLY struct slot and
type-checks a slot with a declared :TYPE, so writing every slot back
unconditionally would make BACKUP signal on any such struct in node data --
turning a lossy snapshot into no snapshot at all."
  (let ((frozen (make-bk-frozen "cold")))
    (is (eq frozen (graph-db::backup-literalize frozen))
        "an unchanged struct should be passed through, not rebuilt")))

(test snapshot-text-preserves-geometry-struct-coordinate-element-type
  "REGRESSION.  A GEOMETRY reached through a node's data alist keeps the packed
DOUBLE-FLOAT element type of its coordinate ring across the snapshot TEXT round
trip (real writer + real restore readtable), and #V reads correctly nested
inside #S.  Before the fix the ring was written as a bare #(...)."
  (let* ((poly (%bk-polygon))
         (plist (list :v 'geo-place (list (cons :loc poly))
                      :id (graph-db::gen-vertex-id)
                      :revision 0 :deleted-p nil)))
    (multiple-value-bind (back text) (%backup-text-round-trip plist)
      (is (search "#V(DOUBLE-FLOAT" text)
          "writer should emit an element-typed #V ring inside the #S literal; ~
wrote: ~A" text)
      ;; the caller's geometry must not have been mutated into a wrapper
      (is (typep (first (geometry-coordinates poly))
                 '(simple-array double-float (*)))
          "BACKUP-LITERALIZE mutated the caller's geometry: ~S"
          (first (geometry-coordinates poly)))
      (let ((g (cdr (assoc :loc (third back)))))
        (is-true (geometryp g) "the :LOC geometry did not survive: ~S" back)
        (when (geometryp g)
          (is (eq :polygon (geometry-kind g))
              "restored geometry kind is ~S" (geometry-kind g))
          (let ((ring (first (geometry-coordinates g))))
            (is (equal 'double-float (array-element-type ring))
                "restored ring lost its element type: ~S (type-of ~S)"
                (array-element-type ring) (type-of ring))
            (is (typep ring '(simple-array double-float (*)))
                "restored ring is not a packed double-float array: ~S"
                (type-of ring)))
          ;; the consequence the element type actually buys: WALK-RING is
          ;; TYPEP-gated on (simple-array double-float (*)), so an untyped ring
          ;; makes the bbox four NILs and every caller does arithmetic on NIL.
          (multiple-value-bind (min-lon min-lat max-lon max-lat)
              (geometry-bbox g)
            (is (equal (list 30.0d0 50.0d0 31.0d0 51.0d0)
                       (list min-lon min-lat max-lon max-lat))
                "restored geometry bbox is ~S, expected (30 50 31 51)"
                (list min-lon min-lat max-lon max-lat))))))))

(test snapshot-replay-preserves-geometry-slot-element-type
  "REGRESSION, end to end.  A GEO-PLACE holding a POLYGON survives snapshot +
replay into a fresh graph with the ring's DOUBLE-FLOAT element type intact, so
GEOMETRY-BBOX on the restored node still answers.  Snapshot replay is the only
recovery route for graphs with no rebuild path, so this is the recovery gate."
  (with-temp-directory (dir1)
    (with-temp-directory (dir2)
      (let* ((p1 (namestring dir1)) (p2 (namestring dir2))
             (id nil))
        (let ((g (make-graph *integration-graph-name* p1
                             :buffer-pool-size 1000)))
          (let ((*graph* g))
            (with-transaction ()
              (setq id (id (make-geo-place :loc (%bk-polygon)))))
            (graph-db:snapshot g))
          (close-graph g :snapshot-p nil))
        (let ((g2 (make-graph *integration-graph-name* p2
                              :buffer-pool-size 1000)))
          (unwind-protect
               (let ((*graph* g2))
                 (graph-db:replay g2 (merge-pathnames "txn-log/" dir1)
                                  :graph-db/test)
                 (let ((v (lookup-vertex id)))
                   (is-true v "the polygon vertex ~A was not restored" id)
                   (when v
                     (let* ((geom (slot-value v 'loc))
                            (ring (and (geometryp geom)
                                       (first (geometry-coordinates geom)))))
                       (is-true (geometryp geom)
                                "restored LOC is not a geometry: ~S" geom)
                       (when ring
                         (is (equal 'double-float
                                    (array-element-type ring))
                             "restored ring lost its element type: ~S"
                             (type-of ring))
                         (is (typep ring '(simple-array double-float (*)))
                             "restored ring is not packed double-float: ~S"
                             (type-of ring))
                         (multiple-value-bind (min-lon min-lat max-lon max-lat)
                             (geometry-bbox geom)
                           (is (equal (list 30.0d0 50.0d0 31.0d0 51.0d0)
                                      (list min-lon min-lat max-lon max-lat))
                               "restored bbox is ~S, expected (30 50 31 51)"
                               (list min-lon min-lat max-lon max-lat))))))))
            (close-graph g2 :snapshot-p nil)
            (collect-garbage)))))))
