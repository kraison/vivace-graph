;;;; type-id widened to 32 bits (GH #166).  See
;;;; docs/superpowers/specs/2026-08-20-namespaces-design.md §3.4.
(in-package #:graph-db/test)

(def-suite type-id-width-suite :in graph-db-suite
  :description "type-id is 32 bits wide, on disk and in memory.")
(in-suite type-id-width-suite)

;; GRAPH-DB has no production PACK-UINT-TO-MMAP (PACK-UINT packs into a byte
;; vector, not an mmap); this test needs to hand-write a legacy 31-byte v2
;; head byte-by-byte, so it gets a test-local helper instead of a production
;; one added just for this.  Little-endian, mirroring GRAPH-DB::PACK-UINT.
(defun %pack-uint-to-mmap (mf offset value nbytes)
  (dotimes (i nbytes)
    (graph-db::set-byte mf (+ offset i) (ldb (byte 8 (* i 8)) value)))
  (+ offset nbytes))

;; Declared once at load time, its own graph name -- this file loads before
;; graph-tests.lisp (see graph-db.asd), so it cannot depend on
;; *INTEGRATION-GRAPH-NAME*'s schema.  Two vertex types (so a mark that skips
;; every OTHER type-id would still be caught) plus one edge type (the
;; edge-index half of the GC fix, gc.lisp's MAP-IDX call on EDGE-INDEX, is
;; otherwise untested).  Start from a clean slate so reloading this file
;; doesn't register the type metadata more than once (graph-tests.lisp:13
;; and mvcc-tests.lisp:19 do the same for their own graph names).
(eval-when (:load-toplevel :execute)
  (setf (gethash :ti-gc-reopen-test *schema-node-metadata*) nil))
(def-vertex ti-gc-thing-a () ((label :type string)) :ti-gc-reopen-test)
(def-vertex ti-gc-thing-b () ((label :type string)) :ti-gc-reopen-test)
(def-edge ti-gc-link () () :ti-gc-reopen-test)

;; Schema for the v2 -> v3 MIGRATE-GRAPH test below.  Same load-order
;; constraint as TI-GC-*: this file loads before graph-tests.lisp, so the
;; migration test cannot reuse G-PERSON/G-KNOWS -- it needs its own classes,
;; matching tests/fixtures/v2-graph.tar.gz (built with these exact names).
(eval-when (:load-toplevel :execute)
  (setf (gethash :ti-migration-fixture *schema-node-metadata*) nil))
(def-vertex ti-mig-person () ((name :type string) (age)) :ti-migration-fixture)
(def-vertex ti-mig-employee (ti-mig-person) ((title)) :ti-migration-fixture)
(def-edge ti-mig-knows () ((since)) :ti-migration-fixture)
(def-edge ti-mig-likes () () :ti-migration-fixture)

(test node-head-is-33-bytes
  (is (= 33 graph-db::+node-header-size+)))

(test ve-key-is-20-bytes
  (is (= 20 graph-db::+ve-key-bytes+)))

(test node-head-round-trips-a-type-id-above-16-bits
  ;; The whole point: 70000 does not fit in the old 2-byte field.
  (with-temp-directory (dir)
    (let* ((path (merge-pathnames "head.dat" dir))
           (mf (graph-db::mmap-file (namestring path) :size 128))
           (v (graph-db::%make-vertex :type-id 70000 :revision 7
                                      :data-pointer 12345)))
      (unwind-protect
           (progn
             (graph-db::serialize-node-head mf v 0)
             ;; Pin the v3 head's on-disk type-id byte order: nothing else in
             ;; the tree does.  Little-endian, matching PACK-UINT's existing
             ;; convention for revision/data-pointer/etc.  70000 = #x11170.
             (is (equal '(#x70 #x11 #x01 #x00)
                        (list (graph-db::get-byte mf 1)
                              (graph-db::get-byte mf 2)
                              (graph-db::get-byte mf 3)
                              (graph-db::get-byte mf 4))))
             (multiple-value-bind (d w h ti vw ve vev type-id revision)
                 (graph-db::deserialize-node-head mf 0)
               (declare (ignore d w h ti vw ve vev))
               (is (= 70000 type-id))
               (is (= 7 revision))))
        (graph-db::munmap-file mf)))))

(test ve-key-round-trips-a-type-id-above-16-bits
  (let* ((k (graph-db::make-ve-key :id (graph-db::gen-vertex-id)
                                   :type-id 70000))
         (vec (graph-db::serialize-ve-key k))
         (back (graph-db::deserialize-ve-key vec)))
    (is (= 70000 (graph-db::ve-key-type-id back)))))

(test ve-key-type-id-stays-big-endian
  ;; Convention, not a requirement -- the ve-index is a hash table, so these
  ;; bytes are only compared for equality.  Pinned so the convention is not
  ;; lost by accident.
  (let* ((k (graph-db::make-ve-key :id (graph-db::gen-vertex-id)
                                   :type-id #x01020304))
         (vec (graph-db::serialize-ve-key k)))
    (is (equal '(#x01 #x02 #x03 #x04)
               (list (aref vec 16) (aref vec 17)
                     (aref vec 18) (aref vec 19))))))

(test vev-key-round-trips-a-type-id-above-16-bits
  ;; The vev-index carries its own type-id.  Omitting it truncates silently.
  (let* ((k (graph-db::make-vev-key :out-id (graph-db::gen-vertex-id)
                                    :in-id (graph-db::gen-vertex-id)
                                    :type-id 70000))
         (vec (graph-db::serialize-vev-key k))
         (back (graph-db::deserialize-vev-key vec)))
    (is (= 70000 (graph-db::vev-key-type-id back)))))

(test vev-key-type-id-stays-big-endian
  ;; Mirrors VE-KEY-TYPE-ID-STAYS-BIG-ENDIAN: convention, not a requirement.
  (let* ((k (graph-db::make-vev-key :out-id (graph-db::gen-vertex-id)
                                    :in-id (graph-db::gen-vertex-id)
                                    :type-id #x01020304))
         (vec (graph-db::serialize-vev-key k)))
    (is (equal '(#x01 #x02 #x03 #x04)
               (list (aref vec 32) (aref vec 33)
                     (aref vec 34) (aref vec 35))))))

(test ve-key-mmap-round-trips-a-type-id-above-16-bits
  ;; SERIALIZE-VE-KEY-MMAP / DESERIALIZE-VE-KEY-MMAP are what actually write
  ;; the on-disk ve-index lhash -- a separate code path, with independently
  ;; written offsets, from the vector forms above.  Nothing else in the tree
  ;; exercises the mmap pair at all.
  (with-temp-directory (dir)
    (let* ((path (merge-pathnames "ve.dat" dir))
           (mf (graph-db::mmap-file (namestring path) :size 128))
           (k (graph-db::make-ve-key :id (graph-db::gen-vertex-id)
                                     :type-id 70000)))
      (unwind-protect
           (progn
             (graph-db::serialize-ve-key-mmap mf k 0)
             (let ((back (graph-db::deserialize-ve-key-mmap mf 0)))
               (is (= 70000 (graph-db::ve-key-type-id back)))))
        (graph-db::munmap-file mf)))))

(test vev-key-mmap-round-trips-a-type-id-above-16-bits
  ;; See VE-KEY-MMAP-ROUND-TRIPS-A-TYPE-ID-ABOVE-16-BITS: same separate,
  ;; otherwise-untested code path, for the vev-index.
  (with-temp-directory (dir)
    (let* ((path (merge-pathnames "vev.dat" dir))
           (mf (graph-db::mmap-file (namestring path) :size 128))
           (k (graph-db::make-vev-key :out-id (graph-db::gen-vertex-id)
                                      :in-id (graph-db::gen-vertex-id)
                                      :type-id 70000)))
      (unwind-protect
           (progn
             (graph-db::serialize-vev-key-mmap mf k 0)
             (let ((back (graph-db::deserialize-vev-key-mmap mf 0)))
               (is (= 70000 (graph-db::vev-key-type-id back)))))
        (graph-db::munmap-file mf)))))

(test key-width-constants-match-their-buffers
  ;; buffer-pool.lisp pre-allocates by size; 18 was ve-key and 34 vev-key.
  (is (= 20 graph-db::+ve-key-bytes+))
  (is (= 36 graph-db::+vev-key-bytes+)))

(test v2-legacy-reader-still-reads-a-31-byte-head
  ;; Migration depends on this: the OLD layout must remain readable.
  (with-temp-directory (dir)
    (let* ((path (merge-pathnames "v2.dat" dir))
           (mf (graph-db::mmap-file (namestring path) :size 128)))
      (unwind-protect
           (let ((i 0))
             ;; Hand-write a v2 head: flags(1) type-id(2 LE) revision(4)
             ;; data-pointer(8) commit-epoch(8) prev-pointer(8).
             (graph-db::set-byte mf 0 0)
             (setf i 1)
             (setf i (%pack-uint-to-mmap mf i 513 2))
             (setf i (%pack-uint-to-mmap mf i 9 4))
             (setf i (%pack-uint-to-mmap mf i 4096 8))
             (setf i (%pack-uint-to-mmap mf i 77 8))
             (%pack-uint-to-mmap mf i 0 8)
             (multiple-value-bind
                   (d w h ti vw ve vev type-id revision ptr epoch prev offset)
                 (graph-db::deserialize-node-head-v2 mf 0)
               (declare (ignore d w h ti vw ve vev prev))
               (is (= 513 type-id))
               (is (= 9 revision))
               (is (= 4096 ptr))
               (is (= 77 epoch))
               ;; The edge codec resumes from here when the reader is rebound
               ;; to the v2 (31-byte) legacy shape; wrong, and a rebound edge
               ;; read positions from/to/weight into the wrong bytes.
               (is (= 30 offset))))
        (graph-db::munmap-file mf)))))

(test the-registry-can-assign-a-type-id-above-16-bits
  "Assignment moved from the per-graph counter to the system registry (#186);
the id it hands out must still span the full 32-bit type-id field."
  (with-temp-directory (dir)
    ;; Seed through the FILE, not the struct slot: REGISTRY-INTERN re-reads
    ;; under its lock and recomputes the next id from what is on disk, so a
    ;; slot poked in memory would be discarded before the assignment.
    (with-open-file (out (merge-pathnames "type-registry.log" dir)
                         :direction :output :if-does-not-exist :create)
      (let ((*package* (find-package :keyword))
            (*print-pretty* nil))
        (format out "~S~%" (list :symbol 'tiw-seed :parent :vertex
                                 :id 69999))))
    (let ((r (graph-db::open-type-registry (namestring dir))))
      (unwind-protect
           (progn
             (is (= 69999 (graph-db::registry-id-for r 'tiw-seed :vertex))
                 "a persisted id above 16 bits reads back intact")
             (is (= 70000 (graph-db::registry-intern r 'tiw-wide :vertex))
                 "and assignment continues above it"))
        (graph-db::close-type-registry r)))))

(test prev-pointer-offset-lands-on-prev-pointer
  ;; The reaper patches this window in place; a wrong offset is invisible for
  ;; realistic values (real heap addresses and epochs are far below 2^48,
  ;; so offset 23 -- the pre-#166 value -- and offset 25 zero the SAME bytes
  ;; for every value the rest of the suite generates).  All-ones fields pin
  ;; it for real.
  (let ((v (graph-db::%make-vertex :type-id 70000 :revision 7))
        (buf (graph-db::make-byte-vector graph-db::+node-header-size+)))
    (setf (graph-db::data-pointer v) #xFFFFFFFFFFFFFFFF
          (graph-db::commit-epoch v) #xFFFFFFFFFFFFFFFF
          (graph-db::prev-pointer v) #xFFFFFFFFFFFFFFFF)
    (graph-db::serialize-node-head buf v 0)
    (dotimes (i 8)
      (graph-db::set-byte buf (+ graph-db::+node-prev-pointer-offset+ i) 0))
    (multiple-value-bind (d w h ti vw ve vev tid rev ptr epoch prev)
        (graph-db::deserialize-node-head buf 0)
      (declare (ignore d w h ti vw ve vev))
      (is (= 0 prev))
      (is (= #xFFFFFFFFFFFFFFFF epoch))
      (is (= #xFFFFFFFFFFFFFFFF ptr))
      (is (= 70000 tid))
      (is (= 7 rev)))))

(test opening-a-v2-versioned-graph-signals
  ;; Closing the storage-version hole: OPEN-LHASH restores key-bytes /
  ;; value-bytes from config.dat with no validation against the compile-time
  ;; +VE-KEY-BYTES+ / +NODE-HEADER-SIZE+ (#166 review).  Nothing downstream
  ;; of OPEN-MEMORY's version gate on heap.dat catches a v2 graph, so the
  ;; gate itself -- +STORAGE-VERSION+ bumped to 3 -- must refuse it, not
  ;; open-graph and misread every record instead.
  (with-temp-directory (dir)
    (let ((loc (namestring dir)))
      (graph-db::close-graph
       (graph-db::make-graph :storage-version-gate-test loc
                             :buffer-pool-p nil)
       :snapshot-p nil)
      ;; Patch heap.dat's stamped byte back to 2, simulating a graph written
      ;; before this build.
      (let ((mf (graph-db::mmap-file
                 (namestring (merge-pathnames "heap.dat" dir))
                 :create-p nil)))
        (unwind-protect
             (graph-db::set-byte
              mf graph-db::+memory-storage-version-offset+ 2)
          (graph-db::munmap-file mf)))
      (signals error
        (graph-db::open-graph :storage-version-gate-test loc
                              :buffer-pool-p nil :gc-heap-p nil)))))

(test v2-refusal-names-versions-and-points-at-migrate-graph
  ;; The gate signalling is not enough on its own -- an operator meeting this
  ;; needs to learn what to do from the error alone.  Pin the message content,
  ;; not just SIGNALS ERROR: it must name the version FOUND (2), the version
  ;; EXPECTED (3), and point at MIGRATE-GRAPH.
  (with-temp-directory (dir)
    (let ((loc (namestring dir)))
      (graph-db::close-graph
       (graph-db::make-graph :storage-version-gate-message-test loc
                             :buffer-pool-p nil)
       :snapshot-p nil)
      (let ((mf (graph-db::mmap-file
                 (namestring (merge-pathnames "heap.dat" dir))
                 :create-p nil)))
        (unwind-protect
             (graph-db::set-byte
              mf graph-db::+memory-storage-version-offset+ 2)
          (graph-db::munmap-file mf)))
      (handler-case
          (progn
            (graph-db::open-graph :storage-version-gate-message-test loc
                                  :buffer-pool-p nil :gc-heap-p nil)
            (fail "expected OPEN-GRAPH to signal on a v2 graph"))
        (error (c)
          (let ((msg (princ-to-string c)))
            (is (search "v2" msg) "message names the version found: ~A" msg)
            (is (search "v3" msg) "message names the version expected: ~A" msg)
            (is (search "MIGRATE-GRAPH" msg)
                "message points at the fix: ~A" msg)))))))

;;; ---------------------------------------------------------------------------
;;; v2 -> v3 migration (MIGRATE-GRAPH; type-id widened 2 -> 4 bytes, #166 T3)
;;;
;;; tests/fixtures/v2-graph.tar.gz is a pristine v2 graph (storage-version 2,
;;; 31-byte head, 2-byte type-id) built on 3d0e2b4 -- the commit immediately
;;; before the widening -- with the TI-MIG-* schema declared above: 12
;;; vertices (10 TI-MIG-PERSON, 2 TI-MIG-EMPLOYEE) chained by 5 TI-MIG-KNOWS
;;; edges plus 1 TI-MIG-LIKES = 18 records, deliberately more than
;;; *RESTORE-OBJECTS-PER-TRANSACTION* (10): a replay that silently drops its
;;; last batch loses data a smaller fixture could not expose (the boundary
;;; falls mid-vertex-list, so a dropped batch loses both vertices AND every
;;; edge).  Mirrors tests/mvcc-tests.lisp's v1 -> v3 fixture and test shape
;;; (see MIGRATE-V1-GRAPH-TO-V3); the heap is a 1 GB sparse file, so it
;;; ships tar+gzipped.
;;; ---------------------------------------------------------------------------

(defun extract-v2-fixture (dest)
  "Extract the committed v2 graph fixture into DEST (created if needed);
return DEST as a string."
  (let ((tarball (asdf:system-relative-pathname
                  :graph-db/test "tests/fixtures/v2-graph.tar.gz")))
    (ensure-directories-exist dest)
    (uiop:run-program (list "tar" "xzf" (namestring tarball)
                            "-C" (namestring dest))
                      :output t :error-output t)
    (namestring dest)))

(defun %data-fingerprint (dir)
  "A sorted SHA256 listing of every file under DIR's heap and node/index
tables -- used to assert MIGRATE-GRAPH leaves OLD-LOCATION's DATA untouched.
Excludes schema.dat and tx/: an ordinary open rewrites schema.dat
(UPDATE-SCHEMA calls SAVE-SCHEMA on every open; same content, re-serialized)
and creates a new empty tx/replication-*.log file (the transaction manager
always opens one).  No open in these tests rewrites schema.dat any more --
MIGRATE-GRAPH suppresses the replay for both of its own, and %READ-V2-GRAPH's
guard open is frozen (#186) -- but the exclusion stays: it names what an
ORDINARY open does, which is what the assertion is defending against.
Neither is data loss; both were confirmed to be the ONLY two things that
change, by exhaustive diff during the #166 migration work.  Shells out (find +
sha256sum), the same portability boundary EXTRACT-V1-FIXTURE already accepts
by shelling out to tar."
  (uiop:run-program
   (format nil "find ~A -type f -not -name schema.dat -not -path '*/tx/*' ~
-print0 | sort -z | xargs -0 sha256sum"
           (uiop:escape-sh-token (namestring (truename dir))))
   :output '(:string :stripped t) :force-shell t))

(defun %read-v2-graph (dir)
  "Open the v2 graph at DIR read-only (the same head shim MIGRATE-GRAPH
uses) and return (values vertices knows likes) via %FIXTURE-VERTICES /
%FIXTURE-EDGES.  Used both to capture MIGRATE-GRAPH's expected input and to
confirm, post-migration, that OLD-LOCATION still opens at v2 and still holds
every node -- the guarantee that actually backs the rollback story.

FROZEN: a v2 store's ids are its own and this system's registry has no
opinion on them, so an ordinary open would reconcile -- and refuse as soon as
a caller has primed the registry (GH #186).  Reading a store the registry
disagrees with is exactly what WITH-SCHEMA-FROZEN is for."
  (let ((graph-db::*node-head-reader* 'graph-db::deserialize-node-head-v2))
    (let ((g (graph-db:with-schema-frozen ()
               (graph-db:open-graph :ti-mig-guard dir
                                    :accept-versions '(2)
                                    :gc-heap-p nil :buffer-pool-p nil))))
      (unwind-protect
           (let ((graph-db::*graph* g))
             (values (%fixture-vertices g)
                     (%fixture-edges g 'ti-mig-knows)
                     (%fixture-edges g 'ti-mig-likes)))
        (graph-db:close-graph g :snapshot-p nil)))))

(defun %fixture-vertices (graph)
  "(id revision name age title-or-nil) for every live TI-MIG-PERSON in GRAPH,
sorted by name -- the pre- and post-migration comparison shape."
  (sort (graph-db::map-vertices
         (lambda (v)
           (list (graph-db::id v) (graph-db::revision v)
                 (slot-value v 'name) (slot-value v 'age)
                 (when (typep v 'ti-mig-employee)
                   (slot-value v 'title))))
         graph :collect-p t :vertex-type 'ti-mig-person)
        #'string< :key (lambda (row) (third row))))

(defun %fixture-edges (graph type)
  "(from to weight since-or-nil) for every live edge of TYPE in GRAPH, sorted
by SINCE (edges with no SINCE slot -- ti-mig-likes -- sort first)."
  (sort (graph-db::map-edges
         (lambda (e)
           (list (graph-db::from e) (graph-db::to e) (graph-db::weight e)
                 (when (eq type 'ti-mig-knows)
                   (slot-value e 'since))))
         graph :collect-p t :edge-type type)
        #'< :key (lambda (row) (or (fourth row) -1))))

(test migrate-v2-graph-to-v3-without-renumbering
  "A v2 (31-byte head, 2-byte type-id) graph cannot be opened directly by v3
code but MIGRATE-GRAPH carries it across (logical snapshot + replay),
preserving every node's id, revision, type, slot values and type-id, and
leaving OLD-LOCATION's data untouched and the source itself still openable
at v2 afterward -- see %DATA-FINGERPRINT for the bookkeeping file (tx/*)
that DOES change and why that is not data loss.

Pins the DEFAULT mode, :RENUMBER-P NIL, which is passed explicitly below.
The type-id half of this guarantee became mode-dependent at #186 (spec
§10.1): under :RENUMBER-P T every id is taken from the system registry
instead, which is the exact reverse.  See the seeding suite for that mode's
counterpart tests."
  #+ecl
  (skip "v2 fixture was cl-store'd by SBCL; ECL's cl-store cannot restore it ~
(graph on-disk dirs are not portable across Lisp implementations).")
  #-ecl
  (with-temp-directory (root)
    (let ((old-dir (extract-v2-fixture (merge-pathnames "v2/" root)))
          (new-dir (namestring (merge-pathnames "v3/" root)))
          (knows 'ti-mig-knows)
          (likes 'ti-mig-likes))
      ;; v3 code refuses to open the v2 graph directly (the format gate).
      (signals error (graph-db:open-graph :ti-mig-guard old-dir
                                          :buffer-pool-p nil :gc-heap-p nil))
      ;; Read the v2 graph's data directly (same shim MIGRATE-GRAPH uses),
      ;; WITHOUT migrating through it, so "expected" comes from the source,
      ;; not from the code under test.
      (let ((before (%data-fingerprint old-dir)))
        (multiple-value-bind (expected-vertices expected-knows expected-likes)
            (%read-v2-graph old-dir)
          (let (expected-type-ids)
            (let ((graph-db::*node-head-reader*
                   'graph-db::deserialize-node-head-v2))
              ;; FROZEN, like %READ-V2-GRAPH: reading a v2 source's own
              ;; type-ids is exactly the read an ordinary open refuses once
              ;; the registry has an opinion about those ids (GH #186).
              (let ((old (graph-db:with-schema-frozen ()
                           (graph-db:open-graph :ti-mig-guard old-dir
                                                :accept-versions '(2)
                                                :gc-heap-p nil
                                                :buffer-pool-p nil))))
                (unwind-protect
                     (setq expected-type-ids
                           (mapcar
                            (lambda (pair)
                              (cons (car pair)
                                    (graph-db::node-type-id
                                     (graph-db::lookup-node-type-by-name
                                      (car pair) (cdr pair) :graph old))))
                            '((ti-mig-person . :vertex)
                              (ti-mig-employee . :vertex)
                              (ti-mig-knows . :edge)
                              (ti-mig-likes . :edge))))
                  (graph-db:close-graph old :snapshot-p nil))))
            (is (= 12 (length expected-vertices))
                "fixture sanity: 12 people expected before migration")
            (is (= 5 (length expected-knows)) "fixture sanity: 5 knows edges")
            (is (= 1 (length expected-likes)) "fixture sanity: 1 likes edge")
            (is (< 10 (+ (length expected-vertices) (length expected-knows)
                        (length expected-likes)))
                "fixture sanity: 18 records must exceed one replay batch ~
(10), or a dropped-last-batch bug below would go undetected")
            ;; ...but MIGRATE-GRAPH brings it forward to v3.
            (let ((g (graph-db::migrate-graph
                      :ti-migration-fixture old-dir new-dir
                      :package :graph-db/test
                      :renumber-p nil
                      :snapshot-file
                      (namestring
                       (merge-pathnames "migrate.snapshot" root)))))
              (unwind-protect
                   (let ((graph-db::*graph* g))
                     (is (= 3 graph-db::+storage-version+)
                         "migrated graph is written in the current (v3) ~
format")
                     ;; Every node: id, revision, type and every slot value
                     ;; intact.  (Nearest wrong implementation this rejects:
                     ;; one that drops the last replay batch -- the length
                     ;; check fails outright, not silently under-compares two
                     ;; equally-short lists; and one that preserves ids but
                     ;; loses a slot -- EQUALP compares NAME/AGE/TITLE
                     ;; positionally, not just presence.)
                     (is (equalp expected-vertices (%fixture-vertices g))
                         "vertices: id+revision+name+age+title must match ~
exactly")
                     (is (equalp expected-knows (%fixture-edges g knows))
                         "knows edges: from+to+weight+since must match ~
exactly")
                     (is (equalp expected-likes (%fixture-edges g likes))
                         "likes edges: from+to+weight must match exactly")
                     ;; Type-ids preserved, because :RENUMBER-P is NIL.
                     ;; The other mode is asserted in the seeding suite
                     ;; (#186, spec §10.1).
                     (dolist (expected expected-type-ids)
                       (let* ((name (car expected))
                              (parent (if (member name '(ti-mig-person
                                                         ti-mig-employee))
                                          :vertex :edge))
                              (actual (graph-db::node-type-id
                                       (graph-db::lookup-node-type-by-name
                                        name parent :graph g))))
                         (is (= (cdr expected) actual)
                             "~A's type-id must survive a :RENUMBER-P NIL ~
migration unchanged (expected ~A, got ~A)"
                             name (cdr expected) actual))))
                (graph-db:close-graph g)))
            ;; OLD-LOCATION's DATA is untouched -- scoped past schema.dat and
            ;; tx/, the two bookkeeping files any OPEN rewrites/creates (see
            ;; %DATA-FINGERPRINT).  (Nearest wrong implementation this
            ;; rejects: one that "succeeds" by touching nothing, i.e. an
            ;; empty/no-op migration -- caught above by the literal
            ;; vertex/edge counts, not by this check alone.)
            (is (equal before (%data-fingerprint old-dir))
                "OLD-LOCATION's heap and node/index tables must be ~
unchanged by MIGRATE-GRAPH")
            ;; The actual rollback guarantee: the source still opens at v2
            ;; and still holds every node, after MIGRATE-GRAPH has run.
            (multiple-value-bind (v k l) (%read-v2-graph old-dir)
              (is (equalp expected-vertices v)
                  "OLD-LOCATION must still open at v2 with every vertex ~
intact after migration")
              (is (equalp expected-knows k)
                  "OLD-LOCATION must still open at v2 with every knows ~
edge intact after migration")
              (is (equalp expected-likes l)
                  "OLD-LOCATION must still open at v2 with every likes ~
edge intact after migration"))))))))

;;; ---------------------------------------------------------------------------
;;; Task 2 (#166): type-index no longer preallocates the whole id space, and
;;; +MAX-NODE-TYPES+ stops being a ceiling GET-NEXT-TYPE-ID can outrun.
;;; ---------------------------------------------------------------------------

(test lookup-node-type-by-id-accepts-a-type-id-above-16-bits
  ;; #166 review: GET-NEXT-TYPE-ID has no ceiling and can hand out any
  ;; (unsigned-byte 32) id, but LOOKUP-NODE-TYPE-BY-ID's assert used to cap at
  ;; the old 16-bit +MAX-NODE-TYPES+ (65536) -- "can assign, cannot read
  ;; back."  +MAX-NODE-TYPES+ now matches the id field's full width, so a
  ;; lookup anywhere in (UNSIGNED-BYTE 32) must not trip the assert (a MISS is
  ;; fine; an assertion error is not).
  (with-test-graph (g)
    (finishes (graph-db::lookup-node-type-by-id 4294967295 :vertex :graph g))
    (finishes (graph-db::lookup-node-type-by-id 70000 :vertex :graph g))))

(test type-index-does-not-preallocate-the-whole-space
  ;; The file is sized for the types in use, not for the id space.
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "ti.dat" dir)))
           (heap (graph-db::create-memory
                  (namestring (merge-pathnames "h.dat" dir)) (* 1024 1024))))
      (unwind-protect
           (let ((idx (graph-db::make-type-index path heap)))
             (unwind-protect
                  (is (< (with-open-file (s path :element-type
                                               '(unsigned-byte 8))
                           (file-length s))
                         (* 1024 1024)))   ; well under the old ~1.1 MB
               (graph-db::close-type-index idx)))
        (graph-db::close-memory heap)))))

(test type-index-grows-for-a-large-type-id
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "ti2.dat" dir)))
           (heap (graph-db::create-memory
                  (namestring (merge-pathnames "h2.dat" dir)) (* 1024 1024)))
           (id (graph-db::gen-vertex-id)))
      (unwind-protect
           (let ((idx (graph-db::make-type-index path heap)))
             (unwind-protect
                  (progn
                    (graph-db::type-index-push id 70000 idx)
                    (is (graph-db::index-list-member-p
                         id (graph-db::get-type-index-list idx 70000))))
               (graph-db::close-type-index idx)))
        (graph-db::close-memory heap)))))

(test type-index-locks-are-bounded
  (is (<= graph-db::+type-index-lock-stripes+ 1024)))

(test type-index-grown-slot-survives-close-and-reopen
  ;; The nearest wrong implementation: grow the FILE but leave the in-memory
  ;; CAPACITY (or the cache) stale.  That would still pass a naive push/get
  ;; round-trip against the live IDX, since the live struct's cache already
  ;; has the entry -- only a fresh OPEN-TYPE-INDEX, which recomputes capacity
  ;; from the file as it actually is on disk, catches a capacity that was
  ;; never persisted or a write that never reached the mmap.
  ;;
  ;; SCOPE: this exercises TYPE-INDEX in isolation, talking to it directly.
  ;; It does NOT cover GC-HEAP, which walks the type-index cache through a
  ;; SEPARATE reader (MAP-TYPE-INDEX-LIST-ADDRESSES, gc.lisp) that this test
  ;; never calls -- see TI-GC-THING-SURVIVES-A-GC-ON-REOPEN below for that
  ;; path, which is where a real regression of this kind actually surfaced.
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "ti3.dat" dir)))
           (heap (graph-db::create-memory
                  (namestring (merge-pathnames "h3.dat" dir)) (* 1024 1024)))
           (id (graph-db::gen-vertex-id)))
      (unwind-protect
           (progn
             (let ((idx (graph-db::make-type-index path heap)))
               (graph-db::type-index-push id 70000 idx)
               (graph-db::close-type-index idx))
             (let ((idx (graph-db::open-type-index path heap)))
               (unwind-protect
                    (is (graph-db::index-list-member-p
                         id (graph-db::get-type-index-list idx 70000)))
                 (graph-db::close-type-index idx))))
        (graph-db::close-memory heap)))))

(test ti-gc-thing-survives-a-gc-on-reopen
  ;; #166 regression: OPEN-GRAPH's default :GC-HEAP-P T runs a mark-and-sweep
  ;; over the heap (gc.lisp) whose mark phase walks the type-index through
  ;; MAP-TYPE-INDEX-LIST-ADDRESSES.  That function used to MAPHASH the
  ;; type-index's cache directly, which was safe only while the cache was
  ;; fully populated at open (the pre-#166 eager loop this task removed).
  ;; With a lazy cache, a type never touched THIS session -- e.g. right after
  ;; reopen, before any scan -- mapped to an EMPTY set of marked addresses,
  ;; so GC-HEAP swept that type's still-live node data as garbage.  A
  ;; type-index-only round-trip (the test above) cannot see this: it never
  ;; calls GC-HEAP.  This is the one that actually caught the regression.
  ;;
  ;; TWO vertex types plus ONE edge type, one instance each: a single type
  ;; would not catch "enumerates most types but misses one" (e.g. a mark
  ;; skipping every even type-id), and gc.lisp's MAP-IDX runs once for
  ;; EDGE-INDEX and once for VERTEX-INDEX -- an edge-blind fix would leave
  ;; TI-GC-LINK's data silently swept while every vertex test stayed green.
  (with-temp-directory (dir)
    (let ((g (make-graph :ti-gc-reopen-test (namestring dir)
                         :buffer-pool-size 1000)))
      (let ((*graph* g))
        (with-transaction ()
          (let ((va (make-ti-gc-thing-a :label "A-SURVIVED"))
                (vb (make-ti-gc-thing-b :label "B-SURVIVED")))
            (make-ti-gc-link :from va :to vb))))
      (close-graph g :snapshot-p nil))
    (let ((g (open-graph :ti-gc-reopen-test (namestring dir))))
      (unwind-protect
           (let ((seen-a '()) (seen-b '()) (seen-links '()))
             (map-vertices (lambda (v) (push (slot-value v 'label) seen-a))
                           g :vertex-type 'ti-gc-thing-a)
             (map-vertices (lambda (v) (push (slot-value v 'label) seen-b))
                           g :vertex-type 'ti-gc-thing-b)
             (map-edges (lambda (e) (push (id e) seen-links))
                       g :edge-type 'ti-gc-link)
             (is (equal '("A-SURVIVED") seen-a)
                 "type A's vertex must survive the default GC-HEAP-P T scan")
             (is (equal '("B-SURVIVED") seen-b)
                 "type B's vertex must survive the default GC-HEAP-P T scan")
             (is (= 1 (length seen-links))
                 "the edge must survive the default GC-HEAP-P T scan"))
        (close-graph g :snapshot-p nil)
        (collect-garbage)))))

(test type-index-colliding-stripe-types-stay-isolated
  ;; Two type-ids that land on the SAME lock stripe must still be isolated on
  ;; disk.  The nearest wrong implementation confuses the STRIPE index with
  ;; the TYPE-ID when computing the mmap offset (both would look identical
  ;; for any type-id < +TYPE-INDEX-LOCK-STRIPES+, which the smaller tests
  ;; above never exceed) -- that bug would alias these two types' index-lists
  ;; onto the same on-disk slot.
  (with-temp-type-index (idx heap)
    (let ((a (gen-id)) (b (gen-id))
          (type-a 1)
          (type-b (+ 1 graph-db::+type-index-lock-stripes+)))
      (is (= (mod type-a graph-db::+type-index-lock-stripes+)
             (mod type-b graph-db::+type-index-lock-stripes+))
          "test setup: TYPE-A and TYPE-B must actually share a stripe")
      (type-index-push a type-a idx)
      (type-index-push b type-b idx)
      (is-true (key-in-list-p a (get-type-index-list idx type-a)))
      (is-true (key-in-list-p b (get-type-index-list idx type-b)))
      (is-false (key-in-list-p a (get-type-index-list idx type-b)))
      (is-false (key-in-list-p b (get-type-index-list idx type-a))))))

(test type-index-concurrent-push-to-one-type-id-loses-nothing
  ;; Two threads push DISTINCT ids into the SAME type-id's index-list at
  ;; once -- the one shared mutable resource TYPE-INDEX-PUSH's lock actually
  ;; has to protect.  INDEX-LIST-PUSH's head update
  ;; (index-list.lisp:116-119) is `(sb-ext:cas (index-list-head il)
  ;; (index-list-head il) address)` -- the "expected" argument is a SECOND
  ;; read of the same place, so it is an unconditional store, not a real
  ;; compare-and-swap.  Unserialized, two concurrent pushes race to read the
  ;; old head, and the loser's write clobbers the winner's: one push is
  ;; silently dropped.  (An earlier version of this test pushed to two
  ;; DIFFERENT type-ids sharing a lock stripe -- disjoint index-list objects
  ;; and disjoint mmap offsets, so it proved nothing about locking at all: a
  ;; constant stripe function, an off-by-one stripe function, or no lock
  ;; whatsoever would all have passed it just the same.)
  (with-temp-type-index (idx heap)
    (let* ((n 200)
           (type-id 1)
           (ids-a (loop repeat n collect (gen-id)))
           (ids-b (loop repeat n collect (gen-id)))
           (err nil))
      (flet ((push-all (ids)
               (lambda ()
                 (handler-case
                     (dolist (id ids) (type-index-push id type-id idx))
                   (error (e) (setf err e))))))
        (let ((ta (bordeaux-threads:make-thread (push-all ids-a)
                                                 :name "ti-push-a"))
              (tb (bordeaux-threads:make-thread (push-all ids-b)
                                                 :name "ti-push-b")))
          (bordeaux-threads:join-thread ta)
          (bordeaux-threads:join-thread tb)))
      (is (null err) "a pushing thread signaled: ~A" err)
      (let ((listed (index-list-keys (get-type-index-list idx type-id))))
        (is (= (* 2 n) (length listed))
            "expected ~D ids (~D dropped by a lost-update race), got ~D"
            (* 2 n) (- (* 2 n) (length listed)) (length listed))
        (is (null (set-exclusive-or (append ids-a ids-b) listed
                                    :test #'equalp))
            "the pushed ids and the surviving ids must be the same set")))))

;;; ---------------------------------------------------------------------------
;;; GC mark-phase type-id enumeration under registry-sparse ids (GH #194)
;;; ---------------------------------------------------------------------------

;; Its own graph name, same reload discipline as TI-GC-REOPEN-TEST above.
;; THREE vertex types plus one edge type: dropping any single id from the
;; enumeration must lose exactly one type's nodes.
(eval-when (:load-toplevel :execute)
  (setf (gethash :ti-gc-sparse-test *schema-node-metadata*) nil))
(def-vertex ti-sparse-a () ((label :type string)) :ti-gc-sparse-test)
(def-vertex ti-sparse-b () ((label :type string)) :ti-gc-sparse-test)
(def-vertex ti-sparse-c () ((label :type string)) :ti-gc-sparse-test)
(def-edge ti-sparse-link () () :ti-gc-sparse-test)

;; The ids this test pins.  Non-contiguous BY CONSTRUCTION -- the shape a
;; store in a many-store system actually has -- and placed by %REGISTRY-
;; ADOPT, the same sanctioned write RECONCILE-SCHEMA-WITH-REGISTRY uses to
;; record a store's pre-existing ids (GH #186, spec 10.1).
(defparameter *ti-sparse-ids*
  '((ti-sparse-a    :vertex 4001)
    (ti-sparse-b    :vertex 4013)
    (ti-sparse-c    :vertex 4057)
    (ti-sparse-link :edge   4007)))

(defun %adopt-sparse-test-ids ()
  "Place *TI-SPARSE-IDS* in the run's registry, idempotently.  Fails loudly
if some other type already holds one of the ids (would invalidate the
fixture, and %REGISTRY-ADOPT refuses it by contract)."
  (let ((r (graph-db::ensure-type-registry)))
    (graph-db::with-registry-append-lock (r)
      (loop for (sym parent id) in *ti-sparse-ids*
            do (let ((known (graph-db::registry-id-for r sym parent))
                     (holder (gethash id (graph-db::registry-ids-table
                                          r parent))))
                 (cond ((eql known id))   ; already placed (re-run)
                       (known
                        (error "~S already has id ~D, not ~D" sym known id))
                       ((and holder (not (eq holder sym)))
                        (error "id ~D is already held by ~S" id holder))
                       (t (graph-db::%registry-adopt r sym parent id))))))))

(test gc-mark-enumerates-every-sparse-type-id
  "GC-HEAP's mark phase must enumerate EVERY assigned type-id; a type-id
MAP-TYPE-INDEX-LIST-ADDRESSES misses has its nodes swept -- silent, total
loss of one type.  Broken twice: #166 (enumerated the lazily-populated
cache, empty on a fresh open) and #186 (DOTIMES against the schema
counter, missing registry-sparse ids).  Both were caught only by
incidental tests; this one holds the invariant by name, against a store
whose ids are sparse and non-contiguous (GH #194).  Trap: the sparse ids
are adopted into the RUN's shared registry, so they must stay clear of
ids other tests mint."
  (%adopt-sparse-test-ids)
  (with-temp-directory (dir)
    (let ((node-ids (make-hash-table :test 'eq)))
      (let ((g (make-graph :ti-gc-sparse-test (namestring dir)
                           :buffer-pool-size 1000)))
        ;; The fixture's premise: the store's ids really are the sparse
        ;; ones, not a dense mint.
        (loop for (sym parent id) in *ti-sparse-ids*
              do (is (eql id (graph-db::node-type-id
                              (graph-db::lookup-node-type-by-name
                               sym parent :graph g)))
                     "~S must hold sparse id ~D" sym id))
        (let ((*graph* g))
          (with-transaction ()
            (let ((va (make-ti-sparse-a :label "A-LIVES"))
                  (vb (make-ti-sparse-b :label "B-LIVES"))
                  (vc (make-ti-sparse-c :label "C-LIVES")))
              (setf (gethash 'ti-sparse-a node-ids) (id va)
                    (gethash 'ti-sparse-b node-ids) (id vb)
                    (gethash 'ti-sparse-c node-ids) (id vc)
                    (gethash 'ti-sparse-link node-ids)
                    (id (make-ti-sparse-link :from va :to vc))))))
        (close-graph g :snapshot-p nil))
      ;; Reopen fresh (lazy, unpopulated type-index cache -- the #166
      ;; shape; :GC-HEAP-P T is the default) and then GC again explicitly.
      (let ((g (open-graph :ti-gc-sparse-test (namestring dir))))
        (unwind-protect
             (let ((*graph* g))
               (graph-db::gc-heap g)
               ;; Every node's data block must still be ALLOCATED --
               ;; checked before touching data, since FREE threads its
               ;; free-list through the block.
               (let ((blocks (make-hash-table)))
                 (graph-db::map-memory
                  (lambda (addr size free-p)
                    (declare (ignore size))
                    (setf (gethash addr blocks) free-p))
                  (graph-db::heap g) :include-free-p t)
                 (flet ((alive-p (sym lookup)
                          (let* ((node (funcall lookup
                                                (gethash sym node-ids)))
                                 (dp (and node
                                          (graph-db::data-pointer node))))
                            (is-true node "~S: node must still look up"
                                     sym)
                            ;; a slotless node (the edge) has no data
                            ;; block at all -- data-pointer 0
                            (when (and dp (plusp dp))
                              (multiple-value-bind (free-p found-p)
                                  (gethash dp blocks)
                                (is-true found-p
                                         "~S: data block ~D must exist"
                                         sym dp)
                                (is-false free-p
                                          "~S: data block ~D was swept"
                                          sym dp))))))
                   (alive-p 'ti-sparse-a #'lookup-vertex)
                   (alive-p 'ti-sparse-b #'lookup-vertex)
                   (alive-p 'ti-sparse-c #'lookup-vertex)
                   (alive-p 'ti-sparse-link #'lookup-edge)))
               ;; And every node of every type is still reachable through
               ;; its type index, data intact.
               (flet ((labels-of (type)
                        (let (seen)
                          (map-vertices
                           (lambda (v)
                             (push (slot-value v 'label) seen))
                           g :vertex-type type)
                          seen)))
                 (is (equal '("A-LIVES") (labels-of 'ti-sparse-a)))
                 (is (equal '("B-LIVES") (labels-of 'ti-sparse-b)))
                 (is (equal '("C-LIVES") (labels-of 'ti-sparse-c))))
               (let (links)
                 (map-edges (lambda (e) (push (id e) links))
                            g :edge-type 'ti-sparse-link)
                 (is (= 1 (length links))
                     "the edge must survive both GC passes")))
          (close-graph g :snapshot-p nil)
          (collect-garbage))))))
