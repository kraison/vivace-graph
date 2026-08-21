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
;; *INTEGRATION-GRAPH-NAME*'s schema.
(def-vertex ti-gc-thing () ((label :type string)) :ti-gc-reopen-test)

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

(test schema-can-assign-a-type-id-above-16-bits
  (let ((s (graph-db::make-schema)))
    (setf (graph-db::schema-next-vertex-id s) 70000)
    (is (= 70000 (graph-db::get-next-type-id s :vertex)))
    (is (= 70001 (graph-db::schema-next-vertex-id s)))))

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
                  (namestring (merge-pathnames "h.dat" dir)) (* 1024 1024)))
           (idx (graph-db::make-type-index path heap)))
      (unwind-protect
           (is (< (with-open-file (s path :element-type '(unsigned-byte 8))
                    (file-length s))
                  (* 1024 1024)))          ; well under the old ~1.1 MB
        (graph-db::close-type-index idx)))))

(test type-index-grows-for-a-large-type-id
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "ti2.dat" dir)))
           (heap (graph-db::create-memory
                  (namestring (merge-pathnames "h2.dat" dir)) (* 1024 1024)))
           (idx (graph-db::make-type-index path heap))
           (id (graph-db::gen-vertex-id)))
      (unwind-protect
           (progn
             (graph-db::type-index-push id 70000 idx)
             (is (graph-db::index-list-member-p
                  id (graph-db::get-type-index-list idx 70000))))
        (graph-db::close-type-index idx)))))

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
      (let ((idx (graph-db::make-type-index path heap)))
        (graph-db::type-index-push id 70000 idx)
        (graph-db::close-type-index idx))
      (let ((idx (graph-db::open-type-index path heap)))
        (unwind-protect
             (is (graph-db::index-list-member-p
                  id (graph-db::get-type-index-list idx 70000)))
          (graph-db::close-type-index idx))))))

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
  (with-temp-directory (dir)
    (let ((g (make-graph :ti-gc-reopen-test (namestring dir)
                         :buffer-pool-size 1000)))
      (let ((*graph* g))
        (with-transaction () (make-ti-gc-thing :label "SURVIVED")))
      (close-graph g :snapshot-p nil))
    (let ((g (open-graph :ti-gc-reopen-test (namestring dir))))
      (unwind-protect
           (let ((seen '()))
             (map-vertices (lambda (v) (push (slot-value v 'label) seen))
                           g :vertex-type 'ti-gc-thing)
             (is (equal '("SURVIVED") seen)
                 "a vertex must survive OPEN-GRAPH's default GC-HEAP-P T scan"))
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

(test type-index-concurrent-push-to-colliding-stripe-types
  ;; Correctness under concurrency, not just under a single thread: two
  ;; type-ids sharing a stripe are pushed to from two threads at once.  A
  ;; stripe computed wrongly enough to under-serialize one of these types
  ;; (e.g. a bug that only sometimes maps a type-id to its stripe) would lose
  ;; pushes to a lost-update race on that type's index-list head; a stripe
  ;; computed wrongly the OTHER way (aliasing the two types' data, not just
  ;; their locks) would show up as cross-contamination between the two lists.
  (with-temp-type-index (idx heap)
    (let* ((n 200)
           (type-a 1)
           (type-b (+ 1 graph-db::+type-index-lock-stripes+))
           (ids-a (loop repeat n collect (gen-id)))
           (ids-b (loop repeat n collect (gen-id)))
           (err nil))
      (flet ((push-all (ids type-id)
               (lambda ()
                 (handler-case
                     (dolist (id ids) (type-index-push id type-id idx))
                   (error (e) (setf err e))))))
        (let ((ta (bordeaux-threads:make-thread (push-all ids-a type-a)
                                                 :name "ti-stripe-a"))
              (tb (bordeaux-threads:make-thread (push-all ids-b type-b)
                                                 :name "ti-stripe-b")))
          (bordeaux-threads:join-thread ta)
          (bordeaux-threads:join-thread tb)))
      (is (null err) "a pushing thread signaled: ~A" err)
      (let ((listed-a (index-list-keys (get-type-index-list idx type-a)))
            (listed-b (index-list-keys (get-type-index-list idx type-b))))
        (is (= n (length listed-a)))
        (is (= n (length listed-b)))
        (dolist (id ids-a) (is-true (member id listed-a :test #'equalp)))
        (dolist (id ids-b) (is-true (member id listed-b :test #'equalp)))))))
