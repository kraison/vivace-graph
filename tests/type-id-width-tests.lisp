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
                   (d w h ti vw ve vev type-id revision ptr epoch)
                 (graph-db::deserialize-node-head-v2 mf 0)
               (declare (ignore d w h ti vw ve vev))
               (is (= 513 type-id))
               (is (= 9 revision))
               (is (= 4096 ptr))
               (is (= 77 epoch))))
        (graph-db::munmap-file mf)))))

(test schema-can-assign-a-type-id-above-16-bits
  (let ((s (graph-db::make-schema)))
    (setf (graph-db::schema-next-vertex-id s) 70000)
    (is (= 70000 (graph-db::get-next-type-id s :vertex)))
    (is (= 70001 (graph-db::schema-next-vertex-id s)))))
