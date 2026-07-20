(in-package :graph-db)

;;; Vector segment: a derived, mmap-backed index holding one fixed-width
;;; single-float vector per node, addressable by node id.  See
;;; docs/superpowers/specs/2026-07-20-vector-segments-design.md sec 5.
;;;
;;; This file is the FILE FORMAT and its unit operations ONLY.  Transaction
;;; hooks, rebuild-from-nodes, and scan/score are later steps.
;;;
;;; The on-disk id array is authoritative.  ID->SLOT is a RAM-only hash rebuilt
;;; at open by sweeping it (sec 5.1); it is never persisted.

(defstruct (vector-segment (:constructor %make-vector-segment)
                           (:conc-name segment-)
                           (:predicate vector-segment-p))
  (mmap nil)                 ; a mapped-file (mmap.lisp)
  (dimension 0 :type fixnum) ; fixed at create time
  (id->slot nil))            ; equalp hash: 16-byte id vector -> slot index

(defun %seg-write-header (mmap &key magic format dimension element-type
                                    capacity live-count free-head)
  (serialize-uint64 mmap magic 0)
  (serialize-uint64 mmap format 8)
  (serialize-uint64 mmap dimension 16)
  (serialize-uint64 mmap element-type 24)
  (serialize-uint64 mmap capacity 32)
  (serialize-uint64 mmap live-count 40)
  (serialize-uint64 mmap free-head 48)
  (serialize-uint64 mmap 0 56))

(defun %seg-vblock-offset (capacity)
  "Byte offset of the vector block for CAPACITY slots."
  (+ +segment-id-array-offset+ (* capacity +key-bytes+)))

(defun %seg-file-bytes (capacity dimension)
  "Total bytes a segment file needs for CAPACITY slots of DIMENSION."
  (+ (%seg-vblock-offset capacity) (* capacity dimension 4)))

(defun create-vector-segment (path dimension &key (initial-capacity 1024))
  "Create a new vector segment at PATH holding DIMENSION-wide single-float
vectors, with room for INITIAL-CAPACITY slots.  DIMENSION is fixed for the life
of the segment.  Returns an open VECTOR-SEGMENT."
  (check-type dimension (integer 1))
  (check-type initial-capacity (integer 1))
  (let* ((bytes (%seg-file-bytes initial-capacity dimension))
         (mmap (mmap-file path :create-p t :size bytes)))
    (%seg-write-header mmap
                       :magic +segment-magic+
                       :format +segment-format+
                       :dimension dimension
                       :element-type +fv-single-float+
                       :capacity initial-capacity
                       :live-count 0
                       :free-head +no-slot+)
    (%make-vector-segment :mmap mmap
                          :dimension dimension
                          :id->slot (make-hash-table :test 'equalp))))

(defun segment-capacity (segment)
  (deserialize-uint64 (segment-mmap segment) 32))

(defun segment-live-count (segment)
  (deserialize-uint64 (segment-mmap segment) 40))

(defun %seg-free-head (segment)
  (deserialize-uint64 (segment-mmap segment) 48))

(defun open-vector-segment (path)
  "Open an existing vector segment at PATH.  Validates magic and format, reads
the header, and rebuilds the RAM id->slot map by sweeping the id array (the
on-disk id array is authoritative; the map is never persisted)."
  (let ((mmap (mmap-file path :create-p nil)))
    (let ((magic (deserialize-uint64 mmap 0))
          (format (deserialize-uint64 mmap 8)))
      (unless (= magic +segment-magic+)
        (error "~A is not a vector segment (magic ~X)" path magic))
      (unless (= format +segment-format+)
        (error "vector segment ~A is format ~D, expected ~D"
               path format +segment-format+)))
    (let ((segment (%make-vector-segment
                    :mmap mmap
                    :dimension (deserialize-uint64 mmap 16)
                    :id->slot (make-hash-table :test 'equalp))))
      (%seg-rebuild-id->slot segment)
      segment)))

(defun close-vector-segment (segment)
  "Release the segment's mmap."
  (when (segment-mmap segment)
    (munmap-file (segment-mmap segment))
    (setf (segment-mmap segment) nil))
  nil)

(defun %seg-rebuild-id->slot (segment)
  ;; Filled in in a later task; the header round-trip test does not exercise it.
  (declare (ignore segment))
  nil)

(defun %seg-id-offset (slot)
  (+ +segment-id-array-offset+ (* slot +key-bytes+)))

(defun %seg-vec-offset (segment slot)
  (+ (%seg-vblock-offset (segment-capacity segment))
     (* slot (segment-dimension segment) 4)))

(defun %seg-read-vector (segment slot)
  "Read slot SLOT's vector as a fresh (simple-array single-float (*))."
  (let* ((dim (segment-dimension segment))
         (off (%seg-vec-offset segment slot))
         (bytes (get-bytes (segment-mmap segment) off (* dim 4)))
         (v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v)
      (let ((bits 0) (b (* i 4)))
        (dotimes (k 4)
          (setf bits (dpb (aref bytes (+ b k)) (byte 8 (* k 8)) bits)))
        (setf (aref v i) (ieee-floats:decode-float32 bits))))))

(defun %seg-write-vector (segment slot vector)
  "Write VECTOR into slot SLOT's vector-block region."
  (declare (type (simple-array single-float (*)) vector))
  (let* ((dim (segment-dimension segment))
         (off (%seg-vec-offset segment slot))
         (bytes (make-array (* dim 4) :element-type '(unsigned-byte 8))))
    (dotimes (i dim)
      (let ((bits (ieee-floats:encode-float32 (aref vector i)))
            (b (* i 4)))
        (dotimes (k 4)
          (setf (aref bytes (+ b k)) (ldb (byte 8 (* k 8)) bits)))))
    (set-bytes (segment-mmap segment) bytes off (* dim 4))))

(defun %seg-write-id (segment slot id)
  ;; The free-list scheme marks a free cell by all-ones in its first 8 bytes, so
  ;; a real id whose first 8 bytes are all-ones would be misread as free after a
  ;; reopen (sec 5.1 rebuild).  Engine ids are uuids and never all-ones, but an
  ;; arbitrary caller-supplied id could be; reject it loudly rather than corrupt
  ;; silently.
  (let ((first8 0))
    (dotimes (k 8) (setf first8 (dpb (aref id k) (byte 8 (* k 8)) first8)))
    (when (= first8 +free-slot-marker+)
      (error "node id's first 8 bytes are all-ones, colliding with the segment ~
              free-slot marker")))
  (set-bytes (segment-mmap segment) id (%seg-id-offset slot) +key-bytes+))

(defun %seg-slot-of (segment id)
  "Slot index storing ID, or NIL."
  (gethash id (segment-id->slot segment)))

(defun %seg-claim-slot (segment)
  "Return a slot index to write a NEW id into: the free-list head if any, else
the next slot past live-count when capacity allows.  Signals when full -- Task 5
replaces this with growth."
  (let* ((mmap (segment-mmap segment))
         (free-head (%seg-free-head segment)))
    (if (/= free-head +no-slot+)
        ;; Pop the free list: its cell's second 8 bytes hold the next free slot.
        (let ((next (deserialize-uint64 mmap (+ (%seg-id-offset free-head) 8))))
          (serialize-uint64 mmap next 48)   ; free-head := next
          free-head)
        (let ((cap (segment-capacity segment))
              (live (segment-live-count segment)))
          (when (>= live cap)
            (error "segment full: capacity ~D (growth is Task 5)" cap))
          live))))

(defun segment-put (segment id vector)
  "Store VECTOR under the 16-byte ID.  Overwrites if ID is present; else takes a
free slot (or the next free index).  Returns the slot index.  VECTOR's length
must equal the segment's dimension, or this signals."
  (check-type vector (simple-array single-float (*)))
  (unless (= (length vector) (segment-dimension segment))
    (error "vector length ~D does not match segment dimension ~D"
           (length vector) (segment-dimension segment)))
  (let ((existing (%seg-slot-of segment id)))
    (if existing
        (progn (%seg-write-vector segment existing vector) existing)
        (let ((slot (%seg-claim-slot segment)))
          (%seg-write-id segment slot id)
          (%seg-write-vector segment slot vector)
          (setf (gethash id (segment-id->slot segment)) slot)
          (serialize-uint64 (segment-mmap segment)
                            (1+ (segment-live-count segment)) 40)
          slot))))

(defun segment-get (segment id)
  "The vector stored under ID as a fresh (simple-array single-float (*)), or NIL."
  (let ((slot (%seg-slot-of segment id)))
    (when slot (%seg-read-vector segment slot))))
