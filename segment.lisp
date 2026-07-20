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
