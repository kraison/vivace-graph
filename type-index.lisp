(in-package :graph-db)

(defstruct (type-index
             (:constructor %make-type-index)
             (:print-function
              (lambda (i s d)
                (declare (ignore d))
                (format s "#<TYPE-INDEX ~A" (type-index-table i)))))
  table
  ;; The allocator heap these index-lists live in.  Held so the lazy %TI-LIST
  ;; (#46) can deserialize without needing *GRAPH* bound (e.g. in unit tests).
  heap
  ;; Stripe locks (#166).  A mutex per type-id cost 65,536 of them per index
  ;; per store; 256 fixed stripes, selected by (MOD TYPE-ID
  ;; +TYPE-INDEX-LOCK-STRIPES+), bound that.  REAL BEHAVIOUR CHANGE, not a
  ;; pure optimisation: two type-ids landing on the same stripe now serialise
  ;; against each other on push/remove.  Accepted because the critical
  ;; section is a single push or remove on one index-list, so the added
  ;; contention is negligible next to the memory it bounds.
  (locks (map-into (make-array +type-index-lock-stripes+)
                   #+ccl 'make-lock
                   #+lispworks 'mp:make-lock
                   #+ecl 'mp:make-lock
                   #+sbcl 'sb-thread:make-mutex))
  (cache
   #+sbcl (make-hash-table :test 'eq :synchronized t)
   #+lispworks (make-hash-table :test 'eq :single-thread nil)
   #+ccl (make-hash-table :test 'eq :shared t)
   #+ecl (make-hash-table :test 'eq))
  ;; ECL only: its hash tables predate :SYNCHRONIZED (GH #101), so the cache
  ;; is guarded by this explicit lock instead.  Elsewhere CACHE's own
  ;; synchronized/shared option makes a single GETHASH/(SETF GETHASH) atomic,
  ;; and a lazy-populate race just deserializes the same on-disk bytes twice
  ;; -- idempotent, not corrupting.
  #+ecl (cache-lock (mp:make-lock))
  ;; Number of type-id slots TABLE currently has room for.  Grown on demand by
  ;; %TI-ENSURE-CAPACITY, which also extends TABLE itself; guarded by
  ;; GROW-LOCK, a lock distinct from the per-type stripe locks above because a
  ;; grow touches the whole mapping, not one type's slot.  Set from the actual
  ;; file size at both MAKE-TYPE-INDEX and OPEN-TYPE-INDEX, so a grow that ran
  ;; in a prior session is picked up correctly on reopen (#166).
  (capacity 0 :type (integer 0))
  (grow-lock #+ccl (make-lock)
             #+lispworks (mp:make-lock)
             #+ecl (mp:make-lock)
             #+sbcl (sb-thread:make-mutex)))

(defun make-type-index (location heap)
  (let* ((table (mmap-file location
                           :size (* +type-index-initial-types+
                                    +index-list-bytes+)))
         (idx (%make-type-index :table table :heap heap
                                :capacity +type-index-initial-types+)))
    ;; No eager per-slot write.  MMAP-FILE's freshly created region is
    ;; zero-filled, and a zero index-list slot (flags byte 0, head 0) IS an
    ;; empty index-list -- see MAKE-INDEX-LIST / SERIALIZE-INDEX-LIST, which
    ;; produce exactly those bytes for an empty list.  %TI-LIST deserializes
    ;; (and caches) each type's slot lazily, on first touch -- the ECL-only
    ;; path from #46, generalized to every implementation now that
    ;; +MAX-NODE-TYPES+ is too large to preallocate for at all (#166).
    idx))

(defun open-type-index (location heap)
  (let* ((table (mmap-file location :create-p nil))
         (idx (%make-type-index
               :table table :heap heap
               :capacity (floor (mapped-file-length table)
                                +index-list-bytes+))))
    ;; Lazy (#46, generalized in #166): do NOT deserialize an index-list for
    ;; every possible type at open.  %TI-LIST deserializes on first touch.
    idx))

(defun %ti-ensure-capacity (idx type-id)
  "Grow IDX's table so TYPE-ID has a slot.  Type-ids are assigned sequentially
from 1 (schema.lisp), so most stores never call this: the table is sized at
open for the types actually in use (+TYPE-INDEX-INITIAL-TYPES+), not the id
ceiling.  Doubles the capacity until TYPE-ID fits and extends the mmap in
place via EXTEND-MAPPED-FILE (mmap.lisp) -- the same primitive
GROW-MEMORY/ACQUIRE-OVERFLOW-BUCKET use for the heap and the linear hash, not
the vector-segment's relocating grow: that one moves the mapping's base
pointer, which is safe there only because SEGMENT-PUT/GET take a per-segment
rw-lock excluding every reader, a lock TYPE-INDEX reads do not take."
  (when (>= type-id (type-index-capacity idx))
    (with-lock ((type-index-grow-lock idx))
      ;; Re-check under the lock: another thread may have already grown past
      ;; TYPE-ID while this one waited.
      (when (>= type-id (type-index-capacity idx))
        (let ((new-cap (max 1 (type-index-capacity idx)))
              (table (type-index-table idx)))
          (loop while (<= new-cap type-id) do (setf new-cap (* 2 new-cap)))
          (let* ((have (mapped-file-length table))
                 (need (* new-cap +index-list-bytes+)))
            (when (> need have)
              (extend-mapped-file table (- need have))))
          (setf (type-index-capacity idx) new-cap))))))

(defun %ti-list (idx type-id)
  "The index-list for TYPE-ID in IDX, growing IDX first if TYPE-ID lands past
its current capacity (#166).  The cache is lazy (#46, generalized to every
implementation in #166): return the cached list, or deserialize-and-cache it
on first touch.  On ECL this is guarded by an explicit lock (GH #101); see the
CACHE-LOCK slot comment for why other implementations need none."
  (%ti-ensure-capacity idx type-id)
  #+ecl
  (with-lock ((type-index-cache-lock idx))
    (or (gethash type-id (type-index-cache idx))
        (setf (gethash type-id (type-index-cache idx))
              (deserialize-index-list (type-index-table idx)
                                      (* type-id +index-list-bytes+)
                                      (type-index-heap idx)))))
  #-ecl
  (or (gethash type-id (type-index-cache idx))
      (setf (gethash type-id (type-index-cache idx))
            (deserialize-index-list (type-index-table idx)
                                    (* type-id +index-list-bytes+)
                                    (type-index-heap idx)))))

(defmethod close-type-index ((index type-index))
  (munmap-file (type-index-table index) :save-p t))

(defgeneric add-to-type-index (node graph &key unless-present))
(defgeneric remove-from-type-index (node graph))

(defmethod type-index-push ((uuid array) (type-id integer) (idx type-index)
                            &key unless-present)
  (let ((lock (aref (type-index-locks idx)
                    (mod type-id +type-index-lock-stripes+))))
    (with-lock (lock)
      (let ((il (%ti-list idx type-id)))
        (if unless-present
            (index-list-pushnew uuid il)
            (index-list-push uuid il))
        ;; FIXME: could be optimized to only write the new head
        (serialize-index-list (type-index-table idx)
                              il
                              (* type-id +index-list-bytes+))
        il))))

(defmethod type-index-remove ((uuid array) (type-id integer) (idx type-index))
  (let ((lock (aref (type-index-locks idx)
                    (mod type-id +type-index-lock-stripes+))))
    (with-lock (lock)
      (let ((il (%ti-list idx type-id)))
        (remove-from-index-list uuid il)
        (serialize-index-list (type-index-table idx)
                              il
                              (* type-id +index-list-bytes+))
        il))))

(defmethod get-type-index-list ((idx type-index) (type-id integer))
  (%ti-list idx type-id))
