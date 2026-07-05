(in-package :graph-db)

(defgeneric %map-index-list (fn il &key collect-p))

(defstruct (index-list
             (:constructor %make-index-list)
             (:print-function
              (lambda (il s d)
                (declare (ignore d))
                (format s "#<INDEX-LIST (HEAD ~A)>"
                        (index-list-head il)))))
  heap
  ;; Per-index-list pcons cache, DISABLED (defaults to NIL).
  ;;
  ;; This cache is dead code: every read of it is commented out (see
  ;; DESERIALIZE-PCONS, "using the cache causes the system to hang"), so it was
  ;; only ever written, never consulted.  Worse, eagerly allocating one weak
  ;; hash-table per index-list crashes CCL's GC once many are live at the same
  ;; time -- e.g. the 65536 index-lists a TYPE-INDEX holds -- which made
  ;; MAKE-GRAPH unusable on CCL.  The original allocation is preserved below
  ;; (commented out) in case the cache is ever revived; if so, also re-enable
  ;; the guarded writes in MAKE-INDEX-LIST / DELETE-INDEX-LIST and the reads in
  ;; DESERIALIZE-PCONS.
  (cache nil
   ;; #+sbcl (make-hash-table :weakness :value :synchronized t)
   ;; #+ccl (make-hash-table :weak :value :shared t)
   ;; #+lispworks (make-hash-table :weak-kind :value :single-thread nil)
   )
  head
  (lock (make-rw-lock))
  dirty-p)

(defmethod flags-as-int ((il index-list))
  (let ((flags 0))
    (when (index-list-dirty-p il)
      (setq flags (dpb 1 (byte 1 0) flags)))
    flags))

(defmethod deserialize-pcons ((il index-list) (address integer) &optional pcons-buffer)
  ;; FIXME: using the cache causes the system to hang; not sure why yet
  ;;(or (and *cache-enabled* (gethash address (index-list-cache il)))
  ;;(setf (gethash address (index-list-cache il))
  (let ((pcons (or pcons-buffer (get-pcons-buffer))))
    (setf (%pcons-car pcons) (get-bytes (index-list-heap il) address 16)
          (%pcons-cdr pcons) (deserialize-uint64
                              (index-list-heap il) (+ 16 address))
          (%pcons-deleted-p pcons) (ldb-test (byte 1 0)
                                             (get-byte (index-list-heap il)
                                                       (+ 24 address))))
    pcons))

(defmethod serialize-index-list ((mf mapped-file) (il index-list) (offset integer))
  (declare (type word offset))
  ;; flags: dirty-p
  (let ((flags (flags-as-int il)))
    ;;(log:debug "SETTING BYTE ~A FLAGS ~A" offset flags)
    (set-byte mf offset flags))
  (incf offset)
  ;; head
  ;;(log:debug "SETTING BYTES ~A HEAD ~A" offset (index-list-head il))
  (setq offset (serialize-uint64 mf (index-list-head il) offset))
  ;;(log:debug "RETURNING OFFSET ~A" offset)
  offset)

(defmethod deserialize-index-list ((mf mapped-file) (offset integer)
                                   &optional (heap (heap *graph*)))
  ;; HEAP defaults to the current graph's heap (the :value-deserializer callers in
  ;; ve-index/vev-index rely on that), but callers without *GRAPH* bound -- e.g.
  ;; the lazy type-index %TI-LIST (#46) -- pass it explicitly.
  (let ((il (%make-index-list))
        (flags (get-byte mf offset)))
    (setf (index-list-dirty-p il) (ldb-test (byte 1 0) flags)
          (index-list-head il) (deserialize-uint64 mf (incf offset))
          (index-list-heap il) heap)
    il))

(defmethod %map-index-list (fn (il index-list) &key collect-p include-deleted-p)
  (let ((address (index-list-head il))
        (result nil)
        (pcons-buffer (get-pcons-buffer)))
    (loop
       until (eq address 0) do
         (let ((pcons (deserialize-pcons il address pcons-buffer)))
           (when (or include-deleted-p
                     (not (%pcons-deleted-p pcons)))
             (if collect-p
                 (push (funcall fn pcons) result)
                 (funcall fn pcons)))
           (setq address (%pcons-cdr pcons))))
    (nreverse result)))

(defmethod map-index-list (fn (il index-list) &key collect-p include-deleted-p)
  (%map-index-list (lambda (pcons)
                     (funcall fn (%pcons-car pcons)))
                   il
                   :collect-p collect-p
                   :include-deleted-p include-deleted-p))

(defmethod %il-nth ((n integer) (il index-list))
  (let ((counter 0))
    (%map-index-list (lambda (id)
                       (when (= counter n)
                         (return-from %il-nth id))
                       (incf counter))
                     il)
    nil))

(defmethod index-list-push ((uuid array) (il index-list))
  (let ((pcons (get-pcons-buffer)))
    (setf (%pcons-car pcons) uuid
          (%pcons-cdr pcons) (index-list-head il)
          (%pcons-deleted-p pcons) nil)
    (let ((address (serialize-pcons pcons (index-list-heap il))))
      ;; Cache it
      ;;(setf (gethash address (index-list-cache il)) pcons)
      ;; Update index-list values
      ;;(log:debug "OLD HEAD: ~A" (index-list-head il))
      #+sbcl
      (sb-ext:cas (index-list-head il)
                  (index-list-head il)
                  address)
      #+ccl
      (ccl::conditional-store (index-list-head il)
                              (index-list-head il)
                              address)
      #+lispworks
      (sys:compare-and-swap (index-list-head il)
                            (index-list-head il)
                            address)
      #+ecl
      (setf (index-list-head il) address)
      ;;(log:debug "NEW HEAD: ~A" (index-list-head il))
      il)))

(defmethod index-list-member-p (object (il index-list)
                                &key (test 'uuid-array-equal) (key 'identity))
  (block nil
    (map-index-list (lambda (element)
                      (when (funcall test object (funcall key element))
                        (return t)))
                    il)
    nil))

(defmethod index-list-pushnew ((uuid array) (il index-list))
  (unless (index-list-member-p uuid il)
    (index-list-push uuid il)))

(defun make-index-list (heap &rest uuids)
  (let ((il (%make-index-list :heap heap))
        (addresses nil))
    (handler-case
        (let ((prev 0))
          (dolist (id (nreverse uuids))
            (let* ((uuid-address (allocate heap (+ 16 8 1))))
              (push uuid-address addresses)
              ;;(log:debug "Serializing UUID:~A to ADDR:~A" id uuid-address)
              ;; id
              (dotimes (i 16)
                (set-byte heap (+ i uuid-address) (aref id i)))
              ;; next pointer
              (serialize-uint64 heap prev (+ 16 uuid-address))
              ;; flags: deleted-p
              (set-byte heap (+ 24 uuid-address) 0)
              (let ((pcons (get-pcons-buffer)))
                (setf (%pcons-car pcons) id
                      (%pcons-cdr pcons) uuid-address
                      (%pcons-deleted-p pcons) nil)
                (when (index-list-cache il)
                  (setf (gethash uuid-address (index-list-cache il))
                        pcons))
                (setq prev uuid-address))))
          (setf (index-list-head il) prev)
          il)
      (error (c)
        (dolist (address (nreverse addresses))
          (free heap address))
        (error c)))))

(defmethod delete-index-list ((il index-list))
  (dolist (addr (let ((address (index-list-head il)))
                  (loop
                     until (eq address 0)
                     collecting
                     (deserialize-uint64
                      (index-list-heap il) (+ 16 address)))))
    (free (index-list-heap il) addr))
  (free (index-list-heap il) (index-list-head il))
  (when (index-list-cache il)
    (clrhash (index-list-cache il)))
  (setf (index-list-head il) 0)
  nil)

(defmethod remove-from-index-list ((uuid array) (il index-list) &key remove-all-p)
  (let ((address (index-list-head il)) (prev nil)
        (pcons-buffer (get-pcons-buffer)))
    (loop until (eq address 0) do
         (let ((pcons (deserialize-pcons il address pcons-buffer)))
           (unless (%pcons-deleted-p pcons)
             (let ((id (%pcons-car pcons))
                   (next (%pcons-cdr pcons)))
               (if (uuid-array-equal id uuid)
                   (progn ;; remove it!
                     (mark-pcons-deleted pcons (index-list-heap il) address)
                     (setf (index-list-dirty-p il) t)
                     (setq address next)
                     (unless remove-all-p
                       ;; Only remove the first one!
                       (return)))
                   ;; otherwise, just advance
                   (setq prev address
                         address next))))))
    il))
