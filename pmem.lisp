(in-package :graph-db)

(defvar *pmem*)
(alexandria:define-constant +pmem-magic-byte+ #x1A)
(alexandria:define-constant +stack-pointer-offset+ 1)
(alexandria:define-constant +heap-pointer-offset+ 5)
(alexandria:define-constant +stack-pointer-start-offset+ 9)

(defstruct (pmem
             (:conc-name %pmem-)
             (:constructor %make-pmem))
  memory size offset stack-pointer heap-pointer lock cache)

(defmethod %pmem-mmap ((pmem pmem))
  (memory-mmap (%pmem-memory pmem)))

(defmethod set-stack-pointer ((pmem pmem) (address integer))
  (serialize-uint32 (%pmem-mmap pmem) address (%pmem-stack-pointer pmem)))

(defun (setf stack-pointer) (value pmem)
  (set-stack-pointer pmem value))

(defmethod stack-pointer ((pmem pmem))
  (deserialize-uint32 (%pmem-mmap pmem) (%pmem-stack-pointer pmem)))

(defmethod set-heap-pointer ((pmem pmem) (address integer))
  (serialize-uint32 (%pmem-mmap pmem) address (%pmem-heap-pointer pmem)))

(defun (setf heap-pointer) (value pmem)
  (set-heap-pointer pmem value))

(defmethod heap-pointer ((pmem pmem))
  (deserialize-uint32 (%pmem-mmap pmem) (%pmem-heap-pointer pmem)))

(defun make-pmem (memory &key (size (expt 2 24)))
  (when (> size (expt 2 32))
    (error "Cannot create pmem greater than ~S bytes in size" (expt 2 32)))
  (let ((offset (allocate memory size)))
    (set-byte memory offset +pmem-magic-byte+)
    (let ((pmem
           (%make-pmem :memory memory
                       :size size
                       :offset offset
                       :stack-pointer (+ offset +stack-pointer-offset+)
                       :heap-pointer (+ offset +heap-pointer-offset+)
                       :lock (make-recursive-lock)
                       :cache
                       #+sbcl (make-hash-table :weakness :value :synchronized t)
                       #+ccl (make-hash-table :weak :value :shared t)
                       #+lispworks (make-hash-table :weak-kind :value :single-thread nil)
                       #+ecl (make-hash-table :weakness :value
                                              #+graph-db-ecl-sync-hash :synchronized
                                              #+graph-db-ecl-sync-hash t))))
      (setf (stack-pointer pmem) +stack-pointer-start-offset+)
      (setf (heap-pointer pmem) (+ offset size))
      pmem)))

(defmethod free-pmem ((pmem pmem))
  (free (%pmem-memory pmem) (%pmem-offset pmem))
  (setf (%pmem-memory pmem) nil
        (%pmem-size pmem) 0
        (%pmem-offset pmem) 0
        (%pmem-stack-pointer pmem) nil
        (%pmem-heap-pointer pmem) nil)
  nil)

(defmethod stack-allocate ((pmem pmem) (size integer))
  (with-recursive-lock-held ((%pmem-lock pmem))
    (let ((address (stack-pointer pmem)))
      (if (>= address (heap-pointer pmem))
          (error "Cannot stack allocate ~S bytes: out of memory in ~S" size pmem)
          (progn
            (incf (stack-pointer pmem) size)
            address)))))

(defmethod heap-allocate ((pmem pmem) (size integer))
  (with-recursive-lock-held ((%pmem-lock pmem))
    (let ((address (+ size (heap-pointer pmem))))
      (if (<= address (stack-pointer pmem))
          (error "Cannot heap allocate ~S bytes: out of memory in ~S" size pmem)
          (progn
            (decf (heap-pointer pmem) size)
            (heap-pointer pmem))))))

