(in-package :graph-db)

(defstruct (ve-key
             (:print-function
              (lambda (i s d)
                (declare (ignore d))
                (let ((*print-base* 10))
                  (format s "#<VE-KEY ~S ~S>"
                          (string-id (ve-key-id i))
                          (ve-key-type-id i))))))
  (id +null-key+ :type (simple-array (unsigned-byte 8) (16))) ;; node-id
  (type-id 0 :type (integer 0 65535))) ;; type-id

(defvar *ve-null-key* (make-ve-key))

(defmethod %hash ((ve-key ve-key))
  (declare (optimize (speed 3) (safety 0)))
  #|
  (let ((hash 5381))
    (dotimes (i 16)
      (let ((item (aref (ve-key-id ve-key) i)))
        (setf hash (+ (+ hash (ash hash -5)) item))))
    (+ (+ hash (ash hash -5)) (ve-key-type-id ve-key))))
  |#
  (+ (%hash (ve-key-id ve-key)) (ve-key-type-id ve-key)))

(declaim (inline %ve-key-equal))
(defun %ve-key-equal (key1 key2)
  (declare (optimize (speed 3) (safety 0)))
  (and (= (ve-key-type-id key1) (ve-key-type-id key2))
       (equalp (ve-key-id key1) (ve-key-id key2))))

(defgeneric ve-key-equal (x y &optional offset1 offset2)
  (:method ((key1 ve-key) (key2 ve-key) &optional _a _b)
    (declare (ignore _a _b))
    (%ve-key-equal key1 key2))
  (:method ((key1 ve-key) (mf mapped-file) &optional offset _)
    (declare (ignore _))
    (let ((key2 (deserialize-ve-key-mmap mf offset)))
      (%ve-key-equal key1 key2)))
  (:method ((key1 ve-key) (y array) &optional _a _b)
    (declare (ignore _a _b))
    (let ((key2 (deserialize-ve-key y)))
      (%ve-key-equal key1 key2)))
  (:method ((x array) (key2 ve-key) &optional _a _b)
    (declare (ignore _a _b))
    (let ((key1 (deserialize-ve-key x)))
      (%ve-key-equal key1 key2)))
  )

(defun sxhash-ve-key (k) (sxhash (%hash k)))
#+sbcl (sb-ext:define-hash-table-test ve-key-equal sxhash-ve-key)
(defun make-ve-cache ()
  #+ccl
  (make-hash-table :test 've-key-equal
                   :hash-function 'sxhash-ve-key
                   :shared t
                   :weak :value)
  #+lispworks
  (make-hash-table :test 've-key-equal
                   :hash-function 'sxhash-ve-key
                   :single-thread nil 
                   :weak-kind :value)
  #+sbcl
  (make-hash-table :test 've-key-equal :synchronized t :weakness :value)
  ;; %VE-KEY-EQUAL is exactly EQUALP on the VE-KEY struct (= type-id + equalp
  ;; id), so ECL's native EQUALP tables are an exact substitute (no custom
  ;; hash-table test support on ECL).
  #+ecl
  (make-hash-table :test 'equalp :weakness :value))

(defstruct (ve-index
             (:constructor %make-ve-index))
  table
  (cache (make-ve-cache)))

(defmethod serialize-ve-key-mmap ((mf mapped-file) (ve-key ve-key)
                                  (offset integer))
  (declare (type word offset))
  (dotimes (i 16)
    (set-byte mf offset (aref (ve-key-id ve-key) i))
    (incf offset))
  ;; Big endian ints for easy comparison in ve-key-lessp
  (set-byte mf offset (ldb (byte 8 (* 1 8)) (ve-key-type-id ve-key)))
  (incf offset)
  (set-byte mf offset (ldb (byte 8 (* 0 8)) (ve-key-type-id ve-key)))
  (incf offset))

(defmethod deserialize-ve-key-mmap ((mf mapped-file) (offset integer))
  (declare (type word offset))
  (let ((id (get-buffer 16)) (type-id 0))
    (declare (type (array (unsigned-byte 8) (16)) id))
    (declare (type word type-id))
    (dotimes (i 16)
      (setf (aref id i) (get-byte mf offset))
      (incf offset))
    ;; Big endian ints for easy comparison in ve-key-lessp
    (setq type-id (dpb (get-byte mf offset) (byte 8 (* 1 8)) type-id))
    (incf offset)
    (setq type-id (dpb (get-byte mf offset) (byte 8 (* 0 8)) type-id))
    (make-ve-key :id id :type-id type-id)))

(defmethod serialize-ve-key ((array array))
  array)

(defmethod serialize-ve-key ((ve-key ve-key))
  (let ((vec (get-buffer 18)))
    (dotimes (i 16)
      (setf (aref vec i) (aref (ve-key-id ve-key) i)))
    ;; Big endian ints for easy comparison in ve-key-lessp
    (setf (aref vec 16) (ldb (byte 8 (* 1 8)) (ve-key-type-id ve-key)))
    (setf (aref vec 17) (ldb (byte 8 (* 0 8)) (ve-key-type-id ve-key)))
    vec))

(defmethod deserialize-ve-key ((vec array))
  (let ((id (get-buffer 16)) (type-id 0))
    (declare (type (array (unsigned-byte 8) (16)) id))
    (declare (type word type-id))
    (dotimes (i 16)
      (setf (aref id i) (aref vec i)))
    ;; Big endian ints for easy comparison in ve-key-lessp
    (setq type-id (dpb (aref vec 16) (byte 8 (* 1 8)) type-id))
    (setq type-id (dpb (aref vec 17) (byte 8 (* 0 8)) type-id))
    (values (make-ve-key :id id :type-id type-id) 18)))

(defun make-ve-index (location)
  (let* ((idx (make-lhash :test 've-key-equal
                          :location location
                          :value-bytes +index-list-bytes+
                          :key-bytes +ve-key-bytes+
                          :null-key *ve-null-key*
                          :bucket-size 24
                          :buckets (expt 2 16)
                          :key-serializer 'serialize-ve-key-mmap
                          :key-deserializer 'deserialize-ve-key-mmap
                          :value-serializer 'serialize-index-list
                          :value-deserializer 'deserialize-index-list)))
    (%make-ve-index :table idx)))

(defun open-ve-index (location)
  (%make-ve-index :table (open-lhash location)))

(defmethod close-ve-index ((index ve-index))
  (close-lhash (ve-index-table index)))

(declaim (inline cache-index-list))
(defmethod cache-index-list ((index ve-index) (key ve-key) (il index-list))
  (setf (gethash key (ve-index-cache index)) il))

;; Both bind *GRAPH* to the OWNING graph so the lhash value-deserializer
;; (DESERIALIZE-INDEX-LIST, which defaults the index-list heap to (HEAP *GRAPH*))
;; resolves against GRAPH, not the ambient *GRAPH* -- else a cross-graph read binds
;; the list to the wrong heap and poisons the cache (wrong-graph audit SUSPECT #3).
(defmethod lookup-ve-in-index-list ((key ve-key) (graph graph))
  (let ((*graph* graph))
    (or (gethash key (ve-index-cache (ve-index-in graph)))
        (let ((table (ve-index-table (ve-index-in graph))))
          (with-locked-hash-key (table key)
            (let ((il (lhash-get table key)))
              (when il
                (cache-index-list (ve-index-in graph) key il))))))))

(defmethod lookup-ve-out-index-list ((key ve-key) (graph graph))
  (let ((*graph* graph))
    (or (gethash key (ve-index-cache (ve-index-out graph)))
        (let ((table (ve-index-table (ve-index-out graph))))
          (with-locked-hash-key (table key)
            (let ((il (lhash-get table key)))
              (when il
                (cache-index-list (ve-index-out graph) key il))))))))

(defmethod ve-index-push ((idx ve-index) (key ve-key) (id array)
                          &key unless-present heap)
  (let ((table (ve-index-table idx)))
    (with-locked-hash-key (table key)
      ;;(log:debug "ve-index-push ~A:~A" key id)
      (let ((index-list (%lhash-get table key)))
        (if index-list
            (progn
              ;;(log:debug "add-to-ve-index: Got ~A" index-list)
              (if unless-present
                  (index-list-pushnew id index-list)
                  (index-list-push id index-list))
              (%lhash-update table key index-list)
              ;;(log:debug "add-to-ve-index: AFTER PUSH: ~A" index-list)
              )
            (progn
              ;; Allocate the new index-list in the OWNING graph's heap (passed
              ;; in by the caller), not (heap *graph*) -- else an edge inserted
              ;; while *graph* names a different graph (slave apply, replay, a
              ;; second open graph) stores an index-list head that points into a
              ;; foreign heap.  Sibling ADD-TO-VEV-INDEX threads (heap graph) too.
              (setq index-list
                    (make-index-list (or heap (heap *graph*)) id))
              ;;(log:debug "add-to-ve-index: Made new ~A" index-list)
              (%lhash-insert table key index-list)))
        (cache-index-list idx key index-list)))))

(defmethod ve-index-remove ((idx ve-index) (key ve-key) (id array))
  (let ((table (ve-index-table idx)))
    (with-locked-hash-key (table key)
      (let ((index-list (%lhash-get table key)))
        (when index-list
          ;;(log:debug "Removing ~A from ~A" edge index-list)
          (remove-from-index-list id index-list)
          (%lhash-update table key index-list)
          (cache-index-list idx key index-list))))))

(defgeneric add-to-ve-index (edge graph &key unless-present))
(defgeneric remove-from-ve-index (edge graph))
