(in-package :graph-db)

(defstruct (vev-key
             (:print-function
              (lambda (i s d)
                (declare (ignore d))
                (let ((*print-base* 10))
                  (format s "#<VEV-KEY ~S -> ~S -> ~S>"
                          (uuid:byte-array-to-uuid (vev-key-out-id i))
                          (vev-key-type-id i)
                          (uuid:byte-array-to-uuid (vev-key-in-id i)))))))
  (out-id +null-key+ :type (simple-array (unsigned-byte 8) (16))) ;; node-id
  (in-id +null-key+ :type (simple-array (unsigned-byte 8) (16))) ;; node-id
  (type-id 0 :type (integer 0 65535))) ;; type-id

(defvar *vev-null-key* (make-vev-key))

(defmethod %hash ((vev-key vev-key))
  (declare (optimize (speed 3) (safety 0)))
  #|
  (let ((hash 5381))
    (dotimes (i 16)
      (let ((item (aref (vev-key-in-id vev-key) i)))
        (setf hash (+ (+ hash (ash hash -5)) item))))
    (dotimes (i 16)
      (let ((item (aref (vev-key-out-id vev-key) i)))
        (setf hash (+ (+ hash (ash hash -5)) item))))
    (+ (+ hash (ash hash -5) (vev-key-type-id vev-key))))
  |#
  (+ (%hash (vev-key-in-id vev-key))
     (%hash (vev-key-out-id vev-key))
     (vev-key-type-id vev-key)))

(declaim (inline %vev-key-equal))
(defun %vev-key-equal (key1 key2)
  (declare (optimize (speed 3) (safety 0)))
  (and (= (vev-key-type-id key1) (vev-key-type-id key2))
       (equalp (vev-key-out-id key1) (vev-key-out-id key2))
       (equalp (vev-key-in-id key1) (vev-key-in-id key2))))

(defgeneric vev-key-equal (x y &optional offset1 offset2)
  (:method ((key1 vev-key) (key2 vev-key) &optional _a _b)
    (declare (ignore _a _b))
    (%vev-key-equal key1 key2))
  (:method ((key1 vev-key) (mf mapped-file) &optional offset _)
    (declare (ignore _))
    (let ((key2 (deserialize-vev-key-mmap mf offset)))
      (%vev-key-equal key1 key2)))
  (:method ((key1 vev-key) (y array) &optional _a _b)
    (declare (ignore _a _b))
    (let ((key2 (deserialize-vev-key y)))
      (%vev-key-equal key1 key2)))
  (:method ((x array) (key2 vev-key) &optional _a _b)
    (declare (ignore _a _b))
    (let ((key1 (deserialize-vev-key x)))
      (%vev-key-equal key1 key2)))
  (:method ((x array) (y array) &optional _a _b)
    (declare (ignore _a _b))
    (let ((key1 (deserialize-vev-key x))
          (key2 (deserialize-vev-key y)))
      (%vev-key-equal key1 key2)))
  )

(defun sxhash-vev-key (k) (sxhash (%hash k)))
#+sbcl (sb-ext:define-hash-table-test vev-key-equal sxhash-vev-key)
(defun make-vev-cache ()
  #+ccl
  (make-hash-table :test 'vev-key-equal
                   :hash-function 'sxhash-vev-key
                   :shared t
                   :weak :value)
  #+lispworks
  (make-hash-table :test 'vev-key-equal
                   :hash-function 'sxhash-vev-key
                   :single-thread nil
                   :weak-kind :value)
  #+sbcl
  (make-hash-table :test 'vev-key-equal :synchronized t :weakness :value)
  ;; %VEV-KEY-EQUAL is exactly EQUALP on the VEV-KEY struct (= type-id + equalp
  ;; out-id + equalp in-id), so ECL's native EQUALP tables are an exact
  ;; substitute (no custom hash-table test support on ECL).
  #+ecl
  (make-hash-table :test 'equalp :weakness :value))

(defstruct (vev-index
             (:constructor %make-vev-index))
  table
  (cache (make-vev-cache)))

(defmethod serialize-vev-key-mmap ((mf mapped-file) (vev-key vev-key)
                                   (offset integer))
  (declare (type word offset))
  (dotimes (i 16)
    (set-byte mf offset (aref (vev-key-out-id vev-key) i))
    (incf offset))
  (dotimes (i 16)
    (set-byte mf offset (aref (vev-key-in-id vev-key) i))
    (incf offset))
  ;; Big endian ints for easy comparison in vev-key-lessp
  (set-byte mf offset (ldb (byte 8 (* 1 8)) (vev-key-type-id vev-key)))
  (incf offset)
  (set-byte mf offset (ldb (byte 8 (* 0 8)) (vev-key-type-id vev-key)))
  (incf offset))

(defmethod deserialize-vev-key-mmap ((mf mapped-file) (offset integer))
  (declare (type word offset))
  (let ((out-id (get-buffer 16)) (in-id (get-buffer 16)) (type-id 0))
    (declare (type (array (unsigned-byte 8) (16)) out-id))
    (declare (type (array (unsigned-byte 8) (16)) in-id))
    (declare (type word type-id))
    (dotimes (i 16)
      (setf (aref out-id i) (get-byte mf offset))
      (incf offset))
    (dotimes (i 16)
      (setf (aref in-id i) (get-byte mf offset))
      (incf offset))
    ;; Big endian ints for easy comparison in vev-key-lessp
    (setq type-id (dpb (get-byte mf offset) (byte 8 (* 1 8)) type-id))
    (incf offset)
    (setq type-id (dpb (get-byte mf offset) (byte 8 (* 0 8)) type-id))
    (incf offset)
    (make-vev-key :out-id out-id :in-id in-id :type-id type-id)))

(defmethod serialize-vev-key ((array array))
  array)

(defmethod serialize-vev-key ((vev-key vev-key))
  (let ((vec (get-buffer 34)) (offset 0))
    (dotimes (i 16)
      (setf (aref vec offset) (aref (vev-key-out-id vev-key) i))
      (incf offset))
    (dotimes (i 16)
      (setf (aref vec offset) (aref (vev-key-in-id vev-key) i))
      (incf offset))
    ;; Big endian ints for easy comparison in vev-key-lessp
    (setf (aref vec 32) (ldb (byte 8 (* 1 8)) (vev-key-type-id vev-key)))
    (setf (aref vec 33) (ldb (byte 8 (* 0 8)) (vev-key-type-id vev-key)))
    vec))

(defmethod deserialize-vev-key ((vec array))
  (let ((out-id (get-buffer 16)) (in-id (get-buffer 16)) (type-id 0))
    (declare (type (array (unsigned-byte 8) (16)) out-id))
    (declare (type (array (unsigned-byte 8) (16)) in-id))
    (declare (type word type-id))
    (dotimes (i 16)
      (setf (aref out-id i) (aref vec i)))
    (dotimes (i 16)
      (setf (aref in-id i) (aref vec (+ 16 i))))
    ;; Big endian ints for easy comparison in vev-key-lessp
    (setq type-id (dpb (aref vec 32) (byte 8 (* 1 8)) type-id))
    (setq type-id (dpb (aref vec 33) (byte 8 (* 0 8)) type-id))
    (values (make-vev-key :out-id out-id :in-id in-id :type-id type-id) 34)))

(defun make-vev-index (location)
  (let* ((idx (make-lhash :test 'vev-key-equal
                          :location location
                          :value-bytes +index-list-bytes+
                          :key-bytes +vev-key-bytes+
                          :null-key *vev-null-key*
                          :bucket-size 24
                          :buckets (expt 2 16)
                          :key-serializer 'serialize-vev-key-mmap
                          :key-deserializer 'deserialize-vev-key-mmap
                          :value-serializer 'serialize-index-list
                          :value-deserializer 'deserialize-index-list)))
    (%make-vev-index :table idx)))

(defun open-vev-index (location)
  (%make-vev-index :table (open-lhash location)))

(defmethod close-vev-index ((index vev-index))
  (close-lhash (vev-index-table index)))

(defmethod cache-index-list ((index vev-index) (key vev-key) (il index-list))
  (setf (gethash key (vev-index-cache index)) il))

(defmethod lookup-vev-index-list ((key vev-key) (graph graph))
  (or (gethash key (vev-index-cache (vev-index graph)))
      (let ((table (vev-index-table (vev-index graph))))
        (with-locked-hash-key (table key)
          (let ((il (lhash-get table key)))
            (when il
              (cache-index-list (vev-index graph) key il)))))))

(defgeneric add-to-vev-index (edge graph &key unless-present))
(defgeneric remove-from-vev-index (edge graph))

#|
(defvar *test-heap* nil)
(defun test-vev ()
  (let ((table (make-vev-index "/var/tmp/vev2/"))
        (*test-heap* (create-memory "/var/tmp/vev2.dat" 1024000)))
    (unwind-protect
         (let ((id1 (gen-id)) (id2 (gen-id))
               (id3 (gen-id)) (id4 (gen-id))
               (id5 (gen-id)) (id6 (gen-id))
               (e1-id (gen-id))
               (e2-id (gen-id))
               (e3-id (gen-id))
               (e4-id (gen-id))
               (e5-id (gen-id)))
           (add-to-new-vev-index table *test-heap* id1 id2 0 e1-id)
           (add-to-new-vev-index table *test-heap* id1 id2 0 e4-id)
           (add-to-new-vev-index table *test-heap* id3 id4 1 e2-id)
           (add-to-new-vev-index table *test-heap* id5 id6 2 e3-id)
           (add-to-new-vev-index table *test-heap* id1 id2 0 e5-id)
           (values (get-from-new-vev-index table id1 id2 0)
                   (%map-index-list 'string-uuid
                                    (get-from-new-vev-index table id1 id2 0)
                                    :collect-p t)
                   (get-from-new-vev-index table id3 id4 1)
                   (%map-index-list 'string-uuid
                                    (get-from-new-vev-index table id3 id4 1)
                                    :collect-p t)
                   (get-from-new-vev-index table id5 id6 2)
                   (%map-index-list 'string-uuid
                                    (get-from-new-vev-index table id5 id6 2)
                                    :collect-p t)
                   (map-lhash 'identity table :collect-p t)))
      (when table
        (close-memory *test-heap*)
        (close-vev-index table)
        (cl-fad:delete-directory-and-files "/var/tmp/vev2/")))))
|#
