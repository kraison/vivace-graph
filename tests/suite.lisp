;;;; Master test suite and shared fixtures for graph-db.

(in-package #:graph-db/test)

(def-suite graph-db-suite
  :description "All graph-db unit tests.")

(defun run-tests ()
  "Run the entire graph-db test suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db)."
  ;; The storage layers log prolifically at :debug/:info; keep test output
  ;; to genuine problems.
  (log:config :error)
  ;; ECL caps its GC heap conservatively; running the whole suite in one image
  ;; (many graphs + MVCC version retention + buffer pools) exceeds the default
  ;; and aborts with EXT:STORAGE-EXHAUSTED.  Raise the ceiling (a limit, not a
  ;; reservation) so the suite fits.  SBCL/CCL grow their heaps automatically.
  #+ecl (ext:set-limit 'ext:heap-size (* 6 1024 1024 1024))
  (let ((results (run 'graph-db-suite)))
    (explain! results)
    (results-status results)))

;;; ---------------------------------------------------------------------------
;;; Temp-file fixtures
;;;
;;; The storage layers all live in mmap'd files, so each test needs a
;;; private scratch directory that is reliably torn down afterwards.
;;; ---------------------------------------------------------------------------

;; SBCL's initial *RANDOM-STATE* is a fixed constant, so an unseeded (RANDOM ...)
;; produces the SAME name sequence in every image: two concurrent suite runs on a
;; shared host would share -- and delete -- each other's scratch dirs.  Seed from
;; entropy, lazily so a dumped image reseeds in each new process, and add a
;; counter so one image can never repeat a name either.
(defvar *scratch-random-state* nil)
(defvar *scratch-counter* 0)

(defun scratch-tag ()
  "A name fragment unique across concurrent processes and across calls."
  (unless *scratch-random-state*
    (setf *scratch-random-state* (make-random-state t)))
  (format nil "~36R-~36R"
          (random (expt 36 12) *scratch-random-state*)
          (incf *scratch-counter*)))

(defun make-temp-directory ()
  "Create and return a fresh, unique scratch directory pathname."
  (let ((dir (merge-pathnames (format nil "graph-db-test-~A/" (scratch-tag))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun make-temp-file-name (prefix type)
  "A unique, not-yet-created scratch file pathname (PREFIX-<tag>.TYPE)."
  (merge-pathnames (format nil "~A-~A.~A" prefix (scratch-tag) type)
                   (uiop:temporary-directory)))

(defmacro with-temp-directory ((var) &body body)
  "Bind VAR to a fresh scratch directory, run BODY, then delete the tree."
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun collect-garbage ()
  "Force a full GC.  Each graph / type-index preallocates 65536 index-lists
per type table, so without reclaiming between tests a whole suite run in one
image exhausts the default heap."
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  #+lispworks (hcl:gc-all)
  #+ecl (ext:gc t))

(defmacro with-temp-memory ((var &key (size '(* 1024 1024 64))) &body body)
  "Bind VAR to a freshly created MEMORY backed by a temp file, run BODY,
then close it and remove the scratch directory."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (create-memory (namestring (merge-pathnames "heap.dat" ,dir))
                                  ,size)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-memory ,var)))))))

(defmacro with-temp-lhash ((var &rest make-args) &body body)
  "Bind VAR to a freshly created LHASH rooted in a temp directory, run
BODY, then close it and remove the scratch directory.  MAKE-ARGS are
passed through to MAKE-LHASH (e.g. :buckets 4)."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (make-lhash :location ,dir ,@make-args)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-lhash ,var)))))))

(defmacro with-temp-type-index ((idx-var heap-var &key (size '(* 1024 1024 16)))
                                &body body)
  "Bind HEAP-VAR to a temp heap and IDX-VAR to a fresh type-index backed by
a table file in the same scratch directory; tear both down afterwards."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let* ((,heap-var (create-memory
                          (namestring (merge-pathnames "heap.dat" ,dir))
                          ,size))
              (,idx-var (make-type-index
                         (namestring (merge-pathnames "type-index.dat" ,dir))
                         ,heap-var)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-type-index ,idx-var))
           (ignore-errors (close-memory ,heap-var))
           (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; UUID-key helpers (16-byte octet vectors, as used for node ids)
;;; ---------------------------------------------------------------------------

(defun key-in-list-p (uuid il)
  "True if UUID (a 16-byte octet vector) is a live member of index-list IL."
  (index-list-member-p uuid il))

(defun index-list-keys (il)
  "The live keys of index-list IL, in order."
  (map-index-list (lambda (id) (copy-seq id)) il :collect-p t))

;;; ---------------------------------------------------------------------------
;;; Full on-disk graph fixture
;;;
;;; A graph couples a heap, indexes, vertex/edge tables and a schema, all in
;;; a directory.  WITH-TEST-GRAPH builds a fresh one (of the name the
;;; integration schema is defined against) in a temp directory, binds *graph*,
;;; and tears it down (no snapshot needed for throwaway data).
;;; ---------------------------------------------------------------------------

(defparameter *integration-graph-name* :graph-db-integration-test)

(defmacro with-test-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *integration-graph-name*
                             (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Skip-list construction helper
;;;
;;; Skip lists need a heap plus a full complement of key/value
;;; serializers and comparators.  This builds an integer-keyed list with
;;; fixnum sentinels, mirroring the configuration in graph-db's own
;;; sl-test / sl-perf-test routines.
;;; ---------------------------------------------------------------------------

(defun make-integer-skip-list (heap &key duplicates-allowed-p)
  "Return an integer-keyed skip list over HEAP."
  (make-skip-list :heap heap
                  :head-key most-negative-fixnum
                  :head-value 0
                  :tail-key most-positive-fixnum
                  :tail-value 0
                  :key-equal '=
                  :key-comparison '<
                  :key-serializer 'serialize
                  :key-deserializer 'deserialize
                  :value-serializer 'serialize
                  :value-deserializer 'deserialize
                  :value-equal 'equal
                  :duplicates-allowed-p duplicates-allowed-p))
