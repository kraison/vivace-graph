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
  ;; Type-ids come from the system-wide registry, so a system directory is
  ;; mandatory for every store the suite opens (GH #186).  One directory for
  ;; the whole run, which is the shape a real system has: many stores, one
  ;; registry.  Tests that need their own bind GRAPH-DB::*SYSTEM-DIRECTORY*
  ;; themselves.
  (let* ((system-dir (make-temp-directory))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'graph-db-suite)))
           (explain! results)
           (results-status results))
      ;; Everything this run scratched -- system-dir included -- lives
      ;; under the shared per-run parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run))))

;;; ---------------------------------------------------------------------------
;;; Temp-file fixtures
;;;
;;; The storage layers all live in mmap'd files, so each test needs a
;;; private scratch directory that is reliably torn down afterwards.
;;; All scratch lives under GRAPH-DB-TEST-SCRATCH's per-run parent, which
;;; also sweeps stale trees from killed runs (GH #214).
;;; ---------------------------------------------------------------------------

(defun make-temp-directory ()
  "Create and return a fresh, unique scratch directory pathname."
  (graph-db-test-scratch:make-scratch-directory "graph-db-test"))

(defun make-temp-file-name (prefix type)
  "A unique, not-yet-created scratch file pathname (PREFIX-<tag>.TYPE)."
  (graph-db-test-scratch:make-scratch-file-name prefix type))

(defmacro with-temp-directory ((var) &body body)
  "Bind VAR to a fresh scratch directory, run BODY, then delete the tree."
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun collect-garbage ()
  "Force a full GC.  This reclaims Lisp-heap objects only -- index-list
structs, buffer-pool entries, node instances -- not mmap'd regions (those
are freed by MUNMAP-FILE, not GC).  Each graph creates plenty of the former
per test; without reclaiming between tests, a whole suite run in one image
exhausts the default heap.  (Before #166, a type-index alone preallocated
65536 index-list structs per type table -- no longer true, but many tests
per image still adds up.)"
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
