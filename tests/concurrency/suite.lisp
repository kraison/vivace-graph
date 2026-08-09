;;;; Master suite, shared fixtures, and schema for concurrency tests.
;;;;
;;;; Schema types are defined once at load time against
;;;; *CONCURRENCY-GRAPH-NAME*.  Each test creates a fresh graph of that name
;;;; via WITH-CONC-GRAPH so tests are isolated from each other.

(in-package #:graph-db/concurrency-test)

;;; ---------------------------------------------------------------------------
;;; Suite root
;;; ---------------------------------------------------------------------------

(def-suite concurrency-suite
  :description "Thread-safety and concurrency tests for graph-db.")

(defun run-concurrency-tests ()
  "Run the concurrency suite.  Returns T on all-pass.
Called by (asdf:test-system :graph-db/concurrency-test)."
  (log:config :error)
  (let ((results (run 'concurrency-suite)))
    (explain! results)
    (results-status results)))

;;; ---------------------------------------------------------------------------
;;; Temp-directory and GC helpers (mirrored from graph-db/test)
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
  (let ((dir (merge-pathnames (format nil "graph-db-conc-~A/" (scratch-tag))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-temp-directory ((var) &body body)
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t :if-does-not-exist :ignore))))

(defun collect-garbage ()
  #+sbcl (sb-ext:gc :full t)
  #+ccl  (ccl:gc)
  #+lispworks (hcl:gc-all)
  #+ecl  (ext:gc t))

;;; ---------------------------------------------------------------------------
;;; Thread-count constant
;;;
;;; Keep modest on ECL (slower thread startup / teardown).
;;; ---------------------------------------------------------------------------

(defparameter *thread-count* 8)

;;; ---------------------------------------------------------------------------
;;; Schema
;;;
;;; Fresh types for the concurrency graph so these tests are independent of
;;; the integration schema in graph-db/test.  Reset the metadata list first
;;; so reloading this file does not accumulate duplicate registrations.
;;; ---------------------------------------------------------------------------

(defparameter *concurrency-graph-name* :graph-db-concurrency-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *concurrency-graph-name* *schema-node-metadata*) nil))

(def-vertex c-item ()
  ((value))
  :graph-db-concurrency-test)

(def-edge c-link ()
  ()
  :graph-db-concurrency-test)

;; Spatial vertex for the spatial concurrency suite.  A geometry slot marked
;; :index t makes the type spatially indexed via the write-path hook (no
;; hand-written node-geometry method needed).
(def-vertex c-place ()
  ((loc :type geometry :index t))
  :graph-db-concurrency-test)

;;; ---------------------------------------------------------------------------
;;; Graph fixture
;;; ---------------------------------------------------------------------------

(defmacro with-conc-graph ((g) &body body)
  "Bind G and *GRAPH* to a fresh on-disk graph; tear it down afterwards."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *concurrency-graph-name*
                             (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect
              (let ((*graph* ,g))
                ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Storage-layer fixtures (used by data-structure-tests)
;;; ---------------------------------------------------------------------------

(defmacro with-conc-lhash ((var &rest make-args) &body body)
  "Bind VAR to a fresh lhash in a temp directory; tear it down afterwards."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (make-lhash :location ,dir ,@make-args)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-lhash ,var)))))))

(defmacro with-conc-memory ((var &key (size '(* 1024 1024 64))) &body body)
  "Bind VAR to a fresh heap in a temp directory; tear it down afterwards."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,var (create-memory
                    (namestring (merge-pathnames "heap.dat" ,dir))
                    ,size)))
         (unwind-protect (progn ,@body)
           (ignore-errors (close-memory ,var)))))))

(defun make-conc-integer-skip-list (heap &key duplicates-allowed-p)
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

;;; ---------------------------------------------------------------------------
;;; View setup (called inside with-conc-graph, before any inserts)
;;; ---------------------------------------------------------------------------

(defun define-concurrency-views ()
  "Register the views used by the concurrent-view tests on *GRAPH*."
  (def-view c-item-by-value :lessp (c-item :graph-db-concurrency-test)
    (:map (lambda (item)
            (yield (slot-value item 'value) t)))))
