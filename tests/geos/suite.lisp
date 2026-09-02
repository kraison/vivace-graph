;;;; Master suite + runner for the GEOS integration tests.

(in-package #:graph-db/geos-test)

(def-suite geos-suite
  :description "Tests for the optional libgeos_c integration (graph-db/geos).")

(defun run-geos-tests ()
  "Run the GEOS test suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/geos)."
  (log:config :error)
  #+ecl (ext:set-limit 'ext:heap-size (* 6 1024 1024 1024))
  ;; Type-ids come from the system-wide registry, so every store this suite
  ;; opens needs a system directory (GH #186).  One for the whole run, which
  ;; is the shape a real system has: many stores, one registry.
  (let* ((system-dir (make-temp-directory))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'geos-suite)))
           (explain! results)
           (results-status results))
      ;; system-dir and all test scratch live under the shared per-run
      ;; parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run))))

;;; A handful of tests need to run with GEOS forced unavailable to prove the
;;; fallback path.  This binds the flag off for the dynamic extent of BODY.
(defmacro without-geos (&body body)
  `(let ((*geos-available-p* nil)
         (*geos-makevalid-available-p* nil))
     ,@body))

;;; --------------------------------------------------------------------------
;;; Graph fixture + geometry vertex type for the S3 query tests.
;;; --------------------------------------------------------------------------

(defparameter *geos-graph-name* :graph-db-geos-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *geos-graph-name* graph-db::*schema-node-metadata*) nil))

;; A geometry slot marked :index t makes the type spatially indexed.
(def-vertex geos-place ()
  ((geom :type graph-db::geometry :index t))
  :graph-db-geos-test)

(defun make-temp-directory ()
  "A fresh scratch dir under the shared per-run parent (GH #214)."
  (graph-db-test-scratch:make-scratch-directory "graph-db-geos"))

(defmacro with-geos-graph ((g) &body body)
  "Bind G + *graph* to a fresh on-disk graph; tear it down afterwards."
  (let ((dir (gensym "DIR")))
    `(let ((,dir (make-temp-directory)))
       (unwind-protect
            (let ((,g (make-graph *geos-graph-name* (namestring ,dir)
                                  :buffer-pool-size 1000)))
              (unwind-protect (let ((*graph* ,g)) ,@body)
                (ignore-errors (close-graph ,g :snapshot-p nil))))
         (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore)))))
