;;;; Master suite + runner for the GEOS integration tests.

(in-package #:graph-db/geos-test)

(def-suite geos-suite
  :description "Tests for the optional libgeos_c integration (graph-db/geos).")

(defun run-geos-tests ()
  "Run the GEOS test suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/geos)."
  (log:config :error)
  #+ecl (ext:set-limit 'ext:heap-size (* 6 1024 1024 1024))
  (let ((results (run 'geos-suite)))
    (explain! results)
    (results-status results)))

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
  (let ((dir (merge-pathnames (format nil "graph-db-geos-~A/" (scratch-tag))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

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
