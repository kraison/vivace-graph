;;;; Master suite + runner for the spacetime tests.

(in-package #:graph-db/spacetime-test)

(def-suite spacetime-suite
  :description "Temporal extents, the Allen algebra, and standing (#130).")

(defun run-spacetime-tests ()
  "Run the spacetime suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/spacetime)."
  (log:config :error)
  (let ((results (run 'spacetime-suite)))
    (explain! results)
    (results-status results)))

(defun exact-interval (s e)
  "An interval extent with exact endpoints.  Three lines, duplicated from
cl-temporal-extent's own suite rather than shared: coupling two test suites
so one can borrow a fixture costs more than the duplication (#159)."
  (make-interval (exact-bound s) (exact-bound e)))

(defun ts (year month day &optional (hour 0) (minute 0) (sec 0) (nsec 0))
  "A UTC timestamp.  Every test builds times through this, so none of them
can accidentally depend on the host timezone (design §3.5)."
  (encode-timestamp nsec sec minute hour day month year
                    :timezone +utc-zone+))

;;; ---------------------------------------------------------------------------
;;; Temp-directory + GC fixtures for claim-tests.lisp (GH #131).
;;;
;;; Self-contained rather than depending on GRAPH-DB/TEST, which would pull in
;;; the whole GRAPH-DB system plus every core test file for two helpers --
;;; the same call GRAPH-DB/GEOS-TEST makes (tests/geos/suite.lisp).
;;; ---------------------------------------------------------------------------

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
  (let ((dir (merge-pathnames
              (format nil "graph-db-spacetime-~A/" (scratch-tag))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defmacro with-temp-directory ((var) &body body)
  "Bind VAR to a fresh scratch directory, run BODY, then delete the tree."
  `(let ((,var (make-temp-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t
                                        :if-does-not-exist :ignore))))

(defun collect-garbage ()
  "Force a full GC between graph-backed tests (mirrors GRAPH-DB/TEST)."
  #+sbcl (sb-ext:gc :full t)
  #+ccl (ccl:gc)
  #+lispworks (hcl:gc-all)
  #+ecl (ext:gc t))
