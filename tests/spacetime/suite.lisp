;;;; Master suite + runner for the spacetime tests.

(in-package #:graph-db/spacetime-test)

(def-suite spacetime-suite
  :description "Temporal extents, the Allen algebra, and standing (#130).")

(defun run-spacetime-tests ()
  "Run the spacetime suite.  Returns T when every test passed.
Invoked by (asdf:test-system :graph-db/spacetime)."
  (log:config :error)
  ;; Type-ids come from the system-wide registry, so every store this suite
  ;; opens needs a system directory (GH #186).  One for the whole run, which
  ;; is the shape a real system has: many stores, one registry.
  (let* ((system-dir (make-temp-directory))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil))
    (unwind-protect
         (let ((results (run 'spacetime-suite)))
           (explain! results)
           (results-status results))
      ;; system-dir and all test scratch live under the shared per-run
      ;; parent; drop it whole (GH #214).
      (graph-db-test-scratch:cleanup-scratch-run))))

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

(defun make-temp-directory ()
  "A fresh scratch dir under the shared per-run parent (GH #214)."
  (graph-db-test-scratch:make-scratch-directory "graph-db-spacetime"))

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
