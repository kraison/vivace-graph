;;;; Shared scratch-space manager for every graph-db test suite (GH #214).
;;;;
;;;; All test scratch (per-test directories, loose files, each runner's
;;;; system directory) lives under ONE lazily created per-run parent in
;;;; the temp root, so a killed or crashed run leaks exactly one tree.
;;;; Creating that parent also sweeps stale scratch left by earlier
;;;; aborted runs.  Loaded by the GRAPH-DB/TEST-SCRATCH system, which all
;;;; test systems depend on.

(defpackage #:graph-db-test-scratch
  (:use #:cl)
  (:export #:scratch-tag
           #:scratch-run-directory
           #:make-scratch-directory
           #:make-scratch-file-name
           #:cleanup-scratch-run
           #:sweep-stale-scratch
           #:*scratch-prefixes*))

(in-package #:graph-db-test-scratch)

;; SBCL's initial *RANDOM-STATE* is a fixed constant, so an unseeded
;; (RANDOM ...) yields the SAME name sequence in every image: concurrent
;; runs on a shared host would collide.  Seed from entropy, lazily so a
;; dumped image reseeds per process; the counter keeps one image from
;; ever repeating a name.
(defvar *scratch-random-state* nil)
(defvar *scratch-counter* 0)

(defun scratch-tag ()
  "A name fragment unique across concurrent processes and across calls."
  (unless *scratch-random-state*
    (setf *scratch-random-state* (make-random-state t)))
  (format nil "~36R-~36R"
          (random (expt 36 12) *scratch-random-state*)
          (incf *scratch-counter*)))

(defvar *scratch-run-directory* nil
  "The current run's scratch parent pathname, or NIL before first
use (and again after CLEANUP-SCRATCH-RUN).")

;; Temp-root entry names the stale sweep may delete: the per-run parents
;; plus every legacy flat prefix the suites ever used.  NOTHING outside
;; this list is ever touched (GH #214).
(defparameter *scratch-prefixes*
  '("graph-db-test-"       ; main suite dirs + graph-db-test-run-* parents
    "graph-db-conc-" "graph-db-acid-" "graph-db-stress-"
    "graph-db-cstress-" "graph-db-perf-" "graph-db-geos-"
    "graph-db-spacetime-" "graph-db-algo-"
    "gda-test-" "gda-io-"  ; legacy algorithms scratch
    "vgseg-" "vgquery-")   ; legacy segment-test loose files
  "Name prefixes SWEEP-STALE-SCRATCH may delete in its root.")

(defun scratch-name-matches-p (name)
  "True when NAME starts with one of *SCRATCH-PREFIXES*."
  (some (lambda (prefix)
          (let ((len (length prefix)))
            (and (>= (length name) len)
                 (string= prefix name :end2 len))))
        *scratch-prefixes*))

(defun sweep-stale-scratch (&key (root (uiop:temporary-directory))
                              (max-age-seconds (* 24 60 60)))
  "Delete ROOT entries whose names match *SCRATCH-PREFIXES* and whose
FILE-WRITE-DATE is at least MAX-AGE-SECONDS old.  Returns the count
deleted.  Each deletion is guarded, so one EPERM or race skips that
entry instead of aborting the sweep.  The age guard is what makes this
safe on a shared host: a live run's scratch is always younger than the
24h default, so never lower the threshold against the real temp root."
  ;; Accepted residual risk: an image wedged idle for over 24h can have
  ;; its live parent swept by another image -- normal activity keeps the
  ;; parent's mtime fresh via direct-child churn (GH #214).
  (let ((true-root (uiop:truename* root))
        (live (and *scratch-run-directory*
                   (uiop:truename* *scratch-run-directory*)))
        (now (get-universal-time))
        (deleted 0))
    (unless true-root
      (return-from sweep-stale-scratch 0))
    (flet ((stale-p (path)
             (let ((write-date (ignore-errors (file-write-date path))))
               (and write-date (>= (- now write-date) max-age-seconds))))
           ;; Skip symlinks: TRUENAME resolves them, so a link's truename
           ;; differs from its listed path under TRUE-ROOT.  Deleting
           ;; through one would empty its target (GH #214).
           (not-a-symlink-p (path)
             (equal (uiop:truename* path) path)))
      (dolist (dir (ignore-errors (uiop:subdirectories true-root)))
        (let ((name (first (last (pathname-directory dir)))))
          (when (and (stringp name)
                     (scratch-name-matches-p name)
                     (not (equal dir *scratch-run-directory*))
                     (not (equal dir live))
                     (not-a-symlink-p dir)
                     (stale-p dir))
            (handler-case
                (progn
                  (uiop:delete-directory-tree
                   dir :validate t :if-does-not-exist :ignore)
                  (incf deleted))
              (error () nil)))))
      (dolist (file (ignore-errors (uiop:directory-files true-root)))
        (let ((name (file-namestring file)))
          (when (and (stringp name)
                     (scratch-name-matches-p name)
                     (not-a-symlink-p file)
                     (stale-p file))
            (handler-case
                (progn (delete-file file) (incf deleted))
              (error () nil))))))
    deleted))

(defun scratch-run-directory ()
  "The current run's scratch parent, created on first call.  Creation
also sweeps the temp root for stale scratch (once per run parent --
CLEANUP-SCRATCH-RUN resets, so the next run in the same image sweeps
again).  Returns a directory pathname."
  (or *scratch-run-directory*
      (let ((dir (merge-pathnames
                  (format nil "graph-db-test-run-~A/" (scratch-tag))
                  (uiop:temporary-directory))))
        (ensure-directories-exist dir)
        (setf *scratch-run-directory* dir)
        (ignore-errors (sweep-stale-scratch))
        dir)))

(defun make-scratch-directory (&optional (prefix "graph-db-test"))
  "Create and return a fresh scratch directory PREFIX-<tag>/ under the
per-run parent.  Callers still delete it promptly; the parent catches
whatever an aborted run leaves behind."
  (let ((dir (merge-pathnames (format nil "~A-~A/" prefix (scratch-tag))
                              (scratch-run-directory))))
    (ensure-directories-exist dir)
    dir))

(defun make-scratch-file-name (prefix type)
  "A unique, not-yet-created scratch file pathname under the per-run
parent (PREFIX-<tag>.TYPE)."
  (merge-pathnames (format nil "~A-~A.~A" prefix (scratch-tag) type)
                   (scratch-run-directory)))

(defun cleanup-scratch-run ()
  "Delete this image's scratch parent (if any) and forget it, so a later
run in the same image gets a fresh one.  Safe to call when none exists.
Trap: everything under the parent dies -- close mmaps first."
  (let ((dir *scratch-run-directory*))
    (setf *scratch-run-directory* nil)
    (when dir
      (uiop:delete-directory-tree dir :validate t
                                      :if-does-not-exist :ignore))))
