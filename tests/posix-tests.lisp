;;;; POSIX syscall wrappers.
(in-package #:graph-db/test)

(def-suite posix-suite :in graph-db-suite
  :description "Thin syscall wrappers over CFFI.")
(in-suite posix-suite)

(defun %open-rw (path)
  (graph-db::%posix-open path (logior graph-db::+o-creat+
                                      graph-db::+o-rdwr+)))

(test flock-denies-a-second-open-file-description
  "flock locks attach to the open file description, not the process, so two
OPEN(2) calls in one image contend exactly as two processes would.  That is
what makes GH #182's guard testable without spawning a child."
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "lockme" dir)))
           (a (%open-rw path))
           (b (%open-rw path))
           (op (logior graph-db::+lock-ex+ graph-db::+lock-nb+)))
      (unwind-protect
           (progn
             (is-true (graph-db::%posix-flock a op)
                      "the first descriptor takes the lock")
             (is (null (graph-db::%posix-flock b op))
                 "the second is denied, and denial is NIL, not an error"))
        (graph-db::%posix-close a)
        (graph-db::%posix-close b)))))

(test flock-signals-rather-than-reporting-held-on-a-real-error
  "Acceptance criterion: a genuine failure must stay distinguishable from a
held lock.  EBADF on an invalid descriptor is the cheapest way to reach the
signalling branch -- if %ERRNO cannot report on this implementation, this test
is what catches the resulting misreport."
  (signals error (graph-db::%posix-flock -1 (logior graph-db::+lock-ex+
                                                    graph-db::+lock-nb+))))

(test flock-releases-on-close
  "Close is the only release path the clock uses -- CLOSE-SYSTEM-CLOCK closes
the fd rather than calling LOCK_UN -- so this is the release semantics that
matter."
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "lockme" dir)))
           (a (%open-rw path))
           (op (logior graph-db::+lock-ex+ graph-db::+lock-nb+)))
      ;; A is closed mid-test -- that close IS the release under test -- so
      ;; the cleanup is conditional rather than unconditional, or a clean run
      ;; would double-close.  Without the guard, a real error from
      ;; %POSIX-FLOCK exits non-locally past the close and leaks a held lock
      ;; into the rest of the run.
      (unwind-protect
           (progn
             (is-true (graph-db::%posix-flock a op))
             (graph-db::%posix-close a)
             (setf a nil)
             (let ((b (%open-rw path)))
               (unwind-protect
                    (is-true (graph-db::%posix-flock b op)
                             "closing the holder frees the lock")
                 (graph-db::%posix-close b))))
        (when a (graph-db::%posix-close a))))))
