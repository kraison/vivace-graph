;;;; POSIX syscall wrappers.
(in-package #:graph-db/test)

(def-suite posix-suite :in graph-db-suite
  :description "Thin syscall wrappers over CFFI.")
(in-suite posix-suite)

(defun %open-rw (path)
  (graph-db::%posix-open path (logior graph-db::+o-creat+
                                      graph-db::+o-rdwr+)))

;;; ---------------------------------------------------------------------------
;;; GH #218.  open(2) is variadic; MODE must go through CFFI's varargs path.
;;;
;;; Every test above this point asserts only that the fd is valid, which is
;;; why the defect shipped: a file created with a garbage mode still yields a
;;; perfectly good descriptor to the process that created it.  The mode is the
;;; thing to assert.
;;; ---------------------------------------------------------------------------

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;; ECL has no stat wrapper; a direct C call is the same idiom the fix under
;; test uses (GH #306).
#+ecl
(ffi:clines "#include <sys/stat.h>")

#+(or sbcl ecl)
(defun %stat-mode (path)
  #+sbcl (sb-posix:stat-mode (sb-posix:stat path))
  #+ecl (ffi:c-inline ((namestring path)) (:cstring) :int
                      "{ struct stat sb; stat(#0,&sb); @(return)=sb.st_mode; }"))

#+(or sbcl ecl)
(test posix-open-creates-with-exactly-the-requested-mode
  "Apple arm64 passes variadic arguments differently from fixed ones, so a
MODE handed to open(2) as a fixed argument is read from where the callee never
wrote.  Observed 0140 and 0200 on consecutive runs for a requested 0640 --
arbitrary, and different each time, which is why an equality assertion is the
only honest one.  Ablate by restoring the plain FOREIGN-FUNCALL in
%POSIX-OPEN: this goes red on Darwin/arm64.  It cannot fail on x86_64, where
the two conventions coincide -- that platform split is the whole of GH #218.
ECL needs its own ablation: CFFI's varargs form drops MODE there too (mode
000), so %POSIX-OPEN compiles a direct C call under #+ecl -- GH #306."
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "modeprobe" dir)))
           (fd (graph-db::%posix-open path (logior graph-db::+o-creat+
                                                   graph-db::+o-rdwr+)
                                      #o640)))
      (graph-db::%posix-close fd)
      (is (= #o640 (logand #o777 (%stat-mode path)))
          "created with the requested mode, not an arbitrary one"))))

(test posix-open-creates-a-file-its-own-image-can-reopen
  "The consequence the mode bug actually had: %REGISTRY-APPEND (#186) reopens
the path it just created with an ordinary WITH-OPEN-FILE, and a mode without
owner read/write fails it EACCES -- after which no graph in the image opens at
all.  Weaker than the mode assertion above (a too-permissive mode passes) but
implementation-independent, so it holds where SB-POSIX is unavailable."
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "reopenme" dir)))
           (fd (%open-rw path)))
      (graph-db::%posix-close fd)
      (finishes
        (with-open-file (s path :direction :io :if-exists :append
                                :if-does-not-exist :error)
          (declare (ignorable s)))))))

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

(test allocated-size-sees-through-sparse-files
  "GH #274: a sparse file's allocated size tracks its data extents, not
its apparent length; a dense file reports (about) its length."
  (let ((path (namestring
               (graph-db-test-scratch:make-scratch-file-name
                "sparse" "dat"))))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-does-not-exist :create)
             ;; 64 MiB apparent, one page of data at the end.
             (file-position s (* 64 1024 1024))
             (write-sequence (make-array 4096 :element-type
                                              '(unsigned-byte 8)
                                              :initial-element 7)
                             s))
           (let ((alloc (graph-db::%posix-allocated-size path)))
             ;; NIL = filesystem without SEEK_DATA; nothing to assert.
             (when alloc
               (is (< alloc (* 8 1024 1024))
                   "allocated ~D should be far below the 64MiB apparent"
                   alloc)
               (is (plusp alloc)))))
      (ignore-errors (delete-file path)))))
