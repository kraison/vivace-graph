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

(test data-extents-walk-the-file
  "#434: %POSIX-DATA-EXTENTS lists (START . END) data runs and the file
length; an empty file has none, a dense page is one run, data at both
ends of a hole is two.  The second value is NIL only where SEEK_DATA is
unsupported, never on Linux."
  (let ((path (namestring
               (graph-db-test-scratch:make-scratch-file-name
                "extents" "dat")))
        (page (make-array 4096 :element-type '(unsigned-byte 8)
                               :initial-element 9))
        (mib (* 1024 1024)))
    (flet ((extents ()
             (multiple-value-bind (extents length supported)
                 (graph-db::%posix-data-extents path)
               #+graph-db-posix-linux
               (is-true supported "SEEK_DATA unsupported?")
               (values (and supported extents) length supported))))
      (unwind-protect
           (progn
             (with-open-file (s path :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-does-not-exist :create))
             (multiple-value-bind (extents length) (extents)
               (is (null extents) "empty file: ~S" extents)
               (is (eql 0 length)))
             (with-open-file (s path :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
               (write-sequence page s))
             (multiple-value-bind (extents length supported) (extents)
               (when supported
                 (is (equal '((0 . 4096)) extents) "dense: ~S" extents))
               (is (eql 4096 length)))
             (with-open-file (s path :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
               (write-sequence page s)
               (file-position s (* 64 mib))
               (write-sequence page s)
               ;; A zero byte at the end allocates a block, as the
               ;; engine's %POSIX-EXTEND-FILE-BACKING does: a third run.
               (file-position s (1- (* 80 mib)))
               (write-byte 0 s))
             (multiple-value-bind (extents length supported) (extents)
               (is (eql (* 80 mib) length))
               (when supported
                 (is (= 3 (length extents)) "three runs: ~S" extents)
                 (is (equal '(0 . 4096) (first extents)))
                 (is (and (<= (car (second extents)) (* 64 mib))
                          (<= (+ (* 64 mib) 4096) (cdr (second extents))))
                     "second run should cover the page at 64 MiB: ~S"
                     (second extents))
                 (is (< (cdr (second extents)) (* 65 mib))
                     "the hole after it is not data: ~S" extents)
                 (is (eql (* 80 mib) (cdr (third extents))))))
             ;; ftruncate makes a hole with no block behind it.
             (%truncate-file path (* 96 mib))
             (multiple-value-bind (extents length supported) (extents)
               (is (eql (* 96 mib) length))
               (when supported
                 (is (= 3 (length extents)) "a trailing hole adds no run")
                 (is (eql (* 80 mib) (cdr (third extents)))))))
        (ignore-errors (delete-file path))))))

(test allocated-size-sees-through-sparse-files
  "GH #274: a sparse file's allocated size tracks its data extents, not
its apparent length; a dense file reports its length, an empty file 0.
NIL means a filesystem without SEEK_DATA, which no Linux filesystem in
use here is -- there it is a failure, not a skip (#438)."
  (let ((path (namestring
               (graph-db-test-scratch:make-scratch-file-name
                "sparse" "dat")))
        (page (make-array 4096 :element-type '(unsigned-byte 8)
                               :initial-element 7)))
    (flet ((allocated ()
             (let ((alloc (graph-db::%posix-allocated-size path)))
               #+graph-db-posix-linux
               (is-true alloc "the SEEK_DATA walk answered NIL")
               alloc)))
      (unwind-protect
           (progn
             (with-open-file (s path :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-does-not-exist :create))
             (let ((alloc (allocated)))
               (when alloc (is (eql 0 alloc) "empty file: ~D" alloc)))
             (with-open-file (s path :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
               (write-sequence page s))
             (let ((alloc (allocated)))
               (when alloc (is (eql 4096 alloc) "dense page: ~D" alloc)))
             (with-open-file (s path :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
               ;; 64 MiB apparent, one page of data at the end.
               (file-position s (* 64 1024 1024))
               (write-sequence page s))
             (let ((alloc (allocated)))
               (when alloc
                 (is (< alloc (* 8 1024 1024))
                     "allocated ~D should be far below the 64MiB apparent"
                     alloc)
                 (is (plusp alloc)))))
        (ignore-errors (delete-file path))))))
