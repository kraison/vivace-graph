(in-package :graph-db)

;;; The image-level epoch clock (GH #168).  One clock per *system* -- a
;;; directory of stores -- rather than one per store, so a query spanning
;;; stores resolves at one instant.  See
;;; docs/superpowers/specs/2026-08-20-namespaces-design.md §6.

(defvar *system-clock* nil
  "The image's SYSTEM-CLOCK, or NIL.  NIL means every store keeps its own
transaction-id counter, which is the pre-#168 behaviour and the default.")

(defstruct (system-clock (:constructor %make-system-clock))
  (location nil)
  ;; Next epoch to hand out.  In memory; CEILING is what disk guarantees.
  (counter 0 :type (unsigned-byte 64))
  ;; Disk holds this value; every issued id is strictly below it.  A crash
  ;; therefore cannot reissue: reopen resumes at the persisted ceiling.
  (ceiling 0 :type (unsigned-byte 64))
  (block-size 4096 :type (unsigned-byte 32))
  (lock (make-recursive-lock "system clock"))
  (journal nil)
  ;; Held open for the clock's lifetime; the kernel releases it on process
  ;; death, so a stale lock cannot happen (GH #182).
  (lock-fd nil))

(define-condition system-clock-in-use (error)
  ((location :initarg :location :reader system-clock-in-use-location))
  (:report
   (lambda (c s)
     (format s "The system clock at ~A is held by another process.  Only one ~
image may allocate epochs for a system; a second would issue epochs colliding ~
with the holder's (GH #182).  A lease-holding process must not open the clock ~
directory -- see the design's §8.1."
             (system-clock-in-use-location c)))))

(defun %clock-lock-file (location)
  (make-pathname :name "system-clock" :type "lock" :defaults location))

(defun %clock-counter-file (location)
  (make-pathname :name "system-clock" :type "dat" :defaults location))

(defun %clock-journal-file (location)
  (make-pathname :name "system-journal" :type "log" :defaults location))

(defun %write-clock-ceiling (clock value)
  "Persist VALUE as the durable ceiling.  Every id issued is < VALUE.
:OVERWRITE, not :SUPERSEDE: the file is always 8 bytes, so a crash
mid-write can't leave it short or absent (cf. transactions.lisp:939)."
  (let ((buf (make-byte-vector 8)))
    (serialize-uint64 buf value 0)
    (with-open-file (s (%clock-counter-file (system-clock-location clock))
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-does-not-exist :create
                       :if-exists :overwrite)
      (write-sequence buf s)
      ;; Flushes to the OS, not FSYNC: survives a process crash, not a
      ;; power loss -- same limitation as the other counter writers.
      (finish-output s)))
  (setf (system-clock-ceiling clock) value))

(defun %read-clock-ceiling (location)
  (let ((file (%clock-counter-file location))
        (buf (make-byte-vector 8)))
    (if (probe-file file)
        (with-open-file (s file :direction :input
                                :element-type '(unsigned-byte 8))
          (unless (= 8 (read-sequence buf s))
            (error "Short read from ~A" file))
          (deserialize-uint64 buf 0))
        0)))

(defun open-system-clock (location &key (block-size 4096))
  "Open or create the system clock in directory LOCATION.  Ids resume above
the persisted ceiling, so a crash never reissues one.  Signals
SYSTEM-CLOCK-IN-USE if another live process holds LOCATION (GH #182)."
  (ensure-directories-exist location)
  (let ((fd (%posix-open (%clock-lock-file location)
                         (logior +o-creat+ +o-rdwr+)))
        (opened nil))
    (unwind-protect
         (progn
           (unless (handler-case
                       (%posix-flock fd (logior +lock-ex+ +lock-nb+))
                     ;; A real failure (EBADF, ENOLCK) reports an fd number and
                     ;; a raw errno; name the directory it was for.
                     (error (e)
                       (error "Cannot lock the system clock at ~A: ~A"
                              location e)))
             (error 'system-clock-in-use :location location))
           (let* ((ceiling (%read-clock-ceiling location))
                  (clock (%make-system-clock :location location
                                             :counter ceiling
                                             :ceiling ceiling
                                             :block-size block-size
                                             :lock-fd fd)))
             (%write-clock-ceiling clock (+ ceiling block-size))
             (setf opened t)
             clock))
      (unless opened (%posix-close fd)))))

(defun close-system-clock (clock)
  "Persist the exact counter so a clean reopen wastes no ids.  Releases the
directory lock even if that persist fails: a stranded lock would refuse every
later open in this image, for the life of the process (GH #182)."
  (with-recursive-lock-held ((system-clock-lock clock))
    (unwind-protect
         (progn
           (%write-clock-ceiling clock (system-clock-counter clock))
           (when (system-clock-journal clock)
             (close (system-clock-journal clock))
             (setf (system-clock-journal clock) nil)))
      (when (system-clock-lock-fd clock)
        ;; Closing the fd is the release; there is no LOCK_UN path.
        (%posix-close (system-clock-lock-fd clock))
        (setf (system-clock-lock-fd clock) nil))))
  clock)

(defun journal-append (clock kind &rest plist)
  "Append one lifecycle record.  KIND is :CREATE :DETACH :SWAP :ATTACH
:RETIRE :RETIRE-LIVE :RESTORE or :SWAP-ABORTED (the last four: GH #171)."
  (let ((record (list* :kind kind :epoch (clock-current-epoch clock) plist)))
    (with-recursive-lock-held ((system-clock-lock clock))
      (unless (system-clock-journal clock)
        (setf (system-clock-journal clock)
              (open (%clock-journal-file (system-clock-location clock))
                    :direction :output
                    :if-exists :append
                    :if-does-not-exist :create)))
      (let ((s (system-clock-journal clock)))
        ;; Full print-control set (GH #226); the #191 corruption/torn-
        ;; tail machinery below is untouched -- this only closes the
        ;; gap where an ambient *PRINT-LENGTH*/*PRINT-BASE* etc. could
        ;; produce a line JOURNAL-RECORDS then can't read back.
        (with-sidecar-output ()
          (format s "~S~%" record))
        (finish-output s)))
    record))

(define-condition system-journal-corrupt (error)
  ((file :initarg :file :reader journal-corrupt-file)
   (position :initarg :position :reader journal-corrupt-position)
   (cause :initarg :cause :reader journal-corrupt-cause))
  (:report
   (lambda (c s)
     (format s "System journal ~A is unreadable at byte ~D, and intact ~
records FOLLOW the damage, so this is not a torn tail from a power loss ~
-- the file is corrupt and no record can be trusted (GH #191).  ~
Underlying reader condition: ~A"
             (journal-corrupt-file c)
             (journal-corrupt-position c)
             (journal-corrupt-cause c)))))

(define-condition system-journal-torn-tail (warning)
  ((file :initarg :file :reader journal-torn-file)
   (position :initarg :position :reader journal-torn-position))
  (:report
   (lambda (c s)
     (format s "System journal ~A ends in a torn record at byte ~D -- ~
one lifecycle event was in flight during a power loss.  Dropping the ~
tail; the preceding history is intact (GH #191)."
             (journal-torn-file c)
             (journal-torn-position c)))))

(defun %journal-later-record-p (file pos)
  "True when FILE holds a complete readable record after the failed read
at byte POS -- corruption mid-file, not a torn tail.  Skips forward a
line at a time and retries; a torn TAIL by definition has nothing
readable after it.  Stream position after a failed READ is unspecified,
so the next READ-LINE may re-skip part of the bad text -- harmless: the
scan only has to find ONE later record or reach EOF (GH #191).  If the
damage destroys the newline so the last GOOD record shares a line with
leading garbage, the line-at-a-time scan skips both and truncation drops
that good record too -- outside #191's power-loss model, where a torn
append is always a strict suffix with the prior newline intact."
  (with-open-file (s file :direction :input)
    (file-position s pos)
    (let ((*read-eval* nil))
      (loop
        (unless (read-line s nil) (return nil))
        (handler-case
            (let ((r (read s nil :eof)))
              (return (not (eq r :eof))))
          (error () nil))))))

(defun %journal-truncate-to (clock file pos)
  "Drop everything at and after byte POS: write the good prefix to a temp
file and RENAME-FILE over the original, so a crash here cannot lose the
intact history too.  Closes the append stream first -- after the rename
it would still reference the OLD inode and later appends would vanish.
Caller holds the clock lock AND the directory flock (SYSTEM-CLOCK-LOCK-FD
non-nil) -- a stale CLOCK whose lock was released by CLOSE-SYSTEM-CLOCK
must never call this: another process may hold the real lock and be
mid-append, and renaming out from under it silently loses every record
appended after the rename (GH #182, #191).  POS is octets: SBCL's
FILE-POSITION on character streams reports octet offsets, which the
multibyte test pins (GH #191)."
  (when (system-clock-journal clock)
    (close (system-clock-journal clock))
    (setf (system-clock-journal clock) nil))
  (let ((tmp (make-pathname :type "tmp" :defaults file))
        (buf (make-byte-vector pos)))
    (with-open-file (in file :direction :input
                             :element-type '(unsigned-byte 8))
      (unless (= pos (read-sequence buf in))
        (error "Short read rewriting ~A" file)))
    (with-open-file (out tmp :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
      (write-sequence buf out)
      (finish-output out))
    ;; Replaces the target (POSIX rename semantics on SBCL).
    (rename-file tmp file)))

(defun journal-records (clock)
  "Every lifecycle record, oldest first, as (values RECORDS TORN-P).
Read with evaluation disabled -- the journal is data and must never
execute.  A torn FINAL record (power loss mid-append) is dropped: the
tail is truncated, SYSTEM-JOURNAL-TORN-TAIL is warned, and the intact
history is returned with TORN-P true.  Damage anywhere ELSE signals
SYSTEM-JOURNAL-CORRUPT and touches nothing (GH #191).

Truncation only runs while CLOCK holds the directory flock
(SYSTEM-CLOCK-LOCK-FD non-nil) -- after CLOSE-SYSTEM-CLOCK the lock is
released and another process may own the file and be mid-append; an
unowned CLOCK still warns and returns TORN-P true but leaves the file
untouched, so it re-warns on every read until an owning reader truncates
it.  That repeated warning is the accepted cost (GH #182, #191)."
  (let ((file (%clock-journal-file (system-clock-location clock))))
    (with-recursive-lock-held ((system-clock-lock clock))
      (when (system-clock-journal clock)
        (finish-output (system-clock-journal clock)))
      (when (probe-file file)
        (with-open-file (s file :direction :input)
          (with-sidecar-input ()
            (let ((records nil)
                  (torn-p nil))
              (loop
                (let* ((pos (file-position s))
                       (r (handler-case (read s nil :eof)
                            (error (e)
                              (when (%journal-later-record-p file pos)
                                (error 'system-journal-corrupt
                                       :file file :position pos :cause e))
                              (when (system-clock-lock-fd clock)
                                (%journal-truncate-to clock file pos))
                              (setq torn-p t)
                              (warn 'system-journal-torn-tail
                                    :file file :position pos)
                              :eof))))
                  (when (eq r :eof)
                    (return (values (nreverse records) torn-p)))
                  (push r records))))))))))

(defun %clock-reserve (clock needed)
  "Raise the durable ceiling so COUNTER + NEEDED stays below it.  Caller
holds the lock."
  (let ((target (+ (system-clock-counter clock) needed)))
    (when (>= target (system-clock-ceiling clock))
      (%write-clock-ceiling
       clock (+ target (system-clock-block-size clock))))))

(defun clock-next-epoch (clock)
  "Allocate and return a fresh epoch."
  (with-recursive-lock-held ((system-clock-lock clock))
    (%clock-reserve clock 1)
    (prog1 (system-clock-counter clock)
      (incf (system-clock-counter clock)))))

(defun clock-current-epoch (clock)
  "The next epoch CLOCK-NEXT-EPOCH would return."
  (with-recursive-lock-held ((system-clock-lock clock))
    (system-clock-counter clock)))

(defun clock-peek-epoch (clock)
  "The counter without taking the lock.  Monotonic, so a stale (smaller)
value only makes the reaper more conservative -- never less.  For
PIN-READ-EPOCH; use CLOCK-CURRENT-EPOCH where exactness matters."
  (system-clock-counter clock))

(defun clock-observe-epoch (clock epoch)
  "Raise CLOCK so it strictly exceeds EPOCH.  Monotonic and idempotent; a
lower EPOCH is a no-op.  Foreign epochs reach here from peer sync, so the
clock is not purely local -- see spec §6.  EPOCH may be NIL (a no-op):
callers like PEER-OBSERVE-EPOCH may not always have one to report."
  (with-recursive-lock-held ((system-clock-lock clock))
    (when (and epoch (>= epoch (system-clock-counter clock)))
      (setf (system-clock-counter clock) (1+ epoch))
      (%clock-reserve clock 0))
    (system-clock-counter clock)))

(defun clock-lease-epochs (clock n)
  "Reserve N epochs for a detached store and skip the clock past them.
Returns (values START END); the holder allocates in [START, END)."
  (with-recursive-lock-held ((system-clock-lock clock))
    (%clock-reserve clock n)
    (let* ((start (system-clock-counter clock))
           (end (+ start n)))
      (setf (system-clock-counter clock) end)
      (values start end))))
