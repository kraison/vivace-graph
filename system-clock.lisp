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
  (journal nil))

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
the persisted ceiling, so a crash never reissues one."
  (ensure-directories-exist location)
  (let* ((ceiling (%read-clock-ceiling location))
         (clock (%make-system-clock :location location
                                    :counter ceiling
                                    :ceiling ceiling
                                    :block-size block-size)))
    (%write-clock-ceiling clock (+ ceiling block-size))
    clock))

(defun close-system-clock (clock)
  "Persist the exact counter so a clean reopen wastes no ids."
  (with-recursive-lock-held ((system-clock-lock clock))
    (%write-clock-ceiling clock (system-clock-counter clock))
    (when (system-clock-journal clock)
      (close (system-clock-journal clock))
      (setf (system-clock-journal clock) nil)))
  clock)

(defun journal-append (clock kind &rest plist)
  "Append one lifecycle record.  KIND is :CREATE :DETACH :SWAP :ATTACH or
:RETIRE.  Consumed by #170 and #171."
  (let ((record (list* :kind kind :epoch (clock-current-epoch clock) plist)))
    (with-recursive-lock-held ((system-clock-lock clock))
      (unless (system-clock-journal clock)
        (setf (system-clock-journal clock)
              (open (%clock-journal-file (system-clock-location clock))
                    :direction :output
                    :if-exists :append
                    :if-does-not-exist :create)))
      (let ((s (system-clock-journal clock)))
        (let ((*print-readably* nil) (*print-pretty* nil))
          (format s "~S~%" record))
        (finish-output s)))
    record))

(defun journal-records (clock)
  "Every lifecycle record, oldest first.  Read with evaluation disabled --
the journal is data and must never execute."
  (let ((file (%clock-journal-file (system-clock-location clock))))
    (when (system-clock-journal clock)
      (finish-output (system-clock-journal clock)))
    (when (probe-file file)
      (with-open-file (s file :direction :input)
        (let ((*read-eval* nil))
          (loop for r = (read s nil :eof)
                until (eq r :eof)
                collect r))))))

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
      (%clock-reserve clock 0)
      (values start end))))
