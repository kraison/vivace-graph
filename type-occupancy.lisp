(in-package :graph-db)

;;; Edge store-occupancy sidecar (GH #167, spec R4).  An image-level HINT
;;; of which stores hold which edge classes -- maintained on write, never
;;; the source of truth.  Absent file, no registry, or unknown class all
;;; answer NIL ("no hint, sweep everything"): a stale or missing sidecar
;;; costs a wasted lookup, never a wrong answer.

(defvar *edge-occupancy* (make-hash-table :test 'eq)
  "CLASS-SYMBOL -> list of store names known to hold that edge class.
Loaded lazily from EDGE-OCCUPANCY.DAT on first use in this image.")

(defvar *edge-occupancy-loaded-p* nil
  "T once *EDGE-OCCUPANCY* has been populated from disk this image.")

(defvar *edge-occupancy-loaded-file* nil
  "The pathname *EDGE-OCCUPANCY* was last loaded from (or NIL for
in-image-only).  Mirrors ENSURE-TYPE-REGISTRY's own re-open-on-change
check: tests rebind *SYSTEM-DIRECTORY* per store, and without this the
cache would keep answering for whatever directory was current first.")

(defvar *edge-occupancy-lock* (make-lock "edge occupancy"))

(defun %edge-occupancy-file ()
  "EDGE-OCCUPANCY.DAT beside the type registry, or NIL when no system
directory is configured, or any other failure (R4 fallback: in-image
only -- this must never signal, GH #167)."
  (handler-case
      (make-pathname :name "edge-occupancy" :type "dat"
                     :defaults (type-registry-location
                                (ensure-type-registry)))
    (error () nil)))

(defun %edge-occupancy-print-package ()
  ;; COMMON-LISP, not KEYWORD: class symbols must print package-qualified
  ;; (the #171 manifest discipline), and KEYWORD cannot hold them.
  (load-time-value (find-package "COMMON-LISP")))

(defun %parse-edge-occupancy-line (line)
  "Read LINE as a (NAME STORE) list.  *READ-EVAL* NIL: the file is data.
Returns NIL on anything malformed -- a torn or corrupt line is only a
lost hint, never an error."
  (handler-case
      (let ((*read-eval* nil))
        (multiple-value-bind (form pos) (read-from-string line)
          (when (and (= pos (length line))
                     (consp form) (symbolp (first form))
                     (= (length form) 2))
            form)))
    (error () nil)))

(defun %load-edge-occupancy (file)
  "Populate *EDGE-OCCUPANCY* from FILE, if it exists.  Caller holds
*EDGE-OCCUPANCY-LOCK*.  Any read failure (FILE is a directory, a
permission error, a race with an external deletion after PROBE-FILE)
degrades to no-hint, matching R4 -- this must never signal into a real
write (GH #167)."
  (clrhash *edge-occupancy*)
  (when (and file (probe-file file))
    (handler-case
        (with-open-file (s file :direction :input)
          (loop
            (let ((line (read-line s nil :eof)))
              (when (eq line :eof) (return))
              (let ((parsed (%parse-edge-occupancy-line line)))
                (when parsed
                  (destructuring-bind (name store) parsed
                    (pushnew store (gethash name *edge-occupancy*)
                             :test 'equal)))))))
      (error () (clrhash *edge-occupancy*))))
  (setf *edge-occupancy-loaded-file* file)
  (setf *edge-occupancy-loaded-p* t))

(defun %ensure-edge-occupancy-loaded ()
  (let ((file (%edge-occupancy-file)))
    (unless (and *edge-occupancy-loaded-p*
                 (equal *edge-occupancy-loaded-file* file))
      (%load-edge-occupancy file))))

(defun %note-edge-occupancy (name store)
  "Record that edge class NAME has been instantiated into STORE, if the
pair is new.  Appends one `~S'-printed (NAME STORE) line to
EDGE-OCCUPANCY.DAT when a system directory is configured; otherwise the
hint lives in-image only for this session (R4 fallback)."
  (with-lock-held (*edge-occupancy-lock*)
    (%ensure-edge-occupancy-loaded)
    (unless (member store (gethash name *edge-occupancy*) :test 'equal)
      (push store (gethash name *edge-occupancy*))
      (let ((file (%edge-occupancy-file)))
        (when file
          ;; The in-image PUSH above already stands; a failed append (disk
          ;; full, permissions, fd exhaustion) must not propagate into the
          ;; caller's real edge write -- it only means the hint stays
          ;; in-image-only for this session (GH #167, R4, review round 1).
          (handler-case
              (with-open-file (s file :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create)
                (let ((*print-readably* nil)
                      (*print-pretty* nil)
                      (*package* (%edge-occupancy-print-package)))
                  (format s "~S~%" (list name store)))
                (finish-output s))
            (error () nil))))))
  (values))

(defun edge-type-stores (name)
  "The store names known to hold NAME edges, or NIL when there is no
hint -- meaning either NAME has never been instantiated in this image
(or its sidecar), or NAME is not a known edge class.  Never signals: a
missing/unreadable sidecar is exactly the fail-safe case (GH #167)."
  (with-lock-held (*edge-occupancy-lock*)
    (%ensure-edge-occupancy-loaded)
    (copy-list (gethash name *edge-occupancy*))))

(defun %clear-edge-occupancy-cache ()
  "Reset the in-image occupancy state.  Tests use this after rebinding
*SYSTEM-DIRECTORY* so the next lookup re-reads from the new location."
  (with-lock-held (*edge-occupancy-lock*)
    (clrhash *edge-occupancy*)
    (setf *edge-occupancy-loaded-p* nil)
    (setf *edge-occupancy-loaded-file* nil))
  (values))
