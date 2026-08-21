(in-package :graph-db)

;;; The image-level type-id registry (GH #186).  Append-only; hosts read it,
;;; none recompute it (D14).  Keyed on the PACKAGE-QUALIFIED symbol, so two
;;; packages' same-named types never collide (cf. #190).
;;;
;;; Unlike the system clock (#182), the append lock here is taken fresh for
;;; each REGISTRY-INTERN call and held only across its read-decide-append --
;;; never for the registry's lifetime.  Opening a registry must not exclude
;;; another image from opening one too; only a concurrent assignment needs
;;; exclusion.

(defstruct (type-registry (:constructor %make-type-registry))
  (location nil)
  ;; symbol -> id, per parent.  Rebuilt from the file at open and at every
  ;; REGISTRY-INTERN re-read.
  (vertex (make-hash-table :test 'eq))
  (edge   (make-hash-table :test 'eq))
  (next-vertex 1 :type (unsigned-byte 32))
  (next-edge   1 :type (unsigned-byte 32))
  ;; Oldest-first; rebuilt alongside the hash tables.
  (entries nil :type list)
  (lock (make-recursive-lock "type registry")))

(define-condition type-registry-busy (error)
  ((location :initarg :location :reader type-registry-busy-location))
  (:report
   (lambda (c s)
     (format s "The type registry at ~A is held by another image's ~
REGISTRY-INTERN.  Retry once the holder's assignment completes (GH #186)."
             (type-registry-busy-location c)))))

(defun %registry-file (location)
  (make-pathname :name "type-registry" :type "log" :defaults location))

(defun %registry-table (registry parent)
  (ecase parent
    (:vertex (type-registry-vertex registry))
    (:edge   (type-registry-edge registry))))

;; Printing/reading always happens with *PACKAGE* bound to KEYWORD -- a
;; package nothing is ever accessible in -- so every non-keyword symbol
;; prints fully package-qualified regardless of what package happens to be
;; ambient at the call site.  Without this, CL's printer omits the package
;; prefix whenever the symbol is accessible in the CURRENT *package*, which
;; is the ordinary case for a caller registering its own type: the record
;; would round-trip correctly only by accident of which package is current
;; at read time.  That is the same defect #190 records for the per-graph
;; keyword alias, just one level up.  Round-trip review, GH #186 task 1.
(defun %registry-print-package ()
  (load-time-value (find-package "KEYWORD")))

(defun %parse-registry-record (line)
  "Read LINE as a (:SYMBOL :PARENT :ID) plist.  Signals on anything else --
trailing garbage, an unbalanced form, or a missing/wrong-typed key.
*READ-EVAL* is NIL: the file is data and must never execute."
  (let ((*read-eval* nil)
        (*package* (%registry-print-package)))
    (multiple-value-bind (form pos) (read-from-string line)
      (unless (= pos (length line))
        (error "trailing garbage in type registry record: ~S" line))
      (destructuring-bind (&key symbol parent id) form
        (unless (and (symbolp symbol)
                     (member parent '(:vertex :edge))
                     (integerp id))
          (error "malformed type registry record: ~S" line))
        (list symbol parent id)))))

(defun %registry-load (registry)
  "Rebuild REGISTRY's in-memory state from disk, oldest record first.
Tolerates a truncated FINAL record (logs and stops); signals on a malformed
record anywhere earlier -- GH #191 is the defect this must not repeat, since
the registry is the only record of what a type-id means.

Builds fresh hash tables and swaps them into REGISTRY only once fully
populated, rather than CLRHASH-ing the live ones in place: a concurrent
REGISTRY-ID-FOR (deliberately lock-free -- see its docstring) must always
see either the old, complete table or the new one, never a window where
both are empty and every symbol reads back a spurious NIL."
  (with-recursive-lock-held ((type-registry-lock registry))
    (let ((file (%registry-file (type-registry-location registry)))
          (vertex (make-hash-table :test 'eq))
          (edge   (make-hash-table :test 'eq))
          (max-vertex 0)
          (max-edge 0)
          (entries nil))
      (flet ((table-for (parent)
               (ecase parent
                 (:vertex vertex)
                 (:edge   edge))))
        (when (probe-file file)
          (with-open-file (s file :direction :input)
            (loop
              (multiple-value-bind (line missing-newline-p)
                  (read-line s nil :eof)
                (when (eq line :eof) (return))
                (let ((parsed
                        (handler-case (%parse-registry-record line)
                          (error (e)
                            (if missing-newline-p
                                (progn
                                  (log:warn "type registry: dropping torn ~
final record in ~A: ~A" file e)
                                  (return))
                                (error "malformed type registry record in ~
~A: ~A" file e))))))
                  (destructuring-bind (symbol parent id) parsed
                    (setf (gethash symbol (table-for parent)) id)
                    (push (list symbol parent id) entries)
                    (ecase parent
                      (:vertex (setf max-vertex (max max-vertex id)))
                      (:edge   (setf max-edge (max max-edge id)))))))))))
      (setf (type-registry-entries registry) (nreverse entries))
      (setf (type-registry-next-vertex registry) (1+ max-vertex))
      (setf (type-registry-next-edge registry) (1+ max-edge))
      ;; The swap: readers see VERTEX/EDGE either fully old or fully new.
      (setf (type-registry-vertex registry) vertex)
      (setf (type-registry-edge registry) edge)))
  registry)

(defun %registry-append (registry record)
  "Append RECORD (a plist) as one `~S' line and flush.  Caller holds the
append lock."
  (let ((file (%registry-file (type-registry-location registry))))
    (with-open-file (s file :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (let ((*print-readably* nil)
            (*print-pretty* nil)
            (*package* (%registry-print-package)))
        (format s "~S~%" record))
      (finish-output s))))

(defun %registry-assign (registry symbol parent)
  "Assign SYMBOL a fresh id under PARENT and persist it.  Caller holds the
append lock and has already re-checked SYMBOL is absent."
  (let ((id (ecase parent
              (:vertex (prog1 (type-registry-next-vertex registry)
                         (incf (type-registry-next-vertex registry))))
              (:edge (prog1 (type-registry-next-edge registry)
                       (incf (type-registry-next-edge registry)))))))
    (%registry-append registry (list :symbol symbol :parent parent :id id))
    (setf (gethash symbol (%registry-table registry parent)) id)
    (setf (type-registry-entries registry)
          (nconc (type-registry-entries registry)
                 (list (list symbol parent id))))
    id))

(defun open-type-registry (location)
  "Open or create the type-id registry rooted at LOCATION.  Reads the
current file once; REGISTRY-INTERN's own re-read under lock -- not this
call -- is what makes concurrent assignment safe."
  (ensure-directories-exist location)
  (let ((registry (%make-type-registry :location location)))
    (%registry-load registry)
    registry))

(defun close-type-registry (registry)
  "No persistent state to release: the append lock is per-call, not held
for the registry's lifetime (contrast the system clock, #182)."
  registry)

(defun registry-id-for (registry symbol parent)
  "The id for SYMBOL under PARENT, or NIL.  A pure read of in-memory
state -- never touches disk or the append lock, and deliberately takes no
lock of its own: %REGISTRY-LOAD's table swap (not a lock here) is what
keeps this safe to call from any thread at any time."
  (gethash symbol (%registry-table registry parent)))

(defun registry-entries (registry)
  "Every (SYMBOL PARENT ID) in REGISTRY, oldest first."
  (type-registry-entries registry))

(defun registry-intern (registry symbol parent)
  "The id for SYMBOL under PARENT, assigning one if absent.  The read-decide-
append runs under an exclusive flock: two images that both find SYMBOL absent
would otherwise assign it different ids, or one id to two symbols (#186)."
  (with-recursive-lock-held ((type-registry-lock registry))
    (or (registry-id-for registry symbol parent)
        (let* ((file (%registry-file (type-registry-location registry)))
               (fd (%posix-open file (logior +o-creat+ +o-rdwr+))))
          (unwind-protect
               (progn
                 (unless (%posix-flock fd (logior +lock-ex+ +lock-nb+))
                   (error 'type-registry-busy
                          :location (type-registry-location registry)))
                 ;; Re-read under the lock: another image may have assigned
                 ;; SYMBOL between our miss above and taking the lock.
                 (%registry-load registry)
                 (or (registry-id-for registry symbol parent)
                     (%registry-assign registry symbol parent)))
            (%posix-close fd))))))
