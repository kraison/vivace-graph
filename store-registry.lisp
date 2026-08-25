(in-package :graph-db)

;;; The image-level store-id registry (GH #169).  One stable numeric id
;;; per graph-name, 1..4095, never reused: the id rides inside every v8
;;; node id, so reuse would rebind existing ids to the wrong store.
;;; Append-only; same idioms as the type registry (GH #186, #191).

(defconstant +store-tag-bits+ 12)
(defconstant +max-store-tag+ 4095)

(defstruct (store-registry (:constructor %make-store-registry))
  (location nil)
  ;; name (symbol or string) -> id.  EQUAL: string names compare by
  ;; content (cf. the strchk fixtures), symbols by identity.
  (names (make-hash-table :test 'equal))
  (next 1 :type (unsigned-byte 16))
  (lock (make-recursive-lock "store registry")))

(define-condition store-registry-full (error)
  ((location :initarg :location :reader store-registry-full-location))
  (:report
   (lambda (c s)
     (format s "The store registry at ~A has assigned all ~D ids.  The ~
v8 store tag is ~D bits (GH #169); widening it is a format change."
             (store-registry-full-location c)
             +max-store-tag+ +store-tag-bits+))))

(defvar *store-registry* nil)
(defvar *store-registry-open-lock* (make-lock "store registry open"))

(defun %store-registry-file (location)
  (make-pathname :name "store-registry" :type "log" :defaults location))

(defun %store-registry-lock-file (location)
  (make-pathname :name "store-registry" :type "lock" :defaults location))

(defun %parse-store-registry-record (line)
  "LINE as (:NAME <symbol-or-string> :ID <int>).  WITH-SIDECAR-INPUT
binds the full reader-control set, KEYWORD-packaged (GH #226):
*READ-EVAL* nil, data never code (GH #169)."
  (with-sidecar-input (:package :keyword)
    (multiple-value-bind (form pos) (read-from-string line)
      (unless (= pos (length line))
        (error "trailing garbage in store registry record: ~S" line))
      (destructuring-bind (&key name id) form
        (unless (and (or (symbolp name) (stringp name)) (integerp id))
          (error "malformed store registry record: ~S" line))
        (list name id)))))

(defun %store-registry-load (registry)
  "Rebuild from disk.  Torn FINAL record: drop and warn; damage earlier:
signal (the #191 policy -- this file is the only record of what a store
tag means)."
  (with-recursive-lock-held ((store-registry-lock registry))
    (let ((file (%store-registry-file (store-registry-location registry)))
          (names (make-hash-table :test 'equal))
          (max-id 0))
      (when (probe-file file)
        (with-open-file (s file :direction :input)
          (loop
            (multiple-value-bind (line missing-newline-p)
                (read-line s nil :eof)
              (when (eq line :eof) (return))
              (let ((parsed
                      (handler-case (%parse-store-registry-record line)
                        (error (e)
                          (if missing-newline-p
                              (progn
                                (log:warn "store registry: dropping torn ~
final record in ~A: ~A" file e)
                                (return))
                              (error "malformed store registry record ~
in ~A: ~A" file e))))))
                (destructuring-bind (name id) parsed
                  (setf (gethash name names) id)
                  (setf max-id (max max-id id))))))))
      (setf (store-registry-next registry) (1+ max-id))
      ;; Swap, never clrhash: lock-free readers see old-complete or new.
      (setf (store-registry-names registry) names)))
  registry)

(defun ensure-store-registry ()
  "The image's store registry, opening it on first use and reopening
whenever *SYSTEM-DIRECTORY* changes (tests rebind it per store; mirrors
ENSURE-TYPE-REGISTRY, #186).  Signals SYSTEM-DIRECTORY-REQUIRED without
*SYSTEM-DIRECTORY* -- a store id must mean the same thing to every image
of the system (GH #169, #186)."
  (unless *system-directory*
    (error 'system-directory-required))
  (with-lock-held (*store-registry-open-lock*)
    (unless (and *store-registry*
                 (equal (pathname (store-registry-location *store-registry*))
                        (pathname *system-directory*)))
      (setf *store-registry*
            (%store-registry-load
             (%make-store-registry :location *system-directory*))))
    *store-registry*))

(defun store-registry-id-for (name &optional
                                     (registry (ensure-store-registry)))
  "NAME's store id, or NIL.  Lock-free read: NIL is a hint, not proof --
only STORE-REGISTRY-INTERN re-checks under the lock."
  (gethash name (store-registry-names registry)))

(defun store-registry-name-for (id &optional
                                     (registry (ensure-store-registry)))
  "The graph-name assigned ID, or NIL."
  (maphash (lambda (name known)
             (when (eql known id) (return-from store-registry-name-for
                                    name)))
           (store-registry-names registry))
  nil)

(defun store-registry-intern (name &optional
                                     (registry (ensure-store-registry)))
  "NAME's store id, minted if this system has not seen it.  Read-decide-
append under the registry flock, re-reading first: another image may
have assigned since our last load (GH #169; same discipline as
REGISTRY-INTERN, #186).

Also serializes same-image callers under REGISTRY's own recursive lock:
flock() locks an open file DESCRIPTION, not a process, so two threads in
this image each opening their own fd on the lock file would otherwise
hold independent, non-conflicting locks and could both mint an id for
the same name (a defect the type-registry idiom this mirrors avoids via
REGISTRY-INTERN's outer WITH-RECURSIVE-LOCK-HELD -- #186)."
  (with-recursive-lock-held ((store-registry-lock registry))
    (or (store-registry-id-for name registry)
        (let ((fd (%posix-open (%store-registry-lock-file
                                 (store-registry-location registry))
                                (logior +o-creat+ +o-rdwr+))))
          (unwind-protect
               (progn
                 (%posix-flock fd +lock-ex+)
                 (%store-registry-load registry)
                 (or (store-registry-id-for name registry)
                     (let ((id (store-registry-next registry)))
                       (when (> id +max-store-tag+)
                         (error 'store-registry-full
                                :location (store-registry-location
                                           registry)))
                       (with-open-file
                           (s (%store-registry-file
                               (store-registry-location registry))
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
                         ;; :READABLY T preserves this writer's
                         ;; pre-#226 *PRINT-READABLY* T -- #191
                         ;; strictness for the id ledger (GH #226).
                         (with-sidecar-output (:package :keyword
                                                :readably t)
                           (format s "~S~%" (list :name name :id id)))
                         (finish-output s))
                       (setf (gethash name (store-registry-names registry))
                             id)
                       (setf (store-registry-next registry) (1+ id))
                       id)))
            (%posix-close fd))))))
