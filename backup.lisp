(in-package :graph-db)

(defgeneric backup (object location &key include-deleted-p))

;;; ---------------------------------------------------------------------------
;;; Printing specialized vectors readably (issue #56).
;;;
;;; BACKUP prints a node as one s-expression with (FORMAT ... "~S"), so the
;;; standard printer decides how vectors look -- and it prints EVERY vector as
;;; #(...), throwing the element type away.  The restore reader then cannot tell
;;; an id (a byte vector) from a SINGLE-FLOAT embedding.
;;;
;;; The fix is an explicit, portable encoding we control both ends of: a vector
;;; whose element type is not T is wrapped, just before printing, in a
;;; BACKUP-VECTOR-LITERAL whose PRINT-OBJECT emits #V(<element-type> e1 e2 ...).
;;; RESTORE-SHARP-V-READER (transaction-restore.lisp) reads it back.
;;;
;;; Wrapping is done by a pre-pass rather than a PPRINT-DISPATCH entry because
;;; BACKUP binds *PRINT-PRETTY* to NIL, which disables pprint dispatch entirely.
;;; PRINT-OBJECT is honoured either way, including inside nested alists.
;;; ---------------------------------------------------------------------------

(defstruct (backup-vector-literal (:constructor make-backup-vector-literal (vector))
                                  (:copier nil))
  "Print-time wrapper: prints its VECTOR as #V(<element-type> e1 e2 ...)."
  (vector #() :read-only t))

(defparameter *backup-element-types*
  '((unsigned-byte 8) (signed-byte 8)
    (unsigned-byte 16) (signed-byte 16)
    (unsigned-byte 32) (signed-byte 32)
    (unsigned-byte 64) (signed-byte 64)
    single-float double-float bit character base-char)
  "Standard element-type specifiers tried, narrowest first, when canonicalizing
a vector's element type for a snapshot.  See %BACKUP-ELEMENT-TYPE-SPEC.")

(defun %backup-element-type-spec (vector)
  "A PORTABLE type specifier for VECTOR's element type.

ARRAY-ELEMENT-TYPE returns the implementation's OWN name for the upgraded type,
and printing that into a snapshot makes the snapshot implementation-specific:
ECL says EXT:BYTE8 where SBCL says (UNSIGNED-BYTE 8), and an ECL-written
snapshot then fails to READ on SBCL with \"Package EXT does not exist\" -- which
breaks snapshot+replay, the documented path for moving a graph between
implementations (an ECL field device backing up to an SBCL hub, say).

So map the upgraded type back to the standard specifier that upgrades to it and
print that instead.  Falls back to the raw ARRAY-ELEMENT-TYPE if nothing
matches, which is no worse than the old behaviour."
  (let ((et (array-element-type vector)))
    (or (find-if (lambda (spec) (equal (upgraded-array-element-type spec) et))
                 *backup-element-types*)
        et)))

(defmethod print-object ((object backup-vector-literal) stream)
  (let ((vector (backup-vector-literal-vector object)))
    (write-string "#V(" stream)
    (prin1 (%backup-element-type-spec vector) stream)
    (loop for element across vector
          do (write-char #\Space stream)
             (prin1 element stream))
    (write-char #\) stream)))

(defun %backup-literalize-struct (object)
  "A shallow copy of struct OBJECT with each slot value literalized, or OBJECT
itself when literalizing changed nothing.

Structs print as #S(...) and their slot values go through the same standard
printer as everything else, so a specialized vector reached only through a slot
-- a GEOMETRY's packed coordinate ring, say -- has to be walked too or it prints
as a bare #(...) and restores with element type T (geometry.lisp's WALK-RING
then matches nothing and GEOMETRY-BBOX returns NILs).

Unchanged slots are never written back, because on SBCL (SETF SLOT-VALUE) has
no writer for a :READ-ONLY slot and type-checks a slot with a declared :TYPE.
A struct that both constrains a slot AND stores a specialized vector in it
therefore still signals -- loudly, at backup time, which is the intended
trade against writing a snapshot that cannot be replayed.

Slot access is via the MOP, which GRAPH-DB :USEs per implementation
(package.lisp); CLASS-SLOTS over a STRUCTURE-CLASS is the same mechanism
cl-store-ecl.lisp relies on, and is verified on SBCL and ECL."
  (let ((changed nil))
    (dolist (slot (class-slots (class-of object)))
      (let* ((name (slot-definition-name slot))
             (old (slot-value object name))
             (new (backup-literalize old)))
        (unless (eq new old)
          (push (cons name new) changed))))
    (if (null changed)
        object
        (let ((copy (copy-structure object)))
          (loop for (name . new) in changed
                do (setf (slot-value copy name) new))
          copy))))

(defun backup-literalize (object)
  "Recursively copy OBJECT, wrapping every specialized vector for printing.

A vector whose ARRAY-ELEMENT-TYPE is not T loses that type through the standard
#(...) printer, so it is wrapped in a BACKUP-VECTOR-LITERAL.  Strings are left
alone: they already print and read back correctly.  Conses, general (element
type T) vectors and structs are walked so that specialized vectors nested
inside a node's data alist are wrapped too.

The struct branch reads back through the standard #S(...) reader, so the struct
type must be defined -- and its name readable -- in the restoring image; see
RECREATE-GRAPH's :PACKAGE-NAME."
  (typecase object
    (cons (cons (backup-literalize (car object))
                (backup-literalize (cdr object))))
    (string object)
    (vector
     (if (eq (array-element-type object) t)
         (let ((copy (make-array (length object))))
           (dotimes (i (length object) copy)
             (setf (aref copy i) (backup-literalize (aref object i)))))
         (make-backup-vector-literal object)))
    (structure-object (%backup-literalize-struct object))
    (t object)))

(defun write-backup-plist (plist stream)
  "Print PLIST as one readable snapshot line on STREAM."
  (let ((*print-pretty* nil))
    (format stream "~S~%" (backup-literalize plist))))

(defmethod backup :around ((node node) location &key include-deleted-p)
  (when (or include-deleted-p (not (deleted-p node)))
    (call-next-method)))

(defmethod backup ((v vertex) (stream stream) &key include-deleted-p)
  (declare (ignore include-deleted-p))
  (let ((plist
         (list :v
               (type-of v)
               (when (slot-boundp v 'data)
                 (data v))
               :id (id v)
               :revision (revision v)
               :deleted-p (deleted-p v))))
    (write-backup-plist plist stream)))

(defmethod backup ((e edge) (stream stream) &key include-deleted-p)
  (declare (ignore include-deleted-p))
  (let ((plist
         (list :e
               (type-of e)
               (from e)
               (to e)
               (weight e)
               (when (slot-boundp e 'data)
                 (data e))
               :id (id e)
               :revision (revision e)
               :deleted-p (deleted-p e))))
    (write-backup-plist plist stream)))

(defmethod backup ((graph graph) location &key include-deleted-p)
  (ensure-directories-exist location)
  (let ((count 0))
    ;; The implementation default DIVERGES -- SBCL errors, ECL silently
    ;; overwrites -- so a user's backup was destroyed on one and protected on the
    ;; other.  Pinned to :ERROR (GH #100).  Safe only now that no caller derives a
    ;; backup path from the clock; ECL's permissive default was what hid the
    ;; constant txn-log snapshot name.
    (with-open-file (out location :direction :output :if-exists :error)
      (map-vertices (lambda (v)
                      (maybe-init-node-data v :graph graph)
                      (incf count)
                      (backup v out))
                    graph :include-deleted-p include-deleted-p)
      (map-edges (lambda (e)
                   (maybe-init-node-data e :graph graph)
                   (incf count)
                   (backup e out))
                 graph :include-deleted-p include-deleted-p)
      (values count location))))

(defmethod check-data-integrity ((graph graph) &key include-deleted-p)
  (let ((*cache-enabled* nil))
    (let ((problems nil) (count 0))
      (map-vertices (lambda (v)
                      (incf count)
                      (when (= 0 (mod count 1000))
                        (format t ".")
                        (force-output))
                      (handler-case
                          (maybe-init-node-data v :graph graph)
                        (error (c)
                          (log:error "data integrity ~A: ~A" (string-id v) c)
                          (push (cons (string-id v) c) problems))))
                    graph :include-deleted-p include-deleted-p)
      (map-edges (lambda (e)
                      (incf count)
                      (when (= 0 (mod count 1000))
                        (format t ".")
                        (force-output))
                   (handler-case
                       (maybe-init-node-data e :graph graph)
                     (error (c)
                       (log:error "data integrity ~A: ~A" (string-id e) c)
                       (push (cons (string-id e) c) problems))))
                 graph :include-deleted-p include-deleted-p)
      (terpri)
      problems)))

;;; ---------------------------------------------------------------------------
;;; v1 -> v2 migration (MVCC head growth, storage-version 1 -> 2)
;;;
;;; v2 grew the node head 15 -> 31 bytes (commit-epoch + prev-pointer), so v2
;;; code cannot open a v1 graph directly.  MIGRATE-GRAPH does a format-agnostic
;;; LOGICAL snapshot + replay: open the v1 graph read-only with a 15-byte head
;;; shim, BACKUP every live node to a pointer-free plist file, then MAKE-GRAPH a
;;; fresh v2 graph and RECREATE-GRAPH (replay) into it.  Precedent: the
;;; pre-58f87d6 UUID/hash change was migrated the same way (snapshot + replay).
;;; ---------------------------------------------------------------------------

(defun %migration-snapshot-file (name)
  "A per-run path for MIGRATE-GRAPH's intermediate snapshot.  A name-only path is
constant across runs, users and processes on a host, so one aborted migration left
a file that made every later migration of that name die with a bare FILE-EXISTS
naming a path the caller never chose (GH #98).

Uniqueness comes from a v4 UUID, not from the clock: GETTIMEOFDAY has no #+ecl
branch and returns NIL there, which would make every ECL path identical -- the
failure mode this function exists to prevent.  See GH #100."
  (format nil "~Amigrate-~A-~A.snapshot"
          (or #+sbcl (sb-ext:native-namestring (uiop:temporary-directory))
              "/tmp/")
          name (uuid:make-v4-uuid)))

(defun migrate-graph (name old-location new-location
                      &key (package :graph-db) include-deleted-p
                           (delete-snapshot-p t)
                           (snapshot-file (%migration-snapshot-file name)))
  "Migrate a pre-MVCC (v1) graph at OLD-LOCATION to the current (v2) on-disk
format at NEW-LOCATION, returning the new, open graph.

Migration is a logical snapshot + replay: the v1 graph is opened read-only with
a 15-byte head shim, every live node is written to a format-independent snapshot
file, then a fresh v2 graph is created and the snapshot replayed through the
normal MAKE-VERTEX / MAKE-EDGE path.  The v1 graph's schema (its type-id
registry) is copied to the new graph, so type-ids are preserved.

OLD-LOCATION is left byte-for-byte untouched; NEW-LOCATION must not already hold
a graph.  The CLOS classes for the graph's node types must already be defined in
this image (load your DEF-VERTEX / DEF-EDGE forms first).  :INCLUDE-DELETED-P
carries tombstoned nodes across too; :DELETE-SNAPSHOT-P (default T) removes the
intermediate snapshot file on every exit, including a failed migration.

:SNAPSHOT-FILE defaults to a PER-RUN path under the temporary directory.  It is
deliberately not keyed on NAME alone: such a path is constant across runs, users
and processes on a host, so an aborted migration left a file behind that made
every later migration of that name fail with a bare FILE-EXISTS (GH #98).  Pass
an explicit path if you want a predictable one."
  (when (equal (namestring (truename (ensure-directories-exist
                                      (merge-pathnames "" old-location))))
               (ignore-errors
                (namestring (truename (merge-pathnames "" new-location)))))
    (error "MIGRATE-GRAPH: old and new locations must differ (~A)" old-location))
  (let ((old-schema nil))
    ;; The snapshot is removed on EVERY exit, not just the success path (GH #98):
    ;; an abort anywhere below used to leak it, and with the old name-only default
    ;; path that leaked file then broke the retry the user reaches for next.
    (unwind-protect
         (progn
           ;; 1. Open the v1 graph read-only (15-byte heads) and snapshot it logically.
           (let ((*node-head-reader* 'deserialize-node-head-v1))
             (let ((old (open-graph name old-location
                                    ;; tolerate v1 AND v2 so a re-run is harmless
                                    :accept-versions (list 1 +storage-version+)
                                    :gc-heap-p nil :buffer-pool-p t)))
               (unwind-protect
                    (let ((*graph* old)) ;; map-vertices' all-types branch reads *graph*
                      (setq old-schema (schema old))
                      (log:info "MIGRATE-GRAPH: snapshotting v1 graph ~A -> ~A"
                                old-location snapshot-file)
                      (backup old snapshot-file :include-deleted-p include-deleted-p))
                 (close-graph old :snapshot-p nil))))
           ;; 2. Create the v2 graph, adopt the v1 schema (preserving type-ids), replay.
           (let ((new (make-graph name new-location)))
             (handler-case
                 (progn
                   (setf (schema new) old-schema)
                   (restore-schema-locks (schema new))
                   (setf (schema-lock (schema new)) (make-recursive-lock))
                   (save-schema (schema new) new)
                   (log:info "MIGRATE-GRAPH: replaying snapshot into v2 graph ~A"
                             new-location)
                   (recreate-graph new snapshot-file :package-name package)
                   new)
               (error (c)
                 (close-graph new)
                 (error c)))))
      (when delete-snapshot-p
        (ignore-errors (delete-file snapshot-file))))))
