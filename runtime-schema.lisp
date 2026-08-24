(in-package :graph-db)

;;; Runtime schema definition API (GH #172, R2+R4).  ENSURE-NAMESPACE and
;;; CREATE-VERTEX-TYPE/CREATE-EDGE-TYPE are the functional twins of
;;; DEF-VERTEX/DEF-EDGE: they build a class from data arriving at runtime
;;; instead of a macro form, then hand off to %INSTALL-NODE-TYPE (schema.lisp)
;;; -- the same path DEF-NODE-TYPE's expansion uses, so a runtime type is
;;; indistinguishable from a source one once built.  Spec:
;;; docs/superpowers/specs/2026-08-24-runtime-schema-172-design.md

;;; ---------------------------------------------------------------------
;;; The system manifest (R2): schema-manifest.dat beside the type
;;; registry.  Append-only, one readable line per record, mirroring
;;; type-occupancy.lisp's sidecar discipline -- a failed append never
;;; aborts the definition, and no system directory degrades to
;;; in-image-only for this session.
;;; ---------------------------------------------------------------------

(defvar *schema-manifest-lock* (make-lock "schema manifest"))

(defun %schema-manifest-path (dir)
  (make-pathname :name "schema-manifest" :type "dat" :defaults dir))

(defun %schema-manifest-file ()
  "SCHEMA-MANIFEST.DAT beside the type registry, or NIL when no system
directory is configured, or any other failure -- fallback: in-image
only (GH #172, mirrors %EDGE-OCCUPANCY-FILE)."
  (handler-case
      (%schema-manifest-path (type-registry-location (ensure-type-registry)))
    (error () nil)))

(defun %schema-manifest-print-package ()
  ;; COMMON-LISP, not KEYWORD: class symbols must print package-qualified
  ;; (the #167 manifest discipline; see %EDGE-OCCUPANCY-PRINT-PACKAGE).
  (load-time-value (find-package "COMMON-LISP")))

(defun %write-schema-manifest-record (plist)
  "The UNLOCKED write: append PLIST as one ~S-printed, package-qualified
line.  Returns T on a successful write, NIL when it degraded (no file,
or the write itself failed) -- never signals.  Callers take
*SCHEMA-MANIFEST-LOCK* themselves; this must not lock on its own, so a
caller that also needs to update a cache under the same critical
section (see %SCHEMA-MANIFEST-APPEND-IF-CHANGED, schema.lisp) can take
the lock exactly once (GH #172, review round 2)."
  (let ((file (%schema-manifest-file)))
    (and file
        (handler-case
            (progn
              (with-open-file (s file :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create)
                (let ((*print-readably* nil)
                      (*print-pretty* nil)
                      (*package* (%schema-manifest-print-package)))
                  (format s "~S~%" plist))
                (finish-output s))
              t)
          (error () nil)))))

(defun %append-schema-manifest-record (plist)
  "Append PLIST to the schema manifest, under *SCHEMA-MANIFEST-LOCK*.
Never signals.  Returns T on a successful write, NIL when it degraded
to in-image-only (no system directory, or the write itself failed) --
GH #172, R2.  The T/NIL contract is what
%SCHEMA-MANIFEST-APPEND-IF-CHANGED needs: caching a row as written
before the outcome is known would let one transient failure (disk
full, unwritable directory) silently drop it for the rest of the
session (review round 2)."
  (with-lock-held (*schema-manifest-lock*)
    (%write-schema-manifest-record plist)))

(defun %parse-schema-manifest-line (line)
  "Read LINE as a plist headed by a keyword.  *READ-EVAL* NIL: the file
is data.  Returns NIL on anything malformed -- a torn or corrupt line is
only a lost record, never an error (GH #172)."
  (handler-case
      (let ((*read-eval* nil)
            (*package* (%schema-manifest-print-package)))
        (multiple-value-bind (form pos) (read-from-string line)
          (when (and (= pos (length line))
                     (consp form) (keywordp (first form)))
            form)))
    (error () nil)))

(defun read-schema-manifest (dir)
  "Read schema-manifest.dat under DIR.  Returns (VALUES NAMESPACE-RECORDS
TYPE-RECORDS); last record per name wins, malformed/torn lines are
skipped, a missing file yields two empty lists (GH #172, R2)."
  (let ((file (ignore-errors (%schema-manifest-path dir)))
        (ns-table (make-hash-table :test 'equal))
        (ns-order nil)
        (type-table (make-hash-table :test 'eq))
        (type-order nil))
    (when (and file (probe-file file))
      (handler-case
          (with-open-file (s file :direction :input)
            (loop
              (let ((line (read-line s nil :eof)))
                (when (eq line :eof) (return))
                (let ((rec (%parse-schema-manifest-line line)))
                  (when rec
                    (cond
                      ((getf rec :namespace)
                       (let ((key (getf rec :namespace)))
                         (unless (nth-value 1 (gethash key ns-table))
                           (push key ns-order))
                         (setf (gethash key ns-table) rec)))
                      ((getf rec :type)
                       (let ((key (getf rec :type)))
                         (unless (nth-value 1 (gethash key type-table))
                           (push key type-order))
                         (setf (gethash key type-table) rec)))))))))
        (error () nil)))
    (values (mapcar (lambda (k) (gethash k ns-table)) (nreverse ns-order))
            (mapcar (lambda (k) (gethash k type-table))
                    (nreverse type-order)))))

;;; ---------------------------------------------------------------------
;;; ENSURE-NAMESPACE (R4)
;;; ---------------------------------------------------------------------

(defun ensure-namespace (name &key nicknames (record-p t))
  "Ensure a package for a runtime schema namespace, idempotent and
manifest-logged.  NAME and each of NICKNAMES may be a string or a
symbol.  Created with MAKE-PACKAGE :USE NIL -- a schema namespace only
holds the class and accessor symbols CREATE-VERTEX-TYPE/CREATE-EDGE-TYPE
intern into it, it is not a code package; (:use #:cl #:graph-db) is what
a hand-written SOURCE package uses, and is what EXPORT-SCHEMA-SOURCE
emits for one, not what this creates.  Allocates no files and no store.

RECORD-P NIL suppresses the manifest append: MATERIALIZE-SCHEMA is
replaying rows the manifest already holds, and re-appending an
unchanged namespace row on every load would grow the file forever
(GH #172, R4)."
  (let* ((pkg-name (string name))
         (nick-strings (mapcar #'string nicknames))
         (pkg (find-package pkg-name))
         ;; The set actually applied to the package -- review round 1,
         ;; I1: the manifest row must record THIS, not just this call's
         ;; own NICK-STRINGS, or a later no-nickname call's last-wins
         ;; row would drop every nickname a prior call had established.
         (wanted (if pkg
                    (union (package-nicknames pkg) nick-strings
                          :test #'string=)
                    nick-strings)))
    (if pkg
        (unless (= (length wanted) (length (package-nicknames pkg)))
          (rename-package pkg pkg-name wanted))
        (setf pkg (make-package pkg-name :nicknames wanted :use nil)))
    (when record-p
      (%append-schema-manifest-record
       (list :namespace pkg-name :nicknames wanted
             :time (get-universal-time))))
    pkg))

;;; ---------------------------------------------------------------------
;;; Schema functions and the :CHECK slot option (R5)
;;;
;;; Behaviour cannot be data: a closure does not serialize.  A runtime
;;; type that wants a constraint stores the NAME of a function the image
;;; provides, and the name is resolved against this registry -- at
;;; CREATE-*-TYPE time, at MATERIALIZE-SCHEMA time, and again at each
;;; check.  Enforcement lives beside the other value constraints; see
;;; %VALUE-CONSTRAINT-VIOLATIONS (value-constraint.lisp).
;;; ---------------------------------------------------------------------

(defvar *schema-functions* (make-hash-table :test 'eq)
  "NAME (symbol) -> function, for the :CHECK slot option (GH #172, R5).")

(defvar *schema-functions-lock* (make-lock "schema functions"))

(defvar *schema-check-slots-present-p* nil
  "T once any class in this image has a :CHECK slot.  Set by
COMPUTE-EFFECTIVE-SLOT-DEFINITION (node-class.lisp); read by
VALIDATE-VALUE-CONSTRAINTS so a schema with no :CHECK pays nothing
(GH #172, R5).")

(define-condition schema-function-unresolved (error)
  ((names :initarg :names :reader unresolved-function-names))
  (:report
   (lambda (c s)
     (format s "No schema function is registered under~{ ~S~}.  ~
Behaviour ships in the image, not in the metadata: call ~
REGISTER-SCHEMA-FUNCTION before the schema that names it is used ~
(GH #172)."
             (unresolved-function-names c)))))

(define-condition materialize-unresolved-functions
    (schema-function-unresolved)
  ()
  (:report
   (lambda (c s)
     (format s "MATERIALIZE-SCHEMA: the manifest names schema ~
function(s)~{ ~S~} that this image does not provide.  Nothing was ~
built.  Register them before materializing (GH #172, R3)."
             (unresolved-function-names c)))))

(define-condition materialize-unresolved-parents (error)
  ((names :initarg :names :reader unresolved-parent-names))
  (:report
   (lambda (c s)
     (format s "MATERIALIZE-SCHEMA: the manifest names parent type(s)~
~{ ~S~} that neither exist as finalized classes nor appear among the ~
rows being materialized.  Nothing was built: building them would leave ~
FORWARD-REFERENCED-CLASSes behind, and every later materialization ~
would then skip those names as \"already defined\".  Widen ~
:NAMESPACES, or load the source that defines them first (GH #172, R3)."
             (unresolved-parent-names c)))))

(defun register-schema-function (name fn)
  "Register FN under NAME for the :CHECK slot option.  Returns NAME.
Re-registering replaces (GH #172, R5)."
  (check-type name symbol)
  (with-lock-held (*schema-functions-lock*)
    (setf (gethash name *schema-functions*) fn))
  name)

(defun find-schema-function (name)
  "The function registered under NAME, or NIL (GH #172, R5)."
  (with-lock-held (*schema-functions-lock*)
    (values (gethash name *schema-functions*))))

(defun %unregister-schema-function (name)
  "Withdraw NAME's registration.  T if one was withdrawn.  Exists for
tests simulating an image that no longer provides it (GH #172)."
  (with-lock-held (*schema-functions-lock*)
    (remhash name *schema-functions*)))

(defun %resolve-schema-function (name)
  "NAME's function, or SCHEMA-FUNCTION-UNRESOLVED.  Resolution is at
CHECK time so a re-registration takes effect immediately; presence is
verified far earlier (GH #172, R5)."
  (or (find-schema-function name)
      (error 'schema-function-unresolved :names (list name))))

(defun %slot-check-names (slot-specs)
  "The :CHECK names declared by SLOT-SPECS (normalized or raw)."
  (loop for spec in slot-specs
        for name = (and (consp spec) (getf (rest spec) :check))
        when name collect name))

(defun %unresolved-check-names (slot-spec-lists)
  "Every :CHECK name in SLOT-SPEC-LISTS with no registered function,
deduplicated, in order of appearance (GH #172, R5)."
  (let ((missing nil))
    (dolist (specs slot-spec-lists (nreverse missing))
      (dolist (name (%slot-check-names specs))
        (unless (or (find-schema-function name) (member name missing))
          (push name missing))))))

;;; ---------------------------------------------------------------------
;;; CREATE-VERTEX-TYPE / CREATE-EDGE-TYPE (R4)
;;; ---------------------------------------------------------------------

(defun %refused-schema-package-p (pkg)
  "COMMON-LISP and KEYWORD tolerate no new symbols; a schema type can
never be homed in either (GH #172, R4)."
  (and (member pkg (list (find-package :common-lisp)
                         (find-package :keyword)))
       t))

(defun %refuse-schema-package (designator pkg)
  (error "~A: cannot define a schema type in ~A -- use ~
ENSURE-NAMESPACE to create a namespace package first (GH #172)."
         designator (package-name pkg)))

(defun %parse-schema-type-name (name)
  "NAME as a symbol.  A string is split on ':' by hand -- NEVER READ,
since NAME is untrusted runtime data and *READ-EVAL* NIL alone does not
rule out reader-macro side effects on an arbitrary token.  Handles both
NAME:SYM and NAME::SYM.  A missing package errors naming ENSURE-NAMESPACE.
The COMMON-LISP/KEYWORD refusal is checked on the PACKAGE NAME here,
before INTERN -- review round 1, I2: checking only the interned symbol's
package afterward is too late, since INTERN into COMMON-LISP itself
signals SBCL's own package-lock error first, and INTERN into KEYWORD
just silently succeeds (GH #172, R4)."
  (if (symbolp name)
      name
      (let* ((s (string name))
             (c1 (position #\: s)))
        (unless c1
          (error "~S is not a symbol or a \"PACKAGE:NAME\" string ~
(GH #172)." name))
        (let* ((c2 (position #\: s :start (1+ c1)))
               (name-start (if (and c2 (= c2 (1+ c1))) (1+ c2) (1+ c1)))
               (pkg-name (subseq s 0 c1))
               (sym-name (subseq s name-start))
               (pkg (find-package pkg-name)))
          (unless pkg
            (error "No package named ~A -- call ENSURE-NAMESPACE first ~
(GH #172)." pkg-name))
          (when (%refused-schema-package-p pkg)
            (%refuse-schema-package s pkg))
          (intern sym-name pkg)))))

(defun %check-schema-name-package (sym)
  "Refuse to define a schema type whose home package is COMMON-LISP or
KEYWORD -- neither tolerates new symbols; the package-lock guard in
%INSTALL-NODE-HELPERS/%INSTALL-EDGE-FUNCTORS is the same rule applied to
the generated helpers.  Covers the SYMBOL-argument path; the
\"PACKAGE:NAME\" string path is checked earlier, in
%PARSE-SCHEMA-TYPE-NAME, before the symbol is even interned (GH #172,
R4)."
  (let ((pkg (symbol-package sym)))
    (when (%refused-schema-package-p pkg)
      (%refuse-schema-package sym pkg))))

(defun %retarget-slot-specs (specs pkg)
  "Re-intern each spec's slot NAME and :ACCESSOR into PKG.  Runtime
SLOT-SPECS arrive as data, read in the CALLER's package; the generated
accessor must live with the class's own symbols, exactly as
DEF-VERTEX's slot symbols already do -- both read from one source form
there, so no retargeting is needed on that path (GH #172, R4)."
  (mapcar
   (lambda (spec)
     (destructuring-bind (name &rest plist &key accessor &allow-other-keys)
         spec
       (list* (intern (symbol-name name) pkg)
              :accessor (intern (symbol-name accessor) pkg)
              (let ((copy (copy-list plist)))
                (remf copy :accessor)
                copy))))
   specs))

(defun %ensure-node-class (name parents kind normalized-slots)
  "Build or redefine NAME's CLOS class from data via the MOP.  NORMALIZED-
SLOTS are %NORMALIZE-SLOT-SPECS output; only :TYPE is forwarded to
:DIRECT-SLOTS (GH #172, R4)."
  (ensure-class
   name
   :direct-superclasses
   (append parents (list (ecase kind (:vertex 'vertex) (:edge 'edge))))
   :direct-slots
   (mapcar (lambda (spec)
             (destructuring-bind (sname &key accessor initarg type check
                                        &allow-other-keys)
                 spec
               (append
                (list :name sname
                      :initargs (list initarg)
                      :readers (list accessor)
                      :writers (list (list 'setf accessor)))
                (when type (list :type type))
                ;; R5: the only other slot option a runtime type may
                ;; carry.  NODE-CLASS's slot definitions accept it.
                (when check (list :check check)))))
           normalized-slots)
   :metaclass (find-class 'node-class)))

(defun %create-node-type (name slot-specs kind &key parents default-store
                                                    keep-revisions)
  "Shared body of CREATE-VERTEX-TYPE/CREATE-EDGE-TYPE.  %ENSURE-NODE-CLASS
must run before %INSTALL-NODE-TYPE -- the latter requires the class to
already exist, since it calls FINALIZE-INHERITANCE (GH #172, R4)."
  (let* ((sym (%parse-schema-type-name name))
         (specs (%retarget-slot-specs (%normalize-slot-specs slot-specs)
                                      (symbol-package sym))))
    (%check-schema-name-package sym)
    ;; Fail fast, like MATERIALIZE-SCHEMA: a :CHECK naming a function
    ;; this image does not provide is a broken definition, not a
    ;; surprise at first write (GH #172, R5).
    (let ((missing (%unresolved-check-names (list specs))))
      (when missing
        (error 'schema-function-unresolved :names missing)))
    (%ensure-node-class sym parents kind specs)
    (let ((*schema-provenance* :runtime))
      (%install-node-type
       (make-node-type
        :name sym
        :parent-type kind
        :graph-name default-store
        :slots specs
        :package (package-name (symbol-package sym))
        :constructor (intern (format nil "MAKE-~A" (symbol-name sym))
                             (%schema-symbol-package sym))
        :keep-revisions keep-revisions)))
    (find-class sym)))

(defun create-vertex-type (name slot-specs &key parents default-store
                                                keep-revisions)
  "Runtime twin of DEF-VERTEX: build and register a vertex type from data
instead of a macro form.  NAME is a symbol or a \"PACKAGE:NAME\" string --
the package must already exist (see ENSURE-NAMESPACE); a missing one
errors.  SLOT-SPECS are the same CLOS-style specs DEF-VERTEX takes.
DEFAULT-STORE defaults to NIL, meaning \"no default store\": the generated
constructor then requires an explicit :GRAPH argument, since a runtime
type need not commit to placement at creation.  Redefining an existing
name -- runtime- or source-defined -- follows ordinary CLOS class
redefinition, with the #196 divergence warning when slot sets disagree
across stores, exactly like re-evaluating DEF-VERTEX.  Returns the
finalized class (GH #172, R4)."
  (%create-node-type name slot-specs :vertex :parents parents
                     :default-store default-store
                     :keep-revisions keep-revisions))

(defun create-edge-type (name slot-specs &key parents default-store
                                              keep-revisions)
  "Runtime twin of DEF-EDGE; see CREATE-VERTEX-TYPE for the shared
semantics.  Also installs the NAME/2 and NAME/3 Prolog functors
(GH #172, R4)."
  (%create-node-type name slot-specs :edge :parents parents
                     :default-store default-store
                     :keep-revisions keep-revisions))

;;; ---------------------------------------------------------------------
;;; MATERIALIZE-SCHEMA (R3): the load-order answer
;;; ---------------------------------------------------------------------

(defun %materialize-namespace-filter (namespaces)
  "NAMESPACES as a list of package-name strings, or NIL for \"all\"."
  (when namespaces
    (mapcar (lambda (n) (string-upcase (string n)))
            (if (listp namespaces) namespaces (list namespaces)))))

(defun %manifest-type-row-package (line)
  "The package-name prefix of a (:TYPE PKG::NAME ...) manifest line, or
NIL.  Textual on purpose: READ interns, and interning needs the very
package this is trying to discover -- a type row whose namespace record
was hand-trimmed away would otherwise read as a missing-package error
and vanish (GH #172, R3)."
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (when (and (> (length trimmed) 6)
               (string-equal "(:TYPE" (subseq trimmed 0 6)))
      (let* ((rest (string-left-trim '(#\Space #\Tab) (subseq trimmed 6)))
             (end (or (position-if
                       (lambda (c) (member c '(#\Space #\Tab #\)))) rest)
                      (length rest)))
             (token (subseq rest 0 end))
             (colon (position #\: token)))
        (when (and colon (plusp colon))
          (subseq token 0 colon))))))

(defun %materialize-orphan-packages (dir filter)
  "Create a package for every type row in DIR's manifest whose package
does not exist and has no namespace record.  Warns: a manifest that
lost its namespace rows still materializes, but not silently
(GH #172, R3)."
  (let ((file (ignore-errors (%schema-manifest-path dir))))
    (when (and file (probe-file file))
      (handler-case
          (with-open-file (s file :direction :input)
            (loop
              (let ((line (read-line s nil :eof)))
                (when (eq line :eof) (return))
                (let ((pkg (%manifest-type-row-package line)))
                  (when (and pkg
                             (or (null filter)
                                 (member pkg filter :test #'string-equal))
                             (not (find-package pkg)))
                    (warn "Schema manifest: type row names package ~A ~
with no namespace record; creating it with no nicknames (GH #172)." pkg)
                    (ensure-namespace pkg))))))
        (error () nil)))))

(defun %materialize-row-wanted-p (row filter)
  "Is ROW a type row this materialization should consider?  A row naming
a locked package or an unknown kind is skipped with a warning rather
than crashing: the manifest is data, and may have been hand-edited
(GH #172, R3)."
  (let ((name (getf row :type))
        (kind (getf row :kind)))
    (cond
      ((not (and (symbolp name) (symbol-package name))) nil)
      ((%refused-schema-package-p (symbol-package name))
       (warn "Schema manifest: ignoring type row ~S in locked package ~
~A (GH #172)." name (package-name (symbol-package name)))
       nil)
      ((not (member kind '(:vertex :edge)))
       (warn "Schema manifest: ignoring type row ~S with kind ~S ~
(GH #172)." name kind)
       nil)
      (t (or (null filter)
             (member (package-name (symbol-package name)) filter
                     :test #'string-equal))))))

(defun %materialized-class-present-p (name)
  "Does NAME already name a REAL class?  A FORWARD-REFERENCED-CLASS is
absent, not present: it is the stub ENSURE-CLASS leaves behind for an
unknown superclass, and treating it as \"source wins\" is exactly how a
half-built materialization poisons every later one (GH #172, R3,
review round 1, I-2)."
  (let ((class (find-class name nil)))
    (and class (not (typep class 'forward-referenced-class)))))

(defun %unresolved-parent-names (rows)
  "Every parent named by ROWS that is neither a real class already nor
a row in ROWS itself, deduplicated in order of appearance.  ROWS is the
whole build set, skipped rows included: a skipped row's class exists,
so it is a legitimate parent (GH #172, R3, review round 1, I-2)."
  (let ((known (make-hash-table :test 'eq))
        (missing nil))
    (dolist (row rows)
      (setf (gethash (getf row :type) known) t))
    (dolist (row rows (nreverse missing))
      (dolist (parent (getf row :parents))
        (unless (or (gethash parent known)
                    (%materialized-class-present-p parent)
                    (member parent missing))
          (push parent missing))))))

(defun %materialize-sort (rows)
  "ROWS reordered so a row builds after any parent of it that is also
in ROWS; manifest order is the tiebreak.  Redefinition re-appends a
row, so append order alone can put a parent after its child.  A cycle
-- impossible through the API, reachable by hand-editing -- falls back
to manifest order for what remains (GH #172, R3)."
  (let ((pending (copy-list rows))
        (built (make-hash-table :test 'eq))
        (out nil))
    (loop while pending do
      (let ((progressed nil))
        (dolist (row pending)
          (when (every (lambda (p)
                         (or (gethash p built)
                             (not (find p pending
                                        :key (lambda (r) (getf r :type))))))
                       (getf row :parents))
            (push row out)
            (setf (gethash (getf row :type) built) t)
            (setf pending (remove row pending))
            (setf progressed t)))
        (unless progressed
          (dolist (row pending) (push row out))
          (setf pending nil))))
    (nreverse out)))

(defun %warn-if-row-diverges (row)
  "Source wins on the skip path, but not silently.  %WARN-IF-DIVERGENT-
ACROSS-STORES never runs here -- nothing is installed -- so compare the
row against the registered meta and signal the #196 condition directly
(GH #172, R3)."
  (let* ((name (getf row :type))
         (meta (%find-registered-node-type name (getf row :kind)
                                           (getf row :default-store))))
    (when (and meta (not (equal (node-type-slots meta) (getf row :slots))))
      (warn 'divergent-node-type-redefinition
            :name name
            :graph-name (getf row :default-store)
            :other-graphs (list (node-type-graph-name meta))))))

(defun %materialize-row (row)
  "Build ROW's class and install it through the shared path.  ROW's
slots are already normalized and retargeted (the manifest records what
%INSTALL-NODE-TYPE registered), so no re-normalization is needed.  No
evaluation: plists in, MOP calls out (GH #172, R3)."
  (let ((name (getf row :type))
        (kind (getf row :kind))
        (slots (getf row :slots)))
    (%ensure-node-class name (getf row :parents) kind slots)
    ;; The row's own provenance, defaulting to :RUNTIME -- never the
    ;; :SOURCE default of *SCHEMA-PROVENANCE*, which would relabel every
    ;; materialized runtime type as source-defined (GH #172, R2).
    (let ((*schema-provenance* (or (getf row :provenance) :runtime)))
      (%install-node-type
       (make-node-type
        :name name
        :parent-type kind
        :graph-name (getf row :default-store)
        :slots slots
        :package (package-name (symbol-package name))
        :constructor (intern (format nil "MAKE-~A" (symbol-name name))
                             (%schema-symbol-package name))
        :keep-revisions (getf row :keep-revisions))))
    name))

(defun %materialize-schema (dir &key namespaces)
  "The functional core of MATERIALIZE-SCHEMA; see that macro.  Use the
macro in a file: it carries the EVAL-WHEN this needs to run before the
methods below it compile (GH #172, R3)."
  (let ((filter (%materialize-namespace-filter namespaces))
        (namespace-count 0)
        (materialized 0)
        (skipped 0))
    (dolist (rec (read-schema-manifest dir))
      (let ((name (string (getf rec :namespace))))
        (when (or (null filter) (member name filter :test #'string-equal))
          ;; :RECORD-P NIL -- this row is already in the manifest.
          (ensure-namespace name :nicknames (getf rec :nicknames)
                                 :record-p nil)
          (incf namespace-count))))
    ;; Packages first, and not only from namespace rows: a type row
    ;; cannot even be READ until its package exists.
    (%materialize-orphan-packages dir filter)
    (let ((rows (remove-if-not
                 (lambda (r) (%materialize-row-wanted-p r filter))
                 (nth-value 1 (read-schema-manifest dir))))
          (pending nil))
      ;; Fail fast, before anything is built: ONE error naming every
      ;; unresolved :CHECK function (approved point C), and one naming
      ;; every unbuildable parent (I-2).  Half a materialization is
      ;; worse than none: the stub classes it leaves behind make every
      ;; later attempt skip those names as already defined.
      (let ((missing (%unresolved-check-names
                      (mapcar (lambda (r) (getf r :slots)) rows))))
        (when missing
          (error 'materialize-unresolved-functions :names missing)))
      (let ((orphans (%unresolved-parent-names rows)))
        (when orphans
          (error 'materialize-unresolved-parents :names orphans)))
      (dolist (row rows)
        (if (%materialized-class-present-p (getf row :type))
            (progn (%warn-if-row-diverges row) (incf skipped))
            (push row pending)))
      ;; Nothing this call installs may touch the manifest: every row
      ;; came out of it (M-1).
      (let ((*record-manifest-rows* nil))
        (dolist (row (%materialize-sort (nreverse pending)))
          (%materialize-row row)
          (incf materialized))))
    (list :namespaces namespace-count
          :materialized materialized
          :skipped-existing skipped)))

(defmacro materialize-schema (dir &key namespaces)
  "Rebuild every runtime-defined package and class recorded in DIR's
schema manifest, so methods compiled after this form see their classes.
Put it in its own file, loaded after the static schema and before any
file with methods on a runtime type; the EVAL-WHEN is carried here so a
caller cannot get it wrong.

Idempotent, and SOURCE WINS: a type whose class already exists is left
alone, with the #196 divergence warning when the manifest's slot set
disagrees with the live class.
:NAMESPACES narrows to the named packages.  Nothing is evaluated --
the input is plists and the output is MOP calls -- and a :CHECK
function the image does not provide aborts the whole call before
anything is built (MATERIALIZE-UNRESOLVED-FUNCTIONS, naming all of
them), as does a row whose parent neither exists nor is being built
(MATERIALIZE-UNRESOLVED-PARENTS).

Returns (:NAMESPACES n :MATERIALIZED n :SKIPPED-EXISTING n).
:NAMESPACES counts packages ensured, :MATERIALIZED classes built by
THIS call, and :SKIPPED-EXISTING rows left alone because the class was
already present -- whatever defined it, source or an earlier
materialization (under compile-then-load, the load pass sees the
compile pass's work here) (GH #172, R3)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%materialize-schema ,dir :namespaces ,namespaces)))
