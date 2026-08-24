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

(defun %append-schema-manifest-record (plist)
  "Append PLIST as one ~S-printed, package-qualified line to the schema
manifest.  Locked; never signals -- a failed append (disk full, no
system directory, permissions) leaves the definition itself intact and
the record in-image-only for this session (GH #172, R2)."
  (with-lock-held (*schema-manifest-lock*)
    (let ((file (%schema-manifest-file)))
      (when file
        (handler-case
            (with-open-file (s file :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
              (let ((*print-readably* nil)
                    (*print-pretty* nil)
                    (*package* (%schema-manifest-print-package)))
                (format s "~S~%" plist))
              (finish-output s))
          (error () nil)))))
  (values))

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

(defun ensure-namespace (name &key nicknames)
  "Ensure a package for a runtime schema namespace, idempotent and
manifest-logged.  NAME and each of NICKNAMES may be a string or a
symbol.  Created with MAKE-PACKAGE :USE NIL -- a schema namespace only
holds the class and accessor symbols CREATE-VERTEX-TYPE/CREATE-EDGE-TYPE
intern into it, it is not a code package; (:use #:cl #:graph-db) is what
a hand-written SOURCE package uses, and is what EXPORT-SCHEMA-SOURCE
emits for one, not what this creates.  Allocates no files and no store
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
    (%append-schema-manifest-record
     (list :namespace pkg-name :nicknames wanted
           :time (get-universal-time)))
    pkg))

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
             (destructuring-bind (sname &key accessor initarg type
                                        &allow-other-keys)
                 spec
               (append
                (list :name sname
                      :initargs (list initarg)
                      :readers (list accessor)
                      :writers (list (list 'setf accessor)))
                (when type (list :type type)))))
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
