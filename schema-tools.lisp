(in-package :graph-db)

;;; Visibility tooling (GH #172, R6): DESCRIBE-SCHEMA (a plain-text dump
;;; joining the manifest with live metas) and EXPORT-SCHEMA-SOURCE (the
;;; promotion path -- metadata back into def-vertex/def-edge text).  Both
;;; are READ-ONLY: neither binds *RECORD-MANIFEST-ROWS* nor appends to the
;;; manifest.  Spec: docs/superpowers/specs/2026-08-24-runtime-schema-172-
;;; design.md, R6.

;;; ---------------------------------------------------------------------
;;; Joining the manifest with live metas
;;; ---------------------------------------------------------------------

(defun %schema-entry-parents (name kind row)
  "Direct superclasses of NAME's live class, minus the VERTEX/EDGE base --
the same computation %SCHEMA-MANIFEST-TYPE-RECORD makes.  Falls back to
ROW's own :PARENTS when the class no longer exists (GH #172, R6)."
  (let ((class (find-class name nil)))
    (if class
        (let ((base (ecase kind (:vertex (find-class 'vertex))
                           (:edge (find-class 'edge)))))
          (mapcar #'class-name
                  (remove base (class-direct-superclasses class))))
        (getf row :parents))))

(defun %describe-schema-entries ()
  "Every known node type as one plist: (:NAME :KIND :DEFAULT-STORE
:SLOTS :PARENTS :KEEP-REVISIONS :PROVENANCE :TIME).  Joins the system
manifest with every live META across every open store; a type with no
manifest row (a pre-#172 store) gets :PROVENANCE :SOURCE and :TIME NIL
-- describe/export must never signal on a missing or damaged manifest,
so this degrades to live-metas-only rather than erroring (GH #172, R6)."
  (let ((rows (nth-value 1 (ignore-errors
                            (read-schema-manifest *system-directory*))))
        (seen (make-hash-table :test 'equal))
        (entries nil))
    (flet ((key (name kind) (cons name kind))
           (add (name kind default-store slots parents keep-revisions
                 provenance time)
             (push (list :name name :kind kind
                        :default-store default-store :slots slots
                        :parents parents :keep-revisions keep-revisions
                        :provenance provenance :time time)
                   entries)))
      (dolist (row rows)
        (let ((name (getf row :type)) (kind (getf row :kind)))
          (when (and (symbolp name) (symbol-package name)
                    (member kind '(:vertex :edge)))
            (let ((meta (%find-registered-node-type
                        name kind (getf row :default-store))))
              (setf (gethash (key name kind) seen) t)
              (add name kind (getf row :default-store)
                   (if meta (node-type-slots meta) (getf row :slots))
                   (%schema-entry-parents name kind row)
                   (getf row :keep-revisions)
                   (or (getf row :provenance) :source)
                   (getf row :time))))))
      (maphash
       (lambda (store metas)
         (declare (ignore store))
         (dolist (meta metas)
           (let* ((name (node-type-name meta))
                  (kind (node-type-parent-type meta))
                  (k (key name kind)))
             (unless (gethash k seen)
               (setf (gethash k seen) t)
               (add name kind (node-type-graph-name meta)
                    (node-type-slots meta)
                    (%schema-entry-parents name kind nil)
                    (node-type-keep-revisions meta)
                    :source nil)))))
       *schema-node-metadata*))
    (nreverse entries)))

(defun %normalize-namespace-designator (namespace)
  (and namespace (string-upcase (string namespace))))

(defun %normalize-store-designator (store)
  (cond ((null store) nil)
        ((typep store 'graph) (graph-name store))
        (t store)))

(defun %entry-live-in-store-p (entry store-key)
  (find-if (lambda (m)
             (and (eq (node-type-name m) (getf entry :name))
                 (eq (node-type-parent-type m) (getf entry :kind))))
           (gethash store-key *schema-node-metadata*)))

(defun %schema-since-universal-time (since)
  "SINCE as a universal time: an integer as-is, or a \"YYYY-MM-DD\"
string parsed at local midnight.  NIL passes through (GH #172, R6)."
  (etypecase since
    (null nil)
    (integer since)
    (string
     (encode-universal-time 0 0 0
                            (parse-integer since :start 8 :end 10)
                            (parse-integer since :start 5 :end 7)
                            (parse-integer since :start 0 :end 4)))))

(defun %filter-schema-entries (entries namespace store since)
  "ENTRIES restricted to NAMESPACE's package, to types live in STORE,
and (via SINCE) to rows recorded at or after that time -- an entry with
no manifest row has no :TIME and is dropped whenever SINCE is given,
since there is nothing to compare (GH #172, R6)."
  (let ((ns (%normalize-namespace-designator namespace))
        (store-key (%normalize-store-designator store))
        (since-ut (%schema-since-universal-time since)))
    (remove-if-not
     (lambda (e)
       (and (or (null ns)
               (string-equal ns (package-name
                                  (symbol-package (getf e :name)))))
           (or (null store-key) (%entry-live-in-store-p e store-key))
           (or (null since-ut)
               (and (getf e :time) (>= (getf e :time) since-ut)))))
     entries)))

(defun %group-schema-entries-by-namespace (entries)
  "ENTRIES as an alist of (PACKAGE-NAME . ENTRIES), package names sorted,
entries within a group sorted by type name -- deterministic output
(GH #172, R6)."
  (let ((table (make-hash-table :test 'equal)) (order nil))
    (dolist (e entries)
      (let ((pkg (package-name (symbol-package (getf e :name)))))
        (unless (nth-value 1 (gethash pkg table)) (push pkg order))
        (push e (gethash pkg table))))
    (mapcar (lambda (pkg)
              (cons pkg
                    (sort (gethash pkg table) #'string<
                          :key (lambda (e) (symbol-name (getf e :name))))))
            (sort (nreverse order) #'string<))))

;;; ---------------------------------------------------------------------
;;; Bare, downcased text for the generated source -- shared by
;;; DESCRIBE-SCHEMA (default-store) and EXPORT-SCHEMA-SOURCE (whole
;;; forms).
;;; ---------------------------------------------------------------------

(defun %schema-source-token (x)
  "X as generated-source text: symbols print bare and downcased, with
no package prefix -- the generated file's own IN-PACKAGE supplies the
reader context that makes a bare token resolve correctly, so printing
a home package here would only add noise (GH #172, R6)."
  (cond
    ((null x) "nil")
    ((eq x t) "t")
    ((keywordp x) (format nil ":~(~A~)" (symbol-name x)))
    ((symbolp x) (string-downcase (symbol-name x)))
    ((stringp x) (prin1-to-string x))
    ((integerp x) (princ-to-string x))
    (t (string-downcase (princ-to-string x)))))

(defun %schema-source-form (form)
  "FORM (nested lists of symbols/keywords/literals only, as produced by
NODE-TYPE-SLOTS) as downcased source text (GH #172, R6)."
  (if (consp form)
      (format nil "(~{~A~^ ~})" (mapcar #'%schema-source-form form))
      (%schema-source-token form)))

;;; ---------------------------------------------------------------------
;;; DESCRIBE-SCHEMA
;;; ---------------------------------------------------------------------

(defun %schema-iso-date (universal-time)
  (multiple-value-bind (sec min hr day month year)
      (decode-universal-time universal-time 0)
    (declare (ignore sec min hr))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun %schema-provenance-tag (provenance time)
  (if (eq provenance :runtime)
      (format nil "[runtime~@[ ~A~]]" (and time (%schema-iso-date time)))
      "[source]"))

(defun %describe-schema-slot (spec stream)
  (destructuring-bind (name &key type check &allow-other-keys) spec
    (format stream "    ~A" (symbol-name name))
    (when type (format stream "  ~A" (princ-to-string type)))
    (when check (format stream "  :check ~A" (symbol-name check)))
    (terpri stream)))

(defun describe-schema (&key namespace store since
                        (stream *standard-output*))
  "Plain-text dump of the schema, joining the system manifest with every
live node-type meta: grouped by namespace (the package name of each
type symbol), one line per type -- name, kind, default store, and a
provenance tag ([source] or [runtime YYYY-MM-DD]; a type with no
manifest row, a pre-#172 store, always prints [source]) -- then one
line per slot: name, type, and its :CHECK name if any.

NAMESPACE restricts to one namespace's package (a string, symbol, or
keyword).  STORE restricts to types instantiated in that open store (a
graph designator or a graph object).  SINCE (a universal time, or a
\"YYYY-MM-DD\" string) filters by each row's record time, so the dump
doubles as a change log.  Never signals on a missing or damaged
manifest -- degrades to live metas only, all tagged [source]
(GH #172, R6)."
  (let ((groups (%group-schema-entries-by-namespace
                (%filter-schema-entries (%describe-schema-entries)
                                        namespace store since))))
    (dolist (group groups)
      (format stream "Namespace ~A~%" (car group))
      (dolist (e (cdr group))
        (format stream "  ~A (~(~A~)) default-store ~A   ~A~%"
                (symbol-name (getf e :name))
                (getf e :kind)
                (%schema-source-token (getf e :default-store))
                (%schema-provenance-tag (getf e :provenance)
                                        (getf e :time)))
        (dolist (spec (getf e :slots))
          (%describe-schema-slot spec stream)))))
  (values))

;;; ---------------------------------------------------------------------
;;; EXPORT-SCHEMA-SOURCE
;;; ---------------------------------------------------------------------

(defun %minimize-slot-spec (spec)
  "SPEC (a normalized, retargeted slot spec, as NODE-TYPE-SLOTS stores
it) with :ACCESSOR/:INITARG dropped when they are exactly what
%NORMALIZE-SLOT-SPECS would have supplied -- so the exported form reads
the way a developer would have written it, not the way the engine
expanded it (GH #172, R6)."
  (destructuring-bind (name &rest plist &key accessor initarg
                            &allow-other-keys)
      spec
    (let ((rest (copy-list plist)))
      (remf rest :accessor)
      (remf rest :initarg)
      (unless (eq accessor name)
        (setf rest (list* :accessor accessor rest)))
      (unless (eq initarg (intern (symbol-name name) :keyword))
        (setf rest (list* :initarg initarg rest)))
      (if rest (list* name rest) name))))

(defun %write-schema-def-form (entry stream)
  (let ((macro (ecase (getf entry :kind)
                (:vertex "def-vertex") (:edge "def-edge")))
        (slots (mapcar #'%minimize-slot-spec (getf entry :slots))))
    (format stream "(~A ~A (~{~A~^ ~})~%    ("
            macro
            (%schema-source-token (getf entry :name))
            (mapcar #'%schema-source-token (getf entry :parents)))
    (loop for spec in slots
          for firstp = t then nil
          do (unless firstp (format stream "~%     "))
             (write-string (%schema-source-form spec) stream))
    (format stream ")~%    ~A"
            (%schema-source-token (getf entry :default-store)))
    (when (getf entry :keep-revisions)
      (format stream "~%    :keep-revisions ~A"
              (getf entry :keep-revisions)))
    (format stream ")~%~%")))

(defun %schema-namespace-nicknames (pkg-name ns-rows)
  (getf (find pkg-name ns-rows :key (lambda (r) (getf r :namespace))
             :test #'string-equal)
        :nicknames))

(defun %write-schema-namespace (group ns-rows stream)
  (let* ((pkg-name (car group))
         (entries (cdr group))
         (nicks (%schema-namespace-nicknames pkg-name ns-rows)))
    (format stream "(defpackage #:~(~A~) (:use #:cl #:graph-db)~%"
            pkg-name)
    (when nicks
      (format stream "  (:nicknames~{ #:~(~A~)~})~%" nicks))
    (format stream "  (:export~{ #:~(~A~)~}))~%"
            (mapcar (lambda (e) (symbol-name (getf e :name))) entries))
    (format stream "(in-package #:~(~A~))~%~%" pkg-name)
    (dolist (e entries) (%write-schema-def-form e stream))))

(defun export-schema-source (path &key namespace store)
  "Write PATH: a generated-header comment, one DEFPACKAGE per exported
namespace ((:use #:cl #:graph-db) plus :export of its class names --
the SOURCE-package shape, per ENSURE-NAMESPACE's docstring; the runtime
package it actually creates uses :USE NIL instead), IN-PACKAGE, and
DEF-VERTEX/DEF-EDGE forms rebuilt from metadata.  NAMESPACE/STORE
restrict exactly as in DESCRIBE-SCHEMA.

This is the promotion path: loading the file is the ordinary source
path, and is idempotent (same names -> same registry ids, per
REGISTRY-INTERN). Export never runs implicitly, and the engine never
reads the file back -- it is for the developer's build only.  Returns
the file's truename (GH #172, R6)."
  (let* ((entries (%filter-schema-entries (%describe-schema-entries)
                                          namespace store nil))
         (groups (%group-schema-entries-by-namespace entries))
         (ns-rows (nth-value 0 (ignore-errors
                                (read-schema-manifest *system-directory*)))))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-right-margin* 79))
        (format out ";;; Generated by graph-db:export-schema-source ~A.~%"
                (%schema-iso-date (get-universal-time)))
        (format out ";;; Source of truth remains the persisted metadata ~
until this~%")
        (format out ";;; file is loaded as part of the system; loading ~
it is~%")
        (format out ";;; idempotent (same names -> same registry ids).~%~%")
        (dolist (group groups)
          (%write-schema-namespace group ns-rows out))))
    (truename path)))
