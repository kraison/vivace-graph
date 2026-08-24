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

(defun %read-schema-manifest-safe (dir)
  "(VALUES NAMESPACE-ROWS TYPE-ROWS), like READ-SCHEMA-MANIFEST, but
never signals: an error (a malformed DIR, an unreadable file despite
READ-SCHEMA-MANIFEST's own guards) degrades to two empty lists, exactly
like a missing manifest -- HANDLER-CASE, not IGNORE-ERRORS, because
IGNORE-ERRORS's error branch returns (VALUES NIL CONDITION), and a
caller taking this function's SECOND value would get the CONDITION
object where it expects the type-row list (GH #172, R6, review round
1)."
  (handler-case (read-schema-manifest dir)
    (error () (values nil nil))))

(defun %describe-schema-entries ()
  "Every known node type as one plist: (:NAME :KIND :DEFAULT-STORE
:SLOTS :PARENTS :KEEP-REVISIONS :PROVENANCE :TIME).  Joins the system
manifest with every live META across every open store; a type with no
manifest row (a pre-#172 store) gets :PROVENANCE :SOURCE and :TIME NIL
-- describe/export must never signal on a missing or damaged manifest,
so this degrades to live-metas-only rather than erroring (GH #172, R6)."
  (let ((rows (nth-value 1 (%read-schema-manifest-safe *system-directory*)))
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
string parsed at UTC midnight -- matching %SCHEMA-ISO-DATE, which
decodes each row's :TIME at UTC too, so the two agree instead of one
using the local zone and the other UTC (GH #172, review round 3, M-6).
NIL passes through."
  (etypecase since
    (null nil)
    (integer since)
    (string
     (encode-universal-time 0 0 0
                            (parse-integer since :start 8 :end 10)
                            (parse-integer since :start 5 :end 7)
                            (parse-integer since :start 0 :end 4)
                            0))))

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

(defun %schema-name-shadowed-p (name)
  "T when NAME (a string) already names an EXTERNAL symbol of COMMON-
LISP or GRAPH-DB -- the two packages the generated file's DEFPACKAGE
always :USEs.  A symbol homed in the target namespace but sharing NAME
with one of these (e.g. a slot named TYPE) needs a :SHADOW clause in
the generated DEFPACKAGE (%SCHEMA-NAMESPACE-SHADOW-NAMES): INTERN --
which is what both a bare token AND an explicit NS::NAME qualification
reduce to -- returns the INHERITED symbol whenever the name is merely
accessible via :USE, so qualification ALONE cannot recover a distinct
local symbol; only :SHADOW makes the local one accessible instead
(GH #172, review round 3, M-2)."
  (or (eq :external (nth-value 1 (find-symbol name (find-package :cl))))
      (eq :external (nth-value 1 (find-symbol name (find-package
                                                     :graph-db))))))

(defun %schema-symbol-prints-bare-p (sym target-pkg-name)
  "T when SYM's home package is one the generated file's DEFPACKAGE
makes a bare token resolve to the SAME symbol under: the namespace
itself (matched by NAME -- the export-time package and the one the
generated file's IN-PACKAGE creates at load time are different OBJECTS
with the same name; a colliding name is safe to print bare too, since
%WRITE-SCHEMA-NAMESPACE adds a :SHADOW clause for it -- see
%SCHEMA-NAME-SHADOWED-P), or COMMON-LISP/GRAPH-DB, which the generated
DEFPACKAGE always :USEs.  Any OTHER home package must be qualified:
printing it bare would, at load time, INTERN A NEW SYMBOL under the
namespace package instead of finding the original one, silently
breaking any EQ-keyed lookup on it -- e.g. a :CHECK name against
*SCHEMA-FUNCTIONS* (GH #172, R6, review round 1)."
  (let ((home (symbol-package sym)))
    (and home
        (or (and target-pkg-name
                (string-equal (package-name home) target-pkg-name))
            (eq home (find-package :common-lisp))
            (eq home (find-package :graph-db))))))

(defun %schema-qualified-symbol (sym)
  "SYM as PACKAGE:NAME (external) or PACKAGE::NAME (internal), downcased
(GH #172, R6, review round 1)."
  (let* ((pkg (symbol-package sym))
         (external-p (eq :external
                        (nth-value 1
                                   (find-symbol (symbol-name sym) pkg)))))
    (format nil "~(~A~)~:[::~;:~]~(~A~)"
            (package-name pkg) external-p (symbol-name sym))))

(defun %schema-source-symbol-token (sym target-pkg-name)
  "SYM as source text under TARGET-PKG-NAME: bare when
%SCHEMA-SYMBOL-PRINTS-BARE-P allows it, package-qualified otherwise --
except an UNINTERNED symbol (no home package at all), which
%SCHEMA-QUALIFIED-SYMBOL cannot name.  Unreachable through the ordinary
CREATE-*-TYPE/DEF-VERTEX APIs (their slot data always comes from a real
interned symbol), but EXPORT-SCHEMA-SOURCE must never signal an ERROR
regardless: fall back to the bare, downcased name -- the closest-to-
correct readable text, since it will still intern into whatever package
the generated file's IN-PACKAGE is active under -- and WARN, so the
file is flagged for hand review rather than silently wrong (GH #172,
R6, review round 2)."
  (if (symbol-package sym)
      (if (%schema-symbol-prints-bare-p sym target-pkg-name)
          (string-downcase (symbol-name sym))
          (%schema-qualified-symbol sym))
      (progn
        (warn "EXPORT-SCHEMA-SOURCE: symbol ~S~@[, exporting namespace ~
~A,~] has no home package (uninterned) -- printing it bare.  The ~
generated file will intern a NEW symbol under its own IN-PACKAGE; ~
review this output by hand (GH #172, R6, review round 2)."
              sym target-pkg-name)
        (string-downcase (symbol-name sym)))))

(defun %schema-source-token (x &optional target-pkg-name)
  "X as generated-source text.  A symbol prints bare and downcased only
when %SCHEMA-SYMBOL-PRINTS-BARE-P says a bare token will resolve back
to the SAME symbol under TARGET-PKG-NAME (the namespace the caller is
printing this form for); otherwise it prints package-qualified.
TARGET-PKG-NAME NIL (DESCRIBE-SCHEMA's callers, and any keyword/literal
value) never qualifies -- describe-schema's output is not reloaded
(GH #172, R6)."
  (cond
    ((null x) "nil")
    ((eq x t) "t")
    ((keywordp x) (format nil ":~(~A~)" (symbol-name x)))
    ((symbolp x) (%schema-source-symbol-token x target-pkg-name))
    ((stringp x) (prin1-to-string x))
    ((integerp x) (princ-to-string x))
    (t (string-downcase (princ-to-string x)))))

(defun %schema-source-form (form &optional target-pkg-name)
  "FORM (nested lists of symbols/keywords/literals only, as produced by
NODE-TYPE-SLOTS) as downcased source text, package-qualifying any
symbol foreign to TARGET-PKG-NAME (GH #172, R6)."
  (if (consp form)
      (format nil "(~{~A~^ ~})"
              (mapcar (lambda (f) (%schema-source-form f target-pkg-name))
                      form))
      (%schema-source-token form target-pkg-name)))

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
doubles as a change log; a \"YYYY-MM-DD\" string is parsed at UTC
midnight, matching the UTC dates printed in [runtime YYYY-MM-DD] tags
(GH #172, review round 3, M-6).  Never signals on a missing or damaged
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

(defun %write-schema-def-form (entry pkg-name stream)
  "Write ENTRY's DEF-VERTEX/DEF-EDGE form for the namespace PKG-NAME
(a string): every symbol not homed in PKG-NAME/COMMON-LISP/GRAPH-DB is
package-qualified so it re-reads to the SAME symbol later, not a new
one interned into the freshly (re)created namespace package (GH #172,
R6, review round 1)."
  (let ((macro (ecase (getf entry :kind)
                (:vertex "def-vertex") (:edge "def-edge")))
        (slots (mapcar #'%minimize-slot-spec (getf entry :slots))))
    (format stream "(~A ~A (~{~A~^ ~})~%    ("
            macro
            (%schema-source-token (getf entry :name) pkg-name)
            (mapcar (lambda (p) (%schema-source-token p pkg-name))
                    (getf entry :parents)))
    (loop for spec in slots
          for firstp = t then nil
          do (unless firstp (format stream "~%     "))
             (write-string (%schema-source-form spec pkg-name) stream))
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

(defun %schema-namespace-shadow-names (pkg-name entries)
  "Every symbol NAME (a type name, a parent, or a slot/accessor name --
any leaf ENTRIES prints) that is homed in PKG-NAME and shares its name
with an external COMMON-LISP or GRAPH-DB symbol, deduplicated.  The
generated DEFPACKAGE must :SHADOW these: %SCHEMA-NAME-SHADOWED-P's
docstring explains why qualification alone cannot recover the local
symbol once the namespace package itself :USEs CL/GRAPH-DB (GH #172,
review round 3, M-2)."
  (let ((pkg-name (string-upcase pkg-name)) (out nil))
    (labels ((consider (sym)
               (when (and (symbolp sym) (symbol-package sym)
                         (string= (package-name (symbol-package sym))
                                 pkg-name)
                         (%schema-name-shadowed-p (symbol-name sym))
                         (not (member (symbol-name sym) out
                                     :test #'string=)))
                 (push (symbol-name sym) out)))
             (walk (x) (if (consp x) (progn (walk (car x)) (walk (cdr x)))
                          (consider x))))
      (dolist (e entries)
        (consider (getf e :name))
        (mapc #'consider (getf e :parents))
        (dolist (spec (getf e :slots)) (walk (%minimize-slot-spec spec)))))
    (nreverse out)))

(defun %write-schema-namespace (group ns-rows stream)
  (let* ((pkg-name (car group))
         (entries (cdr group))
         (nicks (%schema-namespace-nicknames pkg-name ns-rows))
         (shadows (%schema-namespace-shadow-names pkg-name entries)))
    (format stream "(defpackage #:~(~A~) (:use #:cl #:graph-db)~%"
            pkg-name)
    (when nicks
      (format stream "  (:nicknames~{ #:~(~A~)~})~%" nicks))
    (when shadows
      (format stream "  (:shadow~{ #:~(~A~)~})~%" shadows))
    ;; One name per line (MINOR 1, review round 1): a ten-plus-type
    ;; namespace's :EXPORT list is the known >80-column offender when
    ;; run together on one line.
    (format stream "  (:export~%")
    (dolist (e entries)
      (format stream "   #:~(~A~)~%" (symbol-name (getf e :name))))
    (format stream "   ))~%")
    (format stream "(in-package #:~(~A~))~%~%" pkg-name)
    (dolist (e entries) (%write-schema-def-form e pkg-name stream))))

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
         (ns-rows (nth-value 0
                             (%read-schema-manifest-safe
                              *system-directory*))))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (format out ";;; Generated by graph-db:export-schema-source ~A.~%"
              (%schema-iso-date (get-universal-time)))
      (format out ";;; Source of truth remains the persisted metadata ~
until this~%")
      (format out ";;; file is loaded as part of the system; loading ~
it is~%")
      (format out ";;; idempotent (same names -> same registry ids).~%~%")
      (dolist (group groups)
        (%write-schema-namespace group ns-rows out)))
    (truename path)))
