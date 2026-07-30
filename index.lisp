(in-package :graph-db)

;;;; General ordered secondary index (:INDEX slot option / DEF-INDEX).
;;;; See docs/general-index-design.md for the full design.
;;;;
;;;; This is ":UNIQUE minus enforcement": a duplicate-free composite
;;;; (canonical-value . node-id) ordered map maintained on the commit APPLY path,
;;;; supporting EQUALITY lookup AND ascending RANGE scans.  It reuses the view
;;;; composite-key codec and rides the ordered-map seam (MAKE-VIEW-SKIP-LIST), so
;;;; it is backend-agnostic -- skip list or B+ tree per the graph's :INDEX-BACKEND,
;;;; and a MEM-SKIP-LIST on a memory-graph -- with NO hash-table special case
;;;; (unlike :UNIQUE, whose memory backend is an equality-only hash; an ordered
;;;; index needs order on every backend, for range).
;;;;
;;;; Unlike :UNIQUE there is:
;;;;   * no enforcement (no VALIDATE pass -- an ordered index never rejects),
;;;;   * no :ORIGIN/scope partitioning,
;;;;   * MANY ids per value (a non-unique slot): a value is one prefix in the map,
;;;;     retrieved by range-scanning [(value +null-key+) (value +max-key+)].

;;; ---------------------------------------------------------------------------
;;; Spec resolution: the :INDEX / DEF-INDEX spec -> a CANONICALIZER (type-as-hint)
;;; ---------------------------------------------------------------------------

(defun %resolve-index-canonicalizer (spec)
  "Resolve an index SPEC to a 1-arg CANONICALIZER applied to the slot value before
keying, or NIL for identity.  T (plain :INDEX t) -> NIL; a symbol / #'fn / lambda
form -> that function (case-fold etc.).  Mirrors :UNIQUE's resolver, minus the
hash-test (the ordered map keys by LESS-THAN, not a hash)."
  (cond
    ((or (null spec) (eq spec t))                 nil)
    ((functionp spec)                             spec)
    ((and (consp spec) (eq (car spec) 'function)) (fdefinition (cadr spec)))
    ((and (consp spec) (eq (car spec) 'lambda))   (coerce spec 'function))
    ((symbolp spec)                               (fdefinition spec))
    (t (error "Invalid :INDEX spec ~S" spec))))

;;; ---------------------------------------------------------------------------
;;; Per-class descriptors (MOP introspection over the :INDEX slot option)
;;; ---------------------------------------------------------------------------

;; %INDEXED-SLOT-OWNER-NAME lives in node-class.lisp (it is shared with the
;; spatial maintenance path in transactions.lisp, which loads before this file).

(defun class-indexed-slots (class)
  "List of (SLOT-NAME OWNER-NAME SPEC) for CLASS's :INDEX effective slots; NIL for a
class with none (the common case).  SPEC is the raw :INDEX value (T or a
canonicalizer)."
  (when (class-finalized-p class)
    (loop for s in (class-slots class)
          for spec = (indexed-p s)
          when spec
          collect (list (slot-definition-name s)
                        (%indexed-slot-owner-name class (slot-definition-name s))
                        spec))))

;;; ---------------------------------------------------------------------------
;;; DEF-INDEX: declarative registry (mirror the #49 DEF-VIEW registry).  A DEF-INDEX
;;; adds an ordered index on (OWNER . SLOT) that need NOT be marked :INDEX on the
;;; slot; it is unioned with the MOP :INDEX slots in CLASS-SECONDARY-INDEX-DESCRIPTORS
;;; so apply / rebuild / restore all cover it.
;;; ---------------------------------------------------------------------------

(defvar *schema-index-metadata* (make-hash-table)
  "graph-name (symbol) -> list of INDEX-SPECs (newest first): the declarative
DEF-INDEX registry, reconciled at open by INSTALL-SECONDARY-INDEXES.")

(defstruct (index-spec (:constructor make-index-spec))
  owner-name slot-name graph-name canonicalize)

(defun register-index-spec (spec)
  "Phase 1: record SPEC.  Duplicates accumulate; resolved newest-wins."
  (push spec (gethash (index-spec-graph-name spec) *schema-index-metadata*))
  spec)

(defun %registered-index-specs (graph)
  "DEF-INDEX specs registered for GRAPH, de-duped by (owner . slot), newest-wins."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (spec (gethash (graph-name graph) *schema-index-metadata*))
      (let ((k (cons (index-spec-owner-name spec) (index-spec-slot-name spec))))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push spec result))))
    (nreverse result)))

(defun %slot-present-p (class slot-name)
  (and (find slot-name (class-slots class) :key #'slot-definition-name) t))

(defun %applicable-index-descriptors (class graph)
  "(slot owner spec) descriptors from the DEF-INDEX registry applying to CLASS:
owner is CLASS or an ancestor (subtype IS-A) and the slot exists in CLASS."
  (when (class-finalized-p class)
    (loop for spec in (%registered-index-specs graph)
          for owner = (index-spec-owner-name spec)
          for slot = (index-spec-slot-name spec)
          when (and (subtypep (class-name class) owner)
                    (%slot-present-p class slot))
          collect (list slot owner (index-spec-canonicalize spec)))))

(defun class-secondary-index-descriptors (class graph)
  "All (slot owner spec) descriptors for CLASS: its MOP :INDEX effective slots plus
the DEF-INDEX definitions applicable to it, de-duped by (owner . slot) (MOP first).
This is the single input to maintenance (apply / rebuild)."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (d (append (class-indexed-slots class)
                       (%applicable-index-descriptors class graph)))
      (let ((k (cons (second d) (first d))))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push d result))))
    (nreverse result)))

;;; ---------------------------------------------------------------------------
;;; The index + the graph's registry
;;; ---------------------------------------------------------------------------

(defstruct (slot-index (:constructor %make-slot-index))
  owner-name slot-name canonicalizer
  ;; The backing ordered map: a heap skip-list / B+ tree on an on-disk graph, a
  ;; MEM-SKIP-LIST on a memory-graph -- always ordered (needed for range).  Keyed
  ;; by the composite (canonical-value id) under REDUCE-COMP-LESSP, like a view.
  skip-list)

(defun make-secondary-skip-list (graph)
  "The ordered map backing a secondary index -- a view-style composite
(canonical-value id) map under REDUCE-COMP-LESSP.  Follows *INDEX-BACKEND* on an
on-disk graph and returns a MEM-SKIP-LIST on a memory-graph."
  (make-view-skip-list graph (make-view :sort-order :lessp)))

(defun %open-secondary-skip-list (graph address &optional (backend :skip-list))
  "Reopen an on-disk secondary index at ADDRESS with BACKEND's opener (same codec/
order as a view / unique index)."
  (open-heap-index backend :address address :heap (indexes graph)
                   :comparison 'reduce-comp-lessp))

(defun %index-key (six value)
  "The canonical key VALUE maps to in SIX (canonicalizer applied), or NIL for a
NULL/unbound value (exempt, SQL-style -- not indexed)."
  (when value
    (let ((c (slot-index-canonicalizer six)))
      (if c (funcall c value) value))))

(defun %indexable-value-p (value)
  "True if VALUE belongs in a general ordered index.  Excludes NULL/unbound (exempt,
SQL-style) and GEOMETRY values -- an :INDEX-marked geometry slot is the SPATIAL
index's domain (NODE-GEOMETRY picks it by GEOMETRYP), so the same slot option
routes a geometry slot to spatial and a scalar slot here, dispatched by value type.
Detected by runtime value (GEOMETRYP), like the spatial hook -- the declared
`:type geometry' symbol is not reliably comparable across packages."
  (and value (not (geometryp value))))

;;; Backend-agnostic ops over the ordered map (VALUE = the canonical key).

(defun ix-lookup (six value)
  "List of node-ids whose indexed slot canonicalizes to VALUE (a prefix range scan);
NIL if none.  VALUE is the already-canonical key."
  (let ((cur (make-range-cursor (slot-index-skip-list six)
                                (list value +null-key+) (list value +max-key+)))
        (ids '()))
    (loop for node = (cursor-next cur :eoc) until (eql node :eoc)
          do (push (second (%sn-key node)) ids))
    (nreverse ids)))

(defun ix-put (six value id)
  "Enter node ID under canonical VALUE.  The id lives in the composite key's second
slot (read back by IX-LOOKUP); the skip-node VALUE is unused -- store NIL, not the
raw id byte array (which SERIALIZE cannot round-trip)."
  (add-to-skip-list (slot-index-skip-list six) (list value id) nil))

(defun ix-remove (six value id)
  "Remove node ID's entry under canonical VALUE (that composite key only)."
  (remove-from-skip-list (slot-index-skip-list six) (list value id)))

(defun ix-map (six fn &key start end)
  "Call FN with (VALUE ID) for every entry with START <= value <= END, in ascending
value order.  Open-ended when START/END is NIL.  Bounded ranges use a range cursor
(the efficient path); an open end falls back to an ordered full scan + bound filter."
  (let ((sl (slot-index-skip-list six)))
    (if (and start end)
        ;; Efficient bounded path: [ (start +null-key+) .. (end +max-key+) ].
        (let ((cur (make-range-cursor sl (list start +null-key+) (list end +max-key+))))
          (loop for node = (cursor-next cur :eoc) until (eql node :eoc)
                do (funcall fn (first (%sn-key node)) (second (%sn-key node)))))
        ;; Open-ended: ordered full scan, filtering by whatever bound is present.
        (let ((cur (make-cursor sl)))
          (loop for node = (cursor-next cur :eoc) until (eql node :eoc)
                for k = (first (%sn-key node))
                when (and (or (null start) (not (less-than k start)))
                          (or (null end)   (not (less-than end k))))
                do (funcall fn k (second (%sn-key node))))))))

(defun ix-count (six)
  "Number of live entries in SIX."
  (let ((cur (make-cursor (slot-index-skip-list six))) (n 0))
    (loop for node = (cursor-next cur :eoc) until (eql node :eoc) do (incf n))
    n))

(defun %slot-index-for (graph descriptor)
  "Get-or-create the (empty) SLOT-INDEX for DESCRIPTOR = (slot owner spec), keyed by
(owner . slot) in GRAPH's registry.  Resolves the canonicalizer from SPEC on
creation and builds the ordered map on the graph's backend."
  (destructuring-bind (slot-name owner-name spec) descriptor
    (let* ((reg (or (secondary-indexes graph)
                    (setf (secondary-indexes graph)
                          (make-hash-table :test 'equal
                                           #+sbcl :synchronized #+sbcl t
                                           #+ccl :shared #+ccl t
                                           #+graph-db-ecl-sync-hash :synchronized
                                           #+graph-db-ecl-sync-hash t))))
           (key (cons owner-name slot-name)))
      (or (gethash key reg)
          (let ((six (%make-slot-index
                      :owner-name owner-name :slot-name slot-name
                      :canonicalizer (%resolve-index-canonicalizer spec))))
            (setf (slot-index-skip-list six) (make-secondary-skip-list graph))
            (setf (gethash key reg) six))))))

;;; ---------------------------------------------------------------------------
;;; Maintenance (APPLY, post-durability, journal-replayable -- no enforcement)
;;; ---------------------------------------------------------------------------

(defun %ix-claim (node graph)
  "Index NODE's indexed slot values (create / new value of an update)."
  (dolist (d (class-secondary-index-descriptors (class-of node) graph))
    (let ((value (slot-value node (first d))))
      ;; Gate BEFORE %SLOT-INDEX-FOR so a geometry slot never creates an ordered
      ;; index (it is the spatial index's; %INDEXABLE-VALUE-P skips it).
      (when (%indexable-value-p value)
        (let* ((six (%slot-index-for graph d))
               (key (%index-key six value)))
          (when key (ix-put six key (id node))))))))

(defun %ix-release (node graph)
  "Remove NODE's indexed slot values (delete / old value of an update)."
  (dolist (d (class-secondary-index-descriptors (class-of node) graph))
    (let ((value (slot-value node (first d))))
      (when (%indexable-value-p value)
        (let* ((six (%slot-index-for graph d))
               (key (%index-key six value)))
          (when key (ix-remove six key (id node))))))))

(defgeneric apply-tx-write-to-secondary-indexes (write graph)
  (:method (write graph) (declare (ignore write graph)) nil))

(defmethod apply-tx-write-to-secondary-indexes ((write tx-create) graph)
  (%ix-claim (node write) graph))

(defmethod apply-tx-write-to-secondary-indexes ((write tx-update) graph)
  (%ix-release (old-node write) graph)
  (unless (deleted-p (node write))
    (%ix-claim (node write) graph)))

;; tx-delete is a tx-update subclass; the node is marked deleted -> release only.
(defmethod apply-tx-write-to-secondary-indexes ((write tx-delete) graph)
  (%ix-release (old-node write) graph)
  (%ix-release (node write) graph))

(defun apply-tx-writes-to-secondary-indexes (writes graph)
  (dolist (write writes) (apply-tx-write-to-secondary-indexes write graph)))

;;; ---------------------------------------------------------------------------
;;; Rebuild on open (fallback when there is no sidecar to reopen from)
;;; ---------------------------------------------------------------------------

(defun %graph-has-indexed-slots-p (graph)
  "Cheap guard so a graph with no :INDEX slots pays nothing at open."
  (dolist (nt (all-node-types graph) nil)
    (let* ((name (if (node-type-p nt) (node-type-name nt) nt))
           (c (and name (ignore-errors (find-class name nil)))))
      (when (and c (class-finalized-p c) (class-indexed-slots c))
        (return t)))))

(defun rebuild-secondary-indexes (graph)
  "(Re)populate the secondary indexes by scanning live nodes once, off the commit
path (at open).  The fallback when there is no sidecar to reopen from (fresh graph,
or a crash before the roots were saved)."
  (when (%graph-has-indexed-slots-p graph)
    (let ((*graph* graph))
      (flet ((index-node (node)
               (unless (deleted-p node)
                 (dolist (d (class-secondary-index-descriptors (class-of node) graph))
                   (let ((value (slot-value node (first d))))
                     (when (%indexable-value-p value)
                       (let* ((six (%slot-index-for graph d))
                              (key (%index-key six value)))
                         (when key (ix-put six key (id node))))))))))
        (map-vertices #'index-node graph)
        (map-edges #'index-node graph)))))

;;; ---------------------------------------------------------------------------
;;; Durable persistence -- ON-DISK backend (each index is a persistent heap ordered
;;; map; save its root address in a sidecar at CLOSE, reopen at OPEN -- no node scan.
;;; Mirrors the unique-index sidecar.  Memory-backend image dump/load is a follow-up.
;;; ---------------------------------------------------------------------------

(defun secondary-index-root-file (location)
  (format nil "~A/secondary-indexes.dat" location))

(defun save-secondary-index-roots (graph)
  "Persist the on-disk secondary indexes' roots (owner slot address backend-tag).
No-op with no heap (memory) or no indexes.  Called at CLOSE-GRAPH.  The canonicalizer
is NOT stored (a function is not serializable); it is re-resolved from the owner
class's live :INDEX spec on reopen -- only the address+backend are needed to reopen
the ordered map."
  (when (and (indexes graph) (secondary-indexes graph))
    (let ((roots '()))
      (maphash (lambda (k six)
                 (declare (ignore k))
                 (when (and (slot-index-skip-list six)
                            (view-index-p (slot-index-skip-list six)))
                   (push (list (slot-index-owner-name six) (slot-index-slot-name six)
                               (view-index-address (slot-index-skip-list six))
                               (view-index-backend-tag (slot-index-skip-list six)))
                         roots)))
               (secondary-indexes graph))
      (%atomic-cl-store roots (secondary-index-root-file (location graph))))))

(defun %owner-slot-canonicalizer (owner-name slot-name graph)
  "Resolve the canonicalizer for (OWNER-NAME . SLOT-NAME) from the owner class's live
:INDEX spec, or from a matching DEF-INDEX spec registered for GRAPH; NIL if neither
is found.  Used on reopen to re-associate a persisted index with its live spec (the
canonicalizer is a function, not serializable, so it is re-resolved, not stored)."
  (or (let ((c (ignore-errors (find-class owner-name nil))))
        (when (and c (class-finalized-p c))
          (let ((d (find slot-name (class-indexed-slots c) :key #'first)))
            (when d (%resolve-index-canonicalizer (third d))))))
      (let ((spec (find-if (lambda (s)
                             (and (eq (index-spec-owner-name s) owner-name)
                                  (eq (index-spec-slot-name s) slot-name)))
                           (%registered-index-specs graph))))
        (when spec (%resolve-index-canonicalizer (index-spec-canonicalize spec))))))

(defun restore-secondary-index-roots (graph)
  "Reopen the on-disk secondary indexes from the sidecar -- no node scan.  Returns T
if a sidecar was present (caller skips REBUILD-SECONDARY-INDEXES); NIL to fall back
to rebuild (a fresh graph, or an unreadable sidecar).  The canonicalizer is
re-resolved from the owner class's live :INDEX spec.

An UNREADABLE sidecar falls back to rebuild rather than failing the open (GH #63),
mirroring RESTORE-SPATIAL-INDEX-ROOTS -- nodes remain authoritative, so
REBUILD-SECONDARY-INDEXES reconstructs the truth.  :UNREADABLE is a sentinel
distinct from NIL: a graph with no secondary indexes declared saves an empty
list, which must still count as a successfully-restored (if empty) sidecar,
not trigger a spurious rebuild."
  (let ((file (secondary-index-root-file (location graph))))
    (when (probe-file file)
      (let ((records (handler-case (cl-store:restore file)
                        (error (e)
                          (warn "Secondary index sidecar ~A is unreadable (~A); rebuilding ~
                                 from live nodes, which are authoritative."
                                file e)
                          :unreadable))))
        (unless (eq records :unreadable)
          (let ((reg (or (secondary-indexes graph)
                         (setf (secondary-indexes graph)
                               (make-hash-table :test 'equal
                                                #+sbcl :synchronized #+sbcl t
                                                #+ccl :shared #+ccl t
                                                #+graph-db-ecl-sync-hash :synchronized
                                                #+graph-db-ecl-sync-hash t)))))
            (dolist (r records)
              (destructuring-bind (owner slot address &optional (backend :skip-list)) r
                (setf (gethash (cons owner slot) reg)
                      (%make-slot-index :owner-name owner :slot-name slot
                                        :canonicalizer (%owner-slot-canonicalizer owner slot graph)
                                        :skip-list (%open-secondary-skip-list
                                                    graph address backend))))))
          t)))))

(defun regenerate-secondary-indexes (graph)
  "Drop every on-disk secondary index and rebuild it on GRAPH's CURRENT :INDEX-BACKEND,
persisting the new backend tags.  The parallel of REGENERATE-ALL-VIEWS /
REGENERATE-UNIQUE-INDEXES / REBUILD-SPATIAL-INDEX for an in-place backend switch."
  (when (secondary-indexes graph)
    (maphash (lambda (k six)
               (declare (ignore k))
               (let ((sl (slot-index-skip-list six)))
                 (when (and sl (view-index-p sl)) (delete-view-index sl))))
             (secondary-indexes graph))
    (clrhash (secondary-indexes graph)))
  (rebuild-secondary-indexes graph)
  (save-secondary-index-roots graph)
  graph)

;;; ---------------------------------------------------------------------------
;;; DEF-INDEX build + open-time install (Phase 2, mirror INSTALL-VIEWS)
;;; ---------------------------------------------------------------------------

(defun %build-index-for-spec (graph spec)
  "Create and fully populate the ordered index for a DEF-INDEX SPEC by scanning
OWNER's nodes (and subclasses)."
  (let* ((owner (index-spec-owner-name spec))
         (slot (index-spec-slot-name spec))
         (six (%slot-index-for graph (list slot owner (index-spec-canonicalize spec))))
         (*graph* graph))
    (flet ((index-node (node)
             (unless (deleted-p node)
               (let ((value (ignore-errors (slot-value node slot))))
                 (when (%indexable-value-p value)
                   (let ((key (%index-key six value)))
                     (when key (ix-put six key (id node)))))))))
      (if (subtypep owner 'edge)
          (map-edges #'index-node graph :edge-type owner)
          (map-vertices #'index-node graph :vertex-type owner)))
    six))

(defun %ensure-index-built (graph spec)
  "Build SPEC's index unless it already exists in GRAPH's registry (idempotent)."
  (let ((key (cons (index-spec-owner-name spec) (index-spec-slot-name spec))))
    (unless (and (secondary-indexes graph) (gethash key (secondary-indexes graph)))
      (%build-index-for-spec graph spec))))

(defun install-secondary-indexes (graph)
  "Phase 2 (mirror INSTALL-VIEWS): build any DEF-INDEX registered for GRAPH that is
missing from its registry -- one defined before the graph opened, or added since the
last close.  Scans only for the missing, so a normal reopen (sidecar restored all)
does no work.  Called at open right after the restore-or-rebuild."
  (dolist (spec (%registered-index-specs graph))
    (%ensure-index-built graph spec)))

(defmacro def-index (owner-class slot graph-name &key canonicalize)
  "Declare an ordered secondary index on OWNER-CLASS.SLOT in GRAPH-NAME (spanning
OWNER-CLASS's subclasses).  Declarative and idempotent like DEF-VIEW: it registers
the index and, if the graph is already open, builds it now; otherwise INSTALL-
SECONDARY-INDEXES builds it at open (an index may thus be declared before its graph
exists).  :CANONICALIZE is an optional 1-arg function (symbol / #'fn / lambda form)
applied to the value before keying -- e.g. STRING-DOWNCASE for a case-insensitive
index.  Query with INDEX-LOOKUP / INDEX-RANGE / MAP-INDEX.

Unlike the (slot :index t) slot option, DEF-INDEX need not touch the class
definition, and is the home for future composite / multi-slot indexes.  Re-evaluating
an unchanged DEF-INDEX is a no-op; to adopt a changed :CANONICALIZE, force a rebuild
with REGENERATE-SECONDARY-INDEXES."
  `(let ((spec (make-index-spec :owner-name ',owner-class :slot-name ',slot
                                :graph-name ',graph-name
                                :canonicalize ,(when canonicalize `',canonicalize))))
     (register-index-spec spec)
     (let ((g (lookup-graph ',graph-name)))
       (when g (%ensure-index-built g spec)))
     spec))

;;; ---------------------------------------------------------------------------
;;; Query API -- all resolve node ids in the PASSED graph (wrong-graph discipline).
;;; ---------------------------------------------------------------------------

(defun %secondary-index-lookup (graph class-name slot-name)
  "The SLOT-INDEX covering CLASS-NAME.SLOT-NAME, or NIL if none has been CREATED yet.
An index rooted at an ancestor of CLASS-NAME covers it (subtype IS-A).  NB: indexes
are created lazily on first non-null value, so NIL here does NOT mean the slot is
unindexed -- see %SLOT-INDEX-DECLARED-P."
  (let ((reg (secondary-indexes graph)))
    (when reg
      (or (gethash (cons class-name slot-name) reg)
          (let ((class (ignore-errors (find-class class-name nil))))
            (when class
              (loop for c in (class-precedence-list class)
                    for hit = (and (typep c 'node-class)
                                   (gethash (cons (class-name c) slot-name) reg))
                    when hit return hit)))))))

(defun %slot-index-declared-p (class-name slot-name)
  "True if CLASS-NAME declares SLOT-NAME as an :INDEX slot (effective, so inherited
:INDEX from an ancestor counts).  Distinguishes a declared-but-empty index (no
struct created yet) from a genuinely unindexed slot."
  (let ((class (ignore-errors (find-class class-name nil))))
    (and class (class-finalized-p class)
         (find slot-name (class-indexed-slots class) :key #'first)
         t)))

(defun %def-index-declared-p (graph class-name slot-name)
  "True if a DEF-INDEX registered for GRAPH covers CLASS-NAME.SLOT-NAME (owner is
CLASS-NAME or an ancestor)."
  (and (ignore-errors (find-class class-name nil))
       (some (lambda (spec)
               (and (eq (index-spec-slot-name spec) slot-name)
                    (subtypep class-name (index-spec-owner-name spec))))
             (%registered-index-specs graph))))

(defun %require-index (graph class-name slot-name)
  "The SLOT-INDEX for CLASS-NAME.SLOT-NAME.  Returns NIL when the slot is a declared
index (via :INDEX or DEF-INDEX) but no entries exist yet (a legitimately empty
result); signals only when the slot is not indexed at all (a programming error)."
  (let ((six (%secondary-index-lookup graph class-name slot-name)))
    (cond (six six)
          ((or (%slot-index-declared-p class-name slot-name)
               (%def-index-declared-p graph class-name slot-name))
           nil)                                             ; declared, empty
          (t (error "No secondary index on ~S.~S in ~S"
                    class-name slot-name (graph-name graph))))))

(defun index-lookup (graph class-name slot-name value &key (collect-p t))
  "Nodes of CLASS-NAME (and subclasses) whose SLOT-NAME equals VALUE, via the
secondary index (equality).  Signals if no index covers CLASS-NAME.SLOT-NAME.
Resolves ids in GRAPH.  With COLLECT-P NIL, returns T as soon as one match is found."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                            ; NIL => declared but empty => no matches
      (let ((key (%index-key six value))
            (result '()))
        (when key
          (dolist (id (ix-lookup six key))
            (let ((node (%node-by-id id graph)))
              (when (and node (not (deleted-p node)))
                (if collect-p (push node result) (return-from index-lookup t))))))
        (when collect-p (nreverse result))))))

(defun map-index (fn graph class-name slot-name &key start end)
  "Call FN on each live node of CLASS-NAME (and subclasses) whose SLOT-NAME is in
[START,END] (inclusive; open-ended when NIL), in ascending value order.  Resolves
ids in GRAPH."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                            ; NIL => declared but empty => nothing to map
      (let ((skey (and start (%index-key six start)))
            (ekey (and end   (%index-key six end))))
        (ix-map six
                (lambda (value id)
                  (declare (ignore value))
                  (let ((node (%node-by-id id graph)))
                    (when (and node (not (deleted-p node)))
                      (funcall fn node))))
                :start skey :end ekey)))))

(defun index-range (graph class-name slot-name &key start end)
  "Nodes of CLASS-NAME (and subclasses) whose SLOT-NAME is in [START,END], ascending.
Resolves ids in GRAPH."
  (let ((result '()))
    (map-index (lambda (node) (push node result)) graph class-name slot-name
               :start start :end end)
    (nreverse result)))
