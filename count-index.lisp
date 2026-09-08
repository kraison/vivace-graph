;;;; count-index.lisp -- a counting index: a counter pair per leading
;;;; prefix of a node's tuple, maintained at commit apply, so a listing
;;;; with counts is a lookup (GH #361).  Spec: docs/superpowers/specs/
;;;; 2026-09-08-count-index-design.md.  Parallel to index.lisp on the
;;;; DEF-UNIQUE pattern: own registry, own graph slot, own sidecar,
;;;; id-free keys (facts note §X).

(in-package #:graph-db)

;;; --------------------------------------------------------------------
;;; Registry (spec §2.1)
;;; --------------------------------------------------------------------

(defvar *schema-count-metadata* (make-hash-table)
  "graph-name -> list of COUNT-INDEX-SPECs (newest first): the
DEF-COUNT-INDEX registry, reconciled at open by INSTALL-COUNT-INDEXES.")

(defstruct (count-index-spec (:constructor make-count-index-spec))
  owner-name slot-names graph-name canonicalize name current-p)

(defun count-index-spec-identity (spec)
  "See %SPEC-IDENTITY: one identity rule across registries (GH #140)."
  (%spec-identity (count-index-spec-owner-name spec)
                  (count-index-spec-slot-names spec)
                  (count-index-spec-name spec)))

(defun register-count-index-spec (spec)
  "Record SPEC, replacing one of the same identity in place."
  (let* ((g (count-index-spec-graph-name spec))
         (id (count-index-spec-identity spec))
         (existing (gethash g *schema-count-metadata*))
         (hit (find id existing :key #'count-index-spec-identity
                                :test #'equal)))
    (setf (gethash g *schema-count-metadata*)
          (if hit (substitute spec hit existing) (cons spec existing))))
  spec)

(defun unregister-count-index-spec (owner-name graph-name
                                    &key slot-names name)
  "Withdraw the declaration identified by (OWNER . NAME) or (OWNER .
SLOT-NAMES); T if one was withdrawn, NIL silently otherwise."
  (let* ((id (%spec-identity owner-name (%normalize-slots slot-names) name))
         (existing (gethash graph-name *schema-count-metadata*))
         (hit (find id existing :key #'count-index-spec-identity
                                :test #'equal)))
    (when hit
      (setf (gethash graph-name *schema-count-metadata*)
            (remove hit existing))
      t)))

(defun %registered-count-index-specs (graph)
  "Count specs registered for GRAPH, de-duped by identity, newest wins."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (spec (gethash (graph-name graph) *schema-count-metadata*))
      (let ((k (count-index-spec-identity spec)))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push spec result))))
    (nreverse result)))

(defun %count-spec-for (owner-name slot-names graph)
  "The live count spec covering (OWNER-NAME . SLOT-NAMES) for GRAPH, or
NIL.  Count indexes come only from DEF-COUNT-INDEX (no MOP arm), so
absence is positive evidence to drop a sidecar record."
  (find-if (lambda (s)
             (and (eq (count-index-spec-owner-name s) owner-name)
                  (equal (count-index-spec-slot-names s) slot-names)))
           (%registered-count-index-specs graph)))

(defun %count-index-spec-declared-p (owner-name slot-names graph)
  (and (%count-spec-for owner-name slot-names graph) t))

(defun %applicable-count-index-specs (class graph)
  "The count specs applying to CLASS: the owner is CLASS or an ancestor
and every slot exists in CLASS (as %APPLICABLE-INDEX-DESCRIPTORS)."
  (when (class-finalized-p class)
    (loop for spec in (%registered-count-index-specs graph)
          when (and (subtypep (class-name class)
                              (count-index-spec-owner-name spec))
                    (every (lambda (s) (%slot-present-p class s))
                           (count-index-spec-slot-names spec)))
          collect spec)))

(defmacro def-count-index (owner-class slots graph-name
                           &key name current-p canonicalize)
  "Declare a counting index on OWNER-CLASS.SLOTS in GRAPH-NAME: a
counter pair (ALL . CURRENT) per leading prefix of each node's tuple,
maintained at commit apply (GH #361).  CURRENT-P names a one-argument
predicate, funcalled at maintenance; NIL keeps ALL only.
:CANONICALIZE is an optional 1-arg function (symbol / #'fn / lambda
form), or a positional list of one per slot, applied to a component
before keying.  Declarative and idempotent like DEF-INDEX: registers,
builds now if the graph is open.  Trap: counts are live at commit
granularity (spec R1).  Re-evaluating an unchanged declaration is a
no-op; to adopt a changed :CURRENT-P or :CANONICALIZE on an open graph,
run REBUILD-COUNT-INDEXES -- the built map keeps the ones it was
created with."
  `(let ((spec (make-count-index-spec
                :owner-name ',owner-class
                :slot-names (%normalize-slots ',slots)
                :graph-name ',graph-name
                :name ',name
                :current-p ',current-p
                :canonicalize ,(when canonicalize `',canonicalize))))
     (register-count-index-spec spec)
     (let ((g (lookup-graph ',graph-name)))
       (when g (%ensure-count-index-built g spec)))
     spec))

(defmacro undef-count-index (owner-class graph-name &key slots name)
  "Withdraw a DEF-COUNT-INDEX declaration; warns when nothing matched
(GH #152).  The built map is reclaimed at the next open."
  `(%withdrawn-p (unregister-count-index-spec ',owner-class ',graph-name
                                              :slot-names ',slots
                                              :name ',name)
                 :count-index ',owner-class ',graph-name ',name ',slots))

;;; --------------------------------------------------------------------
;;; The map (spec §2.2)
;;; --------------------------------------------------------------------

(defstruct (count-index (:constructor %make-count-index))
  owner-name slot-names canonicalizers current-p skip-list)

(defgeneric make-count-skip-list (graph)
  (:documentation "The ordered map backing a count index: keys (DEPTH
V1 ... VK) with no trailing id, under %INDEX-VALUE-LESSP / EQUAL, plain
SERIALIZE codec (GH #361, facts X4/X5).  A MEM-SKIP-LIST on a memory
graph.")
  (:method ((graph graph))
    (make-heap-index (graph-index-backend graph) (indexes graph)
                     '%index-value-lessp
                     :head-key (list +min-sentinel+)
                     :tail-key (list +max-sentinel+)
                     :key-equal 'equal
                     :key-serializer 'serialize
                     :key-deserializer 'deserialize))
  (:method ((graph memory-graph-mixin))
    (make-mem-skip-list :key-equal 'equal
                        :key-comparison '%index-value-lessp
                        :value-equal 'equal
                        :head-key (list +min-sentinel+) :head-value nil
                        :tail-key (list +max-sentinel+) :tail-value nil
                        :duplicates-allowed-p nil)))

(defun %open-count-skip-list (graph address backend)
  "Reopen a persisted count map at ADDRESS (Task 4's restore)."
  (open-heap-index backend :address address :heap (indexes graph)
                   :comparison '%index-value-lessp :key-equal 'equal
                   :key-serializer 'serialize
                   :key-deserializer 'deserialize))

(defun %count-registry (graph)
  (or (count-indexes graph)
      (setf (count-indexes graph)
            (make-hash-table :test 'equal
                             #+sbcl :synchronized #+sbcl t
                             #+ccl :shared #+ccl t
                             #+graph-db-ecl-sync-hash :synchronized
                             #+graph-db-ecl-sync-hash t))))

(defun %count-index-for (graph spec)
  "Get-or-create the COUNT-INDEX for SPEC in GRAPH, keyed
(owner . slot-names) in the count registry (separate from the secondary
one, so both kinds may share an owner and slots -- facts X1)."
  (let* ((reg (%count-registry graph))
         (slot-names (count-index-spec-slot-names spec))
         (key (cons (count-index-spec-owner-name spec) slot-names)))
    (or (gethash key reg)
        (let ((cix (%make-count-index
                    :owner-name (count-index-spec-owner-name spec)
                    :slot-names slot-names
                    :canonicalizers (%resolve-index-canonicalizers
                                     (count-index-spec-canonicalize spec)
                                     (length slot-names))
                    :current-p (count-index-spec-current-p spec))))
          (setf (count-index-skip-list cix) (make-count-skip-list graph))
          (setf (gethash key reg) cix)))))

;;; --------------------------------------------------------------------
;;; Counter steps (spec §2.3)
;;; --------------------------------------------------------------------

(defun %count-tuple (cix node)
  "NODE's canonical components for CIX, +NULL-COMPONENT+ for a null;
NIL when every component is null or one is a geometry (nothing to
count) -- the write-side mirror of %INDEX-TUPLE-KEY."
  (let ((slots (count-index-slot-names cix)))
    (when (%tuple-indexable-p node slots)
      (let* ((cans (count-index-canonicalizers cix))
             (any nil)
             (key (loop for s in slots
                        for i from 0
                        for v = (slot-value node s)
                        collect (cond ((null v) +null-component+)
                                      (t (setf any t)
                                         (let ((c (nth i cans)))
                                           (if c (funcall c v) v)))))))
        (when any key)))))

(defun %current-p (cix node)
  "1 when CIX's CURRENT-P holds on NODE, else 0 (0 with no predicate)."
  (let ((p (count-index-current-p cix)))
    (if (and p (funcall p node)) 1 0)))

(defun %count-adjust (cix components d-all d-current)
  "Add D-ALL / D-CURRENT to the pair under COMPONENTS in CIX: create the
key, remove it when ALL reaches 0 (R4).  Read then branch --
UPDATE-IN-SKIP-LIST inserts on the on-disk backends and not on the
memory one -- and pass the old value so the update stays in place
(facts X2, B8).  No lock of its own (R9)."
  (let* ((sl (count-index-skip-list cix))
         (key (cons (length components) (copy-list components)))
         (node (find-in-skip-list sl key))
         (old (and node (%sn-value node)))
         (all (+ (if old (car old) 0) d-all))
         (current (and (count-index-current-p cix)
                       (+ (if old (or (cdr old) 0) 0) d-current))))
    (cond ((<= all 0)
           (when node (remove-from-skip-list sl key)))
          ((null node)
           (add-to-skip-list sl key (cons all current)))
          (t (update-in-skip-list sl key (cons all current) old)))))

(defun %count-node (cix node d-all d-current)
  "Apply D-ALL / D-CURRENT for NODE at every leading prefix of its
tuple; nothing for a tuple that is not countable."
  (let ((tuple (%count-tuple cix node)))
    (when tuple
      (loop for k from 1 to (length tuple)
            do (%count-adjust cix (subseq tuple 0 k) d-all d-current)))))

;;; --------------------------------------------------------------------
;;; Maintenance (spec §2.3): a pass beside the secondary one at every
;;; apply, serialised by the manager lock / the device writer (R9).
;;; --------------------------------------------------------------------

(defgeneric apply-tx-write-to-count-indexes (write graph)
  (:documentation "Adjust GRAPH's count indexes for one transaction
WRITE; nothing for a write kind with no method.")
  (:method (write graph) (declare (ignore write graph)) nil))

(defun %count-specs-for (node graph)
  "GRAPH's count specs applying to NODE's class, as a list."
  (%applicable-count-index-specs (class-of node) graph))

(defmethod apply-tx-write-to-count-indexes ((write tx-create) graph)
  (let ((node (node write)))
    (dolist (spec (%count-specs-for node graph))
      (let ((cix (%count-index-for graph spec)))
        (%count-node cix node 1 (%current-p cix node))))))

(defmethod apply-tx-write-to-count-indexes ((write tx-update) graph)
  ;; Tuple unchanged: CURRENT moves by the predicate's flip.  Tuple
  ;; changed: the old contribution leaves, the new one arrives.
  (let ((old (old-node write)) (new (node write)))
    (dolist (spec (%count-specs-for new graph))
      (let* ((cix (%count-index-for graph spec))
             (ot (%count-tuple cix old))
             (nt (%count-tuple cix new)))
        (if (equal ot nt)
            (let ((d (- (%current-p cix new) (%current-p cix old))))
              (unless (or (zerop d) (null nt))
                (loop for k from 1 to (length nt)
                      do (%count-adjust cix (subseq nt 0 k) 0 d))))
            (progn
              (%count-node cix old -1 (- (%current-p cix old)))
              (%count-node cix new 1 (%current-p cix new))))))))

;; TX-DELETE is a TX-UPDATE subclass: subtract the old node ONCE (the
;; secondary method releases twice; a counter cannot, facts C12).
(defmethod apply-tx-write-to-count-indexes ((write tx-delete) graph)
  (let ((old (old-node write)))
    (dolist (spec (%count-specs-for old graph))
      (let ((cix (%count-index-for graph spec)))
        (%count-node cix old -1 (- (%current-p cix old)))))))

(defun apply-tx-writes-to-count-indexes (writes graph)
  "The count pass of an apply (GH #361).  Under *ADD-TO-INDEXES-UNLESS-
PRESENT-P* -- crash-recovery replay and a device re-pull, which may
apply a write twice -- it counts nothing and marks the maps stale (R8).
Nothing at all for a graph with no count declarations."
  (when (gethash (graph-name graph) *schema-count-metadata*)
    (if *add-to-indexes-unless-present-p*
        (setf (count-indexes-stale-p graph) t)
        (dolist (write writes)
          (apply-tx-write-to-count-indexes write graph)))))

(defun %count-purge (node graph)
  "PEER-PURGE-NODE's count release: the node leaves without a write."
  (dolist (spec (%count-specs-for node graph))
    (let ((cix (%count-index-for graph spec)))
      (%count-node cix node -1 (- (%current-p cix node))))))

;;; --------------------------------------------------------------------
;;; Build, install, rebuild (spec §2.3-2.4)
;;; --------------------------------------------------------------------

(defun %build-count-index-for-spec (graph spec)
  "Build SPEC's map over the live nodes of its owner and return the
COUNT-INDEX: a typed scan (subclasses included, as MAP-VERTICES
defaults), deleted nodes skipped, one bad node tolerated the way
%BUILD-INDEX-FOR-SPEC does.  Trap: it ADDS to whatever the map already
holds, so a caller wanting authority clears the registry first."
  (let ((cix (%count-index-for graph spec))
        (owner (count-index-spec-owner-name spec)))
    (flet ((count-node (node)
             (unless (deleted-p node)
               (ignore-errors
                (%count-node cix node 1 (%current-p cix node))))))
      (if (subtypep owner 'edge)
          (map-edges #'count-node graph :edge-type owner)
          (map-vertices #'count-node graph :vertex-type owner)))
    cix))

(defun %ensure-count-index-built (graph spec)
  "SPEC's COUNT-INDEX in GRAPH, built by scan unless the registry
already holds it -- idempotent on the (owner . slot-names) key."
  (let ((key (cons (count-index-spec-owner-name spec)
                   (count-index-spec-slot-names spec))))
    (or (and (count-indexes graph)
             (gethash key (count-indexes graph)))
        (%build-count-index-for-spec graph spec))))

(defun install-count-indexes (graph)
  "Build any declared count index missing from GRAPH's registry."
  (dolist (spec (%registered-count-index-specs graph))
    (%ensure-count-index-built graph spec)))

(defun rebuild-count-indexes (graph)
  "Drop every count map and rebuild each declared one by scan, clearing
the stale flag (R8); returns GRAPH.  Authoritative and idempotent, and
the only repair for a re-applied write (facts X7)."
  (when (count-indexes graph)
    (maphash (lambda (k cix)
               (declare (ignore k))
               (let ((sl (count-index-skip-list cix)))
                 (when (and sl (view-index-p sl))
                   (delete-view-index sl))))
             (count-indexes graph))
    (clrhash (count-indexes graph)))
  (dolist (spec (%registered-count-index-specs graph))
    (%build-count-index-for-spec graph spec))
  (setf (count-indexes-stale-p graph) nil)
  graph)

;;; --------------------------------------------------------------------
;;; Queries (spec §2.5)
;;; --------------------------------------------------------------------

(defun %count-refresh (graph)
  "Rebuild the count maps when a re-apply left them stale (R8)."
  (when (count-indexes-stale-p graph)
    (rebuild-count-indexes graph)))

(defun %require-count-index (graph class-name slot-name)
  "The COUNT-INDEX on CLASS-NAME.SLOT-NAME -- CLASS-NAME's own or an
ancestor's -- or NIL when declared but not built (a lazy memory graph,
a declaration with no node yet); QUERY-PRECONDITION-ERROR when none is
declared."
  (let* ((slot-names (%normalize-slots slot-name))
         (reg (count-indexes graph))
         (class (find-class class-name nil)))
    (or (and reg
             (or (gethash (cons class-name slot-names) reg)
                 (and class (class-finalized-p class)
                      (loop for c in (cdr (class-precedence-list class))
                            for hit = (gethash (cons (class-name c)
                                                     slot-names)
                                               reg)
                            when hit return hit))))
        (if (some (lambda (s)
                    (and (subtypep class-name
                                   (count-index-spec-owner-name s))
                         (equal (count-index-spec-slot-names s)
                                slot-names)))
                  (%registered-count-index-specs graph))
            nil
            (error 'query-precondition-error
                   :reason (format nil "No count index on ~S.~S in ~S"
                                   class-name slot-name
                                   (graph-name graph)))))))

(defun %count-query-key (cix value)
  "VALUE -- a value or a component list of at most CIX's arity -- as a
canonical prefix, NIL mapped to +NULL-COMPONENT+; NIL for a full-arity
all-null tuple.  Shares %INDEX-KEY's arity rule exactly: at arity 1
VALUE is CIX's one component as-is, even list-valued; at arity > 1 it is
a list of up to ARITY components.  Signals on more than the arity."
  (let* ((arity (length (count-index-slot-names cix)))
         (cans (count-index-canonicalizers cix))
         (vals (if (= arity 1) (list value) value))
         (any nil)
         (key (loop for v in vals
                    for i from 0
                    collect (cond ((null v) +null-component+)
                                  (t (setf any t)
                                     (let ((c (nth i cans)))
                                       (if c (funcall c v) v)))))))
    (when (> (length vals) arity)
      (error 'query-precondition-error
             :reason (format nil "Count index on ~S has arity ~D; got ~D"
                             (count-index-slot-names cix) arity
                             (length vals))))
    (when (or any (< (length vals) arity)) key)))

(defun count-index-lookup (graph class-name slot-name tuple)
  "(VALUES ALL CURRENT) for TUPLE -- a full tuple or a leading prefix --
in the count index on CLASS-NAME.SLOT-NAME.  ALL is 0 for an absent
name or an all-null full tuple, and CURRENT is NIL whenever the index
has no CURRENT-P; a declared-but-unbuilt index answers 0 0, having no
map to ask.  Signals QUERY-PRECONDITION-ERROR when none is declared.
Trap: live at commit granularity; a re-apply is repaired by a rebuild
first (GH #361)."
  (let ((*graph* graph))
    (%count-refresh graph)
    (let ((cix (%require-count-index graph class-name slot-name)))
      (if (null cix)
          (values 0 0)
          ;; An absent name reads NIL for CURRENT on a predicate-free
          ;; index too, so a caller need not know whether the key
          ;; happens to exist to read the pair (GH #361).
          (let ((zero (and (count-index-current-p cix) 0))
                (key (%count-query-key cix tuple)))
            (if (null key)
                (values 0 zero)
                (let ((node (find-in-skip-list
                             (count-index-skip-list cix)
                             (cons (length key) key))))
                  (if (null node)
                      (values 0 zero)
                      (let ((v (%sn-value node)))
                        (values (car v) (cdr v)))))))))))

(defun map-count-index (fn graph class-name slot-name
                        &key (depth 1) prefix)
  "Call FN with (COMPONENTS ALL CURRENT) for each entry at DEPTH of the
count index on CLASS-NAME.SLOT-NAME whose leading components equal
PREFIX, in index order, a null component read back as NIL.  Zero calls
for a declared-but-empty index; signals when none is declared, or on a
DEPTH or PREFIX beyond the arity.  Trap: not an atomic snapshot, live at
commit granularity (GH #361)."
  (let ((*graph* graph))
    (%count-refresh graph)
    (let ((cix (%require-count-index graph class-name slot-name)))
      (when cix
        (let ((arity (length (count-index-slot-names cix)))
              (pre (and prefix (%count-query-key cix prefix))))
          (unless (<= 1 depth arity)
            (error 'query-precondition-error
                   :reason (format nil "Count index on ~S has arity ~D; ~
cannot list depth ~D" (count-index-slot-names cix) arity depth)))
          (when (> (length pre) depth)
            (error 'query-precondition-error
                   :reason (format nil "A :PREFIX of ~D component(s) at ~
depth ~D" (length pre) depth)))
          (unless (and prefix (null pre)) ; all-null full prefix: nothing
            (let* ((lo (cons depth pre))
                   (hi (append (cons depth pre)
                               (make-list (- depth (length pre))
                                          :initial-element +max-sentinel+)))
                   (cur (make-range-cursor (count-index-skip-list cix)
                                           lo hi)))
              (loop for node = (cursor-next cur :eoc)
                    until (eql node :eoc)
                    do (let ((k (%sn-key node)) (v (%sn-value node)))
                         (funcall fn (%ix-prefix-out (rest k) depth)
                                  (car v) (cdr v)))))))))))
