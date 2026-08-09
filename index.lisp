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

(defun %resolve-index-canonicalizers (spec arity)
  "SPEC -> a list of ARITY canonicalizers (NIL = identity).  A POSITIONAL list
applies element I to component I, and must supply exactly ARITY entries --
signalled rather than silently truncated/padded, since a wrong count is
exactly the \"no error, wrong results\" class of bug this index chases
elsewhere (GH #107).  Anything else (T, NIL, a bare symbol, #'FN, or a
LAMBDA form) is a single spec applying to component 0 only, with every
other component defaulting to identity -- the single-slot / single-function
backward-compatible form every existing caller uses.

#'FN reads as (FUNCTION FN) and a LAMBDA form as (LAMBDA ...) -- both are
conses, so \"is it a cons?\" cannot distinguish a positional list from a
single function designator; excluding those two heads is what does."
  (if (and (consp spec) (not (member (car spec) '(function lambda))))
      (progn
        (unless (= (length spec) arity)
          (error ":CANONICALIZE list ~S has ~D entries, need exactly ~D ~
                   (one per indexed slot)" spec (length spec) arity))
        (loop for i from 0 below arity
              collect (%resolve-index-canonicalizer (nth i spec))))
      (cons (%resolve-index-canonicalizer spec)
            (make-list (max 0 (1- arity)) :initial-element nil))))

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

(defun %normalize-slots (slot-or-list)
  "A slot designator as a list.  A bare symbol becomes a 1-list, so single-slot
and multi-slot share one code path (GH #107)."
  (if (listp slot-or-list) slot-or-list (list slot-or-list)))

(defstruct (index-spec (:constructor make-index-spec))
  owner-name slot-names graph-name canonicalize)

(defun register-index-spec (spec)
  "Phase 1: record SPEC.  Duplicates accumulate; resolved newest-wins."
  (push spec (gethash (index-spec-graph-name spec) *schema-index-metadata*))
  spec)

(defun %registered-index-specs (graph)
  "DEF-INDEX specs registered for GRAPH, de-duped by (owner . slot-names),
newest-wins."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (spec (gethash (graph-name graph) *schema-index-metadata*))
      (let ((k (cons (index-spec-owner-name spec)
                     (index-spec-slot-names spec))))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push spec result))))
    (nreverse result)))

(defun %slot-present-p (class slot-name)
  (and (find slot-name (class-slots class) :key #'slot-definition-name) t))

(defun %applicable-index-descriptors (class graph)
  "(slot-names owner spec) descriptors from the DEF-INDEX registry applying to
CLASS: owner is CLASS or an ancestor (subtype IS-A) and every slot in
SLOT-NAMES exists in CLASS.  SLOT-NAMES is the full list from the spec --
truncating it to its first element here (as Task 3 did) is the multi-slot
trap: a 3-slot DEF-INDEX would key on its first slot only (GH #107)."
  (when (class-finalized-p class)
    (loop for spec in (%registered-index-specs graph)
          for owner = (index-spec-owner-name spec)
          for slot-names = (index-spec-slot-names spec)
          when (and (subtypep (class-name class) owner)
                    (every (lambda (s) (%slot-present-p class s)) slot-names))
          collect (list slot-names owner (index-spec-canonicalize spec)))))

(defun class-secondary-index-descriptors (class graph)
  "All (slot-names owner spec) descriptors for CLASS: its MOP :INDEX effective
slots plus the DEF-INDEX definitions applicable to it, de-duped by
(owner . slot-names) (MOP first).  SLOT-NAMES is always a list here --
%NORMALIZE-SLOTS wraps a MOP slot's bare name -- so every downstream
consumer (write path, query path) sees one uniform shape regardless of
origin (GH #107).  This is the single input to maintenance (apply / rebuild)."
  (let ((seen (make-hash-table :test 'equal)) (result '()))
    (dolist (d (append (class-indexed-slots class)
                       (%applicable-index-descriptors class graph)))
      (let* ((slot-names (%normalize-slots (first d)))
             (k (cons (second d) slot-names)))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push (list slot-names (second d) (third d)) result))))
    (nreverse result)))

;;; ---------------------------------------------------------------------------
;;; The index + the graph's registry
;;; ---------------------------------------------------------------------------

(defstruct (slot-index (:constructor %make-slot-index))
  owner-name slot-names canonicalizers
  ;; The backing ordered map: a heap skip-list / B+ tree on an on-disk graph, a
  ;; MEM-SKIP-LIST on a memory-graph -- always ordered (needed for range).  Keyed
  ;; by the flat composite (v1 ... vn id) under %INDEX-COMP-LESSP, arity-aware
  ;; since Task 4 (GH #107).
  skip-list)

(defun %index-head-key (arity)
  "Lower sentinel key for an index of ARITY value components (GH #107)."
  (append (make-list arity :initial-element +min-sentinel+) (list +null-key+)))

(defun %index-tail-key (arity)
  "Upper sentinel key for an index of ARITY value components (GH #107)."
  (append (make-list arity :initial-element +max-sentinel+) (list +max-key+)))

;; MAKE-SECONDARY-SKIP-LIST does not go through MAKE-VIEW-SKIP-LIST: that
;; generic is shared with real views, which must stay on REDUCE-COMP-LESSP /
;; REDUCE-EQUAL (GH #107) -- moving it would silently change every view's
;; ordering too.  Dispatch here is its own generic, specializing the same way
;; (GRAPH default, MEMORY-GRAPH-MIXIN override) so an index skip-list gets
;; %INDEX-COMP-LESSP / %INDEX-EQUAL on both backends without touching the view
;; path.  Both methods live here (not in memory-graph.lisp, which loads before
;; this file) since they need %INDEX-COMP-LESSP et al., defined below.
(defgeneric make-secondary-skip-list (graph arity)
  (:documentation "The ordered map backing a secondary index of ARITY value
components -- a flat (v1 ... vn id) composite under %INDEX-COMP-LESSP /
%INDEX-EQUAL.  Follows *INDEX-BACKEND* on an on-disk graph and returns a
MEM-SKIP-LIST on a memory-graph.  At ARITY 1 this orders identically to the
pre-Task-4 REDUCE-COMP-LESSP map, so an existing single-slot index reopens
with no rebuild (GH #107).")
  (:method ((graph graph) arity)
    (make-heap-index (graph-index-backend graph) (indexes graph)
                     '%index-comp-lessp
                     :head-key (%index-head-key arity)
                     :tail-key (%index-tail-key arity)
                     :key-equal '%index-equal
                     :key-serializer '%index-key-serialize
                     :key-deserializer '%index-key-deserialize))
  (:method ((graph memory-graph-mixin) arity)
    (make-mem-skip-list
     :key-equal '%index-equal
     :key-comparison '%index-comp-lessp
     :value-equal 'equal
     :head-key (%index-head-key arity)
     :head-value nil
     :tail-key (%index-tail-key arity)
     :tail-value nil
     :duplicates-allowed-p nil)))

(defun %open-secondary-skip-list (graph address arity
                                  &optional (backend :skip-list))
  "Reopen an on-disk secondary index of ARITY value components at ADDRESS with
BACKEND's opener, on %INDEX-COMP-LESSP.  ARITY is accepted for signature
symmetry with MAKE-SECONDARY-SKIP-LIST; the persisted head/tail nodes already
carry their serialized keys, so reopening does not need to rebuild sentinels
from it (GH #107)."
  (declare (ignore arity))
  (open-heap-index backend :address address :heap (indexes graph)
                   :comparison '%index-comp-lessp
                   :key-equal '%index-equal
                   :key-serializer '%index-key-serialize
                   :key-deserializer '%index-key-deserialize))

;;; Ordering for a flat index key (v1 ... vn id): every component but the last
;;; compares with LESS-THAN, the trailing node id with KEY-VECTOR<.  At n=2 this
;;; is exactly REDUCE-COMP-LESSP, which is what lets an existing single-slot
;;; index reopen under it without a rebuild (GH #107).  Keys of unequal length
;;; are tolerated -- the shorter sorts first -- so a prefix bound works.

(defun %index-comp-lessp (key1 key2)
  "True when KEY1 sorts before KEY2.  See the comment above for the contract."
  (let ((n1 (length key1))
        (n2 (length key2)))
    (loop for a in key1
          for b in key2
          for i from 0
          do (if (and (= i (1- n1)) (= i (1- n2)))
                 (return (key-vector< a b))
                 (cond ((less-than a b) (return t))
                       ((equal a b))
                       (t (return nil))))
          finally (return (< n1 n2)))))

(defun %index-equal (key1 key2)
  "Key equality matching %INDEX-COMP-LESSP: components by EQUAL, trailing id by
EQUALP.  At n=2 this is exactly REDUCE-EQUAL."
  (let ((n1 (length key1))
        (n2 (length key2)))
    (and (= n1 n2)
         (loop for a in key1
               for b in key2
               for i from 0
               always (if (= i (1- n1)) (equalp a b) (equal a b))))))

(defun %index-value-lessp (a b)
  "Lexicographic order for two VALUE-only component lists (no trailing id), by
LESS-THAN per component; a strict prefix sorts before its extension.  Used by
IX-MAP's open-ended range filter, where there is no id to anchor the
%INDEX-COMP-LESSP last-position special case (GH #107)."
  (loop for x in a
        for y in b
        do (cond ((less-than x y) (return t))
                 ((equal x y))
                 (t (return nil)))
        finally (return (< (length a) (length b)))))

;;; ---------------------------------------------------------------------------
;;; Index key codec (GH #107)
;;; ---------------------------------------------------------------------------
;;; VIEW-KEY-SERIALIZE / VIEW-KEY-DESERIALIZE (views.lisp) hard-code a
;;; 2-element (value id) key -- SECOND is spliced in raw as the id, so a 3+
;;; element flat index key (v1 ... vn id) signals a TYPE-ERROR.  Views and
;;; :UNIQUE stay on that pair, byte-for-byte, untouched; this is the index's
;;; own pair.  At arity 1 it delegates outright (byte-identical output, no
;;; rebuild for an existing on-disk single-slot index).  At arity >= 2 the
;;; value tuple (v1 ... vn) is SERIALIZEd as one list behind +INDEX-TUPLE+, a
;;; marker the deserializer checks first -- without it, a serialized list is
;;; ambiguous between "the tuple's values" and "the arity-1 value happens to
;;; be a list" (the case VIEW-KEY-DESERIALIZE already handles via its general,
;;; non-+STRING+ branch).
;;;
;;; VIEW-KEY-SERIALIZE's CONCATENATE builds an unspecialized (VECTOR T), which
;;; DESERIALIZE / VIEW-KEY-DESERIALIZE's own (ARRAY (UNSIGNED-BYTE 8))
;;; parameter declarations reject outside of safety-0 code -- invisible in
;;; production because a serialized key is always written to and re-read from
;;; the mmap heap before it is next deserialized, never round-tripped in
;;; memory.  A codec test round-trips in memory with no heap in between, so
;;; %INDEX-KEY-SERIALIZE always returns a genuinely (UNSIGNED-BYTE 8)-typed
;;; array (COERCE / CONCATENATE with that result type), which satisfies the
;;; declaration either way and costs nothing on the disk path.

(defun %index-key-serialize (key)
  "Serialize flat index KEY, (v1 ... vn id), to a byte vector.  N=1 delegates
to VIEW-KEY-SERIALIZE outright (byte-identical, including its +STRING+ fast
path).  N>=2 is ID ++ +INDEX-TUPLE+ ++ SERIALIZE of (v1 ... vn) as one list."
  (let ((vals (butlast key)))
    (if (= (length vals) 1)
        (coerce (view-key-serialize key) '(simple-array (unsigned-byte 8) (*)))
        (let ((id (car (last key))))
          (concatenate '(simple-array (unsigned-byte 8) (*))
                       id (list +index-tuple+) (serialize vals))))))

(defun %index-key-deserialize (array)
  "Inverse of %INDEX-KEY-SERIALIZE.  Returns (VALUES key length).  +INDEX-TUPLE+
right after the 16-byte id means an N>=2 value tuple follows; anything else
(including the +STRING+ fast path) is VIEW-KEY-DESERIALIZE's arity-1 shape,
so a list-valued single component still comes back as one component, not a
tuple."
  (declare (type (array (unsigned-byte 8)) array))
  (if (and (> (length array) 16) (= (aref array 16) +index-tuple+))
      (let ((id (make-array 16 :element-type '(unsigned-byte 8))))
        (dotimes (i 16) (setf (aref id i) (aref array i)))
        (multiple-value-bind (vals length) (deserialize (subseq array 17))
          (values (append vals (list id)) (+ 17 length))))
      (view-key-deserialize array)))

(defun %index-key (six value)
  "The canonical component list for query VALUE against SIX: per-position
canonicalizer applied, +NULL-COMPONENT+ substituted for a null component.  At
arity 1, VALUE is SIX's one component as-is (even list-valued -- e.g. a
list-valued single slot); at arity > 1, VALUE is a list of up to SIX's arity
components, left-to-right.  NIL when every given component is null --
nothing to look up (the query-side mirror of %INDEX-TUPLE-KEY, GH #107)."
  (let* ((arity (length (slot-index-slot-names six)))
         (cans (slot-index-canonicalizers six))
         (vals (if (= arity 1) (list value) value))
         (any nil)
         (key (loop for v in vals
                    for i from 0
                    collect (cond ((null v) +null-component+)
                                  (t (setf any t)
                                     (let ((c (nth i cans)))
                                       (if c (funcall c v) v)))))))
    (when any key)))

(defun %index-tuple-key (six node)
  "The value components of NODE's key for SIX: per-position canonicalizer
applied, +NULL-COMPONENT+ substituted for a null component so the row stays
findable by a prefix scan of its populated parts.  NIL only when EVERY
component is null -- nothing to index (the write-side mirror of
%INDEX-KEY, GH #107)."
  (let* ((slots (slot-index-slot-names six))
         (cans  (slot-index-canonicalizers six))
         (any nil)
         (key (loop for s in slots
                    for i from 0
                    for v = (slot-value node s)
                    collect (cond ((null v) +null-component+)
                                  (t (setf any t)
                                     (let ((c (nth i cans)))
                                       (if c (funcall c v) v)))))))
    (when any key)))

(defun %tuple-indexable-p (node slot-names)
  "True unless some component of NODE's SLOT-NAMES tuple is a real geometry
value -- geometry is the spatial index's domain, not this one's.  Unlike
%INDEXABLE-VALUE-P, a null component does NOT fail this gate: a tuple with a
null component is still indexed, via +NULL-COMPONENT+ in %INDEX-TUPLE-KEY
(GH #107)."
  (notany (lambda (s) (geometryp (slot-value node s))) slot-names))

;;; Backend-agnostic ops over the ordered map (VALUE = the canonical key).

(defun %index-bounds (six value prefix)
  "Low/high range-cursor bounds (as VALUES) for VALUE -- a canonical
component list from %INDEX-KEY, or a bare scalar at arity 1 -- against SIX.

Full arity: [VALUE+NULL-KEY, VALUE+MAX-KEY], an exact-tuple window (the
pre-Task-7 hardcoded pair).  Fewer components with PREFIX T:
[VALUE, VALUE padded with +MAX-SENTINEL+ per missing slot, +MAX-KEY+],
matching every stored tuple with VALUE as a leading prefix -- the low bound
needs no padding, since a shorter key already sorts below any longer key
sharing it (%INDEX-COMP-LESSP).  Fewer components with PREFIX NIL, or MORE
than the arity regardless of PREFIX, signals: a wrong-length value is
otherwise indistinguishable from an intended prefix and would silently
return a superset -- silent-wrong-answer is this project's dominant defect
class (GH #107)."
  (let* ((vals (if (listp value) value (list value)))
         (arity (length (slot-index-slot-names six)))
         (n (length vals)))
    (cond ((= n arity)
           (values (append vals (list +null-key+))
                   (append vals (list +max-key+))))
          ((and prefix (< n arity))
           (values vals
                   (append vals
                           (make-list (- arity n)
                                      :initial-element +max-sentinel+)
                           (list +max-key+))))
          (t (error "Index on ~S has arity ~D; got ~D value(s)~
~:[~; -- pass :PREFIX T for a prefix scan~]"
                    (slot-index-slot-names six) arity n (< n arity))))))

(defun ix-lookup (six key &key prefix)
  "List of node-ids whose indexed tuple matches KEY, the canonical component
list from %INDEX-KEY: an exact match when KEY supplies SIX's full arity, or --
with PREFIX T -- a range scan when KEY supplies fewer, matching every stored
tuple that starts with KEY.  NIL if none.  Bounds via %INDEX-BOUNDS (GH #107)."
  (multiple-value-bind (lo hi) (%index-bounds six key prefix)
    (let ((cur (make-range-cursor (slot-index-skip-list six) lo hi))
          (ids '()))
      (loop for node = (cursor-next cur :eoc) until (eql node :eoc)
            do (push (car (last (%sn-key node))) ids))
      (nreverse ids))))

(defun ix-put (six key id)
  "Enter node ID under canonical tuple KEY (SIX's full-arity component list).
The composite skip-list key is (v1 ... vn id) -- id last, read back by
IX-LOOKUP / IX-MAP via (CAR (LAST ...)); the skip-node VALUE is unused --
store NIL, not the raw id byte array (which SERIALIZE cannot round-trip)."
  (add-to-skip-list (slot-index-skip-list six) (append key (list id)) nil))

(defun ix-remove (six key id)
  "Remove node ID's entry under canonical tuple KEY (that composite key only)."
  (remove-from-skip-list (slot-index-skip-list six) (append key (list id))))

(defun ix-map (six fn &key start end)
  "Call FN with (KEY ID) for every entry with START <= key <= END (component-
wise, by %INDEX-VALUE-LESSP), in ascending order; KEY is the component list,
id excluded.  Open-ended when START/END is NIL.  START/END are tuples --
full arity, or fewer components (a range endpoint may always be a prefix, no
:PREFIX flag needed, unlike an equality IX-LOOKUP).  Bounded ranges use a
range cursor (the efficient path), its bounds via %INDEX-BOUNDS on each
endpoint separately -- taking START's low bound and END's high bound -- so a
short tuple pads the same way a prefix IX-LOOKUP does, rather than an
id-position sentinel landing in a value-position slot (GH #107).  An open end
falls back to an ordered full scan + bound filter, which already tolerates a
short tuple via %INDEX-VALUE-LESSP's own length tie-break."
  (let ((sl (slot-index-skip-list six)))
    (if (and start end)
        (let ((cur (make-range-cursor
                    sl
                    (nth-value 0 (%index-bounds six start t))
                    (nth-value 1 (%index-bounds six end t)))))
          (loop for node = (cursor-next cur :eoc) until (eql node :eoc)
                do (funcall fn (butlast (%sn-key node))
                            (car (last (%sn-key node))))))
        ;; Open-ended: ordered full scan, filtering by whatever bound is present.
        (let ((cur (make-cursor sl)))
          (loop for node = (cursor-next cur :eoc) until (eql node :eoc)
                for k = (butlast (%sn-key node))
                when (and (or (null start) (not (%index-value-lessp k start)))
                          (or (null end)   (not (%index-value-lessp end k))))
                do (funcall fn k (car (last (%sn-key node)))))))))

(defun ix-count (six)
  "Number of live entries in SIX."
  (let ((cur (make-cursor (slot-index-skip-list six))) (n 0))
    (loop for node = (cursor-next cur :eoc) until (eql node :eoc) do (incf n))
    n))

(defun %slot-index-for (graph descriptor)
  "Get-or-create the (empty) SLOT-INDEX for DESCRIPTOR = (slot owner spec),
keyed by (owner . slot-names) in GRAPH's registry.  Resolves SPEC to one
canonicalizer per slot (positional list, or a single spec applying to
component 0 -- see %RESOLVE-INDEX-CANONICALIZERS) on creation and builds
the ordered map on the graph's backend."
  (destructuring-bind (slot-name owner-name spec) descriptor
    (let* ((reg (or (secondary-indexes graph)
                    (setf (secondary-indexes graph)
                          (make-hash-table :test 'equal
                                           #+sbcl :synchronized #+sbcl t
                                           #+ccl :shared #+ccl t
                                           #+graph-db-ecl-sync-hash :synchronized
                                           #+graph-db-ecl-sync-hash t))))
           (slot-names (%normalize-slots slot-name))
           (key (cons owner-name slot-names))
           (canonicalizers (%resolve-index-canonicalizers
                             spec (length slot-names))))
      (or (gethash key reg)
          (let ((six (%make-slot-index
                      :owner-name owner-name :slot-names slot-names
                      :canonicalizers canonicalizers)))
            (setf (slot-index-skip-list six)
                  (make-secondary-skip-list graph (length slot-names)))
            (setf (gethash key reg) six))))))

;;; ---------------------------------------------------------------------------
;;; Maintenance (APPLY, post-durability, journal-replayable -- no enforcement)
;;; ---------------------------------------------------------------------------

(defun %ix-claim (node graph)
  "Index NODE's indexed slot values (create / new value of an update)."
  (dolist (d (class-secondary-index-descriptors (class-of node) graph))
    (let ((slot-names (first d)))
      ;; Gate BEFORE %SLOT-INDEX-FOR so a geometry component never creates an
      ;; ordered index (it is the spatial index's; %TUPLE-INDEXABLE-P skips
      ;; it).  A null component does NOT gate here -- %INDEX-TUPLE-KEY still
      ;; indexes the row, under +NULL-COMPONENT+ (GH #107).
      (when (%tuple-indexable-p node slot-names)
        (let* ((six (%slot-index-for graph d))
               (key (%index-tuple-key six node)))
          (when key (ix-put six key (id node))))))))

(defun %ix-release (node graph)
  "Remove NODE's indexed slot values (delete / old value of an update)."
  (dolist (d (class-secondary-index-descriptors (class-of node) graph))
    (let ((slot-names (first d)))
      (when (%tuple-indexable-p node slot-names)
        (let* ((six (%slot-index-for graph d))
               (key (%index-tuple-key six node)))
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
                   (let ((slot-names (first d)))
                     (when (%tuple-indexable-p node slot-names)
                       (let* ((six (%slot-index-for graph d))
                              (key (%index-tuple-key six node)))
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
  "Persist the on-disk secondary indexes' roots (owner slot-names address
backend-tag).  No-op with no heap (memory) or no indexes.  Called at
CLOSE-GRAPH.  The canonicalizer is NOT stored (a function is not
serializable); it is re-resolved from the owner class's live :INDEX spec on
reopen -- only the address+backend are needed to reopen the ordered map."
  (when (and (indexes graph) (secondary-indexes graph))
    (let ((roots '()))
      (maphash (lambda (k six)
                 (declare (ignore k))
                 (when (and (slot-index-skip-list six)
                            (view-index-p (slot-index-skip-list six)))
                   (push (list (slot-index-owner-name six)
                               (slot-index-slot-names six)
                               (view-index-address (slot-index-skip-list six))
                               (view-index-backend-tag (slot-index-skip-list six)))
                         roots)))
               (secondary-indexes graph))
      (%atomic-cl-store roots (secondary-index-root-file (location graph))))))

(defun %owner-slot-canonicalizer (owner-name slot-names graph)
  "Resolve the ARITY (LENGTH SLOT-NAMES) canonicalizers for (OWNER-NAME .
SLOT-NAMES) from the owner class's live :INDEX spec, or from a matching
DEF-INDEX spec registered for GRAPH; a list of ARITY NILs if neither is
found.  Used on reopen to re-associate a persisted index with its live spec
(a canonicalizer is a function, not serializable, so it is re-resolved, not
stored).

The MOP :INDEX branch only fires when SLOT-NAMES is genuinely single-slot
(arity 1) -- a slot's :INDEX option is inherently single-slot, so at arity >
1, matching just (FIRST SLOT-NAMES) with no arity check would find an
UNRELATED single-slot :INDEX declaration that merely shares its first slot's
name, and return THAT slot's canonicalizer instead of the composite's.  The
SIX was originally built (at write time, not reopen) with the composite's own
canonicalizer(s) via %RESOLVE-INDEX-CANONICALIZERS, so that mismatch would
silently reopen the index keying with different canonicalizers than the ones
that wrote the entries on disk -- missed matches, no error (GH #107).  The
MOP branch is wrapped in LIST (not left as a bare function-or-NIL) so a found
D short-circuits the OR even when its canonicalizer is identity -- otherwise
an identity match and a genuine non-match would be indistinguishable to OR."
  (let ((arity (length slot-names)))
    (or (when (= arity 1)
          (let ((c (ignore-errors (find-class owner-name nil))))
            (when (and c (class-finalized-p c))
              (let ((d (find (first slot-names) (class-indexed-slots c)
                             :key #'first)))
                (when d (list (%resolve-index-canonicalizer (third d))))))))
        (let* ((matchp (lambda (s)
                         (and (eq (index-spec-owner-name s) owner-name)
                              (equal (index-spec-slot-names s) slot-names))))
               (spec (find-if matchp (%registered-index-specs graph))))
          (when spec
            (%resolve-index-canonicalizers
             (index-spec-canonicalize spec) arity)))
        (make-list arity :initial-element nil))))

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
              (destructuring-bind (owner slot-names address
                                    &optional (backend :skip-list)) r
                (setf (gethash (cons owner slot-names) reg)
                      (%make-slot-index
                       :owner-name owner :slot-names slot-names
                       :canonicalizers (%owner-slot-canonicalizer
                                        owner slot-names graph)
                       :skip-list (%open-secondary-skip-list
                                   graph address (length slot-names)
                                   backend))))))
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
OWNER's nodes (and subclasses).  SLOT-NAMES is passed through whole --
unwrapping it to (FIRST ...) here was the multi-slot trap: %SLOT-INDEX-FOR
would re-wrap a bare symbol into a 1-list and silently key a 3-slot spec on
its first slot only (GH #107).  The geometry gate and key build are wrapped
in one IGNORE-ERRORS, matching this function's pre-existing tolerance for a
legacy node whose slot(s) predate the DEF-INDEX (the scan path only; the
live commit-apply path in %IX-CLAIM does not need this)."
  (let* ((owner (index-spec-owner-name spec))
         (slot-names (index-spec-slot-names spec))
         (six (%slot-index-for
               graph (list slot-names owner (index-spec-canonicalize spec))))
         (*graph* graph))
    (flet ((index-node (node)
             (unless (deleted-p node)
               (let ((key (ignore-errors
                           (and (%tuple-indexable-p node slot-names)
                                (%index-tuple-key six node)))))
                 (when key (ix-put six key (id node)))))))
      (if (subtypep owner 'edge)
          (map-edges #'index-node graph :edge-type owner)
          (map-vertices #'index-node graph :vertex-type owner)))
    six))

(defun %ensure-index-built (graph spec)
  "Build SPEC's index unless it already exists in GRAPH's registry (idempotent)."
  (let ((key (cons (index-spec-owner-name spec) (index-spec-slot-names spec))))
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
  `(let ((spec (make-index-spec :owner-name ',owner-class
                                :slot-names (%normalize-slots ',slot)
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
An index rooted at an ancestor of CLASS-NAME covers it (subtype IS-A).  NB: a
declared index's SIX may not exist yet (e.g. every tuple seen so far was
all-null, or the class simply has no live nodes), so NIL here does NOT mean
the slot is unindexed -- see %SLOT-INDEX-DECLARED-P.  SLOT-NAME may be a bare
symbol or an already-normalized list -- both resolve (GH #107)."
  (let ((reg (secondary-indexes graph))
        (slot-names (%normalize-slots slot-name)))
    (when reg
      (or (gethash (cons class-name slot-names) reg)
          (let ((class (ignore-errors (find-class class-name nil))))
            (when class
              (loop for c in (class-precedence-list class)
                    for k = (cons (class-name c) slot-names)
                    for hit = (and (typep c 'node-class) (gethash k reg))
                    when hit return hit)))))))

(defun %slot-index-declared-p (class-name slot-name)
  "True if CLASS-NAME declares SLOT-NAME as an :INDEX slot (effective, so inherited
:INDEX from an ancestor counts).  Distinguishes a declared-but-empty index (no
struct created yet) from a genuinely unindexed slot.  SLOT-NAME may be a bare
symbol or an already-normalized list.  Only matches at arity 1 -- a slot's
:INDEX option is inherently single-slot, so an undeclared multi-slot
combination that merely shares its first element with an unrelated :INDEX
slot must not be misreported as declared here (GH #107)."
  (let ((class (ignore-errors (find-class class-name nil)))
        (slot-names (%normalize-slots slot-name)))
    (and class (class-finalized-p class)
         (= (length slot-names) 1)
         (find (first slot-names) (class-indexed-slots class) :key #'first)
         t)))

(defun %def-index-declared-p (graph class-name slot-name)
  "True if a DEF-INDEX registered for GRAPH covers CLASS-NAME.SLOT-NAME (owner is
CLASS-NAME or an ancestor).  SLOT-NAME may be a bare symbol or an already-
normalized list (GH #107)."
  (let ((slot-names (%normalize-slots slot-name)))
    (and (ignore-errors (find-class class-name nil))
         (some (lambda (spec)
                 (and (equal (index-spec-slot-names spec) slot-names)
                      (subtypep class-name (index-spec-owner-name spec))))
               (%registered-index-specs graph)))))

(defun %require-index (graph class-name slot-name)
  "The SLOT-INDEX for CLASS-NAME.SLOT-NAME.  Returns NIL when the slot is a declared
index (via :INDEX or DEF-INDEX) but no entries exist yet (a legitimately empty
result); signals only when the slot is not indexed at all (a programming error)."
  (let* ((slot-names (%normalize-slots slot-name))
         (six (%secondary-index-lookup graph class-name slot-names)))
    (cond (six six)
          ((or (%slot-index-declared-p class-name slot-names)
               (%def-index-declared-p graph class-name slot-names))
           nil)                                             ; declared, empty
          (t (error "No secondary index on ~S.~S in ~S"
                    class-name slot-name (graph-name graph))))))

(defun index-lookup (graph class-name slot-name value
                     &key (collect-p t) prefix)
  "Nodes of CLASS-NAME (and subclasses) whose indexed slot(s) equal VALUE, via
the secondary index.  VALUE is a scalar for a single-slot index; for a
multi-slot index it is a list of component values, one per SLOT-NAME
position, left-to-right -- the full arity for an exact match, or fewer
components with PREFIX T for a scan of every tuple that starts with them (a
tuple with a null component is still findable this way -- see
%INDEX-TUPLE-KEY).  Signals if no index covers CLASS-NAME.SLOT-NAME.  Resolves
ids in GRAPH.  With COLLECT-P NIL, returns T as soon as one match is found
(GH #107)."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                            ; NIL => declared but empty => no matches
      (let ((key (%index-key six value))
            (result '()))
        (when key
          (dolist (id (ix-lookup six key :prefix prefix))
            (let ((node (%node-by-id id graph)))
              (when (and node (not (deleted-p node)))
                (if collect-p (push node result) (return-from index-lookup t))))))
        (when collect-p (nreverse result))))))

(defun map-index (fn graph class-name slot-name &key start end)
  "Call FN on each live node of CLASS-NAME (and subclasses) whose SLOT-NAME is in
[START,END] (inclusive; open-ended when NIL), in ascending value order.  START/
END are scalars for a single-slot index; for a multi-slot index each is a
tuple (list of component values) -- full arity, or fewer components, which
bound only on the components given (GH #107).  Resolves ids in GRAPH."
  (let* ((*graph* graph)
         (six (%require-index graph class-name slot-name)))
    (when six                            ; NIL => declared but empty => nothing to map
      (let ((skey (and start (%index-key six start)))
            (ekey (and end   (%index-key six end))))
        (ix-map six
                (lambda (key id)
                  (declare (ignore key))
                  (let ((node (%node-by-id id graph)))
                    (when (and node (not (deleted-p node)))
                      (funcall fn node))))
                :start skey :end ekey)))))

(defun index-range (graph class-name slot-name &key start end)
  "Nodes of CLASS-NAME (and subclasses) whose SLOT-NAME is in [START,END], ascending.
START/END are tuples for a multi-slot index -- see MAP-INDEX.  Resolves ids in
GRAPH."
  (let ((result '()))
    (map-index (lambda (node) (push node result)) graph class-name slot-name
               :start start :end end)
    (nreverse result)))
