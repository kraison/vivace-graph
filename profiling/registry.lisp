;;;; Subsystem Registry for VivaceGraph Profiling Tool
;;;;
;;;; The registry maps a subsystem keyword to the set of function symbols that
;;;; SB-PROFILE should deterministically trace for that subsystem.
;;;;
;;;; Coverage rules (learned the hard way -- see docs/profiler-guide.md):
;;;;
;;;;   1. GENERIC FUNCTIONS ARE INCLUDED.  SB-PROFILE encapsulates a GF just
;;;;      fine and reports calls aggregated across its methods.  The engine's
;;;;      hottest entry points -- SERIALIZE, DESERIALIZE, DESERIALIZE-HELP,
;;;;      LOOKUP-VERTEX, GET-BYTES, NODE-GEOMETRY and every GEOS topology op --
;;;;      are ALL generic.  Excluding them (as this file used to) made the
;;;;      serialization workload structurally unable to see a single
;;;;      serialization function.
;;;;
;;;;   2. Classification is TOKEN-based, not substring-based.  Bare substring
;;;;      matching put MAPPED-FILE-LENGTH in :serialization (via "LENGTH") and
;;;;      %SORT-UNIQUE in :views (via "SORT").
;;;;
;;;;   3. The registry is REBUILDABLE at profile time, not just at load time.
;;;;      Optional systems (notably GRAPH-DB/GEOS) may be loaded after this
;;;;      file, and their functions must still be registered.
;;;;
;;;; Functions declaimed INLINE cannot be traced by SB-PROFILE at all -- their
;;;; call sites are open-coded, so no encapsulation is possible.  They are still
;;;; registered (harmless) but will simply never appear in a report.  Use
;;;; SB-SPROF to see them.
(in-package #:graph-db/profiler)

(defvar *subsystem-registry* (make-hash-table :test 'eq)
  "Hash table mapping subsystem keyword symbols to list of function symbols.")

(defparameter *profiled-packages* '("GRAPH-DB")
  "Package names scanned by POPULATE-ALL-GRAPH-DB-FUNCTIONS.  Only symbols whose
HOME package is one of these is registered, so re-exported CL symbols are not
swept in.")

(defparameter *subsystem-aliases*
  '((:graph-storage :graph-core)
    (:graph        :graph-core)
    (:index-backends :skip-list :bplus-tree)
    (:index          :skip-list :bplus-tree)
    (:indexes        :skip-list :bplus-tree)
    (:mmap           :mmap-storage)
    (:storage        :mmap-storage)
    (:txn            :transactions)
    (:transaction    :transactions))
  "Alist of ALIAS -> one or more canonical subsystem keys.

:GRAPH-STORAGE and :INDEX-BACKENDS in particular were requested by every
real-world workload but never registered by anything, so those workloads
silently traced ZERO functions for them.  Aliasing is deliberately permissive:
a profiler that quietly measures nothing is worse than one that over-measures.")

(defun profileable-symbol-p (sym)
  "True if SYM names something SB-PROFILE can encapsulate.
Macros and special operators cannot be profiled; generic functions can."
  (and (symbolp sym)
       (fboundp sym)
       (not (macro-function sym))
       (not (special-operator-p sym))))

(defun register-subsystem-functions (subsystem-key function-symbols)
  "Register a list of function symbols for a given SUBSYSTEM-KEY keyword.
Generic functions ARE eligible -- SB-PROFILE handles them."
  (let ((clean-list (remove-if-not #'profileable-symbol-p function-symbols)))
    (setf (gethash subsystem-key *subsystem-registry*)
          (union (gethash subsystem-key *subsystem-registry* '()) clean-list))
    (gethash subsystem-key *subsystem-registry*)))

(defun resolve-subsystem-key (subsystem-key)
  "Expand SUBSYSTEM-KEY through *SUBSYSTEM-ALIASES* into a list of canonical keys."
  (let ((entry (assoc subsystem-key *subsystem-aliases*)))
    (if entry (rest entry) (list subsystem-key))))

(defun get-subsystem-functions (subsystem-key)
  "Retrieve the list of valid, fbound function symbols for SUBSYSTEM-KEY.
Accepts :ALL, a canonical key, or an alias (see *SUBSYSTEM-ALIASES*)."
  (if (eq subsystem-key :all)
      (let ((all '()))
        (maphash (lambda (k funcs)
                   (declare (ignore k))
                   (setf all (union all funcs)))
                 *subsystem-registry*)
        all)
      (let ((acc '()))
        (dolist (k (resolve-subsystem-key subsystem-key) acc)
          (setf acc (union acc (gethash k *subsystem-registry* '())))))))

(defun list-subsystems ()
  "Return a list of all registered subsystem keywords."
  (alexandria:hash-table-keys *subsystem-registry*))

(defun all-subsystems ()
  "Return all registered subsystem keys."
  (list-subsystems))

;;; ---------------------------------------------------------------------------
;;; Token-aware classification
;;; ---------------------------------------------------------------------------

(defun %name-tokens (name)
  "Split NAME on #\\- into tokens, stripping a leading % from each."
  (let ((tokens '()) (start 0))
    (dotimes (i (1+ (length name)))
      (when (or (= i (length name)) (char= (char name i) #\-))
        (when (> i start)
          (let ((tok (subseq name start i)))
            (push (string-left-trim "%" tok) tokens)))
        (setf start (1+ i))))
    (nreverse tokens)))

(defun %any-token (tokens candidates)
  "True if any of CANDIDATES appears as a whole token in TOKENS."
  (some (lambda (c) (member c tokens :test #'string=)) candidates))

(defun %any-substring (name candidates)
  "True if any of CANDIDATES appears anywhere in NAME.  Use only for
distinctive stems (SERIAL, PROLOG, REPLICAT) that cannot collide."
  (some (lambda (c) (search c name)) candidates))

(defun classify-symbol-subsystem (sym)
  "Determine the subsystem keyword for SYM.

Order matters.  GEOS topology is tested BEFORE :SPATIAL, because every GEOS
entry point is named GEOMETRY-* and would otherwise be swallowed by the \"GEOM\"
test and reported as geohash/spatial-index work."
  (let* ((name (symbol-name sym))
         (tokens (%name-tokens name)))
    (cond
      ;; --- GEOS topology + FFI (must precede :spatial) ---
      ((or (%any-substring name '("GEOS"))
           (%any-token tokens '("UNION" "INTERSECTION" "DIFFERENCE" "INTERSECTS"
                                "CONTAINS" "WITHIN" "COVERS" "DISJOINT" "TOUCHES"
                                "CROSSES" "OVERLAPS" "BUFFER" "CENTROID" "SIMPLIFY"
                                "CONVEXHULL" "WKT" "WKB"))
           (%any-substring name '("MAKE-VALID" "AREA-M2" "-VALID-P")))
       :geos)
      ;; --- serialization codecs ---
      ((or (%any-substring name '("SERIAL" "DESERIAL"))
           (%any-token tokens '("ENCODE" "DECODE" "OCTET" "OCTETS" "BLOB"
                                "PACK" "UNPACK" "CODEC")))
       :serialization)
      ;; --- mmap / raw memory ---
      ((or (%any-substring name '("MMAP" "MPOINTER"))
           (%any-token tokens '("SAP" "ARENA" "ALLOC" "ALLOCATE" "FREE" "BYTE"
                                "BYTES" "BUF" "BUFFER" "POINTER" "MAPPED"
                                "MUNMAP" "MSYNC" "EXTEND" "UINT" "UINT32"
                                "UINT40" "UINT64" "WORD" "OFFSET")))
       :mmap-storage)
      ;; --- index backends ---
      ((or (%any-substring name '("SKIP-LIST" "SKIPLIST"))
           (%any-token tokens '("SL" "SKIP")))
       :skip-list)
      ((or (%any-substring name '("BPLUS" "B-PLUS"))
           (%any-token tokens '("BPT" "LEAF" "PAGE" "SPLIT" "TREE")))
       :bplus-tree)
      ;; --- spatial indexing / geometry representation (non-GEOS) ---
      ((or (%any-substring name '("SPATIAL" "GEOHASH" "GEOMETRY"))
           (%any-token tokens '("BBOX" "MBR" "CELL" "CELLS" "POINT" "POLYGON"
                                "MULTIPOLYGON" "LINESTRING" "GEOM" "COORD"
                                "COORDS" "COORDINATES" "LON" "LAT" "ENVELOPE")))
       :spatial)
      ;; --- prolog / logic engine ---
      ((or (%any-substring name '("PROLOG" "UNIFY" "FUNCTOR"))
           (%any-token tokens '("DEREF" "CLAUSE" "CLAUSES" "PREDICATE" "RULE"
                                "GOAL" "BINDING" "BINDINGS" "TRAIL")))
       :prolog)
      ;; --- views / secondary indexes ---
      ((or (%any-substring name '("VIEW"))
           (%any-token tokens '("YIELD" "MAP" "REDUCE" "EMIT" "ROLLUP")))
       :views)
      ;; --- replication (before :transactions -- TXN-LOG is replication) ---
      ((or (%any-substring name '("REPLICAT"))
           (%any-token tokens '("PEER" "PEERS" "SYNC" "STREAM" "ORIGIN"
                                "ORIGINS" "GOSSIP" "REPLICA")))
       :replication)
      ;; --- transactions / concurrency ---
      ((or (%any-substring name '("TRANSACTION"))
           (%any-token tokens '("TX" "TXN" "COMMIT" "ABORT" "ROLLBACK" "OCC"
                                "EPOCH" "REAPER" "LOCK" "LOCKS" "UNLOCK"
                                "SNAPSHOT" "VALIDATE" "CONFLICT" "MVCC"
                                "READSET" "WRITESET")))
       :transactions)
      (t :graph-core))))

;;; ---------------------------------------------------------------------------
;;; Population
;;; ---------------------------------------------------------------------------

(defun populate-all-graph-db-functions ()
  "Scan *PROFILED-PACKAGES* and register every profileable function symbol.
Includes generic functions; excludes macros and special operators."
  (dolist (pkg-name *profiled-packages*)
    (let ((pkg (find-package pkg-name)))
      (when pkg
        (do-symbols (s pkg)
          (when (and (eq (symbol-package s) pkg)
                     (profileable-symbol-p s))
            (register-subsystem-functions (classify-symbol-subsystem s) (list s))))))))

(defparameter *hot-path-function-names*
  '(;; read path
    "LOOKUP-VERTEX" "LOOKUP-EDGE" "MAP-VERTICES" "MAP-EDGES" "MAYBE-INIT-NODE-DATA"
    ;; codec
    "SERIALIZE" "DESERIALIZE" "DESERIALIZE-HELP" "DESERIALIZE-HELP-MMAP"
    "SERIALIZE-MULTIPLE" "EXTRACT-ALL-SUBSEQS"
    ;; raw memory
    "GET-BYTES" "GET-BYTE" "SET-BYTES" "MMAP-FILE" "EXTEND-MAPPED-FILE"
    ;; geometry / spatial
    "GEOMETRY-COORDINATE-PAIRS" "MAP-GEOMETRY-COORDINATES" "NODE-GEOMETRY"
    "MAKE-POINT" "MAKE-POLYGON" "SPATIAL-INDEX-INSERT" "SPATIAL-INDEX-QUERY-BBOX"
    "FIND-NODES-INTERSECTING" "POINT-IN-RING-P" "POINT-IN-POLYGON-RINGS-P"
    "GEOMETRY-CONTAINS-POINT-P"
    ;; slot-access bookkeeping -- genuinely hot: primitive-node.lisp consults
    ;; these on EVERY slot read and write, so they sit under every
    ;; materialization.  See vivace-graph #87.
    "PERSISTENT-SLOT-NAMES" "EPHEMERAL-SLOT-NAMES" "META-SLOT-NAMES"
    "PERSISTENT-P" "EPHEMERAL-P" "META-P"
    ;; transactions
    "WITH-TRANSACTION" "COMMIT-TRANSACTION" "VALIDATE-TRANSACTION")
  "A small, curated set of the engine's genuinely hot entry points.

Rationale: SB-PROFILE encapsulates every function it traces, so profiling all
~1900 symbols adds per-call overhead everywhere and distorts exactly the
measurement you are trying to take.  The :HOT-PATH subsystem exists for
low-distortion runs; :ALL remains available when completeness matters more
than fidelity.")

(defun populate-hot-path-subsystem ()
  "Register the curated :HOT-PATH subsystem from *HOT-PATH-FUNCTION-NAMES*."
  (dolist (pkg-name *profiled-packages*)
    (let ((pkg (find-package pkg-name)))
      (when pkg
        (dolist (n *hot-path-function-names*)
          (let ((s (find-symbol n pkg)))
            (when (and s (profileable-symbol-p s))
              (register-subsystem-functions :hot-path (list s)))))))))

;; Initialize default subsystem function bindings across all vivace-graph layers
(graph-db:def-vertex prof-node ()
  ((name :type string)
   (find-key :type string)
   (centroid :type geometry :index t)
   (geom :type geometry :index t)
   (value)
   (label))
  :prof-graph)

(graph-db:def-edge prof-link ()
  ((label))
  :prof-graph)

(graph-db:def-view prof-view :lessp (prof-node :prof-graph)
  (:map (lambda (node)
          (let ((lbl (slot-value node 'label)))
            (when lbl (graph-db::yield lbl 1))))))

(defun init-default-subsystem-registry ()
  "Populate default subsystem function bindings across all vivace-graph layers."
  (clrhash *subsystem-registry*)
  (populate-all-graph-db-functions)
  (populate-hot-path-subsystem)
  *subsystem-registry*)

(defun refresh-subsystem-registry ()
  "Rebuild the registry from the CURRENT image.

Call this after loading an optional system (e.g. GRAPH-DB/GEOS) so its
functions become visible to the profiler.  PROFILE-BLOCK calls it automatically
unless *AUTO-REFRESH-REGISTRY* is NIL, because the registry used to be a
load-time snapshot: anything loaded later was silently untraceable."
  (init-default-subsystem-registry))

(defvar *auto-refresh-registry* t
  "When true, PROFILE-BLOCK refreshes the registry before each run so that
late-loaded systems are covered.  Set to NIL to pin a hand-built registry.")

(defun subsystem-coverage-report (&optional (stream *standard-output*))
  "Print how many functions each subsystem will trace, plus what is unreachable.
Use this to sanity-check coverage BEFORE trusting a profiling run."
  (format stream "~&=== Subsystem coverage ===~%")
  (let ((rows '()) (generic 0) (plain 0))
    (maphash (lambda (k v) (push (cons k (length v)) rows)) *subsystem-registry*)
    (dolist (r (sort rows #'> :key #'cdr))
      (format stream "  ~18A ~5D~%" (car r) (cdr r)))
    (dolist (pkg-name *profiled-packages*)
      (let ((pkg (find-package pkg-name)))
        (when pkg
          (do-symbols (s pkg)
            (when (and (eq (symbol-package s) pkg) (profileable-symbol-p s))
              (if (typep (fdefinition s) 'generic-function) (incf generic) (incf plain)))))))
    (format stream "  ---~%  plain functions: ~D   generic functions: ~D (both traced)~%"
            plain generic)
    (format stream "  packages scanned: ~{~A~^, ~}~%" *profiled-packages*)
    (unless (find-package "GRAPH-DB")
      (format stream "  WARNING: GRAPH-DB package not found.~%"))
    (let ((geos (get-subsystem-functions :geos)))
      (when (< (length geos) 5)
        (format stream "  NOTE: only ~D GEOS functions registered -- is GRAPH-DB/GEOS loaded?~%"
                (length geos))))))

;; Initialize on load
(init-default-subsystem-registry)
