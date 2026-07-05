;;;; MVCC tests (the v2 node head + later the versioned write path / reaper).
;;;;
;;;; Phase 1: the head format gained commit-epoch (8) + prev-pointer (8),
;;;; appended after data-pointer (15 -> 31 bytes).  These tests serialize a
;;;; node head to a byte buffer and read it back, asserting the new fields
;;;; round-trip for both vertices and edges.

(in-package #:graph-db/test)

(def-suite mvcc-suite
  :description "MVCC: v2 node-head codec (commit-epoch + prev-pointer), versioning."
  :in graph-db-suite)

(in-suite mvcc-suite)

;; A dedicated graph + type for the per-type :keep-revisions test, isolated from
;; the shared integration schema.
(eval-when (:load-toplevel :execute)
  (setf (gethash :mvcc-keep-test *schema-node-metadata*) nil))
(def-vertex mvcc-kept-node () ((n)) :mvcc-keep-test :keep-revisions 2)

(test node-head-v2-round-trip-vertex
  "A vertex head round-trips revision, data-pointer, commit-epoch and prev-pointer
through serialize-node-head / deserialize-node-head (31-byte v2 head)."
  (with-test-graph (g)
    (declare (ignore g))
    (let (v)
      (with-transaction () (setq v (make-g-person :name "H" :age 3)))
      (setf (graph-db::revision v)     7
            (graph-db::data-pointer v) 4242
            (graph-db::commit-epoch v) 123456789
            (graph-db::prev-pointer v) 987654321)
      (let ((buf (graph-db::make-byte-vector graph-db::+node-header-size+)))
        (is (= 31 graph-db::+node-header-size+))
        (is (= 30 (graph-db::serialize-node-head buf v 0))
            "serialize returns the final offset (31-byte head ends at 30)")
        (multiple-value-bind (del wr hw tiw vw vew vvw type-id rev ptr epoch prev offset)
            (graph-db::deserialize-node-head buf 0)
          (declare (ignore del wr hw tiw vw vew vvw))
          (is (= (graph-db::type-id v) type-id))
          (is (= 7 rev))
          (is (= 4242 ptr))
          (is (= 123456789 epoch) "commit-epoch round-trips")
          (is (= 987654321 prev) "prev-pointer round-trips")
          (is (= 30 offset)))))))

(test node-head-v2-round-trip-edge
  "An edge head round-trips its from/to/weight AND the new commit-epoch /
prev-pointer (the edge codec positions from/to/weight after the node head, so it
auto-shifts with the larger v2 head)."
  (with-test-graph (g)
    (declare (ignore g))
    (let (e aid bid)
      (with-transaction ()
        (let ((a (make-g-person :name "A")) (b (make-g-person :name "B")))
          (setq aid (id a) bid (id b))
          (setq e (make-g-knows :from a :to b :weight 2.5))))
      (setf (graph-db::revision e)     4
            (graph-db::commit-epoch e) 555
            (graph-db::prev-pointer e) 666)
      (let ((buf (graph-db::make-byte-vector graph-db::+edge-header-size+)))
        (is (= 71 graph-db::+edge-header-size+))
        (graph-db::serialize-edge-head buf e 0)
        (let ((e2 (graph-db::deserialize-edge-head buf 0)))
          (is (= 4 (graph-db::revision e2)))
          (is (= 555 (graph-db::commit-epoch e2)) "edge commit-epoch round-trips")
          (is (= 666 (graph-db::prev-pointer e2)) "edge prev-pointer round-trips")
          (is (equalp aid (from e2)))
          (is (equalp bid (to e2)))
          (is (= 2.5 (weight e2))))))))

;;; ---------------------------------------------------------------------------
;;; v1 -> v2 migration (MIGRATE-GRAPH)
;;;
;;; tests/fixtures/v1-graph.tar.gz is a pristine pre-MVCC (storage-version 1,
;;; 15-byte head) graph built on the experiment branch with this same test
;;; schema (g-person/g-employee/g-knows/g-likes): Alice(30) -knows-> Bob(25),
;;; Bob -knows-> Carol(40, employee "Boss"), Alice -likes-> Carol.  The heap is a
;;; 1 GB sparse file, so it ships tar+gzipped (~13 KB of zero-fill) and the test
;;; extracts it to a scratch dir.
;;; ---------------------------------------------------------------------------

(defun extract-v1-fixture (dest)
  "Extract the committed v1 graph fixture into DEST (created if needed); return
DEST as a string."
  (let ((tarball (asdf:system-relative-pathname
                  :graph-db/test "tests/fixtures/v1-graph.tar.gz")))
    (ensure-directories-exist dest)
    (uiop:run-program (list "tar" "xzf" (namestring tarball)
                            "-C" (namestring dest))
                      :output t :error-output t)
    (namestring dest)))

(test migrate-v1-graph-to-v2
  "A pre-MVCC (v1, 15-byte head) graph cannot be opened directly by v2 code but
MIGRATE-GRAPH carries it across (logical snapshot + replay), preserving every
node, its slot data, the subclass, and the edge topology."
  ;; The committed v1 fixture's struct.dat/schema.dat were cl-store'd by SBCL.
  ;; cl-store's struct encoding is not portable across implementations, so ECL
  ;; cannot restore an SBCL-written graph (a pre-existing property of the on-disk
  ;; format, not of MIGRATE-GRAPH).  Skip there; SBCL and CCL restore it fine.
  #+ecl
  (skip "v1 fixture was cl-store'd by SBCL; ECL's cl-store cannot restore it ~
(graph on-disk dirs are not portable across Lisp implementations).")
  #-ecl
  (with-temp-directory (root)
    (let ((old-dir (extract-v1-fixture (merge-pathnames "v1/" root)))
          (new-dir (namestring (merge-pathnames "v2/" root))))
      ;; v2 code refuses to open the v1 graph directly (the format gate).
      (signals error (graph-db:open-graph :mvcc-mig-guard old-dir
                                          :buffer-pool-p nil :gc-heap-p nil))
      ;; ...but MIGRATE-GRAPH brings it forward to v2.
      (let ((g (graph-db::migrate-graph :graph-db-mvcc-migration old-dir new-dir
                                        :package :graph-db/test)))
        (unwind-protect
             (let ((*graph* g))
               (is (= 2 graph-db::+storage-version+)
                   "the migrated graph is written in the current (v2) format")
               ;; All three people survive, with name + age intact.  :vertex-type
               ;; g-person includes the g-employee subclass (Carol) by default.
               (let ((people (sort (map-vertices
                                    (lambda (v) (list (slot-value v 'name)
                                                      (slot-value v 'age)))
                                    g :collect-p t :vertex-type 'g-person)
                                   #'string< :key #'car)))
                 (is (equal '(("Alice" 30) ("Bob" 25) ("Carol" 40)) people)))
               ;; Carol's subclass + extra slot round-tripped.
               (let ((titles (map-vertices (lambda (v) (slot-value v 'title))
                                           g :collect-p t :vertex-type 'g-employee)))
                 (is (equal '("Boss") titles)))
               ;; Edge topology: two g-knows, one g-likes.
               (is (= 2 (length (map-edges #'identity g
                                           :collect-p t :edge-type 'g-knows))))
               (is (= 1 (length (map-edges #'identity g
                                           :collect-p t :edge-type 'g-likes)))))
          (graph-db:close-graph g))))))

;;; ---------------------------------------------------------------------------
;;; Versioned write path + reaper (P2)
;;; ---------------------------------------------------------------------------

(defun version-chain-length (node graph)
  "Number of archived versions chained behind NODE's live head (via prev-pointer)."
  (let ((p (graph-db::prev-pointer node))
        (n 0))
    (loop
      (when (zerop p) (return n))
      (incf n)
      (multiple-value-bind (data-ptr epoch prev)
          (graph-db::read-archived-head graph p)
        (declare (ignore data-ptr epoch))
        (setf p prev)))))

(defun bump-age (id new-age)
  "Update vertex ID's age inside a transaction (a versioning write)."
  (with-transaction ()
    (let ((c (copy (lookup-vertex id))))
      (setf (slot-value c 'age) new-age)
      (save c))))

(test versioned-update-retains-then-reaps-prior-version
  "An update archives the prior version (prev-pointer chain grows), and the lazy
epoch-gated reaper keeps the chain bounded (keep=0 => a single retained version
in steady state) while the live head always reads the newest data."
  (with-test-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-g-person :name "v" :age 0))))
      (is (= 0 (version-chain-length (lookup-vertex id) g))
          "a freshly created node has no prior versions")
      (bump-age id 1)
      (is (= 1 (version-chain-length (lookup-vertex id) g))
          "the first update retains the prior version (cannot be reaped yet)")
      (bump-age id 2)
      (bump-age id 3)
      (is (= 1 (version-chain-length (lookup-vertex id) g))
          "steady state retains exactly one prior version (older ones reaped)")
      (is (= 3 (slot-value (lookup-vertex id) 'age))
          "the live head always reflects the newest committed value"))))

(test read-pin-retains-versions-until-released
  "A held read-epoch pin lower-bounds the reaper's safe floor, so every version
that was live at/after the pin is retained while it is held; once released, the
reaper collapses the chain back to the steady-state size."
  (with-test-graph (g)
    (let ((tm (graph-db::transaction-manager g))
          id token)
      (with-transaction () (setq id (id (make-g-person :name "p" :age 0))))
      ;; Pin at the current epoch BEFORE any update.
      (setq token (graph-db::pin-read-epoch tm))
      (unwind-protect
           (progn
             (bump-age id 1)
             (bump-age id 2)
             (bump-age id 3)
             (is (>= (version-chain-length (lookup-vertex id) g) 2)
                 "a held read pin keeps prior versions from being reaped"))
        (graph-db::unpin-read-epoch tm token))
      ;; With the pin released the reaper can collapse the chain again.
      (bump-age id 4)
      (bump-age id 5)
      (is (= 1 (version-chain-length (lookup-vertex id) g))
          "after the pin is released the chain returns to steady-state size")
      (is (= 5 (slot-value (lookup-vertex id) 'age))))))

(test versioned-reopen-preserves-live-and-chain
  "CLOSE-GRAPH + OPEN-GRAPH (which runs the version-aware GC-HEAP) preserves the
live data of a repeatedly-updated node and does not corrupt its retained version
chain."
  (with-temp-directory (dir)
    (let (id)
      (let ((g (make-graph *integration-graph-name* (namestring dir)
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction () (setq id (id (make-g-person :name "r" :age 0))))
          (dotimes (i 4) (bump-age id (1+ i))))
        (close-graph g))
      ;; Reopen: default :gc-heap-p T sweeps the heap; the version-aware roots
      ;; must keep the live head's data (and its retained version) alive.
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let ((*graph* g))
               (is (= 4 (slot-value (lookup-vertex id) 'age))
                   "live data survives reopen + gc-heap")
               ;; the chain is still walkable (no dangling prev-pointer)
               (is (<= 0 (version-chain-length (lookup-vertex id) g))))
          (close-graph g)
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Retention config (P3): graph-level + per-type :keep-revisions
;;; ---------------------------------------------------------------------------

(test keep-revisions-graph-default-retains-window
  "A graph created with :KEEP-REVISIONS N keeps N prior versions of every node in
steady state (even when older versions are otherwise epoch-reclaimable)."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000 :keep-revisions 3))
          id)
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (setq id (id (make-g-person :name "k" :age 0))))
             (dotimes (i 8) (bump-age id (1+ i)))   ; no concurrent readers
             (is (= 3 (version-chain-length (lookup-vertex id) g))
                 "graph keep-revisions=3 retains exactly three prior versions")
             (is (= 8 (slot-value (lookup-vertex id) 'age))))
        (close-graph g :snapshot-p nil)
        (collect-garbage)))))

(test keep-revisions-per-type-overrides-default
  "A node-type defined with :KEEP-REVISIONS N retains N prior versions regardless
of the graph default."
  (with-temp-directory (dir)
    (let ((g (make-graph :mvcc-keep-test (namestring dir) :buffer-pool-size 1000))
          id)                              ; graph default keep-revisions = 0
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (setq id (id (make-mvcc-kept-node :n 0))))
             (dotimes (i 8)
               (with-transaction ()
                 (let ((c (copy (lookup-vertex id))))
                   (setf (slot-value c 'n) (1+ i))
                   (save c))))
             (is (= 2 (version-chain-length (lookup-vertex id) g))
                 "type keep-revisions=2 overrides the graph default (0)")
             (is (= 8 (slot-value (lookup-vertex id) 'n))))
        (close-graph g :snapshot-p nil)
        (collect-garbage)))))

;;; ---------------------------------------------------------------------------
;;; P4 PROTOTYPE: snapshot-isolation reads
;;; ---------------------------------------------------------------------------

(test snapshot-read-sees-version-at-transaction-start
  "A transaction reads the version that was live as of its start epoch: an
update committed by a later transaction is invisible to it (repeatable read),
and the older version is retained because the open transaction holds the floor."
  (with-test-graph (g)
    (let ((tm (graph-db::transaction-manager g))
          id)
      (with-transaction () (setq id (id (make-g-person :name "S" :age 0))))
      ;; Open transaction A NOW (captures its start epoch) -- but do not commit.
      (let ((txn-a (graph-db::create-transaction tm)))
        (unwind-protect
             (progn
               ;; A concurrent, fully-committed update bumps age 0 -> 1.
               (with-transaction ()
                 (let ((c (copy (lookup-vertex id))))
                   (setf (slot-value c 'age) 1)
                   (save c)))
               ;; Reads through A's snapshot still see age 0 (the version that was
               ;; live when A started); the live head is age 1.
               (let ((graph-db:*transaction* txn-a))
                 (is (= 0 (slot-value (lookup-vertex id) 'age))
                     "snapshot read sees the pre-update version"))
               ;; With snapshot reads OFF, the same read sees the live version (1),
               ;; confirming the resolver is what produces the isolation.
               (let ((graph-db:*transaction* (graph-db::create-transaction tm))
                     (graph-db::*snapshot-reads-p* nil))
                 (is (= 1 (slot-value (lookup-vertex id) 'age))
                     "without snapshot reads, a read sees the live version")))
          (ignore-errors (graph-db::remove-transaction txn-a tm)))))))

;; NOTE (prototype limitation): snapshot isolation here applies to LOOKUP only.
;; Scans (map-vertices / map-edges) read the LIVE version of each node, so a
;; transaction that scans can observe newer data than its lookups.  Full
;; snapshot-consistent scans would resolve each node's version during the scan
;; (extra cost) and are out of scope for this prototype.

(test snapshot-consistent-across-multiple-lookups
  "An open transaction sees a consistent point-in-time snapshot across several
lookups: a later transaction that updates multiple nodes is invisible to it for
ALL of them (not a torn mix of old and new)."
  (with-test-graph (g)
    (let ((tm (graph-db::transaction-manager g))
          xid yid)
      (with-transaction ()
        (setq xid (id (make-g-person :name "X" :age 10))
              yid (id (make-g-person :name "Y" :age 20))))
      (let ((txn-a (graph-db::create-transaction tm)))
        (unwind-protect
             (progn
               ;; One later transaction updates BOTH X and Y.
               (with-transaction ()
                 (let ((cx (copy (lookup-vertex xid)))
                       (cy (copy (lookup-vertex yid))))
                   (setf (slot-value cx 'age) 11) (save cx)
                   (setf (slot-value cy 'age) 21) (save cy)))
               (let ((graph-db:*transaction* txn-a))
                 (is (= 10 (slot-value (lookup-vertex xid) 'age))
                     "snapshot: X still at its pre-update value")
                 (is (= 20 (slot-value (lookup-vertex yid) 'age))
                     "snapshot: Y still at its pre-update value (consistent)")))
          (ignore-errors (graph-db::remove-transaction txn-a tm)))))))

(test snapshot-hides-nodes-created-after-start
  "A node created by a transaction that commits after our snapshot started is
invisible to a lookup in our transaction (no phantom)."
  (with-test-graph (g)
    (let ((tm (graph-db::transaction-manager g))
          zid)
      (with-transaction () (make-g-person :name "seed"))  ; advance the epoch past 0
      (let ((txn-a (graph-db::create-transaction tm)))
        (unwind-protect
             (progn
               (with-transaction () (setq zid (id (make-g-person :name "Z"))))
               (let ((graph-db:*transaction* txn-a))
                 (is (null (lookup-vertex zid))
                     "node committed after snapshot start is invisible to lookup")))
          (ignore-errors (graph-db::remove-transaction txn-a tm)))))))

(test is-a-unbound-type-honors-snapshot
  "is-a/2 with BOTH arguments unbound -- (is-a ?n ?type) -- enumerates vertices
via per-type scans (each through lookup-vertex), so it is snapshot-consistent: a
vertex committed after our snapshot started is invisible.  Previously this branch
did a single untyped lhash scan that read live versions and leaked the phantom."
  (with-test-graph (g)
    (let ((tm (graph-db::transaction-manager g)))
      (with-transaction () (make-g-person :name "S" :age 0))
      (let ((txn-a (graph-db::create-transaction tm)))
        (unwind-protect
             (progn
               ;; A concurrent committed insert adds a second person AFTER A began.
               (with-transaction () (make-g-person :name "T" :age 1))
               ;; Through A's snapshot, the unbound is-a sees only the pre-snapshot
               ;; vertex -- not the phantom committed after A started.
               (let ((graph-db:*transaction* txn-a))
                 (is (= 1 (length (select (:flat nil) (?n ?type) (is-a ?n ?type))))
                     "snapshot is-a (unbound type) sees only pre-snapshot vertices"))
               ;; A fresh transaction (snapshot starts after both commits) sees both,
               ;; confirming the enumeration itself is complete.
               (let ((graph-db:*transaction* (graph-db::create-transaction tm)))
                 (is (= 2 (length (select (:flat nil) (?n ?type) (is-a ?n ?type))))
                     "a later snapshot sees the full live set")))
          (ignore-errors (graph-db::remove-transaction txn-a tm)))))))

;;; ---------------------------------------------------------------------------
;;; Snapshot query mode (#45 Phase 1): SELECT :snapshot t runs a query under a
;;; single consistent MVCC read snapshot.
;;; ---------------------------------------------------------------------------

(test snapshot-option-establishes-a-read-transaction
  "SELECT :snapshot t runs the query inside a read transaction (so reads resolve
at one epoch); a plain query has no transaction bound."
  (with-test-graph (g)
    (declare (ignore g))
    ;; the lisp escape reports whether a transaction is active during the query
    (is (equal '(:yes)
               (select (:flat t :snapshot t) (?b)
                       (lisp ?b (if graph-db:*transaction* :yes :no)))))
    (is (equal '(:no)
               (select (:flat t) (?b)
                       (lisp ?b (if graph-db:*transaction* :yes :no)))))))

(test snapshot-query-is-stable-across-a-concurrent-commit
  "A query bound to a snapshot keeps seeing that snapshot's data even though
another transaction commits a new vertex partway through -- the count taken
before and after the interleaved insert is identical."
  (with-test-graph (g)
    (let ((tm (graph-db::transaction-manager g)))
      (with-transaction () (make-g-person :name "seed"))   ; advance epoch
      (let ((txn (graph-db::create-transaction tm)))
        (unwind-protect
             (let ((graph-db:*transaction* txn))            ; pin the query snapshot
               (let ((before (select-count (?p) (is-a ?p g-person))))
                 (is (= 1 before))
                 ;; a concurrent transaction commits a new person AFTER our snapshot
                 (with-transaction () (make-g-person :name "later"))
                 (is (= before (select-count (?p) (is-a ?p g-person)))
                     "snapshot query does not see the post-snapshot insert")))
          (ignore-errors (graph-db::remove-transaction txn tm)))))
    ;; outside the snapshot, the new vertex is of course visible
    (is (= 2 (select-count (?p) (is-a ?p g-person))))))
