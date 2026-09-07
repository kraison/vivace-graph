;;;; MVCC tests (the v2 node head + later the versioned write path / reaper).
;;;;
;;;; Phase 1: the head format gained commit-epoch (8) + prev-pointer (8),
;;;; appended after data-pointer (15 -> 31 bytes).  These tests serialize a
;;;; node head to a byte buffer and read it back, asserting the new fields
;;;; round-trip for both vertices and edges.

(in-package #:graph-db/test)

(def-suite mvcc-suite
  :description "MVCC: v3 head codec (commit-epoch, prev-pointer), versioning."
  :in graph-db-suite)

(in-suite mvcc-suite)

;; A dedicated graph + type for the per-type :keep-revisions test, isolated from
;; the shared integration schema.
(eval-when (:load-toplevel :execute)
  (setf (gethash :mvcc-keep-test *schema-node-metadata*) nil))
(def-vertex mvcc-kept-node () ((n)) :mvcc-keep-test :keep-revisions 2)

(test node-head-v3-round-trip-vertex
  "A vertex head round-trips revision, data-pointer, commit-epoch and
prev-pointer through serialize-node-head / deserialize-node-head (33-byte
head; type-id widened to 4 bytes, GH #166)."
  (with-test-graph (g)
    (declare (ignore g))
    (let (v)
      (with-transaction () (setq v (make-g-person :name "H" :age 3)))
      (setf (graph-db::revision v)     7
            (graph-db::data-pointer v) 4242
            (graph-db::commit-epoch v) 123456789
            (graph-db::prev-pointer v) 987654321)
      (let ((buf (graph-db::make-byte-vector graph-db::+node-header-size+)))
        (is (= 33 graph-db::+node-header-size+))
        (is (= 32 (graph-db::serialize-node-head buf v 0))
            "serialize returns the final offset (33-byte head ends at 32)")
        (multiple-value-bind (del wr hw tiw vw vew vvw type-id rev ptr epoch prev offset)
            (graph-db::deserialize-node-head buf 0)
          (declare (ignore del wr hw tiw vw vew vvw))
          (is (= (graph-db::type-id v) type-id))
          (is (= 7 rev))
          (is (= 4242 ptr))
          (is (= 123456789 epoch) "commit-epoch round-trips")
          (is (= 987654321 prev) "prev-pointer round-trips")
          (is (= 32 offset)))))))

(test node-head-v3-round-trip-edge
  "An edge head round-trips its from/to/weight AND the new commit-epoch /
prev-pointer (the edge codec positions from/to/weight after the node head, so it
auto-shifts with the larger v3 head)."
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
        (is (= 73 graph-db::+edge-header-size+))
        (graph-db::serialize-edge-head buf e 0)
        (let ((e2 (graph-db::deserialize-edge-head buf 0)))
          (is (= 4 (graph-db::revision e2)))
          (is (= 555 (graph-db::commit-epoch e2)) "edge commit-epoch round-trips")
          (is (= 666 (graph-db::prev-pointer e2)) "edge prev-pointer round-trips")
          (is (equalp aid (from e2)))
          (is (equalp bid (to e2)))
          (is (= 2.5 (weight e2))))))))

;;; ---------------------------------------------------------------------------
;;; v1 -> v3 migration (MIGRATE-GRAPH)
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

(test migrate-v1-graph-to-v3-without-renumbering
  "A pre-MVCC (v1, 15-byte head) graph cannot be opened directly by v3 code but
MIGRATE-GRAPH carries it across (logical snapshot + replay), preserving every
node, its slot data, the subclass, and the edge topology.

Pins the DEFAULT mode, :RENUMBER-P NIL, which is passed explicitly below: the
type-id guarantee became mode-dependent at #186 (spec §10.1), and :RENUMBER-P T
is the exact reverse of what this test asserts."
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
          (new-dir (namestring (merge-pathnames "v3/" root))))
      ;; v3 code refuses to open the v1 graph directly (the format gate).
      (signals error (graph-db:open-graph :mvcc-mig-guard old-dir
                                          :buffer-pool-p nil :gc-heap-p nil))
      ;; ...but MIGRATE-GRAPH brings it forward to v3.  The default
      ;; snapshot-file is per-run since GH #98, so this no longer has to dodge
      ;; a shared path -- but keep it inside our temp tree anyway, so a killed
      ;; run leaves nothing behind in the system temp directory.
      (let ((g (graph-db::migrate-graph :graph-db-mvcc-migration old-dir new-dir
                                        :package :graph-db/test
                                        :renumber-p nil
                                        :snapshot-file
                                        (namestring
                                         (merge-pathnames "migrate.snapshot" root)))))
        (unwind-protect
             (let ((*graph* g))
               (is (= 3 graph-db::+storage-version+)
                   "the migrated graph is written in the current (v3) format")
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

(test migration-snapshot-path-is-per-run
  "GH #98: the default snapshot path must not be keyed on the graph name alone.
A name-only path is constant across runs, users and processes on a host, so one
aborted migration made every later migration of that name die with a bare
FILE-EXISTS naming a path the caller never chose."
  (let ((a (graph-db::%migration-snapshot-file :g-dup))
        (b (graph-db::%migration-snapshot-file :g-dup)))
    (is (not (equal a b))
        "two runs must not collide; got ~A twice" a)
    ;; Guard the cosmetic half too: the old formatter produced `/tmp//migrate-...'.
    (is (not (search "//" (subseq a (min 1 (length a)))))
        "path should not contain a doubled separator: ~A" a)))

(test failed-migration-does-not-leak-its-snapshot
  "GH #98: cleanup used to sit on the success path only, so any abort between the
snapshot and the replay leaked the file permanently.  It is now on every exit."
  #+ecl
  (skip "v1 fixture was cl-store'd by SBCL; ECL's cl-store cannot restore it.")
  #-ecl
  (with-temp-directory (root)
    (let* ((old-dir (extract-v1-fixture (merge-pathnames "v1/" root)))
           (new-dir (namestring (merge-pathnames "v2/" root)))
           (snap (namestring (merge-pathnames "leak.snapshot" root)))
           (orig (fdefinition 'graph-db::recreate-graph)))
      ;; Abort in step 2, AFTER the snapshot exists -- the window that leaked.
      (setf (fdefinition 'graph-db::recreate-graph)
            (lambda (&rest args)
              (declare (ignore args))
              (error "forced failure for the leak test")))
      (unwind-protect
           (signals error
             (graph-db::migrate-graph :graph-db-mvcc-migration old-dir new-dir
                                      :package :graph-db/test
                                      :snapshot-file snap))
        (setf (fdefinition 'graph-db::recreate-graph) orig))
      (is (not (probe-file snap))
          "the snapshot must be removed even when the migration fails"))))

(defparameter *migration-origin*
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 9)
  "A fixed device origin id for the sidecar-migration tests (GH #289).")

(test migration-carries-the-peer-replication-sidecars
  "GH #289: MIGRATE-GRAPH is snapshot + replay, which carries the nodes and
nothing else.  A peer-replicating store's Lamport clock, applied-op index,
field stamps, node origins and conflict records live BESIDE the graph, and
OPEN-GRAPH recreated each one empty after a migration -- a reset clock
loses later LWW races, an empty applied-op index re-applies ops, both
silently and only at the next sync.  Node UUIDs survive the replay, so the
sidecars' keys stay valid and they are carried verbatim."
  (with-temp-directory (root)
    (let ((old-dir (namestring (merge-pathnames "peer-old/" root)))
          (new-dir (namestring (merge-pathnames "peer-new/" root)))
          (op-id (gen-id)))
      (let ((g (make-graph *integration-graph-name* old-dir
                           :peer-role :device :origin-id *migration-origin*
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (dotimes (i 3) (with-transaction () (make-g-person :name "m")))
          (graph-db::peer-observe-lamport g 40)
          (graph-db::record-applied-op g op-id 40))
        (close-graph g :snapshot-p nil))
      (let ((g (graph-db::migrate-graph
                *integration-graph-name* old-dir new-dir
                :package :graph-db/test
                :snapshot-file (namestring
                                (merge-pathnames "peer.snapshot" root)))))
        (close-graph g :snapshot-p nil))
      (dolist (f '("lamport.dat" "applied-ops/struct.dat"
                   "applied-ops/table.dat"))
        (is (probe-file (merge-pathnames f new-dir)) "~A was carried" f))
      (let ((g (open-graph *integration-graph-name* new-dir
                           :peer-role :device :origin-id *migration-origin*
                           :peer-host "localhost" :replication-port 0)))
        (unwind-protect
             (let ((*graph* g))
               (is (= 40 (graph-db::lamport-counter g))
                   "the Lamport clock survives the migration")
               (is-true (graph-db::op-applied-p g op-id)
                        "the applied-op index survives the migration")
               (is (= 3 (length (map-vertices #'identity g :collect-p t
                                              :vertex-type 'g-person)))))
          (close-graph g :snapshot-p nil))))))

(test migrating-a-non-peer-store-carries-no-sidecars
  "The other side of GH #289: a store with no peer sidecars migrates as
before -- nothing is invented for it."
  (with-temp-directory (root)
    (let ((old-dir (namestring (merge-pathnames "plain-old/" root)))
          (new-dir (namestring (merge-pathnames "plain-new/" root))))
      (let ((g (make-graph *integration-graph-name* old-dir
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction () (make-g-person :name "p")))
        (close-graph g :snapshot-p nil))
      (let ((g (graph-db::migrate-graph
                *integration-graph-name* old-dir new-dir
                :package :graph-db/test
                :snapshot-file (namestring
                                (merge-pathnames "plain.snapshot" root)))))
        (close-graph g :snapshot-p nil))
      (is (not (probe-file (merge-pathnames "lamport.dat" new-dir))))
      (is (not (probe-file (merge-pathnames "applied-ops/" new-dir)))))))

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

(defun bump-since (eid new-since)
  "Update edge EID's SINCE inside a transaction (a versioning write)."
  (with-transaction ()
    (let ((c (copy (lookup-edge eid))))
      (setf (slot-value c 'since) new-since)
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
at one epoch); a plain query has no transaction bound.  The snapshot lives in
*READ-SNAPSHOTS* keyed by graph, not in *TRANSACTION* (GH #53), so the escape
asks READ-TRANSACTION -- which answers for either."
  (with-test-graph (g)
    (declare (ignore g))
    ;; the lisp escape reports whether a transaction is active during the query
    (is (equal '(:yes)
               (select (:flat t :snapshot t) (?b)
                       (lisp ?b (if (graph-db::read-transaction *graph*) :yes :no)))))
    (is (equal '(:no)
               (select (:flat t) (?b)
                       (lisp ?b (if (graph-db::read-transaction *graph*) :yes :no)))))
    ;; and it is specifically NOT the read-write *TRANSACTION*
    (is (equal '(:no)
               (select (:flat t :snapshot t) (?b)
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

;;; ---------------------------------------------------------------------------
;;; GH #115: node-local time travel (spec 2026-09-07)
;;; ---------------------------------------------------------------------------

(defun %epoch-of (thunk)
  "Run THUNK in a transaction on *GRAPH*; the committed epoch."
  (graph-db::transaction-id
   (with-transaction () (funcall thunk) graph-db:*transaction*)))

(defmacro with-kept-graph ((g keep) &body body)
  "A fresh integration graph with :KEEP-REVISIONS KEEP, *GRAPH* bound."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *integration-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000 :keep-revisions ,keep)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (close-graph ,g :snapshot-p nil)
           (collect-garbage))))))

(test as-of-answers-the-version-live-at-each-epoch
  "Spec §2, R2: an as-of read is inclusive -- the version whose commit
epoch is the newest at or below E -- and NIL before the node existed."
  (with-kept-graph (g 3)
    (let (id e0 e1 e2 e3)
      (setq e0 (%epoch-of (lambda () (make-g-person :name "seed" :age 0))))
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 1) (setq e2 (latest-epoch g))
      (bump-age id 2) (setq e3 (latest-epoch g))
      (is (= e1 (1+ e0)) "control: consecutive commits, no clock")
      (is (= e3 (1+ e2))
          "LATEST-EPOCH tracks each new commit, not just the first")
      (with-as-of ((g) e0)
        (is (null (lookup-vertex id)) "before creation: absent"))
      (with-as-of ((g) e1)
        (is (= 0 (slot-value (lookup-vertex id) 'age))
            "inclusive at the creating epoch"))
      (with-as-of ((g) e2)
        (is (= 1 (slot-value (lookup-vertex id) 'age))))
      (with-as-of ((g) e3)
        (is (= 2 (slot-value (lookup-vertex id) 'age))))
      (is (= 2 (slot-value (lookup-vertex id) 'age))
          "outside the extent the live version answers"))))

(test as-of-reads-are-repeatable-across-a-concurrent-commit
  "Spec §3.1: reads inside one extent resolve at one epoch even when a
transaction commits an update meanwhile."
  (with-kept-graph (g 3)
    (let (id e)
      (setq e (%epoch-of
               (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (with-as-of ((g) e)
        (is (= 0 (slot-value (lookup-vertex id) 'age)))
        (bump-age id 7)
        (is (= 0 (slot-value (lookup-vertex id) 'age))
            "the concurrent update is invisible at E")))))

(test as-of-refuses-what-it-cannot-answer
  "Spec §2.2: the refusals, each by reason; the same epoch inherits and a
plain snapshot inside an as-of extent inherits it."
  (with-test-graph (g)
    (let (id e)
      (setq e (%epoch-of
               (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (flet ((reason (thunk)
               (handler-case (progn (funcall thunk) nil)
                 (as-of-refused (c) (as-of-refused-reason c)))))
        (is (eq :no-version-history
                (reason
                 (lambda ()
                   (let ((bare (make-instance 'graph-db::graph)))
                     (with-as-of ((bare) e) nil)))))
            "no transaction manager yet: refused, not silently live")
        (is (eq :future-epoch
                (reason (lambda () (with-as-of ((g) (1+ e)) nil)))))
        (is (eq :read-write-transaction
                (reason (lambda ()
                          (with-transaction () (with-as-of ((g) e) nil))))))
        (is (eq :snapshot-active
                (reason (lambda ()
                          (with-as-of ((g) e)
                            (with-as-of ((g) (1- e)) nil))))))
        (is (eq :snapshot-active
                (reason (lambda ()
                          (graph-db:with-read-snapshot (g)
                            (with-as-of ((g) e) nil))))))
        (is (null (reason (lambda ()
                            (with-as-of ((g) e) (with-as-of ((g) e) nil)))))
            "the same epoch inherits")
        (is (null (reason (lambda ()
                            (with-as-of ((g) e)
                              (graph-db:with-read-snapshot (g) nil)))))
            "a plain snapshot inside an as-of extent inherits it")))))

(test as-of-snapshot-holds-the-reaper-floor
  "Spec §2.3: an open as-of extent retains the versions live at E, as a
held read pin does (READ-PIN-RETAINS-VERSIONS-UNTIL-RELEASED); after the
extent the chain returns to steady state."
  (with-test-graph (g)
    (let (id e)
      (setq e (%epoch-of
               (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (flet ((live () (graph-db::lookup-node (graph-db::vertex-table g) id g)))
        (with-as-of ((g) e)
          (bump-age id 1) (bump-age id 2) (bump-age id 3)
          (is (>= (version-chain-length (live) g) 2)
              "an open as-of extent keeps prior versions from being reaped"))
        (bump-age id 4) (bump-age id 5)
        (is (= 1 (version-chain-length (live) g))
            "after the extent the chain returns to steady-state size")))))

(test as-of-reports-a-reaped-version-instead-of-lying
  "Spec §3.2, R4: with :KEEP-REVISIONS 1 and three updates the chain holds
the live version and one archived; an epoch older than that signals
VERSION-REAPED-ERROR naming the oldest retained epoch, :IF-REAPED :SKIP
answers NIL and counts, and the retained epoch still answers."
  (with-kept-graph (g 1)
    (let (id e1 e2 e3 e4)
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 1) (setq e2 (latest-epoch g))
      (bump-age id 2) (setq e3 (latest-epoch g))
      (bump-age id 3) (setq e4 (latest-epoch g))
      (let ((c (handler-case (with-as-of ((g) e1) (lookup-vertex id) nil)
                 (version-reaped-error (c) c))))
        (is (typep c 'version-reaped-error) "as-of E1 is reaped")
        (when (typep c 'version-reaped-error)
          (is (= e3 (version-reaped-oldest-epoch c))
              "the oldest retained version is the one committed at E3")
          (is (= 2 (version-reaped-oldest-revision c)))
          (is (= e1 (version-reaped-epoch c)))))
      (signals version-reaped-error (with-as-of ((g) e2) (lookup-vertex id)))
      (with-as-of ((g) e1 :if-reaped :skip)
        (is (null (lookup-vertex id)) ":skip answers NIL")
        (is (= 1 (as-of-skipped-count g)) "and counts the skip"))
      (is (null (as-of-skipped-count g)) "no count outside an extent")
      (with-as-of ((g) e3)
        (is (= 2 (slot-value (lookup-vertex id) 'age)) "E3 is retained"))
      (with-as-of ((g) e4)
        (is (= 3 (slot-value (lookup-vertex id) 'age)))))))

(test keep-revisions-zero-is-no-time-travel
  "Spec §3.2 (as ruled in the plan): the default :KEEP-REVISIONS 0 keeps
the live version and the one lagging version the committing transaction's
own floor retains; two updates later the creation epoch is reaped, and so
is an epoch before the node existed, because revision 0 is gone (the
documented limit, spec §3.2)."
  (with-test-graph (g)
    (let (id e0 e1)
      (setq e0 (%epoch-of (lambda () (make-g-person :name "seed" :age 0))))
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 1) (bump-age id 2)
      (signals version-reaped-error (with-as-of ((g) e1) (lookup-vertex id)))
      ;; With revision 0 gone, "created after E0" is unknowable: the
      ;; read reports reaped, the documented limit (spec §3.2).  The
      ;; "before creation is NIL" case lives in
      ;; AS-OF-ANSWERS-THE-VERSION-LIVE-AT-EACH-EPOCH, whose chain is intact.
      (signals version-reaped-error
        (with-as-of ((g) e0) (lookup-vertex id))))))

(defun %names-at (g e)
  "The NAMEs of G's G-PERSON vertices as of epoch E, sorted.  Runs a
typed scan inside a fresh as-of extent, so E must be nameable."
  (with-as-of ((g) e)
    (sort (map-vertices (lambda (v) (slot-value v 'name)) g
                        :collect-p t :vertex-type 'g-person)
          #'string<)))

(test as-of-typed-scan-reconstructs-membership-both-ways
  "Spec §3.3, the lookup path (§3.1, §3.2) reached through a typed scan:
at E the scan excludes a vertex created after E and includes one deleted
after E; at the deletion epoch it is gone.  A soft delete leaves its
type-index entry unflagged in the chain, so this holds without the
tombstone walk -- AS-OF-WALKS-COMPACTION-TOMBSTONES covers that.  The
case SNAPSHOT-HIDES-NODES-CREATED-AFTER-START never covered."
  (with-kept-graph (g 3)
    (let (b e-mid e-del)
      (with-transaction () (make-g-person :name "a" :age 1))
      (setq e-mid (%epoch-of
                   (lambda () (setq b (id (make-g-person :name "b" :age 2))))))
      (with-transaction () (make-g-person :name "c" :age 3))
      (setq e-del (%epoch-of (lambda () (mark-deleted (lookup-vertex b)))))
      (is (equal '("a" "b") (%names-at g e-mid))
          "c not yet created, b not yet deleted")
      (is (equal '("a" "c") (%names-at g e-del))
          "at the deletion epoch b is gone (inclusive)")
      (is (equal '("a" "c") (%names-at g (latest-epoch g))))
      (with-as-of ((g) e-mid)
        (is (= 1 (select-count (?p) (is-a ?p g-person)
                               (node-slot-value ?p name "b")))
            "is-a/2 enumerates through the same scan")))))

(test as-of-adjacency-reconstructs-edges-and-endpoints
  "Spec §3.3, the same lookup path reached through adjacency:
OUTGOING-EDGES at E excludes an edge created after E, includes one
deleted after E, and an edge whose endpoint was deleted after E is
active at E.  Soft-deleted ve/vev entries stay unflagged in the chain,
so this too holds without the tombstone walk --
AS-OF-WALKS-COMPACTED-ADJACENCY-TOMBSTONES covers that."
  (with-kept-graph (g 3)
    ;; B is bound for symmetry with A and C; only the vertex it names is
    ;; used, hence IGNORABLE.
    (let (a b c e-mid e-del)
      (declare (ignorable b))
      (with-transaction ()
        (let ((va (make-g-person :name "a" :age 1))
              (vb (make-g-person :name "b" :age 2))
              (vc (make-g-person :name "c" :age 3)))
          (setq a (id va) b (id vb) c (id vc))
          (make-g-knows :from va :to vb :since 1)))
      (setq e-mid (latest-epoch g))
      (with-transaction ()
        (make-g-knows :from (lookup-vertex a) :to (lookup-vertex c) :since 2))
      (setq e-del (%epoch-of
                   (lambda ()
                     (mark-deleted
                      (find 1 (outgoing-edges (lookup-vertex a))
                            :key (lambda (ed) (slot-value ed 'since))))
                     (mark-deleted (lookup-vertex c)))))
      (flet ((sinces-at (e)
               (with-as-of ((g) e)
                 (sort (mapcar (lambda (ed) (slot-value ed 'since))
                               (outgoing-edges (lookup-vertex a)))
                       #'<))))
        (is (equal '(1) (sinces-at e-mid)) "the second edge is not yet born")
        (is (equal '(1 2) (sinces-at (1- e-del)))
            "both born, neither deleted")
        (is (equal '() (sinces-at e-del))
            "at E-DEL the first edge is deleted and the second's endpoint
c is deleted, so neither is active")
        (is (equal '() (sinces-at (latest-epoch g))))))))

(test as-of-refuses-the-untyped-scan
  "Spec R6: the raw lhash walk reads live versions; under a named epoch it
is refused rather than answering live."
  (with-test-graph (g)
    (with-transaction () (make-g-person :name "a" :age 1))
    (let ((e (latest-epoch g)))
      (is (eq :untyped-scan
              (handler-case
                  (with-as-of ((g) e) (map-vertices #'identity g) nil)
                (as-of-refused (c) (as-of-refused-reason c)))))
      (is (eq :untyped-scan
              (handler-case
                  (with-as-of ((g) e) (map-edges #'identity g) nil)
                (as-of-refused (c) (as-of-refused-reason c))))))))

(test as-of-walks-compaction-tombstones
  "Spec §3.3, the tombstone walk itself: COMPACT-VERTICES de-indexes a
soft-deleted vertex by flagging its type-index pcons -- MARK-PCONS-DELETED
leaves the cell in the chain -- so membership at E survives compaction
only because an as-of scan passes :INCLUDE-DELETED-P T at the index-list
level.  Drop that flag and the first assertion goes red."
  (with-kept-graph (g 3)
    (let (b e-mid)
      (with-transaction () (make-g-person :name "a" :age 1))
      (with-transaction ()
        (setq b (id (make-g-person :name "b" :age 2))))
      (setq e-mid (latest-epoch g))
      (with-transaction () (mark-deleted (lookup-vertex b)))
      ;; Outside any as-of extent: COMPACT-VERTICES drives the untyped
      ;; scan, which an as-of snapshot refuses (R6).
      (compact-vertices g)
      (is (equal '("a" "b") (%names-at g e-mid))
          "b's FLAGGED type-index entry is still membership at E")
      (is (equal '("a") (%names-at g (latest-epoch g)))
          "control: at the latest epoch b resolves deleted and is gone"))))

(test as-of-walks-compacted-adjacency-tombstones
  "Spec §3.3 for edges: COMPACT-EDGES de-indexes a soft-deleted edge by
flagging its type/ve/vev pcons, so at E the edge is found again only
through the as-of :INCLUDE-DELETED-P T walk of the index list.  One
assertion per index that walk covers -- ve (adjacency), type, vev
(endpoint pair) -- plus EDGE-EXISTS-P's own vev walk."
  (with-kept-graph (g 3)
    (let (a b e-mid)
      (with-transaction ()
        (let ((va (make-g-person :name "a" :age 1))
              (vb (make-g-person :name "b" :age 2)))
          (setq a (id va) b (id vb))
          (make-g-knows :from va :to vb :since 1)))
      (setq e-mid (latest-epoch g))
      (with-transaction ()
        (mark-deleted (first (outgoing-edges (lookup-vertex a)))))
      ;; Outside any as-of extent, as COMPACT-VERTICES above.
      (compact-edges g)
      (flet ((sinces (&rest args)
               (mapcar (lambda (ed) (slot-value ed 'since))
                       (apply #'map-edges #'identity g :collect-p t args))))
        (with-as-of ((g) e-mid)
          (is (equal '(1) (sinces :vertex (lookup-vertex a)
                                  :direction :out))
              "ve index: the FLAGGED entry is still adjacency at E")
          (is (equal '(1) (sinces :edge-type 'g-knows))
              "type index: the same edge, through the typed scan")
          (is (equal '(1) (sinces :from-vertex (lookup-vertex a)
                                  :to-vertex (lookup-vertex b)))
              "vev index: the same edge, through the endpoint pair")
          (is (not (null (edge-exists-p 'g-knows (lookup-vertex a)
                                        (lookup-vertex b))))
              "EDGE-EXISTS-P walks the flagged vev entry too"))
        (with-as-of ((g) (latest-epoch g))
          (is (equal '() (sinces :vertex (lookup-vertex a)
                                 :direction :out))
              "control: at the latest epoch the edge resolves deleted")
          (is (null (edge-exists-p 'g-knows (lookup-vertex a)
                                   (lookup-vertex b)))
              "control: EDGE-EXISTS-P agrees at the latest epoch"))))))

(test per-call-as-of-opens-a-snapshot-for-the-call
  "Spec §3.4: :AS-OF on a lookup or scan answers at E for that call, the
result outlives the call, and inside an as-of extent at the same epoch it
inherits."
  (with-kept-graph (g 3)
    (let (id e1)
      (setq e1 (%epoch-of
                (lambda () (setq id (id (make-g-person :name "p" :age 0))))))
      (bump-age id 5)
      (let ((old (lookup-vertex id :as-of e1)))
        (is (= 0 (slot-value old 'age)) "the version at E1, materialised")
        (is (null graph-db:*read-snapshots*) "the snapshot closed"))
      (is (= 5 (slot-value (lookup-vertex id) 'age)))
      (is (equal '(0) (map-vertices (lambda (v) (slot-value v 'age)) g
                                    :collect-p t :vertex-type 'g-person
                                    :as-of e1)))
      (with-as-of ((g) e1)
        (is (= 0 (slot-value (lookup-vertex id :as-of e1) 'age))
            "same epoch inherits"))
      (signals as-of-refused (lookup-vertex id :as-of (1+ (latest-epoch g))))
      (signals as-of-refused
        (with-as-of ((g) e1) (lookup-vertex id :as-of (latest-epoch g)))))))

(test per-call-as-of-on-lookup-edge-and-map-edges
  "Spec §3.4, the edge twin of the vertex case: :AS-OF on LOOKUP-EDGE and
MAP-EDGES answers at E for that call and the result outlives the call;
LOOKUP-EDGE refuses a future epoch, same as LOOKUP-VERTEX."
  (with-kept-graph (g 3)
    (let (eid e1)
      (setq e1 (%epoch-of
                (lambda ()
                  (let ((a (make-g-person :name "a"))
                        (b (make-g-person :name "b")))
                    (setq eid
                          (id (make-g-knows :from a :to b :since 0)))))))
      (bump-since eid 5)
      (let ((old (lookup-edge eid :as-of e1)))
        (is (= 0 (slot-value old 'since)) "the version at E1, materialised")
        (is (null graph-db:*read-snapshots*) "the snapshot closed"))
      (is (= 5 (slot-value (lookup-edge eid) 'since)))
      (is (equal '(0) (map-edges (lambda (e) (slot-value e 'since)) g
                                 :collect-p t :edge-type 'g-knows
                                 :as-of e1)))
      (signals as-of-refused
        (lookup-edge eid :as-of (1+ (latest-epoch g)))))))

(test select-as-of-runs-the-query-at-an-epoch
  "Spec §3.5: SELECT :AS-OF E parallels :SNAPSHOT T and equals the same
query run at E; both together is a macroexpansion-time error."
  (with-kept-graph (g 3)
    (let (e1)
      (setq e1 (%epoch-of (lambda () (make-g-person :name "a" :age 1))))
      (with-transaction () (make-g-person :name "b" :age 2))
      (is (= 1 (length (select (:as-of e1) (?p) (is-a ?p g-person)))))
      (is (= 2 (select-count (?p) (is-a ?p g-person))))
      (is (equal '("a")
                 (select (:as-of e1 :flat t) (?n)
                   (is-a ?p g-person) (node-slot-value ?p name ?n))))
      (signals error
        (macroexpand-1 '(select (:snapshot t :as-of 1) (?p)
                          (is-a ?p g-person)))))))

(test edge-and-node-history-walk-the-chain-newest-first
  "Spec §4: EDGE-HISTORY is VERTEX-HISTORY's edge twin; NODE-HISTORY
dispatches on the node's class; entries are (VERSION . COMMIT-EPOCH)
newest first."
  (with-kept-graph (g 3)
    (let (aid eid e1 e2)
      (setq e1 (%epoch-of
                (lambda ()
                  (let ((a (make-g-person :name "a" :age 1))
                        (b (make-g-person :name "b" :age 2)))
                    (setq aid (id a))
                    (setq eid (id (make-g-knows :from a :to b :since 1)))))))
      (setq e2 (%epoch-of
                (lambda ()
                  (let ((c (copy (lookup-edge eid))))
                    (setf (slot-value c 'since) 2)
                    (save c)))))
      (let ((h (edge-history g eid)))
        (is (= 2 (length h)))
        (is (equal (list e2 e1) (mapcar #'cdr h)) "newest first")
        (is (equal '(2 1) (mapcar (lambda (p) (slot-value (car p) 'since)) h)))
        (is (equal (mapcar #'cdr h)
                   (mapcar #'cdr (node-history (lookup-edge eid))))))
      (is (equal (mapcar #'cdr (vertex-history g aid))
                 (mapcar #'cdr (node-history (lookup-vertex aid)))))
      (is (= 1 (length (edge-history g eid :limit 1)))))))
