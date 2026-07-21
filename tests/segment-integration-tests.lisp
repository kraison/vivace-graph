;;;; Tests for vector-segment transaction integration (Phase 2 step 3).

(in-package #:graph-db/test)

(def-suite segment-integration-suite
  :description "vector-index declaration, apply-path maintenance, recovery."
  :in graph-db-suite)

(in-suite segment-integration-suite)

;; Declared once at load time, like the other integration schema in this file.
(def-vertex si-doc ()
  ((title :type string)
   (embedding :vector-index t))
  :graph-db-integration-test)

(def-vertex si-sub (si-doc)
  ((extra))
  :graph-db-integration-test)

(test vector-index-slot-is-recognised
  "A :vector-index slot reports vector-index-p on the effective slot, and the
option is inherited by a subclass."
  (let ((doc-slot (find 'embedding (graph-db::class-slots (find-class 'si-doc))
                        :key #'graph-db::slot-definition-name))
        (sub-slot (find 'embedding (graph-db::class-slots (find-class 'si-sub))
                        :key #'graph-db::slot-definition-name)))
    (is (graph-db::vector-index-p doc-slot))
    (is (graph-db::vector-index-p sub-slot)
        "a :vector-index slot on the parent must apply to the subclass")))

(test node-vector-index-slots-lists-declared-slots
  "node-vector-index-slots returns the :vector-index slot names of a class."
  (is (member 'embedding
              (graph-db::node-vector-index-slots (find-class 'si-doc))))
  (is (null (graph-db::node-vector-index-slots
             (find-class 'graph-db::vertex)))
      "a class with no :vector-index slot has none"))

(test graph-has-empty-vector-segments-table
  "A fresh graph exposes an empty vector-segments hash."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (is (hash-table-p (graph-db::vector-segments g)))
             (is (= 0 (hash-table-count (graph-db::vector-segments g)))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(defun %si-embedding (dim base)
  (let ((v (make-array dim :element-type 'single-float)))
    (dotimes (i dim v) (setf (aref v i) (coerce (+ base (* 0.01 i)) 'single-float)))))

(defun %si-segment (graph slot)
  (gethash (cons 'si-doc slot) (graph-db::vector-segments graph)))

(test create-populates-the-segment
  "Creating a node with a conforming :vector-index value, through a transaction,
lazily creates the segment and stores the vector under the node id."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000))
          (id nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0))))))
             (let ((seg (%si-segment g 'embedding)))
               (is (not (null seg)) "segment was not created on insert")
               (let ((back (graph-db::segment-get seg id)))
                 (is (typep back '(simple-array single-float (*)))
                     "vector not stored (got ~S)" back)
                 (is (= 8 (length back)))
                 (is (every #'= (%si-embedding 8 1.0) back)))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test create-without-conforming-value-makes-no-segment
  "A node whose :vector-index slot is nil creates no segment."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction () (make-si-doc :title "no-vec")))
             (is (null (%si-segment g 'embedding))
                 "a nil embedding must not create a segment"))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test update-overwrites-the-vector (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction ()
               (let ((v (copy (lookup-vertex id))))
                 (setf (slot-value v 'embedding) (%si-embedding 8 5.0))
                 (save v))))
           (let ((back (graph-db::segment-get (%si-segment g 'embedding) id)))
             (is (typep back '(simple-array single-float (*))))
             (is (every #'= (%si-embedding 8 5.0) back))))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test clearing-the-value-removes-the-entry (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction ()
               (let ((v (copy (lookup-vertex id))))
                 (setf (slot-value v 'embedding) nil)
                 (save v))))
           (is (null (graph-db::segment-get (%si-segment g 'embedding) id))
               "an update to nil must remove the segment entry"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test delete-removes-the-entry (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (with-transaction () (mark-deleted (lookup-vertex id :graph g))))
           (is (null (graph-db::segment-get (%si-segment g 'embedding) id))
               "deleting a node must remove its segment entry"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test wrong-dimension-signals-and-rolls-back (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil)
        (count-before nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0)))))
             (setf count-before (length (map-vertices #'identity g
                                                       :collect-p t :vertex-type 'si-doc)))
             ;; a 9-dim vector into an established 8-dim segment must signal
             (signals error
               (let ((*graph* g))
                 (with-transaction ()
                   (make-si-doc :title "bad" :embedding (%si-embedding 9 2.0))))))
           ;; the bad NODE must not have been persisted -- the whole transaction
           ;; (node write included, not just the segment write) rolled back
           (is (= count-before (length (map-vertices #'identity g
                                                      :collect-p t :vertex-type 'si-doc)))
               "the rolled-back transaction must not have persisted the bad node")
           ;; the good node is still there; the bad transaction rolled back
           (let ((back (graph-db::segment-get (%si-segment g 'embedding) id)))
             (is (typep back '(simple-array single-float (*))))
             (is (= 8 (length back)))
             (is (every #'= (%si-embedding 8 1.0) back)))
           (is (= 1 (graph-db::segment-live-count (%si-segment g 'embedding)))
               "the rolled-back insert must not have landed in the segment"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test delete-with-no-established-segment-is-a-safe-no-op (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (id nil))
    (unwind-protect
         (progn
           (let ((*graph* g))
             ;; embedding nil at create -> no segment for (si-doc . embedding) is
             ;; ever established
             (with-transaction () (setf id (id (make-si-doc :title "no-vec"))))
             (is (null (%si-segment g 'embedding)))
             ;; deleting must not error even though there is no segment to
             ;; look up -- the (WHEN SEG ...) guard in TX-DELETE must hold
             (finishes
               (with-transaction () (mark-deleted (lookup-vertex id :graph g))))
             (is (null (%si-segment g 'embedding))
                 "deleting a node with no vector value must not create a segment")))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test same-transaction-dimension-conflict-rolls-back (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
    (unwind-protect
         (progn
           ;; fresh graph -- no prior segment for (si-doc . embedding) exists,
           ;; so neither write can see an already-established segment.  Both
           ;; writes are in the SAME transaction: the first would establish
           ;; the dimension, the second conflicts with it.
           (signals error
             (let ((*graph* g))
               (with-transaction ()
                 (make-si-doc :title "a" :embedding (%si-embedding 8 1.0))
                 (make-si-doc :title "b" :embedding (%si-embedding 9 2.0)))))
           ;; neither node may have been persisted -- the whole transaction
           ;; rolled back, not just the second write's segment update
           (is (= 0 (length (map-vertices #'identity g
                                          :collect-p t :vertex-type 'si-doc)))
               "an intra-transaction dimension conflict must roll back both writes")
           (is (null (%si-segment g 'embedding))
               "no segment should have been established by a rolled-back transaction"))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test cross-subclass-dimension-conflict-rolls-back (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
        (doc-id nil))
    (unwind-protect
         (progn
           ;; establish the owner segment at dimension 8 via a si-doc instance
           (let ((*graph* g))
             (with-transaction ()
               (setf doc-id (id (make-si-doc :title "a" :embedding (%si-embedding 8 1.0))))))
           ;; a si-sub instance shares the SAME (si-doc . embedding) owner
           ;; segment (Model B) -- a 9-dim embedding on the subclass must be
           ;; caught against the established 8-dim segment, exactly like a
           ;; mismatched si-doc write would be
           (signals error
             (let ((*graph* g))
               (with-transaction ()
                 (make-si-sub :title "bad-sub" :extra "x"
                              :embedding (%si-embedding 9 2.0)))))
           (is (= 0 (length (map-vertices #'identity g
                                          :collect-p t :vertex-type 'si-sub)))
               "the rolled-back si-sub transaction must not have persisted the bad node")
           (let ((owner-seg (%si-segment g 'embedding)))
             (is (= 1 (graph-db::segment-live-count owner-seg))
                 "the mismatched si-sub write must not have landed in the shared owner segment")
             (let ((back (graph-db::segment-get owner-seg doc-id)))
               (is (typep back '(simple-array single-float (*))))
               (is (every #'= (%si-embedding 8 1.0) back)
                   "the original si-doc entry in the owner segment must be untouched"))))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test same-transaction-cross-subclass-dimension-conflict-rolls-back (with-temp-directory (dir)
  (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
    (unwind-protect
         (progn
           ;; fresh graph -- no prior segment for (si-doc . embedding) exists,
           ;; so neither write can see an already-established segment.  Both
           ;; writes are in the SAME transaction, and they span the class
           ;; hierarchy: si-doc then si-sub.  Under Model B both map to the
           ;; same owner key (si-doc . embedding), so this is the specific
           ;; scenario that exercises the INTRA hash's cross-subclass
           ;; owner-keying -- distinct from same-transaction-dimension-
           ;; conflict-rolls-back (same class, si-doc/si-doc) and from
           ;; cross-subclass-dimension-conflict-rolls-back (cross class, but
           ;; against an already-COMMITTED segment, not intra-tx).
           (signals error
             (let ((*graph* g))
               (with-transaction ()
                 (make-si-doc :title "p" :embedding (%si-embedding 8 1.0))
                 (make-si-sub :title "c" :extra "x" :embedding (%si-embedding 9 2.0)))))
           ;; neither node may have been persisted -- the whole transaction
           ;; rolled back, not just the second write's segment update
           (is (= 0 (length (map-vertices #'identity g
                                          :collect-p t :vertex-type 'si-doc)))
               "an intra-transaction cross-subclass dimension conflict must roll back both writes")
           ;; no owner segment may have been left with a stray entry -- either
           ;; it's entirely absent, or (if present) empty.  The boolean is
           ;; computed OUTSIDE the IS form: FiveAM's IS macro special-cases
           ;; AND/OR for nicer diagnostics by evaluating every subform up
           ;; front, which defeats short-circuiting -- (OR (NULL X) (F X))
           ;; inside IS would call (F NIL) even when X is NIL.
           (let* ((owner-seg (%si-segment g 'embedding))
                  (owner-clean-p (or (null owner-seg)
                                      (= 0 (graph-db::segment-live-count owner-seg)))))
             (is (not (null owner-clean-p))
                 "no owner segment entry may survive a rolled-back cross-subclass transaction")))
      (close-graph g :snapshot-p nil))
    (collect-garbage))))

(test segment-survives-clean-reopen
  "After a clean close, reopening the graph opens the segment as-is and its
vectors are intact."
  (with-temp-directory (dir)
    (let ((id nil))
      (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 3.0))))))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let ((back (graph-db::segment-get (%si-segment g 'embedding) id)))
               (is (typep back '(simple-array single-float (*))))
               (is (every #'= (%si-embedding 8 3.0) back)))
          (close-graph g :snapshot-p nil)))
      (collect-garbage))))

(test subclass-shares-owner-segment
  "A :vector-index slot declared on a parent class is maintained in ONE segment
owned by the DECLARING class, spanning subclasses (Model B) -- a si-sub
instance's vector lands in the (si-doc . embedding) owner segment, not a
separate (si-sub . embedding) segment.  Direct regression for owner-keyed
segment ownership: exact-class keying (Model A) would give si-sub its own
segment, and both the live-count-2 assertion and the no-separate-segment
assertion below would fail."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
          (doc-id nil) (sub-id nil))
      (unwind-protect
           (progn
             (let ((*graph* g))
               (with-transaction ()
                 (setf doc-id (id (make-si-doc :title "doc" :embedding (%si-embedding 8 1.0)))))
               (with-transaction ()
                 (setf sub-id (id (make-si-sub :title "sub" :extra "x"
                                               :embedding (%si-embedding 8 2.0))))))
             ;; both the si-doc and the si-sub instance land in the ONE owner segment
             (let ((owner-seg (%si-segment g 'embedding)))
               (is (not (null owner-seg)) "the owner segment must exist")
               (is (= 2 (graph-db::segment-live-count owner-seg))
                   "the owner segment must hold BOTH the si-doc and the si-sub instance")
               (let ((doc-back (graph-db::segment-get owner-seg doc-id)))
                 (is (typep doc-back '(simple-array single-float (*)))
                     "the si-doc id must be present in the owner segment")
                 (is (every #'= (%si-embedding 8 1.0) doc-back)
                     "the si-doc vector must be intact in the owner segment"))
               (let ((sub-back (graph-db::segment-get owner-seg sub-id)))
                 (is (typep sub-back '(simple-array single-float (*)))
                     "the si-sub id must be present in the OWNER segment, not a separate one")
                 (is (every #'= (%si-embedding 8 2.0) sub-back)
                     "the si-sub vector must be intact in the owner segment")))
             ;; there must be no separate per-subclass (si-sub . embedding) segment
             (is (null (gethash (cons 'si-sub 'embedding) (graph-db::vector-segments g)))
                 "there must be no separate per-subclass si-sub segment under Model B")
             ;; a rebuild keyed on the owner reproduces the same live-count-2 segment,
             ;; sweeping the subclass instance in via MAP-VERTICES' default
             ;; :include-subclasses-p t
             (let ((rebuilt (graph-db::rebuild-vector-segment g 'si-doc 'embedding)))
               (is (= 2 (graph-db::segment-live-count rebuilt))
                   "rebuilding the owner segment must sweep in the subclass instance too")
               (is (typep (graph-db::segment-get rebuilt doc-id) '(simple-array single-float (*)))
                   "the si-doc id must be present in the rebuilt owner segment")
               (is (typep (graph-db::segment-get rebuilt sub-id) '(simple-array single-float (*)))
                   "the si-sub id must be present in the rebuilt owner segment")))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test invariant-segment-matches-rebuild
  "After an arbitrary create/update/delete sequence spanning BOTH a parent class
(si-doc) and a subclass (si-sub), the live OWNER segment -- which under Model B
holds BOTH classes' instances in the one (si-doc . embedding) segment -- equals
a fresh rebuild-from-nodes: same id set, same vectors, for the whole
hierarchy."
  (with-temp-directory (dir)
    (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000))
          (kept '())      ; merged (id . index) cells across si-doc + si-sub --
                          ; both share the ONE owner segment under Model B
          (kept-doc '())
          (kept-sub '()))
      (unwind-protect
           (progn
             (let ((*graph* g))
               ;; create 12 si-doc + 6 si-sub
               (dotimes (i 12)
                 (with-transaction ()
                   (let ((n (make-si-doc :title (format nil "n~d" i)
                                         :embedding (%si-embedding 8 (coerce i 'single-float)))))
                     (push (cons i (id n)) kept-doc))))
               (dotimes (i 6)
                 (with-transaction ()
                   (let ((n (make-si-sub :title (format nil "s~d" i) :extra "x"
                                         :embedding (%si-embedding 8 (coerce (+ 50 i) 'single-float)))))
                     (push (cons i (id n)) kept-sub))))
               ;; delete 3 si-doc, 2 si-sub
               (dolist (i '(2 5 9))
                 (with-transaction () (mark-deleted (lookup-vertex (cdr (assoc i kept-doc)) :graph g)))
                 (setf kept-doc (remove i kept-doc :key #'car)))
               (dolist (i '(1 4))
                 (with-transaction () (mark-deleted (lookup-vertex (cdr (assoc i kept-sub)) :graph g)))
                 (setf kept-sub (remove i kept-sub :key #'car)))
               ;; update 2 si-doc, 1 si-sub (copy-modify-save)
               (dolist (i '(0 7))
                 (with-transaction ()
                   (let ((v (copy (lookup-vertex (cdr (assoc i kept-doc)) :graph g))))
                     (setf (slot-value v 'embedding)
                           (%si-embedding 8 (coerce (+ 100 i) 'single-float)))
                     (save v))))
               (dolist (i '(0))
                 (with-transaction ()
                   (let ((v (copy (lookup-vertex (cdr (assoc i kept-sub)) :graph g))))
                     (setf (slot-value v 'embedding)
                           (%si-embedding 8 (coerce (+ 200 i) 'single-float)))
                     (save v)))))
             (setf kept (append kept-doc kept-sub))
             ;; snapshot the live OWNER segment's (id -> vector) map -- si-doc
             ;; AND si-sub instances share this ONE (si-doc . embedding) segment
             (let* ((live (gethash (cons 'si-doc 'embedding) (graph-db::vector-segments g)))
                    (live-map (make-hash-table :test 'equalp))
                    ;; captured BEFORE rebuild-vector-segment runs: it closes
                    ;; and replaces the currently-registered segment, so LIVE's
                    ;; own live-count is unreadable after that call.
                    (live-count-before (progn
                                          (is (not (null live))
                                              "the owner segment must exist before rebuild")
                                          (graph-db::segment-live-count live))))
               (dolist (cell kept)
                 (setf (gethash (cdr cell) live-map)
                       (graph-db::segment-get live (cdr cell))))
               ;; rebuild from nodes (sweeps si-doc AND si-sub, default
               ;; :include-subclasses-p t) into a fresh segment and compare
               (let ((rebuilt (graph-db::rebuild-vector-segment g 'si-doc 'embedding)))
                 (is (= (hash-table-count live-map)
                        (graph-db::segment-live-count rebuilt))
                     "rebuild has a different id count than the live owner segment")
                 ;; direct id-count invariant: the LIVE segment's own occupancy
                 ;; must equal the rebuild's -- catches a stale/leaked entry
                 ;; (e.g. a deleted id maintenance failed to remove) that the
                 ;; kept-only comparison above cannot see, since KEPT already
                 ;; excludes deleted ids by construction.
                 (is (= live-count-before
                        (graph-db::segment-live-count rebuilt))
                     "live owner segment's live-count differs from a fresh rebuild's ~
live-count -- the live segment likely retains an entry the rebuild does not")
                 (loop for id being the hash-keys of live-map using (hash-value v)
                       for r = (graph-db::segment-get rebuilt id)
                       do (is (typep r '(simple-array single-float (*)))
                              "id missing from rebuild")
                          (is (and v (every #'= v r))
                              "vector differs between live and rebuilt")))))
        (close-graph g :snapshot-p nil))
      (collect-garbage))))

(test unclean-shutdown-rebuilds-not-reopens
  "A segment left dirty on disk (simulated crash, carrying drift a stale reopen
would trust) causes OPEN-GRAPH -> RESTORE-VECTOR-SEGMENTS to rebuild it from
live nodes rather than open it as-is.  Drives the real end-to-end recovery
path (OPEN-GRAPH), not REBUILD-VECTOR-SEGMENT directly."
  (with-temp-directory (dir)
    (let ((id nil) (path nil) (phantom-id nil))
      (let ((g (make-graph *integration-graph-name* (namestring dir) :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction () (setf id (id (make-si-doc :title "a" :embedding (%si-embedding 8 4.0))))))
        (setf path (graph-db::%segment-file g 'si-doc 'embedding))
        ;; the graph itself closes CLEANLY -- no top-level .dirty marker, so
        ;; OPEN-GRAPH below will succeed without requiring recovery.
        (close-graph g :snapshot-p nil))
      ;; Simulate an unclean shutdown of JUST the vector segment: open it
      ;; directly (OPEN-VECTOR-SEGMENT flips the on-disk clean flag to dirty
      ;; as a side effect of opening -- the same mechanism
      ;; SEGMENT-CLEAN-SHUTDOWN-FLAG in segment-tests.lisp exercises) and
      ;; inject a PHANTOM entry with no corresponding live node, standing in
      ;; for drift a real crash could leave behind.  Deliberately never close
      ;; it, so both the dirty flag and the phantom entry persist on disk.
      (setf phantom-id (graph-db::gen-vertex-id))
      (let ((raw (graph-db::open-vector-segment path)))
        (graph-db::segment-put raw phantom-id (%si-embedding 8 999.0)))
      (let ((g (open-graph *integration-graph-name* (namestring dir))))
        (unwind-protect
             (let ((seg (%si-segment g 'embedding)))
               (is (not (null seg)) "segment must exist after open")
               (is (null (graph-db::segment-get seg phantom-id))
                   "a REBUILT segment must not carry a phantom entry a stale ~
as-is reopen would have kept -- proves RESTORE-VECTOR-SEGMENTS took the ~
rebuild branch, not the open-as-is branch")
               (is (= 1 (graph-db::segment-live-count seg))
                   "rebuild must contain exactly the one live node, not the phantom")
               (let ((back (graph-db::segment-get seg id)))
                 (is (typep back '(simple-array single-float (*))))
                 (is (every #'= (%si-embedding 8 4.0) back))))
          (close-graph g :snapshot-p nil)))
      (collect-garbage))))
