;;;; Tests for the subset-replication filter (spatial area-of-operations).
;;;;
;;;; A slave graph's REPLICATION-FILTER predicate decides which replicated
;;;; writes it applies; MAKE-SPATIAL-REPLICATION-FILTER builds one from an area
;;;; geometry.  APPLY-TRANSACTION runs the per-transaction FILTER-WRITES on a
;;;; slave.  End-to-end master/slave streaming is covered by the separate
;;;; process-based harness in tests/replication/; here we unit-test the filter
;;;; logic.  Reuses GEO-PLACE + NODE-GEOMETRY from spatial-hook-tests.

(in-package #:graph-db/test)

(def-suite subset-replication-suite
  :description
      "Subset replication: spatial area filter predicate + write filtering."
  :in graph-db-suite)

(in-suite subset-replication-suite)

(defparameter *area*
  '(((12.340 45.670) (12.350 45.670) (12.350 45.676) (12.340 45.676) (12.340
                                                                       45.670)))
  "A small synthetic coverage-area polygon (ring list).")

(test spatial-filter-predicate
  "The area filter accepts in-area spatial nodes and all non-spatial nodes; it
rejects spatial nodes outside the area."
  (with-test-graph (g)
    (declare (ignore g))
    (let (in-node out-node person)
      (with-transaction ()
        ;; in-node inside the area; out-node far away
        (setq in-node  (make-geo-place :loc (make-point 12.345d0 45.673d0))
              out-node (make-geo-place :loc (make-point 2.47d0 41.78d0))
              person   (make-g-person :name "no geometry")))
      (let ((filter (make-spatial-replication-filter (make-polygon *area*))))
        (is (funcall filter in-node)  "in-area spatial node accepted")
        (is (not (funcall filter out-node)) "out-of-area spatial node rejected")
        (is (funcall filter person)   "non-spatial node accepted (replicates in full)")))))

(test filter-writes-keeps-subset
  "filter-writes drops the writes the filter rejects; nil filter keeps all."
  (with-test-graph (g)
    (declare (ignore g))
    (let (in-node out-node person)
      (with-transaction ()
        (setq in-node  (make-geo-place :loc (make-point 12.345d0 45.673d0))
              out-node (make-geo-place :loc (make-point 2.47d0 41.78d0))
              person   (make-g-person :name "x")))
      (let* ((filter (make-spatial-replication-filter (make-polygon *area*)))
             (w-in     (make-instance 'graph-db::tx-create :node in-node))
             (w-out    (make-instance 'graph-db::tx-create :node out-node))
             (w-person (make-instance 'graph-db::tx-create :node person))
             (writes   (list w-in w-out w-person))
             (kept     (filter-writes writes filter)))
        (is (= 2 (length kept)))
        (is (member w-in kept))
        (is (not (member w-out kept)))
        (is (member w-person kept))
        ;; No filter -> every write is kept (full replication, the default).
        (is (= 3 (length (filter-writes writes nil))))))))

(test slave-graph-carries-replication-filter
  "The replication-filter slot exists on slave-graph and round-trips a value."
  (let ((s (make-instance 'graph-db::slave-graph)))
    (is (null (replication-filter s)))
    (setf (replication-filter s) (make-spatial-replication-filter (make-polygon
                                                                    *area*)))
    (is (functionp (replication-filter s)))))

(test reconcile-handles-boundary-crossing
  "reconcile-slave-writes transforms an update that crosses the area boundary: a
node LEAVING the subset becomes a delete; one ENTERING becomes a create; a node
staying in/out keeps/drops.  (Generalises filter-writes with slave presence.)"
  (with-test-graph (g)
    (let (p a)
      (with-transaction ()
        ;; P present in-area; A will be removed
        (setq p (make-geo-place :loc (make-point 12.345d0 45.673d0))
              a (make-geo-place :loc (make-point 12.346d0 45.674d0))))
      ;; A previously left the subset, so the slave no longer holds it (deleted).
      (with-transaction () (mark-deleted (lookup-vertex (id a))))
      (let* ((filter (make-spatial-replication-filter (make-polygon *area*)))
             (far-pt   (make-point 2.47d0 41.78d0))
             (live-p (lookup-vertex (id p)))
             (mv (lambda (src lon-lat-geom)            ; copy SRC, set its loc
                   (let ((v (copy src)))
                     (setf (slot-value v 'loc) lon-lat-geom) v)))
             w-p-out w-p-in w-a-in w-a-out)
        ;; COPY only registers a copy as writable against a live *TRANSACTION*
        ;; (GH #135); building these synthetic tx-updates outside one relied on
        ;; unsupported behaviour (a bare warning today, not a contract).
        (with-transaction ()
          (setq w-p-out (make-instance
                        'graph-db::tx-update
                        :node (funcall mv live-p far-pt) :old-node live-p))
          (setq w-p-in  (make-instance
                        'graph-db::tx-update
                        :node (funcall mv live-p (make-point 12.347d0 45.675d0))
                        :old-node live-p))
          (setq w-a-in  (make-instance
                        'graph-db::tx-update
                        :node (funcall mv a (make-point 12.348d0 45.675d0))
                        :old-node a))
          (setq w-a-out (make-instance
                        'graph-db::tx-update
                        :node (funcall mv a far-pt) :old-node a)))
        (flet ((one (w) (graph-db::reconcile-slave-writes (list w) filter g)))
          ;; present + leaves subset -> delete
          (let ((r (one w-p-out)))
            (is (= 1 (length r)))
            (is (typep (first r) 'graph-db::tx-delete) "leaving node -> delete"))
          ;; present + stays in subset -> update kept
          (let ((r (one w-p-in)))
            (is (= 1 (length r)))
            (is (eq w-p-in (first r)) "staying node -> update kept"))
          ;; absent + enters subset -> create
          (let ((r (one w-a-in)))
            (is (= 1 (length r)))
            (is (typep (first r) 'graph-db::tx-create) "entering node -> create")
            (is (not (typep (first r) 'graph-db::tx-update))))
          ;; absent + stays out -> dropped
          (is (null (one w-a-out)) "still-outside node -> dropped"))))))
