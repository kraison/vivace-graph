;;;; Branch B hub re-home apply (B2d-2): the resolver wired into the HUB push path.
;;;;
;;;; peer-merge-apply-tests drives the DEVICE pull-apply; this drives the HUB
;;;; REHOME-AUTHORED-OP -- the system-of-record side, where a device-pushed authored
;;;; op is applied through a JOURNALING transaction (so every other device pulls the
;;;; result) while merging field-by-field against the hub's live copy and preserving
;;;; the original op-id/origin/lamport on the re-journaled feed entry (design §5).
;;;;
;;;; Single image, no transport: build a device-authored op by hand and re-home it on
;;;; a hub peer-graph.  Assert: the merge lands (LWW / safety), the op-id is preserved
;;;; (a re-push is deduped), a release surfaces a conflict, and -- the hub's whole
;;;; point -- the merged result is RE-JOURNALED to the feed under the author's op-id.
;;;; Reuses g-person + the *apply-policy* (name :safety, else :lww) from the apply suite.

(in-package #:graph-db/test)

(def-suite peer-rehome-suite
  :description "Branch B hub re-home apply (REHOME-AUTHORED-OP)."
  :in graph-db-suite)

(in-suite peer-rehome-suite)

(defparameter *rehome-hub-origin* (id16 4) "The hub (receiver / system-of-record) origin.")

(defmacro with-rehome-hub ((g) &body body)
  "Run BODY with a hub peer-graph (merge-policy *apply-policy*) bound to G and to
*graph*.  Port 0 -> the accept loop binds an ephemeral port and idles; close joins it."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :hub :origin-id *rehome-hub-origin*
                           :replication-port 0 :replication-key "k"
                           :merge-policy *apply-policy* :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(defun feed-op-ids (graph)
  "The op-ids of every authored op in GRAPH's replication logs (the pull feed)."
  (let ((*graph* graph) (ids '()))
    (dolist (log (graph-db::applicable-replication-logs 0 graph) (nreverse ids))
      (ignore-errors
       (with-open-file (in log :element-type '(unsigned-byte 8))
         (loop
           (let ((meta (handler-case (graph-db::read-stream-packet in) (error () nil))))
             (unless meta (return))
             (let ((txh-vec (handler-case (graph-db::read-stream-packet in) (error () nil))))
               (unless txh-vec (return))
               (let ((wc (graph-db::write-count (graph-db::deserialize-tx-header-vector txh-vec))))
                 (dotimes (i wc) (handler-case (graph-db::read-stream-packet in) (error () (return))))
                 (multiple-value-bind (origin op-id lamport op-class)
                     (graph-db::deserialize-peer-meta meta)
                   (declare (ignore origin lamport))
                   (when (= op-class graph-db::+peer-op-authored+)
                     (push op-id ids))))))))))))

(defun id-in (id list) (member id list :test #'equalp))

(test rehome-lww-incoming-newer-lands-and-rejournals
  "A held field bucketed :lww takes the incoming value when the incoming stamp is
newer; the op-id is preserved (a re-push is deduped) and the merged result is
re-journaled to the pull feed under the author's op-id for other devices."
  (with-rehome-hub (g)
    (let ((vid (id (with-transaction () (make-g-person :name "SAFE" :age 5))))) ; hub lamport 1
      (with-transaction ()                                                       ; local edit, stamp age=2
        (let ((v (copy (lookup-vertex vid)))) (setf (slot-value v 'age) 6) (save v)))
      (let* ((op (make-authored-op (lookup-vertex vid)
                                   '((:name . "SAFE") (:age . 5))
                                   '((:name . "SAFE") (:age . 9))
                                   10 *merge-hub-origin*))
             (opid (graph-db::peer-op-op-id op)))
        (is (graph-db::rehome-authored-op g op) "the op re-homes")
        (let ((v (lookup-vertex vid)))
          (is (= 9 (slot-value v 'age)) "incoming (lamport 10) beats local (2)")
          (is (equal "SAFE" (slot-value v 'name)) "the carried field is untouched"))
        (is (= 10 (graph-db::node-field-stamp g vid :age)) ":age stamp advanced to 10")
        (is (>= (graph-db::lamport-counter g) 10) "the hub clock observed the incoming stamp")
        ;; identity preserved: the same op-id is now applied -> a re-push is a no-op.
        (is (graph-db::op-applied-p g opid) "the author's op-id is recorded (preserved)")
        (is (null (graph-db::rehome-authored-op g op)) "re-pushing the same op is deduped")
        ;; re-journaled to the feed under the AUTHOR's op-id (so device E pulls it).
        (is (id-in opid (feed-op-ids g)) "the merged result is re-journaled under the op-id")))))

(test rehome-safety-release-keeps-danger-and-surfaces
  "On the hub, an incoming RELEASE (dangerous -> SAFE) on a :safety field keeps the
dangerous local value and surfaces a conflict -- and still re-journals (the retained
dangerous value must propagate to other devices)."
  (with-rehome-hub (g)
    (let ((vid (id (with-transaction () (make-g-person :name "Confirmed" :age 5)))))
      (let* ((op (make-authored-op (lookup-vertex vid)
                                   '((:name . "Confirmed") (:age . 5))
                                   '((:name . "SAFE") (:age . 5))
                                   10 *merge-hub-origin*))
             (opid (graph-db::peer-op-op-id op)))
        (is (graph-db::rehome-authored-op g op) "the op re-homes")
        (is (equal "Confirmed" (slot-value (lookup-vertex vid) 'name))
            "the release attempt keeps the DANGEROUS local value")
        (let ((cs (graph-db::get-peer-conflicts g)))
          (is (= 1 (length cs)) "one conflict is surfaced")
          (is (equal "Confirmed" (graph-db::peer-conflict-kept-value (first cs))))
          (is (equal "SAFE" (graph-db::peer-conflict-loser-value (first cs)))))
        (is (id-in opid (feed-op-ids g)) "the kept-dangerous result still re-journals")))))

(test rehome-create-of-unheld-node-lands
  "A device authors a brand-new node the hub does not hold: re-home creates it on the
hub with the incoming state and re-journals it under the op-id."
  (with-rehome-hub (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name 'g-person :vertex :graph g)))
           (newid (gen-id))
           (base (make-instance 'g-person :id newid :type-id tid :revision 0))
           (op (progn (setf (graph-db::data base) '((:name . "Fresh") (:age . 3)))
                      (graph-db::make-peer-op
                       :kind :authored :op-id (graph-db::gen-op-id)
                       :origin *merge-hub-origin* :lamport 7 :tx-id 8
                       :writes (list (make-instance 'graph-db::tx-create :node base)))))
           (opid (graph-db::peer-op-op-id op)))
      (is (graph-db::rehome-authored-op g op) "the create re-homes")
      (let ((v (lookup-vertex newid)))
        (is (not (null v)) "the node now exists on the hub")
        (is (equal "Fresh" (slot-value v 'name)))
        (is (= 3 (slot-value v 'age))))
      (is (= 7 (graph-db::node-field-stamp g newid :name)) "create stamps the fields")
      (is (id-in opid (feed-op-ids g)) "the created node re-journals under the op-id"))))

;;; --- B3-2: safety-bearing edge-removal surfacing (re-home path) ---------------
;;;
;;; g-knows is treated as a safety-bearing edge, g-likes as structural.  A device's
;;; removal of a still-live safety-bearing edge SURFACES + is KEPT; a structural edge
;;; (or a safety edge the hub already removed) applies with no surface.

(defparameter *edge-policy*
  (graph-db::make-merge-policy
   :field-bucket #'apply-field-bucket :safety-merge #'test-safety-merge
   :edge-bucket (lambda (etype) (if (eq etype 'g-knows) :safety-edge :structural))))

(defmacro with-edge-hub ((g) &body body)
  "A hub peer-graph whose merge-policy buckets g-knows :safety-edge."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :hub :origin-id *rehome-hub-origin*
                           :replication-port 0 :replication-key "k"
                           :merge-policy *edge-policy* :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(defun make-edge-delete-op (edge origin lamport)
  "An authored op that deletes EDGE (its id + type are all the re-home delete path reads)."
  (graph-db::make-peer-op
   :kind :authored :op-id (graph-db::gen-op-id) :origin origin :lamport lamport :tx-id 50
   :writes (list (make-instance 'graph-db::tx-delete :node edge :old-node edge))))

(defun ecount (g) (length (map-edges #'identity g :collect-p t)))

(test rehome-safety-edge-removal-surfaces-and-keeps
  "A device removing a still-live safety-bearing edge SURFACES a conflict and the edge
is KEPT (a safety linkage is never dropped from the authoritative record silently)."
  (with-edge-hub (g)
    (let (eid)
      (with-transaction ()
        (let ((a (make-g-person :name "A" :age 1))
              (b (make-g-person :name "B" :age 2)))
          (setq eid (id (make-g-knows :from a :to b :since 2020)))))
      (is (= 1 (ecount g)) "the safety edge exists")
      (let ((op (make-edge-delete-op (lookup-edge eid) *merge-hub-origin* 10)))
        (is (graph-db::rehome-authored-op g op) "the delete re-homes")
        (is (= 1 (ecount g)) "the safety-bearing edge is KEPT (still live)")
        (let ((cs (graph-db::get-peer-conflicts g)))
          (is (= 1 (length cs)) "a conflict is surfaced")
          (let ((c (first cs)))
            (is (eq :safety-edge (graph-db::peer-conflict-bucket c)))
            (is (equal "removed" (graph-db::peer-conflict-loser-value c)))
            (is (equalp eid (graph-db::peer-conflict-node-id c)) "keyed on the edge id")
            (is (equalp *merge-hub-origin* (graph-db::peer-conflict-loser-origin c))
                "the removal's author is recorded")))))))

(test rehome-structural-edge-removal-applies
  "A non-safety-bearing edge removal applies with no surface (plain remove-wins)."
  (with-edge-hub (g)
    (let (eid)
      (with-transaction ()
        (let ((a (make-g-person :name "A" :age 1))
              (b (make-g-person :name "B" :age 2)))
          (setq eid (id (make-g-likes :from a :to b)))))
      (let ((op (make-edge-delete-op (lookup-edge eid) *merge-hub-origin* 10)))
        (is (graph-db::rehome-authored-op g op) "the delete re-homes")
        (is (= 0 (ecount g)) "the structural edge is removed")
        (is (null (graph-db::get-peer-conflicts g)) "no conflict for a structural edge")))))

(test rehome-safety-edge-removal-when-hub-already-removed-applies
  "When the hub has ALREADY removed the safety edge, a device's removal converges
silently (no surface) -- the still-live gate is what makes it a conflict."
  (with-edge-hub (g)
    (let (eid edge)
      (with-transaction ()
        (let ((a (make-g-person :name "A" :age 1))
              (b (make-g-person :name "B" :age 2)))
          (setq eid (id (make-g-knows :from a :to b :since 2020)))))
      (setq edge (lookup-edge eid))                 ; capture before the hub removes it
      (with-transaction () (mark-deleted (lookup-edge eid)))
      (is (= 0 (ecount g)) "the hub already removed it")
      (let ((op (make-edge-delete-op edge *merge-hub-origin* 10)))
        (is (graph-db::rehome-authored-op g op) "the delete re-homes")
        (is (null (graph-db::get-peer-conflicts g))
            "no surface when the edge is already gone at the hub")))))

;;; --- B3-2b: find-of-type edge multiplicity surfacing (re-home path) -----------
;;;
;;; g-likes stands in for find-of-type (bucket :safety-surface-edge).  A 2nd concurrent
;;; g-likes edge out of the same source vertex surfaces a multiplicity conflict; both
;;; edges are kept (edges union).  Also exercises edge-create re-home (from/to carried).

(defparameter *multiplicity-policy*
  (graph-db::make-merge-policy
   :edge-bucket (lambda (etype) (if (eq etype 'g-likes) :safety-surface-edge :structural))))

(defmacro with-multiplicity-hub ((g) &body body)
  `(with-temp-directory (dir)
     (let ((,g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :hub :origin-id *rehome-hub-origin*
                           :replication-port 0 :replication-key "k"
                           :merge-policy *multiplicity-policy* :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(defun make-edge-create-op (edge-type from to graph origin lamport)
  "An authored op creating a fresh EDGE-TYPE edge FROM->TO (ids)."
  (let* ((tid (graph-db::node-type-id
               (graph-db::lookup-node-type-by-name edge-type :edge :graph graph)))
         (e (make-instance edge-type :id (gen-id) :type-id tid :revision 0
                                     :from from :to to)))
    (setf (graph-db::data e) nil (graph-db::bytes e) (graph-db::serialize nil))
    (graph-db::make-peer-op
     :kind :authored :op-id (graph-db::gen-op-id) :origin origin :lamport lamport :tx-id 60
     :writes (list (make-instance 'graph-db::tx-create :node e)))))

(test rehome-find-of-type-multiplicity-surfaces
  "A device's FIRST find-of-type-style edge re-homes with no conflict; a 2nd concurrent
one out of the same find SURFACES a multiplicity conflict and both edges are kept."
  (with-multiplicity-hub (g)
    (let (aid bid cid)
      (with-transaction ()
        (setq aid (id (make-g-person :name "Find"  :age 0))
              bid (id (make-g-person :name "TypeB" :age 0))
              cid (id (make-g-person :name "TypeC" :age 0))))
      ;; 1st classification: no sibling -> created (from/to preserved), no conflict
      (is (graph-db::rehome-authored-op
           g (make-edge-create-op 'g-likes aid bid g *merge-hub-origin* 10))
          "1st type edge re-homes")
      (is (= 1 (ecount g)) "the edge was created with from/to intact")
      (is (null (graph-db::get-peer-conflicts g)) "1st classification: no multiplicity")
      ;; 2nd concurrent classification: sibling exists -> surface, both kept
      (is (graph-db::rehome-authored-op
           g (make-edge-create-op 'g-likes aid cid g *merge-hub-origin* 11))
          "2nd type edge re-homes")
      (is (= 2 (ecount g)) "both type edges are kept (union)")
      (let ((cs (graph-db::get-peer-conflicts g)))
        (is (= 1 (length cs)) "the multiplicity surfaces once")
        (let ((c (first cs)))
          (is (eq :safety-surface-edge (graph-db::peer-conflict-bucket c)))
          (is (equalp aid (graph-db::peer-conflict-node-id c)) "keyed on the FIND")
          (is (equalp bid (graph-db::peer-conflict-kept-value c)) "kept = existing type target")
          (is (equalp cid (graph-db::peer-conflict-loser-value c)) "loser = new type target"))))))
