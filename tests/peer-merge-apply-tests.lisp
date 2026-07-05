;;;; Branch B device pull-apply merge (B2d-1): the resolver wired into a live graph.
;;;;
;;;; peer-merge-tests.lisp exercises the PURE resolver; this drives the whole
;;;; device path -- APPLY-PEER-AUTHORED-OP with a MERGE-POLICY set -- against a real
;;;; peer-graph.  Create a node locally (minting field stamps), then hand the apply
;;;; path a conflicting AUTHORED op built by hand and assert: the LWW winner lands,
;;;; a safety release keeps the dangerous local value and surfaces a conflict, and
;;;; the applied op-id/lamport advance.  Reuses g-person (name/age); the mock policy
;;;; buckets :name :safety (dangerous unless "SAFE") and :age :lww.

(in-package #:graph-db/test)

(def-suite peer-merge-apply-suite
  :description "Branch B device pull-apply merge (APPLY-PEER-AUTHORED-OP)."
  :in graph-db-suite)

(in-suite peer-merge-apply-suite)

(defparameter *merge-dev-origin* (id16 2) "The device (receiver) origin.")
(defparameter *merge-hub-origin* (id16 9) "The incoming op's (hub) origin -- higher.")

(defun apply-field-bucket (type slot)
  (declare (ignore type))
  (case slot (:name :safety) (t :lww)))

(defparameter *apply-policy*
  (graph-db::make-merge-policy :field-bucket #'apply-field-bucket
                               :safety-merge #'test-safety-merge))

(defun make-authored-op (local-node old-data new-data lamport origin)
  "Build an AUTHORED peer-op of a single tx-update against LOCAL-NODE's id: the
author saw OLD-DATA and wrote NEW-DATA.  NODE/OLD-NODE are copies of the live node
carrying the two data alists (only their DATA matters to the resolver)."
  (let ((base (copy local-node))
        (new (copy local-node)))
    (setf (graph-db::data base) old-data
          (graph-db::data new) new-data)
    (graph-db::make-peer-op
     :kind :authored :op-id (graph-db::gen-op-id) :origin origin :lamport lamport
     :tx-id 9999
     :writes (list (make-instance 'graph-db::tx-update :node new :old-node base)))))

(defmacro with-merge-device ((g) &body body)
  "Run BODY with a device peer-graph (merge-policy *apply-policy*) bound to G and to
*graph*, cleaning up afterward."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :device :origin-id *merge-dev-origin*
                           :peer-host "localhost" :replication-port 0
                           :merge-policy *apply-policy* :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(test apply-lww-incoming-newer-lands
  "A held field bucketed :lww takes the incoming value when the incoming stamp is
newer, and the applied op advances the op-id set + Lamport clock."
  (with-merge-device (g)
    (let ((vid (id (with-transaction () (make-g-person :name "SAFE" :age 5))))) ; lamport 1
      ;; local edit bumps :age's stamp to lamport 2 (loses to the incoming 10)
      (with-transaction ()
        (let ((v (copy (lookup-vertex vid)))) (setf (slot-value v 'age) 6) (save v)))
      (let ((op (make-authored-op (lookup-vertex vid)
                                  '((:name . "SAFE") (:age . 5))
                                  '((:name . "SAFE") (:age . 9))
                                  10 *merge-hub-origin*)))
        (is (graph-db::apply-peer-authored-op g op) "the op applies")
        (let ((v (lookup-vertex vid)))
          (is (= 9 (slot-value v 'age)) "incoming (lamport 10) beats local (2)")
          (is (equal "SAFE" (slot-value v 'name)) "the carried field is untouched"))
        (is (null (graph-db::get-peer-conflicts g)) "LWW never surfaces")
        (is (= 10 (graph-db::node-field-stamp g vid :age)) ":age stamp advanced to 10")
        (is (>= (graph-db::lamport-counter g) 10) "the clock observed the incoming stamp")
        (is (null (graph-db::apply-peer-authored-op g op)) "re-applying the op is a no-op")))))

(test apply-safety-release-keeps-danger-and-surfaces
  "A held field bucketed :safety: an incoming RELEASE (dangerous -> SAFE) keeps the
dangerous local value and surfaces a peer-conflict for the review surface."
  (with-merge-device (g)
    (let ((vid (id (with-transaction () (make-g-person :name "Confirmed" :age 5)))))
      (let ((op (make-authored-op (lookup-vertex vid)
                                  '((:name . "Confirmed") (:age . 5))
                                  '((:name . "SAFE") (:age . 5))
                                  10 *merge-hub-origin*)))
        (is (graph-db::apply-peer-authored-op g op) "the op applies")
        (is (equal "Confirmed" (slot-value (lookup-vertex vid) 'name))
            "the release attempt keeps the DANGEROUS local value")
        (let ((cs (graph-db::get-peer-conflicts g)))
          (is (= 1 (length cs)) "one conflict is surfaced")
          (let ((c (first cs)))
            (is (equal "Confirmed" (graph-db::peer-conflict-kept-value c)))
            (is (equal "SAFE" (graph-db::peer-conflict-loser-value c)))
            (is (eq :safety (graph-db::peer-conflict-bucket c)))))))))

(test apply-safety-reopen-toward-danger-lands
  "An incoming re-open toward danger (SAFE -> dangerous) auto-applies with no surface."
  (with-merge-device (g)
    (let ((vid (id (with-transaction () (make-g-person :name "SAFE" :age 5)))))
      (let ((op (make-authored-op (lookup-vertex vid)
                                  '((:name . "SAFE") (:age . 5))
                                  '((:name . "Confirmed") (:age . 5))
                                  10 *merge-hub-origin*)))
        (is (graph-db::apply-peer-authored-op g op) "the op applies")
        (is (equal "Confirmed" (slot-value (lookup-vertex vid) 'name))
            "re-opening toward danger auto-applies (fail-safe)")
        (is (null (graph-db::get-peer-conflicts g)) "a toward-danger change never surfaces")
        (is (= 10 (graph-db::node-field-stamp g vid :name)) "and stamps the winner")))))
