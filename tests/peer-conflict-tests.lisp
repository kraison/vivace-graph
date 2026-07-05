;;;; Branch B durable conflict-record store + enumeration API (B3).
;;;;
;;;; The resolver surfaces losers (peer-merge-tests); this covers RETAINING them for
;;;; the app review surface: a conflict carries the causing op-id, is idempotent
;;;; (re-recording the same (node, slot, op-id) is a no-op), survives close/reopen
;;;; with its arbitrary field VALUES intact, and supports resolve / filter / clear.
;;;; Reuses g-person + *apply-policy* / with-merge-device / make-authored-op from the
;;;; apply suite (name :safety -> a release surfaces a conflict).

(in-package #:graph-db/test)

(def-suite peer-conflict-suite
  :description "Branch B durable conflict enumeration API (B3)."
  :in graph-db-suite)

(in-suite peer-conflict-suite)

(defun surface-a-conflict (g vid)
  "Apply a conflicting authored op (release the :safety name Confirmed->SAFE) to VID
on G, returning the op-id that surfaced the conflict."
  (let ((op (make-authored-op (lookup-vertex vid)
                              '((:name . "Confirmed") (:age . 5))
                              '((:name . "SAFE") (:age . 5))
                              10 *merge-hub-origin*)))
    (graph-db::apply-peer-authored-op g op)
    (graph-db::peer-op-op-id op)))

(test conflict-recorded-with-op-id-and-idempotent
  "A surfaced conflict carries the causing op-id, and re-recording the same
(node, slot, op-id) does not duplicate it."
  (with-merge-device (g)
    (let* ((vid (id (with-transaction () (make-g-person :name "Confirmed" :age 5))))
           (opid (surface-a-conflict g vid)))
      (let ((cs (graph-db::get-peer-conflicts g)))
        (is (= 1 (length cs)) "one conflict surfaced")
        (is (equalp opid (graph-db::peer-conflict-op-id (first cs)))
            "the conflict carries the causing op-id"))
      ;; re-record the identical conflict -> idempotent
      (graph-db::record-peer-conflict
       g (graph-db::make-peer-conflict :node-id vid :slot :name :bucket :safety
                                       :kept-value "Confirmed" :loser-value "SAFE"
                                       :op-id opid))
      (is (= 1 (graph-db::count-peer-conflicts g :include-resolved t))
          "the duplicate (same node/slot/op-id) is not retained")
      ;; a DIFFERENT op-id on the same field is a distinct conflict
      (graph-db::record-peer-conflict
       g (graph-db::make-peer-conflict :node-id vid :slot :name :bucket :safety
                                       :kept-value "Confirmed" :loser-value "SAFE"
                                       :op-id (id16 123)))
      (is (= 2 (graph-db::count-peer-conflicts g :include-resolved t))
          "a distinct causing op-id is a distinct conflict"))))

(test conflicts-survive-close-reopen
  "Conflict records (with their arbitrary field values + ids) are durable across a
close/reopen."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) vid opid)
      (let ((g (make-graph *integration-graph-name* path
                           :peer-role :device :origin-id *merge-dev-origin*
                           :peer-host "localhost" :replication-port 0
                           :merge-policy *apply-policy* :buffer-pool-size 1000)))
        (let ((*graph* g))
          (setq vid (id (with-transaction () (make-g-person :name "Confirmed" :age 5)))
                opid (surface-a-conflict g vid)))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* path
                           :peer-role :device :origin-id *merge-dev-origin*
                           :peer-host "localhost" :replication-port 0
                           :merge-policy *apply-policy*)))
        (unwind-protect
             (let ((cs (graph-db::get-peer-conflicts g)))
               (is (= 1 (length cs)) "the conflict survived reopen")
               (let ((c (first cs)))
                 (is (equal "Confirmed" (graph-db::peer-conflict-kept-value c))
                     "kept-value round-trips")
                 (is (equal "SAFE" (graph-db::peer-conflict-loser-value c))
                     "loser-value round-trips")
                 (is (equalp opid (graph-db::peer-conflict-op-id c)) "op-id round-trips")
                 (is (equalp *merge-hub-origin* (graph-db::peer-conflict-loser-origin c))
                     "loser-origin round-trips")
                 (is (eq :safety (graph-db::peer-conflict-bucket c)) "bucket round-trips")))
          (close-graph g :snapshot-p nil))))))

(test resolve-filter-and-clear-conflicts
  "The review workflow: filter by node/slot, resolve (drops from the open set but
stays retained), and clear resolved."
  (with-merge-device (g)
    (let ((vid (id (with-transaction () (make-g-person :name "Confirmed" :age 5)))))
      (surface-a-conflict g vid)
      (is (= 1 (graph-db::count-peer-conflicts g)) "one open conflict")
      (let ((c (first (graph-db::get-peer-conflicts g :node-id vid :slot :name))))
        (is (not (null c)) "filter by node + slot finds it")
        (is (null (graph-db::get-peer-conflicts g :slot :age)) "filter by another slot is empty")
        (graph-db::resolve-peer-conflict g c))
      (is (= 0 (graph-db::count-peer-conflicts g)) "resolved -> no longer counted open")
      (is (= 1 (length (graph-db::get-peer-conflicts g :include-resolved t)))
          "but still retained (resolved) for the audit trail")
      (graph-db::clear-resolved-peer-conflicts g)
      (is (= 0 (length (graph-db::get-peer-conflicts g :include-resolved t)))
          "clear-resolved drops it"))))
