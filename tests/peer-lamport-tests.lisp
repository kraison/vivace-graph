;;;; Durable Lamport clock (peer replication, Branch B / PT-8).
;;;;
;;;; A peer-graph's logical clock must be monotonic ACROSS restarts: if it reset
;;;; to 0 after a crash, the replica's post-restart authored ops would get tiny
;;;; stamps and silently lose every LWW race (safety data dropped).  So the
;;;; counter is persisted on every advance and reloaded on open, and advanced to
;;;; MAX(local, received) whenever an op is applied.  Single-image test on a
;;;; device-role peer-graph (which mints a stamp per local commit); the multi-
;;;; writer conflict behaviour that consumes these stamps is tested by the
;;;; process-based peer harnesses.  Reuses the g-person schema from graph-tests.

(in-package #:graph-db/test)

(def-suite peer-lamport-suite
  :description "Durable, monotonic Lamport clock (PT-8)."
  :in graph-db-suite)

(in-suite peer-lamport-suite)

(defparameter *lamport-origin*
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 3)
  "A fixed device origin id for the lamport test.")

(test lamport-clock-persists-and-recovers
  "The Lamport counter advances on each authored commit, is persisted, survives a
close/reopen (never resets), jumps forward on OBSERVE, and never moves backward."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      ;; --- mint via commits; confirm advance + on-disk persistence ---
      (let ((g (make-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (is (= 0 (graph-db::lamport-counter g)) "fresh peer-graph starts at 0")
          (dotimes (i 3) (with-transaction () (make-g-person :name "l")))
          (is (= 3 (graph-db::lamport-counter g)) "three commits advance to 3")
          (is (= 3 (graph-db::load-lamport-counter g)) "the counter is on disk"))
        (close-graph g :snapshot-p nil))
      ;; --- reopen: recovers, does not reset to 0 ---
      (let ((g (open-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0)))
        (unwind-protect
             (let ((*graph* g))
               (is (= 3 (graph-db::lamport-counter g)) "reopen recovers the counter")
               ;; observe a higher received stamp -> jump; next mint is after it
               (graph-db::peer-observe-lamport g 10)
               (is (= 10 (graph-db::lamport-counter g)) "observe jumps forward")
               (with-transaction () (make-g-person :name "l"))
               (is (= 11 (graph-db::lamport-counter g)) "next mint is after the observed stamp")
               ;; a lower received stamp is a no-op (monotonic)
               (graph-db::peer-observe-lamport g 5)
               (is (= 11 (graph-db::lamport-counter g)) "a lower observe never moves it back"))
          (close-graph g :snapshot-p nil))))))

(test pull-cursor-is-separate-from-own-feed-seq
  "A device's pull-cursor (highest HUB feed-seq applied) is a durable scalar DISTINCT
from HIGHEST-TRANSACTION-ID (its own feed-seq): local authoring advances the latter
but never the pull-cursor; an applied hub frontier advances the pull-cursor but not
the feed-seq; both survive close/reopen.  Guards the B2d-2b conflation fix -- a
Branch B device writes, so the two number spaces must not share one scalar."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (is (= 0 (graph-db::load-peer-pull-cursor g)) "fresh pull-cursor is 0")
          (dotimes (i 3) (with-transaction () (make-g-person :name "d")))  ; own feed-seq -> 3
          (is (= 0 (graph-db::load-peer-pull-cursor g))
              "local authoring never moves the pull-cursor")
          (is (> (graph-db::load-highest-transaction-id g) 0)
              "but it advances the device's own feed-seq")
          (graph-db::persist-peer-pull-cursor 42 g)                        ; a pull's hub frontier
          (is (= 42 (graph-db::load-peer-pull-cursor g)) "pull-cursor advances to the frontier")
          (is (< (graph-db::load-highest-transaction-id g) 42)
              "the device's own feed-seq is unaffected by the pull-cursor"))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0)))
        (unwind-protect
             (is (= 42 (graph-db::load-peer-pull-cursor g)) "the pull-cursor survives reopen")
          (close-graph g :snapshot-p nil))))))

(test field-stamps-track-and-recover
  "A locally-authored edit stamps each changed field with (lamport . origin); an
update advances the changed field's stamp but leaves an unchanged field's; the
stamps survive close/reopen (B2b)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) vid)
      (let ((g (make-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (let ((*graph* g))
          (setq vid (id (with-transaction () (make-g-person :name "A" :age 7)))) ; lamport 1
          (is (= 1 (graph-db::node-field-stamp g vid :name)) "create stamps :name")
          (is (= 1 (graph-db::node-field-stamp g vid :age))  "create stamps :age")
          (multiple-value-bind (lam org) (graph-db::node-field-stamp g vid :name)
            (declare (ignore lam))
            (is (equalp org *lamport-origin*) "stamp origin is the graph origin"))
          ;; update only :age -> its stamp advances, :name's does not
          (with-transaction ()
            (let ((v (copy (lookup-vertex vid)))) (setf (slot-value v 'age) 8) (save v))) ; lamport 2
          (is (= 2 (graph-db::node-field-stamp g vid :age)) "update advances the changed field")
          (is (= 1 (graph-db::node-field-stamp g vid :name)) "the unchanged field keeps its stamp"))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *integration-graph-name* path
                           :peer-role :device :origin-id *lamport-origin*
                           :peer-host "localhost" :replication-port 0)))
        (unwind-protect
             (progn
               (is (= 2 (graph-db::node-field-stamp g vid :age)) "reopen recovers :age stamp")
               (is (= 1 (graph-db::node-field-stamp g vid :name)) "reopen recovers :name stamp"))
          (close-graph g :snapshot-p nil))))))
