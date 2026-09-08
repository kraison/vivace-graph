;;;; General ordered index under peer replication: the device pull-apply paths
;;;; (APPLY-PEER-CREATE-WRITES / APPLY-PEER-AUTHORED-OP) maintain the secondary
;;;; index, and PEER-PURGE-NODE releases it.  No enforcement -- so simpler than the
;;;; #6 unique peer path (no :origin / conflict machinery).

(in-package #:graph-db/test)

(def-suite peer-index-suite
  :description "General ordered index across peer replication (pull-apply maintenance)."
  :in graph-db-suite)

(in-suite peer-index-suite)

(defparameter *pi-graph-name* :graph-db-peer-index-test)
(defparameter *pi-remote-origin* (id16 8) "A remote (other-device/hub) origin.")

(eval-when (:load-toplevel :execute)
  (setf (gethash *pi-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex pi-item ()
  ((sku  :initarg :sku  :accessor pi-sku  :index t)
   (name :initarg :name :accessor pi-name))
  :graph-db-peer-index-test)

;; Multi-slot: mirrors IX-CLAIM's (ns key rel) shape from index-tests.lisp.
;; The pull-apply paths share APPLY-TX-WRITES-TO-SECONDARY-INDEXES with the
;; local-commit path, so tuple support should be automatic -- proved below
;; rather than assumed (#107).
(def-vertex pi-claim ()
  ((ns  :initarg :ns  :accessor pi-claim-ns)
   (key :initarg :key :accessor pi-claim-key)
   (rel :initarg :rel :accessor pi-claim-rel))
  :graph-db-peer-index-test)

(def-index pi-claim (ns key rel) :graph-db-peer-index-test)

(defmacro with-pi-device ((g) &body body)
  "An on-disk DEVICE peer-graph named *PI-GRAPH-NAME* bound to G and *graph*."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *pi-graph-name* (namestring dir)
                           :peer-role :device :origin-id (id16 3)
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(defun pi-authored-create (graph type data origin &key (lamport 5) (tx-id 9000))
  "An AUTHORED peer-op that CREATEs a TYPE node with DATA, attributed to ORIGIN (as
if pulled from that author).  Returns (values op new-id).

%MAKE-VERTEX, not a bare (MAKE-INSTANCE type ...): see
tests/peer-unique-tests.lisp's PU-AUTHORED-CREATE docstring (GH #135)."
  (let* ((tid (graph-db::node-type-id
               (graph-db::lookup-node-type-by-name type :vertex :graph graph)))
         (nid (gen-id))
         (n (graph-db::%make-vertex :class type :id nid
                                    :type-id tid :revision 0)))
    (setf (graph-db::data n) data)
    (values (graph-db::make-peer-op
             :kind :authored :op-id (graph-db::gen-op-id) :origin origin
             :lamport lamport :tx-id tx-id
             :writes (list (make-instance 'graph-db::tx-create :node n)))
            nid)))

(test authored-pull-indexes-node
  "A node pulled via APPLY-PEER-AUTHORED-OP is entered into the device's secondary
index (findable by INDEX-LOOKUP)."
  (with-pi-device (g)
    (graph-db::apply-peer-authored-op
     g (pi-authored-create g 'pi-item '((:sku . "A-1") (:name . "widget"))
                           *pi-remote-origin*))
    (is (equal '("widget") (mapcar #'pi-name (index-lookup g 'pi-item 'sku "A-1"))))
    (is (null (index-lookup g 'pi-item 'sku "nope")))))

(test state-sync-pull-indexes-node
  "The state-sync path (APPLY-PEER-CREATE-WRITES) also indexes the pulled node."
  (with-pi-device (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name 'pi-item :vertex :graph g)))
           ;; %MAKE-VERTEX (GH #135) -- see PI-AUTHORED-CREATE above.
           (n (graph-db::%make-vertex :class 'pi-item :id (gen-id)
                                      :type-id tid :revision 0)))
      (setf (graph-db::data n) '((:sku . "S-9") (:name . "gadget")))
      (graph-db::apply-peer-create-writes
       g 7777 (list (make-instance 'graph-db::tx-create :node n)) *pi-remote-origin*))
    (is (equal '("gadget") (mapcar #'pi-name (index-lookup g 'pi-item 'sku "S-9"))))))

(test purge-releases-index-entry
  "PEER-PURGE-NODE releases a purged node's index entries."
  (with-pi-device (g)
    (multiple-value-bind (op nid)
        (pi-authored-create g 'pi-item '((:sku . "P-3") (:name . "thing"))
                            *pi-remote-origin*)
      (graph-db::apply-peer-authored-op g op)
      (is (= 1 (length (index-lookup g 'pi-item 'sku "P-3"))) "indexed after pull")
      (graph-db::apply-peer-purge g (list nid))
      (is (null (index-lookup g 'pi-item 'sku "P-3")) "released after purge"))))

;;; --- multi-slot tuple keys, same three call sites (GH #107) ----------------

(test authored-pull-indexes-multi-slot-tuple
  "The AUTHORED pull-apply path (peer-streaming.lisp:818) indexes a pulled
multi-slot tuple, resolvable through INDEX-LOOKUP with the full value list."
  (with-pi-device (g)
    (graph-db::apply-peer-authored-op
     g (pi-authored-create g 'pi-claim
                           '((:ns . "ops") (:key . "e1") (:rel . "at"))
                           *pi-remote-origin*))
    (let ((hits (index-lookup g 'pi-claim '(ns key rel)
                              (list "ops" "e1" "at"))))
      (is (= 1 (length hits)))
      (is (string= "e1" (pi-claim-key (first hits)))))
    (is (null (index-lookup g 'pi-claim '(ns key rel)
                            (list "ops" "nope" "at"))))))

(test state-sync-pull-indexes-multi-slot-tuple
  "The state-sync pull-apply path (peer-streaming.lisp:781) also indexes a
pulled multi-slot tuple."
  (with-pi-device (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name
                  'pi-claim :vertex :graph g)))
           ;; %MAKE-VERTEX (GH #135) -- see PI-AUTHORED-CREATE above.
           (n (graph-db::%make-vertex :class 'pi-claim :id (gen-id)
                                      :type-id tid :revision 0)))
      (setf (graph-db::data n) '((:ns . "sync") (:key . "e9") (:rel . "near")))
      (graph-db::apply-peer-create-writes
       g 7777 (list (make-instance 'graph-db::tx-create :node n))
       *pi-remote-origin*))
    (let ((hits (index-lookup g 'pi-claim '(ns key rel)
                              (list "sync" "e9" "near"))))
      (is (= 1 (length hits)))
      (is (string= "e9" (pi-claim-key (first hits)))))))

(test purge-releases-multi-slot-index-entry
  "PEER-PURGE-NODE releases a purged node's multi-slot index entry too."
  (with-pi-device (g)
    (multiple-value-bind (op nid)
        (pi-authored-create g 'pi-claim
                            '((:ns . "ops") (:key . "p1") (:rel . "at"))
                            *pi-remote-origin*)
      (graph-db::apply-peer-authored-op g op)
      (is (= 1 (length (index-lookup g 'pi-claim '(ns key rel)
                                     (list "ops" "p1" "at"))))
          "indexed after pull")
      (graph-db::apply-peer-purge g (list nid))
      (is (null (index-lookup g 'pi-claim '(ns key rel)
                              (list "ops" "p1" "at")))
          "released after purge"))))

;;; --- counting index on the device (GH #361) ---------------------------
;;;
;;; This file pins the stale/rebuild contract: BOTH peer applies bind
;;; *ADD-TO-INDEXES-UNLESS-PRESENT-P*, so every pull marks the count
;;; maps stale and counts nothing, and the repair is a scan rebuild --
;;; at the next count query, or at CLOSE-GRAPH if none comes first.
;;; A device that stopped marking stale, or a close that saved the
;;; stale maps, turns these tests red and nothing else does.

(defun pi-live-p (node)
  "True unless PI-CLAIM-REL is \"dead\"; the count index's CURRENT-P."
  (not (equal (pi-claim-rel node) "dead")))

(def-count-index pi-claim (ns key) :graph-db-peer-index-test
  :name pi-count :current-p pi-live-p)

(test authored-pull-counts-the-node
  "APPLY-PEER-AUTHORED-OP maintains the device's count index (#361):
the maintenance pass marks the map stale and COUNT-INDEX-LOOKUP
rebuilds it (spec R8)."
  (with-pi-device (g)
    (graph-db::apply-peer-authored-op
     g (pi-authored-create g 'pi-claim
                           '((:ns . "ops") (:key . "e1") (:rel . "at"))
                           *pi-remote-origin*))
    (is (equal '(1 1)
               (multiple-value-list
                (count-index-lookup g 'pi-claim '(ns key) '("ops")))))))

(test a-pulled-retraction-moves-current
  "A TX-UPDATE over the wire whose OLD node is live and whose NEW node
is not moves CURRENT and leaves ALL untouched (#361, spec R8)."
  (with-pi-device (g)
    (multiple-value-bind (op nid)
        (pi-authored-create g 'pi-claim
                            '((:ns . "ops") (:key . "e1") (:rel . "at"))
                            *pi-remote-origin*)
      (graph-db::apply-peer-authored-op g op)
      (let* ((old (lookup-vertex nid :graph g))
             (new (graph-db::%copy old))
             (op2 (progn
                    ;; DATA is a plain slot, not a guarded persistent one
                    ;; (PI-AUTHORED-CREATE sets it the same way); a
                    ;; SETF through the PI-CLAIM-REL accessor signals
                    ;; MUTATING-UNREGISTERED-NODE with no *TRANSACTION*
                    ;; bound, which %COPY (unlike COPY) does not do.
                    (setf (graph-db::data new)
                          '((:ns . "ops") (:key . "e1") (:rel . "dead")))
                    (graph-db::make-peer-op
                     :kind :authored :op-id (graph-db::gen-op-id)
                     :origin *pi-remote-origin* :lamport 6 :tx-id 9001
                     :writes (list (make-instance 'graph-db::tx-update
                                                  :node new
                                                  :old-node old))))))
        (graph-db::apply-peer-authored-op g op2))
      (is (equal '(1 0)
                 (multiple-value-list
                  (count-index-lookup g 'pi-claim '(ns key) '("ops"))))))))

(test a-state-sync-re-pull-does-not-double-count
  "Spec R8 (#361): APPLY-PEER-CREATE-WRITES binds *ADD-TO-INDEXES-
UNLESS-PRESENT-P*, so re-applying the same create leaves the counter
at one (stale flag set twice, one rebuild on the next lookup)."
  (with-pi-device (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name
                  'pi-claim :vertex :graph g)))
           (n (graph-db::%make-vertex :class 'pi-claim :id (gen-id)
                                      :type-id tid :revision 0)))
      (setf (graph-db::data n)
            '((:ns . "ops") (:key . "e1") (:rel . "at")))
      (dotimes (i 2)
        (graph-db::apply-peer-create-writes
         g 7777 (list (make-instance 'graph-db::tx-create :node n))
         *pi-remote-origin*)))
    (is (equal '(1 1)
               (multiple-value-list
                (count-index-lookup g 'pi-claim '(ns key) '("ops")))))))

(test purge-releases-the-counter
  "PEER-PURGE-NODE subtracts the purged node's contribution (#361).
The interleaved lookup rebuilds and clears the stale flag before the
purge, so the final read reflects %COUNT-PURGE's own decrement rather
than a rebuild-from-live-nodes that would mask its absence."
  (with-pi-device (g)
    (multiple-value-bind (op nid)
        (pi-authored-create g 'pi-claim
                            '((:ns . "ops") (:key . "e1") (:rel . "at"))
                            *pi-remote-origin*)
      (graph-db::apply-peer-authored-op g op)
      (is (equal '(1 1)
                 (multiple-value-list
                  (count-index-lookup g 'pi-claim '(ns key) '("ops"))))
          "built before the purge, stale flag now clear")
      (graph-db::apply-peer-purge g (list nid))
      (is (equal '(0 0)
                 (multiple-value-list
                  (count-index-lookup g 'pi-claim '(ns key) '("ops"))))))))

(test a-device-close-after-a-pull-persists-rebuilt-count-maps
  "Review of GH #361 (R8): every pull marks a device's count maps stale
and the flag is NOT persisted, so a pull, a clean close with no count
query between them, and a reopen would restore under-counted maps
marked built -- silently wrong until something else marked them stale.
CLOSE-GRAPH rebuilds first.  The maps are built BEFORE the second pull,
so a missing close-time rebuild leaves a real under-count to read back
rather than an absent sidecar the open would rebuild anyway.  Ablation,
recorded in the task report: without the close-time rebuild the
reopened lookup reads 1 1, one short of what a local commit gives."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *pi-graph-name* path
                           :peer-role :device :origin-id (id16 3)
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (graph-db::apply-peer-authored-op
                g (pi-authored-create
                   g 'pi-claim
                   '((:ns . "ops") (:key . "e1") (:rel . "at"))
                   *pi-remote-origin*))
               (is (equal '(1 1)
                          (multiple-value-list
                           (count-index-lookup g 'pi-claim '(ns key)
                                               '("ops"))))
                   "the first pull's repair built the maps")
               (graph-db::apply-peer-authored-op
                g (pi-authored-create
                   g 'pi-claim
                   '((:ns . "ops") (:key . "e2") (:rel . "at"))
                   *pi-remote-origin* :lamport 6 :tx-id 9001))
               (is (eq t (graph-db::count-indexes-stale-p g))
                   "the second pull marked them stale and counted none"))
          ;; No count query after that pull: the close must repair.
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph *pi-graph-name* path
                           :peer-role :device :origin-id (id16 3)
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(2 2)
                          (multiple-value-list
                           (count-index-lookup g 'pi-claim '(ns key)
                                               '("ops"))))
                   "the reopened maps hold what a local commit would")
               (is (null (graph-db::count-indexes-stale-p g))
                   "and nothing was left to repair"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))
