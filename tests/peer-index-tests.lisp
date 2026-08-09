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
if pulled from that author).  Returns (values op new-id)."
  (let* ((tid (graph-db::node-type-id
               (graph-db::lookup-node-type-by-name type :vertex :graph graph)))
         (nid (gen-id))
         (n (make-instance type :id nid :type-id tid :revision 0)))
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
           (n (make-instance 'pi-item :id (gen-id) :type-id tid :revision 0)))
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
           (n (make-instance 'pi-claim :id (gen-id) :type-id tid :revision 0)))
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
