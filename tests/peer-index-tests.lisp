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
