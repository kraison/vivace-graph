(in-package :graph-db)

;;; Mapping graph nodes
(defun map-all-nodes (fn graph)
  "Call FN for each node (vertex or edge) in GRAPH. "
  (map-vertices fn graph)
  (map-edges fn graph))

(defun map-node-addresses (fn graph)
  "Call FN with the heap address of each node in GRAPH."
  (map-all-nodes (lambda (node)
                   (let ((data-pointer (data-pointer node)))
                     (unless (zerop data-pointer)
                       (funcall fn data-pointer))))
                 graph))

;;; Mapping graph index lists
(defun map-index-list-addresses (fn index-list)
  "Call FN with the heap address of each pcons in INDEX-LIST."
  (let ((address (index-list-head index-list)))
    (loop
       (when (zerop address)
         (return))
       (funcall fn address)
       (let ((pcons (deserialize-pcons index-list address)))
         (setf address (%pcons-cdr pcons))))))

(defun map-edge-index-list-addresses (fn graph)
  "Call FN with each index list heap address involved in the edge
indexes (ve-index-in, ve-index-out, vev-index) of GRAPH."
  (let ((in (ve-index-table (ve-index-in graph)))
        (out (ve-index-table (ve-index-out graph)))
        (vev (vev-index-table (vev-index graph))))
    (flet ((map-table (fn table)
             (map-lhash (lambda (cons)
                          (let ((index-list (cdr cons)))
                            (map-index-list-addresses fn index-list)))
                        table)))
      (map-table fn in)
      (map-table fn out)
      (map-table fn vev))))

(defun map-type-index-list-addresses (fn graph)
  "Call FN with the heap address of the index lists of each type index
table index (edge-index, vertex-index) of GRAPH."
  #+ecl
  ;; Lazy type-index (#46): the ECL cache is sparse (only touched types are
  ;; materialized), so maphashing it would miss the index-lists of types not yet
  ;; referenced this session and free their still-live nodes.  Enumerate every
  ;; ASSIGNED type-id instead -- ids are dense from 0 (0 = generic) up to the
  ;; schema's next-*-id -- materializing each via GET-TYPE-INDEX-LIST.
  (let ((schema (schema graph)))
    (flet ((map-idx (idx next-id)
             (dotimes (tid next-id)
               (let ((il (get-type-index-list idx tid)))
                 (when il (map-index-list-addresses fn il))))))
      (map-idx (edge-index graph) (schema-next-edge-id schema))
      (map-idx (vertex-index graph) (schema-next-vertex-id schema))))
  #-ecl
  (let ((edge-table (type-index-cache (edge-index graph)))
        (vertex-table (type-index-cache (vertex-index graph))))
    (flet ((map-table (fn table)
             (maphash (lambda (key index-list)
                        (declare (ignore key))
                        (map-index-list-addresses fn index-list))
                      table)))
      (map-table fn edge-table)
      (map-table fn vertex-table))))

(defun map-all-index-list-addresses (fn graph)
  "Call FN with the heap address of the elements of all index lists
used for indexing in GRAPH."
  (map-type-index-list-addresses fn graph)
  (map-edge-index-list-addresses fn graph))

;;; Mapping graph view skip lists -- but view skip lists use their own
;;; heap, not GC'd for now.
(defun map-view-skip-lists (fn graph)
  "Call FN for each view skip list in FN."
  (let ((views-table (views graph)))
    (maphash (lambda (class-name view-group)
               (declare (ignore class-name))
               (maphash (lambda (slot-name view)
                          (declare (ignore slot-name))
                          (let ((skip-list (view-skip-list view)))
                            (funcall fn skip-list)))
                        (view-group-table view-group)))
             views-table)))

(defun map-all-view-skip-list-addresses (fn graph)
  "Call FN for each heap address in each view skip list of GRAPH."
  (map-view-skip-lists
   (lambda (skip-list)
     (map-skip-list (lambda (skip-list-node)
                      (funcall fn
                               (%sn-addr skip-list-node)))
                    skip-list))
   graph))

;;; Putting it all together
(defun map-all-heap-allocations (fn graph)
  "Call FN for each 'raw' heap address in GRAPH, determined only by
walking the heap."
  (map-memory (lambda (allocation-data-offset size free?)
                (declare (ignore size free?))
                (funcall fn allocation-data-offset))
              (heap graph)))

(defun map-version-chain-addresses (fn graph)
  "Call FN with the heap address of every archived MVCC version head AND its
retained data block, reachable via PREV-POINTER chains from the live nodes.
Without this, GC-HEAP (which roots only live heads) would free the retained
version chain on the next OPEN-GRAPH, leaving live PREV-POINTERs dangling."
  (map-all-nodes
   (lambda (node)
     (let ((p (prev-pointer node)))
       (loop
         (when (zerop p) (return))
         (funcall fn p)                       ; the archived head block
         (multiple-value-bind (d w hw tiw vw vew vvw type-id rev data-ptr epoch prev off)
             (deserialize-node-head (heap graph) p)
           (declare (ignore d w hw tiw vw vew vvw type-id rev epoch off))
           (when (> data-ptr 0)
             (funcall fn data-ptr))            ; the archived version's data block
           (setf p prev)))))
   graph))

(defun map-all-graph-allocations (fn graph)
  "Call FN for each heap address referenced through a data structure
within GRAPH."
  (map-node-addresses fn graph)
  (map-version-chain-addresses fn graph)
  (map-all-index-list-addresses fn graph))

(defun heap-allocation-table (graph)
  "Return a hash table mapping each 'raw' heap address of GRAPH to T."
  (let ((table (make-hash-table)))
    (map-all-heap-allocations (lambda (address)
                                (setf (gethash address table) t))
                              graph)
    table))

(defun graph-allocation-table (graph)
  "Return a hash table mapping each heap address referenced through a
data structure in GRAPH to T. Used for debugging, not used in GC."
  (let ((table (make-hash-table)))
    (map-all-graph-allocations (lambda (address)
                                 (setf (gethash address table) t))
                               graph)
    table))

(defun gc-heap (graph)
  "Garbage-collect the heap of GRAPH by calling FREE on any
allocations that are not referenced through any data structure in
GRAPH."
  ;; A memory-graph has no mmap heap (nodes are live objects, DATA-POINTER 0),
  ;; so heap GC is a no-op -- there is nothing to reclaim and (HEAP GRAPH) is NIL.
  ;; Guard so an explicit GC-HEAP on a memory-graph is safe rather than a nil-heap
  ;; dereference.
  (unless (heap graph)
    (return-from gc-heap nil))
  (log:debug "gc-ing graph database heap.")
  (let ((allocation-table (heap-allocation-table graph))
        (heap (heap graph)))
    (map-all-graph-allocations (lambda (pointer)
                                 (remhash pointer allocation-table))
                               graph)
    (maphash (lambda (pointer _)
               (declare (ignore _))
               (log:debug "Freeing ~A" pointer)
               (free heap pointer))
             allocation-table))
  (log:debug "gc complete"))
