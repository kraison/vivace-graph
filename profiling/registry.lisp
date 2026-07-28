;;;; Subsystem Registry for VivaceGraph Profiling Tool
(in-package #:graph-db/profiler)

(defvar *subsystem-registry* (make-hash-table :test 'eq)
  "Hash table mapping subsystem keyword symbols to list of function symbols.")

(defun register-subsystem-functions (subsystem-key function-symbols)
  "Register a list of function symbols for a given SUBSYSTEM-KEY keyword."
  (let ((clean-list (remove-if-not (lambda (sym)
                                     (and (symbolp sym) (fboundp sym)))
                                   function-symbols)))
    (setf (gethash subsystem-key *subsystem-registry*) clean-list)
    clean-list))

(defun get-subsystem-functions (subsystem-key)
  "Retrieve the list of valid, fbound function symbols for SUBSYSTEM-KEY."
  (if (eq subsystem-key :all)
      (let ((all '()))
        (maphash (lambda (k funcs)
                   (declare (ignore k))
                   (setf all (union all funcs)))
                 *subsystem-registry*)
        all)
      (gethash subsystem-key *subsystem-registry*)))

(defun list-subsystems ()
  "Return a list of all registered subsystem keywords."
  (alexandria:hash-table-keys *subsystem-registry*))

(defun all-subsystems ()
  "Return all registered subsystem keys."
  (list-subsystems))

(defun init-default-subsystem-registry ()
  "Populate default subsystem function bindings across all vivace-graph layers."
  (clrhash *subsystem-registry*)
  
  ;; 1. MMap & Memory Storage
  (register-subsystem-functions
   :mmap-storage
   '(graph-db::get-byte
     graph-db::set-byte
     graph-db::get-bytes
     graph-db::set-bytes
     graph-db::%get-byte
     graph-db::%set-byte
     graph-db::allocate-memory
     graph-db::free-memory
     graph-db::read-uint64-from-seq
     graph-db::write-uint64-to-seq))

  ;; 2. Serialization & Encoding
  (register-subsystem-functions
   :serialization
   '(graph-db::serialize
     graph-db::deserialize
     graph-db::extract-length
     graph-db::extract-all-subseqs
     graph-db::serialize-raw-bytes
     graph-db::view-key-serialize
     graph-db::view-key-deserialize
     graph-db::%octets-to-string-fast))

  ;; 3. Skip List Index
  (register-subsystem-functions
   :skip-list
   '(graph-db::make-skip-list
     graph-db::read-skip-node
     graph-db::read-skip-node-bytes
     graph-db::%find-in-skip-list
     graph-db::add-to-skip-list
     graph-db::remove-from-skip-list
     graph-db::lock-skip-node
     graph-db::unlock-skip-node))

  ;; 4. B+ Tree Index
  (register-subsystem-functions
   :bplus-tree
   '(graph-db::make-bplus-tree
     graph-db::%bpt-read-page
     graph-db::%bpt-decode-page
     graph-db::%bpt-decode-page-entry
     graph-db::%bpt-page-bsearch
     graph-db::%bpt-descend-to-leaf
     graph-db::%bpt-leaf-at
     graph-db::bpt-insert
     graph-db::bpt-remove
     graph-db::bpt-find))

  ;; 5. Core Graph Storage & Node/Edge Lookup
  (register-subsystem-functions
   :graph-core
   '(graph-db::make-graph
     graph-db::open-graph
     graph-db::close-graph
     graph-db::lookup-vertex
     graph-db::lookup-edge
     graph-db::lookup-node
     graph-db::make-vertex
     graph-db::make-edge
     graph-db::save
     graph-db::delete-node
     graph-db::mark-deleted))

  ;; 6. Transactions & Concurrency Engine
  (register-subsystem-functions
   :transactions
   '(graph-db::call-with-transaction
     graph-db::validate
     graph-db::%commit
     graph-db::%rollback
     graph-db::apply-transaction
     graph-db::overlapping-transactions
     graph-db::object-sets-intersect-p
     graph-db::add-to-object-set
     graph-db::read-object
     graph-db::write-object))

  ;; 7. Views System
  (register-subsystem-functions
   :views
   '(graph-db::make-view
     graph-db::map-view
     graph-db::reduce-view
     graph-db::add-to-view
     graph-db::remove-from-view
     graph-db::regenerate-view
     graph-db::invoke-view))

  ;; 8. Spatial Index
  (register-subsystem-functions
   :spatial
   '(graph-db::make-spatial-index
     graph-db::open-spatial-index
     graph-db::spatial-index-insert
     graph-db::spatial-index-remove
     graph-db::spatial-index-query-bbox
     graph-db::find-nodes-within
     graph-db::find-nodes-near
     graph-db::find-nodes-intersecting))

  ;; 9. GEOS C Bridge Integration
  (register-subsystem-functions
   :geos
   '(graph-db::geometry-intersects-p
     graph-db::geometry-contains-geometry-p
     graph-db::geometry-make-valid
     graph-db::geometry-valid-p
     graph-db::geometry-distance-exact))

  ;; 10. Prolog Query Solver
  (register-subsystem-functions
   :prolog
   '(graph-db::prolog-compile-query
     graph-db::run-prolog-query
     graph-db::resolve-functor
     graph-db::deref-var))

  ;; 11. Replication Transport
  (register-subsystem-functions
   :replication
   '(graph-db::apply-transaction-writes
     graph-db::stream-transaction-log
     graph-db::merge-peer-field)))

;; Initialize on load
(init-default-subsystem-registry)
