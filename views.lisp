(in-package :graph-db)

(defvar *view-rv* nil)

(defstruct view-group
  class-name
  ;;(dirty-p (sb-concurrency:make-gate :open t)) ;; Not currently used
  (table #+sbcl (make-hash-table :test 'eql :synchronized t)
         #+lispworks (make-hash-table :test 'eql :single-thread nil)
         #+ccl (make-hash-table :test 'eql :shared t)
         #+ecl (make-hash-table :test 'eql
                                #+graph-db-ecl-sync-hash :synchronized
                                #+graph-db-ecl-sync-hash t))
  (lock (make-rw-lock)))

(defstruct (view
             (:print-function
              (lambda (v s d)
                (declare (ignore d))
                (format s "#<VIEW '~S' OF '~S' IN '~S'>"
                        (view-name v) (view-class-name v) (view-graph-name v)))))
  name
  class-name
  map-fn
  map-code
  reduce-fn
  reduce-code
  graph-name
  heap
  pointer
  skip-list
  (lock (make-rw-lock))
  lookup-fn
  ;; The (map-code . reduce-code) the cached MAP-FN/REDUCE-FN were built from, so
  ;; COMPILE-VIEW-CODE can skip a recompile that would produce identical functions
  ;; (GH #89).  Transient: SAVE-VIEWS persists an explicit field alist, not the
  ;; struct, so this never reaches disk.
  compiled-from
  (sort-order :lessp))

(defun yield (key value)
  "Emit one index entry, KEY -> VALUE, from inside a view's :MAP lambda.  KEY
determines the view's ordering and is what you query by; VALUE is carried
through (and, for a map-reduce view, fed to the :REDUCE function).  Call it
once per entry you want the node to contribute (zero, one, or many times)."
  ;;(dbg "PUSHING (~S ~S) ON TO *VIEW-RV*" key value)
  (push (list key value) *view-rv*))

(defmethod view-group-exists-p ((group-name symbol) (graph graph))
  (gethash group-name (views graph)))

(defmethod list-views ((graph graph))
  (let ((views nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (maphash (lambda (k1 v1)
                          (declare (ignore k1))
                          (push (cons (view-group-class-name v)
                                      (view-name v1))
                                views))
                        (view-group-table v)))
             (views graph))
    views))

(defmethod lookup-view-group ((group-name symbol) (graph graph))
  (gethash group-name (views graph)))

(defmethod lookup-view-group (group-name (graph null))
  (declare (ignore group-name graph))
  nil)

(defmethod lookup-view-group ((group-name symbol) (graph-name symbol))
  (let ((g (lookup-graph graph-name)))
    (when g
      (lookup-view-group group-name g))))

(defmethod lookup-view-group ((node node) graph)
  (lookup-view-group (class-name (class-of node)) graph))

(defmacro with-write-locked-view-group ((name graph) &body body)
  `(let ((view-group (lookup-view-group ,name ,graph)))
     (if view-group
         (with-write-lock ((view-group-lock view-group))
           ,@body)
         (error 'invalid-view-error :class-name ,name))))

(defmacro with-read-locked-view-group ((name graph) &body body)
  `(let ((view-group (lookup-view-group ,name ,graph)))
     (if view-group
         (with-read-lock ((view-group-lock view-group))
           ,@body)
         (error 'invalid-view-error :class-name ,name))))

(defmethod lock-view-groups ((graph graph) (node node) &key (max-tries 10000))
  (let ((view-groups (lookup-view-groups graph node))
        (sleep 0.001))
    (when view-groups
      ;;(log:debug "LOCKING VIEW GROUPS FOR ~A: ~A" (type-of node) view-groups)
      (let ((tries 0))
        (loop until (> tries max-tries) do
             (incf tries)
             (let ((locks nil))
               (handler-case
                   (progn
                     (dolist (group view-groups)
                       (let ((lock (acquire-write-lock (view-group-lock group)
                                                       :wait-p nil)))
                         (if (rw-lock-p lock)
                             (push lock locks)
                             (error "~A" group)))))
                 (error (c)
                   (declare (ignore c))
                   ;;(log:debug "UNABLE TO ACQUIRE LOCK: ~A" c)
                   ;;(log:debug "UNABLE TO LOCK VIEW GROUPS FOR ~A; TRY ~A. WAITING"
                   ;;node tries)
                   (map nil (lambda (lock)
                              (when (rw-lock-p lock)
                                (release-write-lock lock)))
                        locks)
                   (sleep (* tries sleep)))
                 (:no-error (rv)
                   (declare (ignore rv))
                   ;;(log:debug "LOCKED VIEW GROUPS FOR ~A!" node)
                   (return-from lock-view-groups (nreverse locks))))))
        ;;(log:error "max-tries exceeded trying to lock views for ~A" node)
        (error 'view-lock-error
               :message
               (format nil "max-tries exceeded trying to view-lock ~A" node))))))

(defmacro with-locked-view-groups ((node graph) &body body)
  `(let ((locks nil))
     (unwind-protect
          (progn
            (setq locks (lock-view-groups ,graph ,node))
            (progn ,@body))
       (progn
         (when locks
           ;;(log:debug "UNLOCKING ~A" locks)
           (map nil 'release-write-lock locks)
           ;;(log:debug "UNLOCKED ~A" locks)
           )))))

(defun view-key-serialize (key)
  (let ((payload (serialize (first key))))
    (let ((d (concatenate 'vector (second key) payload)))
      d)))

(defun view-key-deserialize (array)
  (declare (type (array (unsigned-byte 8)) array))
  (let ((node-id (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16) (setf (aref node-id i) (aref array i)))
    (if (and (> (length array) 16) (= (aref array 16) +string+))
        (multiple-value-bind (data-len header-len) (extract-length array :start 16)
          (let ((str (%octets-to-string-fast array :start (+ 16 header-len) :end (+ 16 header-len data-len))))
            (values (list str node-id) (+ 16 header-len data-len))))
        (multiple-value-bind (payload length) (deserialize (subseq array 16))
          (values (list payload node-id) (+ length 16))))))


(defmethod restore-views ((graph graph))
  (let ((views-file (format nil "~A/views.dat" (location graph)))
        (view-table (make-hash-table
                     #+sbcl :synchronized #+sbcl t
                     #+ccl :shared #+ccl t
                     #+lispworks :single-thread #+lispworks nil
                     #+graph-db-ecl-sync-hash :synchronized
                     #+graph-db-ecl-sync-hash t)))
    (when (probe-file views-file)
      (let ((blob (cl-store:restore views-file)))
        (dolist (view-data blob)
          (let* ((view-group-name (car view-data))
                 (view-group (make-view-group :class-name view-group-name)))
            (setf (gethash view-group-name view-table) view-group)
            (dolist (view (rest view-data))
              ;;(log:info "RESTORING ~S VIEW ~S" view-group-name (cdr (assoc :name view)))
              (let* ((view-name (cdr (assoc :name view)))
                     (v (make-view :name view-name
                                   :class-name view-group-name
                                   :graph-name (graph-name graph)
                                   :lookup-fn (cdr (assoc :lookup-fn view))
                                   :map-code (cdr (assoc :map-code view))
                                   :reduce-code (cdr (assoc :reduce-code view))
                                   :heap (indexes graph)
                                   :sort-order (cdr (assoc :sort-order view))
                                   :pointer (cdr (assoc :pointer view)))))
                (if (view-pointer v)
                    ;; Reopen with the backend the index was written with (:backend
                    ;; absent on pre-B+-tree graphs -> defaults to :skip-list).
                    (setf (view-skip-list v)
                          (open-heap-index (or (cdr (assoc :backend view)) :skip-list)
                                           :address (cdr (assoc :pointer view))
                                           :heap (indexes graph)
                                           :comparison (view-index-comparison v)))
                    ;;(log:info "~A didn't have a pointer; cannot restore skip list!" v)
                    )
                (setf (gethash view-name (view-group-table view-group)) v)))))))
    (setf (views graph) view-table)))

(defmethod save-views ((graph graph))
  (with-recursive-lock-held ((views-lock graph))
    (let ((views-file (format nil "~A/views.dat" (location graph)))
          (blob nil))
      (maphash
       (lambda (class-name view-group)
         (let ((views nil))
           (maphash
            (lambda (view-name view)
              (let ((view-alist nil))
                (setq view-alist (acons :name view-name view-alist))
                (setq view-alist (acons :lookup-fn (view-lookup-fn view) view-alist))
                (setq view-alist (acons :map-code (view-map-code view) view-alist))
                (setq view-alist (acons :reduce-code (view-reduce-code view) view-alist))
                (setq view-alist (acons :pointer (view-pointer view) view-alist))
                ;; Persist which ordered-map backend this index uses so RESTORE-VIEWS
                ;; reopens it correctly.  A memory-graph's in-RAM index (and any view
                ;; without a heap index) has no backend tag -> restore defaults to
                ;; :skip-list (harmless: memory-graphs rebuild views on open).
                (setq view-alist
                      (acons :backend
                             (let ((sl (view-skip-list view)))
                               (when (view-index-p sl) (view-index-backend-tag sl)))
                             view-alist))
                (setq view-alist (acons :sort-order (view-sort-order view) view-alist))
                (push view-alist views)))
            (view-group-table view-group))
           (push (cons class-name views) blob)))
       (views graph))
      ;;(log:debug "SAVING VIEWS: ~S" blob)
      (cl-store:store blob views-file)
      blob)))

(defmethod delete-view ((graph graph) (class-name symbol) (view-name symbol))
  "Delete this view's index"
  (with-write-locked-view-group (class-name graph)
    (let ((view (lookup-view graph class-name view-name)))
      (unless view
        (error "Cannot delete view ~A/~A: view does not exist"
               class-name view-name))
      ;;(log:info "Deleting ~A" view)
      (when (view-index-p (view-skip-list view))
        (delete-view-index (view-skip-list view)))
      (remhash view-name (view-group-table
                          (gethash class-name (views graph))))))
  (save-views graph))

(defmethod get-view-table-for-class ((graph graph) (class-name symbol))
  (let ((view-group (gethash class-name (views graph))))
    (unless (view-group-p view-group)
      (setq view-group
            (setf (gethash class-name (views graph))
                  (make-view-group :class-name class-name)))
      (save-views graph))
    (view-group-table view-group)))

(defmethod get-view-table-for-class ((graph-name symbol) (class-name symbol))
  (let ((graph (lookup-graph graph-name)))
    (if graph
        (get-view-table-for-class graph class-name)
        (error "Graph '~S' not loaded" graph-name))))

(defmethod lookup-view ((graph graph) (class-name symbol) (view-name symbol))
  (let ((view-group (lookup-view-group class-name graph)))
    (let ((view (gethash view-name (view-group-table view-group))))
      view)))

(defmethod all-views ((graph graph))
  (let ((views nil))
    (dolist (class-name (all-node-types graph))
      (when (lookup-view-group class-name graph)
        (let ((view-group (gethash class-name (views graph))))
          (when view-group
            (with-locked-hash-table ((view-group-table view-group))
              (loop for view-name being the hash-keys in (view-group-table view-group)
                   do
                   (push (cons class-name view-name) views)))))))
    views))

(defmethod lookup-view-groups ((graph graph) (class-name symbol))
  (let ((ancestor-classes (find-ancestor-classes class-name)))
    (sort
     (delete
      nil
      (delete-duplicates
       (mapcar (lambda (class)
                 (let ((class-name (class-name class)))
                   (when (lookup-view-group class-name graph)
                     (let ((group (gethash class-name (views graph))))
                       group))))
               ancestor-classes)))
     'string-lessp :key 'view-group-class-name)))

(defmethod lookup-view-groups ((graph graph) (node node))
  (lookup-view-groups graph (class-name (class-of node))))

(defmethod lookup-views ((graph graph) (class-name symbol))
  (let ((ancestor-classes (find-ancestor-classes class-name)))
    (delete-duplicates
     (mapcan (lambda (class)
               (let ((class-name (class-name class)))
                 (when (lookup-view-group class-name graph)
                   (let ((view-group (gethash class-name (views graph))))
                     (when view-group
                       (with-locked-hash-table ((view-group-table view-group))
                         (loop for view being the hash-values in (view-group-table view-group)
                            collecting view)))))))
             ancestor-classes))))

(defmethod lookup-views ((graph graph) (node node))
  (lookup-views graph (class-name (class-of node))))

#|
;; Not currently used
(defmethod set-view-group-dirty ((graph graph) (class-name symbol))
  (let ((view-group (lookup-view-group class-name graph)))
    (sb-concurrency:close-gate (view-group-dirty-p view-group))))

;; Not currently used
(defmethod set-view-group-clean ((graph graph) (class-name symbol))
  (let ((view-group (lookup-view-group class-name graph)))
    (sb-concurrency:open-gate (view-group-dirty-p view-group))))
|#

(defmethod compile-view-code ((view view))
  "Compile the view's map/reduce source into functions, memoized on the SOURCE.

ADD-TO-VIEW calls this on EVERY node addition, and it used to READ-FROM-STRING
and EVAL unconditionally -- invoking the reader and the compiler once per view
per node to rebuild functions that had not changed.  That was ~77% of with-view
write time and ~193 KB/node of pure garbage (GH #89).

Keyed on the source rather than on (FUNCTIONP MAP-FN), so a redefined view still
recompiles: the bare functionp guard would cache the first compile forever.

Deliberately unlocked, like %NODE-SLOT-INFO's cache: two threads racing here
compile equal functions and one wins, which is harmless.  What must not happen is
publishing COMPILED-FROM before the functions it describes, so it is set LAST --
otherwise another thread could skip the compile while the functions are stale."
  (let ((key (cons (view-map-code view) (view-reduce-code view))))
    (unless (and (functionp (view-map-fn view))
                 (equal key (view-compiled-from view)))
      (setf (view-map-fn view)
            (eval (read-from-string (view-map-code view))))
      ;; Assigned unconditionally so dropping a view's :REDUCE clears the stale
      ;; function rather than leaving the old one bound.
      (setf (view-reduce-fn view)
            (when (view-reduce-code view)
              (eval (read-from-string (view-reduce-code view)))))
      (setf (view-compiled-from view) key))))

(defun reduce-equal (key1 key2)
  ;;(log:debug "REDUCE-EQUAL ~S < ~S" key1 key2)
  (and (equal (first key1) (first key2))
       (equalp (second key1) (second key2))))

(defun reduce-comp-lessp (key1 key2)
  ;;(log:debug "REDUCE-COMP-LESSP ~S < ~S" key1 key2)
  (cond ((less-than (first key1) (first key2))
         t)
        ((and (equal (first key1) (first key2))
              (key-vector< (second key1) (second key2)))
         t)
        (t nil)))

(defun reduce-comp-greaterp (key1 key2)
  ;;(log:debug "REDUCE-COMP-GREATERP ~S < ~S" key1 key2)
  (cond ((greater-than (first key1) (first key2))
         t)
        ((and (equal (first key1) (first key2))
              (key-vector> (second key1) (second key2)))
         t)
        (t nil)))

(defmethod add-to-view ((graph graph) (view view) (node node))
  "Add node to view."
  ;;(log:debug "Adding ~A to ~A" node view)
  (compile-view-code view)
  (let ((*view-rv* nil))
    ;;(log:debug "ADDING TO ~A" view)
    ;;(log:debug "VIEW: Calling ~S on ~S" (view-map-fn view) node)
    (funcall (view-map-fn view) node)
    ;;(log:debug "VIEW-RV: ~S" *view-rv*)
    (mapcar (lambda (rv)
              (destructuring-bind (key val) rv
                ;;(log:debug "VIEW: Adding ~S:~S to ~S" key val (view-skip-list view))
                (add-to-skip-list (view-skip-list view)
                                  (list key (id node))
                                  val)
                (when (functionp (view-reduce-fn view))
                  (let* ((agg-key (list key +null-key+))
                         (agg-node
                          (find-in-skip-list (view-skip-list view) agg-key)))
                    ;;(log:debug "REDUCE: ADDING TO SL: ~S -> ~S" agg-key agg-node)
                    (if agg-node
                        (let ((agg-val
                               (funcall (view-reduce-fn view)
                                        (list (%sn-key agg-node) key)
                                        (list (%sn-value agg-node) val))))
                          (update-in-skip-list (view-skip-list view)
                                               agg-key agg-val))
                        (add-to-skip-list (view-skip-list view)
                                          agg-key val)))
                  (let* ((agg-key (list +reduce-master-key+ +max-key+))
                         (agg-node
                          (find-in-skip-list (view-skip-list view) agg-key)))
                    ;;(log:debug "REDUCE: ADDING TO SL: ~S -> ~S" agg-key agg-node)
                    (if agg-node
                        (let ((agg-val
                               (funcall (view-reduce-fn view)
                                        (list (%sn-key agg-node) key)
                                        (list (%sn-value agg-node) val))))
                          (update-in-skip-list (view-skip-list view)
                                               agg-key agg-val))
                        (add-to-skip-list (view-skip-list view)
                                          agg-key val)))
                  )))
            *view-rv*)))

(defmethod get-non-aggregate-pairs ((skip-list skip-list) key)
  (let ((keys nil) (values nil))
    (let ((cursor (make-range-cursor skip-list
                                     (list key +null-key+)
                                     (list key +max-key+))))
      (loop for node = (cursor-next cursor :eoc)
         until (eql node :eoc)
           do
           (unless (equalp +null-key+ (second (%sn-key node)))
             (push (first (%sn-key node)) keys)
             (push (%sn-value node) values))))
    (values keys values)))

(defmethod get-all-aggregate-pairs ((skip-list skip-list))
  (let ((keys nil) (values nil))
    (let ((cursor (make-cursor skip-list)))
      (loop for node = (cursor-next cursor :eoc)
         until (eql node :eoc)
         do
           (when (equalp +null-key+ (second (%sn-key node)))
             (push (first (%sn-key node)) keys)
             (push (%sn-value node) values))))
    (values keys values)))

;;; mem-skip-list variants (in-memory backend, #50).  The mem cursor exposes the same
;;; make-range-cursor / make-cursor / cursor-next / %sn-key / %sn-value protocol as the
;;; on-disk skip-list, so these are the on-disk methods above verbatim with the specializer
;;; swapped.  Without them, maintaining a REDUCE (aggregate) view on a memory-graph signals
;;; "no applicable method for get-non-aggregate-pairs on MEM-SKIP-LIST" -- which is what the
;;; mine-action app's eo-find rollup views hit during peer-sync.
(defmethod get-non-aggregate-pairs ((skip-list mem-skip-list) key)
  (let ((keys nil) (values nil))
    (let ((cursor (make-range-cursor skip-list
                                     (list key +null-key+)
                                     (list key +max-key+))))
      (loop for node = (cursor-next cursor :eoc)
         until (eql node :eoc)
         do
           (unless (equalp +null-key+ (second (%sn-key node)))
             (push (first (%sn-key node)) keys)
             (push (%sn-value node) values))))
    (values keys values)))

(defmethod get-all-aggregate-pairs ((skip-list mem-skip-list))
  (let ((keys nil) (values nil))
    (let ((cursor (make-cursor skip-list)))
      (loop for node = (cursor-next cursor :eoc)
         until (eql node :eoc)
         do
           (when (equalp +null-key+ (second (%sn-key node)))
             (push (first (%sn-key node)) keys)
             (push (%sn-value node) values))))
    (values keys values)))

;;; bplus-tree variants (the B+ tree ordered-map backend).  The B+ tree cursor
;;; exposes the SAME make-range-cursor / make-cursor / cursor-next / %sn-key /
;;; %sn-value protocol, so these are the on-disk skip-list methods above verbatim
;;; with the specializer swapped -- needed so a REDUCE (aggregate) view maintained
;;; on a B+ tree-backed graph can roll up its aggregates.
(defmethod get-non-aggregate-pairs ((skip-list bplus-tree) key)
  (let ((keys nil) (values nil))
    (let ((cursor (make-range-cursor skip-list
                                     (list key +null-key+)
                                     (list key +max-key+))))
      (loop for node = (cursor-next cursor :eoc)
         until (eql node :eoc)
         do
           (unless (equalp +null-key+ (second (%sn-key node)))
             (push (first (%sn-key node)) keys)
             (push (%sn-value node) values))))
    (values keys values)))

(defmethod get-all-aggregate-pairs ((skip-list bplus-tree))
  (let ((keys nil) (values nil))
    (let ((cursor (make-cursor skip-list)))
      (loop for node = (cursor-next cursor :eoc)
         until (eql node :eoc)
         do
           (when (equalp +null-key+ (second (%sn-key node)))
             (push (first (%sn-key node)) keys)
             (push (%sn-value node) values))))
    (values keys values)))

(defmethod remove-from-view ((graph graph) (view view) (node node))
  "Remove node from view."
  (compile-view-code view)
  (let ((*view-rv* nil))
    (funcall (view-map-fn view) node)
    ;;(log:debug "VIEW-RV: ~S" *view-rv*)
    (mapcar
     (lambda (rv)
       (destructuring-bind (key val) rv
         (remove-from-skip-list (view-skip-list view) (list key (id node)))
         (when (functionp (view-reduce-fn view))
           (let ((agg-key (list key +null-key+)))
             (remove-from-skip-list (view-skip-list view) agg-key)
             (multiple-value-bind (keys values)
                 (get-non-aggregate-pairs (view-skip-list view) key)
               (when keys
                 (let ((agg-val (funcall (view-reduce-fn view) keys values)))
                   (add-to-skip-list (view-skip-list view) agg-key agg-val)))))
           (let* ((agg-key (list +reduce-master-key+ +max-key+))
                  (agg-node (find-in-skip-list (view-skip-list view) agg-key)))
             (multiple-value-bind (keys values)
                 (get-all-aggregate-pairs (view-skip-list view))
               (when keys
                 (let ((agg-val (funcall (view-reduce-fn view) keys values)))
                   (if agg-node
                       (update-in-skip-list (view-skip-list view) agg-key agg-val)
                       (add-to-skip-list (view-skip-list view) agg-key val)))))))))
     *view-rv*)))

(defmethod %add-to-views ((graph graph) (node node) (class-name symbol))
  (dolist (view (lookup-views graph class-name))
    ;;(log:debug "Adding ~S to view ~S:~S" node class-name (view-name view))
    (add-to-view graph view node)))

(defmethod add-to-views ((graph graph) (node node))
  "Add node to indices for its class's named views"
  (with-locked-view-groups (node graph)
    (%add-to-views graph node (class-name (class-of node)))))
#|
    (dolist (class (find-ancestor-classes (class-of node)))
    (let ((class-name (class-name class)))
      (when (lookup-view-group class-name graph)
        (with-write-locked-view-group (class-name graph)
          (%add-to-views graph node class-name))))))
|#

(defmethod %remove-from-views ((graph graph) (node node) (class-name symbol))
  (dolist (view (lookup-views graph class-name))
    ;;(log:debug "Removing ~S from view ~S:~S" node class-name (view-name view))
    (remove-from-view graph view node)))

(defmethod remove-from-views ((graph graph) (node node))
  "Remove node from indices for its class's named views"
  (with-locked-view-groups (node graph)
    (%remove-from-views graph node (class-name (class-of node)))))
#|
  (dolist (class (find-ancestor-classes (class-of node)))
    (let ((class-name (class-name class)))
      (when (lookup-view-group class-name graph)
        (with-write-locked-view-group (class-name graph)
          (%remove-from-views graph node class-name))))))
|#

(defmethod %update-in-views ((graph graph) (new-node node) (old-node node)
                             (class-name symbol))
  (dolist (view (lookup-views graph class-name))
    (remove-from-view graph view old-node)
    (add-to-view graph view new-node)))

(defmethod update-in-views ((graph graph) (new-node node) (old-node node))
  "Add node to indices for its class's named views"
  (with-locked-view-groups (new-node graph)
    (%update-in-views graph new-node old-node (class-name (class-of new-node)))))
#|
  (dolist (class (find-ancestor-classes (class-of new-node)))
    (let ((class-name (class-name class)))
      (when (lookup-view-group class-name graph)
        (with-write-locked-view-group (class-name graph)
          (%update-in-views graph new-node old-node class-name))))))
|#

(defun view-key-equal (key1 key2)
  (equal (first key1) (first key2)))

(defun view-less-than (key1 key2)
  (less-than (first key1) (first key2)))

;; *INDEX-BACKEND*, MAKE-HEAP-INDEX and OPEN-HEAP-INDEX live in bplus-tree.lisp
;; (loaded before spatial-index.lisp and this file) so views, :unique, and spatial
;; all share one create/open.

(defun view-index-comparison (view)
  (if (eql :greaterp (view-sort-order view))
      'reduce-comp-greaterp 'reduce-comp-lessp))

(defgeneric make-view-skip-list (graph view)
  (:documentation "Create the ordered map backing VIEW.  A normal graph uses a
heap-backed index -- a skip list or (when *INDEX-BACKEND* is :BPLUS-TREE) a
B+ tree, persisted via VIEW-POINTER; a memory-graph overrides this to return an
in-RAM mem-skip-list.")
  (:method ((graph graph) view)
    (make-heap-index (graph-index-backend graph) (indexes graph)
                     (view-index-comparison view))))

(defmethod regenerate-view ((graph graph) (class-name symbol) (view-name symbol))
  "Regenerate this view's index"
  (with-write-locked-view-group (class-name graph)
    (let ((view (lookup-view graph class-name view-name)))
      (unless view
        (error 'invalid-view-error
               :class-name class-name
               :view-name view-name))
      ;; First, if exists, delete the old index (skip list or B+ tree)
      (when (view-index-p (view-skip-list view))
        (delete-view-index (view-skip-list view)))
      ;; Then, create a new index.  MAKE-VIEW-SKIP-LIST dispatches: a heap-backed
      ;; skip-list or B+ tree for a normal graph (persisted via VIEW-POINTER), an
      ;; in-RAM mem-skip-list for a memory-graph (no pointer / heap).
      (let ((sl (make-view-skip-list graph view)))
        (setf (view-skip-list view) sl)
        (when (view-index-p sl)
          (setf (view-pointer view) (view-index-address sl)
                (view-heap view) (indexes graph))))
      (save-views graph)
      (cond ((subtypep class-name 'vertex)
             (map-vertices (lambda (vertex)
                             (add-to-view graph view vertex))
                           graph :vertex-type class-name))
            ((subtypep class-name 'edge)
             (map-edges (lambda (edge)
                          (add-to-view graph view edge))
                        graph :edge-type class-name))
            (t
             (error "~S is not a subtype of either edge or vertex!" class-name)))
      view)))

(defmethod regenerate-all-views ((graph graph))
  (map nil
       (lambda (pair)
         (destructuring-bind (class-name . view-name) pair
           (regenerate-view graph class-name view-name)))
       (all-views graph)))

(defmethod map-view (fn (class-name symbol) (view-name symbol)
                     &key (graph *graph*) key start-key end-key count skip
                       collect-p include-deleted-p write-p)
  "Call FN on entries of the view VIEW-NAME (defined on CLASS-NAME) in GRAPH.
FN receives (key id value).  Restrict to a single :KEY, or to a :START-KEY /
:END-KEY range, and page with :SKIP / :COUNT.  With :COLLECT-P, collect and
return FN's values.  This walks the raw (unreduced) view entries; see
MAP-REDUCED-VIEW for aggregated results and INVOKE-GRAPH-VIEW for the common
high-level lookup."
  (if (lookup-view-group class-name graph)
      (let ((thunk
             (lambda ()
               ;; Resolve nodes (the view's LOOKUP-FN -> LOOKUP-<type> -> LOOKUP-VERTEX)
               ;; from the GRAPH being queried, not the ambient *GRAPH*.  Otherwise
               ;; MAP-VIEW/INVOKE-GRAPH-VIEW with an explicit :GRAPH reads the index
               ;; from that graph but resolves node ids against *GRAPH* -- so querying
               ;; any graph that is not the current *GRAPH* (e.g. right after a reopen,
               ;; or a second graph) looks the node up in the wrong (or a closed) graph
               ;; and hits (VERTEX-TABLE NIL) -> no-applicable-method on LOOKUP-NODE.
               (let ((*graph* graph))
                 (let ((view (lookup-view graph class-name view-name)))
                 (unless view
                   (error 'invalid-view-error
                          :class-name class-name
                          :view-name view-name))
                 (let* ((lookup-fn (view-lookup-fn view))
                        (skip-list (view-skip-list view))
                        ;; The view skip list is sorted per the view's order, and
                        ;; same-key entries are tiebroken by node id in that SAME
                        ;; direction (see reduce-comp-lessp / reduce-comp-greaterp).
                        ;; So the bounds that bracket a key must follow the order:
                        ;; for :greaterp the id sentinels and the open-ended key
                        ;; sentinels are reversed.  Otherwise a :key / :start-key /
                        ;; :end-key lookup on a :greaterp view brackets an empty
                        ;; range and returns nothing (issue #18).
                        (greaterp-p (eql :greaterp (view-sort-order view)))
                        (start-id (if greaterp-p +max-key+ +null-key+))
                        (end-id   (if greaterp-p +null-key+ +max-key+))
                        (start-sentinel (if greaterp-p +max-sentinel+ +min-sentinel+))
                        (end-sentinel   (if greaterp-p +min-sentinel+ +max-sentinel+))
                        (cursor (if (and (null start-key) (null key) (null end-key))
                                    (make-cursor skip-list)
                                    (make-range-cursor skip-list
                                                       (list (cond (key key)
                                                                   (start-key start-key)
                                                                   (t start-sentinel))
                                                             start-id)
                                                       (list (cond (key key)
                                                                   (end-key end-key)
                                                                   (t end-sentinel))
                                                             end-id))))
                        (result nil) (found-count 0) (cursor-count 0))
                   (loop
                      for node = (cursor-next cursor)
                      until (or (null node) (and count (= found-count count)))
                      do
                      ;;(log:debug "~S" node)
                        ;; Count VISIBLE (non-deleted) entries for paging, then SKIP the
                        ;; first SKIP of them.  cursor-count must advance per visible
                        ;; entry -- previously it was incremented only inside the skip
                        ;; guard, so (> 0 skip) was never true and :skip dropped every
                        ;; result.
                        (let ((pnode (funcall lookup-fn (second (%sn-key node)))))
                          (unless (or include-deleted-p (null pnode) (deleted-p pnode))
                            (incf cursor-count)
                            (when (or (null skip) (> cursor-count skip))
                              (incf found-count)
                              (if collect-p
                                  (push (funcall fn
                                                 (first (%sn-key node))
                                                 (second (%sn-key node))
                                                 (%sn-value node))
                                        result)
                                  (funcall fn
                                           (first (%sn-key node))
                                           (second (%sn-key node))
                                           (%sn-value node)))))))
                   (when collect-p
                     (values (nreverse result) found-count))))))))
        (if write-p
            (with-write-locked-view-group (class-name graph)
              (funcall thunk))
            (with-read-locked-view-group (class-name graph)
              (funcall thunk))))
      (error 'invalid-view-error
             :class-name class-name
             :view-name view-name)))

(defun default-map-fn (key id val)
  (list (cons :key key) (cons :id id) (cons :value val)))

(defmethod map-reduced-view (fn (class-name symbol) (view-name symbol)
                             &key (graph *graph*) start-key end-key count
                               skip collect-p)
  "Call FN on the aggregated entries of the map-reduce view VIEW-NAME (defined
on CLASS-NAME) in GRAPH.  FN receives (key id reduced-value), one call per
distinct key, where REDUCED-VALUE is the output of the view's :REDUCE function.
Supports :START-KEY/:END-KEY range, :SKIP/:COUNT paging, and :COLLECT-P."
  (if (lookup-view-group class-name graph)
      (with-read-locked-view-group (class-name graph)
        (let ((view (lookup-view graph class-name view-name)))
          (unless view
            (error 'invalid-view-error
                   :class-name class-name
                   :view-name view-name))
          (let* ((skip-list (view-skip-list view))
                 (cursor (make-cursor skip-list))
                 (result nil) (found-count 0) (total-count 0)
                 (comparator (if (eql :greaterp (view-sort-order view))
                                 'greater-than
                                 'less-than)))
            (loop
               for node = (cursor-next cursor)
               while (and node
                          (or (null end-key)
                              (equal (first (%sn-key node)) end-key)
                              (funcall comparator (first (%sn-key node)) end-key)))
               do
                 (when (and (equalp +null-key+ (second (%sn-key node)))
                            (or (null start-key)
                                (or (equal (first (%sn-key node)) start-key)
                                    (funcall comparator start-key (first (%sn-key node))))))
                   (incf total-count)
                   (when (or (null skip) (> total-count skip))
                     (if collect-p
                         (push
                          (funcall fn (first (%sn-key node)) nil (%sn-value node))
                          result)
                         (funcall fn (first (%sn-key node)) nil (%sn-value node)))
                     (incf found-count)))
                 (when (and count (= count found-count))
                   (return)))
            (when collect-p
              (values (nreverse result) found-count)))))
      (error 'invalid-view-error
             :class-name class-name
             :view-name view-name)))

(defmethod invoke-graph-view ((class-name symbol) (view-name symbol)
                              &key (graph *graph*) key start-key end-key count
                                skip group-p (reduce-p t))
  "Query the view VIEW-NAME (defined on CLASS-NAME) in GRAPH and return its
matches as a list of alists, each with keys :KEY, :ID and :VALUE.  This is the
usual high-level lookup.

For a map-only view (or with :REDUCE-P nil), returns the matching entries,
optionally narrowed by :KEY or a :START-KEY/:END-KEY range and paged with
:SKIP/:COUNT.  For a map-reduce view it returns reduced results: the grand
aggregate by default, the per-key aggregate for a given :KEY with :GROUP-P, or
all groups with :GROUP-P alone.  Signals INVALID-VIEW-ERROR if the view does
not exist."
  (if (lookup-view-group class-name graph)
      (with-read-locked-view-group (class-name graph)
        (let ((view (lookup-view graph class-name view-name)))
          (unless view
            (error 'invalid-view-error
                   :class-name class-name
                   :view-name view-name))
          (if (or (null (view-reduce-code view)) (null reduce-p))
              ;; Simple map view
              (map-view 'default-map-fn
                        class-name view-name
                        :key key :count count :skip skip
                        :start-key start-key :end-key end-key
                        :collect-p t :graph graph)
              ;; Reduce view
              (cond ((and group-p key)
                     (let ((node (find-in-skip-list (view-skip-list view)
                                                    (list key +null-key+))))
                       (when node
                         (default-map-fn (first (%sn-key node)) nil (%sn-value node)))))
                    (key
                     (map-view 'default-map-fn
                               class-name view-name
                               :key key :count count :skip skip
                               :collect-p t :graph graph))
                    (group-p
                     (map-reduced-view 'default-map-fn
                                       class-name view-name
                                       :start-key start-key
                                       :end-key end-key
                                       :skip skip :count count
                                       :collect-p t :graph graph))
                    (t
                     (let ((node (find-in-skip-list (view-skip-list view)
                                                    (list +reduce-master-key+
                                                          +max-key+))))
                       (when node
                         (default-map-fn nil nil (%sn-value node)))))))))
      (error 'invalid-view-error
             :class-name class-name
             :view-name view-name)))

#|
(def-view email (customer :offerly)
  (:map
   (lambda (vertex)
     (emit (email vertex) (id vertex)))))

(def-view want-count (in-want-list :offerly)
  (:map
   (lambda (edge)
     (emit (to edge) 1)))
  (:reduce
   (lambda (keys vals)
     (declare (ignore keys))
     (apply '+ vals))))
|#

(defun fully-qualified-expression-string (expression)
  ;;(declare (ignore colonp atp args))
  (let ((*package* (find-package :keyword)))
    (format nil "~S" expression)))

;;; ---------------------------------------------------------------------------
;;; Declarative, idempotent view definition (issue #49).
;;;
;;; DEF-VIEW mirrors the schema two-phase pattern that DEF-VERTEX/DEF-EDGE use
;;; (see DEF-NODE-TYPE / UPDATE-SCHEMA in schema.lisp):
;;;
;;;   Phase 1 -- DEF-VIEW registers a VIEW-SPEC in *SCHEMA-VIEW-METADATA* (no open
;;;     graph required) and, if the graph is already open, reconciles it now.
;;;   Phase 2 -- INSTALL-VIEWS, called at open right after RESTORE-VIEWS, walks the
;;;     registry and reconciles each spec against the restored view.
;;;
;;; Reconciliation keeps an already-persisted index whose definition is unchanged
;;; (an O(1) restart -- RESTORE-VIEWS already reopened its skip-list), rebuilds one
;;; whose :MAP/:REDUCE/sort-order changed (with a LOG:WARN), and builds a brand-new
;;; one.  This replaces the old DEF-VIEW, which required an open graph and rebuilt
;;; the index unconditionally on every load.
;;; ---------------------------------------------------------------------------

(defvar *schema-view-metadata* (make-hash-table)
  "graph-name (symbol) -> list of VIEW-SPECs (newest pushed on the front): the
declarative registry DEF-VIEW writes and INSTALL-VIEWS reconciles at open.")

(defstruct (view-spec (:constructor make-view-spec))
  name class-name graph-name lookup-fn map-code reduce-code (sort-order :lessp))

(defun register-view-spec (spec)
  "Phase 1: record SPEC in the registry.  Duplicates accumulate (like the schema
registry); INSTALL-VIEWS resolves them newest-wins."
  (push spec (gethash (view-spec-graph-name spec) *schema-view-metadata*))
  spec)

(defun view-spec-unchanged-p (spec view)
  "True when the restored VIEW already matches SPEC -- same :MAP/:REDUCE code and
sort order -- so its persisted index can be kept as-is.  The map/reduce code is
stored keyword-package-printed, so STRING= is an exact comparison."
  (and (equal (view-spec-map-code spec) (view-map-code view))
       (equal (view-spec-reduce-code spec) (view-reduce-code view))
       (eql (view-spec-sort-order spec) (view-sort-order view))))

(defun %spec->view (spec graph)
  (make-view :name (view-spec-name spec)
             :class-name (view-spec-class-name spec)
             :graph-name (view-spec-graph-name spec)
             :lookup-fn (view-spec-lookup-fn spec)
             :heap (indexes graph)
             :map-code (view-spec-map-code spec)
             :reduce-code (view-spec-reduce-code spec)
             :map-fn nil :reduce-fn nil
             :sort-order (view-spec-sort-order spec)))

(defmethod install-view ((spec view-spec) (graph graph))
  "Phase-2 reconcile of one registered view SPEC against GRAPH (issue #49): KEEP an
already-persisted index whose definition is unchanged (O(1)); REBUILD it -- with a
LOG:WARN -- when the :MAP/:REDUCE/sort-order changed; BUILD it when the view is new
or has no persisted index.  REGENERATE-VIEW dispatches to the on-disk or in-RAM
skip-list via MAKE-VIEW-SKIP-LIST, so this is backend-agnostic.  Returns the view."
  (let ((class-name (view-spec-class-name spec))
        (view-name (view-spec-name spec))
        (graph-name (view-spec-graph-name spec)))
    ;; Ensure the view-group exists (creates it on this class's first view), so
    ;; WITH-WRITE-LOCKED-VIEW-GROUP has a group to lock.
    (get-view-table-for-class graph class-name)
    (with-write-locked-view-group (class-name graph)
      (let* ((table (view-group-table (lookup-view-group class-name graph)))
             (existing (gethash view-name table)))
        (cond
          ;; KEEP: a live, persisted index whose definition is unchanged -> O(1).
          ;; RESTORE-VIEWS already reopened the skip-list; the view self-compiles
          ;; its map/reduce fns on the next maintenance op, so there is nothing to do.
          ((and existing (view-skip-list existing) (view-spec-unchanged-p spec existing))
           existing)
          ;; REBUILD: the definition changed -- adopt the new code and rescan.
          (existing
           (log:warn "def-view ~S of ~S in ~S changed since last load; regenerating its index."
                     view-name class-name graph-name)
           (setf (view-map-code existing) (view-spec-map-code spec)
                 (view-reduce-code existing) (view-spec-reduce-code spec)
                 (view-sort-order existing) (view-spec-sort-order spec)
                 (view-lookup-fn existing) (view-spec-lookup-fn spec)
                 (view-map-fn existing) nil
                 (view-reduce-fn existing) nil)
           (regenerate-view graph class-name view-name))
          ;; BUILD: a brand-new view (or one whose index did not persist).
          (t
           (setf (gethash view-name table) (%spec->view spec graph))
           (save-views graph)
           (regenerate-view graph class-name view-name)))))))

(defmethod install-views ((graph graph))
  "Phase 2 (issue #49; mirror of UPDATE-SCHEMA): reconcile every VIEW-SPEC
registered for GRAPH against its restored views.  Called at open right after
RESTORE-VIEWS, so the node types the views scan are already instantiated (a
regenerate does MAP-VERTICES/-EDGES + SUBTYPEP on the class).  De-dupes the
push-registry by (class . name), newest-wins."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (spec (gethash (graph-name graph) *schema-view-metadata*))
      (let ((key (cons (view-spec-class-name spec) (view-spec-name spec))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (install-view spec graph))))))

(defmacro def-view (name sort-order parents &body body)
  "Define a view (a secondary index) named NAME over a node type.  Declarative and
idempotent (issue #49): like DEF-VERTEX/DEF-EDGE, it registers a spec and reconciles
against the graph, rebuilding the index ONLY when the definition actually changed.

PARENTS is (CLASS-NAME GRAPH-NAME).  SORT-ORDER is the key comparator, e.g. :LESSP
or :GREATERP.  BODY holds a (:MAP lambda) and optionally a (:REDUCE lambda):
  - the :MAP lambda receives a node and calls YIELD to emit key/value entries;
  - the optional :REDUCE lambda receives (keys values) and aggregates them,
    making this a map-reduce view.

The graph need NOT be open when DEF-VIEW runs -- a view may be co-located with its
DEF-VERTEX/DEF-EDGE and loaded before OPEN-GRAPH, which reconciles it at open time.
If the graph IS already open (the classic MAKE-GRAPH -> DEF-VIEW ordering), the view
is reconciled immediately.  Re-evaluating an UNCHANGED DEF-VIEW (or restarting) does
NOT rebuild the persisted index -- restart is O(1); changing the :MAP/:REDUCE/sort
order rebuilds it and emits a LOG:WARN.  Force a rebuild with REGENERATE-VIEW (one
view), REGENERATE-ALL-VIEWS, or OPEN-GRAPH's :REGENERATE-VIEWS T.

Once defined, the view is maintained incrementally as matching nodes are saved.
Query it with INVOKE-GRAPH-VIEW, MAP-VIEW, or MAP-REDUCED-VIEW.  Example:
  (def-view user-by-username :lessp (user :social-app)
    (:map (lambda (u) (when (username u) (yield (username u) nil)))))"
  (let ((map-code (cadr (assoc :map body)))
        (reduce-code (cadr (assoc :reduce body))))
    `(let ((spec (make-view-spec
                  :name ',name
                  :class-name ',(first parents)
                  :graph-name ',(second parents)
                  :lookup-fn ',(intern (format nil "LOOKUP-~A" (first parents)))
                  :map-code ,(fully-qualified-expression-string map-code)
                  :reduce-code ,(when reduce-code
                                  (fully-qualified-expression-string reduce-code))
                  :sort-order ',sort-order)))
       (register-view-spec spec)
       ;; Reconcile now if the graph is already open; otherwise INSTALL-VIEWS does it
       ;; at open (a view can thus be defined before its graph exists).
       (let ((g (lookup-graph ',(second parents))))
         (when g (install-view spec g)))
       spec)))
