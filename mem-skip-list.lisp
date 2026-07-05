(in-package :graph-db)

;;;; In-RAM skip list (the memory-graph's `mem-ordered-map`).
;;;;
;;;; A fresh, self-contained ordered map for the memory backend's views and
;;;; spatial index.  It deliberately reuses the on-disk skip list's SKIP-NODE
;;;; struct (so %SN-KEY / %SN-VALUE and every consumer -- map-view,
;;;; spatial-index-query-bbox -- work unchanged) and the generic cursor entry
;;;; points (make-cursor / make-range-cursor / cursor-next), but the nodes live
;;;; in RAM: a node's POINTERS slot holds DIRECT node references, not heap
;;;; addresses.  So there is no heap, no serialization, no node cache.
;;;;
;;;; Concurrency: because an in-RAM forward pointer is a single Lisp slot (an
;;;; atomic word read), the torn-multi-byte-read problem that forced the on-disk
;;;; list's CAS / SEGV-retry / per-node-lock / marked/fully-linked machinery does
;;;; NOT exist here.  A memory-graph is single-writer, so a plain per-list
;;;; reader/writer lock (shared reads, exclusive writes) is correct on every
;;;; implementation and uncontended in practice -- the same rw-lock shape the
;;;; on-disk list already uses on ECL, minus all the lock-free scaffolding.
;;;;
;;;; cl-skip-list (github.com/kraison/cl-skip-list) is the algorithm reference,
;;;; but its MCAS/CAS concurrency is SBCL-only and dropped here.

(alexandria:define-constant +mem-skip-list-max-level+ 32 :test '=
  :documentation "Enough for ~2^32 elements; keeps head/pred arrays small.")

(defstruct (mem-skip-list (:constructor %make-mem-skip-list)
                          (:predicate mem-skip-list-p)
                          (:print-function
                           (lambda (sl s d) (declare (ignore d))
                             (format s "#<MEM-SKIP-LIST ~A entries>"
                                     (mem-skip-list-count sl)))))
  head
  tail
  (max-level +mem-skip-list-max-level+ :type (integer 1 255))
  comparison           ; (a b) -> generalized boolean : true when a strictly < b
  key-equal            ; (a b) -> generalized boolean
  (value-equal #'equalp)
  (duplicates-allowed-p nil)
  (count 0 :type fixnum)
  (lock (make-rw-lock)))

(defun make-mem-skip-list (&key key-comparison key-equal (value-equal #'equalp)
                             head-key head-value tail-key tail-value
                             duplicates-allowed-p
                             (max-level +mem-skip-list-max-level+))
  "Create an empty in-RAM skip list.  KEY-COMPARISON is strict less-than and
KEY-EQUAL is equality over keys (mirroring make-skip-list); HEAD-KEY / TAIL-KEY
are the sentinel keys (HEAD-KEY sorts before, TAIL-KEY after, every real key)."
  (let* ((tail (%make-skip-node :key tail-key :value tail-value :level 0
                                :pointers (make-array 0) :tail-p t))
         (head (%make-skip-node :key head-key :value head-value :level max-level
                                :pointers (make-array max-level :initial-element tail)
                                :head-p t)))
    (%make-mem-skip-list :head head :tail tail :max-level max-level
                         :comparison key-comparison :key-equal key-equal
                         :value-equal value-equal
                         :duplicates-allowed-p duplicates-allowed-p)))

(declaim (inline %msn-forward))
(defun %msn-forward (node level)
  "The direct successor reference of NODE at LEVEL."
  (svref (%sn-pointers node) level))

;;; Search core (lock-free; caller holds the list's read or write lock).  Fills
;;; PREDS/SUCCS[level] with the predecessor / first-node->=KEY at each level.
;;; With VALUE-P, a node is only a match when its VALUE also equals VALUE, so
;;; PREDS/SUCCS position on the specific (key,value) node among duplicates
;;; (mirrors the on-disk %FIND-KV-IN-SKIP-LIST).
(defun %mem-find (sl key preds succs &optional value value-p)
  (let ((cmp (mem-skip-list-comparison sl))
        (keq (mem-skip-list-key-equal sl))
        (veq (mem-skip-list-value-equal sl))
        (tail (mem-skip-list-tail sl))
        (pred (mem-skip-list-head sl)))
    (loop for level from (1- (mem-skip-list-max-level sl)) downto 0 do
      (let ((curr (%msn-forward pred level)))
        (loop while (and (not (eq curr tail))
                         (or (funcall cmp (%sn-key curr) key)
                             (and value-p
                                  (funcall keq key (%sn-key curr))
                                  (not (funcall veq value (%sn-value curr))))))
              do (setq pred curr
                       curr (%msn-forward pred level)))
        (setf (aref preds level) pred
              (aref succs level) curr)))
    (values preds succs)))

(defmethod add-to-skip-list ((sl mem-skip-list) key value)
  "Insert KEY->VALUE.  With duplicates disallowed, a second identical key is a
no-op returning NIL (matching the on-disk list's non-erroring behaviour)."
  (with-write-lock ((mem-skip-list-lock sl))
    (let* ((max (mem-skip-list-max-level sl))
           (preds (make-array max))
           (succs (make-array max)))
      (%mem-find sl key preds succs)
      (let ((succ (aref succs 0)))
        (when (and (not (mem-skip-list-duplicates-allowed-p sl))
                   (not (eq succ (mem-skip-list-tail sl)))
                   (funcall (mem-skip-list-key-equal sl) key (%sn-key succ)))
          (return-from add-to-skip-list nil)))
      (let* ((level (random-level max))
             (node (%make-skip-node :key key :value value :level level
                                    :pointers (make-array level :initial-element nil))))
        (dotimes (l level)
          (setf (svref (%sn-pointers node) l) (aref succs l))
          (setf (svref (%sn-pointers (aref preds l)) l) node))
        (incf (mem-skip-list-count sl))
        node))))

(defmethod remove-from-skip-list ((sl mem-skip-list) key
                                  &optional (value nil value-p))
  "Remove the node with KEY (and VALUE, if given) -- one node; with duplicates,
the leftmost match.  Returns the removed node or NIL."
  (with-write-lock ((mem-skip-list-lock sl))
    (let* ((max (mem-skip-list-max-level sl))
           (preds (make-array max))
           (succs (make-array max)))
      (%mem-find sl key preds succs value value-p)
      (let ((target (aref succs 0)))
        (when (and (not (eq target (mem-skip-list-tail sl)))
                   (funcall (mem-skip-list-key-equal sl) key (%sn-key target))
                   (or (not value-p)
                       (funcall (mem-skip-list-value-equal sl) value (%sn-value target))))
          ;; Unsplice: at every level where PRED still points at TARGET, bypass it.
          (dotimes (level (%sn-level target))
            (when (eq (%msn-forward (aref preds level) level) target)
              (setf (svref (%sn-pointers (aref preds level)) level)
                    (%msn-forward target level))))
          (decf (mem-skip-list-count sl))
          target)))))

;;; Cursors.  The generic cursor entry points (make-cursor / make-range-cursor /
;;; cursor-next) are shared with the on-disk list, so map-view / spatial queries
;;; consume these unchanged.  Each cursor step takes the read lock (cheap under
;;; single-writer); a node removed mid-scan still forwards correctly (its pointer
;;; slots are left intact on unsplice), so a scan never crashes -- it is simply
;;; not snapshot-isolated, matching the memory-graph's read model.

(defclass mem-sl-cursor (cursor)
  ((node :initarg :node :accessor msc-node)
   (skip-list :initarg :skip-list :accessor msc-sl)))

(defclass mem-sl-range-cursor (mem-sl-cursor)
  ((end :initarg :end :accessor msc-end)))

(defun %mem-cursor-step (c eoc)
  "Return the current node and advance; EOC at the tail."
  (let ((node (msc-node c)))
    (if (or (null node) (eq node (mem-skip-list-tail (msc-sl c))))
        eoc
        (progn (setf (msc-node c) (%msn-forward node 0))
               node))))

(defmethod cursor-next ((c mem-sl-cursor) &optional eoc)
  (with-read-lock ((mem-skip-list-lock (msc-sl c)))
    (%mem-cursor-step c eoc)))

(defmethod cursor-next ((c mem-sl-range-cursor) &optional eoc)
  (with-read-lock ((mem-skip-list-lock (msc-sl c)))
    (let* ((sl (msc-sl c)) (node (msc-node c)) (end (msc-end c)))
      (if (or (null node)
              (eq node (mem-skip-list-tail sl))
              ;; stop once past END (i.e. NOT (node.key <= END))
              (not (or (funcall (mem-skip-list-comparison sl) (%sn-key node) end)
                       (funcall (mem-skip-list-key-equal sl) (%sn-key node) end))))
          eoc
          (%mem-cursor-step c eoc)))))

(defmethod make-cursor ((sl mem-skip-list) &key &allow-other-keys)
  (with-read-lock ((mem-skip-list-lock sl))
    (make-instance 'mem-sl-cursor
                   :node (%msn-forward (mem-skip-list-head sl) 0)
                   :skip-list sl)))

(defmethod make-range-cursor ((sl mem-skip-list) start end &key &allow-other-keys)
  (with-read-lock ((mem-skip-list-lock sl))
    (let* ((max (mem-skip-list-max-level sl))
           (preds (make-array max))
           (succs (make-array max)))
      (%mem-find sl start preds succs)
      ;; succs[0] is the leftmost node with key >= START (captures duplicates).
      (make-instance 'mem-sl-range-cursor
                     :node (aref succs 0) :end end :skip-list sl))))

(defun map-mem-skip-list (fn sl &key collect-p)
  "Call FN on each node in order.  With COLLECT-P, return the list of results."
  (let ((cursor (make-cursor sl)) (acc '()))
    (loop for node = (cursor-next cursor :eoc)
          until (eq node :eoc)
          do (let ((r (funcall fn node))) (when collect-p (push r acc))))
    (when collect-p (nreverse acc))))

;; find-in-skip-list / update-in-skip-list -- used by map-reduce views on the
;; view's ordered map.  Return values mirror the on-disk methods (node + level).
(defmethod find-in-skip-list ((sl mem-skip-list) key &optional preds succs)
  (with-read-lock ((mem-skip-list-lock sl))
    (let ((preds (or preds (make-array (mem-skip-list-max-level sl))))
          (succs (or succs (make-array (mem-skip-list-max-level sl)))))
      (%mem-find sl key preds succs)
      (let ((node (aref succs 0)))
        (if (and (not (eq node (mem-skip-list-tail sl)))
                 (funcall (mem-skip-list-key-equal sl) key (%sn-key node)))
            (values node 0 preds succs)
            (values nil -1 preds succs))))))

(defmethod update-in-skip-list ((sl mem-skip-list) key value &optional old-value)
  "In-place update of the value under KEY (nodes are live, so just SETF the
slot).  Returns the node, or NIL if KEY is absent."
  (declare (ignore old-value))
  (with-write-lock ((mem-skip-list-lock sl))
    (let ((preds (make-array (mem-skip-list-max-level sl)))
          (succs (make-array (mem-skip-list-max-level sl))))
      (%mem-find sl key preds succs)
      (let ((node (aref succs 0)))
        (when (and (not (eq node (mem-skip-list-tail sl)))
                   (funcall (mem-skip-list-key-equal sl) key (%sn-key node)))
          (setf (%sn-value node) value)
          node)))))

(defun mem-skip-list-lookup (sl key &optional value value-p)
  "The value stored under KEY (or the (KEY,VALUE) node's value), or NIL."
  (with-read-lock ((mem-skip-list-lock sl))
    (let* ((max (mem-skip-list-max-level sl))
           (preds (make-array max))
           (succs (make-array max)))
      (%mem-find sl key preds succs value value-p)
      (let ((node (aref succs 0)))
        (when (and (not (eq node (mem-skip-list-tail sl)))
                   (funcall (mem-skip-list-key-equal sl) key (%sn-key node))
                   (or (not value-p)
                       (funcall (mem-skip-list-value-equal sl) value (%sn-value node))))
          (values (%sn-value node) node))))))
