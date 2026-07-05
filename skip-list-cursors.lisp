(in-package :graph-db)

(defclass skip-list-cursor (cursor)
  ((node :initarg :node :accessor skip-list-cursor-node)
   (skip-list :initarg :skip-list :accessor skip-list)))

;;; %CURSOR-NEXT is the lock-free stepping core: the caller must hold the
;;; skip-list read lock (WITH-SL-READ-LOCK), or the list must be uncontended.
;;; The public CURSOR-NEXT wraps it in the read lock for standalone callers; the
;;; lock-held MAP-* scans below call %CURSOR-NEXT directly so the per-step read
;;; lock is not re-entered (a nested read lock can deadlock against a waiting
;;; writer).  The value/key/range :AROUND methods hang off %CURSOR-NEXT so both
;;; entry points pick them up.
(defmethod %cursor-next ((slc skip-list-cursor) &optional eoc)
  (with-slots (node) slc
    (if node
        (if (funcall (%sl-key-equal (skip-list slc))
                     (%sn-key node)
                     (%sn-key (%sl-tail (skip-list slc))))
            eoc
            (let ((result node))
              (setq node (node-forward (skip-list slc) node))
              result))
        eoc)))

(defmethod cursor-next ((slc skip-list-cursor) &optional eoc)
  (with-sl-read-lock ((skip-list slc))
    (%cursor-next slc eoc)))

(defclass skip-list-value-cursor (skip-list-cursor)
  ())

(defmethod %cursor-next :around ((slc skip-list-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (%sn-value result))))

(defclass skip-list-key-cursor (skip-list-cursor)
  ())

(defmethod %cursor-next :around ((slc skip-list-key-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
        eoc
        (%sn-key result))))

(defmethod make-cursor ((sl skip-list) &key cursor
                        (cursor-class 'skip-list-cursor)
                        &allow-other-keys)
  (if cursor
      (progn (setf (skip-list-cursor-node cursor)
                   (node-forward sl (%sl-head sl)))
             cursor)
      (make-instance cursor-class
                     :skip-list sl
                     :node (node-forward sl (%sl-head sl)))))

(defmethod make-values-cursor ((sl skip-list) &key &allow-other-keys)
  (make-cursor sl :cursor-class 'skip-list-value-cursor))

(defmethod make-keys-cursor ((sl skip-list) &key &allow-other-keys)
  (make-cursor sl :cursor-class 'skip-list-key-cursor))

(defclass skip-list-range-cursor (skip-list-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod %cursor-next :around ((slc skip-list-range-cursor) &optional eoc)
  (with-slots (node end) slc
    (if (and node
             (or (funcall (%sl-comparison (skip-list slc)) (%sn-key node) end)
                 (funcall (%sl-key-equal (skip-list slc)) (%sn-key node) end)))
        (call-next-method)
        eoc)))

(defmethod make-range-cursor ((sl skip-list) start end &key &allow-other-keys)
  (let ((preds (make-array (%sl-max-level sl)))
        (succs (make-array (%sl-max-level sl))))
    (multiple-value-bind (node level-found preds succs)
        (find-in-skip-list sl start preds succs)
      ;; SUCCS[0] is the leftmost node whose key >= START.  Start the cursor
      ;; there rather than at NODE: with duplicate keys, FIND-IN-SKIP-LIST
      ;; returns whichever duplicate has the tallest (random) tower -- often a
      ;; middle one -- so starting at NODE would skip the earlier duplicates
      ;; (making skip-list-fetch-all and range queries nondeterministic).
      ;; SUCCS[0] captures every duplicate and is also correct when START
      ;; itself is absent (it is then the first node past START).
      (declare (ignore node level-found preds))
      (when succs
        (make-instance 'skip-list-range-cursor
                       :node (aref succs 0)
                       :end end :skip-list sl)))))

;;; MAP-* hold the read lock for the whole scan (an atomic snapshot) and step the
;;; lock-free %CURSOR-NEXT so the per-step read lock is not re-entered.
(defmethod map-skip-list (fn (sl skip-list) &key collect-p)
  (with-sl-read-lock (sl)
    (let ((cursor (make-cursor sl)) (result nil))
      (do ((node (%cursor-next cursor)
                (%cursor-next cursor)))
          ((null node))
        (if collect-p
            (push (funcall fn node) result)
            (funcall fn node)))
      (when collect-p
        (nreverse result)))))

(defmethod map-skip-list-keys (fn (sl skip-list) &key collect-p)
  (with-sl-read-lock (sl)
    (let ((cursor (make-cursor sl)) (result nil))
      (do ((node (%cursor-next cursor)
                (%cursor-next cursor)))
          ((null node))
        (if collect-p
            (push (funcall fn (%sn-key node)) result)
            (funcall fn (%sn-key node))))
      (when collect-p
        (nreverse result)))))

(defmethod map-skip-list-values (fn (sl skip-list))
  (with-sl-read-lock (sl)
    (let ((cursor (make-values-cursor sl)))
      (do ((val (%cursor-next cursor)
                (%cursor-next cursor)))
          ((null val))
        (funcall fn val)))))

(defmethod skip-list-fetch-all ((sl skip-list) key)
  "Return all values for a key in a skip list where duplicates are allowed."
  ;; No outer lock: MAKE-RANGE-CURSOR and the public CURSOR-NEXT each take the
  ;; read lock themselves (per step), so the scan is concurrency-safe though not
  ;; an atomic snapshot.  (Unused outside tests.)
  (let ((cursor (make-range-cursor sl key key))
        (result nil))
    (if cursor
        (progn
          (do ((node (cursor-next cursor) (cursor-next cursor)))
              ((null node))
            (push (%sn-value node) result))
          (nreverse result))
        nil)))
