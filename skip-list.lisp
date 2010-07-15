(in-package :vivace-graph)

;;; Pseudo-random number generator from FreeBSD.  Thanks to Manuel Odendahl <manuel@bl0rg.net>.
(defparameter *random-n*
  (the fixnum (get-internal-real-time))
  "Internal status of the pseudo-random number generator.")

(defun random-seed (seed)
  "Seed the pseudo-random number generator."
  (setf *random-n* seed))

(defun sl-random ()
  "Pseudo-random number generator from FreeBSD, returns NIL 3/4 of the time."
  (declare (optimize (speed 3))
	   (type integer *random-n*))
  (logtest (logand most-positive-fixnum
		   (setf *random-n*
			 (mod (+ (the number (* *random-n* 1103515245)) 12345)
			      2147483648)))
	   (ash 1 (- (integer-length most-positive-fixnum) 1))))

(defconstant +max-level+ (the fixnum 32)
  "Maximum level of skip-list, should be enough for 2^32 elements.")

(defun random-level ()
  "Returns a random level for a new skip-list node, with SL-RANDOM p for each level."
  (declare (optimize speed))
  (do ((level 1 (1+ level)))
      ((or (= level +max-level+)
	   (sl-random)) level)
    (declare (type fixnum level))))

;;; A node is a SIMPLE-VECTOR containing KEY, VALUE, LEVEL and the forward pointers
(defconstant +skip-node-key+ 0)
(defconstant +skip-node-value+ 1)
(defconstant +skip-node-forward+ 2)
(defconstant +skip-node-level+ 3)

(defmacro skip-node-key (node)
  `(svref (the simple-vector ,node) +skip-node-key+))
(defmacro skip-node-value (node)
  `(mcas-read (the simple-vector ,node) +skip-node-value+))
(defmacro skip-node-forward (node)
  `(mcas-read (the simple-vector ,node) +skip-node-forward+))
(defmacro skip-node-level (node)
  `(svref (the simple-vector ,node) +skip-node-level+))

(defun make-skip-node (key value size &key initial-element)
  (let ((node (make-array 4 :initial-element initial-element)))
    (setf (svref node +skip-node-key+)     key
	  (svref node +skip-node-value+)   value
	  (svref node +skip-node-forward+) (make-array size :initial-element nil)
	  (svref node +skip-node-level+)   size)
    node))

(defun make-head (&key initial-element)
  (make-skip-node :header nil +max-level+ :initial-element initial-element))

(defstruct (skip-list
	     (:predicate skip-list?)
	     (:print-function print-skip-list))
  (head (make-head))
  (key-equal #'equal)
  (value-equal #'equal)
  (value-sort #'less-than)
  (greater-than #'greater-than)
  (duplicates-allowed? nil) ;; Can be: nil, t, and :as-list
  (length 0 :type (UNSIGNED-BYTE 64)))

(defun print-skip-list (sl stream depth)
  (declare (ignore depth))
  (format stream "#<SKIP-LIST OF LENGTH ~A, EQUAL-FUNC: ~A, DUPLICATES ~A>" 
	  (skip-list-length sl) (skip-list-key-equal sl)
	  (if (skip-list-duplicates-allowed? sl) "ALLOWED" "NOT ALLOWED")))

(defmethod skip-list-search ((sl skip-list) key)
  (let ((start-node (skip-list-head sl)))
    (let ((x start-node) (y nil) 
	  (left-list  (make-array +max-level+ :initial-element nil))
	  (right-list (make-array +max-level+ :initial-element nil)))
      (loop for level from (1- (skip-node-level start-node)) downto 0 do
	   (loop
	      (setq y (mcas-read (skip-node-forward x) level))
	      (if (or (null y)
		      (funcall (skip-list-key-equal sl) key (skip-node-key y)) 
		      (funcall (skip-list-greater-than sl) key (skip-node-key y)))
		  (return)
		  (setq x y)))
	   (setf (svref left-list  level) x
		 (svref right-list level) y))
      (values left-list right-list))))

(defmethod skip-list-empty? ((sl skip-list))
  (= (skip-list-length sl) 0))

(defun node-forward (node)
  (svref (skip-node-forward node) 0))

(defmethod skip-list-to-list ((sl skip-list))
  (let ((node (skip-list-head sl)))
    (loop for next = (node-forward node) then (node-forward next)
	  while next
	  collect (list (skip-node-key next) (skip-node-value next)))))

(defmethod skip-list-lookup ((sl skip-list) key)
  (multiple-value-bind (left-list right-list) (skip-list-search sl key)
    (declare (ignore left-list))
    (if (and (svref right-list 0) 
	     (funcall (skip-list-key-equal sl) key (skip-node-key (svref right-list 0))))
	(skip-node-value (svref right-list 0))
	nil)))

(defmethod skip-list-add ((sl skip-list) key value &key replace?)
  (let ((new-node (make-skip-node key value (random-level))))
    (multiple-value-bind (left-list right-list) (skip-list-search sl key)
      (if (and (svref right-list 0) 
	       (funcall (skip-list-key-equal sl) key (skip-node-key (svref right-list 0))))
	  (cond (replace?
		 ;; FIXME: implement w/ cas
		 ())
		((skip-list-duplicates-allowed? sl)
		 (let ((success? nil))
		   (loop until (eq success? +mcas-succeeded+) do
			(setq success?
			      (with-mcas (:equality #'equal)
				(let* ((node (svref right-list 0))
				       (items (skip-node-value node))
				       (new-items (copy-seq items)))
				  (setq new-items 
					(sort (pushnew 
					       value new-items 
					       :test #'(lambda (x y)
							 (funcall (skip-list-value-equal sl) x y)))
					      #'(lambda (x y) 
						  (funcall (skip-list-value-sort sl) x y))))
				  (mcas-set node +skip-node-value+ items new-items)))))
		   success?))
		(t 
		 (error "Skip list already has key ~A with value ~A." 
			key (skip-node-value (svref right-list 0)))))
	  (let ((success? nil))
	    (loop until (eq success? +mcas-succeeded+) do
		 (setq success?
		       (with-mcas (:equality #'equal)
			 (dotimes (i (skip-node-level new-node))
			   (setf (svref (skip-node-forward new-node) i) (svref right-list i))
			   (mcas-set (skip-node-forward (svref left-list i)) i
				     (svref right-list i)
				     new-node)))))
	    (sb-ext:atomic-incf (skip-list-length sl))
	    success?)))))

(defmethod skip-list-delete ((sl skip-list) key &optional value)
  (multiple-value-bind (left-list right-list) (skip-list-search sl key)
    (let ((x (svref right-list 0)))
      (if (or (null x) (not (equal (skip-node-key x) key)))
	  nil
	  (let ((old-value (skip-node-value x)))
	    (if (eq nil old-value)
		nil
		(let ((success?
		       (with-mcas (:equality #'equal)
			 (loop for i from 0 to (1- (skip-node-level x)) do
			      (let ((next-node (mcas-read (skip-node-forward x) i)))
				(if (and next-node
					 (funcall (skip-list-greater-than sl) 
						  (skip-node-key x) (skip-node-key next-node)))
				    nil
				    (progn
				      (mcas-set (skip-node-forward (svref left-list i)) i
						x
						next-node)
				      (mcas-set (skip-node-forward x) i
						next-node
						(svref left-list i))))))
			 (mcas-set x +skip-node-value+ old-value nil))))
		  (when (eq +mcas-succeeded+ success?)
		    (sb-ext:atomic-decf (skip-list-length sl)))
		  success?)))))))
			 
(defun skip-list-test (&optional (size 500))
  (time (let ((sl (make-skip-list)))
	  (dotimes (i size)
	    (skip-list-add sl (format nil "K~A" i) (format nil "V~A" i)))
	  (dotimes (i size)
	    (unless (equal (skip-list-lookup sl (format nil "K~A" i))
			   (format nil "V~A" i))
	      (error "Could not find K~A in skip-list." i)))
	  (dotimes (i 10)
	    (let ((key (format nil "K~A" (random 500))))
	      (format t "Deletion of ~A/~A -> ~A~%" 
		      key (skip-list-lookup sl key) (skip-list-delete sl key))
	      (format t "Deleted ~A: ~A~%" key (skip-list-lookup sl key))))
	  (format t "~A~%" (skip-list-to-list sl)))))
  
;;; cursors
(defclass skip-list-cursor ()
  ((node :initarg :node :accessor skip-list-cursor-node)))

(defmethod sl-cursor-next ((slc skip-list-cursor) lt-func &optional eoc)
  (with-slots (node) slc
    (if node
	(let ((result (list (skip-node-key node)
			    (skip-node-value node))))
	  (setf node (node-forward node))
	  result)
	eoc)))

(defmethod sl-cursor-prev ((slc skip-list-cursor) &optional eoc)
  (declare (ignore eoc))
  (error "Can not move backward in skip-list"))

(defclass skip-list-value-cursor (skip-list-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skip-list-value-cursor) lt-func &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
	eoc
	(second result))))

(defclass skip-list-key-cursor (skip-list-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skip-list-key-cursor) lt-func &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
	eoc
	(first result))))

(defmethod skip-list-cursor ((sl vector) &key cursor (class 'skip-list-cursor))
  (if cursor
      (progn (setf (skip-list-cursor-node cursor)
		   (node-forward (skip-list-header sl)))
	     cursor)
      (make-instance class :node (node-forward (skip-list-header sl)))))

(defmethod skip-list-values-cursor ((sl vector))
  (skip-list-cursor sl :class 'skip-list-value-cursor))

(defmethod skip-list-keys-cursor ((sl vector))
  (skip-list-cursor sl :class 'skip-list-key-cursor))

(defclass skip-list-range-cursor (skip-list-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod sl-cursor-next :around ((slc skip-list-range-cursor) lt-func &optional eoc)
  (with-slots (node end) slc
    (if (and node (funcall lt-func (skip-node-key node) end))
	(call-next-method)
	eoc)))

(defmethod skip-list-range-cursor ((sl vector) start end)
  (let ((node (skip-list-after-node sl start)))
    (when node
      (make-instance 'skip-list-range-cursor :node node :end end))))

(defmethod map-skip-list (fun (sl vector))
  (let ((cursor (skip-list-cursor sl)))
    (do ((val (sl-cursor-next cursor (skip-list< sl)) 
	      (sl-cursor-next cursor (skip-list< sl))))
	((null val))
      (apply fun val))))

(defmethod map-skip-list-values (fun (sl vector))
  (let ((cursor (skip-list-values-cursor sl)))
    (do ((val (sl-cursor-next cursor (skip-list< sl)) 
	      (sl-cursor-next cursor (skip-list< sl))))
	((null val))
      (funcall fun val))))

