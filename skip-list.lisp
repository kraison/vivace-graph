(in-package :vivace-graph)

(defconstant +max-level+ (the fixnum 32)
  "Maximum level of skip-list, should be enough for 2^32 elements.")

(defun random-level ()
  "Returns a random level for a new skip-list node, following Pugh's pattern of 
L1: 50%, L2: 25%, L3: 12.5%, ..."
  (declare (optimize speed))
  (do ((level 1 (1+ level)))
      ((or (= level +max-level+)
	   (= (mt-random 4 (make-mt-random-state)) 3)) ;; 
       level)
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
  (comparison #'less-than)
  (duplicates-allowed? nil)
  (length 0 :type (UNSIGNED-BYTE 64)))

(defun print-skip-list (sl stream depth)
  (declare (ignore depth))
  (format stream "#<SKIP-LIST OF LENGTH ~A, EQUAL-FUNC: ~A, DUPLICATES ~A>" 
	  (skip-list-length sl) (skip-list-key-equal sl)
	  (if (skip-list-duplicates-allowed? sl) "ALLOWED" "NOT ALLOWED")))

(defmethod skip-list-search ((sl skip-list) key &optional value)
  (let ((start-node (skip-list-head sl)))
    (let ((x start-node) (y nil) 
	  (left-list  (make-array +max-level+ :initial-element nil))
	  (right-list (make-array +max-level+ :initial-element nil)))
      (loop for level from (1- (skip-node-level start-node)) downto 0 do
	   (loop
	      (setq y (mcas-read (skip-node-forward x) level))
	      (cond ((or (null y)
			 (funcall (skip-list-comparison sl) key (skip-node-key y)))
		     (return))
		    ((and value 
			  (funcall (skip-list-key-equal sl) key (skip-node-key y))
			  (funcall (skip-list-value-equal sl) value (skip-node-value y)))
		     (return))
		    ((and (null value) (funcall (skip-list-key-equal sl) key (skip-node-key y)))
		     (return))
		    (t (setq x y))))
	   (setf (svref left-list  level) x
		 (svref right-list level) y))
      (values left-list right-list))))

(defmethod skip-list-empty? ((sl skip-list))
  (= (skip-list-length sl) 0))

(defun node-forward (node)
  (declare (type (or null simple-vector) node))
  (if node
      (svref (skip-node-forward node) 0)
      nil))

(defmethod skip-list-to-list ((sl skip-list))
  (let ((node (skip-list-head sl)))
    (loop for next = (node-forward node) then (node-forward next)
	  while next
	  collect (list (skip-node-key next) (skip-node-value next)))))

(defmethod skip-list-lookup ((sl skip-list) key &optional value)
  (multiple-value-bind (left-list right-list) (skip-list-search sl key value)
    (declare (ignore left-list))
    (if (and (svref right-list 0) 
	     (funcall (skip-list-key-equal sl) key (skip-node-key (svref right-list 0))))
	(skip-node-value (svref right-list 0))
	nil)))

(defmethod skip-list-replace-kv ((sl skip-list) key new-value &optional old-value)
  "Replaces a node's value with new-value.  If old-value is supplied, will only replace the value 
if it matches old-value, otherwise throws 'skip-list-kv-not-found-error."
  (multiple-value-bind (left-list right-list) (skip-list-search sl key old-value)
    (declare (ignore left-list))
    (let ((node (svref right-list 0)))
      (when (and node
		 (funcall (skip-list-key-equal sl) key (skip-node-key node)))
	(if old-value
	    (let ((read-value (skip-node-value node)))
	      (if (funcall (skip-list-value-equal sl) old-value read-value)
		  (if (funcall (skip-list-value-equal sl)
			       read-value
			       (cas (svref node +skip-node-value+)
				    read-value
				    new-value))
		      +mcas-succeeded+
		      +mcas-failed+)
		  (error 'skip-list-kv-not-found-error :key key :value old-value)))
	    (let ((read-value (skip-node-value node)))
	      (if (funcall (skip-list-value-equal sl)
			   read-value
			   (cas (svref node +skip-node-value+)
				read-value
				new-value))
		  +mcas-succeeded+
		  +mcas-failed+)))))))
	    

(defmethod skip-list-add ((sl skip-list) key value)
  "Adds a new k/v pair to the skip list.  Will not overwrite existing nodes or values. 
Use skip-list-replace-kv for that.  Be prepared to catch a 'skip-list-duplicate-error."
  (let ((new-node (make-skip-node key value (random-level))))
    (multiple-value-bind (left-list right-list) (skip-list-search sl key value)
      (let ((right-node (svref right-list 0))
	    (left-node (svref left-list 0)))
	(cond ((and right-node
		    (funcall (skip-list-key-equal sl) key (skip-node-key right-node))
		    (not (skip-list-duplicates-allowed? sl)))
	       (error 'skip-list-duplicate-error :key key :value (skip-node-value right-node)))
	      ((and left-node 
		    (funcall (skip-list-key-equal sl) key (skip-node-key left-node))
		    (not (skip-list-duplicates-allowed? sl)))
	       (error 'skip-list-duplicate-error :key key :value (skip-node-value left-node)))
	      (t
	       (with-recursive-mcas (:equality #'equal
					       :success-action 
					       #'(lambda () 
						   (sb-ext:atomic-incf (skip-list-length sl))))
		 (dotimes (i (skip-node-level new-node))
		   (setf (svref (skip-node-forward new-node) i) (svref right-list i))
		   (mcas-set (skip-node-forward (svref left-list i)) i
			     (svref right-list i)
			     new-node)))))))))

(defmethod skip-list-delete ((sl skip-list) key &optional value)
  "Delete a key or k/v pair from the skip list.  If no value is specified and duplicates are
allowed, it will delete the first key it finds."
  (multiple-value-bind (left-list right-list) (skip-list-search sl key value)
    (let ((match-node (svref right-list 0)))
      (if (or (null match-node) (not (equal (skip-node-key match-node) key)))
	  nil
	  (let ((old-value (skip-node-value match-node)))
	    (if (eq nil old-value)
		nil
		(with-recursive-mcas (:equality #'equal 
						:success-action 
						#'(lambda () 
						    (sb-ext:atomic-decf (skip-list-length sl))))
		  (loop for i from 0 to (1- (skip-node-level match-node)) do
		       (let ((next-node (mcas-read (skip-node-forward match-node) i)))
			 (if (and next-node
				  (funcall (skip-list-comparison sl) 
					   (skip-node-key match-node) 
					   (skip-node-key next-node)))
			     nil
			     (progn
			       (mcas-set (skip-node-forward (svref left-list i)) i
					 match-node
					 next-node)
			       (mcas-set (skip-node-forward match-node) i
					 next-node
					 (svref left-list i))))))
		  (mcas-set match-node +skip-node-value+ old-value nil))))))))

;;; cursors, some code borrowed from Manuel Odendahl <manuel@bl0rg.net>'s skip list code
(defclass skip-list-cursor ()
  ((node :initarg :node :accessor skip-list-cursor-node)
   (skip-list :initarg :skip-list :accessor skip-list)))

(defmethod sl-cursor-next ((slc skip-list-cursor) &optional eoc)
  (with-slots (node) slc
    (if node
	(let ((result (list (skip-node-key node)
			    (skip-node-value node))))
	  (setf node (node-forward node))
	  result)
	eoc)))

(defclass skip-list-value-cursor (skip-list-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skip-list-value-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
	eoc
	(second result))))

(defclass skip-list-key-cursor (skip-list-cursor)
  ())

(defmethod sl-cursor-next :around ((slc skip-list-key-cursor) &optional eoc)
  (let ((result (call-next-method)))
    (if (eql result eoc)
	eoc
	(first result))))

(defmethod skip-list-cursor ((sl skip-list) &key cursor (class 'skip-list-cursor))
  (if cursor
      (progn (setf (skip-list-cursor-node cursor)
		   (node-forward (skip-list-head sl)))
	     cursor)
     (make-instance class :node (node-forward (skip-list-head sl)) :skip-list sl)))

(defmethod skip-list-values-cursor ((sl skip-list))
  (skip-list-cursor sl :class 'skip-list-value-cursor))

(defmethod skip-list-keys-cursor ((sl skip-list))
  (skip-list-cursor sl :class 'skip-list-key-cursor))

(defclass skip-list-range-cursor (skip-list-cursor)
  ((end :initarg :end :reader slrc-end)))

(defmethod sl-cursor-next :around ((slc skip-list-range-cursor) &optional eoc)
  (with-slots (node end) slc
    (if (and node 
	     (or 
	      (funcall (skip-list-comparison (skip-list slc)) (skip-node-key node) end)
	      (funcall (skip-list-key-equal (skip-list slc)) (skip-node-key node) end)))
	(call-next-method)
	eoc)))

(defmethod skip-list-range-cursor ((sl skip-list) start end)
  (multiple-value-bind (left-list right-list) (skip-list-search sl start)
    (let ((right-node (svref right-list 0))
	  (left-node (svref left-list 0)))
      (cond ((and left-node (funcall (skip-list-key-equal sl) start (skip-node-key left-node)))
	     (make-instance 'skip-list-range-cursor 
			    :node left-node :end end :skip-list sl))
	    ((and right-node (funcall (skip-list-key-equal sl) start (skip-node-key right-node)))
	     (make-instance 'skip-list-range-cursor 
			    :node right-node :end end :skip-list sl))))))

(defmethod map-skip-list (fun (sl skip-list))
  (let ((cursor (skip-list-cursor sl)))
    (do ((val (sl-cursor-next cursor) 
	      (sl-cursor-next cursor)))
	((null val))
      (apply fun val))))

(defmethod map-skip-list-values (fun (sl skip-list))
  (let ((cursor (skip-list-values-cursor sl)))
    (do ((val (sl-cursor-next cursor) 
	      (sl-cursor-next cursor)))
	((null val))
      (funcall fun val))))

(defmethod skip-list-fetch-all ((sl skip-list) key)
  "Return all values for a key in a skip list where duplicates are allowed."
  (let ((cursor (skip-list-range-cursor sl key key))
	(result nil))
    (if cursor
	(progn
	  (do ((node (sl-cursor-next cursor) (sl-cursor-next cursor)))
	      ((null node))
	    (push (second node) result))
	  (nreverse result))
	nil)))
      
(defun sl-test ()
  (let ((sl (make-skip-list :duplicates-allowed? t)))
    (dotimes (i 1000)
      (skip-list-add sl i (code-char i)))
    (map-skip-list #'(lambda (k v) (format t "~A: ~A~%" k v)) sl)
    (let ((c (skip-list-range-cursor sl 33 126)))
      (do ((i (sl-cursor-next c (skip-list-comparison sl))
	      (sl-cursor-next c (skip-list-comparison sl))))
	  ((null i))
	(format t "~A~%" i)))
    (skip-list-to-list sl)))
