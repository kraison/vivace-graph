;;;-*- Package: SKIP-LIST; Mode: Lisp; Base: 10 -*-

;;; Copyright (c) 1990, 1991, 1992 by Xerox Corporation.  All rights reserved.

;;;; Skip Lists (from CACM volume 33, number 6 (June 1990) pp668-680)
;;;; implementation by Doug Cutting, June 1990

(cl:defpackage :skip-list
  (:export
   ;; high-level interface
   :skip-list :make-skip-list :free-skip-list
   :skip-list-length :skip-list-empty-p
   :skip-list-get :skip-list-remove
   :skip-list-top :skip-list-pop
   :skip-list-merge skip-list-copy
   :skip-list-union :skip-list-intersection :skip-list-difference
   :do-skip-list :map-skip-list :clear-skip-list))

(cl:in-package :skip-list)


;;; Nodes are represented by simple vectors.  The zeroth slot contains the
;;; key.  The first slot contains the node value.  Subsequent slots contain
;;; level pointers.

(defmacro node-key (node) `(svref ,node 0))
(defmacro node-value (node) `(svref ,node 1))
(defmacro node-next (node) `(svref ,node 2))

(defconstant +first-level+ 2)
(defconstant +max-level+
  (1- (+ +first-level+ (integer-length most-positive-fixnum))))

(defmacro node-level (node) 
  `(1- (the fixnum (length (the simple-vector ,node)))))

(defstruct (skip-list
	     (:constructor %make-skip-list (&optional order-fn))
	     (:print-function %print-skip-list))
  (length 0 :type fixnum)
  (header (make-array (1+ +max-level+) :initial-element nil))
  (finger (make-array (1+ +max-level+)))
  (level (1- +first-level+) :type fixnum)
  (order-fn				; returns T, NIL or :EQUAL
   #'(lambda (x y) (declare (ignore x y)) :equal)
    :type function))

(defun %print-skip-list (skip-list stream depth)
  (declare (ignore depth))
  (format stream "#<Skip List: length=~D, order=~A>"
	  (skip-list-length skip-list)
	  (skip-list-order-fn skip-list)))

(defmacro skip-list-empty-p (skip-list)
  `(null (node-next (skip-list-header ,skip-list))))

(defmacro top-node (skip-list)
  `(node-next (skip-list-header ,skip-list)))

(defmacro do-nodes ((node-var skip-list) &body body)
  (let ((skip-list-var (gensym "SKIP-LIST"))
	(next-node-var (gensym "SKIP-LIST")))
    `(let* ((,skip-list-var ,skip-list)
	    (,next-node-var (node-next (skip-list-header ,skip-list-var)))
	    ,node-var)
       (loop 
	 (setq ,node-var ,next-node-var)
	 (if ,node-var
	     (setq ,next-node-var (node-next ,node-var))
	   (return))
	 ,@body))))

(defmacro do-skip-list ((key-var val-var skip-list) &body body)
  (let ((node-var (gensym "NODE")))
    `(do-nodes (,node-var ,skip-list)
       (let ((,key-var (node-key ,node-var))
	     (,val-var (node-value ,node-var)))
	 ,@body))))

;;;; skip-list header allocation
(defparameter *free-skip-lists* nil)

(defun make-skip-list (order-fn)
  (let* ((skip-list (or *free-skip-lists* (%make-skip-list order-fn)))
	 (header (skip-list-header skip-list))
	 (finger (skip-list-finger skip-list)))
    (setf *free-skip-lists* (svref header 0))
    (setf (skip-list-length skip-list) 0
	  (skip-list-level skip-list) (1- +first-level+)
	  (skip-list-order-fn skip-list) order-fn)
    (dotimes (i +max-level+)
      (setf (svref header i) nil
	    (svref finger i) header))
    skip-list))

(defun free-skip-list (skip-list)
  (clear-skip-list skip-list)
  (setf (svref (skip-list-header skip-list) 0) *free-skip-lists*
	*free-skip-lists* skip-list))
	
;;;; node allocation

(defparameter *free-nodes*
    (make-skip-list #'(lambda (x y) (declare (ignore x y)) :equal)))

(defun make-node (key value)
  (let ((node (if (skip-list-empty-p *free-nodes*)
		  (make-array (the fixnum (1+ (the fixnum (random-level)))))
		(pop-node *free-nodes*))))
    (setf (node-key node) key
	  (node-value node) value)
    node))

(defun free-node (node) (insert-node node *free-nodes*))


;;;; header operations

(defun grow-header (level skip-list)
  ;; LEVEL is greater than any previous level.  Repair the FINGER vector.
  (declare (fixnum level))
  (do ((i (1+ (skip-list-level skip-list)) (1+ i))
       (header (skip-list-header skip-list))
       (finger (skip-list-finger skip-list)))
      ((> i level) (setf (skip-list-level skip-list) level))
    (declare (fixnum i))
    (setf (svref finger i) header)))

(defun shrink-header (skip-list)
  ;; decrement level pointer past any NILs in header
  (do ((i (skip-list-level skip-list) (1- i))
       (header (skip-list-header skip-list)))
      ((or (< i +first-level+) (svref header i))
       (setf (skip-list-level skip-list) i))
    (declare (fixnum i))))

;;;FINGER MODIFYING OPERATIONS
;;;finger ops return the node which has been fingered

;;;first-node fingers the first node in the list
(defun finger-start (skip-list)
  (do ((i +first-level+ (1+ i))
       (finger (skip-list-finger skip-list))
       (head (skip-list-header skip-list))
       (level (skip-list-level skip-list)))
      ((or (> i level) (eq (svref finger i) head))
       (node-next head))
    (declare (fixnum i))
    (setf (svref finger i) head)))

;;;end fingers the end of the list (so a following insert is an append)
;;;in order to be useful, it returns the last node in the list (which is not
;;;actually the fingered node)
(defun finger-end (skip-list)
  ;;set finger vector to finger beyond last entry
  (do* ((finger (skip-list-finger skip-list))
	(header (skip-list-header skip-list))
	(node header)
	(level (skip-list-level skip-list) (1- level)))
      ((< level +first-level+)
       node)
    (declare (fixnum level))
    ;; iterate down levels, performing linear search at each level
    (do ((next (svref node level) (svref node level)))
	((null next)
	 (setf (svref finger level) node))
      (setf node next))))

;;;next-node advances the finger by one node
(defun finger-next (skip-list)
  (do* ((finger (skip-list-finger skip-list))
	(level (skip-list-level skip-list))
	(current (node-next finger))
	(i +first-level+ (1+ i)))
      ((or (> i level) 
	   (not (eq (svref (svref finger i) i) current)))
       (node-next current))
    (declare (fixnum i))
    (setf (svref finger i) current)))

(defun cached-set-finger (key skip-list)
  (let* ((order-fn (skip-list-order-fn skip-list))
	 (finger (skip-list-finger skip-list))
	 (header (skip-list-header skip-list) )
	 (prev (node-next finger))
	 (node (node-next prev))
	 (key-leq-node-p
	  (or (null node) (funcall order-fn key (node-key node)))))
    (declare (function order-fn))
    (if (eq key-leq-node-p :equal)
	node
      (let ((key-gt-prev-p
	     (or (eq prev header)
		 (not (funcall order-fn key (node-key prev))))))
	(if (and key-leq-node-p key-gt-prev-p)
	    nil
	  (set-finger
	   key header finger (skip-list-level skip-list) order-fn))))))

(defun set-finger (key header finger slevel order-fn)
  ;; Returns the node vector for KEY or NIL if none exists.
  ;; Also updates the finger vector.
  (let ((last-compared nil)
	(order nil)
	(next nil)
	(node header))
    (do ((level slevel (1- level)))
	((< level +first-level+)
	 (when (and next
		    (eq (funcall order-fn (node-key next) key) :equal))
	   next))
      (declare (fixnum level) (function order-fn))
      ;; iterate down levels, performing linear search at each level
      (loop
	(setq next (svref node level))
	(when (eq next last-compared)	; minimize comparisions
	  (return))
	(setq order (funcall order-fn (node-key next) key))
	(unless (eq order t) (return))	; too far or equal
	(setq node next))		; continue at this level
      ;; note furthest extent of linear scan
      (setq last-compared next)
      ;; update FINGER vector
      (setf (svref finger level) node))))

(defun advance-finger (key skip-list)
  (do ((finger (skip-list-finger skip-list))
       (order-fn (skip-list-order-fn skip-list))
       (level (skip-list-level skip-list))
       (i +first-level+ (1+ i)))
      ((or (> i level)
	   (funcall order-fn key (node-key (svref finger i))))
       (set-finger key finger finger (1- i) order-fn))
    (declare (function order-fn))))

;;;SKIP-LIST-MODIFYING OPS
(defun insert-node (node skip-list)
;;; inserts a new node SKIP-LIST at position pointed to by finger vector. the
;;; finger remains unchanged, i.e. points to the same node as before, which
;;; immediately follows the inserted node.   
  (let ((level (node-level node)))
    (declare (fixnum level))
    (when (> level (skip-list-level skip-list))
      (grow-header level skip-list))
    (do ((i +first-level+ (1+ i))
	 (finger (skip-list-finger skip-list)))
	((> i level))
      (declare (fixnum i))		; splice in new node
      (let ((previous (svref finger i)))
	(setf (svref finger i) node)
	(setf (svref node i) (svref previous i))
	(setf (svref previous i) node)))
    (incf (skip-list-length skip-list))
    node))

(defun remove-node (skip-list)
;;; Removes node fingered in skip-list-finger SKIP-LIST (unless nil)
;;; returns node (or nil).  finger now points to following node
  (let* ((finger (skip-list-finger skip-list))
	 (node (node-next (node-next finger))))
    (when node
      (decf (skip-list-length skip-list))
      (do ((level (node-level node))
	   (i +first-level+ (1+ i)))
	  ((> i level) 
	   (shrink-header skip-list)
	   node)
	(declare (fixnum i) (fixnum level)) ; splice around NODE
	(let ((previous (svref finger i)))  ;note (eq (svref previous i) node)
	  (setf (svref previous i) (svref node i)))))))

;;;; Primary exported ops

(defun skip-list-get (key skip-list &optional default)
;;; Return the value stored under KEY in SKIP-LIST, or DEFAULT if none exists.
  (let ((node (cached-set-finger key skip-list)))
    (if node (node-value node) default)))

(defsetf skip-list-get (key skip-list &optional default) (value)
  (declare (ignore default))
  `(skip-list-put ,key ,skip-list ,value))

(defun skip-list-put (key skip-list value)
;;; Store VALUE under KEY in SKIP-LIST and return VALUE.
  (let ((node (cached-set-finger key skip-list)))
    (if node
	(setf (node-value node) value)
      (progn (insert-node (make-node key value) skip-list)
	     value))))
	 
(defun skip-list-remove (key skip-list)
;;; Removes any entry stored under KEY in SKIP-LIST.  Returns NIL if none is
;;; found or (1) the key and (2) the value stored.
  (let ((node (cached-set-finger key skip-list)))
    (when node
      (multiple-value-prog1 
	  (values (node-key node) (node-value node))
	(free-node (remove-node skip-list))))))

(defun skip-list-top (skip-list)
  (declare #.tdb:*highly-optimized*)
  (let ((node (top-node skip-list)))
    (when node
      (values (node-key node) (node-value node)))))

(defun pop-node (skip-list)
  (finger-start skip-list)
  (remove-node skip-list))

(defun skip-list-pop (skip-list)
  (let ((node (pop-node skip-list)))
    (when node
      (multiple-value-prog1 
	  (values (node-key node) (node-value node))
	(free-node node)))))

(defun map-skip-list (function skip-list)
;;; Call FUNCTION on every KEY and VALUE in SKIP-LIST in order.
  (declare (function function))
  (do-nodes (node skip-list)
    (funcall function (node-key node) (node-value node))))
    
(defun clear-skip-list (skip-list)
  ;; free nodes
  (finger-end skip-list)
  (shift-nodes skip-list *free-nodes*)
  (incf (skip-list-length *free-nodes*) (skip-list-length skip-list))
  (setf (skip-list-length skip-list) 0)
  skip-list)

(defun skip-list-copy (skip-list)
  (let ((new (make-skip-list (skip-list-order-fn skip-list))))
    (do-skip-list (k v skip-list)
      (insert-node (make-node k v) new))))
       
;;; pops all items up to that described by current finger vector
;;; (i.e., brings last "find" to front) and puts all popped stuff onto tail of
;;; dest.  dest finger vector is modified to point beyond its new last vector.
(defun shift-nodes (src dest)
  (let* ((src-level (skip-list-level src))
	 (src-header (skip-list-header src))
	 (src-finger (skip-list-finger src))
	 (dest-finger (skip-list-finger dest)))
    (declare (fixnum src-level))
    (when (> src-level (skip-list-level dest))
      (grow-header src-level dest))
    (finger-end dest)
    (do ((level +first-level+ (1+ level)))
	((or (> level src-level)
	     (eq (svref src-finger level) src-header)))
      (declare (fixnum level))
      (setf (svref (svref dest-finger level) level) (svref src-header level)
	    (svref src-header level) (svref (svref src-finger level) level)
	    (svref (svref src-finger level) level) nil
	    (svref dest-finger level) (svref src-finger level)
	    (svref src-finger level) src-header)))
  (shrink-header src)
  (shrink-header dest)
  src)

;;; destructive merge and intersection at the same time
(defun skip-list-merge (skip1 skip2 merge-values)
  (declare (function merge-values))
  (let* ((order-fn (skip-list-order-fn skip1))
	 (union (make-skip-list order-fn))
	 (intersection (make-skip-list order-fn))
	 (head1 (skip-list-header skip1))
	 (head2 (skip-list-header skip2))
	 (union-count (+ (skip-list-length skip1) (skip-list-length skip2)))
	 (intersect-count 0)
	 (flipped nil))
    (declare (fixnum union-count intersect-count) (function order-fn))
    (loop
      (unless (and (node-next head1) (node-next head2))
	(return))
      (let ((key1 (node-key (node-next head1)))
	    (key2 (node-key (node-next head2))))
	;;WLOG key1 <= key2
	(unless (funcall order-fn key1 key2)
	  (rotatef skip1 skip2)
	  (rotatef key1 key2)
	  (rotatef head1 head2)
	  (setf flipped (not flipped)))
	(let* ((node2 (node-next head2))
	       (node1 (cached-set-finger key2 skip1)))
	  (shift-nodes skip1 union) ;key2 now front of skip1
	  (when node1			;matching keys
	    (incf intersect-count)
	    (let* ((v1 (node-value node1))
		   (v2 (node-value node2))
		   (v (if flipped
			  (funcall merge-values v2 v1)
			(funcall merge-values v1 v2))))
	      (setf (node-value node1) v
		    (node-value node2) v)
	      (insert-node (skip-list-pop skip2) intersection))))))
    (when (null (node-next head1))
      (rotatef skip1 skip2))
    (finger-end skip1)
    (shift-nodes skip1 union)
    (setf (skip-list-length union) (- union-count intersect-count)
	  (skip-list-length intersection) intersect-count)
    (free-skip-list skip1)
    (free-skip-list skip2)
    (values union intersection)))

;;;destructive-union
(defun skip-list-union
    (skip1 skip2 
     &optional (merge-values #'(lambda (x y) (declare (ignore x)) y)))
  (declare (function merge-values))
  (multiple-value-bind (union intersection)
      (skip-list-merge skip1 skip2 merge-values)
    (free-skip-list intersection)
    union))

;;;destructive intersect
(defun skip-list-destructive-intersection
    (skip1 skip2 
     &optional (merge-values #'(lambda (x y) (declare (ignore x)) y)))
  (declare (function merge-values))
  (multiple-value-bind (union intersection)
      (skip-list-merge skip1 skip2 merge-values)
    (free-skip-list union)
    intersection))

;;;nondestructive (to second arg) intersection and difference
(defun skip-list-delta
    (skip1 skip2 
     &optional (merge-values #'(lambda (x y) (declare (ignore x)) y)))
  (declare (function merge-values))
  (finger-start skip1)
  (finger-start skip2)
  (let* ((order-fn (skip-list-order-fn skip1))
	 (dest-intersect (make-skip-list order-fn))
	 (dest-minus (make-skip-list order-fn))
	 (intersect-count 0)
	 (minus-count (skip-list-length skip1))
	 (head1 (skip-list-header skip1)))
    (declare (fixnum intersect-count minus-count) (function order-fn))
    (loop
      (let ((node1 (node-next head1)))
	(when (null node1)
	  (return))
	(let ((node2 (advance-finger (node-key node1) skip2)))
	  (when (null node2) 
	    (return))
	  (let ((node1 (cached-set-finger (node-key node2) skip1)))
	    (shift-nodes skip1 dest-minus)
	    (when (eq (funcall order-fn (node-key node1) (node-key node2))
		      :equal)
	      (incf intersect-count)
	      (setf (node-value node1)
		(funcall merge-values (node-value node1)
			 (node-value node2)))
	      (node-next skip1)
	      (shift-nodes skip1 dest-intersect))))))
    (free-skip-list skip1)
    (setf (skip-list-length dest-intersect) intersect-count
	  (skip-list-length dest-minus) (- minus-count intersect-count))
    (values dest-intersect dest-minus)))

(defun skip-list-intersection
    (skip1 skip2
     &optional (merge-values #'(lambda (x y) (declare (ignore x)) y)))
  (multiple-value-bind (dest-intersect dest-minus)
      (skip-list-delta skip1 skip2 merge-values)
    (free-skip-list dest-minus)
    dest-intersect))

(defun skip-list-difference
    (skip1 skip2 
     &optional (merge-values #'(lambda (x y) (declare (ignore x)) y)))
  (multiple-value-bind (dest-intersect dest-minus)
      (skip-list-delta skip1 skip2 merge-values)
    (free-skip-list dest-intersect)
    dest-minus))

;;;; (COIN-TOSS) returns true three times in four -- bad name, eh?

#+(or excl cmu)
(progn
  ;; Franz' random number generator is too slow & consy
  (defconstant +multiplier+ (+ (* (round most-positive-fixnum 1000) 2 100) 21))
  (defvar *random-fixnum* (random most-positive-fixnum))
  (eval-when (compile eval load)
    (deftype +fixnum () `(integer 0 ,most-positive-fixnum)))
  (declaim (type +fixnum +multiplier+ *random-fixnum*))

  (defun coin-toss ()
    (declare #.tdb:*highly-optimized*)
    ;; assume MOST-POSITIVE-FIXNUM is 2^n-1, i.e. all ones
    (setq *random-fixnum*
	  (logand most-positive-fixnum	; ensure positive
		  (the +fixnum		; assume overflows truncate
		       (1+ (the +fixnum (* *random-fixnum* +multiplier+))))))
    ;; high order bits are 'more random' -- use highest bits
    (logtest *random-fixnum*
	     (ash 3 (- (integer-length most-positive-fixnum) 2)))))
#-(or excl cmu)
(defmacro coin-toss ()
  `(logtest (the fixnum (random 4)) 3))

(defun random-level ()
  (do ((i +first-level+ (1+ i)))
      ((or (coin-toss) (= i +max-level+)) i)
    (declare (fixnum i))))

