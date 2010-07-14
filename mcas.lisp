;;(in-package #:vivace-graph)

(require :sb-concurrency)

(defvar *log-stream* t)
;; MCAS status markers
(defconstant +mcas-undecided+ :undecided)
(defconstant +mcas-failed+ :failed)
(defconstant +mcas-succeeded+ :succeeded)

;; MCAS transaction global
(defvar *mcas* nil)

(defmacro while (test &rest body)
  `(loop until (not ,test) do
	,@body))

;; Make compare-and-swap shorter to call
(defmacro cas (place old new)
  `(sb-ext:compare-and-swap ,place ,old ,new))

(defun get-vector-addr (vector)
  (logandc2 (sb-kernel:get-lisp-obj-address vector) sb-vm:lowtag-mask))

(defstruct (ccas-descriptor
	     (:type vector)
	     (:predicate ccas-descriptor?)
	     (:conc-name cd-)
	     :named)
  vector-addr vector control-vector control-index index old new equality)

(defstruct (safe-update
	     (:type vector)
	     (:predicate safe-update?)
	     (:conc-name update-)
	     :named)
  vector index old new)

(defstruct (mcas-descriptor
	     (:type vector)
	     (:predicate mcas-descriptor?)
	     (:conc-name mcas-)
	     :named)
  (status +mcas-undecided+) (count 0) updates (equality #'equal))

(defun ccas-help (cd)
  ;; FIXME: write barrier?
  (if (eq (svref (cd-control-vector cd) (cd-control-index cd)) +mcas-undecided+)
      (cas (svref (cd-vector cd) (cd-index cd)) cd (cd-new cd))
      (cas (svref (cd-vector cd) (cd-index cd)) cd (cd-old cd))))

(defun ccas-read (vector index)
  ;; FIXME: read barrier
  (let ((r (svref vector index)))
    (if (ccas-descriptor? r)
	(progn
	  (ccas-help r)
	  (ccas-read vector index))
	r)))

(defun ccas (vector index control-vector control-index old new 
	     &optional (equality #'equal))
  (let ((cd (make-ccas-descriptor :vector-addr (get-vector-addr vector)
				  :vector vector
				  :index index
				  :control-vector control-vector
				  :control-index control-index
				  :old old
				  :new new
				  :equality equality)))
    (let ((r (cas (svref vector index) old cd)))
      (while (not (funcall equality r old))
	(if (not (ccas-descriptor? r))
	    (return-from ccas r)
	    (progn
	      (ccas-help r)
	      (setq r (cas (svref vector index) old cd))))))
    (ccas-help cd)))

(defun mcas-help (md)
  (let ((desired-state +mcas-failed+))
    (tagbody
       (dotimes (i (mcas-count md))
	 (let ((update (elt (mcas-updates md) i)))
	   (loop
	      ;; FIXME: embed ccas desriptior in mcas struct and reuse?
	      (ccas (update-vector update) (update-index update) 
		    md 1 
		    (update-old update) md
		    (mcas-equality md))
	      (let ((r (svref (update-vector update) (update-index update))))
		(cond ((and (eq (mcas-status md) +mcas-undecided+) 
			    (equal (update-old update) r))
		       t)
		      ((eq r md)
		       (return))
		      ((not (mcas-descriptor? r))
		       (go decision-point))
		      (t (mcas-help r)))))))
       (setq desired-state +mcas-succeeded+)
     decision-point
       (cas (svref md 1) +mcas-undecided+ desired-state)
       (dotimes (i (mcas-count md))
	 (let ((update (elt (mcas-updates md) i)))
	   (cas (svref (update-vector update) (update-index update)) md
		(if (eq (mcas-status md) +mcas-succeeded+)
		    (update-new update)
		    (update-old update)))))))
  (mcas-status md))

(defun mcas-read (vector index)
  ;; FIXME: read barrier
  (let ((r (svref vector index)))
    (if (mcas-descriptor? r)
	(progn
	  (mcas-help r)
	  (mcas-read vector index))
	r)))

(defun mcas (md)
  (let ((objects (remove-duplicates
		  (mapcar #'(lambda (update) (update-vector update))
			  (mcas-updates md)))))
    (sb-sys:with-pinned-objects (objects)
      (setf (mcas-updates md) 
	    (sort (mcas-updates md) #'<
		  :key #'(lambda (update)
			   (+ (get-vector-addr (update-vector update))
			      (update-index update)))))
      (mcas-help md))))

(defun mcas-set (vector index old new)
  (if (mcas-descriptor? *mcas*)
      (progn
	(push (make-safe-update :vector vector :index index :old old :new new) 
	      (mcas-updates *mcas*))
	(incf (mcas-count *mcas*)))
      (error "MCAS-SET must be called within the body of with-mcas")))

(defmacro with-mcas (lambda-list &body body)
  `(let ((*mcas* (make-mcas-descriptor ,@lambda-list)))
     (progn
       ,@body)
     (mcas *mcas*)))

(defun mcas-test (&key (threads 4)(size 100))
  (sb-profile:reset)
  (sb-profile:profile get-vector-addr 
		      print-ccas-descriptor 
		      new-ccas-descriptor  
		      make-ccas-descriptor
		      ccas-help
		      ccas
		      ccas-read
		      safe-update
		      safe-update?
		      make-mcas-descriptor
		      mcas-descriptor?
		      mcas-help
		      mcas
		      mcas-read
		      mcas-set)
  (let* ((initial (random 100000))
	 (v (make-array size :initial-element initial))
	 (thread-list nil)
	 (queue (sb-concurrency:make-queue)))
    (format t "Initial element is ~A~%" initial)
    (dotimes (x threads)
      (sb-concurrency:enqueue x queue)
      (push (sb-thread:make-thread
	     #'(lambda ()
		 (let ((i (sb-concurrency:dequeue queue)))
		   (sleep (/ i 10))
		   (format t "~A: I IS ~A~%" sb-thread:*current-thread* i)
		   (with-mcas (:equality #'equal)
		     (dotimes (j size)
		       (mcas-set v j (+ i initial) (+ 1 i initial))))))
	     :name (format nil "thread~A" x))
	    thread-list))
    (format t "~A: V 0 = ~A~%" sb-thread:*current-thread* (mcas-read v 0))
    (dolist (thread (reverse thread-list))
      (sb-thread:join-thread thread)
      (format t "~A: V 0 = ~A~%" thread (mcas-read v 0)))
    (format t "v[0] = ~A & v[~A] = ~A~%" (svref v 0) (1- size) (svref v (1- size))))
  (sb-profile:report))
