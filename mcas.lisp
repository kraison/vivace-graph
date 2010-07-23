(in-package #:vivace-graph)

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
  (status +mcas-undecided+) 
  (count 0) 
  updates 
  (equality #'equal) 
  success-actions 
  (durable-changes nil)
  (durability-thread nil)
  (retries 0)
  (timestamp (gettimeofday)))

(defun ccas-help (cd)
  ;; FIXME: add read barrier
  (if (eq (svref (cd-control-vector cd) (cd-control-index cd)) +mcas-undecided+)
      (cas (svref (cd-vector cd) (cd-index cd)) cd (cd-new cd))
      (cas (svref (cd-vector cd) (cd-index cd)) cd (cd-old cd))))

(defun ccas-read (vector index)
  ;; FIXME: add read barrier
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
    ;; FIXME: add read barrier
    (let ((r (cas (svref vector index) old cd)))
      (while (not (funcall equality r old))
	(if (not (ccas-descriptor? r))
	    (return-from ccas r)
	    (progn
	      (ccas-help r)
	      (setq r (cas (svref vector index) old cd))))))
    (ccas-help cd)))

(defun mcas-help (md)
  "This is the bulk of the transaction logic.  Includes durability."
  (let ((state +mcas-failed+))
    (tagbody
       (dotimes (i (mcas-count md))
	 (let ((update (elt (mcas-updates md) i)))
	   (loop
	      ;; Try to replace the slot with our mcas descriptor.
	      (ccas (update-vector update) (update-index update) 
		    md 1 
		    (update-old update) md
		    (mcas-equality md))
	      (let ((r (svref (update-vector update) (update-index update))))
		(cond ((and (eq (mcas-status md) +mcas-undecided+) 
			    (funcall (mcas-equality md) (update-old update) r))
		       ;; Got old value, not our mcas descriptor. Try again.
		       t)
		      ((eq r md)
		       ;; This slot has been successfully replaced by our mcas descriptor. Next.
		       (return)) 
		      ((not (mcas-descriptor? r))
		       ;; Oops, someone else changed this slot.  Abort.
		       (go decision-point))
		      (t 
		       ;; Someone else has a transaction active on this slot. Help them out.
		       (mcas-help r))))))) 
       (setq state +mcas-make-durable+)
     decision-point
       ;; FIXME: Add write barrier needed for non-x86 (Alpha / Sparc)
       (cas (svref md 1) +mcas-undecided+ state)
       (when (eq (mcas-status md) +mcas-make-durable+)
	 (if (null (mcas-durable-changes md))
	     (cas (svref md 1) +mcas-make-durable+ +mcas-succeeded+)
	     (when (null (cas (svref md 7) nil (current-thread))) ;; Set durability-thread to me!
	       (handler-case
		   (make-durable (mcas-timestamp md) (reverse (mcas-durable-changes md)))
		 (error (condition)
		   (format t "Could not make ~A durable: ~A" md condition)
		   (cas (svref md 1) +mcas-make-durable+ +mcas-failed+))
		 (:no-error (durability-successful?)
		   (if durability-successful?
		       (cas (svref md 1) +mcas-make-durable+ +mcas-succeeded+)
		       (cas (svref md 1) +mcas-make-durable+ +mcas-failed+)))))))
       ;; FIXME: add write barrier needed for non-x86 (Alpha / Sparc)
       (cond ((eq (mcas-status md) +mcas-succeeded+)
	      (dotimes (i (mcas-count md))
		(let ((update (elt (mcas-updates md) i)))
		  (cas (svref (update-vector update) (update-index update)) 
		       md 
		       (update-new update)))))
	     ((eq (mcas-status md) +mcas-failed+)
	      (dotimes (i (mcas-count md))
		(let ((update (elt (mcas-updates md) i)))
		  (cas (svref (update-vector update) (update-index update)) 
		       md 
		       (update-old update))))))))
  (mcas-status md))

(defun mcas-read (vector index)
  ;; FIXME: add read barrier
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

(defun reset-mcas (mcas)
  (setf (mcas-status mcas)            +mcas-undecided+
	(mcas-count mcas)             0
	(mcas-updates mcas)           nil
	(mcas-success-actions mcas)   nil
	(mcas-durable-changes mcas)   nil
	(mcas-durability-thread mcas) nil
	(mcas-timestamp mcas)         (gettimeofday))
  mcas)

(defgeneric mcas-successful? (thing))

(defmethod mcas-successful? ((md array))
  (eq +mcas-succeeded+ (mcas-status md)))

(defmethod mcas-successful? ((s symbol))
  (eq +mcas-succeeded+ s))

(defun make-durable (object)
  (if (mcas-descriptor? *mcas*)
      (push (list :add object) (mcas-durable-changes *mcas*))
      (error "make-durable must be called within the body of with-mcas")))

(defun delete-durable (object)
  (if (mcas-descriptor? *mcas*)
      (push (list :delete object) (mcas-durable-changes *mcas*))
      (error "delete-durable must be called within the body of with-mcas")))

(defmacro with-mcas (lambda-list &body body)
  (let ((args (gensym))
	(equality (get-prop lambda-list :equality))
	(success-action (get-prop lambda-list :success-action)))
    `(let ((,args nil))
       (when (functionp ,equality)
	 (push ,equality ,args)
	 (push :equality ,args))
       (when (functionp ,success-action)
	 (push (list ,success-action) ,args)
	 (push :success-actions ,args))
       (let ((*mcas* (apply #'make-mcas-descriptor ,args)))
	 (unwind-protect
	      (loop
		 for retries from 0 to 199
		 do 
		   (progn ,@body)
		   (mcas *mcas*)
		   (if (eq +mcas-succeeded+ (mcas-status *mcas*))
		       (return)
		       (progn
			 (incf (mcas-retries *mcas*))
			 (sleep (* 0.000002 (nth (random 3) (list 0 1 retries))))
			 (reset-mcas *mcas*))))
	   (if (eq +mcas-succeeded+ (mcas-status *mcas*))
	       (dolist (func (reverse (mcas-success-actions *mcas*)))
		 (funcall func))
	       (error 'transaction-error :instance *mcas* :reason "Unknown")))
	 (mcas-status *mcas*)))))

(defmacro with-recursive-mcas (lambda-list &body body)
  (let ((success-action (get-prop lambda-list :success-action)))
    `(if (mcas-descriptor? *mcas*)
	 (progn
	   (when (functionp ,success-action)
	     (push ,success-action (mcas-success-actions *mcas*)))
	   ,@body)
	 (with-mcas ,lambda-list
	   ,@body))))
  

(defun mcas-test (&key (threads 4) (size 100))
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
