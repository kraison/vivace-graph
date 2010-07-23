(in-package #:vivace-graph)

(defun print-transaction (tx stream depth)
  (declare (ignore depth))
  (format stream "#<TX: ~A @ ~A>" (tx-uuid tx) (tx-timestamp tx)))

(defstruct (transaction
	     (:predicate transaction?)
	     (:conc-name tx-)
	     (:print-function print-transaction))
  (uuid (make-uuid))
  (timestamp (gettimeofday))
  graph
  (retries 0)
  (state +tx-undecided+)
  (changes nil))

(defun enqueue-transaction (tx)
  "FIXME: write it to a queue or disk or something durable."
  tx)

(defun start-transaction (graph)
  (if (graph? graph)
      ;; FIXME: push not thread-safe
      (push (make-transaction :graph graph) *active-transactions*)
      (error 'transaction-error :instance nil :reason "GRAPH is not a graph!")))

(defun commit-transaction ()
  (setf (tx-state *transaction*) +tx-succeeded+))

(defun abort-transaction ()
  (setf (tx-state *transaction*) +tx-failed+))

(defun end-transaction ()
  (if (eq (tx-state *transaction*) +tx-succeeded+)
      (progn
	;; FIXME: remove / setq not thread-safe
	(setq *active-transactions* (remove *transaction* *active-transactions*))
	(enqueue-transaction *transaction*))
      (error 'transaction-error :instance *transaction* :reason "Transaction failed.")))

(defun tx-add (object)
  (when (transaction? *transaction*)
    (push (list :add object) (tx-changes *transaction*))))

(defun tx-delete (object)
  (when (transaction? *transaction*)
    (push (list :delete object) (tx-changes *transaction*))))

(defmacro with-transaction (lambda-list &body body)
  (let* ((g (pop lambda-list))
	 (success-action (get-prop lambda-list :success-action))
	 (failure-action (get-prop lambda-list :failure-action)))
    `(let* ((*graph* (or ,g *graph*))
	    (*transaction* (start-transaction *graph*)))
       (unwind-protect
	    (let ((state (with-mcas (:equality 'equal) ,@body)))
	      (loop 
		 for retries from 0 to 199
		 while (not (eq +mcas-succeeded+ state))
		 do 
		   (incf (tx-retries *transaction*))
		   (sleep (* 0.01 retires))
		   (setq state (with-mcas (:equality 'equal) ,@body)))
	      (if (eq state +mcas-succeeded+)
		  (commit-transaction)
		  (abort-transaction)))
	 (end-transaction)))))

(defmacro with-recursive-transaction (lambda-list &body body)
  `(if (transaction? *transaction*)
       (progn ,@body)
       (with-transaction ,lambda-list
	 ,@body)))

	 
