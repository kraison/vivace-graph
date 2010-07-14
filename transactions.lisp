(in-package #:vivace-graph)

(defun print-transaction (tx stream depth)
  (declare (ignore depth))
  (format stream "#<TX: ~A @ ~A>" (tx-uuid tx) (tx-timestamp tx)))

(defstruct (transaction
	     (:predicate transaction?)
	     (:conc-name tx-)
	     (:print-function print-transaction))
  uuid timestamp graph
  (dirty-objects (make-uuid-table)))

(defun start-transaction (graph)
  )

(defun commit-transaction (tx)
  )

(defun abort-transaction (tx)
  )

(defun end-transaction (tx)
  :done)

(defmacro with-transaction (graph &body body)
  (let ((tx (gensym)))
    `(let ((*graph* (or ,graph *graph*))
	   (,tx (start-transaction *graph*)))
       (unwind-protect
	    (progn
	      ,@body)
	 (end-transaction ,tx)))))
