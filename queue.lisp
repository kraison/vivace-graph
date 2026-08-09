(in-package :graph-db)

;; The following queueing code was borrowed and adapted from Russell &
;; Norvig's "Introduction to AI"
(defstruct (queue
             (:print-function
              (lambda (q stream depth)
                (declare (ignore depth))
                (format stream "<QUEUE: ~a>" (queue-elements q)))))
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-queue))

(defun empty-queue-p (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  (elt (queue-elements q) 0))

(defun dequeue (q)
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue-front (q &rest items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-elements q) (nconc items (queue-elements q))
               (queue-last q) (last (queue-elements q))))
        (t (setf (queue-elements q) (nconc items (queue-elements q))))))

(defun enqueue (q &rest items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))

(defun queue-remove (q item)
  (when (member item (queue-elements q))
    (setf (queue-elements q) (delete item (queue-elements q)))
    (setf (queue-last q) (last (queue-elements q)))))
;; End of adapted code
