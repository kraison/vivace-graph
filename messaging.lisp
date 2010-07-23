(in-package #:vivace-graph)

(defstruct (message
	     (:conc-name msg-)
	     (:predicate message?)
	     (:print-function print-message))
  content sender task)

(defun print-message (message stream depth)
  (declare (ignore depth))
  (format stream "#<MESSAGE: ~A FROM ~A>" (msg-content message) (msg-sender message)))

