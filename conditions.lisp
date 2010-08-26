(in-package #:vivace-graph)

(define-condition prolog-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
	     (with-slots (reason) error
	       (format stream "Prolog error: ~A." reason)))))

