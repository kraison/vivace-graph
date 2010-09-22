(in-package #:vivace-graph-client)

(defparameter +max-bytes+ 40960)
(defparameter +magic-byte+ #x2A)
(defparameter +version+ #x01)
(defparameter +ack+ #x01)
(defparameter +success+ #x00)
(defparameter +success-no-results+ #x01)
(defparameter +error+ #x02)
(defparameter +retry+ #x03)
(defparameter +authentication-error+ #x04)
(defparameter +success+ #x05)
(defparameter +hello+ #x06)
(defparameter +quit+ #xFF)

(defparameter +select+ #x00)
(defparameter +select-flat+ #x01)
(defparameter +<-+ #x02)
(defparameter +insert+ #x03)
(defparameter +do-query+ #x04)

(define-condition client-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
	     (with-slots (reason) error
	       (format stream "Vivace-graph client error: ~A." reason)))))

(define-condition authentication-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
	     (with-slots (reason) error
	       (format stream "Vivace-graph client authentication error: ~A." reason)))))

