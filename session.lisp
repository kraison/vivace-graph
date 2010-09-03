(in-package #:vivace-graph)

(defvar *sessions* (make-hash-table :test 'equalp :synchronized t))
(defvar *session* nil)
(defvar *thread-list* nil)
(defvar *thread-list-lock* (make-recursive-lock))
(defparameter +buflen+ 10240)

(define-condition session-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason)
                 error
               (format stream "Session error: ~A." reason)))))

(defstruct (session
             (:print-function print-session)
             (:predicate session?)
             (:conc-name session-))
  (uuid (uuid:make-v1-uuid))
  (thread nil)
  (lock (make-recursive-lock))
  (finished? nil)
  (stream nil)
  (host nil)
  (socket nil)
  (buffer (make-array 0
                      :element-type '(unsigned-byte 8)
                      :adjustable nil
                      :fill-pointer t))
  (error-handler #'(lambda (s)
                     (logger :err "DEFAULT ERROR HANDLER: ERROR IN SESSION ~A." s)))
  (cache (make-hash-table :test 'equalp))
  (history nil))

(defun print-session (session stream depth)
  (declare (ignore depth))
  (format stream "#<SESSION ~A FROM ~A>" (session-uuid session) (session-host session)))

(defun apply-sessions (func)
  (sb-ext:with-locked-hash-table (*sessions*)
    (maphash #'(lambda (id session)
                 (funcall func session))
             *sessions*)))

(defun kill-all-sessions ()
  (apply-sessions #'shutdown-session))

(defun remove-thread (thread)
  (with-recursive-lock-held (*thread-list-lock*)
    (setf *thread-list* (remove thread *thread-list*))))

(defun add-thread (thread)
  (with-recursive-lock-held (*thread-list-lock*)
    (push thread *thread-list*)))

(defun start-session (stream socket)
  (let ((session (make-session :stream stream 
			       :socket socket 
			       :host (ip-to-string (usocket:get-peer-name socket)))))
    (setf (gethash (session-uuid session) *sessions*) session)))

(defmethod initiate-session ((session session))
  )

(defmethod shutdown-session ((session session))
  "Close socket and remove its event handler and clear the session object."
  (logger :debug "terminating ~A" session)
  (ignore-errors (usocket:socket-close (session-socket session)))
  (remove-thread (session-thread session))
  (logger :debug "~A terminated" session)
  (remhash (session-uuid session) *sessions*)
  (setf session nil))

