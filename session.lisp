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

(defstruct (v-session
             (:print-function print-session)
             (:predicate session?)
             (:conc-name v-session-))
  (uuid (uuid:make-v1-uuid))
  (thread nil)
  (finished? nil)
  (stream nil)
  (host nil)
  (socket nil)
  (buffer (make-array 0
                      :element-type '(unsigned-byte 8)
                      :adjustable t
                      :fill-pointer t))
  (query-cache (make-hash-table :test 'equalp))
  (history nil))

(defun print-session (session stream depth)
  (declare (ignore depth))
  (format stream "#<SESSION ~A FROM ~A>" (v-session-uuid session) (v-session-host session)))

(defun apply-sessions (func)
  (sb-ext:with-locked-hash-table (*sessions*)
    (maphash #'(lambda (id session)
                 (funcall func session))
             *sessions*)))

(defun remove-thread (thread)
  (with-recursive-lock-held (*thread-list-lock*)
    (setf *thread-list* (delete thread *thread-list*))))

(defun add-thread (thread)
  (with-recursive-lock-held (*thread-list-lock*)
    (push thread *thread-list*)))

(defun start-session (stream socket)
  (let ((session (make-v-session :stream stream 
				 :socket socket 
				 :host (ip-to-string (usocket:get-peer-name socket)))))
    (setf (gethash (v-session-uuid session) *sessions*) session)))

(defmethod initiate-session ((session v-session))
  session)

(defmethod shutdown-session ((session v-session))
  "Close socket and remove its event handler and clear the session object."
  (logger :debug "terminating ~A" session)
  (ignore-errors (usocket:socket-close (v-session-socket session)))
  (remove-thread (v-session-thread session))
  (remhash (v-session-uuid session) *sessions*)
  (logger :debug "~A terminated" session)
  (setf session nil))

(defun kill-all-sessions ()
  (apply-sessions #'shutdown-session))

