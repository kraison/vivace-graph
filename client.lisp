(in-package #:vivace-graph)

(defstruct (session
	     (:predicate session?))
  socket
  stream
  vars
  query
  pointer
  version)

(defun authenticate (username password graph)
  (declare (ignore username password graph))
  t)

(defmethod get-byte ((socket usocket:socket))
  (read-byte (usocket:socket-stream socket) nil :eof))

(defmethod get-byte ((session session))
  (read-byte (session-stream session) nil :eof))

(defun connect-to-server (ip port)
  (handler-case
      (let ((socket (usocket:socket-connect 
		     ip port :protocol :tcp :element-type '(unsigned-byte 8) :timeout 600)))
	(if socket
	    (progn
	      (write-byte +magic-byte+ (socket-stream socket))
	      (write-byte +version+ (socket-stream socket))
	      (force-output (socket-stream socket))
	      (let ((response (get-byte socket)))
		(cond ((eq response +ack+) t)
		      ((eq response :eof) (error "Server disconnected!"))
		      (t (error "Invalid ACK from server: ~A" response))))
	      (let ((magic-byte (get-byte socket)) (version (get-byte socket)))
		(if (and (eq +magic-byte+ magic-byte) (eq +version+ version))
		    (make-session :socket socket :version +version+ :stream (socket-stream socket))
		    (error "Unknown greeting from server: ~A ~A" magic-byte version))))
	    (error "Unable to connect to ~A:~A" ip port)))
    (error (condition)
      (error 'client-error :reason condition))))

(defmethod disconnect-from-server ((session session))
  (handler-case
      (progn
	(write-byte +magic-byte+ (session-stream session))
	(write-byte +quit+ (session-stream session))
	(force-output (session-stream session))
	(let ((response (get-byte session)))
	  (cond ((or (eq response +ack+) (eq response :eof))
		 (ignore-errors (close (session-stream session)))
		 (setf (session-socket session) nil
		       (session-stream session) nil))
		(t (error "Invalid ACK from server on request to quit: ~A" response)))))
    (error (condition)
      (ignore-errors (close (session-stream session)))
      (error 'client-error :reason condition))))

(defmethod set-vars ((session session) vars)
  (setf (session-vars session) (make-hash-table))
  (loop for i from 0 to (list-length vars) do
       (setf (gethash i (session-vars session)) (nth i vars)
	     (gethash (nth i vars) (session-vars session)) i)))

(defmethod select ((session session) vars &rest goals)
  (set-vars session vars)
  (let ((goals (substitute-vars session vars goals)))
    
