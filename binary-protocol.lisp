(in-package #:vivace-graph)

(defparameter +max-bytes+ 40960)
(defparameter +magic-byte+ #x2A)
(defparameter *query-types* (make-hash-table))
(setf (gethash #x00 *query-types*) 'select
      (gethash #x01 *query-types*) 'select-flat
      (gethash #x02 *query-types*) 'select-bind-list
      (gethash #x03 *query-types*) '<-
      (gethash #x04 *query-types*) 'insert
      (gethash #x05 *query-types*) 'do-query
      (gethash #xFF *query-types*) 'quit)

(defun decode-and-execute (vec)
  (let ((query-type (gethash (aref vec 0) *query-types*)))
    (logger :debug "Got query type of ~A for byte ~A" query-type (aref vec 0))
    (values nil 0 t)))

(defun extract-query-length (stream)
  (let ((int 0))
    (dotimes (i 4)
      (setq int (dpb (read-byte stream nil 0) (byte 8 (* i 8)) int)))
    int))

(defmethod data-received-handler ((session v-session))
  (let ((buffer (v-session-buffer session))
	(magic-byte (read-byte (v-session-stream session) nil :eof)))
    (if (eql magic-byte +magic-byte+)
	(let ((query-length (extract-query-length (v-session-stream session))))
	  (setf (fill-pointer buffer) 0)
	  (dotimes (i query-length)
	    (let ((byte (read-byte (v-session-stream session) nil :eof)))
	      (if (eq byte :eof)
		  (progn
		    (setf (v-session-finished? session) t)
		    (return-from data-received-handler 
		      "Got EOF on stream before all bytes were read."))
		  (vector-push-extend byte buffer))))
	  (multiple-value-bind (response rlen quit?) (decode-and-execute buffer)
	    (unwind-protect
		 (progn
		   (when (pointerp response)
		     (dotimes (i rlen)
		       (logger :debug "Sending byte ~A: ~A to client" i 
			       (mem-aref response :unsigned-char i))
		       (write-byte (mem-aref response :unsigned-char i) 
				   (v-session-stream session)))
		     (force-output (v-session-stream session)))
		   (when quit? (setf (v-session-finished? session) t)))
	      (when (pointerp response) (foreign-free response)))))
	(progn
	  (setf (v-session-finished? session) t)
	  (format nil "Invalid magic byte: ~A" magic-byte)))))
