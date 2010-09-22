(in-package #:vivace-graph)

(defparameter +max-bytes+ 40960)
(defparameter +magic-byte+ #x2A)
(defparameter *response-types* (make-hash-table))
(setf (gethash #x00 *response-types*) :success
      (gethash #x01 *response-types*) :success-no-results
      (gethash #x02 *response-types*) :error
      (gethash #x03 *response-types*) :retry
      (gethash #x04 *response-types*) :authentication-error
      (gethash #xFF *response-types*) :quit)

(defun extract-query-length (stream)
  (let ((int 0))
    (dotimes (i 4)
      (setq int (dpb (read-byte stream nil 0) (byte 8 (* i 8)) int)))
    int))

(defmethod decode-and-execute ((session v-session))
  "Decode and execute a binary query.  First, extract the query length (bytes 1 - 4), then the query
   type. Query types are specified by the 5th byte read from the wire:
   #x00 'select
   #x01 'select-flat
   #x02 '<-
   #x03 'insert
   #x04 'do-query
   #xFF 'quit
   Returns 4 values: a header pointer and length and a response pointer and length.  Both pointers
   must be freed by the calling function!"
  (setf (v-session-buffer-length session) (extract-query-length (v-session-stream session)))
  (case (read-byte (v-session-stream session) nil :eof)
    (#x00 (binary-select session))
    (#x01 (binary-select-flat session))
    (#x02 (binary-<- session))
    (#x03 (binary-insert session))
    (#x04 (binary-do-query session))
    (#xFF 
     (values 
      (foreign-alloc :unsigned-char :count 2 :initial-contents `(,+magic-byte+ #xFF)) 2
      (foreign-alloc :unsigned-char :count 1 :initial-element #xFF) 1))
    (:eof (values nil 0 nil 0 t))
    (otherwise
     (values
      (foreign-alloc :unsigned-char :count 2 :initial-contents `(,+magic-byte+ #x02)) 2
      (foreign-alloc :unsigned-char :count 1 :initial-element #xFF) 1)))) ;; FIXME: make error msg

(defmethod data-received-handler ((session v-session))
  (let ((magic-byte (read-byte (v-session-stream session) nil :eof)))
    (if (eql magic-byte +magic-byte+)
	(multiple-value-bind (header hlen response rlen quit?) (decode-and-execute session)
	  (unwind-protect
	       (progn
		 (when (pointerp header)
		   (dotimes (i hlen)
		     (logger :debug "Sending header byte ~A: ~A to client" i 
			     (mem-aref header :unsigned-char i))
		     (write-byte (mem-aref response :unsigned-char i) 
				 (v-session-stream session)))
		   (force-output (v-session-stream session)))
		 (when (pointerp response)
		   (dotimes (i rlen)
		     (logger :debug "Sending data byte ~A: ~A to client" i 
			     (mem-aref response :unsigned-char i))
		     (write-byte (mem-aref response :unsigned-char i) 
				 (v-session-stream session)))
		   (force-output (v-session-stream session)))
		 (when quit? (setf (v-session-finished? session) t)))
	    (progn
	      (when (pointerp header) (foreign-free header))
	      (when (pointerp response) (foreign-free response)))))
	(progn
	  (setf (v-session-finished? session) t)
	  (format nil "Invalid magic byte: ~A" magic-byte)))))
