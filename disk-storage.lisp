(in-package #:vivace-graph)
;(asdf:oos 'asdf:load-op 'cffi)
;(cffi:defctype size :unsigned-int)

(defstruct (mapped-file
	     (:predicate mapped-file?))
  path pointer stream alien-pointer)

(defmethod mapped-file-length ((mapped-file mapped-file))
  (if (open-stream-p (mapped-file-stream mapped-file))
      (file-length (mapped-file-stream mapped-file))))

(defun mmap-file (file &key (create? t) (size (* 4096 25600)))
  "Use mmap() to map FILE into memory."
  (when (and (not create?) (not (probe-file file)))
    (error "mmap-file: ~A does not exist and create? is not true." file))
  (let* ((file-stream (open file
			    :direction :io 
			    :if-exists :supersede
			    :if-does-not-exist :create))
	 (file-size (file-length file-stream)))
    (when (or create? (> size file-size))
      (sb-posix:lseek (sb-sys:fd-stream-fd file-stream) (1- size) sb-posix:seek-set)
      (cffi:with-foreign-pointer-as-string ((buf buf-size) 255)
	(cffi:foreign-funcall "write" 
			      :int (sb-sys:fd-stream-fd file-stream)
			      :pointer buf 
			      size 1)))
    (let* ((pointer (sb-posix:mmap nil
				   (file-length file-stream)
				   (boole boole-ior sb-posix:prot-read sb-posix:prot-write)
				   sb-posix:map-shared 
				   (sb-sys:fd-stream-fd file-stream)
				   0))
	   (alien-pointer (sb-alien:sap-alien pointer (* sb-alien:unsigned-char))))
      (make-mapped-file :path (truename file)
			:stream file-stream 
			:pointer pointer
			:alien-pointer alien-pointer))))

(defmethod munmap-file ((mapped-file mapped-file) &key (save? nil) (sync sb-posix:ms-async))
  (when save?
    (sb-posix:msync (mapped-file-pointer mapped-file) 
		    (file-length (mapped-file-stream mapped-file)) sync))
  (sb-posix:munmap (mapped-file-pointer mapped-file) 
		   (file-length (mapped-file-stream mapped-file)))
  (close (mapped-file-stream mapped-file))
  (setf (mapped-file-pointer mapped-file) nil
	(mapped-file-alien-pointer mapped-file) nil
	(mapped-file-stream mapped-file) nil)
  nil)

(defmethod extend-mapped-file ((mapped-file mapped-file) (length number) &key save?
			       (sync sb-posix:ms-async))
  (when save?
    (sb-posix:msync (mapped-file-pointer mapped-file)
		    (file-length (mapped-file-stream mapped-file)) sync))
  (sb-posix:munmap (mapped-file-pointer mapped-file) 
		   (file-length (mapped-file-stream mapped-file)))
  (sb-posix:lseek (sb-sys:fd-stream-fd (mapped-file-stream mapped-file)) 
		  (+ (file-length (mapped-file-stream mapped-file)) length)
		  sb-posix:seek-set)
  (cffi:with-foreign-pointer-as-string ((buf buf-size) 255)
    (cffi:foreign-funcall "write" 
			  :int (sb-sys:fd-stream-fd (mapped-file-stream mapped-file)) 
			  :pointer buf 
			  size 1))
  (setf (mapped-file-pointer mapped-file)
	(sb-posix:mmap nil
		       (file-length (mapped-file-stream mapped-file))
		       (boole boole-ior sb-posix:prot-read sb-posix:prot-write)
		       sb-posix:map-shared
		       (sb-sys:fd-stream-fd (mapped-file-stream mapped-file))
		       0)
	(mapped-file-alien-pointer mapped-file)
	(sb-alien:sap-alien (mapped-file-pointer mapped-file) (* sb-alien:unsigned-char)))
  mapped-file)

(defmethod set-byte ((mapped-file mapped-file) offset byte)
  (setf (sb-alien:deref (mapped-file-alien-pointer mapped-file) offset) byte))

(defmethod set-bytes ((mapped-file mapped-file) offset bytes)
  (dotimes (i (length bytes))
    (setf (sb-alien:deref (mapped-file-alien-pointer mapped-file) (+ i offset))
	  (elt bytes i))))

(defmethod get-byte ((mapped-file mapped-file) offset)
  (sb-alien:deref (mapped-file-alien-pointer mapped-file) offset))

(defmethod get-bytes ((mapped-file mapped-file) offset length)
  (let ((bytes (make-array length :adjustable t)))
    (dotimes (i length)
      (setf (aref bytes i)
	    (sb-alien:deref (mapped-file-alien-pointer mapped-file) (+ i offset))))
    bytes))

(defmethod reserve-contiguous-space ((mapped-file mapped-file) length)
  )
