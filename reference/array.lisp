;(in-package #:vivace-graph)

(defvar *a*)

(defun create-lisp-mem-from-array (array file)
  (declare (optimize (speed 1)))
  (with-open-file (map-file file
			    :direction :io
			    :if-exists :supersede
			    :element-type '(unsigned-byte 64))
    ;; Header
    (write-byte sb-vm::simple-array-unsigned-byte-60-widetag map-file)
    ;; Array size placeholder
    (write-byte 8 map-file)
    (loop for in across array
       with count = 0
       do
       (let ((data (ash in sb-vm::n-fixnum-tag-bits)))
	 (incf count)
	 (write-byte data map-file))
       finally
       (file-position map-file 1)
       (write-byte (ash count sb-vm:n-fixnum-tag-bits)
		   map-file))))

(defun load-lisp-mem (map-file variable)
  (let* ((file-stream (open map-file
			    :direction :io 
			    :if-exists :append
			    :if-does-not-exist :create))
	 (sap (sb-posix:mmap 
	       nil
	       (file-length file-stream)
	       (boole boole-ior sb-posix:prot-read sb-posix:prot-write)
	       sb-posix:map-shared
	       (sb-sys:fd-stream-fd file-stream)
	       0))
	 (addr (logior (sb-sys:sap-int sap)
		       sb-vm:other-pointer-lowtag)))
    (setf (symbol-value variable) (sb-kernel:%make-lisp-obj addr))
    (values sap file-stream)))

(defmethod munmap-file (pointer stream &key (save? nil) (sync sb-posix:ms-async))
  (when save?
    (sb-posix:msync pointer (file-length stream) sync))
  (sb-posix:munmap pointer (file-length stream))
  (close stream)
  nil)

