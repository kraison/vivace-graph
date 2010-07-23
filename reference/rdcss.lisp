;(in-package #:vivace-graph)
(asdf:oos 'asdf:load-op 'cl-ppcre)

(defvar *log-stream* t) 

(defun logger (stream msg &rest args)
  (declare (ignore level))
  (format stream "~A~%" (apply #'format nil msg args)))

(defun current-thread ()
  sb-thread:*current-thread* )

(defconstant +casn-undecided+ 0)
(defconstant +casn-failed+ 1)
(defconstant +casn-succeeded+ 2)
(defvar *casn* nil)

(defun get-vector-addr (vector)
  (logandc2 (sb-kernel:get-lisp-obj-address vector) sb-vm:lowtag-mask))

(defmacro while (test &rest body)
  `(loop until (not ,test) do
	,@body))

(defmacro cas (place old new)
  `(sb-ext:compare-and-swap ,place ,old ,new))

(defstruct (rdcss-descriptor
	     (:conc-name rd-)
	     (:print-function print-rdcss-descriptor)
	     (:predicate rdcss-descriptor?))
  vector index control-vector control-index expected-val old-val new-val)

(defun print-rdcss-descriptor (rd stream depth)
  (declare (ignore depth))
  (format stream "#<RDCSS-DESCRIPTOR ~A -> ~A>" 
	  (get-vector-addr (rd-vector rd)) (rd-index rd)))

(defun new-rdcss-descriptor 
    (vector index control-vector control-index expected old new)
  (make-rdcss-descriptor :vector vector
			 :index index
			 :control-vector control-vector
			 :control-index control-index
			 :expected-val expected
			 :old-val old
			 :new-val new))

(defun rdcss-complete (d)
  (if (equal (svref (rd-control-vector d) (rd-control-index d))
	     (rd-expected-val d))
      (cas (svref (rd-vector d) (rd-index d)) d (rd-new-val d))
      (cas (svref (rd-vector d) (rd-index d)) d (rd-old-val d))))

(defun rdcss (d)
  (let ((r (cas (svref (rd-vector d) (rd-index d)) (rd-old-val d) d)))
    (while (rdcss-descriptor? r)
      (rdcss-complete r))
    (when (equal r (rd-old-val d))
      (rdcss-complete d))
    r))

(defun rdcss-read (vector index)
  (let ((r (svref vector index)))
    (while (rdcss-descriptor? r)
      (rdcss-complete r)
      (setq r (svref vector index)))
    r))

(defun make-safe-update (vector index old-value new-value)
  (make-array 5 :initial-contents
	      (list :safe-update
		    vector index old-value new-value)))

(defun safe-update? (u)
  (and (vectorp u) (not (stringp u)) (eq :safe-update (svref u 0))))

(defmacro update-vector (d) `(svref ,d 1))
(defmacro update-index (d) `(svref ,d 2))
(defmacro update-old (d) `(svref ,d 3))
(defmacro update-new (d) `(svref ,d 4))

(defun new-casn-descriptor (&rest updates)
  "Arguments should be vectors of four elements: vector index old-value new-value."
  (make-array 4 :initial-contents
	      (list :casn-descriptor
		    +casn-undecided+
		    (length updates)
		    updates)))

(defun casn-descriptor? (d)
  (and (vectorp d) (not (stringp d)) (eq :casn-descriptor (svref d 0))))

(defmacro casn-status (d) `(svref ,d 1))
(defmacro casn-count (d) `(svref ,d 2))
(defmacro casn-updates (d) `(svref ,d 3))

(defun casn-main (cd)
  (when (= (casn-status cd) +casn-undecided+)
    (let ((status +casn-succeeded+))
      (dotimes (i (casn-count cd))
	(tagbody retry-update
	   (let ((update (elt (casn-updates cd) i)))
	     ;;(logger sb-posix:log-debug  "Working on update ~A~%" i)
     (let ((value (rdcss (new-rdcss-descriptor 
				  (update-vector update)
				  (update-index update)
				  cd
				  1
				  +casn-undecided+
				  (update-old update)
				  cd))))
	       (if (casn-descriptor? value)
		   (when (not (eq value cd))
		     (casn-main value)
		     ;;(logger sb-posix:log-debug "~A Retrying update ~A~%" (current-thread) i)
		     (go retry-update))
		   (when (not (equal value (update-old update)))
		     (setq status +casn-failed+)))))))
      ;; cas does't like the casn-status macro.  must use svref instead.
      (cas (svref cd 1) +casn-undecided+ status))) 
  (let ((succeeded? (= (casn-status cd) +casn-succeeded+)))
    (dotimes (i (casn-count cd))
      (let ((update (elt (casn-updates cd) i)))
	(if succeeded?
	    (cas (svref (update-vector update) (update-index update)) 
		 cd 
		 (update-new update))
	    (cas (svref (update-vector update) (update-index update))
		 cd 
		 (update-old update)))))
    ;;(logger sb-posix:log-debug "~A done." (current-thread))
    succeeded?))

(defun casn (cd)
  (let ((objects (remove-duplicates
		  (mapcar #'(lambda (update) (update-vector update))
			  (casn-updates cd)))))
    (sb-sys:with-pinned-objects (objects)
      (setf (casn-updates cd) 
	    (sort (casn-updates cd)
		  #'(lambda (x y)
		      (and (< (first x) (first y))
			   (< (second x) (second y))))
		  :key #'(lambda (update)
			   (list
			    (get-vector-addr (update-vector update))
			    (update-index update)))))
      (casn-main cd))))

(defun casn-read (vector index)
  (let ((r (rdcss-read vector index)))
    (while (casn-descriptor? r)
      ;;(logger sb-posix:log-debug "~A looping for ~A/~A in casn-read" (current-thread) 
      ;;(casn-status r) (casn-count r))
      (casn-main r)
      (setq r (rdcss-read vector index)))
    r))

(defun casn-set (vector index old new)
  (if (casn-descriptor? *casn*)
      (progn
	(push (make-safe-update vector index old new) (casn-updates *casn*))
	(incf (casn-count *casn*)))
      (error "CASN-SET must be called within the body of with-casn")))

(defmacro with-casn (&body body)
  `(let ((*casn* (new-casn-descriptor)))
     (progn
       ,@body)
     (casn *casn*)))

(defun casn-test (&optional (size 100))
  (let ((*log-stream* (open "/var/tmp/vivace.log" 
			    :direction :output 
			    :if-exists :overwrite
			    :if-does-not-exist :create)))
    (sb-profile:reset)
    (sb-profile:profile get-vector-addr 
			print-rdcss-descriptor 
			new-rdcss-descriptor  
			make-rdcss-descriptor
			rdcss-complete
			rdcss
			rdcss-read
			safe-update
			safe-update?
			new-casn-descriptor
			casn-descriptor?
			casn-main
			casn
			casn-read
			casn-set)
    (let ((v (make-array size :initial-element 0)))
      (let ((t1 (sb-thread:make-thread
		 #'(lambda ()
		     (with-casn
		       (dotimes (i size)
			 (casn-set v i (casn-read v i) (1+ i)))))
		 :name "thread1"))
	    (t2 (sb-thread:make-thread
		 #'(lambda ()
		     (with-casn
		       (dotimes (i size)
			 (casn-set v i (casn-read v i) (+ 2 i)))))
		 :name "thread2"))
	    (t3 (sb-thread:make-thread
		 #'(lambda ()
		     (with-casn
		       (dotimes (i size)
			 (casn-set v i (casn-read v i) (+ 3 i)))))
		 :name "thread3")))
	(format t "V 0 = ~A~%" (casn-read v 0))
	(sb-thread:join-thread t1)
	(format t "V 0 = ~A~%" (casn-read v 0))
	(sb-thread:join-thread t2)
	(format t "V 0 = ~A~%" (casn-read v 0))
	(sb-thread:join-thread t3)
	(format t "V 0 = ~A~%" (casn-read v 0)))
      (format t "v[0] = ~A~%" (svref v 0)))
    (close *log-stream*)
    (sb-profile:report)))


(defun stop-test ()
  (dolist (thr (sb-thread:list-all-threads))
    (when (cl-ppcre:scan "^thread[123]$" (sb-thread:thread-name thr))
      (sb-thread:destroy-thread thr))))

