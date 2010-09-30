(in-package #:vivace-graph)

(setq *graph-table* (make-hash-table :synchronized t :test 'equalp))

(defstruct (graph
	     (:predicate graph?)
	     (:conc-name nil)
	     (:print-function print-graph))
  (graph-uuid (make-uuid))
  (graph-name nil)
  (shutdown? nil)
  (graph-location #P".")
  (namespaces (make-hash-table :synchronized t :test 'equalp))
  (namespace-db nil)
  (triple-db nil)
  (deleted-triple-db nil)
  (predicate-db nil)
  (rule-db nil)
  (full-text-idx nil)
  (rule-idx (make-hash-table :synchronized t :test 'equal))
  (templates (make-hash-table :synchronized t))
  (functors (make-hash-table :synchronized t))
  (rule-cache (make-hash-table :synchronized t))
  (predicate-cache (make-hash-table :synchronized t :test 'equal))
  ;;(node-cache (make-hash-table :test 'equal :synchronized t))
  ;;(triple-cache (make-hash-table :test 'equal :synchronized t))
  (triple-cache (make-lru-cache :test 'equal :max-size 100000))
  ;;(deleted-triple-cache (make-hash-table :test 'equal :synchronized t))
  (deleted-triple-cache (make-lru-cache :test 'equal :max-size 10000))
  (production-pq (make-skip-pq :key-equal #'timestamp= :comparison #'timestamp>
			       :head-value (make-timestamp :day -1000000 :sec 0 :nsec 0)))
  (delete-queue (sb-concurrency:make-queue))
  (needs-indexing-q (sb-concurrency:make-queue))
  (worker-thread nil))

(defun print-graph (graph stream depth)
  (declare (ignore depth))
  (format stream "#<GRAPH #~A: ~A>" (graph-uuid graph) (graph-name graph)))

(defgeneric needs-indexing? (graph))
(defgeneric shutdown-graph (graph))

(defmethod needs-indexing? ((graph graph))
  "Are there pending indexing jobs to be done for GRAPH?"
  (not (sb-concurrency:queue-empty-p (needs-indexing-q graph))))

(defun lookup-graph (name)
  (gethash name *graph-table*))

(defun register-namespace (short-name uri &key errorp)
  (declare (ignore errorp))
  (set-phash (namespace-db *graph*) short-name uri :mode :replace)
  (set-phash (namespace-db *graph*) uri short-name :mode :replace)
  (setf (gethash uri (namespaces *graph*)) short-name)
  (setf (gethash short-name (namespaces *graph*)) uri))

(defun display-namespaces ()
  (maphash #'(lambda (k v) 
	       (unless (uri? k)
		 (format t "~A~A=> ~A~%" k #\Tab v)))
	   (namespaces *graph*)))

(defun get-namespace (short-name)
  (or (gethash short-name (namespaces *graph*))
      (let ((uri (get-phash (namespace-db *graph*) short-name)))
	(when uri
	  (setf (gethash short-name (namespaces *graph*)) uri)))))

(defun read-node (stream)
  (with-output-to-string (out)
    (loop 
       for c = (read-char stream nil :eof)
       do
       (if (member c '(#\Space #\Newline #\Tab #\Return #\)))
	   (progn
	     (unread-char c stream)
	     (return))
	   (format out "~A" c)))))

(defun read-namespace (stream char)
  (declare (ignore char))
  (let ((c (read-char stream nil :eof)))
    (cond ((eql c #\") ;; This is a string, treat it as such
	   (funcall (get-macro-character #\") stream #\"))
	  ((eql c #\<) ;; This is a URI, treat it as such
	   (format nil "<~A>" (funcall (get-macro-character #\") stream #\>)))
	  (t
	   (let ((uri (get-namespace 
		       (with-output-to-string (key-stream)
			 (loop until (or (eq :eof c) (eql c #\:)) do
			      (format key-stream "~A" c)
			      (setq c (read-char stream nil :eof)))))))
	     (if uri
		 (format nil "<~A~A>" uri (read-node stream))
		 nil))))))

(defun enable-!-reader ()
  (set-macro-character #\! #'read-namespace))

(defun disable-namespace-reader ()
  (set-macro-character #\! nil))

(defun namespace-reader-enabled? ()
  (get-macro-character #\!))

(defun shorten-namespace (thing)
  thing)
