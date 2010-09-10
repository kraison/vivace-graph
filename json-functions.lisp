(in-package #:vivace-graph)

(defparameter *prolog-rpc* (make-instance 'ajax-processor :server-uri "/prolog"))
(defparameter *prolog-server* nil)

(defun json-eval (string)
  (in-package #:vivace-graph)
  (let ((form (read-from-string string)))
    (logger :debug "JSON-EVAL GOT FORM ~A" form)
    (unless (valid-prolog-query? form)
      (error "Error: '~A' is not a valid prolog query." string))
    (eval form)))

(defun-ajax query (graph query) (*prolog-rpc*)
  (logger :debug "QUERY GOT: ~A / ~A" graph query)
  (handler-case
      (let ((*graph* (lookup-graph graph)))
	(if (graph? *graph*)
	    (progn
	      (logger :debug "QUERY: GRAPH ~A IS ~A / QUERY IS ~A" graph *graph* query)
	      (let ((result (json-eval query)))
		(logger :debug "QUERY: Sending ~A" result)
		(typecase result
		  (triple (as-list result))
		  (otherwise result))))
	    (invoke-restart 'send-error (format nil "unknown graph ~A" graph))))
    (error (c)
      (invoke-restart 'send-error (format nil "~A" c)))))
