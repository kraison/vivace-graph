(in-package #:vivace-graph)

(defparameter *prolog-rpc* (make-instance 'ajax-processor :server-uri "/prolog"))
(defparameter *prolog-server* nil)

(defun valid-prolog-query? (form)
  (case (first form)
    (select t)
    (select-flat t)
    (<- t)
    (select-bind-list t)
    (do-query t)
    (otherwise nil)))

(defun json-eval (string)
  (in-package #:vivace-graph)
  (let ((form (read-from-string string)))
    (logger :err "GOT FORM ~A" form)
    (unless (valid-prolog-query? form)
      (error "Error: '~A' is not a valid prolog query." string))
    (eval form)))

(defun-ajax query (graph query) (*prolog-rpc*)
  (logger :err "RPC TEST GOT: ~A / ~A" graph query)
  (handler-case
      (let ((*graph* (lookup-graph graph)))
	(if (graph? *graph*)
	    (progn
	      (logger :err "GRAPH ~A IS ~A / QUERY IS ~A" graph *graph* query)
	      (let ((result (json-eval query)))
		(logger :err "Sending ~A" result)
		(json:encode-json-to-string (list result))))
	    (progn
	      (logger :err "[\"Unknown graph: ~A\"]" graph)
	      (format nil "[\"Unknown graph: ~A\"]" graph))))
    (error (c)
      (json:encode-json-to-string (list (format nil "~A" c))))))
