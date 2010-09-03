(in-package #:vivace-graph)

(defun start-json-rpc (&optional (port 9999))
  (let ((rpc-server (hunchentoot:start (make-instance 'hunchentoot:acceptor :port port))))
    (setq hunchentoot:*dispatch-table* 
	  (list 'hunchentoot:dispatch-easy-handlers 
		(hunchentoot:create-static-file-dispatcher-and-handler 
		 "/prolog.html"
		 "/home/raison/work/vivace-graph/prolog.html")
		(create-ajax-dispatcher *prolog-rpc*)))
    (setq hunchentoot:*session-max-time* 28800)
    (setq *prolog-server* rpc-server)))

(defun stop-json-rpc (&optional (server *prolog-server*))
  (hunchentoot:stop server))

