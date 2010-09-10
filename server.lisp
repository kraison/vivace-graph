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

(defun start-vivace-graph (&key (config-file "/etc/vivace-graph.ini"))
  (unless (probe-file config-file)
    (error "Configuration file ~A does not exist or is not readable." config-file)
    (sb-ext:quit :unix-status 255))
  (let ((config (py-configparser:make-config)))
    (py-configparser:read-files config (list config-file))
    (let ((json-port (py-configparser:get-option config "default" "json-rpc-port"))
	  (bin-port (py-configparser:get-option config "default" "binary-port"))
	  (slime-port (py-configparser:get-option config "default" "slime-port"))
	  (pid-file (py-configparser:get-option config "default" "pid-file")))
      (when (probe-file pid-file)
	(with-open-file (in pid-file)
	  (let ((pid (read-line in nil :eof)))
	    (error "PID file ~A exists.  Am I already running as ~a?" pid-file pid)))
	(sb-ext:quit :unix-status 255))
      (handler-case
	  (start-json-rpc :port (parse-integer json-port))
	(SB-INT:SIMPLE-PARSE-ERROR (c)
	  (declare (ignore c))
	  (format t "JSON-RPC port '~A' is not a valid integer" json-port)
	  (sb-ext:quit :unix-status 255)))
      (handler-case
	  (start-binary-listener :port (parse-integer bin-port))
	(SB-INT:SIMPLE-PARSE-ERROR (c)
	  (declare (ignore c))
	  (format t "Binary port '~A' is not a valid integer" bin-port)
	  (sb-ext:quit :unix-status 255)))
      config)))
