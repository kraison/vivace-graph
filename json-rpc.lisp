(in-package #:vivace-graph)

;;;;; Some code in this file is subject to this license:
;;;;;
;;;;; Copyright (c) 2010, Martin Loetzsch
;;;;; All rights reserved.

;;;;; Redistribution and use in source and binary forms, with or
;;;;; without modification, are permitted provided that the following
;;;;; conditions are met:

;;;;;  Redistributions of source code must retain the above copyright
;;;;;  notice, this list of conditions and the following disclaimer.

;;;;;  Redistributions in binary form must reproduce the above
;;;;;  copyright notice, this list of conditions and the following
;;;;;  disclaimer in the documentation and/or other materials provided
;;;;;  with the distribution.

;;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;;;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;;;;; THE POSSIBILITY OF SUCH DAMAGE.


(defclass ajax-processor ()
  ((lisp-fns 
    :accessor lisp-fns :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "Maps the symbol names of the exported functions to
                    their symbols")
   (js-fns
    :accessor js-fns :initform (make-hash-table :test #'equal)
    :type hash-table
    :documentation "Maps the symbol names of the exported functions to
                    a javascript code that can call the function from
                    within the client page")
   (server-uri 
    :initarg :server-uri :initform "/ajax" :accessor server-uri
    :type string
    :documentation "The uri which is used to handle ajax request")
   (content-type :initarg :content-type :type string
     :accessor content-type :initform "text/x-json; charset=\"utf-8\""
     :documentation "The http content type that is sent with each response")
   (reply-external-format 
    :initarg :reply-external-format :type flexi-streams::external-format
    :accessor reply-external-format :initform hunchentoot::+utf-8+
    :documentation "The format for the character output stream"))
  (:documentation "Maintains a list of lisp function that can be
                   called from a client page."))


(defun make-js-symbol (symbol)
  "helper function for making 'foo_bar_' out of 'foo-bar?' "
  (loop with string = (string-downcase symbol)
     for c across "?-<>"
     do (setf string (substitute #\_ c string))
     finally (return string)))


(defmacro defun-ajax (name params (processor) &body body)
  "Declares a defun that can be called from a client page.
Example: (defun-ajax func1 (arg1 arg2) (*ajax-processor*)
   (do-stuff))"
  (let ((js-fn (format nil "

function ~a (~{~a, ~}callback) {
    ajax_call('~a', callback, ~2:*[~{~a~^, ~}]);
}" 
                       (concatenate 'string "ajax_" (make-js-symbol name))
                       (mapcar #'make-js-symbol params) 
                       name)))
    `(progn
       (defun ,name (&key ,@params) ,@body)
       (setf (gethash (symbol-name ',name) (lisp-fns ,processor)) ',name)
       (setf (gethash (symbol-name ',name) (js-fns ,processor)) ',js-fn))))



(defun generate-prologue (processor)
  "Creates a <script> ... </script> html element that contains all the
   client-side javascript code for the ajax communication. Include this 
   script in the <head> </head> of each html page"
  (apply #'concatenate 'string
         `("<script type='text/javascript'>
//<![CDATA[ 
function fetchURI(uri, params, callback) {
  var request;
  if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
  else {
    try { request = new ActiveXObject(\"Msxml2.XMLHTTP\"); } catch (e) {
      try { request = new ActiveXObject(\"Microsoft.XMLHTTP\"); } catch (ee) {
        request = null;
      }}}
  if (!request) alert(\"Browser couldn't make a request object.\");

  request.open('POST', uri, true);
  request.onreadystatechange = function() {
    if (request.readyState != 4) return;
    if (((request.status>=200) && (request.status<300)) || (request.status == 304)) {
      var data = request.responseText;
      if (callback!=null) { callback(data); }
    }
    else { 
      alert('Error while fetching URI ' + uri);
    }
  }
  request.setRequestHeader(\"Content-type\", \"application/x-www-form-urlencoded\");
  request.setRequestHeader(\"Content-length\", params.length);
  request.setRequestHeader(\"Connection\", \"close\");
  request.send(params);
  delete request;
}

function ajax_call(func, callback, args) {
  var uri = '" ,(server-uri processor) "/' + encodeURIComponent(func) + '/';
  var i;
  var params;
  if (args.length > 0) {
    for (i = 0; i < args.length; ++i) {
      if (i > 0) { params += '&' };
      params += 'arg' + i + '=' + encodeURIComponent(args[i]);
    }
  }
  fetchURI(uri, params, callback);
}"
  ,@(loop for js being the hash-values of (js-fns processor)
       collect js)
  "
//]]>
</script>")))


(defun encode-json-for-web (&key result error id)
  (json:encode-json-to-string `((result . ,result)
                                (error . ,error)
                                (id . ,id))))

(defun make-rpc-response (processor &key result error id)
  "Adjusted for our custom response structure.  Called by call-lisp-function to format the 
generated response."
  (setf (hunchentoot:reply-external-format*) (reply-external-format processor))
  (setf (hunchentoot:content-type*) (content-type processor))
  (hunchentoot:no-cache)
  (encode-json-for-web :result result :error error :id id))

(defun call-lisp-function (processor)
  "This is called from hunchentoot on each ajax request. It parses the parameters from the http 
request, calls the lisp function and returns the response."
  (let* ((fn-name (string-trim "/" (subseq (hunchentoot:script-name* hunchentoot:*request*)
					   (length (server-uri processor)))))
	 (fn (gethash fn-name (lisp-fns processor)))
	 (id (uuid:print-bytes nil (make-uuid)))
	 ;;(args (mapcar #'cdr (hunchentoot:post-parameters* hunchentoot:*request*))))
	 (args (hunchentoot:post-parameters* hunchentoot:*request*)))
    (handler-case
	(restart-case
	    (if fn
		(progn
		  (let ((args-plist 
			 (flatten
			  (mapcar #'(lambda (i)
				      (list (if (symbolp (car i))
						(intern (symbol-name (car i)) :keyword) 
						(intern (string-upcase (car i)) :keyword))
					    (cdr i)))
				  args))))
		    (make-rpc-response processor 
				       :id id 
				       :result (restart-case (apply fn args-plist)
						 (use-value (value) value)))))
		(make-rpc-response processor 
				   :id id :error (format nil "Function not found: ~a." fn)))
	  (send-error (error-message)
	    (make-rpc-response processor :id id :error error-message))
	  (send-nothing () nil)
	  (send-internal-error ()
	    (make-rpc-response processor
			       :id id
			       :error "An internal error occurred on the server.")))
      (error (condition)
	(make-rpc-response processor
			   :id nil
			   :error (format nil "Bad input ~a: ~a" args condition))))))

(defun create-ajax-dispatcher (processor)
  "Creates a hunchentoot dispatcher for an ajax processor"
  (hunchentoot:create-prefix-dispatcher (server-uri processor)
					#'(lambda () (call-lisp-function processor))))

