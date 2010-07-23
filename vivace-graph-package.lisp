(in-package #:cl-user)

(defpackage #:vivace-graph
  (:use #:cl #:cffi #:bordeaux-threads)
  (:export #:create-graph
	   #:lookup-node
	   #:<-
	   #:prolog
	   #:query
	   #:traverse
	   ))
