(in-package #:cl-user)

(defpackage #:vivace-graph
  (:use #:cl #:cffi #:bordeaux-threads #:tokyo-cabinet #:tokyo-cabinet-ffi)
  (:export #:create-graph
	   #:lookup-node
	   #:<-
	   #:prolog
	   #:query
	   #:traverse
	   ))
