(in-package #:cl-user)

(defpackage #:vivace-graph
  (:use #:cl #:bordeaux-threads #:tokyo-cabinet #:tokyo-cabinet-ffi)
  (:export #:create-graph
	   #:lookup-node
	   #:<-
	   #:prolog
	   #:query
	   #:traverse
	   ))
