(in-package #:cl-user)

(defpackage #:vivace-graph
  (:use #:cl #:cffi #:bordeaux-threads 
	#:tokyo-cabinet #:tokyo-cabinet-ffi 
	;;#:kyoto-cabinet #:kyoto-cabinet-ffi
	#:local-time)
  (:export #:*graph*
	   #:graph?
	   #:load-graph!
	   #:make-new-graph
	   #:shutdown-graph
	   #:needs-indexing?
	   #:do-indexing

	   #:node?
	   #:node-value
	   #:node-uuid
	   #:node-eql
	   #:node-equalp
	   #:lookup-node
	   #:make-anonymous-node
	   #:make-new-node

	   #:triple?
	   #:triple-subject
	   #:triple-predicate
	   #:triple-object
	   #:triple-eql
	   #:triple-equalp
	   #:add-triple
	   #:get-triples
	   #:bulk-add-triples

	   #:rule?
	   #:defrule
	   #:get-rule
	   #:retract-rule

	   #:<-
	   #:?-
	   #:prolog
	   #:select
	   #:select-flat
	   #:read/1
	   #:wrte/1
	   #:nl/0
	   #:repeat/0
	   #:fail/0
	   #:=/2
	   #:==/2
	   #:/=/2
	   #:>/2
	   #:</2
	   #:>=/2
	   #:<=/2
	   #:lisp/2
	   #:regex-match/2
	   #:var/1
	   #:is/2
	   #:call/1
	   #:not/1
	   #:bagof/3
	   #:setof/3

	   #:timestamp?
	   #:universal-to-timestamp
	   #:timestamp-to-universal
	   #:decode-timestamp
	   ))
