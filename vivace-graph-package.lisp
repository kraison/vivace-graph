(in-package #:cl-user)

(defpackage #:vivace-graph
  (:use #:cl 
	#:cffi 
	#:bordeaux-threads 
	#:kyoto-persistence
	#:local-time
	#:cl-skip-list
	#:hunchentoot)
  (:export #:*graph*
	   #:graph?
	   #:load-graph!
	   #:make-new-graph
	   #:shutdown-graph
	   #:needs-indexing?
	   #:do-indexing
	   #:with-transaction
	   #:*in-transaction-p*
	   #:dbm-rollback
	   #:dbm-commit
	   #:dbm-begin
	   #:triple-db
	   #:functors

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
	   #:map-text-search
	   #:delete-triple
	   #:erase-triple

	   #:rule?
	   #:defrule
	   #:get-rule
	   #:retract-rule
	   #:deftemplate
	   #:fact
	   #:deffacts

	   #:<-
	   #:?-
	   #:!
	   #:prolog
	   #:insert
	   #:select
	   #:select-flat
	   #:select-bind-list
	   #:do-query
	   #:exec-rule
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
	   #:??
	   #:lisp/2
	   #:regex-match/2
	   #:var/1
	   #:is/2
	   #:call/1
	   #:not/1
	   #:bagof/3
	   #:setof/3
	   #:if/2
	   #:if/3
	   #:is-valid/1
	   #:is-valid?/1
	   #:is-invalid/1
	   #:is-invalid?/1
	   #:valid-date?/1
	   #:trigger/1
	   #:assert/1
	   #:retract/1
	   #:show-prolog-vars/2
	   #:select/2
	   #:select-as-bind-alist/2
	   #:triple-search/3
	   #:valid-date?
	   #:*trail*
	   #:*var-counter*
	   #:*predicate*
	   #:*select-list*
	   #:*prolog-global-functors*
	   #:*prolog-trace*
	   #:prolog-error

	   #:timestamp?
	   #:universal-to-timestamp
	   #:timestamp-to-universal
	   #:decode-timestamp

	   #:flatten
	   ))
