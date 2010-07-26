(in-package #:cl-user)

(defpackage #:vivace-graph
  (:use #:cl #:cffi #:bordeaux-threads #:tokyo-cabinet #:tokyo-cabinet-ffi #:local-time)
  (:export 
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
   #:add-rule
   #:delete-rule

   #:<-
   #:prolog
   #:query
   #:traverse
   ))
