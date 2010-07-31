(in-package #:vivace-graph)

(defstruct (rule 
	     (:predicate rule?)
	     (:conc-name rule-))
  name s-expr graph)

(defstruct (alpha-memory
	     (:predicate alpha-memory?)
	     (:conc-name alpha-))
  triples children graph)

(defstruct (token
	     (:predicate token?)
	     (:conc-name token-))
  parent triple graph)

(defstruct (rete-node
	     (:predicate rete-node?)
	     (:conc-name rete-))
  type children parent tokens graph)

