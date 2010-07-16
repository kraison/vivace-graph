(in-package #:vivace-graph)

(defstruct (rule 
	     (:predicate rule?)
	     (:conc-name rule-))
  name graph s-expr)

(defstruct (alpha-memory
	     (:predicate alpha-memory?)
	     (:conc-name alpha-))
  triples children)

(defstruct (token
	     (:predicate token?)
	     (:conc-name token-))
  parent triple)

(defstruct (rete-node
	     (:predicate rete-node?)
	     (:conc-name rete-))
  type children parent tokens)
