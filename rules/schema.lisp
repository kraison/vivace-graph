;;;; rules/schema.lisp -- the rule record and the derivation family, per
;;;; store (spec §5, §9; GH #331).

(in-package #:graph-db.rules)

(defmacro def-rules-schema (graph-name)
  "Declare in the store GRAPH-NAME the RULE record (spec §5) and the
DERIVATION provenance family (spec §9).  Both are per store, as every
DEF-VERTEX is, so a store that holds rules evaluates this once beside
its own schema; GRAPH-NAME is the literal keyword DEF-CLAIM-CLASSES
takes.  One RULE per NAME: the identity facet's uniqueness is the
mechanism (ruling P1); a new version is COPY, SETF RULE-VERSION, SAVE.
NAME and VERSION are canonical strings ([a-z0-9-]+), FAMILY the parent
class's name as a string, HEAD and BODY guarded Prolog text."
  (check-type graph-name keyword)
  `(progn
     (graph-db.spacetime:def-source rule ,graph-name
         ((name :type string :accessor rule-name
                :check graph-db.spacetime:canonical-relation-p)
          (version :type string :accessor rule-version
                   :check graph-db.spacetime:canonical-relation-p)
          (family :type string :accessor rule-family)
          (head :type string :accessor rule-head)
          (body :type string :accessor rule-body)
          (extent-policy :initform :premises :accessor rule-extent-policy)
          (enabled :initform t :accessor rule-enabled))
       :identity (:namespace :rule :key-slot name)
       :space :none
       :time :none
       :attribution :none
       :sensitivity (:class :internal)
       :registration :none
       :indexed-text (:text-fn rule-body))
     (graph-db:def-value-constraint rule extent-policy ,graph-name
       :one-of '(:premises :none) :required t
       :name rule-extent-policy)
     (graph-db.spacetime:def-claim-classes derivation ,graph-name)
     ',graph-name))
