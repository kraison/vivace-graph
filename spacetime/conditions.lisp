;;;; Conditions for graph-db/spacetime (GH #130).

(in-package #:graph-db.spacetime)

(define-condition spacetime-error (error) ()
  (:documentation "Root of every error this subsystem signals."))

(define-condition invalid-standing (spacetime-error)
  ((value :initarg :value :reader invalid-standing-value))
  (:report (lambda (c s)
             (format s "~S is not a standing." (invalid-standing-value c)))))

(define-condition invalid-bound (spacetime-error)
  ((earliest :initarg :earliest :reader invalid-bound-earliest)
   (latest :initarg :latest :reader invalid-bound-latest)
   (reason :initarg :reason :reader invalid-bound-reason))
  (:report (lambda (c s)
             (format s "Bad bound [~S, ~S]: ~A."
                     (invalid-bound-earliest c) (invalid-bound-latest c)
                     (invalid-bound-reason c)))))

(define-condition invalid-extent (spacetime-error)
  ((reason :initarg :reason :reader invalid-extent-reason))
  (:report (lambda (c s)
             (format s "Bad extent: ~A." (invalid-extent-reason c)))))

(define-condition missing-claim-identity-component (spacetime-error)
  ((slot :initarg :slot :reader missing-claim-identity-component-slot))
  (:report (lambda (c s)
             (format s "~S is a claim identity component and may not ~
                        be NIL (design §3.1)."
                     (missing-claim-identity-component-slot c)))))

(define-condition missing-source-facet (spacetime-error)
  ((name :initarg :name :reader missing-source-facet-name)
   (facets :initarg :facets :reader missing-source-facet-facets))
  (:report (lambda (c s)
             (format s "DEF-SOURCE ~S is missing required facets: ~{~S~^, ~}.~
  Every facet must be given; use :NONE to say one does not apply."
                     (missing-source-facet-name c)
                     (missing-source-facet-facets c)))))

(define-condition invalid-source-facet (spacetime-error)
  ((facet :initarg :facet :reader invalid-source-facet-facet)
   (value :initarg :value :reader invalid-source-facet-value)
   (reason :initarg :reason :reader invalid-source-facet-reason))
  (:report (lambda (c s)
             (format s "Bad ~S facet ~S: ~A."
                     (invalid-source-facet-facet c)
                     (invalid-source-facet-value c)
                     (invalid-source-facet-reason c)))))

(define-condition not-a-source (spacetime-error)
  ((class :initarg :class :reader not-a-source-class))
  (:report (lambda (c s)
             (format s "~S was not defined with DEF-SOURCE."
                     (not-a-source-class c)))))

(define-condition unknown-namespace (spacetime-error)
  ((namespace :initarg :namespace :reader unknown-namespace-namespace))
  (:report (lambda (c s)
             (format s "No source class is registered under namespace ~S."
                     (unknown-namespace-namespace c)))))

(define-condition resolution-in-transaction (spacetime-error)
  ((namespace :initarg :namespace :reader resolution-in-transaction-namespace)
   (key :initarg :key :reader resolution-in-transaction-key))
  (:report (lambda (c s)
             (format s "RESOLVE-ENDPOINT ~S/~S was called inside a ~
read-write transaction.  Resolution can cross graphs, and a read-write ~
transaction is single-graph; resolve before opening it."
                     (resolution-in-transaction-namespace c)
                     (resolution-in-transaction-key c)))))

(define-condition ambiguous-endpoint (spacetime-error)
  ((namespace :initarg :namespace :reader ambiguous-endpoint-namespace)
   (key :initarg :key :reader ambiguous-endpoint-key)
   (classes :initarg :classes :reader ambiguous-endpoint-classes))
  (:report (lambda (c s)
             (format s "~S/~S resolves in more than one class: ~{~S~^, ~}.~
  An external key must be unique within its namespace."
                     (ambiguous-endpoint-namespace c)
                     (ambiguous-endpoint-key c)
                     (ambiguous-endpoint-classes c)))))

(define-condition unopened-source-graph (spacetime-error)
  ((class :initarg :class :reader unopened-source-graph-class)
   (graph-name :initarg :graph-name :reader unopened-source-graph-graph-name))
  (:report (lambda (c s)
             (format s "~S's graph ~S is not open; RESOLVE-ENDPOINT cannot ~
  consult it (Finding 4, GH #132 review)."
                     (unopened-source-graph-class c)
                     (unopened-source-graph-graph-name c)))))
