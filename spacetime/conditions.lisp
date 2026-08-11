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
