;;;; Package for graph-db/spacetime -- the temporal substrate (GH #130).
;;;;
;;;; Its own package rather than GRAPH-DB: a public API exported from
;;;; GRAPH-DB would mean editing core's package.lisp, and core gains nothing
;;;; from this programme (design §1.2).

(in-package #:cl-user)

(defpackage #:graph-db.spacetime
  ;; The temporal layer is cl-temporal-extent (#159).  It is :USEd and its
  ;; symbols re-exported below, so a consumer of GRAPH-DB.SPACETIME sees the
  ;; same API it always did.
  (:use #:cl #:temporal-extent)
  (:export
   ;; conditions -- the first four are cl-temporal-extent's, re-exported
   #:spacetime-error #:invalid-standing #:invalid-bound #:invalid-extent
   #:missing-claim-identity-component
   #:missing-claim-identity-component-slot
   ;; standing
   #:standing #:standingp #:standing-absence-p #:standing-present-p
   #:check-standing #:+standings+ #:+absence-standings+
   ;; bound
   #:bound #:bound-p #:make-bound #:exact-bound #:unknown-bound
   #:bound-earliest #:bound-latest #:bound-exact-p #:bound-unknown-p
   #:bound-compare
   ;; extent
   #:temporal-extent #:temporal-extent-p #:+precisions+
   #:make-interval #:make-instant
   #:make-granule-interval #:make-granule-instant #:granule-bounds
   #:extent-kind #:extent-start #:extent-end #:extent-precision
   #:extent-semantics #:extent-standing #:extent-instant-p
   #:extent->sexp #:sexp->extent
   ;; allen
   #:temporal-relation #:temporal-relation-p
   #:temporal-relation-relations #:temporal-relation-standings
   #:temporal-relation-semantics
   #:+allen-relations+ #:+allen-inverses+ #:allen-inverse
   #:allen-relations #:allen-relation #:allen-definite-p
   #:extent-before-p #:extent-meets-p #:extent-overlaps-p
   #:extent-finished-by-p #:extent-contains-p #:extent-starts-p
   #:extent-equals-p #:extent-started-by-p #:extent-during-p
   #:extent-finishes-p #:extent-overlapped-by-p #:extent-met-by-p
   #:extent-after-p
   ;; claim (GH #131)
   #:def-claim-classes #:claim-family #:claim-family-parent
   #:claim-family-unary #:claim-family-binary
   #:claim-subject-namespace #:claim-subject-key
   #:claim-object-namespace #:claim-object-key
   #:claim-relation #:claim-producer #:claim-rule-version
   #:claim-method #:claim-standing #:claim-confidence
   #:claim-extent-sexp #:claim-geometry
   #:claim-precision-m #:claim-fraction                 ; GH #138
   #:claim-extent #:claims-touching
   #:claim-transaction-extent-sexp                     ; GH #148
   #:claim-transaction-extent #:claim-recorded-at
   #:transaction-extent-immutable
   #:claims-by-producer #:delete-claims-by-producer   ; GH #145
   #:unknown-claim-family
   ;; source onboarding contract (GH #132)
   #:def-source #:source-contract #:+source-facets+
   #:source-facets #:source-facets-p #:source-facets-class
   #:source-facets-graph #:source-facets-identity #:source-facets-space
   #:source-facets-time #:source-facets-attribution
   #:source-facets-sensitivity #:source-facets-registration
   #:source-facets-indexed-text
   #:missing-source-facet #:invalid-source-facet #:not-a-source
   #:resolve-endpoint #:source-disclosable-p #:+disclosure-classes+
   #:unknown-namespace #:ambiguous-endpoint #:ambiguous-endpoint-classes
   #:resolution-in-transaction #:namespace-sources
   #:unopened-source-graph #:unopened-source-graph-class
   #:unopened-source-graph-graph-name
   ;; registration (GH #138)
   #:register-geometry))
