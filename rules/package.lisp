;;;; rules/package.lisp -- rules as versioned producers (GH #304).
;;;;
;;;; Empty for S1: the claim functors are global, so facts.lisp homes
;;;; them in GRAPH-DB (see its header).  S2's rule record and DEF-RULE
;;;; live here (spec §5).
;;;;
;;;; Do NOT import spacetime's CLAIM-PRODUCER / CLAIM-RELATION /
;;;; CLAIM-STANDING / CLAIM-RULE-VERSION accessors here: the guard
;;;; resolves a functor by interning the goal head's name in that
;;;; symbol's own package, so an imported accessor would send
;;;; CLAIM-PRODUCER/2 to GRAPH-DB.SPACETIME and the goal would fail as
;;;; an unknown functor (docs/rules.md).

(defpackage #:graph-db.rules
  (:use #:cl)
  (:export
   ;; the store schema (spec §5, §9)
   #:def-rules-schema #:rule #:make-rule #:rule-p #:rule-name
   #:rule-version #:rule-family #:rule-head #:rule-body
   #:rule-extent-policy #:rule-enabled #:derivation
   ;; the in-image escape hatch (spec §5)
   #:def-rule #:undef-rule #:find-def-rule #:rule-spec #:rule-spec-p
   #:rule-spec-name #:rule-spec-version #:rule-spec-family
   #:rule-spec-head #:rule-spec-body #:rule-spec-extent-policy
   #:rule-spec-enabled
   ;; compiling (spec §6)
   #:compile-rule #:compiled-rule #:compiled-rule-p #:compiled-rule-spec
   #:compiled-rule-relation #:compiled-rule-reads
   #:rule-compile-error #:rule-compile-error-rule
   #:rule-compile-error-reason
   ;; running (spec §7)
   #:run-rule #:run-rules
   #:*rules-max-inferences* #:*rules-timeout* #:*rules-max-solutions*
   #:rule-report #:rule-report-p #:rule-report-rule-name
   #:rule-report-version #:rule-report-outcome #:rule-report-derived
   #:rule-report-kept #:rule-report-swept #:rule-report-disjoint-premises
   #:rule-report-refusals #:rule-report-inferences
   #:rule-report-elapsed
   ;; provenance (spec §9)
   #:premises-of #:dependents-of))
