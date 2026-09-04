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
  (:use #:cl))
