;;;; rules/package.lisp -- rules as versioned producers (GH #304).
;;;;
;;;; Empty for S1: the claim functors are global, so facts.lisp homes
;;;; them in GRAPH-DB (see its header).  S2's rule record and DEF-RULE
;;;; live here (spec §5).

(defpackage #:graph-db.rules
  (:use #:cl))
