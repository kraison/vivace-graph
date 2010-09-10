(in-package #:vivace-graph)

(cffi:defctype size :unsigned-int)

;; Prolog constants and specials
(defconstant +unbound+ :unbound)
(ignore-errors (defconstant +no-bindings+ '((t . t))))
(defconstant +fail+ nil)

;; Certainty factors
(defconstant +cf-true+ 1.0)
(defconstant +cf-false+ -1.0)
(defconstant +cf-unknown+ 0.0)

(defconstant +needs-lookup+ :needs-lookup)

;; User-defined type identifiers for serializing. Start at 100
(defconstant +triple+ 101)
;;(defconstant +node+ 102)
(defconstant +predicate+ 103)
(defconstant +rule+ 105)

;; Tags for sorting entry types in tokyo cabinet
(defconstant +triple-key+ 201)
;;(defconstant +node-key+ 202)
(defconstant +predicate-key+ 209)
(defconstant +triple-subject+ 203)
(defconstant +triple-predicate+ 204)
(defconstant +triple-object+ 205)
(defconstant +triple-subject-predicate+ 206)
(defconstant +triple-subject-object+ 207)
(defconstant +triple-predicate-object+ 208)
;;(defconstant +node-ref-count+ 209)
(defconstant +deleted-triple-key+ 210)
(defconstant +text-index+ 211)
(defconstant +rule-key+ 212)
