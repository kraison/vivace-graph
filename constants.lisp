(in-package #:vivace-graph)

(cffi:defctype size :unsigned-int)

;; MCAS status markers
(defconstant +mcas-undecided+ :undecided)
(defconstant +mcas-failed+ :failed)
(defconstant +mcas-succeeded+ :succeeded)

;; MCAS transaction global
(defvar *mcas* nil)

;; Built-in type identifiers for serializing
(defconstant +unknown+ 0)
(defconstant +negative-integer+ 1)
(defconstant +positive-integer+ 2)
(defconstant +character+ 3)
(defconstant +symbol+ 4)
(defconstant +string+ 5)
(defconstant +list+ 6)
(defconstant +vector+ 7)
(defconstant +single-float+ 8)
(defconstant +double-float+ 9)
(defconstant +ratio+ 10)
(defconstant +t+ 11)
(defconstant +null+ 12)
(defconstant +blob+ 13) ;; Uninterpreted octets
(defconstant +dotted-list+ 14)

;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+ 100)
(defconstant +triple+ 101)
(defconstant +node+ 102)
(defconstant +predicate+ 103)
(defconstant +timestamp+ 104)

;; Tags for sorting entry types in tokyo cabinet
(defconstant +triple-key+ 201)
(defconstant +node-key+ 202)
(defconstant +predicate-key+ 209)
(defconstant +triple-subject+ 203)
(defconstant +triple-predicate+ 204)
(defconstant +triple-object+ 205)
(defconstant +triple-subject-predicate+ 206)
(defconstant +triple-subject-object+ 207)
(defconstant +triple-predicate-object+ 208)
(defconstant +node-ref-count+ 209)
(defconstant +deleted-triple-key+ 210)
(defconstant +text-index+ 211)
