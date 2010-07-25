(in-package #:vivace-graph)

(cffi:defctype size :unsigned-int)

;; MCAS status markers
(defconstant +mcas-undecided+ :undecided)
(defconstant +mcas-failed+ :failed)
(defconstant +mcas-succeeded+ :succeeded)
(defconstant +mcas-make-durable+ :make-durable)

;; MCAS transaction global
(defvar *mcas* nil)

;; Higher level transaction status markers
(defconstant +tx-undecided+ :undecided)
(defconstant +tx-failed+ :failed)
(defconstant +tx-succeeded+ :succeeded)

;; Higher level transaction global
(defvar *transaction* nil)

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

;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+ 100)
(defconstant +triple+ 101)
(defconstant +node+ 102)

;; Tags for sorting entry types in tokyo cabinet
(defconstant +triple-key+ 201)
(defconstant +node-key+ 202)
(defconstant +triple-subject+ 203)
(defconstant +triple-predicate+ 204)
(defconstant +triple-object+ 205)
(defconstant +node-ref-count+ 206)
(defconstant +deleted-triple-key+ 207)
(defconstant +text-index+ 208)
