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
(defconstant +positive-integer+ 1)
(defconstant +negative-integer+ 2)
(defconstant +character+ 3)
(defconstant +symbol+ 4)
(defconstant +string+ 5) ;; Strings up to 255 characters
(defconstant +list+ 6)
(defconstant +vector+ 6)
(defconstant +single-float+ 7)
(defconstant +double-float+ 8)
(defconstant +ratio+ 9)
(defconstant +t+ 10)
(defconstant +null+ 11)
(defconstant +clob+ 12) ;; Strings over 255 characters
(defconstant +blob+ 13)

;; User-defined type identifiers for serializing. Start at 100
(defconstant +uuid+ 100)
(defconstant +triple+ 101)
