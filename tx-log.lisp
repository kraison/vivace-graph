(in-package #:vivace-graph)

(defparameter *tx-log-queue* (sb-concurrency:make-queue :name "tx-log-queue"))

(defstruct (tx-log-element
	     (:predicate tx-log-element?)
	     (:conc-name nil))
  (tx-id nil)
  (tx nil))
