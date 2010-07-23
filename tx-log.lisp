(in-package #:vivace-graph)

(defgeneric persist-object (object)
  (:documentation "Defined for each type to make durable. Returns bytes to write to disk."))
(defgeneric load-object (binary)
  (:documentation "Read a binary array from disk and transform it into the appropriate object."))

(defvar *tx-durability-mailbox* (sb-concurrency:make-mailbox :name "tx-durability-mailbox"))

(defun make-durable (timestamp items)
  (if items
      (sb-concurrency:send-message *tx-durability-mailbox* (list timestamp items))
      t))

(defun transaction-logger (mailbox)
  (loop
     (destructuring-bind (timestamp items) (sb-concurrency:receive-message mailbox)
       )))
       