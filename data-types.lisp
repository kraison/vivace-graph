(in-package #:vivace-graph)

;;; UUIDs
(defun make-uuid ()
  "Create a new UUID."
  (uuid:make-v1-uuid))

(defun sxhash-uuid (uuid) (sxhash (uuid:print-bytes nil uuid)))

(sb-ext:define-hash-table-test uuid:uuid-eql sxhash-uuid)

(defun make-uuid-table (&key synchronized) 
  (make-hash-table :test 'uuid:uuid-eql :synchronized synchronized))


;;; Dates
;;; timestamps provided by local-time lib
(defgeneric timestamp? (thing)
  (:method ((thing timestamp)) t)
  (:method (thing) nil))
