(in-package #:vivace-graph)

(defclass rule ()
  ((name :accessor name :initarg :name :initform nil)
   (text :accessor text :initarg :text :initform nil)
   (graph :accessor graph :initarg :graph :initform :default)))

(defgeneric rule? (thing)
  (:method ((thing rule)) t)
  (:method (thing) nil))

