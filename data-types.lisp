(in-package #:vivace-graph)

;;; timestamps provided by local-time lib
(defmethod serialize ((timestamp timestamp))
  (let ((universal-time (serialize (timestamp-to-universal timestamp))))
    (setf (aref universal-time 0) +timestamp+)
    universal-time))

(defmethod deserialize-help ((become (eql +timestamp+)) bytes)
  (let ((universal-time (deserialize-help +positive-integer+ bytes)))
    (universal-to-timestamp universal-time)))

