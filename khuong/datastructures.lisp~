(defpackage "LOCK-FREE"
    (:use "CL" "SB-EXT"))

(in-package "LOCK-FREE")

;;; Lock-free circular double linked list
(defstruct (2list
             (:constructor %make-2list (left right value))
             (:conc-name #:2list.))
  left right value)

(defun make-2list (value)
  (let ((2list (%make-2list nil nil value)))
    (setf (2list.left  2list) 2list
          (2list.right 2list) 2list)
    2list))

(defun next (2list)
  (declare (type 2list 2list))
  (mcas:read (2list.right 2list)))

(defun prev (2list)
  (declare (type 2list 2list))
  (mcas:read (2list.left 2list)))

(defun value (2list)
  (declare (type 2list 2list))
  (2list.value 2list))

(defun splice (2list to-splice)
  "Splice /to-splice/ to the right of /2list/."
  (declare (type 2list 2list to-splice))
  (loop until
        (mcas:do (2list.right to-splice.left) ()
          (mcas:get  2list.right
                     (2list.right  2list))
          (mcas:cas (2list.right 2list)
                     2list.right to-splice)
          (mcas:get  to-splice.left
                     (2list.left to-splice))
          (mcas:cas (2list.left to-splice)
                     to-splice.left 2list)

          (mcas:cas (2list.left  2list.right)
                     2list     to-splice.left)
          (mcas:cas (2list.right to-splice.left)
                     to-splice 2list.right))
        finally (return 2list)))

(defun cut (from to)
  "Cut the sub-list between (and including) from and to."
  (declare (type 2list from to))
  (loop until
        (mcas:do (left right) ()
          (mcas:get left
                    (2list.left  from))
          (mcas:cas (2list.left from) left to)
          (mcas:get right
                    (2list.right to))
          (mcas:cas (2list.right to)  right from)

          (mcas:cas (2list.right left)  from right)
          (mcas:cas (2list.left  right) to   left))
        finally (return from)))
