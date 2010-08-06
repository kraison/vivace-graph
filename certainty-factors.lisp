;;; Copyright (C) 2000 David E. Young

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; File: certainty-factors.lisp

;;; Description: An implementation of Certainty Factors as found in Peter Norvig's PAIP.

;;; Modified for VivaceGraph by Keivn Raison, 2010

(in-package #:vivace-graph)

(defgeneric belief-factor (tuple))

(defmethod belief-factor ((tuple list))
  (fourth tuple))

(defun certainty-factor-p (number)
  (<= +cf-false+ number +cf-true+))

(deftype certainty-factor ()
  `(and (real)
	(satisfies certainty-factor-p)))

(defun true-p (cf)
  (check-type cf certainty-factor)
  (> cf +cf-unknown+))

(defun false-p (cf)
  (check-type cf certainty-factor)
  (< cf +cf-unknown+))

(defun unknown-p (cf)
  (check-type cf certainty-factor)
  (= cf +cf-unknown+))

(defun cf-or (a b)
  "Combine the certainty factors for the formula (A or B).
  This is used when two rules support the same conclusion."
  (check-type a certainty-factor)
  (check-type b certainty-factor)
  (cond ((and (> a 0) (> b 0))
         (+ a b (* -1 a b)))
        ((and (< a 0) (< b 0))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defun cf-and (a b)
  "Combine the certainty factors for the formula (A and B)."
  (check-type a certainty-factor)
  (check-type b certainty-factor)
  (min a b))

(defun cf-combine (a b)
  (check-type a certainty-factor)
  (check-type b certainty-factor)
  (cond ((and (plusp a)
              (plusp b))
         (+ a b (* -1 a b)))
        ((and (minusp a)
              (minusp b))
         (+ a b (* a b)))
        (t (/ (+ a b)
              (- 1 (min (abs a) (abs b)))))))

(defun conjunct-cf (objects)
  "Combines the certainty factors of objects matched within a single rule."
  (let ((conjuncts
         (loop for obj in objects
	    for cf = (belief-factor obj)
	    if cf collect cf)))
    (if conjuncts
        (apply #'min conjuncts)
	nil)))

(defgeneric recalculate-cf (objects rule-cf old-cf)
  (:method (objects (rule-cf number) (old-cf number))
    (let* ((combined-cf (conjunct-cf objects))
	   (new-cf (if combined-cf (* rule-cf combined-cf) rule-cf)))
      (cf-combine old-cf new-cf)))
  (:method (objects (rule-cf number) (old-cf t))
    (let* ((combined-cf (conjunct-cf objects))
	   (new-cf (if combined-cf combined-cf rule-cf))
	   (factor (if combined-cf rule-cf +cf-true+)))
      (* new-cf factor)))
  (:method (objects (rule-cf t) (old-cf t))
    (let* ((combined-cf (conjunct-cf objects)))
      (if combined-cf
	  (* combined-cf 1.0)
	  nil))))

(defun cf->english (cf)
  (cond ((= cf 1.0) "certain evidence")
        ((> cf 0.8) "strongly suggestive evidence")
        ((> cf 0.5) "suggestive evidence")
        ((> cf 0.0) "weakly suggestive evidence")
        ((= cf 0.0) "no evidence either way")
        ((< cf 0.0) (concatenate 'string (cf->english (- cf)) " against the conclusion"))))

;;; interface into the generic belief system.

(defmethod adjust-belief (objects (rule-belief number) &optional (old-belief nil))
  (recalculate-cf objects rule-belief old-belief))

(defmethod adjust-belief (objects (rule-belief t) &optional old-belief)
  (declare (ignore objects old-belief))
  nil)

(defmethod belief->english ((cf number))
  (cf->english cf))

