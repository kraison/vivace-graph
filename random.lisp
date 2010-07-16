;;; -*- Mode: Lisp -*-
;;;
;;; $Header: /home/gene/library/website/docsrc/jmt/RCS/jmt.lisp,v 395.1 2008/04/20 17:25:47 gene Exp $
;;;
;;; Copyright (c) 2002, 2004 Jason Stover.  All rights reserved.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 2.1 of the
;;; License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;;; USA

;;; This is a implementation of the Mersenne Twister pseudo-random
;;; number generator by M. Matsumoto and T. Nishimura. See
;;; "Mersenne Twister: A 623-dimensionally equidistributed uniform
;;; pseudorandom number generator", ACM Transactions on Modeling and
;;; Computer Simulation vol. 8, no. 1, January 1998, pp 3-30.
;;; As of the time of this writing, the paper can also be found at
;;; www.math.keio.ac.jp/~matumoto/emt.html.

;;; LOG
;;;
;;; WARNING: I just finished this on 1/8/02 after learning
;;; just a bit of Lisp. I haven't tested it to make sure
;;; its generated values look uniformly distributed in high
;;; dimensions. Both a simple histogram and scatterplot of the
;;; values looked okay, but there could be problems. Feel free
;;; to fix them and tell me about your solutions. (JHS)
;;;
;;; 4 February 2004, Gene Michael Stover
;;; Rewrite & testing.
;;;
;;; 7 February 2004, Gene Michael Stover.
;;; Fixed two bugs found by Marc Battyani.  In one of the ASSERTs,
;;; I had APPLYied #'AND as if it's a function, but AND is a macro.
;;; Also, I create the MAG01 array by COERCEing a list to an array.
;;; I guess Lispworks doesn't like that, but Mr. Battyani said it
;;; worked if the array was a 'vector.  So I changed that.  It runs
;;; fine on clisp 2.31 with the two fixes.  I don't have any other
;;; Lisp installed, though, so I can't conveniently test it on others.
;;;
;;; 8 February 2004.  Gene Michael Stover.
;;; Renamed a couple of constants to begin with "mt-" to avoid
;;; name collisions.
;;; MT-RANDOM now works when its "limit" argument is an integral
;;; bignum.  In other words, "(mt-random (expt 2 68))" should
;;; return (- (expt 2 68) 1) with the same frequency it returns
;;; any other number.
(in-package #:vivace-graph)

;;; These two constants should begin with "*mt-" to help avoid
;;; collisions.  (2004-Feb-07, gms)
(defconstant *mt-k2^32* (expt 2 32))
(defconstant *mt-k-inverse-2^32f* (expt 2.0 -32.0)
  "1/(2^32), as a floating-point number")

(defconstant *mt-n* 624)
(defconstant *mt-m* 397)
(defconstant *mt-upper-mask* #x80000000 "most significant w-r bits")
(defconstant *mt-lower-mask* #x7FFFFFFF "least significant r bits")

(defstruct (mt-random-state
	    (:constructor mt-internal-make-random-state))
  ;; Could have avoided MTI, which is an index into ARR, with a
  ;; fill pointer in ARR.  MTI more closely follows the reference
  ;; implementation.
  ;; ARR corresponts to "mt[]" in the reference implementation.
  ;; Probably should have called it MT after all.  Oh well.
  mti					; index into ARR
  arr)					; array of numbers

(labels
 ((next-seed (n) (mod (1+ (* 69069 n)) *mt-k2^32*))
  (get-hi16 (n) (logand n #xFFFF0000))
  (next-elt (n)
	    (logior (get-hi16 n)
		    (ash (get-hi16 (next-seed n)) -16))))
 (defun mt-make-random-state-integer (n)
   "Use the single integer to expand into a bunch of
integers to use as an MT-RANDOM-STATE.
Copied from the 'sgenrand' function in mt19937int.c.
This is mostly an internal function.  I recommend using
MAKE-MT-RANDOM-STATE unless specific circumstances dictate otherwise."
   (mt-internal-make-random-state
    :mti *mt-n*
    :arr (make-array
	  *mt-n*
	  :element-type 'integer
	  :initial-contents (do ((i 0 (1+ i))
				 (sd n (next-seed (next-seed sd)))
				 (lst () (cons (next-elt sd) lst)))
				((>= i *mt-n*) (nreverse lst)))))))

(defvar *mt-random-state* nil
  "Unlike the reference implementation, we'll initialize the random
state to a hopefully somewhat random & unique value., but not until after defining 
(mt-make-random-state-random)")

(let ((some-number 0))
  (defun mt-make-random-state-random ()
    "Generate a new random state from a new, hopefully somewhat
random, value.
This is mostly an internal function.  I recommend using
MAKE-MT-RANDOM-STATE unless specific circumstances dictate otherwise."
    (mt-make-random-state-integer (+ (get-universal-time)
				     (incf some-number)))))

(defun make-mt-random-state (&optional state)
  "Analogous to Common Lisp's MAKE-RANDOM-STATE except that this function
works on random states for JMT's Mersenne Twister implementation."
  (cond ((eq state t) (mt-make-random-state-random))
	((null state)
	 ;; For NIL, return a copy of the current state.
	 (make-mt-random-state *mt-random-state*))
	((integerp state)
	 ;; Expand the integer STATE into controlled junk that is an
	 ;; MT RANDOM STATE.
	 (mt-make-random-state-integer state))
	((typep state 'sequence)
	 ;; It's a list or an array.  It must be of length *MT-N*, & it
	 ;; must contain integers.  We'll create a random state object
	 ;; using a copy of that sequence.
	 (assert state)			; should have caught NIL earlier
	 (assert (eql (length state) *mt-n*))
	 (assert (not (find-if #'integerp state)))
	 (mt-internal-make-random-state
	  :mti 0
	  :arr (copy-seq (coerce state 'array))))
	((mt-random-state-p state)
	 ;; Return a copy of state.  It is an instance of MT-RANDOM-STATE.
	 (mt-internal-make-random-state
	  :mti (mt-random-state-mti state)
	  :arr (copy-seq (mt-random-state-arr state))))
	(t
	 ;; For anything else, error.
	 (cerror "STATE should not have a value of ~A"))))

(setq *mt-random-state* (make-mt-random-state t))

(let* ((matrix-a #x9908B0DF)
       (mag01 (coerce (list 0 matrix-a) 'vector)))
  (defun mt-refill ()
    "In the C program mt19937int.c, there is a function called 'genrand', & in
that function there is a block of code to execute when the mt[] array is
exhausted.  This function is that block of code.  I've removed it from my
MT-GENRAND function for clarity."
    ;; This function is pretty much a direct translation of the C function.
    ;; In other words, you're about to see some very un-Lispy code.
    (let (y kk)
      (setq kk 0)
      (do ()
	  ((>= kk (- *mt-n* *mt-m*)))
	  (setq y (logior
		   (logand (aref (mt-random-state-arr *mt-random-state*)
				 kk)
			   *mt-upper-mask*)
		   (logand (aref (mt-random-state-arr *mt-random-state*)
				 (1+ kk))
			   *mt-lower-mask*)))
	  (setf (aref (mt-random-state-arr *mt-random-state*) kk)
		(logxor
		 (aref (mt-random-state-arr *mt-random-state*) (+ kk *mt-m*))
		 (ash y -1)
		 (aref mag01 (logand y 1))))
	  (incf kk))
      (do ()
	  ((>= kk (- *mt-n* 1)))
	  (setq y (logior
		   (logand 
		    (aref (mt-random-state-arr *mt-random-state*) kk)
		    *mt-upper-mask*)
		   (logand
		    (aref (mt-random-state-arr *mt-random-state*) (1+ kk))
		    *mt-lower-mask*)))
	  (setf (aref (mt-random-state-arr *mt-random-state*) kk)
		(logxor (aref (mt-random-state-arr *mt-random-state*)
			      (+ kk (- *mt-m* *mt-n*)))
			(ash y -1)
			(aref mag01 (logand y 1))))
	  (incf kk))
      (setq y (logior
	       (logand
		(aref (mt-random-state-arr *mt-random-state*) (- *mt-n* 1))
		*mt-upper-mask*)
	       (logand
		(aref (mt-random-state-arr *mt-random-state*) 0)
		*mt-lower-mask*)))
      (setf (aref (mt-random-state-arr *mt-random-state*) (- *mt-n* 1))
	    (logxor
	     (aref (mt-random-state-arr *mt-random-state*) (- *mt-m* 1))
	     (ash y -1)
	     (aref mag01 (logand y 1))))
      (setf (mt-random-state-mti *mt-random-state*) 0))
    'mt-refill))

(defun mt-tempering-shift-u (n)
  (mod (ash n -11) *mt-k2^32*))

(defun mt-tempering-shift-s (n)
  (mod (ash n 7) *mt-k2^32*))

(defun mt-tempering-shift-t (n)
  (mod (ash n 15) *mt-k2^32*))

(defun mt-tempering-shift-l (n)
  (mod (ash n -18) *mt-k2^32*))

(let ((mt-tempering-mask-b #x9d2c5680)
      (mt-tempering-mask-c #xefc60000))
  (defun mt-genrand ()
    (when (>= (mt-random-state-mti *mt-random-state*) *mt-n*)
      (mt-refill))
    (let ((y (aref (mt-random-state-arr *mt-random-state*)
		   (mt-random-state-mti *mt-random-state*))))
      (incf (mt-random-state-mti *mt-random-state*))
      ;; The following separate, explicit SETQ & other expressions
      ;; could be compacted/optimized into a single arithmetic expression
      ;; that does not store into any temporary variables.  That could be
      ;; more efficient at run-time, but I have chosen instead of immitate
      ;; the statements in the C program, mt19937int.c.
      (setq y (logxor y (mt-tempering-shift-u y)))
      (setq y (logxor y (logand (mt-tempering-shift-s y)
				mt-tempering-mask-b)))
      (setq y (logxor y (logand (mt-tempering-shift-t y)
				mt-tempering-mask-c)))
      (setq y (logxor y (mt-tempering-shift-l y)))
      y)))

(defun mt-random (n &optional state)
  (assert (plusp n))
  (when state
    (assert (mt-random-state-p state))
    ;; Save a copy of the random state.
    (setq *mt-random-state* (make-mt-random-state state)))
  (if (integerp n)
      (mod (do ((bits-needed  (log n 2)                             )
		(bit-count            0             (+ 32 bit-count))
		(r                    0  (+ (ash r 32) (mt-genrand))))
	       ((>= bit-count bits-needed) r))
	   n)
    (* (mt-genrand) *mt-k-inverse-2^32f* n)))

;;; --- end of file ---
