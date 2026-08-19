;;;; REGISTER-GEOMETRY: binding a geometry to a registry's regions.
;;;; Design: docs/superpowers/specs/2026-08-19-registration-design.md
;;;; §4-§6 (GH #138).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *region-graph-name* :graph-db-register-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *region-graph-name* graph-db::*schema-node-metadata*) nil))

;; A geometry slot marked :INDEX T is what makes a type spatially indexed,
;; and so scopeable by class name (spatial-query.lisp's SCOPE argument).
(def-vertex ct-region ()
  ((name :type string)
   (geom :type geometry :index t))
  :graph-db-register-test)

(defmacro with-region-graph ((g) &body body)
  "A fresh on-disk graph named *REGION-GRAPH-NAME* in a temp dir.
Modelled on WITH-CLAIM-GRAPH (claim-tests.lisp).  :SPATIAL-PRECISION 5
because these regions are whole degrees across: at the default 7 their
cover hits the cell cap, which coarsens the index and warns (the cover
cap in spatial-index.lisp)."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *region-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000
                             :spatial-precision 5)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun %make-region (graph name ring)
  "A CT-REGION called NAME covering RING, a closed list of (lon lat)."
  (let ((graph-db:*graph* graph))
    (with-transaction ()
      (make-ct-region :name name :geom (make-polygon (list ring))))))

(defun %fraction-of (registration)
  (getf registration :fraction))

(defun %near (expected actual)
  "Fractions are compared to 1d-6, never by equality (plan constraint)."
  (< (abs (- expected actual)) 1d-6))

(test a-point-registers-to-one-region-at-fraction-one
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    (multiple-value-bind (regs evaluated)
        (register-geometry (make-point 1d0 1d0) 'ct-region :graph g)
      (is-true evaluated)
      (is (= 1 (length regs)))
      (is (%near 1d0 (%fraction-of (first regs)))))))

(test a-polygon-registers-fractionally-to-every-region-it-overlaps
  "⚠ Registration is PARTIAL AND FRACTIONAL, not boolean (design §1)."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available")
      (with-region-graph (g)
        (%make-region g "west" '((0d0 0d0) (1d0 0d0) (1d0 2d0) (0d0 2d0)
                                 (0d0 0d0)))
        (%make-region g "east" '((1d0 0d0) (2d0 0d0) (2d0 2d0) (1d0 2d0)
                                 (1d0 0d0)))
        (multiple-value-bind (regs evaluated)
            (register-geometry
             (make-polygon '(((0.5d0 0.5d0) (1.5d0 0.5d0) (1.5d0 1.5d0)
                              (0.5d0 1.5d0) (0.5d0 0.5d0))))
             'ct-region :graph g)
          (is-true evaluated)
          (is (= 2 (length regs)))
          (is (%near 1d0 (reduce #'+ regs :key #'%fraction-of
                                          :initial-value 0d0))
              "the halves partition the subject, so the fractions sum to 1")
          (dolist (r regs)
            (is (< 0.4d0 (%fraction-of r) 0.6d0)))))))

(test a-subject-outside-every-region-registers-to-nothing
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (1d0 0d0) (1d0 1d0) (0d0 1d0)
                          (0d0 0d0)))
    (multiple-value-bind (regs evaluated)
        (register-geometry (make-point 50d0 50d0) 'ct-region :graph g)
      (is-true evaluated "an empty result is an ANSWER, not a failed scan")
      (is (null regs)))))

(test without-geos-a-polygon-refuses-and-a-point-still-registers
  "⚠ A bounding box is OVER-inclusive, so approximating here would bind
records to regions they never touch.  The point is the control: without
it this cannot tell 'refused correctly' from 'broken everywhere'
(design §6)."
  (with-region-graph (g)
    (%make-region g "a" '((0d0 0d0) (2d0 0d0) (2d0 2d0) (0d0 2d0)
                          (0d0 0d0)))
    ;; *GEOS-AVAILABLE-P* is a DEFVAR (globals.lisp), so a LET binding is
    ;; a special binding here as it is in tests/geos/suite.lisp.
    (let ((graph-db::*geos-available-p* nil))
      (multiple-value-bind (regs evaluated)
          (register-geometry
           (make-polygon '(((0.5d0 0.5d0) (1.5d0 0.5d0) (1.5d0 1.5d0)
                            (0.5d0 1.5d0) (0.5d0 0.5d0))))
           'ct-region :graph g)
        (is-false evaluated)
        (is (null regs)))
      (multiple-value-bind (regs evaluated)
          (register-geometry (make-point 1d0 1d0) 'ct-region :graph g)
        (is-true evaluated
                 "a point's candidates are exact with or without GEOS")
        (is (= 1 (length regs)))))))

(test a-line-registers-by-length-not-by-area
  "⚠ A line's AREA is zero, so an area ratio gives it 1.0 in EVERY
region it crosses -- three regions summing to 3.0.  Its fraction is a
LENGTH ratio (design §13).  The regions are stacked north/south and the
line runs along a meridian, where great-circle segments are exactly
additive, so the halves really do sum to the whole."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available")
      (with-region-graph (g)
        (%make-region g "south" '((0d0 0d0) (2d0 0d0) (2d0 1d0) (0d0 1d0)
                                  (0d0 0d0)))
        (%make-region g "north" '((0d0 1d0) (2d0 1d0) (2d0 2d0) (0d0 2d0)
                                  (0d0 1d0)))
        (multiple-value-bind (regs evaluated)
            (register-geometry
             (make-linestring '((1d0 0.5d0) (1d0 1.5d0)))
             'ct-region :graph g)
          (is-true evaluated)
          (is (= 2 (length regs)))
          (let ((fs (mapcar #'%fraction-of regs)))
            (is (%near 1d0 (reduce #'+ fs :initial-value 0d0))
                "the halves partition the line, so the fractions sum to ~
1, but they are ~S" fs)
            (dolist (f fs)
              (is (< 0.4d0 f 0.6d0)
                  "each half is about half the line, not ~A" f)))))))

(test a-region-the-subject-only-touches-is-not-registered
  "GEOS `intersects' is true for boundary contact, so an abutting region
comes back as a candidate with a zero-measure intersection.  Registering
it would bind a record to a region it does not overlap -- the mild form
of the false positive design §6 exists to prevent (design §13).  The
second assertion is the control: without it this passes when
registration is broken entirely."
  (if (not graph-db::*geos-available-p*)
      (skip "GEOS not available")
      (with-region-graph (g)
        (%make-region g "overlapped" '((0d0 0d0) (1d0 0d0) (1d0 2d0)
                                       (0d0 2d0) (0d0 0d0)))
        (%make-region g "touched" '((1d0 0d0) (2d0 0d0) (2d0 2d0)
                                    (1d0 2d0) (1d0 0d0)))
        (multiple-value-bind (regs evaluated)
            (register-geometry
             (make-polygon '(((0.25d0 0.5d0) (1d0 0.5d0) (1d0 1.5d0)
                              (0.25d0 1.5d0) (0.25d0 0.5d0))))
             'ct-region :graph g)
          (is-true evaluated)
          (is (= 1 (length regs))
              "only the overlapped region registers, not ~S"
              (mapcar (lambda (r) (name (getf r :region))) regs))
          (is (string= "overlapped" (name (getf (first regs) :region)))
              "it still registers to the region it genuinely overlaps")
          (is (%near 1d0 (%fraction-of (first regs))))))))
