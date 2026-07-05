;;;; Branch B per-field conflict resolver (MERGE-AUTHORED-FIELDS), the pure core.
;;;;
;;;; No graph/network: feed it local data, an incoming op's new/old data, per-field
;;;; (lamport . origin) stamps, and a mock policy, and assert the merged data, the
;;;; stamp updates, and the surfaced conflicts.  The mock SAFETY-MERGE mirrors the
;;;; app's binary contract (dangerous unless "SAFE"): release surfaces + keeps the
;;;; dangerous local value; re-open toward danger auto-applies; same-class -> LWW.

(in-package #:graph-db/test)

(def-suite peer-merge-suite
  :description "Branch B per-field conflict resolver."
  :in graph-db-suite)

(in-suite peer-merge-suite)

(defun id16 (byte) (make-array 16 :element-type '(unsigned-byte 8) :initial-element byte))
(defparameter *o-low*  (id16 1))
(defparameter *o-high* (id16 9))

(defun test-field-bucket (type slot)
  (declare (ignore type))
  (case slot (confidence :safety) (geom :geometry) (t :lww)))

(defun test-safety-merge (slot local incoming local-newer-p)
  (declare (ignore slot))
  (let ((ld (not (equal local "SAFE")))          ; dangerous unless exactly "SAFE"
        (id (not (equal incoming "SAFE"))))
    (cond ((and ld (not id)) (values local t))               ; release -> keep local, surface
          ((and id (not ld)) (values incoming nil))          ; re-open -> take incoming
          (t (values (if local-newer-p local incoming) nil))))) ; same class -> LWW

(defparameter *test-policy*
  (graph-db::make-merge-policy :field-bucket #'test-field-bucket
                               :safety-merge #'test-safety-merge))

(defun merge3 (node-type local new old lamport origin stamps)
  (graph-db::merge-authored-fields *test-policy* node-type (id16 0)
                                   local new old lamport origin stamps))

;;; --- :lww -----------------------------------------------------------------

(test lww-incoming-newer-wins
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'thing '((x . "a")) '((x . "b")) '((x . "a")) 8 *o-high*
              (list (cons 'x (cons 5 *o-low*))))
    (is (equal "b" (cdr (assoc 'x merged))) "incoming (lamport 8) beats local (5)")
    (is (= 8 (car (cdr (assoc 'x stamps)))) "field stamp advanced to 8")
    (is (null conflicts) "LWW never surfaces")))

(test lww-local-newer-keeps-local
  ;; local field last written at lamport 10; a stale incoming edit at 8 is discarded.
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'thing '((x . "a")) '((x . "b")) '((x . "a")) 8 *o-high*
              (list (cons 'x (cons 10 *o-low*))))
    (is (equal "a" (cdr (assoc 'x merged))) "local (lamport 10) beats incoming (8)")
    (is (null stamps) "no stamp change when local wins")
    (is (null conflicts))))

(test lww-lamport-tie-broken-by-origin
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'thing '((x . "a")) '((x . "b")) '((x . "a")) 5 *o-high*
              (list (cons 'x (cons 5 *o-low*))))
    (declare (ignore stamps))
    (is (equal "b" (cdr (assoc 'x merged)))
        "on a lamport tie the higher origin wins (deterministic convergence)")
    (is (null conflicts))))

;;; --- :safety --------------------------------------------------------------

(test safety-release-keeps-danger-and-surfaces
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'eo-find '((confidence . "Confirmed")) '((confidence . "SAFE"))
              '((confidence . "Confirmed")) 20 *o-high* nil)
    (declare (ignore stamps))
    (is (equal "Confirmed" (cdr (assoc 'confidence merged)))
        "a release attempt keeps the DANGEROUS local value")
    (is (= 1 (length conflicts)) "and surfaces one conflict")
    (let ((c (first conflicts)))
      (is (equal "Confirmed" (graph-db::peer-conflict-kept-value c)))
      (is (equal "SAFE" (graph-db::peer-conflict-loser-value c)))
      (is (eq :safety (graph-db::peer-conflict-bucket c))))))

(test safety-reopen-toward-danger-auto-applies
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'eo-find '((confidence . "SAFE")) '((confidence . "Confirmed"))
              '((confidence . "SAFE")) 20 *o-high* nil)
    (is (equal "Confirmed" (cdr (assoc 'confidence merged)))
        "re-opening toward danger auto-applies (fail-safe)")
    (is (= 20 (car (cdr (assoc 'confidence stamps)))) "and stamps the winner")
    (is (null conflicts) "a toward-danger change never surfaces")))

(test safety-same-class-falls-back-to-lww
  ;; two DANGEROUS values -> same class -> LWW (incoming newer here)
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'eo-find '((confidence . "Suspected")) '((confidence . "Confirmed"))
              '((confidence . "Suspected")) 30 *o-high*
              (list (cons 'confidence (cons 10 *o-low*))))
    (declare (ignore stamps))
    (is (equal "Confirmed" (cdr (assoc 'confidence merged)))
        "same safety class resolves by Lamport")
    (is (null conflicts) "same-class LWW does not surface")))

;;; --- :geometry ------------------------------------------------------------

(test geometry-divergence-surfaces-keeps-local
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'survey '((geom . "poly-A")) '((geom . "poly-B")) '((geom . "poly-0"))
              20 *o-high* nil)
    (declare (ignore stamps))
    (is (equal "poly-A" (cdr (assoc 'geom merged))) "concurrent geometry keeps local")
    (is (= 1 (length conflicts)) "and surfaces")
    (is (equal "poly-B" (graph-db::peer-conflict-loser-value (first conflicts))))))

(test geometry-agree-no-conflict
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'survey '((geom . "poly-A")) '((geom . "poly-A")) '((geom . "poly-0"))
              20 *o-high* (list (cons 'geom (cons 5 *o-low*))))
    (is (equal "poly-A" (cdr (assoc 'geom merged))) "identical geometry is not a conflict")
    (is (= 20 (car (cdr (assoc 'geom stamps)))) "the field clock still advances")
    (is (null conflicts))))

;;; --- carried (unchanged) fields -------------------------------------------

(test carried-field-does-not-clobber-local
  ;; the op carries y unchanged (new y == old y) while changing x; y must be left
  ;; as the local value (a concurrent local edit to y is preserved).
  (multiple-value-bind (merged stamps conflicts)
      (merge3 'thing '((x . "a") (y . "local-y")) '((x . "b") (y . "base-y"))
              '((x . "a") (y . "base-y")) 8 *o-high* nil)
    (declare (ignore stamps conflicts))
    (is (equal "b" (cdr (assoc 'x merged))) "the changed field applies")
    (is (equal "local-y" (cdr (assoc 'y merged)))
        "the carried (unchanged) field keeps the local value")))
