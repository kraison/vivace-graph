;;;; The Allen interval algebra over extents whose endpoints are ranges.
;;;;
;;;; The thirteen relations are determined by the signs of four endpoint
;;;; comparisons; an :AMBIGUOUS comparison is a wildcard, so an imprecise
;;;; extent yields a SET (GH #130, design §4.1).

(in-package #:graph-db.spacetime)

(defparameter +allen-relations+
  '(:before :meets :overlaps :finished-by :contains :starts :equals
    :started-by :during :finishes :overlapped-by :met-by :after)
  "The closed relation vocabulary.  Thirteen, not fourteen: :EQUALS is its
own inverse.")

(defparameter +allen-inverses+
  '((:before . :after) (:meets . :met-by) (:overlaps . :overlapped-by)
    (:finished-by . :finishes) (:contains . :during) (:starts . :started-by)
    (:equals . :equals) (:started-by . :starts) (:during . :contains)
    (:finishes . :finished-by) (:overlapped-by . :overlaps)
    (:met-by . :meets) (:after . :before)))

(defun allen-inverse (relation)
  "The relation R such that (R b a) holds exactly when (RELATION a b) does."
  (or (cdr (assoc relation +allen-inverses+))
      (error 'invalid-extent
             :reason (format nil "~S is not an Allen relation" relation))))

(defparameter +allen-signatures+
  ;; (relation s1?s2 s1?e2 e1?s2 e1?e2), read off canonical NON-degenerate
  ;; examples.  Degenerate extents do not obey this table -- see the instant
  ;; path (design §3.3.1).
  '((:before        :< :< :< :<)
    (:meets         :< :< := :<)
    (:overlaps      :< :< :> :<)
    (:finished-by   :< :< :> :=)
    (:contains      :< :< :> :>)
    (:starts        := :< :> :<)
    (:equals        := :< :> :=)
    (:started-by    := :< :> :>)
    (:during        :> :< :> :<)
    (:finishes      :> :< :> :=)
    (:overlapped-by :> :< :> :>)
    (:met-by        :> := :> :>)
    (:after         :> :> :> :>)))

(defstruct (temporal-relation (:copier nil))
  "RELATIONS is never empty: two extents always stand in at least one Allen
relation, and total ignorance is all thirteen rather than none.  STANDINGS
and SEMANTICS carry both endpoints' values -- not a collapse (design §4.4)."
  (relations nil :read-only t)
  (standings nil :read-only t)
  (semantics nil :read-only t))

(defun %compatible-p (computed expected)
  "An :AMBIGUOUS comparison constrains nothing, so it matches any sign."
  (or (eq computed :ambiguous) (eq computed expected)))

(defun %interval-relations (a b)
  "The relations consistent with A and B's four endpoint comparisons.
Correct only when NEITHER extent is an instant."
  (let ((c1 (bound-compare (extent-start a) (extent-start b)))
        (c2 (bound-compare (extent-start a) (extent-end b)))
        (c3 (bound-compare (extent-end a) (extent-start b)))
        (c4 (bound-compare (extent-end a) (extent-end b))))
    (loop for (rel s1 s2 s3 s4) in +allen-signatures+
          when (and (%compatible-p c1 s1) (%compatible-p c2 s2)
                    (%compatible-p c3 s3) (%compatible-p c4 s4))
            collect rel)))

(defun %instant-vs-instant (a b)
  "Two points relate only three ways.  :AMBIGUOUS admits all three."
  (let ((c (bound-compare (extent-start a) (extent-start b))))
    (ecase c
      (:< '(:before))
      (:= '(:equals))
      (:> '(:after))
      (:ambiguous '(:before :equals :after)))))

(defun %instant-vs-interval (p i)
  "Point P against interval I, per the design §3.3.1 table.  :MEETS and the
other eight are unreachable: under closed intervals a point at I's start is
INSIDE I, so :STARTS states strictly more than :MEETS."
  (let ((cs (bound-compare (extent-start p) (extent-start i)))
        (ce (bound-compare (extent-start p) (extent-end i)))
        (rels '()))
    (flet ((maybe (comparison &rest admissible)
             (member comparison admissible)))
      (when (maybe cs :< :ambiguous) (push :before rels))
      (when (maybe cs := :ambiguous) (push :starts rels))
      (when (and (maybe cs :> :ambiguous) (maybe ce :< :ambiguous))
        (push :during rels))
      (when (maybe ce := :ambiguous) (push :finishes rels))
      (when (maybe ce :> :ambiguous) (push :after rels)))
    (nreverse rels)))

(defun %relations-between (a b)
  "Dispatch on degeneracy: the signature table is read off non-degenerate
examples and does not describe instants (design §3.3.1)."
  (let ((ai (extent-instant-p a))
        (bi (extent-instant-p b)))
    (cond ((and ai bi) (%instant-vs-instant a b))
          (ai (%instant-vs-interval a b))
          (bi (mapcar #'allen-inverse (%instant-vs-interval b a)))
          (t (%interval-relations a b)))))

(defun allen-relations (a b)
  "The TEMPORAL-RELATION between extents A and B: every Allen relation
consistent with their endpoint ranges, plus both standings and semantics."
  (let ((rels (%relations-between a b)))
    (assert rels ()
            "empty relation set for ~S vs ~S -- a signature table bug" a b)
    (make-temporal-relation
     :relations rels
     :standings (remove-duplicates
                 (list (extent-standing a) (extent-standing b)))
     :semantics (remove-duplicates
                 (list (extent-semantics a) (extent-semantics b))))))

(defun allen-relation (a b)
  "The single relation between A and B when the answer is definite, else
NIL.  NIL means \"more than one relation is possible\", never \"unrelated\"."
  (let ((rels (temporal-relation-relations (allen-relations a b))))
    (when (null (cdr rels))
      (car rels))))

(defun allen-definite-p (a b)
  "True when exactly one relation is possible between A and B."
  (null (cdr (temporal-relation-relations (allen-relations a b)))))

(defmacro %define-relation-predicate (name relation)
  `(defun ,name (a b)
     ,(format nil "True when ~S is possible between extents A and B."
              relation)
     (and (member ,relation (temporal-relation-relations
                             (allen-relations a b)))
          t)))

(%define-relation-predicate extent-before-p :before)
(%define-relation-predicate extent-meets-p :meets)
(%define-relation-predicate extent-overlaps-p :overlaps)
(%define-relation-predicate extent-finished-by-p :finished-by)
(%define-relation-predicate extent-contains-p :contains)
(%define-relation-predicate extent-starts-p :starts)
(%define-relation-predicate extent-equals-p :equals)
(%define-relation-predicate extent-started-by-p :started-by)
(%define-relation-predicate extent-during-p :during)
(%define-relation-predicate extent-finishes-p :finishes)
(%define-relation-predicate extent-overlapped-by-p :overlapped-by)
(%define-relation-predicate extent-met-by-p :met-by)
(%define-relation-predicate extent-after-p :after)
