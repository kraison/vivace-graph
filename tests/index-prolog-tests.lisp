;;;; The general ordered index, reached from Prolog (GH #102).
;;;;
;;;; The Lisp half shipped complete -- declaration, subclass scoping, commit-path
;;;; maintenance, a durable sidecar, INDEX-LOOKUP / INDEX-RANGE / MAP-INDEX --
;;;; and none of the 58 Prolog functors touched it.  NODE-SLOT-VALUE/3 is a
;;;; FILTER, not a lookup: it reads a slot off a node it was handed.  So a query
;;;; over an indexed slot generated candidates with IS-A/2 (every instance of the
;;;; class) and filtered them one at a time -- O(instances) where an O(log n)
;;;; index for exactly that lookup was already being maintained on every commit.
;;;;
;;;; These are GENERATING predicates: they call the continuation once per hit.
;;;; GEO-WITHIN/3 is the obvious template and the wrong one -- it filters bound
;;;; arguments.  FIND-WITHIN/3 in spatial-query.lisp is the right one.

(in-package #:graph-db/test)

(def-suite index-prolog-suite
  :description "Index-backed generator predicates: find-by-slot, find-slot-range."
  :in graph-db-suite)

(in-suite index-prolog-suite)

(defparameter *ixp-graph-name* :graph-db-index-prolog-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ixp-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex ixp-user ()
  ((email :initarg :email :accessor ixp-email :index t)
   (age   :initarg :age   :accessor ixp-age   :index t)
   (note  :initarg :note  :accessor ixp-note))          ; NOT indexed
  :graph-db-index-prolog-test)

;; A subclass, to prove a lookup on it resolves to the ancestor's index.
(def-vertex ixp-admin (ixp-user)
  ((level :initarg :level :accessor ixp-level))
  :graph-db-index-prolog-test)

(defmacro with-ixp-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ixp-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defmacro with-ixp-people ((g) &body body)
  "Four users aged 20/30/40 plus an admin aged 50, all with distinct emails."
  `(with-ixp-graph (,g)
     (with-transaction ()
       (make-ixp-user  :email "a@x.com" :age 20 :note "n")
       (make-ixp-user  :email "b@x.com" :age 30 :note "n")
       (make-ixp-user  :email "c@x.com" :age 40 :note "n")
       (make-ixp-admin :email "d@x.com" :age 50 :level 9))
     (locally ,@body)))

(defun ixp-emails (nodes)
  (sort (mapcar #'ixp-email nodes) #'string<))


;;; --- equality ---------------------------------------------------------------

(test find-by-slot-generates-the-matching-node
  "⚠ GENERATES, not filters.  ?NODE is unbound on the way in and is bound once
per hit -- that is the whole point, and what NODE-SLOT-VALUE/3 cannot do."
  (with-ixp-people (g)
    (declare (ignore g))
    (let ((hits (select-flat (?n) (find-by-slot ?n ixp-user email "b@x.com"))))
      (is (= 1 (length hits)))
      (is (string= "b@x.com" (ixp-email (first hits)))))))

(test find-by-slot-yields-nothing-for-an-absent-value
  "A value no node carries is an ordinary empty result, not an error."
  (with-ixp-people (g)
    (declare (ignore g))
    (is (null (select-flat (?n) (find-by-slot ?n ixp-user email "nobody@x.com"))))))

(test find-by-slot-spans-subclasses
  "An index is rooted at the declaring class and covers its subclasses, so the
admin must be found through a query naming the ancestor."
  (with-ixp-people (g)
    (declare (ignore g))
    (let ((hits (select-flat (?n) (find-by-slot ?n ixp-user email "d@x.com"))))
      (is (= 1 (length hits)))
      (is (typep (first hits) 'ixp-admin)))))

(test find-by-slot-resolves-a-subclass-argument-to-the-owning-index
  "⚠ The reverse direction, and the one #102 flagged: the index lives on
IXP-USER, and a query naming IXP-ADMIN must resolve UP to it rather than
signalling that the subclass has no index of its own."
  (with-ixp-people (g)
    (declare (ignore g))
    (let ((hits (select-flat (?n) (find-by-slot ?n ixp-admin email "d@x.com"))))
      (is (= 1 (length hits)))
      (is (string= "d@x.com" (ixp-email (first hits)))))))

(test find-by-slot-signals-on-an-unindexed-slot
  "⚠ Signal, not silent failure.  Silence would make an un-indexed slot look
like a slot with no matching rows -- the absence-as-value trap -- and a scan
fallback would make the predicate's cost unpredictable.  Inherited from
%REQUIRE-INDEX rather than re-decided here."
  (with-ixp-people (g)
    (declare (ignore g))
    (signals error (select-flat (?n) (find-by-slot ?n ixp-user note "n")))))


;;; --- range ------------------------------------------------------------------

(test find-slot-range-generates-in-ascending-order
  (with-ixp-people (g)
    (declare (ignore g))
    (let ((hits (select-flat (?n) (find-slot-range ?n ixp-user age 25 45))))
      (is (equal '("b@x.com" "c@x.com") (ixp-emails hits)))
      (is (equal '(30 40) (mapcar #'ixp-age hits))
          "ascending value order, as MAP-INDEX yields them"))))

(test find-slot-range-is-inclusive-at-both-bounds
  (with-ixp-people (g)
    (declare (ignore g))
    (is (equal '("a@x.com" "b@x.com" "c@x.com")
               (ixp-emails (select-flat (?n)
                             (find-slot-range ?n ixp-user age 20 40)))))))

(test find-slot-range-treats-nil-as-open-ended
  "MAP-INDEX's own convention, surfaced rather than reinvented."
  (with-ixp-people (g)
    (declare (ignore g))
    (is (equal '("c@x.com" "d@x.com")
               (ixp-emails (select-flat (?n)
                             (find-slot-range ?n ixp-user age 40 nil))))
        "open above")
    (is (equal '("a@x.com" "b@x.com")
               (ixp-emails (select-flat (?n)
                             (find-slot-range ?n ixp-user age nil 30))))
        "open below")))

(test find-slot-range-treats-an-unbound-bound-as-open-ended
  "⚠ An unbound variable means \"no bound on that side\", so a caller need not
know NIL is the sentinel.  Without this, an unbound ?START would deref to a
variable struct and be handed to the index as a key."
  (with-ixp-people (g)
    (declare (ignore g))
    (is (equal '("c@x.com" "d@x.com")
               (ixp-emails (select-flat (?n)
                             (find-slot-range ?n ixp-user age 40 ?end)))))))

(test find-slot-range-signals-on-an-unindexed-slot
  (with-ixp-people (g)
    (declare (ignore g))
    (signals error (select-flat (?n) (find-slot-range ?n ixp-user note nil nil)))))


;;; --- the property that motivated the issue ----------------------------------

(test find-by-slot-does-not-scan-the-class
  "⚠ Non-vacuous proof that the INDEX is what answered, not a scan.  The index
is consulted through %REQUIRE-INDEX; a scan-based implementation would still
return the right node here, so the discriminator is that an UNINDEXED slot
signals while an indexed one succeeds -- a scan could serve both."
  (with-ixp-people (g)
    (declare (ignore g))
    (is (= 1 (length (select-flat (?n) (find-by-slot ?n ixp-user age 30)))))
    (signals error (select-flat (?n) (find-by-slot ?n ixp-user note "n")))))
