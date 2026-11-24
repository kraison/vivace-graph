;;;; tests/rules/validity-tests.lisp -- claim-current-valid/10 and the
;;;; validity route (GH #499, design §2, §4).
;;;;
;;;; The route lands in Task 3b; these tests pin the functor's contract
;;;; on the fallback now, and become the route's equivalence gate then.

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defvar *validity-at* nil
  "The instant a looped validity test binds: a goal argument with no
prolog variable is quoted, so LISP/2 reads it with SYMBOL-VALUE
(GH #499).")
(declaim (special *validity-at*))

(defun validity-instant (timestamp)
  "TIMESTAMP in the RFC 3339 form CLAIM-VALID-AT/2 parses."
  (graph-db::%instant-string timestamp))

(defun %same-validity-rows (fused unfused)
  "Assert the thunks FUSED and UNFUSED answer the same rows in the same
order -- CLAIM-CURRENT-VALID/10's equivalence gate (GH #499, §2)."
  (is (equal (funcall fused) (funcall unfused))))

(defmacro %validity-fused ()
  "The baseline fused query at *VALIDITY-AT*: rows (?K ?OK ?P)."
  `(select () (?k ?ok ?p)
     (lisp ?at *validity-at*)
     (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                          nil nil ?at)
     (claim-producer ?c ?p)))

(defmacro %validity-unfused ()
  "The baseline three-goal text at *VALIDITY-AT*: CLAIM-AFTER/9, a
TRUE that keeps it unfused, then the two filters."
  `(select () (?k ?ok ?p)
     (lisp ?at *validity-at*)
     (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok nil nil)
     (true)
     (claim-current ?c)
     (claim-valid-at ?c ?at)
     (claim-producer ?c ?p)))

(test the-fused-functor-answers-as-the-three-goals
  "For every instant the contract names, CLAIM-CURRENT-VALID/10 by hand
answers exactly CLAIM-AFTER/9 followed by (TRUE) (CLAIM-CURRENT ?C)
\(CLAIM-VALID-AT ?C ?AT), row for row and in order -- with no key bound,
a subject key bound, an object key bound on the subject route, and the
object vocabulary route (GH #499, design §2)."
  (with-rules-graph (g)
    (seed-validity g)
    (dolist (instant *validity-instants*)
      (let ((*validity-at* (validity-instant instant)))
        ;; No key bound.
        (%same-validity-rows (lambda () (%validity-fused))
                             (lambda () (%validity-unfused)))
        ;; A subject key bound.
        (%same-validity-rows
         (lambda ()
           (select () (?k ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                  "a4" nil ?at)
             (claim-producer ?c ?p)))
         (lambda ()
           (select () (?k ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                          "a4" nil)
             (true)
             (claim-current ?c)
             (claim-valid-at ?c ?at)
             (claim-producer ?c ?p))))
        ;; An object key bound on the subject route.
        (%same-validity-rows
         (lambda ()
           (select () (?k ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                  nil "o3" ?at)
             (claim-producer ?c ?p)))
         (lambda ()
           (select () (?k ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                          nil "o3")
             (true)
             (claim-current ?c)
             (claim-valid-at ?c ?at)
             (claim-producer ?c ?p))))
        ;; The object vocabulary route, its key bound filtering.
        (%same-validity-rows
         (lambda ()
           (select () (?s ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-current-valid ?c rtt-claim ?sns ?s ?r "vt-obj" ?ok
                                  nil "o3" ?at)
             (claim-producer ?c ?p)))
         (lambda ()
           (select () (?s ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-after ?c rtt-claim ?sns ?s ?r "vt-obj" ?ok
                          nil "o3")
             (true)
             (claim-current ?c)
             (claim-valid-at ?c ?at)
             (claim-producer ?c ?p))))))
    ;; The subject route's row set at T0 is not empty, so the agreement
    ;; above is never vacuous there.
    (let ((*validity-at* (validity-instant *validity-t0*)))
      (is (member "a1" (mapcar #'first (%validity-fused))
                  :test #'string=))))
  ;; Two stores in scope merge as the fallback's routes do.
  (with-two-stores (a b)
    (seed-validity a)
    (seed-validity-b b)
    (let ((graph-db::*claim-scope* (list a b))
          (*validity-at* (validity-instant *validity-t0*)))
      (%same-validity-rows (lambda () (%validity-fused))
                           (lambda () (%validity-unfused))))))

(test t-through-a-variable-bound-earlier
  "?AT bound by (= ?T ...) before the goal answers as the literal does
(GH #499, design §2)."
  (with-rules-graph (g)
    (seed-validity g)
    (is (equal
         (select () (?k ?ok ?p)
           (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                nil nil "2026-06-01T00:00:00.000000000Z")
           (claim-producer ?c ?p))
         (select () (?k ?ok ?p)
           (= ?at "2026-06-01T00:00:00.000000000Z")
           (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                nil nil ?at)
           (claim-producer ?c ?p))))))

(test the-fallback-is-refused-at-the-budget
  "Inside a read-write transaction the functor falls back, and every
candidate it resolves ticks once (GH #499, design §4): 300 closed keys
ahead of the page refuse a 200-inference budget.  The unfused text
refuses too, at its own goals' ticks."
  (with-rules-graph (g)
    (with-transaction ((graph-db::transaction-manager g))
      (dotimes (i 300)
        (make-rtt-claim-binary
         :graph g :subject-namespace :vt
         :subject-key (format nil "c~3,'0D" i)
         :relation "rel" :object-namespace :vt-obj
         :object-key "closed" :producer "closer" :standing :observed
         :extent (interval (ts 2026 1 1) (ts 2026 2 1)))))
    (let ((*validity-at* (validity-instant *validity-t0*)))
      (with-transaction ((graph-db::transaction-manager g))
        (signals graph-db::prolog-resource-error
          (select (:max-inferences 200 :limit 10) (?k)
            (lisp ?at *validity-at*)
            (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                 nil nil ?at)))
        (signals graph-db::prolog-resource-error
          (select (:max-inferences 200 :limit 10) (?k)
            (lisp ?at *validity-at*)
            (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok nil nil)
            (true)
            (claim-current ?c)
            (claim-valid-at ?c ?at)))))))

(test a-retracted-or-unstamped-claim-is-answered-as-claim-current-says
  "An unstamped claim is current, so it is answered; a retracted claim
is not, though CLAIM-AFTER/9 alone would return the retracted open one
(GH #499, design §1, §2).  The fused and three-goal sides agree."
  (with-rules-graph (g)
    (seed-validity g)
    (let* ((*validity-at* (validity-instant *validity-t0*))
           (fused (%validity-fused))
           (unfused (%validity-unfused))
           (keys (mapcar #'first fused))
           (all (select-flat (?k)
                  (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                               nil nil))))
      (is (equal fused unfused))
      (is (member "u1" keys :test #'string=)
          "an unstamped claim is current")
      (is (not (member "a7" keys :test #'string=))
          "a retracted open claim is not")
      (is (not (member "a8" keys :test #'string=))
          "a retracted closed claim is not")
      (is (member "a7" all :test #'string=)
          "CLAIM-AFTER/9 alone answers the retracted open claim"))))

(test each-fallback-answers-the-same
  "Every condition that keeps the validity route off answers as the
three-goal text (GH #499, design §2): a free T, an :AS-OF read, a
snapshot, and a read-write transaction -- including one whose view a
later commit's close cannot reach.  The closed-set cap is the route's
own test (Task 3b)."
  (with-rules-graph (g)
    (seed-validity g)
    (let ((*validity-at* (validity-instant *validity-t0*)))
      ;; A free T.
      (%same-validity-rows
       (lambda ()
         (select () (?k ?ok ?p)
           (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                nil nil ?free)
           (claim-producer ?c ?p)))
       (lambda ()
         (select () (?k ?ok ?p)
           (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok nil nil)
           (true)
           (claim-current ?c)
           (claim-valid-at ?c ?free)
           (claim-producer ?c ?p))))
      ;; :AS-OF, one epoch below the next commit's.
      (let ((epoch (graph-db:latest-epoch g)))
        (with-transaction ((graph-db::transaction-manager g))
          (make-rtt-claim-binary
           :graph g :subject-namespace :vt :subject-key "zz-extra"
           :relation "rel" :object-namespace :vt-obj
           :object-key "x1" :producer "seed-a" :standing :observed
           :extent (interval (ts 2026 1 1) (ts 2026 12 31))))
        (is (equal
             (select (:as-of epoch) (?k ?ok ?p)
               (lisp ?at *validity-at*)
               (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                    nil nil ?at)
               (claim-producer ?c ?p))
             (select (:as-of epoch) (?k ?ok ?p)
               (lisp ?at *validity-at*)
               (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok nil nil)
               (true)
               (claim-current ?c)
               (claim-valid-at ?c ?at)
               (claim-producer ?c ?p)))))
      ;; A snapshot.
      (is (equal
           (select (:snapshot t) (?k ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                  nil nil ?at)
             (claim-producer ?c ?p))
           (select (:snapshot t) (?k ?ok ?p)
             (lisp ?at *validity-at*)
             (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok nil nil)
             (true)
             (claim-current ?c)
             (claim-valid-at ?c ?at)
             (claim-producer ?c ?p))))
      ;; A read-write transaction.
      (with-transaction ((graph-db::transaction-manager g))
        (%same-validity-rows (lambda () (%validity-fused))
                             (lambda () (%validity-unfused))))
      ;; Another thread closes a claim after this transaction began; at
      ;; its start epoch the close is invisible, and both sides agree.
      (let ((before
              (catch 'validity-started
                (with-transaction ((graph-db::transaction-manager g))
                  (bt:join-thread
                   (bt:make-thread
                    (lambda ()
                      (let ((graph-db:*graph* g))
                        (with-transaction ()
                          (close-validity
                           (first (claims-touching
                                   g 'rtt-claim :vt "thread"
                                   :role :subject :relation "rel"))
                           (ts 2025 6 1)))))))
                  (throw 'validity-started
                         (list (%validity-fused)
                               (%validity-unfused)))))))
        (destructuring-bind (fused unfused) before
          (is (equal fused unfused))
          (is (member "thread" (mapcar #'first fused) :test #'string=)
              "the transaction reads the claim as it began")))
      ;; The close committed, so both sides drop the claim now.
      (let ((after (%validity-fused)))
        (is (not (member "thread" (mapcar #'first after)
                         :test #'string=)))
        (%same-validity-rows (lambda () (%validity-fused))
                             (lambda () (%validity-unfused)))))))
