;;;; tests/rules/validity-tests.lisp -- claim-current-valid/10 and the
;;;; validity route (GH #499, design §2, §4).
;;;;
;;;; The three-goal equivalence tests are the route's gate: they hold on
;;;; the fallback and must hold unchanged on the validity route.

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

;;; --- Task 3b: the validity route -----------------------------------------

(defun %seed-validity-history (g)
  "300 keys whose claims closed before *VALIDITY-T0*, then 30 open keys:
the history a valid-at page must not read (GH #499, design §2).  Returns
nothing."
  (let ((graph-db:*graph* g))
    (with-transaction ((graph-db::transaction-manager g))
      (dotimes (i 300)
        (make-rtt-claim-binary
         :graph g :subject-namespace :vt
         :subject-key (format nil "c~3,'0D" i)
         :relation "rel" :object-namespace :vt-obj
         :object-key "closed" :producer "history" :standing :observed
         :extent (interval (ts 2026 1 1) (ts 2026 2 1))))
      (dotimes (i 30)
        (make-rtt-claim-binary
         :graph g :subject-namespace :vt
         :subject-key (format nil "d~3,'0D" i)
         :relation "rel" :object-namespace :vt-obj
         :object-key "open" :producer "current" :standing :observed
         :extent (%validity-open-extent (ts 2026 1 1)))))))

(test the-validity-route-reads-no-closed-history
  "A 10-row fused page over 300 closed keys ahead of 30 open ones answers
under a 200-inference budget and resolves only the rows it needs; the
three-goal text signals at that budget (GH #499, design §2)."
  (with-rules-graph (g)
    (%seed-validity-history g)
    (let ((*validity-at* (validity-instant *validity-t0*)))
      (let ((rows nil) (calls 0))
        (counting-resolutions (resolved)
          (setf rows
                (select (:max-inferences 200 :limit 10) (?k)
                  (lisp ?at *validity-at*)
                  (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                       nil nil ?at)))
          (setf calls resolved))
        (is (= 10 (length rows)))
        (is (equal "d000" (first (first rows))))
        (is (<= 10 calls 12)))
      (signals graph-db::prolog-resource-error
        (select (:max-inferences 200 :limit 10) (?k)
          (lisp ?at *validity-at*)
          (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok nil nil)
          (true)
          (claim-current ?c)
          (claim-valid-at ?c ?at))))))

(test one-key-in-two-stores-pages-as-today
  "With both stores in scope, paging two rows at a time gives the same
sequence fused and unfused, a key's own-store claims first then each
foreign store's in id order (GH #499, design §2 step 3)."
  (with-two-stores (a b)
    (seed-validity a)
    (seed-validity-b b)
    (let ((graph-db::*claim-scope* (list a b))
          (*validity-at* (validity-instant *validity-t0*)))
      (flet ((fused (offset)
               (select (:limit 2 :skip offset) (?k ?ok ?p)
                 (lisp ?at *validity-at*)
                 (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                      nil nil ?at)
                 (claim-producer ?c ?p)))
             (unfused (offset)
               (select (:limit 2 :skip offset) (?k ?ok ?p)
                 (lisp ?at *validity-at*)
                 (claim-after ?c rtt-claim "vt" ?k "rel" ?ons ?ok nil nil)
                 (true)
                 (claim-current ?c)
                 (claim-valid-at ?c ?at)
                 (claim-producer ?c ?p))))
        (let ((seen 0))
          (loop for offset from 0 below 12 by 2
                do (let ((f (fused offset)) (u (unfused offset)))
                     (is (equal f u))
                     (incf seen (length f))))
          (is (= seen (length (%validity-fused)))
              "the pages cover the whole sequence"))
        (is (member "multi" (mapcar #'first (%validity-fused))
                    :test #'string=)
            "the key filed in both stores is on the page")))))

(test a-closed-set-over-the-cap-falls-back
  "With more closed tuples than *VALIDITY-CLOSED-CAP*, the route answers
as the three-goal text (GH #499, design §2)."
  (with-rules-graph (g)
    (seed-validity g)
    (let ((*validity-at* (validity-instant *validity-t0*))
          (graph-db::*validity-closed-cap* 2))
      (is (plusp (length (%validity-fused))))
      (%same-validity-rows (lambda () (%validity-fused))
                           (lambda () (%validity-unfused))))))

(test the-route-stays-off-under-each-fallback
  "The validity route resolves nothing under each fallback condition
(GH #499, design §2): a free T, a snapshot, a read-write transaction, a
closed set over the cap, and an :AS-OF read.  Without them it resolves
some, so the zeros are not vacuous."
  (with-rules-graph (g)
    (seed-validity g)
    (let ((*validity-at* (validity-instant *validity-t0*)))
      (labels ((resolutions (thunk)
                 (let ((calls 0))
                   (counting-resolutions (resolved)
                     (funcall thunk)
                     (setf calls resolved))
                   calls)))
        (is (plusp (resolutions (lambda () (%validity-fused)))))
        ;; A free T.
        (is (= 0 (resolutions
                  (lambda ()
                    (select () (?k ?ok ?p)
                      (claim-current-valid ?c rtt-claim "vt" ?k "rel"
                                           ?ons ?ok nil nil ?free)
                      (claim-producer ?c ?p))))))
        ;; A snapshot.
        (is (= 0 (resolutions
                  (lambda ()
                    (select (:snapshot t) (?k ?ok ?p)
                      (lisp ?at *validity-at*)
                      (claim-current-valid ?c rtt-claim "vt" ?k "rel"
                                           ?ons ?ok nil nil ?at)
                      (claim-producer ?c ?p))))))
        ;; A read-write transaction.
        (is (= 0 (resolutions
                  (lambda ()
                    (with-transaction
                        ((graph-db::transaction-manager g))
                      (%validity-fused))))))
        ;; A closed set over the cap.
        (is (= 0 (resolutions
                  (lambda ()
                    (let ((graph-db::*validity-closed-cap* 2))
                      (%validity-fused))))))
        ;; An :AS-OF read, one epoch below the next commit's.
        (let ((epoch (graph-db:latest-epoch g)))
          (with-transaction ((graph-db::transaction-manager g))
            (make-rtt-claim-binary
             :graph g :subject-namespace :vt :subject-key "zz-extra"
             :relation "rel" :object-namespace :vt-obj
             :object-key "x1" :producer "seed-a" :standing :observed
             :extent (interval (ts 2026 1 1) (ts 2026 12 31))))
          (is (= 0 (resolutions
                    (lambda ()
                      (select (:as-of epoch) (?k ?ok ?p)
                        (lisp ?at *validity-at*)
                        (claim-current-valid ?c rtt-claim "vt" ?k "rel"
                                             ?ons ?ok nil nil ?at)
                        (claim-producer ?c ?p)))))))))))

(test the-validity-route-skips-keys-before-the-bound
  "With ?SKEY-AFTER bound, the route skips the open keys at or before it
before resolving them, as #451's skip does (GH #499, design §2 step 2):
a 5-row page after \"d019\" resolves about its own rows, not the 20
keys before the bound."
  (with-rules-graph (g)
    (%seed-validity-history g)
    (let ((*validity-at* (validity-instant *validity-t0*))
          (rows nil)
          (calls 0))
      (counting-resolutions (resolved)
        (setf rows
              (select (:limit 5) (?k)
                (lisp ?at *validity-at*)
                (claim-current-valid ?c rtt-claim "vt" ?k "rel" ?ons ?ok
                                     "d019" nil ?at)))
        (setf calls resolved))
      (is (equal '("d020" "d021" "d022" "d023" "d024")
                 (mapcar #'first rows)))
      (is (<= 5 calls 7)))))

;;; --- Task 4: the rewrite, rule bodies and explain -----------------------

(defun %validity-rewritten (goals)
  "GOALS through the validity rewrite."
  (graph-db::%fuse-validity goals))

(test the-validity-rewrite-fuses-both-orders
  "The three-goal text, its filters in either order, fuses to
CLAIM-CURRENT-VALID/10 with both filters kept (GH #499, design §3)."
  (let* ((body '((claim ?c rt-claim "host" ?k "runs" ?ons ?o)
                 (claim-current ?c)
                 (claim-valid-at ?c "2026-06-01T00:00:00.000000000Z")))
         (out (%validity-rewritten body)))
    (is (= 3 (length out)))
    (is (string= "CLAIM-CURRENT-VALID" (symbol-name (first (first out)))))
    (is (equal '(?c rt-claim "host" ?k "runs" ?ons ?o nil nil
                 "2026-06-01T00:00:00.000000000Z")
               (rest (first out))))
    (is (equal (rest body) (rest out))))
  (let* ((body '((claim ?c rt-claim "host" ?k "runs" ?ons ?o)
                 (claim-valid-at ?c ?t)
                 (claim-current ?c)))
         (out (%validity-rewritten body)))
    (is (= 3 (length out)))
    (is (equal '(?c rt-claim "host" ?k "runs" ?ons ?o nil nil ?t)
               (rest (first out))))
    (is (equal (rest body) (rest out)))))

(test the-validity-rewrite-fuses-behind-a-key-bound
  "The key-bound rewrite's output -- CLAIM-AFTER/9 and its kept
comparison -- fuses too, the comparison kept (GH #499, design §3)."
  (let* ((body '((claim-after ?c rt-claim "host" ?k "runs" ?ons ?o
                              "k1" nil)
                  (> ?k "k1")
                  (claim-current ?c)
                  (claim-valid-at ?c ?t)))
         (out (%validity-rewritten body)))
    (is (= 4 (length out)))
    (is (string= "CLAIM-CURRENT-VALID" (symbol-name (first (first out)))))
    (is (equal '(?c rt-claim "host" ?k "runs" ?ons ?o "k1" nil ?t)
               (rest (first out))))
    (is (equal '(> ?k "k1") (second out)))
    (is (equal '((claim-current ?c) (claim-valid-at ?c ?t))
               (cddr out)))))

(test the-validity-rewrite-leaves-every-other-shape-alone
  "Everything outside design §3's conditions is returned EQ (GH #499):
one filter, a goal between, another ?C, a ?C bound earlier, a T the
claim goal binds, a numeric T, no claim goal, and nothing."
  (flet ((same (body) (is (eq body (%validity-rewritten body)))))
    (same '((claim ?c rt-claim "host" ?k "runs" ?ons ?o)
            (claim-current ?c)))
    (same '((claim ?c rt-claim "host" ?k "runs" ?ons ?o)
            (true)
            (claim-current ?c)
            (claim-valid-at ?c "2026-06-01T00:00:00.000000000Z")))
    (same '((claim ?c rt-claim "host" ?k "runs" ?ons ?o)
            (claim-current ?d)
            (claim-valid-at ?c "2026-06-01T00:00:00.000000000Z")))
    (same '((claim-touched-after ?c ?t "rtt-claim")
            (claim ?c rt-claim "host" ?k "runs" ?ons ?o)
            (claim-current ?c)
            (claim-valid-at ?c "2026-06-01T00:00:00.000000000Z")))
    (same '((claim ?c rt-claim "host" ?k "runs" ?ons ?ok)
            (claim-current ?c)
            (claim-valid-at ?c ?ok)))
    (same '((claim ?c rt-claim "host" ?k "runs" ?ons ?o)
            (claim-current ?c)
            (claim-valid-at ?c 5)))
    (same '((claim-current ?c) (claim-valid-at ?c ?t)))
    (same '())))

(test the-rewriters-run-in-order
  "The validity rewrite runs after the key-bound and cursor rewrites, so
it sees the CLAIM-AFTER/9 the first makes (GH #499, design §3)."
  (let* ((order graph-db:*clause-body-rewriters*)
         (validity (position 'graph-db::%fuse-validity order))
         (keys (position 'graph-db::%fuse-key-bounds order))
         (cursor (position 'graph-db::%fuse-touched-cursor order)))
    (is (not (null validity)))
    (is (not (null keys)))
    (is (not (null cursor)))
    (is (> validity keys))
    (is (> validity cursor))
    (is (eq 'graph-db::%fuse-validity (car (last order))))))

(test the-recommended-valid-at-text-reads-no-closed-history
  "The three goals plus a > on the key, as the docs recommend, fuse to
the validity route: the page answers under Task 3's 200-inference
budget and resolves only its own rows (GH #499, design §3)."
  (with-rules-graph (g)
    (%seed-validity-history g)
    (let ((*validity-at* (validity-instant *validity-t0*))
          (rows nil) (calls 0))
      (counting-resolutions (resolved)
        (setf rows
              (select (:max-inferences 200 :limit 10) (?k)
                (lisp ?at *validity-at*)
                (claim ?c rtt-claim "vt" ?k "rel" ?ons ?ok)
                (> ?k "c999")
                (claim-current ?c)
                (claim-valid-at ?c ?at)))
        (setf calls resolved))
      (is (= 10 (length rows)))
      (is (equal "d000" (first (first rows))))
      (is (<= 10 calls 12)))))

(test the-guard-path-is-fused-for-valid-at
  "The recommended valid-at text through RUN-GUARDED-PROLOG answers its
page under the same budget, so the guard's compile fused it (GH #499,
design §3)."
  (with-rules-graph (g)
    (%seed-validity-history g)
    (let ((graph-db::*query-default-max-inferences* 200))
      (multiple-value-bind (columns rows)
          (graph-db.query:run-guarded-prolog
           (format nil "(claim ?c rtt-claim \"vt\" ?k \"rel\" ?ons ?ok) ~
(> ?k \"c999\") (claim-current ?c) (claim-valid-at ?c ~S)"
                   (validity-instant *validity-t0*))
           g :limit 10)
        (let ((k (position "k" columns :test #'string=)))
          (is (= 10 (length rows)))
          (is (equal "d000" (nth k (first rows)))))))))

(defun %vf-head (relation)
  "A VREACHES-shaped rtt-claim head over RELATION."
  (format nil "(claim ?c rtt-claim \"vt\" ?a ~S \"vt\" ?b)" relation))

(defun %vf-body (at relation &key true-between step)
  "A closure body over \"next\" and RELATION.  The three goals read
\"next\", or with STEP the recursive RELATION premise after a \"next\"
goal; TRUE-BETWEEN puts (TRUE) before their filters."
  (let ((filters (format nil "~A (claim-current ?q) (claim-valid-at ?q ~S)"
                         (if true-between " (true)" "") at)))
    (if step
        (format nil "(claim ?p rtt-claim \"vt\" ?a \"next\" \"vt\" ?m) ~
(claim ?q rtt-claim \"vt\" ?m ~S \"vt\" ?b)~A" relation filters)
        (format nil "(claim ?q rtt-claim \"vt\" ?a \"next\" \"vt\" ?b)~A"
                filters))))

(defun %vf-link (g from to)
  "A \"next\" rtt-claim FROM -> TO open from 2026-01-01, producer
\"seed-a\"."
  (with-transaction ((graph-db::transaction-manager g))
    (make-rtt-claim-binary
     :graph g :subject-namespace :vt :subject-key from
     :relation "next" :object-namespace :vt :object-key to
     :producer "seed-a" :standing :observed
     :extent (%validity-open-extent (ts 2026 1 1)))))

(defun %vf-pairs (g relation)
  "The (subject . object) pairs of the current claims of RELATION,
sorted."
  (sort (mapcar (lambda (c)
                  (cons (claim-subject-key c) (claim-object-key c)))
                (remove-if-not #'claim-current-p
                               (claims-by-relation g 'rtt-claim relation)))
        (lambda (a b)
          (if (string= (car a) (car b))
              (string< (cdr a) (cdr b))
              (string< (car a) (car b))))))

(test a-fused-rule-body-derives-as-the-three-goals
  "Rules whose premises carry the two filters derive what the same
bodies with a (TRUE) before them derive, and the rewrite fused one
(GH #499, design §3).  Trap: a semi-naive variant moves the recursive
premise first, so only the full evaluation runs it fused."
  (with-rules-graph (g)
    (%vf-link g "a" "b")
    (%vf-link g "b" "c")
    (%vf-link g "c" "d")
    (let* ((at (validity-instant *validity-t0*))
           (fused 0)
           ;; Counts the bodies the rewrite changes, so the agreement
           ;; below is between a fused and an unfused body.
           (graph-db:*clause-body-rewriters*
             (substitute (lambda (goals)
                           (let ((out (graph-db::%fuse-validity goals)))
                             (unless (eq out goals) (incf fused))
                             out))
                         'graph-db::%fuse-validity
                         graph-db:*clause-body-rewriters*)))
      (write-rule g :name "vf-base" :version "1" :family "rtt-claim"
                  :head (%vf-head "freaches")
                  :body (%vf-body at "freaches"))
      (write-rule g :name "vf-step" :version "1" :family "rtt-claim"
                  :head (%vf-head "freaches")
                  :body (%vf-body at "freaches" :step t))
      (write-rule g :name "vu-base" :version "1" :family "rtt-claim"
                  :head (%vf-head "ureaches")
                  :body (%vf-body at "ureaches" :true-between t))
      (write-rule g :name "vu-step" :version "1" :family "rtt-claim"
                  :head (%vf-head "ureaches")
                  :body (%vf-body at "ureaches" :step t :true-between t))
      (graph-db.rules:run-rules g)
      (is (plusp fused) "a rule body was fused")
      (is (equal (%vf-pairs g "freaches") (%vf-pairs g "ureaches")))
      (is (member (cons "a" "d") (%vf-pairs g "freaches")
                  :test #'equal)
          "the fused premise read two hops this run derived"))))

(test the-validity-rewrite-passes-a-cut
  "A body holding an atom goal such as ! rewrites without signalling:
the three goals after a cut fuse, a cut between them blocks the fusion
and returns the body EQ, and a cut alone is left alone (GH #499)."
  (let* ((body '(! (claim ?c rt-claim "host" ?k "runs" ?ons ?o)
                 (claim-current ?c)
                 (claim-valid-at ?c "2026-06-01T00:00:00.000000000Z")
                 !))
         (out (%validity-rewritten body)))
    (is (eq '! (first out)))
    (is (string= "CLAIM-CURRENT-VALID" (symbol-name (first (second out)))))
    (is (eq '! (car (last out)))))
  (let ((body '((claim ?c rt-claim "host" ?k "runs" ?ons ?o)
                !
                (claim-current ?c)
                (claim-valid-at ?c "2026-06-01T00:00:00.000000000Z"))))
    (is (eq body (%validity-rewritten body))))
  (let ((body '(!)))
    (is (eq body (%validity-rewritten body)))))
