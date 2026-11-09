;;;; The Prolog Engine 5.0 before-and-after cases (GH #45): one measured
;;;; case per performance unit of the 5.0 scope, so each unit's "before"
;;;; is recorded before its code changes.  Own graph names, own schema and
;;;; own functors (pbp-fact, pbp-path) so no existing bench's work changes
;;;; (the generation rule, suite.lisp).  Reported, not gated: the numbers
;;;; go to the unit's issue.

(in-package #:graph-db/perf-test)

(defparameter *perf-prolog-graph-name* :graph-db-perf-prolog)

(eval-when (:load-toplevel :execute)
  (setf (gethash *perf-prolog-graph-name* *schema-node-metadata*) nil))

(graph-db.spacetime:def-source pbp-thing :graph-db-perf-prolog
    ((thing-id :initarg :thing-id :accessor pbp-thing-id))
  :identity     (:namespace :pbp-things :key-slot thing-id)
  :space        :none
  :time         :none
  :attribution  (:licence "CC0-1.0" :citation "perf fixtures")
  :sensitivity  (:class :public)
  :registration :none
  :indexed-text :none)

(graph-db.spacetime:def-claim-classes pbp-claim :graph-db-perf-prolog)

(defvar *pbp-probe* nil
  "The key a bound-first-argument lookup reads through (lisp ...).")

(defvar *pbp-start* nil
  "The start vertex a path or hop query reads through (lisp ...).")

(defmacro with-perf-prolog-graph ((g) &body body)
  "A fresh on-disk graph holding PBP-CLAIM claims."
  (let ((d (gensym "DIR")))
    `(with-temp-directory (,d)
       (let ((,g (make-graph *perf-prolog-graph-name* (namestring ,d)
                             :buffer-pool-size 4000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defun pbp-key (i) (format nil "k~5,'0D" i))

(defun pbp-insert-claims (n &key (start 0) (batch 500))
  "Unary claims for keys START..N-1 under :PBP-THINGS, one relation."
  (let ((i start))
    (loop while (< i n) do
      (graph-db:with-transaction ()
        (dotimes (k (min batch (- n i)))
          (make-pbp-claim-unary :subject-namespace :pbp-things
                                :subject-key (pbp-key i) :relation "rel"
                                :producer "perf" :standing :inferred)
          (incf i))))))

(defun pbp-assert (clause)
  "Assert CLAUSE as the <- macro does (prologc.lisp:807), with *PACKAGE*
bound here: add-clause interns the functor name into it, and the queries
in this file read their functors from this package."
  (let ((*package* (find-package '#:graph-db/perf-test))
        (graph-db::*functor* nil))
    (graph-db::add-clause clause)))

(defun pbp-assert-path-clauses ()
  "Assert the two pbp-path clauses once per image.  The guard is
LOOKUP-FUNCTOR on the functor symbol, so a second bench call does not
double the clauses and the recursive case keeps the same shape."
  (let ((sym (find-symbol "PBP-PATH/2" (find-package '#:graph-db/perf-test))))
    (unless (and sym (graph-db::lookup-functor sym))
      (let ((*package* (find-package '#:graph-db/perf-test)))
        (graph-db::<- (pbp-path ?a ?b) (p-knows ?a ?b))
        (graph-db::<- (pbp-path ?a ?c) (p-knows ?a ?b) (pbp-path ?b ?c))))))

(defun pbp-ring (n &key chords)
  "N p-node vertices in the current graph, joined in a ring by p-knows
edges.  With CHORDS, also an edge from every tenth node I to node
\(mod (* 3 I) N).  Returns the vector of ids."
  (let ((ids (insert-p-nodes n :batch 1000)))
    (with-transaction ()
      (dotimes (i n)
        (make-p-knows :from (lookup-vertex (aref ids i))
                      :to (lookup-vertex (aref ids (mod (1+ i) n))))
        (when (and chords (zerop (mod i 10)))
          (make-p-knows :from (lookup-vertex (aref ids i))
                        :to (lookup-vertex (aref ids (mod (* 3 i) n)))))))
    ids))

(defun pbp-bfs (start goal)
  "Breadth-first search in Lisp over OUTGOING-EDGES from vertex START to
vertex GOAL.  Returns the hop count, or NIL when GOAL is unreachable.
The loop shortest_path/4 will wrap (GH #45)."
  (let ((seen (make-hash-table :test 'equal))
        (goal-id (id goal))
        (frontier (list start))
        (hops 0))
    (setf (gethash (id start) seen) t)
    (loop
      (when (some (lambda (v) (equal (id v) goal-id)) frontier)
        (return hops))
      (when (null frontier)
        (return nil))
      (let ((next nil))
        (dolist (v frontier)
          (dolist (e (outgoing-edges v :edge-type 'p-knows))
            (let* ((to (graph-db:to e)) (key (id to)))
              (unless (gethash key seen)
                (setf (gethash key seen) t)
                (push to next)))))
        (setf frontier next)
        (incf hops)))))

(defun pbp-guarded-lookup (g i)
  "One guarded claim lookup of key I, the same shape each call (GH #376)."
  (graph-db.query:run-guarded-prolog
   (format nil "(claim ?c pbp-claim \"pbp-things\" ~S \"rel\" ?ons ?o)"
           (pbp-key i))
   g :limit 10))

(defun pbp-claim-instant (g key)
  "The touched instant (a string) of the claim under KEY."
  (multiple-value-bind (cols rows)
      (graph-db.query:run-guarded-prolog
       (format nil "(claim ?c pbp-claim \"pbp-things\" ~S \"rel\" ?ons ?o) ~
(claim-touched-at ?c ?t)" key)
       g :limit 1 :format :raw)
    (nth (position "t" cols :test #'string-equal) (first rows))))

(defun pbp-ring-closure ()
  "The recursive case from *PBP-START* over the ring (GH #122).  Before
tabling the ring loops to the inference bound: the case records :BOUND
and no solutions.  After tabling it records :COMPLETE and the solution
count.  A bound signal is caught here so the next bench runs."
  (let ((outcome :complete) (solutions 0))
    (timed-seconds ("recursive-ring-closure")
      (handler-case
          (setf solutions
                (length (graph-db:select (:max-inferences 200000) (?x)
                          (lisp ?a *pbp-start*)
                          (pbp-path ?a ?x))))
        (graph-db:prolog-resource-error ()
          (setf outcome :bound))))
    (record "recursive-ring-outcome" :outcome outcome
            :solutions (if (eq outcome :bound) 0 solutions))))

(defun bench-clause-assert ()
  "Unit 2 (GH #121): clause assert cost and bound-first-argument lookup.
The lookup key comes through a (lisp ...) goal, so one compiled query
serves every key and the timed loop holds no compile.  Before #121 the
assert cost grows with the clause count (a full recompile per assert)
and the lookup is a linear scan."
  (let ((n (scale 500)) (k (scale 200)))
    (with-perf-prolog-graph (g)
      (timed-ops ("clause-assert" n)
        (dotimes (i n)
          (pbp-assert (list (list 'pbp-fact (pbp-key i) i)))))
      (timed-ops ("clause-bound-first-arg" k)
        (dotimes (i k)
          (let ((*pbp-probe* (pbp-key (mod (* i 37) n))))
            (graph-db:select-one (?v) (lisp ?k *pbp-probe*)
              (pbp-fact ?k ?v))))))))

(defun bench-guarded-shape-repeat ()
  "Unit 6 (GH #376): repeated guarded claim lookups of one shape, the key
changing each call.  One untimed warm-up call first.  Before #376 each
call compiles the shape afresh."
  (let ((n (scale 2000)) (k (scale 500)))
    (with-perf-prolog-graph (g)
      (pbp-insert-claims n)
      (pbp-guarded-lookup g 0)
      (timed-ops ("guarded-shape-repeat" k)
        (dotimes (i k)
          (pbp-guarded-lookup g (mod i n)))))))

(defun bench-recursive-ring ()
  "Unit 4 (GH #122): a recursive path query around a ring of N nodes.
The ring closes on itself, so an untabled recursion never finishes; the
case records its outcome and returns whether or not the bound is hit."
  (let ((n (scale 200)))
    (pbp-assert-path-clauses)
    (with-perf-graph (g)
      (let* ((ids (pbp-ring n))
             (*pbp-start* (lookup-vertex (aref ids 0))))
        (pbp-ring-closure)))))

(defun bench-change-feed-cursor ()
  "Unit 7 (GH #392): a change-feed cursor over claims touched after an
instant, with 95% of the claims older than that instant.  Before #392
the goal walks every claim; after, a range scan from the cursor."
  (let* ((n (scale 20000)) (split (floor (* n 95) 100)))
    (with-perf-prolog-graph (g)
      (pbp-insert-claims split)
      (let ((instant (pbp-claim-instant g (pbp-key (1- split)))))
        (pbp-insert-claims n :start split)
        (let ((rows nil)
              (text (format nil "(claim ?c pbp-claim \"pbp-things\" ?k ~
\"rel\" ?ons ?o) (claim-touched-at ?c ?t) (instant> ?t ~S)"
                            instant)))
          (let ((graph-db::*query-default-max-inferences* 10000000))
            (timed-seconds ("change-feed-cursor")
              (multiple-value-bind (cols found)
                  (graph-db.query:run-guarded-prolog text g :limit 1000)
                (declare (ignore cols))
                (setf rows found))))
          (record "change-feed-cursor-rows" :rows (length rows)))))))

(defun bench-graph-predicates ()
  "Unit 5 (GH #123): a ring with chords.  A hand-written three-hop select
from varying starts (the query k_hop/4 will replace), and a Lisp
breadth-first search between varying pairs (the loop shortest_path/4
will wrap).  Starts and pairs are looked up before the timers start."
  (let ((n (scale 200)) (k (scale 100)) (kb (scale 100)))
    (with-perf-graph (g)
      (let* ((ids (pbp-ring n :chords t))
             (starts (loop for i below k
                           collect (lookup-vertex (aref ids (mod (* i 13) n)))))
             (pairs (loop for i below kb
                          collect (cons (lookup-vertex
                                         (aref ids (mod (* i 7) n)))
                                        (lookup-vertex
                                         (aref ids
                                               (mod (+ 1 (* i 11)) n)))))))
        (timed-ops ("k-hop-3-select" k)
          (dolist (s starts)
            (let ((*pbp-start* s))
              (graph-db:select-flat (?d) (lisp ?s *pbp-start*)
                (p-knows ?s ?b) (p-knows ?b ?c) (p-knows ?c ?d)))))
        (timed-ops ("shortest-path-lisp-bfs" kb)
          (dolist (p pairs)
            (pbp-bfs (car p) (cdr p))))))))

(defun run-prolog-bench (&key (scale *perf-scale*) (tag "prolog")
                              (output (report-pathname tag))
                              (only nil))
  "Run the five Prolog cases at SCALE and write a report to OUTPUT.  ONLY
names one bench function to run alone, or NIL for all five in order.
Binds what RUN-PERF binds (benchmarks.lisp).  Returns T."
  (let* ((*perf-scale* scale)
         (system-dir (make-temp-directory))
         (graph-db::*system-directory* (namestring system-dir))
         (graph-db::*type-registry* nil)
         (graph-db:*system-clock* nil))
    (reset-perf-report)
    (format t "~&=== graph-db prolog bench (~A, scale ~A) ===~%"
            *lisp-impl* scale)
    (finish-output)
    (unwind-protect
         (progn
           (if only
               (funcall only)
               (progn
                 (bench-clause-assert)
                 (bench-guarded-shape-repeat)
                 (bench-recursive-ring)
                 (bench-change-feed-cursor)
                 (bench-graph-predicates)))
           (write-perf-report output :tag tag))
      (graph-db-test-scratch:cleanup-scratch-run)))
  t)
