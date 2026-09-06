;;;; Tests for the embedded Prolog query language (prologc.lisp,
;;;; prolog-functors.lisp): select / select-flat / select-one over a graph,
;;;; using the is-a/2 built-in and the generated edge functors.
;;;;
;;;; Reuses the schema (g-person, g-knows) from graph-tests.lisp.  Queries run
;;;; against the dynamically-bound *graph* from WITH-TEST-GRAPH.

(in-package #:graph-db/test)

(def-suite query-suite
  :description "Prolog select queries over a graph."
  :in graph-db-suite)

(in-suite query-suite)

(test select-flat-is-a
  "select-flat with is-a returns every vertex of a type."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-person :name "C"))
    (let ((people (select-flat (?p) (is-a ?p g-person))))
      (is (= 3 (length people)))
      (is (every (lambda (p) (typep p 'g-person)) people)))))

(test select-one-returns-single
  (with-test-graph (g)
    (with-transaction () (make-g-person :name "Solo"))
    (let ((p (select-one (?p) (is-a ?p g-person))))
      (is-true (typep p 'g-person))
      (is (string= "Solo" (slot-value p 'name))))))

(test select-no-matches-is-nil
  (with-test-graph (g)
    (is (null (select-flat (?p) (is-a ?p g-person))))))

(test select-edge-functor-pairs
  "The generated <edge>/2 functor yields connected (from to) vertex pairs."
  (with-test-graph (g)
    (let (aid bid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B")))
          (setq aid (id a) bid (id b))
          (make-g-knows :from a :to b)))
      (let ((pairs (select (:flat nil) (?a ?b) (g-knows ?a ?b))))
        (is (= 1 (length pairs)))
        (destructuring-bind (a b) (first pairs)
          (is (equalp aid (id a)))
          (is (equalp bid (id b))))))))

(test select-callback-streams-each-row
  "select :callback hands each row to the function as produced (consing no result
list) and returns the row count; it honors :limit."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A") (make-g-person :name "B") (make-g-person :name "C"))
    (let ((rows '()))
      (let ((n (select (:callback (lambda (row) (push row rows))) (?n)
                       (is-a ?p g-person) (node-slot-value ?p name ?n))))
        (is (= 3 n) "returns the number of rows streamed")
        (is (= 3 (length rows)))
        ;; each streamed row is a one-tuple of the name
        (is (equal '("A" "B" "C")
                   (sort (mapcar #'first rows) #'string<)))))
    ;; :limit caps the stream
    (let ((count 0))
      (select (:callback (lambda (row) (declare (ignore row)) (incf count)) :limit 2)
              (?n) (is-a ?p g-person) (node-slot-value ?p name ?n))
      (is (= 2 count) ":limit bounds the streamed rows"))))

(test select-count-returns-solution-count
  "select-count returns the integer number of solutions (consing no bindings);
0 when there are none, and it composes with query constraints."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-person :name "C"))
    (is (= 3 (select-count () (is-a ?p g-person))))
    (is (= 3 (select-count (?p) (is-a ?p g-person))))
    (is (zerop (select-count (?p) (is-a ?p g-person)
                             (node-slot-value ?p name "nobody"))))))

(test select-count-honors-limit-and-skip
  "The SELECT (:count t ...) form honors :limit (a capped count) and :skip
(count past an offset), matching the rows SELECT would return."
  (with-test-graph (g)
    (with-transaction ()
      (dotimes (i 10) (make-g-person :name (format nil "n~d" i))))
    (is (= 10 (select-count (?p) (is-a ?p g-person))))
    (is (= 4 (select (:count t :limit 4) (?p) (is-a ?p g-person))))
    ;; 10 total, skip 7 -> 3 remain
    (is (= 3 (select (:count t :skip 7) (?p) (is-a ?p g-person))))))

(test select-join-multiple-goals
  "A multi-goal query joins is-a and the edge functor: all (person, known)
pairs.  With A->B and A->C, both pairs are sourced from A."
  (with-test-graph (g)
    (let (aid bid cid)
      (with-transaction ()
        (let ((a (make-g-person :name "A"))
              (b (make-g-person :name "B"))
              (c (make-g-person :name "C")))
          (setq aid (id a) bid (id b) cid (id c))
          (make-g-knows :from a :to b)
          (make-g-knows :from a :to c)))
      (let ((pairs (select (:flat nil) (?a ?b)
                           (is-a ?a g-person)
                           (g-knows ?a ?b))))
        (is (= 2 (length pairs)))
        (is (every (lambda (pair) (equalp aid (id (first pair)))) pairs))
        (let ((targets (mapcar (lambda (pair) (id (second pair))) pairs)))
          (is-true (member bid targets :test #'equalp))
          (is-true (member cid targets :test #'equalp)))))))

(test node-slot-value-with-an-unbound-slot-lists-the-slots
  "GH #351: (node-slot-value ?p ?s ?v) yields one row per data slot of
?p's type, ?s a keyword, ?v the value or NIL; an inherited slot counts."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-employee :name "E" :title "boss")
      (make-g-person :name "F" :age :fail))
    (let ((rows (select (:flat nil) (?s ?v)
                        (is-a ?p g-employee) (node-slot-value ?p ?s ?v))))
      (is (equal '((:age nil) (:name "E") (:title "boss"))
                 (sort (copy-list rows) #'string< :key #'first))))
    ;; :FAIL is now a legitimate slot value: the read-error sentinel is
    ;; two values, not the keyword :FAIL (GH #351).
    (is (equal '(:fail)
               (select-flat (?v) (is-a ?p g-person)
                            (node-slot-value ?p name "F")
                            (node-slot-value ?p age ?v))))
    (is (equal '((:age :fail) (:name "F"))
               (sort (select (:flat nil) (?s ?v)
                             (is-a ?p g-person) (node-slot-value ?p name "F")
                             (node-slot-value ?p ?s ?v))
                     #'string< :key #'first))
        "a :fail-valued slot enumerates through the unbound-slot path")))

(test node-slot-value-with-an-unbound-node-finds-vertices-by-slot
  "GH #351: no IS-A needed to look a vertex up by a slot value, and a
vertex whose type lacks the slot is skipped rather than read as NIL."
  (with-test-graph (g)
    (with-transaction ()
      (make-g-person :name "A")
      (make-g-person :name "B")
      (make-g-employee :name "C" :title "boss"))
    (let ((found (select-flat (?p) (node-slot-value ?p name "B"))))
      (is (= 1 (length found)))
      (is (string= "B" (slot-value (first found) 'name))))
    ;; TITLE is declared on g-employee only: two persons are skipped.
    (is (equal '("boss")
               (select-flat (?t) (node-slot-value ?p title ?t))))
    ;; Both unbound: every vertex, every slot.  3 vertices x 2 slots
    ;; (name, age) + 1 x title = 7 rows.
    (is (= 7 (select-count () (node-slot-value ?p ?s ?v))))))
