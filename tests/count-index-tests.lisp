;;;; tests/count-index-tests.lisp -- the counting index (GH #361).

(in-package #:graph-db/test)

(def-suite count-index-suite
  :description "Counting index (def-count-index): per-prefix counters."
  :in graph-db-suite)

(in-suite count-index-suite)

(defun ix-live-p (node)
  "The CURRENT-P of the test count index: a claim whose REL is \"dead\"
is not current."
  (not (equal (ix-rel node) "dead")))

;; Two count indexes on IX-CLAIM, which already carries two secondary
;; indexes on (ns key rel) and (ns key): the two kinds coexist on one
;; owner and one slot list (facts X1).
(def-count-index ix-claim (ns key) :graph-db-index-test
  :name ix-count-ns-key :current-p ix-live-p)
(def-count-index ix-claim (rel) :graph-db-index-test :name ix-count-rel)

;; Declared and deliberately never built: nothing builds a count map at
;; MAKE-GRAPH until Task 4, so this one pins spec §2.5's
;; declared-but-unbuilt branch (facts G12).  Never pass (ns) to %CIX.
(def-count-index ix-claim (ns) :graph-db-index-test :name ix-count-unbuilt)

(defun %cix (g slots)
  "The built COUNT-INDEX for IX-CLAIM.SLOTS in G.  Nothing installs a
count map at open until Task 4, so build it from the declaration first;
the build is get-or-create, so this stays right once open does it."
  (graph-db::%ensure-count-index-built
   g (graph-db::%count-spec-for 'ix-claim slots g))
  (graph-db::%require-count-index g 'ix-claim slots))

(defun %count-of (g slots tuple)
  "(ALL CURRENT) for TUPLE, as a list."
  (multiple-value-list (count-index-lookup g 'ix-claim slots tuple)))

(defun %entries (g slots &key (depth 1) prefix)
  "MAP-COUNT-INDEX's calls as a list of (COMPONENTS ALL CURRENT)."
  (let ((out '()))
    (map-count-index (lambda (c a n) (push (list c a n) out))
                     g 'ix-claim slots :depth depth :prefix prefix)
    (nreverse out)))

(test count-node-counts-every-prefix
  "Spec §2.2-2.3: %COUNT-NODE adds a node's contribution at every
leading prefix of its tuple; a duplicate tuple is a second contribution;
CURRENT follows CURRENT-P; an index without CURRENT-P keeps ALL only."
  (with-ix-graph (g)
    (let (nodes)
      (with-transaction ()
        (setq nodes (list (make-ix-claim :ns "ops" :key "e1" :rel "at")
                          (make-ix-claim :ns "ops" :key "e1" :rel "dead")
                          (make-ix-claim :ns "ops" :key "e2" :rel "at")
                          (make-ix-claim :ns "hr" :key "p1" :rel "at"))))
      (let ((ck (%cix g '(ns key))) (cr (%cix g '(rel))))
        (dolist (n nodes)
          (graph-db::%count-node ck n 1 (graph-db::%current-p ck n))
          (graph-db::%count-node cr n 1 (graph-db::%current-p cr n))))
      (is (equal '(3 2) (%count-of g '(ns key) '("ops"))))
      (is (equal '(2 1) (%count-of g '(ns key) '("ops" "e1"))))
      (is (equal '(1 1) (%count-of g '(ns key) '("hr"))))
      (is (equal '(0 0) (%count-of g '(ns key) '("none"))) "absent: 0 0")
      (is (equal '(3 nil) (%count-of g '(rel) "at"))
          "no CURRENT-P: ALL only, CURRENT is NIL")
      (is (equal '((("hr") 1 1) (("ops") 3 2)) (%entries g '(ns key)))
          "depth 1, index order (hr < ops)")
      (is (equal '((("ops" "e1") 2 1) (("ops" "e2") 1 1))
                 (%entries g '(ns key) :depth 2 :prefix '("ops"))))
      (is (equal '((("at") 3 nil) (("dead") 1 nil)) (%entries g '(rel)))))))

(test count-adjust-removes-a-key-at-zero-and-moves-current
  "Spec R4, §2.3: subtracting a node's contribution removes the key when
ALL reaches 0 and leaves a sibling; a current-only step moves CURRENT."
  (with-ix-graph (g)
    (let (a b)
      (with-transaction ()
        (setq a (make-ix-claim :ns "ops" :key "e1" :rel "at")
              b (make-ix-claim :ns "ops" :key "e2" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck a 1 1)
        (graph-db::%count-node ck b 1 1)
        (graph-db::%count-node ck a -1 -1)
        (is (equal '(0 0) (%count-of g '(ns key) '("ops" "e1"))))
        (is (equal '(("ops" "e2")) (mapcar #'first
                                           (%entries g '(ns key) :depth 2
                                                     :prefix '("ops"))))
            "the emptied key is gone, not zero")
        (is (equal '(1 1) (%count-of g '(ns key) '("ops"))))
        (graph-db::%count-adjust ck '("ops") 0 -1)
        (is (equal '(1 0) (%count-of g '(ns key) '("ops")))
            "a predicate flip moves CURRENT and leaves ALL")))))

(test count-index-null-component-and-refusals
  "Spec §2.2, §2.5: a null component is stored and read back as NIL and
sorts first; an all-null full tuple answers 0 0; an undeclared index
signals; a depth or prefix beyond the arity signals."
  (with-ix-graph (g)
    (let (n)
      (with-transaction ()
        (setq n (make-ix-claim :ns "ops" :key nil :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck n 1 1))
      (is (equal '(1 1) (%count-of g '(ns key) '("ops" nil))))
      (is (equal '((("ops" nil) 1 1))
                 (%entries g '(ns key) :depth 2 :prefix '("ops"))))
      (is (equal '(0 0) (%count-of g '(ns key) '(nil nil))))
      (signals query-precondition-error
        (count-index-lookup g 'ix-claim '(key) "x"))
      (signals query-precondition-error
        (%entries g '(ns key) :depth 3))
      (signals query-precondition-error
        (%entries g '(ns key) :depth 1 :prefix '("a" "b"))))))

(test count-index-arity-1-takes-a-list-valued-component
  "%INDEX-KEY's arity rule (index.lisp), shared by %COUNT-QUERY-KEY: at
arity 1 the query VALUE is the one component as-is, so a list-valued
slot is findable by its list.  Read as a component list instead, this
lookup would signal on the arity."
  (with-ix-graph (g)
    (unwind-protect
         (progn
           (def-count-index ix-claim (key) :graph-db-index-test
             :name ix-count-key)
           (let (n)
             (with-transaction ()
               (setq n (make-ix-claim :ns "ops" :key '("a" "b")
                                      :rel "at")))
             (graph-db::%count-node (%cix g '(key)) n 1 0))
           (is (equal '(1 nil) (%count-of g '(key) '("a" "b")))
               "the list IS the component, not a two-component tuple")
           ;; The entry's COMPONENTS is a 1-list whose one element is
           ;; the whole slot value.
           (is (equal '(((("a" "b")) 1 nil)) (%entries g '(key)))))
      (undef-count-index ix-claim :graph-db-index-test
                         :name ix-count-key))))

(test declared-but-unbuilt-count-index-answers-empty
  "Spec §2.5, facts G12: a count index that is declared but has no built
map answers 0 0 and calls FN zero times rather than signalling -- the
normal state of every count index until Task 4 installs at open, and of
a lazy memory graph forever.  IX-COUNT-UNBUILT is never given to %CIX."
  (with-ix-graph (g)
    (is (null (graph-db::%require-count-index g 'ix-claim '(ns)))
        "declared but unbuilt resolves to NIL, it does not signal")
    (is (equal '(0 0) (%count-of g '(ns) "x")))
    (is (null (%entries g '(ns))) "MAP-COUNT-INDEX calls FN zero times")))

(test count-adjust-crosses-a-serialization-boundary
  "Facts G5: SERIALIZE's integer width grows at 256 and at 65536; the
counter keeps counting across the first boundary on the heap backend
(in place with OLD-VALUE, or remove+add) and reads back right."
  (with-ix-graph (g)
    (let (n)
      (with-transaction ()
        (setq n (make-ix-claim :ns "big" :key "k" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (dotimes (i 300) (graph-db::%count-node ck n 1 1)))
      (is (equal '(300 300) (%count-of g '(ns key) '("big" "k")))))))

(test count-index-on-the-memory-and-bplus-backends
  "Facts X2: the memory backend's UPDATE-IN-SKIP-LIST does not insert, so
the FIRST increment of a name must go through ADD-TO-SKIP-LIST; the B+
tree backend round-trips the counter pair too."
  (with-ix-memory-graph (g)
    (let (n)
      (with-transaction () (setq n (make-ix-claim :ns "m" :key "k" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck n 1 1)
        (graph-db::%count-node ck n 1 1))
      (is (equal '(2 2) (%count-of g '(ns key) '("m"))))))
  (with-ix-graph (g :backend :bplus-tree)
    (let (n)
      (with-transaction () (setq n (make-ix-claim :ns "b" :key "k" :rel "at")))
      (let ((ck (%cix g '(ns key))))
        (graph-db::%count-node ck n 1 1)
        (graph-db::%count-node ck n 1 0))
      (is (equal '(2 1) (%count-of g '(ns key) '("b" "k")))))))

(test def-count-index-registers-by-name-and-undef-withdraws
  "Spec §2.1: a named declaration replaces in place; UNDEF-COUNT-INDEX
withdraws it (and warns when nothing matched, GH #152); the built map
of a withdrawn declaration is reclaimed at the next open (Task 4)."
  (let ((before (length (gethash :graph-db-index-test
                                 graph-db::*schema-count-metadata*))))
    (def-count-index ix-claim (ns) :graph-db-index-test :name ix-count-tmp)
    (def-count-index ix-claim (ns) :graph-db-index-test :name ix-count-tmp)
    (is (= (1+ before) (length (gethash :graph-db-index-test
                                        graph-db::*schema-count-metadata*)))
        "re-declaring by name replaces, not pushes")
    (is (eq t (undef-count-index ix-claim :graph-db-index-test
                                 :name ix-count-tmp)))
    (is (= before (length (gethash :graph-db-index-test
                                   graph-db::*schema-count-metadata*))))
    (signals schema-withdrawal-matched-nothing
      (undef-count-index ix-claim :graph-db-index-test :name ix-count-tmp))))
