;;;; The linking sweep and the write-time switch (GH #372, spec sec.5,
;;;; sec.11).  Fixtures come from endpoint-edge-tests.lisp, which loads
;;;; first (graph-db.asd): WITH-EE-GRAPH, EE-THING, EE-B, EE-LINKED-TO,
;;;; SAME-NODE-P, MAKE-EE-TWIN; WITH-SOURCE-GRAPH / MAKE-ST-REPORT from
;;;; source-tests.lisp.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test the-write-time-switch-defers-linking
  "Spec sec.9's knob: with *LINK-CLAIMS-AT-WRITE* NIL a claim about
present sources is written key-only, and no edge type is adopted."
  (is-true *link-claims-at-write*)
  (with-ee-graph (g)
    (let (c)
      (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
      (let ((*link-claims-at-write* nil))
        (with-transaction () (setq c (ee-b))))
      (is (null (ee-linked-to c 'subject-of g)))
      (is (null (ee-linked-to c 'object-of g)))
      (is (= 1 (length (claims-touching g 'ee-claim :ee-things "t-1"))))
      ;; The default is untouched outside the binding: a second claim
      ;; links as U1 does.
      (let ((c2 (with-transaction () (ee-b :relation "r2"))))
        (is-true (ee-linked-to c2 'subject-of g))))))

(defun ee-sweep (g &rest keys)
  "LINK-CLAIM-ENDPOINTS on G; the five values as a list."
  (multiple-value-list (apply #'link-claim-endpoints g keys)))

(defun ee-unlinked-b (&rest keys)
  "A binary claim written with linking OFF, inside its own transaction."
  (let ((*link-claims-at-write* nil))
    (with-transaction () (apply #'ee-b keys))))

(test sweep-links-a-claim-written-before-its-source
  "Spec sec.5: the claim came first; the sweep repairs it."
  (with-ee-graph (g)
    (let (c s o)
      (with-transaction () (setq c (ee-b)))
      (is (null (ee-linked-to c 'subject-of g)))
      (with-transaction () (setq s (ee-thing "t-1") o (ee-thing "t-2")))
      (is (equal '(2 0 0 nil nil) (ee-sweep g)))
      (multiple-value-bind (cs co) (claim-endpoints c)
        (is-true (same-node-p s cs))
        (is-true (same-node-p o co))))))

(test sweep-is-idempotent
  (with-ee-graph (g)
    (with-transaction () (ee-b))
    (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
    (is (equal '(2 0 0 nil nil) (ee-sweep g)))
    (is (equal '(0 0 0 nil nil) (ee-sweep g)))
    ;; A claim linked at write is not touched either.
    (with-transaction () (ee-b :relation "r2"))
    (is (equal '(0 0 0 nil nil) (ee-sweep g)))))

(test sweep-counts-every-category-and-never-signals
  "Spec sec.5 / sec.7: unknown namespace -> skipped for the call; no
such key -> unresolved; two candidates -> ambiguous; the claim stays
as it was in every case."
  (with-ee-graph (g)
    (with-transaction () (ee-thing "t-1"))
    (with-transaction ()
      (ee-thing "dup")
      (make-ee-twin :twin-id "dup"))
    (let (k1 k2 k3)
      ;; object in an unregistered namespace (subject t-1 links)
      (with-transaction ()
        (setq k1 (ee-b :object-namespace :ee-nowhere :object "x")))
      ;; object key nobody has
      (with-transaction () (setq k2 (ee-b :object "t-missing")))
      ;; subject with two candidates, written unlinked (warning muffled)
      (handler-bind ((endpoint-link-skipped #'muffle-warning))
        (with-transaction () (setq k3 (ee-b :subject "dup" :object "t-1"))))
      (destructuring-bind (linked unresolved ambiguous skipped more)
          (ee-sweep g)
        (is (= 0 linked))
        (is (= 1 unresolved))
        (is (= 1 ambiguous))
        (is (equal '(:ee-nowhere) skipped))
        (is (null more)))
      (is (null (ee-linked-to k1 'object-of g)))
      (is (null (ee-linked-to k2 'object-of g)))
      (is (null (ee-linked-to k3 'subject-of g)))
      (is-true (ee-linked-to k3 'object-of g))
      ;; Idempotent on the same failures.
      (is (equal '(0 1 1 (:ee-nowhere) nil) (ee-sweep g))))))

(test sweep-honours-since
  "Spec sec.5: :SINCE is a commit epoch; only claims at or above it are
visited, so an operator can sweep exactly what a regeneration wrote."
  (with-ee-graph (g)
    (let (a b)
      (setq a (ee-unlinked-b :relation "a"))
      (setq b (ee-unlinked-b :relation "b"))
      (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
      (let ((eb (claim-commit-epoch b)))
        (is-true (> eb (claim-commit-epoch a)))
        (is (equal '(2 0 0 nil nil) (ee-sweep g :since eb)))
        (is-true (ee-linked-to b 'subject-of g))
        (is (null (ee-linked-to a 'subject-of g)))
        (is (equal '(2 0 0 nil nil) (ee-sweep g)))
        (is-true (ee-linked-to a 'subject-of g))))))

(test sweep-family-restricts-and-an-absent-family-is-nothing
  (with-ee-graph (g)
    (ee-unlinked-b)
    (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
    ;; CT-CLAIM is registered (claim-tests.lisp) but not in this store.
    (is (equal '(0 0 0 nil nil) (ee-sweep g :family 'ct-claim)))
    (is (equal '(2 0 0 nil nil) (ee-sweep g :family 'ee-claim)))
    (signals unknown-claim-family (link-claim-endpoints g :family 'nope))))

(test sweep-limit-bounds-the-work-and-reports-more
  "LIMIT bounds claims with a missing edge examined per call; MORE-P
says whether further such claims remained."
  (with-ee-graph (g)
    (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
    (dolist (r '("r1" "r2" "r3")) (ee-unlinked-b :relation r))
    (destructuring-bind (linked u a s more) (ee-sweep g :limit 2)
      (declare (ignore u a s))
      (is (= 4 linked))
      (is-true more))
    (destructuring-bind (linked u a s more) (ee-sweep g :limit 2)
      (declare (ignore u a s))
      (is (= 2 linked))
      (is (null more)))))
