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

(test sweep-more-p-does-not-mean-progress
  "The documented loop is (AND MORE-P (PLUSP LINKED)): a window of
permanently unresolvable claims returns LINKED 0 with MORE-P T every
call, because each call re-collects them (GH #372)."
  (with-ee-graph (g)
    (dolist (r '("r1" "r2" "r3"))
      (ee-unlinked-b :subject "ghost" :object "ghost-too" :relation r))
    (destructuring-bind (linked u a s more) (ee-sweep g :limit 2)
      (declare (ignore u a s))
      (is (= 0 linked))
      (is-true more))
    ;; Unchanged store, same window: MORE-P alone would spin for ever.
    (is (equal '(0 4 0 nil t) (ee-sweep g :limit 2)))))

(test sweep-links-a-unary-claim
  "A unary claim has one endpoint; the sweep links it like any other,
and links it once."
  (with-ee-graph (g)
    (let (u s)
      (let ((*link-claims-at-write* nil))
        (with-transaction ()
          (setq u (make-ee-claim-unary :subject-namespace :ee-things
                                       :subject-key "t-1" :relation "u"
                                       :producer "p"
                                       :standing :inferred))))
      (with-transaction () (setq s (ee-thing "t-1")))
      (is (equal '(1 0 0 nil nil) (ee-sweep g)))
      (multiple-value-bind (us uo) (claim-endpoints u)
        (is-true (same-node-p s us))
        (is (null uo)))
      (is (equal '(0 0 0 nil nil) (ee-sweep g))))))

(test sweep-links-a-retracted-claim
  "Spec R3 / sec.4.3: history keeps its edges, so a retracted claim
written key-only is still linked -- CLAIMED/4 reads it, RELATED/3 does
not."
  (with-ee-graph (g)
    (let ((c (ee-unlinked-b)))
      (retract-claim c)
      (with-transaction () (ee-thing "t-1") (ee-thing "t-2"))
      (is (equal '(2 0 0 nil nil) (ee-sweep g)))
      (is-true (ee-linked-to c 'subject-of g))
      (is-true (ee-linked-to c 'object-of g)))))

(test sweep-does-not-prune-a-dead-endpoints-edge
  "Spec sec.5: ACTIVE-EDGE-P hides the edge, COMPACT-EDGES reclaims it,
the sweep leaves it alone and does not re-link a deleted source."
  (with-ee-graph (g)
    (let (c o)
      (with-transaction () (ee-thing "t-1") (setq o (ee-thing "t-2")))
      (with-transaction () (setq c (ee-b)))
      (is-true (ee-linked-to c 'object-of g))
      (with-transaction () (graph-db:mark-deleted o))
      ;; Hidden from the active read, present to the raw one.
      (is (null (ee-linked-to c 'object-of g)))
      (is (= 1 (length (graph-db:outgoing-edges
                        c :graph g :edge-type 'object-of
                        :include-deleted-p t))))
      (is (null (nth-value 1 (claim-endpoints c))))
      ;; The sweep sees a missing object edge, resolves nobody, prunes
      ;; nothing.
      (is (equal '(0 1 0 nil nil) (ee-sweep g)))
      (is (= 1 (length (graph-db:outgoing-edges
                        c :graph g :edge-type 'object-of
                        :include-deleted-p t))))
      ;; Reclaiming is COMPACT-EDGES' job.
      (graph-db::compact-edges g)
      (is (null (graph-db:outgoing-edges c :graph g :edge-type 'object-of
                                            :include-deleted-p t)))
      (is-true (ee-linked-to c 'subject-of g)))))

(test sweep-skips-a-detached-source-store-and-keeps-its-edges
  "Spec sec.5: a namespace whose source store is not open is skipped and
counted; an existing cross-store edge survives."
  (with-ee-graph (g)
    (with-source-graph (sg)
      (let (n c1 c2)
        (with-transaction ((graph-db::transaction-manager sg))
          (setq n (make-st-report :headline "one" :report-id "r-1")))
        (setq n (resolve-endpoint :st-reports "r-1"))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-thing "t-1")
            ;; linked across stores by the caller
            (setq c1 (ee-b :object-namespace :st-reports :object "r-1"
                           :object-node n))
            ;; same endpoint, left for the sweep
            (setq c2 (ee-b :object-namespace :st-reports :object "r-1"
                           :relation "later"))))
        (is-true (ee-linked-to c1 'object-of g))
        (is (null (ee-linked-to c2 'object-of g)))
        ;; Open: the sweep resolves across stores and links C2.
        (is (equal '(1 0 0 nil nil) (ee-sweep g)))
        (is (equalp (id n) (ee-linked-to c2 'object-of g)))
        ;; Detached: a third claim cannot be resolved; the namespace is
        ;; skipped, the two existing edges survive.
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (ee-b :object-namespace :st-reports :object "r-1"
                  :relation "latest")))
        (close-graph sg :snapshot-p nil)
        (is (equal '(0 0 0 (:st-reports) nil) (ee-sweep g)))
        (is-true (ee-linked-to c1 'object-of g))
        (is-true (ee-linked-to c2 'object-of g))))))
