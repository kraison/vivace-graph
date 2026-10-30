;;;; tests/read-your-own-writes-tests.lisp -- a transaction sees its own
;;;; writes (GH #408).  Spec: docs/superpowers/specs/
;;;; 2026-10-29-read-your-own-writes-design.md.

(in-package #:graph-db/test)

(def-suite read-your-own-writes-suite
  :description "GH #408: reads inside a transaction see its own writes."
  :in graph-db-suite)

(in-suite read-your-own-writes-suite)

(defparameter *ryow-graph-name* :graph-db-ryow-test)

(def-vertex ryow-node ()
  ((k :type string :accessor ryow-node-k)
   (tag :type string :accessor ryow-node-tag :index t))
  :graph-db-ryow-test
  ;; The pre-image tests read two versions back; the reaper would
  ;; otherwise reclaim the older one at the next commit.
  :keep-revisions 2)

(def-edge ryow-link () () :graph-db-ryow-test)

(def-index ryow-node (k tag) :graph-db-ryow-test)

(defmacro with-ryow-graph ((g) &body body)
  "A fresh on-disk graph with *GRAPH* bound to it, closed on exit."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ryow-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

(defmacro ryow-both-caches (() &body body)
  "Run BODY with the shared node cache on, then off: the cache hides
identity bugs (CLAUDE.md, Tests).  BODY makes its own graph."
  `(dolist (on '(t nil))
     (let ((graph-db::*cache-enabled* on))
       ,@body)))

;;; --- Task 1: the overlay core and the switch -----------------------------

(test ryow-overlay-effective-write-last-wins
  "Create then delete of one id leaves a TX-DELETE and no live node;
update then update leaves the later copy (spec §4.3)."
  (with-ryow-graph (g)
    (with-transaction ()
      (let ((v (make-ryow-node :k "a" :tag "t")))
        (mark-deleted v)
        (let ((ov (graph-db::overlay-of graph-db::*transaction*)))
          (is (typep (graph-db::overlay-effective-write ov (id v))
                     'graph-db::tx-delete))
          (is (null (graph-db::overlay-live-nodes ov))))))
    (let (a)
      (with-transaction () (setf a (make-ryow-node :k "v1" :tag "t")))
      (with-transaction ()
        (let ((c1 (copy a))) (setf (ryow-node-k c1) "v2") (save c1))
        (let ((c2 (copy a))) (setf (ryow-node-k c2) "v3") (save c2))
        (let* ((ov (graph-db::overlay-of graph-db::*transaction*))
               (w (graph-db::overlay-effective-write ov (id a))))
          (is (equal "v3" (ryow-node-k (graph-db::node w))))
          (is (= 1 (length (graph-db::overlay-live-nodes ov)))))))))

(test ryow-switch-off-under-commit-validators
  "*READ-OWN-WRITES* is NIL inside %COMMIT's validators and inside
VALIDATE-WRITES, T in the transaction body (spec §4.6)."
  (with-ryow-graph (g)
    (let* ((seen :unset)
           (graph-db::*commit-validators*
             (list (lambda (tx graph) (declare (ignore tx graph))
                     (setf seen graph-db::*read-own-writes*)))))
      (with-transaction ()
        (is (eq t graph-db::*read-own-writes*))
        (make-ryow-node :k "a" :tag "t"))
      (is (null seen))
      (setf seen :unset)
      (with-transaction ()
        (make-ryow-node :k "b" :tag "t")
        (graph-db:validate-transaction g)
        (is (null seen))
        (setf seen :unset))
      (is (null seen)))))

;;; --- Task 2: layer 1, own writes in the local cache ----------------------

(test ryow-lookup-sees-own-create
  (ryow-both-caches ()
    (with-ryow-graph (g)
      (with-transaction ()
        (let* ((v (make-ryow-node :k "a" :tag "t"))
               (e (make-ryow-link :from v :to v)))
          (is (eq v (lookup-vertex (id v))))
          (is (eq v (lookup-ryow-node (id v))))
          (is (eq e (lookup-edge (id e))))
          (is (eq e (lookup-ryow-link (id e)))))))))

(test ryow-lookup-sees-own-save-and-delete
  (ryow-both-caches ()
    (with-ryow-graph (g)
      (let (a e)
        (with-transaction ()
          (setf a (make-ryow-node :k "a" :tag "t")
                e (make-ryow-link :from a :to a)))
        (with-transaction ()
          (let ((c (copy (lookup-vertex (id a)))))
            (setf (ryow-node-k c) "a2")
            (save c)
            (is (equal "a2" (ryow-node-k (lookup-vertex (id a))))))
          (mark-deleted (lookup-edge (id e)))
          (is (deleted-p (lookup-edge (id e))))
          (is (null (lookup-ryow-link (id e)))))))))

(test ryow-rollback-and-nesting
  "A non-local exit leaves nothing; an inner WITH-TRANSACTION (an
independent commit, #401) does not see the outer's writes."
  (with-ryow-graph (g)
    (let (id)
      (ignore-errors
       (with-transaction ()
         (setf id (id (make-ryow-node :k "x" :tag "t")))
         (error "abort")))
      (with-transaction () (is (null (lookup-vertex id))))
      (with-transaction ()
        (let ((v (make-ryow-node :k "o" :tag "t")))
          (with-transaction ()
            (is (null (lookup-vertex (id v))))))))))

(test ryow-switch-off-reads-committed
  "With *READ-OWN-WRITES* NIL a lookup passes over an own write
(spec §4.6)."
  (with-ryow-graph (g)
    (let (a)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
      (with-transaction ()
        (let ((c (copy (lookup-vertex (id a)))))
          (setf (ryow-node-k c) "a2") (save c)
          (let ((graph-db::*read-own-writes* nil))
            (is (equal "a" (ryow-node-k (lookup-vertex (id a))))))
          (is (equal "a2" (ryow-node-k (lookup-vertex (id a))))))))))

;;; --- Task 3: copy and delete of an own write chain to the committed
;;; pre-image (spec §4.4) -----------------------------------------------

(defun ryow-epoch (g)
  "The last committed epoch of G, for an :AS-OF read of it."
  (graph-db:latest-epoch g))

(test ryow-copy-of-own-update-chains-to-committed
  "The archived pre-image of a save-of-a-copy-of-an-own-update is the
committed version."
  ;; Two committed versions first: an archived UNCOMMITTED copy still
  ;; points at the committed data block, so only the version behind it
  ;; shows the cut chain.
  (with-ryow-graph (g)
    (let (a e-v0 e0)
      (with-transaction () (setf a (make-ryow-node :k "v0" :tag "t")))
      (setf e-v0 (ryow-epoch g))
      (with-transaction ()
        (let ((c (copy a))) (setf (ryow-node-k c) "v1") (save c)))
      (setf a (lookup-vertex (id a))      ; the live head, not the stale
            e0 (ryow-epoch g))            ; first object
      (with-transaction ()
        (let ((c1 (copy a))) (setf (ryow-node-k c1) "v2") (save c1))
        (let ((c2 (copy (lookup-vertex (id a)))))
          (setf (ryow-node-k c2) "v3") (save c2)))
      (is (equal "v3" (ryow-node-k (lookup-vertex (id a)))))
      (is (equal "v1" (ryow-node-k (lookup-vertex (id a) :as-of e0))))
      (is (equal "v0" (ryow-node-k (lookup-vertex (id a) :as-of e-v0)))))))

(test ryow-copy-of-own-create-still-refused
  (with-ryow-graph (g)
    (with-transaction ()
      (let ((v (make-ryow-node :k "a" :tag "t")))
        (signals graph-db::copying-uncommitted-node
          (copy (lookup-vertex (id v))))))))

(test ryow-save-then-delete-pre-image
  "Save then MARK-DELETED of the looked-up copy: the committed index
entry under the old key is released and the chain is intact (review
focus 2)."
  (with-ryow-graph (g)
    (let (a e0)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "old")))
      (setf e0 (ryow-epoch g))
      (with-transaction ()
        (let ((c (copy a)))
          (setf (ryow-node-tag c) "new" (ryow-node-k c) "a2")
          (save c))
        (mark-deleted (lookup-vertex (id a))))
      ;; Entries, not live nodes: the deleted filter would hide an orphan.
      (is (= 0 (index-count g 'ryow-node 'tag "old")))
      (is (= 0 (index-count g 'ryow-node 'tag "new")))
      (is (equal "a" (ryow-node-k (lookup-vertex (id a) :as-of e0)))))))

(test ryow-copy-of-own-delete-undeletes
  (with-ryow-graph (g)
    (let (a)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
      (with-transaction ()
        (mark-deleted a)
        (let ((c (copy (lookup-vertex (id a)))))
          (setf (deleted-p c) nil) (save c)))
      (is (lookup-ryow-node (id a))))))

;;; --- Task 4: typed scans (spec §4.3) --------------------------------------

(test ryow-typed-scan-sees-create-once-and-update-once
  (ryow-both-caches ()
    (with-ryow-graph (g)
      (let (a)
        (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
        (with-transaction ()
          (let ((c (copy a))) (setf (ryow-node-k c) "a2") (save c))
          (make-ryow-node :k "b" :tag "t")
          (let ((ks (map-vertices #'ryow-node-k g :vertex-type 'ryow-node
                                  :collect-p t)))
            (is (equal '("a2" "b") (sort ks #'string<)))))))))

(test ryow-typed-edge-scan-and-create-then-delete-invisible
  "Review focus 1: created-then-deleted appears nowhere live, and
commits only a tombstone."
  (with-ryow-graph (g)
    (flet ((links (&rest args)
             (length (apply #'map-edges #'identity g :edge-type 'ryow-link
                            :collect-p t args))))
      (let (a e0)
        (with-transaction ()
          (setf a (make-ryow-node :k "a" :tag "t")
                e0 (make-ryow-link :from a :to a)))
        (with-transaction ()
          (let ((e (make-ryow-link :from a :to a)))
            (is (= 2 (links)))
            (mark-deleted e)
            (is (= 1 (links)))
            ;; E0 is committed: the walk must skip it (it is an own
            ;; write now) and the overlay supply it once.
            (mark-deleted e0)
            (is (= 0 (links)))
            (is (= 2 (links :include-deleted-p t)))
            (is (= 1 (length (map-vertices #'identity g
                                           :vertex-type 'ryow-node
                                           :collect-p t))))))
        (is (= 0 (links)))))))

;;; --- Task 5: adjacency and EDGE-EXISTS-P (spec §4.3) ----------------------

(test ryow-issue-408-drop-then-make-twice
  "The issue's repro: replace-the-edge twice in one transaction leaves
one edge."
  (ryow-both-caches ()
    (with-ryow-graph (g)
      (let (a b)
        (with-transaction ()
          (setf a (make-ryow-node :k "a" :tag "t")
                b (make-ryow-node :k "b" :tag "t")))
        (with-transaction ()
          (dotimes (i 2)
            (dolist (e (map-edges #'identity g :vertex a :direction :out
                                  :edge-type 'ryow-link :collect-p t))
              (mark-deleted e))
            (make-ryow-link :from a :to b)))
        (is (= 1 (length (outgoing-edges a :edge-type 'ryow-link))))
        (is (= 1 (length (incoming-edges b :edge-type 'ryow-link))))))))

(test ryow-adjacency-in-out-between-and-exists
  (with-ryow-graph (g)
    (let (a b e0)
      (with-transaction ()
        (setf a (make-ryow-node :k "a" :tag "t")
              b (make-ryow-node :k "b" :tag "t")
              e0 (make-ryow-link :from b :to a)))
      (with-transaction ()
        (is (null (edge-exists-p 'ryow-link a b)))
        (let ((e (make-ryow-link :from a :to b)))
          (is (eq e (edge-exists-p 'ryow-link a b)))
          (is (= 1 (length (incoming-edges b :edge-type 'ryow-link))))
          (is (= 1 (length (map-edges #'identity g :from-vertex a
                                      :to-vertex b :edge-type 'ryow-link
                                      :collect-p t))))
          ;; E0 is committed: the walks must skip it once deleted here,
          ;; and supply it once under :INCLUDE-DELETED-P.
          (mark-deleted e0)
          (is (= 0 (length (incoming-edges a :edge-type 'ryow-link))))
          (is (= 1 (length (incoming-edges a :edge-type 'ryow-link
                                           :include-deleted-p t))))
          (is (null (edge-exists-p 'ryow-link b a)))
          (mark-deleted e)
          (is (null (edge-exists-p 'ryow-link a b))))))))

(test ryow-cardinality-counts-once
  "Review focus 4: a :MAX 1 vertex gains one edge and commits; the
validator does not see it twice (spec §4.6)."
  (with-ryow-graph (g)
    (let (a b)
      (with-transaction ()
        (setf a (make-ryow-node :k "a" :tag "t")
              b (make-ryow-node :k "b" :tag "t")))
      ;; The declaration is keyed by graph NAME in a global registry, so
      ;; it must not outlive this test (review finding 3).
      (def-cardinality ryow-node ryow-link :graph-db-ryow-test :max 1)
      (unwind-protect
           (progn
             (finishes
               (with-transaction ()
                 (make-ryow-link :from a :to b)
                 (is (= 1 (length (outgoing-edges a :edge-type 'ryow-link))))
                 (is (null (graph-db::validation-report-violations
                            (graph-db:validate-transaction g))))))
             (signals graph-db::cardinality-violation
               (with-transaction () (make-ryow-link :from a :to b))))
        (undef-cardinality ryow-node :graph-db-ryow-test
                           :edge-type ryow-link))
      ;; Withdrawn: a second link commits.
      (finishes (with-transaction () (make-ryow-link :from a :to b))))))

;;; --- Task 6: secondary-index reads (spec §4.3) ----------------------------

(test ryow-index-lookup-sees-create-and-moved-key
  (ryow-both-caches ()
    (with-ryow-graph (g)
      (let (a)
        (with-transaction () (setf a (make-ryow-node :k "a" :tag "old")))
        (with-transaction ()
          (make-ryow-node :k "b" :tag "new")
          (let ((c (copy a))) (setf (ryow-node-tag c) "new") (save c))
          (is (null (index-lookup g 'ryow-node 'tag "old")))
          (is (equal '("a" "b")
                     (sort (mapcar #'ryow-node-k
                                   (index-lookup g 'ryow-node 'tag "new"))
                           #'string<)))
          (is (eq t (index-lookup g 'ryow-node 'tag "new" :collect-p nil)))
          (is (= 1 (length (index-lookup g 'ryow-node '(k tag) '("b")
                                         :prefix t)))))))))

(test ryow-map-index-order-and-range-with-own-writes
  (with-ryow-graph (g)
    (flet ((tags (start end)
             (mapcar #'ryow-node-tag
                     (index-range g 'ryow-node 'tag :start start :end end))))
      (with-transaction ()
        (make-ryow-node :k "1" :tag "b") (make-ryow-node :k "2" :tag "d"))
      (with-transaction ()
        (make-ryow-node :k "3" :tag "a") (make-ryow-node :k "4" :tag "c")
        (make-ryow-node :k "5" :tag "e")
        (is (equal '("a" "b" "c" "d" "e") (tags "a" "e")))
        (is (equal '("b" "c" "d") (tags "b" "d")))
        ;; A committed entry whose node this transaction deleted drops out
        ;; of the walk; a moved key is merged at its new position.
        (mark-deleted (first (index-lookup g 'ryow-node 'tag "d")))
        (let ((c (copy (first (index-lookup g 'ryow-node 'tag "b")))))
          (setf (ryow-node-tag c) "f") (save c))
        (is (equal '("a" "c" "e" "f") (tags "a" "f")))))))

(test ryow-composite-range-with-short-bounds-keeps-own-rows
  "Review finding 1: on a multi-slot index with both bounds given, a
short END includes every committed extension of it (IX-MAP's bounded
cursor pads it with the max sentinels), so the overlay must include the
transaction's own extensions too."
  (with-ryow-graph (g)
    (flet ((rows (start end)
             (mapcar (lambda (n) (list (ryow-node-k n) (ryow-node-tag n)))
                     (index-range g 'ryow-node '(k tag)
                                  :start start :end end))))
      (with-transaction ()
        (make-ryow-node :k "1" :tag "a") (make-ryow-node :k "4" :tag "z"))
      (with-transaction ()
        (make-ryow-node :k "2" :tag "m") (make-ryow-node :k "4" :tag "x")
        (is (equal '(("1" "a") ("2" "m") ("4" "x") ("4" "z"))
                   (rows '("1") '("4"))))
        (is (equal '(("2" "m") ("4" "x") ("4" "z"))
                   (rows '("2") '("4"))))
        (is (equal '(("1" "a") ("2" "m"))
                   (rows '("1") '("2"))))))))

(test ryow-composite-range-with-one-short-bound-keeps-own-rows
  "GH #413: with ONE bound given, a short tuple is still a prefix bound
\(IX-MAP now takes one range cursor on either path), so the overlay must
include the transaction's own extensions of a short END here too."
  (with-ryow-graph (g)
    (flet ((rows (&rest range)
             (mapcar (lambda (n) (list (ryow-node-k n) (ryow-node-tag n)))
                     (apply #'index-range g 'ryow-node '(k tag) range))))
      (with-transaction ()
        (make-ryow-node :k "1" :tag "a") (make-ryow-node :k "4" :tag "z"))
      (with-transaction ()
        (make-ryow-node :k "2" :tag "m") (make-ryow-node :k "4" :tag "x")
        (is (equal '(("1" "a") ("2" "m") ("4" "x") ("4" "z"))
                   (rows :end '("4"))))
        (is (equal '(("4" "x") ("4" "z"))
                   (rows :start '("4"))))
        (is (equal '(("1" "a") ("2" "m"))
                   (rows :end '("2"))))))))

;;; --- GH #409: entry-level reads that resolve no node ---------------------

(test ryow-index-count-reflects-own-writes
  "GH #409: INDEX-COUNT inside a transaction counts the entries the
transaction will commit: an own create adds one, a moved key leaves its
old value and arrives under the new, a delete leaves."
  (with-ryow-graph (g)
    (flet ((n (value) (index-count g 'ryow-node 'tag value))
           (nk (tuple) (index-count g 'ryow-node '(k tag) tuple :prefix t)))
      (with-transaction ()
        (make-ryow-node :k "1" :tag "a") (make-ryow-node :k "2" :tag "b"))
      (with-transaction ()
        (make-ryow-node :k "3" :tag "a")
        (is (= 2 (n "a")) "an own create counts")
        (is (= 1 (nk '("3"))) "and under a prefix")
        (let ((c (copy (first (index-lookup g 'ryow-node 'tag "b")))))
          (setf (ryow-node-tag c) "c") (save c))
        (is (= 0 (n "b")) "a moved key leaves its old value")
        (is (= 1 (n "c")) "and arrives under the new")
        (mark-deleted (first (index-lookup g 'ryow-node '(k tag)
                                           '("1" "a"))))
        (is (= 1 (n "a")) "a delete leaves")
        (is (= 0 (nk '("1")))))
      (is (= 1 (n "a")) "the same answers once committed")
      (is (= 1 (n "c")))
      (is (= 0 (n "b"))))))

(test ryow-map-index-prefixes-lists-own-prefixes
  "GH #409: MAP-INDEX-PREFIXES inside a transaction lists each prefix
the transaction will commit once, in index order: one its creates
introduce, not one whose only row it deleted or moved away, and a
prefix shared by a committed row and an own row once."
  (with-ryow-graph (g)
    (flet ((walk (arity &optional start)
             (let ((out '()))
               (map-index-prefixes (lambda (p) (push p out)) g 'ryow-node
                                   '(k tag) :arity arity :start start)
               (nreverse out))))
      (with-transaction ()
        (make-ryow-node :k "1" :tag "a") (make-ryow-node :k "2" :tag "b")
        (make-ryow-node :k "4" :tag "d"))
      (with-transaction ()
        (make-ryow-node :k "0" :tag "x")   ; a new first prefix
        (make-ryow-node :k "1" :tag "z")   ; a second row under "1"
        (make-ryow-node :k "3" :tag "y")   ; a new prefix in the middle
        (mark-deleted (first (index-lookup g 'ryow-node '(k tag)
                                           '("2" "b"))))
        (let ((c (copy (first (index-lookup g 'ryow-node '(k tag)
                                            '("4" "d"))))))
          (setf (ryow-node-k c) "5") (save c))   ; "4" moves to "5"
        (is (equal '(("0") ("1") ("3") ("5")) (walk 1)))
        (is (equal '(("0" "x") ("1" "a") ("1" "z") ("3" "y") ("5" "d"))
                   (walk 2)))
        (is (equal '(("1") ("3") ("5")) (walk 1 '("1"))) ":start inclusive")
        (is (equal '(("3") ("5")) (walk 1 '("2")))
            "a :start at the prefix only the deleted row held"))
      (is (equal '(("0") ("1") ("3") ("5")) (walk 1))
          "the same once committed"))))

;;; --- Task 7: OCC (spec §4.5) ----------------------------------------------

(test ryow-own-read-not-in-read-set
  "Review focus 5: reading an own write adds nothing to the read-set,
and an unrelated concurrent commit does not force a retry."
  (with-ryow-graph (g)
    (let ((attempts 0) a)
      (with-transaction () (setf a (make-ryow-node :k "a" :tag "t")))
      (with-transaction ()
        (incf attempts)
        (let ((c (copy a))) (setf (ryow-node-k c) "a2") (save c))
        (lookup-vertex (id a))
        (is (zerop (graph-db::object-set-count
                    (graph-db::read-set graph-db::*transaction*))))
        (when (= attempts 1)
          (bt:join-thread
           (bt:make-thread
            (lambda ()
              (let ((*graph* g))
                (with-transaction ()
                  (make-ryow-node :k "other" :tag "t"))))))))
      (is (= 1 attempts)))))
