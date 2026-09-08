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
  "The COUNT-INDEX for IX-CLAIM.SLOTS in G with an EMPTY map.  The
tests below drive the counter primitives by hand, and the commit pass
(GH #361) has already counted the fixture's nodes, so the built map is
dropped and remade -- what is then read back is exactly the hand-driven
contributions.  Never call it on a declaration the test wants unbuilt."
  (let* ((reg (graph-db::%count-registry g))
         (key (cons 'ix-claim slots))
         (old (gethash key reg)))
    (when old
      (let ((sl (graph-db::count-index-skip-list old)))
        (when (graph-db::view-index-p sl)
          (graph-db::delete-view-index sl)))
      (remhash key reg))
    (graph-db::%count-index-for
     g (graph-db::%count-spec-for 'ix-claim slots g))))

(defun %drop-count-maps (g)
  "Drop G's built count maps, freeing their heap pages: the state a
reopened graph is in until Task 4 installs count indexes at open."
  (let ((reg (graph-db::count-indexes g)))
    (when reg
      (maphash (lambda (k cix)
                 (declare (ignore k))
                 (let ((sl (graph-db::count-index-skip-list cix)))
                   (when (graph-db::view-index-p sl)
                     (graph-db::delete-view-index sl))))
               reg)
      (clrhash reg))))

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

(defvar *ix-resolutions* nil
  "Node resolutions counted by %COUNT-RESOLUTIONS; NIL when not counting.")

(defun %count-resolutions (thunk)
  "Run THUNK counting LOOKUP-VERTEX calls through an :AROUND method
removed afterwards (the generic; an FDEFINITION swap would miss it).
Returns the count.  Callers prove the probe live with a control."
  (let* ((gf #'lookup-vertex)
         (method (eval '(defmethod lookup-vertex :around
                            ((id t) &key &allow-other-keys)
                          (when *ix-resolutions* (incf *ix-resolutions*))
                          (call-next-method)))))
    (unwind-protect
         (let ((*ix-resolutions* 0))
           (funcall thunk)
           *ix-resolutions*)
      (remove-method gf method))))

(test commits-maintain-the-counters
  "Spec §2.3: a committed create counts at every prefix; a retraction-
shaped update (predicate flip, tuple unchanged) moves CURRENT only; a
delete subtracts ONCE, taken while a sibling still holds the prefix up
so a double subtraction is visible; an update that changes an indexed
slot moves both counters and removes the emptied old key."
  (with-ix-graph (g)
    (let (a b)
      (with-transaction ()
        (setq a (id (make-ix-claim :ns "ops" :key "e1" :rel "at"))
              b (id (make-ix-claim :ns "ops" :key "e2" :rel "at")))
        (make-ix-claim :ns "ops" :key "e3" :rel "at"))
      (is (equal '(3 3) (%count-of g '(ns key) '("ops"))))
      (is (equal '(3 nil) (%count-of g '(rel) "at")))
      (with-transaction ()
        (let ((c (copy (lookup-vertex a))))
          (setf (ix-rel c) "dead")
          (save c)))
      (is (equal '(3 2) (%count-of g '(ns key) '("ops")))
          "the flip moved CURRENT and left ALL")
      (is (equal '(2 nil) (%count-of g '(rel) "at")))
      (is (equal '(1 nil) (%count-of g '(rel) "dead"))
          "on the (rel) index the same update is a tuple move")
      (with-transaction () (mark-deleted (lookup-vertex a)))
      (is (equal '(2 2) (%count-of g '(ns key) '("ops")))
          "a delete subtracts once: subtracting twice reads (1 2)")
      (is (equal '(0 0) (%count-of g '(ns key) '("ops" "e1")))
          "and the emptied key is gone")
      (is (equal '(0 nil) (%count-of g '(rel) "dead")))
      (with-transaction ()
        (let ((c (copy (lookup-vertex b))))
          (setf (ix-ns c) "hr")
          (save c)))
      (is (equal '(1 1) (%count-of g '(ns key) '("hr"))))
      (is (equal '(1 1) (%count-of g '(ns key) '("ops"))))
      (is (equal '(0 0) (%count-of g '(ns key) '("ops" "e2")))
          "the moved-away key is gone")
      (with-transaction () (mark-deleted (lookup-vertex b)))
      (is (equal '(0 0) (%count-of g '(ns key) '("hr")))
          "and a key goes at zero"))))

(test a-re-applying-apply-marks-stale-and-a-rebuild-repairs
  "Spec R8, facts X7: under *ADD-TO-INDEXES-UNLESS-PRESENT-P* the pass
counts nothing and marks the maps stale; the next lookup rebuilds by
scan, so applying the same writes twice leaves the counters equal to one
application; the map is corrupted first, so only a real scan can put it
back.  Ablation, recorded in the task report: counting anyway
under the special leaves the stale flag NIL and reads 3 -- the commit's
one plus both re-applications."
  (with-ix-graph (g)
    (let (writes)
      (with-transaction ()
        (make-ix-claim :ns "ops" :key "e1" :rel "at")
        (setq writes (copy-list (graph-db::writes *transaction*))))
      (is (equal '(1 1) (%count-of g '(ns key) '("ops"))) "control")
      ;; Corrupt the built map by hand: a placeholder rebuild that only
      ;; cleared the flag would leave this reading 9 9 (GH #361).
      (graph-db::%count-adjust
       (graph-db::%require-count-index g 'ix-claim '(ns key))
       '("ops") 8 8)
      (is (equal '(9 9) (%count-of g '(ns key) '("ops")))
          "corrupted, and nothing has rebuilt yet")
      (let ((graph-db::*add-to-indexes-unless-present-p* t))
        (graph-db::apply-tx-writes-to-count-indexes writes g)
        (graph-db::apply-tx-writes-to-count-indexes writes g))
      (is (eq t (graph-db::count-indexes-stale-p g)) "marked stale")
      (is (equal '(1 1) (%count-of g '(ns key) '("ops")))
          "the lookup rebuilt by scan: still one, and the 8 is gone")
      (is (null (graph-db::count-indexes-stale-p g))))))

(test a-listing-resolves-no-node
  "Spec §2.5: MAP-COUNT-INDEX and COUNT-INDEX-LOOKUP never resolve a
node.  Control: INDEX-LOOKUP resolves.  Ablation: a listing routed
through the secondary index turns the second check red."
  (with-ix-graph (g)
    (with-transaction ()
      (dotimes (i 20)
        (make-ix-claim :ns (nth (mod i 2) '("a" "b"))
                       :key (format nil "k~D" i) :rel "at")))
    (is (plusp (%count-resolutions
                (lambda () (index-lookup g 'ix-claim '(ns key rel) '("a")
                                         :prefix t))))
        "control: the probe counts the secondary lookup's resolutions")
    (is (= 0 (%count-resolutions
              (lambda ()
                (%entries g '(ns key) :depth 2 :prefix '("a"))
                (count-index-lookup g 'ix-claim '(ns key) '("b")))))
        "the count index answers from the map alone")))

(test a-map-the-pass-materialised-is-not-built
  "Review of GH #361: a commit touching a declared spec with no map
materialises one through %COUNT-INDEX-FOR and counts only that commit,
so registry presence is NOT builtness.  INSTALL-COUNT-INDEXES must
still scan it, and read the whole population, not the one write."
  (with-ix-graph (g)
    (with-transaction ()
      (dotimes (i 3)
        (make-ix-claim :ns "ops" :key (format nil "e~D" i) :rel "at")))
    ;; The state a reopened graph is in until Task 4 installs at open:
    ;; nodes on disk, no count map.
    (%drop-count-maps g)
    (with-transaction ()
      (make-ix-claim :ns "ops" :key "e3" :rel "at"))
    (is (equal '(1 nil) (%count-of g '(ns) "ops"))
        "the pass counted only the commit it saw")
    (graph-db::install-count-indexes g)
    (is (equal '(4 nil) (%count-of g '(ns) "ops"))
        "INSTALL scanned: the whole population, not the one write")
    (is (equal '(4 4) (%count-of g '(ns key) '("ops")))
        "and every other declared map with it")))

(test an-update-whose-new-node-is-deleted-releases
  "Review of GH #361: a TX-UPDATE whose NEW node carries the deleted
flag is a release and nothing more, as in the secondary method
(index.lisp).  Without that branch the tuple is unchanged and the
predicate has not flipped, so nothing moves and the prefix never falls."
  (with-ix-graph (g)
    (let (a)
      (with-transaction ()
        (setq a (id (make-ix-claim :ns "ops" :key "e1" :rel "at")))
        (make-ix-claim :ns "ops" :key "e2" :rel "at"))
      (is (equal '(2 2) (%count-of g '(ns key) '("ops"))) "control")
      (with-transaction ()
        (let ((c (copy (lookup-vertex a))))
          (setf (deleted-p c) t)
          (save c)))
      (is (equal '(1 1) (%count-of g '(ns key) '("ops")))
          "released, not re-counted")
      (is (equal '(0 0) (%count-of g '(ns key) '("ops" "e1")))))))

(test a-rebuild-reads-committed-state-and-pollutes-no-read-set
  "Review of GH #361: the lazy rebuild is authority, not a read of the
caller's view -- it scans with no ambient transaction, no read snapshot
and :RECORD-READS NIL (R1, GH #92).  So an AS-OF extent at an older
epoch still counts the whole committed population, and an open
transaction's read set is unchanged by the scan."
  (with-ix-graph (g)
    (with-transaction ()
      (dotimes (i 2)
        (make-ix-claim :ns "ops" :key (format nil "a~D" i) :rel "at")))
    (let ((old (latest-epoch g)))
      (with-transaction ()
        (dotimes (i 3)
          (make-ix-claim :ns "ops" :key (format nil "b~D" i)
                         :rel "at")))
      (setf (graph-db::count-indexes-stale-p g) t)
      (with-as-of ((g) old)
        (is (equal '(5 5) (%count-of g '(ns key) '("ops")))
            "the rebuild ignored the caller's as-of snapshot")))
    (setf (graph-db::count-indexes-stale-p g) t)
    (with-transaction ()
      (let ((before (graph-db::object-set-count
                     (graph-db::read-set *transaction*))))
        (is (equal '(5 5) (%count-of g '(ns key) '("ops"))))
        (is (= before (graph-db::object-set-count
                       (graph-db::read-set *transaction*)))
            "the scan added nothing to the open read set")))))

;;; --------------------------------------------------------------------
;;; Task 4: persistence, open and close (spec §2.4)
;;; --------------------------------------------------------------------

(defun %count-builds (thunk)
  "Run THUNK counting %BUILD-COUNT-INDEX-FOR-SPEC calls through an
FDEFINITION swap removed afterwards; returns the count.  Callers prove
the probe live with a control that must build."
  (let ((builds 0)
        (orig (fdefinition 'graph-db::%build-count-index-for-spec)))
    (unwind-protect
         (progn
           (setf (fdefinition 'graph-db::%build-count-index-for-spec)
                 (lambda (g s) (incf builds) (funcall orig g s)))
           (funcall thunk)
           builds)
      (setf (fdefinition 'graph-db::%build-count-index-for-spec) orig))))

(test count-index-survives-close-and-reopen
  "Spec §2.4: the sidecar round trip -- close, reopen, same counters and
NO scan (the probe counts zero builds; the last open is the control
proving it fires); and a declaration the stored graph predates is built
at open (the withdrawn open and close make the sidecar unable to
restore it)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-claim :ns "ops" :key "e1" :rel "at")
                 (make-ix-claim :ns "ops" :key "e2" :rel "dead")))
          (close-graph g)))
      (let (g)
        (is (= 0 (%count-builds
                  (lambda ()
                    (setq g (open-graph *ix-graph-name* path)))))
            "the sidecar was taken: nothing scanned at open")
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(2 1) (%count-of g '(ns key) '("ops")))
                   "restored from the sidecar")
               (is (graph-db::count-index-built-p
                    (graph-db::%require-count-index g 'ix-claim
                                                    '(ns key)))
                   "and the restored map is marked built"))
          (close-graph g :snapshot-p nil)))
      ;; Build-at-open: withdraw, open and close once (the sidecar
      ;; record is reclaimed), re-declare, open: the map is built by
      ;; scan.
      (undef-count-index ix-claim :graph-db-index-test :name ix-count-rel)
      (unwind-protect
           (let ((g (open-graph *ix-graph-name* path)))
             (unwind-protect
                  (let ((*graph* g))
                    (signals query-precondition-error
                      (count-index-lookup g 'ix-claim '(rel) "at")))
               (close-graph g :snapshot-p nil)))
        (def-count-index ix-claim (rel) :graph-db-index-test
          :name ix-count-rel))
      (let (g)
        (is (plusp (%count-builds
                    (lambda ()
                      (setq g (open-graph *ix-graph-name* path)))))
            "control: the probe fires on the open that must build")
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(1 nil) (%count-of g '(rel) "at"))
                   "built at open over the pre-existing nodes")
               (is (equal '(1 nil) (%count-of g '(rel) "dead"))))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))

(test a-withdrawn-count-index-is-reclaimed-at-open
  "Spec §2.4 (GH #147): a sidecar record whose declaration is withdrawn
has its pages reclaimed at the next open -- DELETE-VIEW-INDEX is called
for it.  The slots are (ns rel), declared nowhere else: the registry is
keyed (owner . slot-names), so a second live declaration on the same
slots would keep the record alive past the undef."
  (def-count-index ix-claim (ns rel) :graph-db-index-test
    :name ix-count-gone)
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-claim :ns "ops" :key "e1" :rel "at")))
          (close-graph g)))
      (undef-count-index ix-claim :graph-db-index-test
                         :name ix-count-gone)
      (let* ((freed 0)
             (old (fdefinition 'graph-db::delete-view-index)))
        (setf (fdefinition 'graph-db::delete-view-index)
              (lambda (ix) (incf freed) (funcall old ix)))
        (unwind-protect
             (let ((g (open-graph *ix-graph-name* path)))
               (close-graph g :snapshot-p nil)
               (collect-garbage))
          (setf (fdefinition 'graph-db::delete-view-index) old))
        (is (plusp freed) "the retired count map was reclaimed")))))

(test a-rebuild-retires-the-old-map-and-close-frees-it
  "Spec §2.3, last paragraph: MAP-COUNT-INDEX readers hold no lock, so a
rebuild must build a FRESH map and swap it in -- never free pages under
a cursor.  The old map lands on RETIRED-COUNT-MAPS and is freed by
CLOSE-GRAPH, counted through the DELETE-VIEW-INDEX seam."
  (with-temp-directory (dir)
    (let ((g (make-graph *ix-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction ()
               (make-ix-claim :ns "ops" :key "e1" :rel "at"))
             (let ((old (graph-db::count-index-skip-list
                         (graph-db::%require-count-index g 'ix-claim
                                                         '(ns key))))
                   (freed 0)
                   (orig (fdefinition 'graph-db::delete-view-index)))
               (unwind-protect
                    (progn
                      (setf (fdefinition 'graph-db::delete-view-index)
                            (lambda (ix) (incf freed) (funcall orig ix)))
                      (graph-db::rebuild-count-indexes g)
                      (is (= 0 freed)
                          "a rebuild frees no pages mid-session")
                      (is (member old (graph-db::retired-count-maps g))
                          "the old map is retired, not deleted")
                      (is (not (eq old
                                   (graph-db::count-index-skip-list
                                    (graph-db::%require-count-index
                                     g 'ix-claim '(ns key)))))
                          "a fresh map answers queries")
                      (is (equal '(1 1) (%count-of g '(ns key) '("ops")))
                          "and it holds the rebuilt counts")
                      (close-graph g :snapshot-p nil)
                      (is (plusp freed) "close freed the retired map")
                      (is (null (graph-db::retired-count-maps g))))
                 (setf (fdefinition 'graph-db::delete-view-index)
                       orig))))
        (ignore-errors (close-graph g :snapshot-p nil))
        (collect-garbage)))))

(test a-stale-flag-is-repaired-before-the-first-lookup-and-saved
  "Spec R8, facts X7: an apply under *ADD-TO-INDEXES-UNLESS-PRESENT-P*
(what a crash-recovery replay or a device re-pull runs) counts nothing
and flags the maps; the first lookup rebuilds by scan and counts the
write the maps never saw, and the close saves the rebuilt maps."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-claim :ns "ops" :key "e1" :rel "at"))
               (let ((graph-db::*add-to-indexes-unless-present-p* t))
                 (with-transaction ()
                   (make-ix-claim :ns "ops" :key "e2" :rel "at")))
               (is (eq t (graph-db::count-indexes-stale-p g)) "flagged")
               (is (equal '(2 2) (%count-of g '(ns key) '("ops")))
                   "the first lookup rebuilt and counted the flagged ~
write")
               (is (null (graph-db::count-indexes-stale-p g))))
          (close-graph g)))
      (let ((g (open-graph *ix-graph-name* path)))
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(2 2) (%count-of g '(ns key) '("ops")))
                   "the rebuilt maps were saved at close"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))

(test a-crash-recovery-open-rebuilds-the-counters
  "Spec R8, facts D20/X7: RECOVER-TRANSACTIONS replays the WAL tail
under *ADD-TO-INDEXES-UNLESS-PRESENT-P*, which counts nothing, and the
replay precedes the sidecar restore -- so OPEN-GRAPH must rebuild by
scan after restoring.  The crash shape is CRASH-RECOVERY-RESEEDS-THE-
TX-ID-WATERMARK's (detach-tests): a .committed file renamed back to
.txn.  The probe is the assertion; the count alone would be vacuous,
the pre-crash sidecar holding the same value."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-claim :ns "ops" :key "e1" :rel "at"))
               (let ((graph-db::*delete-committed-transaction-files*
                       nil))
                 (with-transaction ()
                   (make-ix-claim :ns "ops" :key "e2" :rel "at"))))
          (close-graph g :snapshot-p nil))
        ;; Fabricate the WAL tail: the survived .committed becomes a
        ;; .txn RECOVER-TRANSACTIONS will replay at the next open.
        (let* ((tx-dir (graph-db::persistent-transaction-directory g))
               (files (directory (merge-pathnames "*.committed" tx-dir))))
          (is (= 1 (length files))
              "sanity: exactly one committed file survived")
          (rename-file (first files)
                       (make-pathname :type "txn"
                                      :defaults (first files)))))
      (let (g)
        (is (plusp (%count-builds
                    (lambda ()
                      (setq g (open-graph *ix-graph-name* path)))))
            "the crash-recovery open rebuilt the counters by scan")
        (unwind-protect
             (let ((*graph* g))
               (is (equal '(2 2) (%count-of g '(ns key) '("ops")))
                   "the replayed write is counted once, not twice"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (collect-garbage))))))
