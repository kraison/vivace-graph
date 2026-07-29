;;;; Tests for the persistent skip list (skip-list.lisp).
;;;;
;;;; Each test builds an integer-keyed skip list over a temp heap via the
;;;; MAKE-INTEGER-SKIP-LIST fixture in suite.lisp.

(in-package #:graph-db/test)

(def-suite skip-list-suite
  :description "skip list add / find / remove / ordering / count."
  :in graph-db-suite)

(in-suite skip-list-suite)

(defun sl-find-value (sl key)
  "Return the value stored under KEY in SL, or NIL if absent."
  (let ((node (find-in-skip-list sl key)))
    (and node (%sn-value node))))

(test list-comparators-are-a-strict-total-order
  "LESS-THAN / GREATER-THAN order LISTS lexicographically -- and a list is NOT
strictly ordered against an EQUAL list.  Regression: the recursion once bottomed
out at (less-than NIL NIL) -> T, so an equal (or prefix) list compared as strictly
less than itself, corrupting any skip list keyed by composite (list ...) keys (the
:ORIGIN unique key (origin value), map-reduce views with list keys)."
  (flet ((lt (a b) (graph-db::less-than a b))
         (gt (a b) (graph-db::greater-than a b)))
    ;; equal lists: neither precedes, in either direction
    (is (not (lt '("o" "v") '("o" "v"))) "equal 2-lists are not <")
    (is (not (gt '("o" "v") '("o" "v"))) "equal 2-lists are not >")
    (is (not (lt '() '())) "equal empty lists are not <")
    ;; strict ordering by first then second element
    (is (lt '("o" "a") '("o" "b")) "same head, a<b")
    (is (gt '("o" "b") '("o" "a")) "same head, b>a")
    (is (lt '("a" "z") '("b" "a")) "head dominates")
    ;; prefix: shorter precedes longer
    (is (lt '("o") '("o" "v")) "a proper prefix precedes the longer list")
    (is (not (lt '("o" "v") '("o"))) "the longer list does not precede its prefix")
    (is (gt '("o" "v") '("o")) "the longer list follows its prefix")
    ;; antisymmetry on a handful of distinct pairs
    (dolist (pair '((("a") . ("a" "a")) (("a" "a") . ("a" "b")) (("a") . ("b"))))
      (is (eq t (and (lt (car pair) (cdr pair)) (not (lt (cdr pair) (car pair)))))
          "exactly one direction is < for distinct keys"))))

(defun sl-live-count (sl)
  "Number of live entries, counted by walking the level-0 chain.
NB: we deliberately do NOT use graph-db's SKIP-LIST-COUNT here -- it has an
infinite loop (it never advances its cursor).  SKIP-LIST-TO-LIST walks the
chain correctly, and %SL-LENGTH is the maintained counter."
  (length (skip-list-to-list sl)))

(test add-and-find
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 5 "five")
      (add-to-skip-list sl 1 "one")
      (add-to-skip-list sl 9 "nine")
      (is (string= "five" (sl-find-value sl 5)))
      (is (string= "one" (sl-find-value sl 1)))
      (is (string= "nine" (sl-find-value sl 9))))))

(test find-missing-returns-nil
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 5 "five")
      (is (null (find-in-skip-list sl 6))))))

(test maintains-sorted-order
  "Keys inserted in random order are returned in ascending order."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap))
          (keys (alexandria:shuffle (loop for i below 100 collect i))))
      (dolist (k keys) (add-to-skip-list sl k (* k k)))
      (let ((dumped (skip-list-to-list sl)))
        (is (equal (loop for i below 100 collect i)
                   (mapcar #'car dumped)))
        ;; values came back attached to the right keys
        (is (every (lambda (pair) (= (cdr pair) (* (car pair) (car pair))))
                   dumped))))))

(test count-matches-inserts
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 50) (add-to-skip-list sl i i))
      (is (= 50 (sl-live-count sl)))
      (is (= 50 (%sl-length sl))))))

(test remove-deletes-key
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 10) (add-to-skip-list sl i (* 10 i)))
      (remove-from-skip-list sl 5)
      (is (null (find-in-skip-list sl 5)))
      (is (= 9 (sl-live-count sl)))
      ;; neighbours survive and the order is still intact
      (is (= 40 (sl-find-value sl 4)))
      (is (= 60 (sl-find-value sl 6)))
      (is (equal '(0 1 2 3 4 6 7 8 9) (mapcar #'car (skip-list-to-list sl)))))))

(test skip-list-disallows-duplicates
  "With duplicates disallowed, re-adding an existing key is a no-op: the
count and the original value are unchanged, and add-to-skip-list returns NIL."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (is-true (add-to-skip-list sl 7 "first"))
      (is (null (add-to-skip-list sl 7 "second")))
      (is (= 1 (sl-live-count sl)))
      (is (= 1 (%sl-length sl)))
      (is (string= "first" (sl-find-value sl 7))))))

(test skip-list-count-terminates
  "Regression: skip-list-count used to loop forever because it never
advanced its cursor.  It must now return the live element count."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 25) (add-to-skip-list sl i i))
      (is (= 25 (skip-list-count sl)))
      (is (= (sl-live-count sl) (skip-list-count sl))))))

(test bulk-insert-all-retrievable
  "A larger shuffled load stays fully retrievable and correctly counted."
  (with-temp-memory (heap :size (* 1024 1024 128))
    (let ((sl (make-integer-skip-list heap))
          (keys (alexandria:shuffle (loop for i below 1000 collect i))))
      (dolist (k keys) (add-to-skip-list sl k (- k)))
      (is (= 1000 (sl-live-count sl)))
      (dolist (k '(0 1 250 499 500 999))
        (is (= (- k) (sl-find-value sl k)))))))

;;; ---------------------------------------------------------------------------
;;; Cursors (skip-list-cursors.lisp): keys/values cursors, map helpers, ranges.
;;; ---------------------------------------------------------------------------

(test cursor-walks-keys-and-values-in-order
  "map-skip-list / -keys / -values and the keys/values cursors all walk the list
in sorted key order."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dolist (k '(5 1 9 3 7))
        (add-to-skip-list sl k (* k 10)))
      ;; map-skip-list (over nodes) visits in key order
      (is (equal '(1 3 5 7 9)
                 (map-skip-list (lambda (n) (%sn-key n)) sl :collect-p t)))
      ;; map-skip-list-keys
      (is (equal '(1 3 5 7 9)
                 (map-skip-list-keys #'identity sl :collect-p t)))
      ;; a keys cursor yields the same, one advance at a time
      (let ((c (make-keys-cursor sl)) (got nil))
        (do ((k (cursor-next c) (cursor-next c))) ((null k))
          (push k got))
        (is (equal '(1 3 5 7 9) (nreverse got))))
      ;; a values cursor yields the values in key order
      (let ((c (make-values-cursor sl)) (got nil))
        (do ((v (cursor-next c) (cursor-next c))) ((null v))
          (push v got))
        (is (equal '(10 30 50 70 90) (nreverse got))))
      ;; map-skip-list-values calls fn on each value in key order
      (let ((vs nil))
        (map-skip-list-values (lambda (v) (push v vs)) sl)
        (is (equal '(10 30 50 70 90) (nreverse vs)))))))

(test range-cursor-restricts-to-bounds
  "make-range-cursor + cursor-next yields exactly the keys within [lo, hi], in
ascending order."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dolist (k (loop for i from 1 to 10 collect i))
        (add-to-skip-list sl k k))
      (let ((c (make-range-cursor sl 3 7)) (got nil))
        (do ((node (cursor-next c) (cursor-next c))) ((null node))
          (push (%sn-key node) got))
        (is (equal '(3 4 5 6 7) (nreverse got))
            "range [3,7] should yield exactly keys 3..7")))))

(test fetch-all-returns-every-value-for-a-key
  "On a duplicates-allowed skip list, skip-list-fetch-all returns every value
stored under a key; a key with one value returns a single-element list and an
absent key returns nil."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap :duplicates-allowed-p t)))
      (add-to-skip-list sl 1 :a)
      (add-to-skip-list sl 2 :b)
      (add-to-skip-list sl 2 :c)
      (add-to-skip-list sl 2 :d)
      (add-to-skip-list sl 3 :e)
      ;; all three values for key 2 come back (order-independent)
      (let ((vals (skip-list-fetch-all sl 2)))
        (is (= 3 (length vals)) "expected 3 values for key 2; got ~S" vals)
        (is (null (set-difference '(:b :c :d) vals))
            "expected {:b :c :d} for key 2; got ~S" vals))
      ;; a singleton key
      (is (equal '(:a) (skip-list-fetch-all sl 1)))
      ;; an absent key
      (is (null (skip-list-fetch-all sl 99))))))

;;; ---------------------------------------------------------------------------
;;; update / find-kv / duplicate-aware remove / node-list / empty edge cases
;;; ---------------------------------------------------------------------------

(test update-changes-existing-value
  "update-in-skip-list replaces the value for an existing key."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 1 10)
      (add-to-skip-list sl 2 20)
      (update-in-skip-list sl 2 222 20)   ; pass old-value to hit the in-place path
      (is (= 222 (sl-find-value sl 2)))
      (is (= 10 (sl-find-value sl 1)) "other keys untouched")
      (is (= 2 (sl-live-count sl)) "update must not change the count"))))

(test update-missing-key-inserts
  "update-in-skip-list on an absent key upserts it."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (is (null (find-in-skip-list sl 7)))
      (update-in-skip-list sl 7 70)
      (is (= 70 (sl-find-value sl 7)))
      (is (= 1 (sl-live-count sl))))))

(test find-kv-matches-key-and-value
  "find-kv-in-skip-list locates the node with a given key AND value among
duplicates, and returns nil when no such pair exists."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap :duplicates-allowed-p t)))
      (add-to-skip-list sl 2 :a)
      (add-to-skip-list sl 2 :b)
      (add-to-skip-list sl 2 :c)
      (let ((n (find-kv-in-skip-list sl 2 :b)))
        (is-true n "should find the (2,:b) pair")
        (when n (is (eql :b (%sn-value n)))))
      (is (null (find-kv-in-skip-list sl 2 :z)) "absent value -> nil"))))

(test remove-one-duplicate-key-is-consistent
  "Removing one of several duplicate-key entries removes exactly one occurrence
and leaves the rest correctly linked: count, fetch-all and the level-0 node list
all agree, and neighbouring keys are intact.  Repeated across random tower
layouts to guard the (formerly nondeterministic) duplicate-splice corruption."
  (dotimes (trial 12)
    (with-temp-memory (heap)
      (let ((sl (make-integer-skip-list heap :duplicates-allowed-p t)))
        (dolist (v '(:a :b :c)) (add-to-skip-list sl 2 v))
        (add-to-skip-list sl 1 :x)
        (add-to-skip-list sl 3 :y)
        (remove-from-skip-list sl 2)
        (is (= 4 (skip-list-count sl))
            "trial ~D: one of three key-2 dups removed (5 -> 4)" trial)
        (is (= 2 (length (skip-list-fetch-all sl 2)))
            "trial ~D: two key-2 values remain" trial)
        (is (= 4 (length (skip-list-to-node-list sl)))
            "trial ~D: level-0 list agrees with count" trial)
        (is-true (find-in-skip-list sl 1) "neighbour key 1 intact")
        (is-true (find-in-skip-list sl 3) "neighbour key 3 intact")))))

(test remove-specific-value-among-duplicates
  "remove-from-skip-list with a value removes exactly that key/value pair,
leaving the other duplicates."
  (dotimes (trial 12)
    (with-temp-memory (heap)
      (let ((sl (make-integer-skip-list heap :duplicates-allowed-p t)))
        (dolist (v '(:a :b :c)) (add-to-skip-list sl 2 v))
        (is-true (remove-from-skip-list sl 2 :b))
        (let ((vals (skip-list-fetch-all sl 2)))
          (is (= 2 (length vals)) "trial ~D: got ~S" trial vals)
          (is (null (set-difference '(:a :c) vals))
              "trial ~D: expected {:a :c}, got ~S" trial vals))))))

(test remove-all-duplicates-empties-key
  "Removing each duplicate of a key in turn leaves the key absent."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap :duplicates-allowed-p t)))
      (dolist (v '(:a :b :c :d)) (add-to-skip-list sl 2 v))
      (dotimes (k 4) (remove-from-skip-list sl 2))
      (is (= 0 (skip-list-count sl)))
      (is (null (find-in-skip-list sl 2)))
      (is (null (skip-list-fetch-all sl 2))))))

(test remove-with-value-on-unique-list
  "The value arg works on a unique-key list, and removing a non-matching value
is a no-op that returns nil."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 1 10)
      (add-to-skip-list sl 2 20)
      (is-true (remove-from-skip-list sl 2 20))
      (is (= 1 (skip-list-count sl)))
      (is (null (find-in-skip-list sl 2)) "key 2 gone")
      (is (= 10 (sl-find-value sl 1)) "key 1 untouched")
      (is (null (remove-from-skip-list sl 1 999)) "wrong value -> no-op")
      (is (= 1 (skip-list-count sl)) "count unchanged after no-op remove"))))

(test to-node-list-returns-nodes-in-order
  "skip-list-to-node-list returns the nodes in ascending key order."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dolist (k '(3 1 2)) (add-to-skip-list sl k (* k 10)))
      (let ((nodes (skip-list-to-node-list sl)))
        (is (equal '(1 2 3) (mapcar #'%sn-key nodes)))
        (is (equal '(10 20 30) (mapcar #'%sn-value nodes)))))))

(test empty-skip-list-edge-cases
  "An empty skip list: find -> nil, count -> 0, to-list -> nil, removing a
missing key is a no-op."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (is (null (find-in-skip-list sl 42)))
      (is (= 0 (sl-live-count sl)))
      (is (null (skip-list-to-list sl)))
      (remove-from-skip-list sl 42)            ; must not error
      (is (= 0 (sl-live-count sl))))))

(test analyze-heights-runs-on-populated-list
  "analyze-sl-heights runs and reports per-level node counts on a populated list."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 50) (add-to-skip-list sl i i))
      (let ((heights (analyze-sl-heights sl)))
        (is-true heights "analyze-sl-heights should return a non-nil report")))))

(test delete-skip-list-runs-clean
  "delete-skip-list tears down a populated list without error."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (dotimes (i 20) (add-to-skip-list sl i i))
      (finishes (delete-skip-list sl)))))

;;; ---- node-cache coherence (GH #83 regression) --------------------------
;;;
;;; READ-SKIP-NODE consults NODE-CACHE-VEC, a direct-mapped array cache added
;;; by cab4c9a.  It is keyed AND validated by heap address, so it is only sound
;;; if an address is evicted before it is freed: otherwise the allocator hands
;;; the same address to a new node, the (%SN-ADDR CACHED) guard matches, and the
;;; read returns the PREVIOUS node's key, value and pointers.

(test removing-a-node-evicts-its-address-from-the-node-cache
  "THE INVARIANT that makes the direct-mapped NODE-CACHE-VEC sound: an address
must not remain in it once REMOVE-FROM-SKIP-LIST hands that address back to the
allocator.

That cache is validated by address -- (= (%SN-ADDR CACHED) ADDR) -- so a stale
entry for a freed address becomes indistinguishable from a live one the moment
the allocator reissues the address, and READ-SKIP-NODE then returns the previous
node's key, value and POINTERS: wrong reads, and traversal cycles where the
stale pointers loop.  GH #83 (cab4c9a) added the cache without this eviction.

Note the CLRHASH below.  ADD-TO-SKIP-LIST populates only the weak hash cache, so
an immediate read is served from there and never touches the direct-mapped one;
the vec is filled only by a genuinely cold read, i.e. after GC has dropped the
weak entry.  Clearing the hash makes that cold read -- and hence this invariant
-- deterministic instead of dependent on when a GC happens to run."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 42 "forty-two")
      ;; stand in for the weak cache having been collected
      (clrhash (graph-db::%sl-node-cache sl))
      (let* ((node (find-in-skip-list sl 42))
             (addr (graph-db::%sn-addr node))
             (idx (graph-db::%node-cache-vec-index addr)))
        (is (eq node (aref (graph-db::%sl-node-cache-vec sl) idx))
            "precondition: a cold read must populate the direct-mapped cache")
        (remove-from-skip-list sl 42)
        ;; NB: compute the boolean OUTSIDE the IS.  FiveAM destructures
        ;; (is (not (f ...))) and evaluates the inner forms separately to build
        ;; its failure message, which defeats AND's short-circuit -- so an
        ;; (is (not (and cached ...))) would call %SN-ADDR on NIL.
        (let* ((cached (aref (graph-db::%sl-node-cache-vec sl) idx))
               (stale-p (and cached (= (graph-db::%sn-addr cached) addr))))
          (is (null stale-p)
              "address ~D was freed but is still in the direct-mapped cache; a ~
node later allocated at that address would read back this stale entry" addr))
        (is (null (sl-find-value sl 42)) "the removed key must be gone")))))

(test cache-enabled-nil-forces-a-fresh-read
  "Binding *CACHE-ENABLED* to NIL must bypass BOTH node caches.  The
direct-mapped cache added by cab4c9a was consulted before the flag was tested
and written unconditionally, so the flag silently stopped disabling skip-list
node caching -- which the profiler's cold-read workloads rely on."
  (with-temp-memory (heap)
    (let ((sl (make-integer-skip-list heap)))
      (add-to-skip-list sl 7 "seven")
      (is (string= "seven" (sl-find-value sl 7)))
      (let ((graph-db::*cache-enabled* nil))
        (is (string= "seven" (sl-find-value sl 7))
            "a read with caching disabled must still return the right value")
        ;; and it must not have been served from, or written to, the vec cache
        (is (null (aref (graph-db::%sl-node-cache-vec sl)
                        (graph-db::%node-cache-vec-index
                         (graph-db::%sn-addr (find-in-skip-list sl 7)))))
            "a read with caching disabled populated the direct-mapped cache")))))
