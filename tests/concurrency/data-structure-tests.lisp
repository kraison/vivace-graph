;;;; CONCURRENT-DATA-STRUCTURE-SUITE
;;;;
;;;; Tests concurrent access to the lower-level on-disk data structures
;;;; that underpin the graph:
;;;;
;;;;   1. Linear hash table under bucket splits: readers must not lose data
;;;;      while a writer triggers rehashing.
;;;;   2. Skip list under concurrent inserts: the per-node lock protocol must
;;;;      preserve all entries and key/value integrity.
;;;;   3. VE-index under simultaneous edge insertion and traversal: readers
;;;;      calling outgoing-edges must never observe corruption or crash while
;;;;      writers are adding edges to the same hub vertex.

(in-package #:graph-db/concurrency-test)

(def-suite concurrent-data-structure-suite
  :description "Data-structure-level correctness under concurrent load."
  :in concurrency-suite)

(in-suite concurrent-data-structure-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: lhash readers during bucket splits
;;;
;;; One writer inserts 1 000 new keys (enough to force several splits of a
;;; 4-bucket table) while N-1 reader threads repeatedly look up keys that
;;; were pre-inserted before the concurrent phase started.  No pre-inserted
;;; key may be lost or return a wrong value after the concurrent phase ends.
;;; ---------------------------------------------------------------------------

(test lhash-concurrent-reads-during-split
  "N-1 reader threads scan pre-inserted keys while 1 writer forces splits;
no key must be lost or corrupted."
  (let* ((pre-count 50)
         (new-count 1000)
         (pre-keys  (loop repeat pre-count collect (gen-id))))
    (with-conc-lhash (h :buckets 4)
      (loop for k in pre-keys for v from 1
            do (lhash-insert h k v))
      (run-threads *thread-count*
                   (lambda (i)
                     (if (= i 0)
                         ;; Writer: insert new keys, forcing bucket splits.
                         (let ((v 100))
                           (dotimes (_ new-count)
                             (lhash-insert h (gen-id) v)
                             (incf v)))
                         ;; Readers: look up every pre-inserted key.
                         (dolist (k pre-keys)
                           (lhash-get h k)))))
      (is (loop for k in pre-keys for v from 1
                always (eql v (lhash-get h k)))
          "One or more pre-inserted keys were lost or corrupted during split")
      (is (= (+ pre-count new-count) (read-lhash-count h))
          "Expected ~D total entries after split; found ~D"
          (+ pre-count new-count)
          (read-lhash-count h)))))

;;; ---------------------------------------------------------------------------
;;; Test 2: skip-list concurrent inserts
;;;
;;; N threads each insert M distinct integer keys into the same skip list.
;;; add-to-skip-list uses per-node locking internally; this test verifies that
;;; the lock protocol correctly serialises pointer updates so no entry is lost
;;; and no key/value pair is corrupted.
;;; ---------------------------------------------------------------------------

(test skip-list-concurrent-inserts
  "N threads each insert M distinct keys into one skip list; final count
= N×M and every key/value pair must be intact."
  ;; Previously skipped on ECL: the skip list's lock-free reads
  ;; (find-in-skip-list -> read-skip-node) read a node's size header + pointers
  ;; while concurrent inserts wrote them as separate bytes; a torn read ->
  ;; out-of-bounds get-bytes -> SIGSEGV on ECL's raw c-inline deref (SBCL/CCL
  ;; tolerate it via the get-bytes SEGV-retry + real CAS).  Now fixed for ECL by
  ;; serializing every public skip-list op with a per-skip-list recursive lock
  ;; (WITH-SL-LOCK; a no-op on the other impls, which keep the lock-free design).
  (let* ((n *thread-count*)
         (m 100))
    (with-conc-memory (heap)
      (let ((sl (make-conc-integer-skip-list heap)))
        ;; Each thread owns a non-overlapping key range to avoid duplicates.
        (run-threads n
                     (lambda (i)
                       (let ((base (* i m)))
                         (dotimes (j m)
                           (add-to-skip-list sl (+ base j) (+ base j))))))
        (let ((entries (skip-list-to-list sl)))
          (is (= (* n m) (length entries))
              "Expected ~D entries; found ~D" (* n m) (length entries))
          (is (every (lambda (pair) (= (car pair) (cdr pair))) entries)
              "Key/value mismatch after concurrent inserts"))))))

(test skip-list-concurrent-add-and-remove
  "GH #294: N/2 adder threads and N/2 remover threads hammer one skip list.
The old locking acquired pred locks one node at a time in list-position
order over STRIPED mutexes -- position order is not stripe order, so two
mutators could take two stripes AB/BA and deadlock (seen in CI run
33417325199); remove also locked the victim before the preds, a second
ordering violation.  Stripe-ordered acquisition (%GRAB-STRIPES) is the fix;
this is the discriminating load.  Each adder owns a key range; removers
chase the same ranges, so preds collide constantly.  The final state must
be exactly the never-removed keys, intact."
  (let* ((n (max 4 *thread-count*))
         (pairs (floor n 2))
         (m 60)
         (keep 10))  ; per range, keys < KEEP are never removed
    (with-conc-memory (heap)
      (let ((sl (make-conc-integer-skip-list heap)))
        (run-threads
         n
         (lambda (i)
           (let* ((pair (mod i pairs))
                  (base (* pair m)))
             (if (< i pairs)
                 ;; Adder for range PAIR.
                 (dotimes (j m)
                   (add-to-skip-list sl (+ base j) (+ base j)))
                 ;; Remover for range PAIR: spin on the removable keys
                 ;; until each has been observed and removed once.
                 (loop for j from keep below m do
                      (loop until (remove-from-skip-list sl (+ base j))
                            do (bt:thread-yield)))))))
        (let ((entries (skip-list-to-list sl)))
          (is (= (* pairs keep) (length entries))
              "Expected ~D surviving entries; found ~D"
              (* pairs keep) (length entries))
          (is (every (lambda (pair) (= (car pair) (cdr pair))) entries)
              "Key/value mismatch after concurrent add+remove")
          (is (every (lambda (pair) (< (mod (car pair) m) keep)) entries)
              "A removed key survived"))))))

;;; ---------------------------------------------------------------------------
;;; Test 3: ve-index concurrent traversal and mutation
;;;
;;; N/2 writer threads add edges from a shared hub vertex while N/2 reader
;;; threads call outgoing-edges on the same hub.  The ve-index uses per-key
;;; lock vectors; this test verifies that concurrent readers never observe a
;;; corrupted or partially-written adjacency list.
;;; ---------------------------------------------------------------------------

(test ve-index-concurrent-traversal-and-mutation
  "N/2 writers add edges to a hub while N/2 readers traverse it;
no corruption or crash; final edge count must equal writers × M."
  (let* ((n       *thread-count*)
         (writers (max 1 (floor n 2)))
         (m       10))
    (with-conc-graph (g)
      (let ((hub-id     nil)
            (target-ids (make-array (* writers m))))
        (with-transaction ()
          (setq hub-id (id (make-c-item :value 0)))
          (dotimes (i (* writers m))
            (setf (aref target-ids i)
                  (id (make-c-item :value (1+ i))))))
        (run-threads n
                     (lambda (i)
                       (if (< i writers)
                           ;; Writer: add M edges from hub to pre-created targets.
                           (let ((base (* i m)))
                             (dotimes (j m)
                               (with-transaction ()
                                 (make-c-link
                                  :from (lookup-vertex hub-id)
                                  :to   (lookup-vertex
                                         (aref target-ids (+ base j)))))))
                           ;; Reader: traverse hub's outgoing edges repeatedly.
                           (dotimes (_ (* m 5))
                             (outgoing-edges (lookup-vertex hub-id))))))
        (is (= (* writers m)
               (length (outgoing-edges (lookup-vertex hub-id))))
            "Expected ~D edges from hub; found ~D"
            (* writers m)
            (length (outgoing-edges (lookup-vertex hub-id))))))))
