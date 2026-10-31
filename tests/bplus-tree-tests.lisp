;;;; Direct tests for the B+ tree (bplus-tree.lisp), the opt-in index
;;;; backend.  Until GH #404/#405 the tree was tested only through the
;;;; graph indexes; vivace-cabinet copied it and gave it this suite, which
;;;; is where both defects were found.

(in-package #:graph-db/test)

(def-suite bplus-tree-suite
  :description "B+ tree insert / find / remove / cursors / splits."
  :in graph-db-suite)

(in-suite bplus-tree-suite)

(defun make-integer-bplus-tree (heap &key (page-size 4096))
  "An integer-keyed, string-valued B+ tree over HEAP."
  (graph-db::make-bplus-tree :heap heap :page-size page-size
                             :key-comparison '< :key-equal '=
                             :value-equal 'equal
                             :key-serializer 'graph-db::serialize
                             :key-deserializer 'graph-db::deserialize
                             :value-serializer 'graph-db::serialize
                             :value-deserializer 'graph-db::deserialize))

(defun bpt-value (tree key)
  "KEY's value in TREE, or NIL."
  (let ((node (find-in-skip-list tree key)))
    (and node (graph-db::%sn-value node))))

(defun bpt-cursor-keys (cursor)
  (loop for node = (graph-db::cursor-next cursor :eoc)
        until (eq node :eoc)
        collect (graph-db::%sn-key node)))

(test bplus-insert-find
  "Inserted keys are found with their values; a duplicate key is refused."
  (with-temp-memory (heap)
    (let ((tree (make-integer-bplus-tree heap))
          (keys (alexandria:shuffle (loop for i from 1 to 200 collect i))))
      (dolist (k keys)
        (is-true (add-to-skip-list tree k (format nil "v~A" k))))
      (is (= 200 (graph-db::bplus-tree-count tree)))
      (dolist (k keys)
        (is (equal (format nil "v~A" k) (bpt-value tree k))))
      (is (null (add-to-skip-list tree 7 "again")))
      (is (equal "v7" (bpt-value tree 7)))
      (is (null (bpt-value tree 1000))))))

(test bplus-cursors-are-ordered
  "A full cursor yields every key in order; a range cursor is inclusive."
  (with-temp-memory (heap)
    (let ((tree (make-integer-bplus-tree heap))
          ;; Both bounds are keys, and 101 (a range start below) is not.
          (keys (adjoin 100
                        (adjoin 20000
                                (remove 101
                                        (remove-duplicates
                                         (loop repeat 5000
                                               collect (random 100000))))))))
      (dolist (k keys)
        (add-to-skip-list tree k "x"))
      (let ((sorted (sort (copy-list keys) #'<)))
        (flet ((within (lo hi)
                 (remove-if-not (lambda (k) (<= lo k hi)) sorted)))
          (is (equal sorted (bpt-cursor-keys (graph-db::make-cursor tree))))
          (is (equal (within 100 20000)
                     (bpt-cursor-keys
                      (graph-db::make-range-cursor tree 100 20000))))
          (is (equal (within 101 20000)
                     (bpt-cursor-keys
                      (graph-db::make-range-cursor tree 101 20000)))))))))

(test bplus-remove-half-then-all
  "Removing half the keys, then the rest, merges pages and collapses the
root; the tree stays correct and usable throughout."
  (with-temp-memory (heap)
    ;; Small pages, so internal nodes split and merge too.
    (let* ((tree (make-integer-bplus-tree heap :page-size 256))
           (keys (alexandria:shuffle (loop for i below 20000 collect i)))
           (gone (remove-if-not #'evenp keys))
           (kept (sort (copy-list (remove-if #'evenp keys)) #'<)))
      (dolist (k keys)
        (add-to-skip-list tree k (format nil "v~A" k)))
      (is (< 2 (graph-db::%bpt-height tree)))
      (dolist (k gone)
        (is-true (remove-from-skip-list tree k)))
      (is (= 10000 (graph-db::bplus-tree-count tree)))
      (is (equal kept (bpt-cursor-keys (graph-db::make-cursor tree))))
      (is (null (bpt-value tree 4)))
      (is (equal "v5" (bpt-value tree 5)))
      (dolist (k kept)
        (remove-from-skip-list tree k))
      (is (= 0 (graph-db::bplus-tree-count tree)))
      (is (= 1 (graph-db::%bpt-height tree)))
      (is (null (bpt-cursor-keys (graph-db::make-cursor tree))))
      (is-true (add-to-skip-list tree 1 "again"))
      (is (equal "again" (bpt-value tree 1))))))

(test bplus-update
  "update-in-skip-list replaces a key's value."
  (with-temp-memory (heap)
    (let ((tree (make-integer-bplus-tree heap)))
      (add-to-skip-list tree 3 "old")
      (graph-db::update-in-skip-list tree 3 "new")
      (is (equal "new" (bpt-value tree 3)))
      (is (= 1 (graph-db::bplus-tree-count tree))))))

(test bplus-split-by-bytes
  "GH #404: a page of small entries splits by bytes, so it still takes
large ones.  Split at the middle ENTRY, 300-, 400- and 600-byte values
failed after 12, 9 and 6 inserts on a 4096-byte page and leaked a page."
  (dolist (size '(300 400 600 1000))
    (with-temp-memory (heap)
      (let ((tree (make-integer-bplus-tree heap))
            (big (make-string size :initial-element #\v)))
        (loop for k from 1 to 12
              do (add-to-skip-list tree k "x"))
        (loop for k from 13 to 60
              do (is-true (add-to-skip-list tree k big)
                          "insert ~D of a ~D-byte value failed" k size))
        (is (= 60 (graph-db::bplus-tree-count tree)))
        (is (equal big (bpt-value tree 60)))
        (is (equal (loop for k from 1 to 60 collect k)
                   (bpt-cursor-keys (graph-db::make-cursor tree))))))))

(test bplus-remove-by-value
  "GH #405: removing with a value removes only when the stored value
matches.  The stored value was compared serialized with EQUAL, which
never matches two octet vectors, so the value-qualified remove was a
no-op."
  (with-temp-memory (heap)
    (let ((tree (make-integer-bplus-tree heap)))
      (add-to-skip-list tree 9 "v")
      (is (null (remove-from-skip-list tree 9 "w"))
          "a wrong value must not remove")
      (is (equal "v" (bpt-value tree 9)))
      (is-true (remove-from-skip-list tree 9 "v")
               "the right value must remove")
      (is (null (bpt-value tree 9)))
      (add-to-skip-list tree 9 "v")
      (is-true (remove-from-skip-list tree 9) "no value: remove by key"))))

;;; ---------------------------------------------------------------------------
;;; GH #319: what a cursor's leaf snapshot and stale sibling link survive.
;;; A writer may run between two CURSOR-NEXT calls (each takes the read
;;; lock on its own), so these interleave the writes by hand.
;;; ---------------------------------------------------------------------------

(defun %bpt-drain (cursor)
  (bpt-cursor-keys cursor))

(test bplus-cursor-survives-a-split-of-its-own-leaf
  "GH #319 (2): the cursor's leaf splits after it was copied.  Every key
that existed when the cursor started is still visited, once, in order."
  (with-temp-memory (heap)
    (let* ((tree (make-integer-bplus-tree heap :page-size 256))
           (keys (loop for k from 0 below 400 by 10 collect k)))
      (dolist (k keys) (add-to-skip-list tree k "x"))
      (let ((cursor (graph-db::make-cursor tree))
            (seen '()))
        ;; Read two entries from the first leaf, then split it.
        (dotimes (i 2)
          (push (graph-db::%sn-key (graph-db::cursor-next cursor :eoc))
                seen))
        (loop for k from 21 to 29 do (add-to-skip-list tree k "new"))
        (loop for k from 31 to 39 do (add-to-skip-list tree k "new"))
        (let ((rest (%bpt-drain cursor)))
          (setq seen (append (nreverse seen) rest)))
        (let ((old-seen (remove-if-not (lambda (k) (member k keys)) seen)))
          (is (equal keys old-seen)
              "pre-existing keys visited: ~S" old-seen))))))

(test bplus-cursor-survives-the-next-leaf-being-merged-and-reused
  "GH #319 (2), the stale-link hazard: the leaf the cursor's copied link
names is merged away and its page reused for new keys elsewhere.  The
cursor must stay in order, emit nothing that was never a key, and reach
every new key; keys of its own copied page may still appear."
  (with-temp-memory (heap)
    (let* ((tree (make-integer-bplus-tree heap :page-size 256))
           (keys (loop for k from 0 below 400 by 10 collect k))
           (far (loop for k from 100000 below 100400 by 10 collect k)))
      (dolist (k keys) (add-to-skip-list tree k "x"))
      (let ((cursor (graph-db::make-cursor tree))
            (seen '()))
        (push (graph-db::%sn-key (graph-db::cursor-next cursor :eoc)) seen)
        ;; Empty the tree except the first key, so every later leaf is
        ;; merged and freed, then grow a new region far away: the freed
        ;; pages come back as the new leaves.
        (dolist (k (rest keys)) (remove-from-skip-list tree k))
        (dolist (k far) (add-to-skip-list tree k "far"))
        (let* ((all (append (nreverse seen) (%bpt-drain cursor))))
          (is (equal all (sort (copy-list all) #'<)) "out of order: ~S" all)
          (is (null (set-difference all (append keys far)))
              "keys that never existed: ~S"
              (set-difference all (append keys far)))
          (is (null (set-difference far all))
              "new keys skipped: ~S" (set-difference far all)))))))

(test bplus-update-never-shows-the-key-absent
  "GH #319 (1): UPDATE-IN-SKIP-LIST is remove-then-add; under two separate
write locks a concurrent reader could find the key gone between them.
A reader hammering the key during 5000 updates must never miss it."
  (with-temp-memory (heap)
    (let ((tree (make-integer-bplus-tree heap))
          (misses 0) (done nil))
      (add-to-skip-list tree 1 "a")
      (let ((reader (bt:make-thread
                     (lambda ()
                       (loop until done
                             do (unless (find-in-skip-list tree 1)
                                  (incf misses))))
                     :name "bpt-update-reader")))
        (dotimes (i 5000)
          (graph-db::update-in-skip-list tree 1 (if (evenp i) "b" "a")))
        (setq done t)
        (bt:join-thread reader))
      (is (zerop misses) "the reader found the key absent ~D times" misses)
      (is (equal "a" (bpt-value tree 1))))))
