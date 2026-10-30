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
