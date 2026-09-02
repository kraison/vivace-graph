;;;; Tests for the on-disk linear hash table (linear-hash.lisp).
;;;;
;;;; Keys are 16-byte UUID octet vectors; values are uint64 integers.

(in-package #:graph-db/test)

(def-suite linear-hash-suite
  :description "lhash insert / get / update / remove / split."
  :in graph-db-suite)

(in-suite linear-hash-suite)

(test insert-and-get
  (with-temp-lhash (h)
    (let ((k (gen-id)))
      (lhash-insert h k 42)
      (is (= 42 (lhash-get h k))))))

(test get-missing-returns-nil
  (with-temp-lhash (h)
    (is (null (lhash-get h (gen-id))))))

(test count-after-inserts
  (with-temp-lhash (h)
    (is (= 0 (read-lhash-count h)))
    (let ((keys (loop repeat 5 collect (gen-id))))
      (loop for k in keys for v from 1 do (lhash-insert h k v))
      (is (= 5 (read-lhash-count h))))))

(test remove-deletes-data
  "After removing a key it is no longer retrievable, while the other keys
keep their values.  (Counts after multi-key removes are exercised
elsewhere; see remove-sole-key-zeroes-count.)"
  (with-temp-lhash (h)
    (let ((k1 (gen-id)) (k2 (gen-id)) (k3 (gen-id)))
      (lhash-insert h k1 1)
      (lhash-insert h k2 2)
      (lhash-insert h k3 3)
      (lhash-remove h k2)
      (is (null (lhash-get h k2)))
      (is (= 1 (lhash-get h k1)))
      (is (= 3 (lhash-get h k3))))))

(test remove-sole-key-zeroes-count
  "Removing the only key leaves its bucket empty, so the count returns to 0."
  (with-temp-lhash (h)
    (let ((k (gen-id)))
      (lhash-insert h k 1)
      (is (= 1 (read-lhash-count h)))
      (lhash-remove h k)
      (is (= 0 (read-lhash-count h)))
      (is (null (lhash-get h k))))))

(test duplicate-key-signals
  (with-temp-lhash (h)
    (let ((k (gen-id)))
      (lhash-insert h k 1)
      (signals duplicate-key-error (lhash-insert h k 2)))))

(test update-changes-value
  (with-temp-lhash (h)
    (let ((k (gen-id)))
      (lhash-insert h k 100)
      (lhash-update h k 200)
      (is (= 200 (lhash-get h k)))
      ;; update must not change the count
      (is (= 1 (read-lhash-count h))))))

(test update-missing-signals
  (with-temp-lhash (h)
    (signals nonexistent-key-error (lhash-update h (gen-id) 1))))

(test remove-missing-leaves-data-intact
  (with-temp-lhash (h)
    (let ((k (gen-id)))
      (lhash-insert h k 1)
      (finishes (lhash-remove h (gen-id)))
      (is (= 1 (lhash-get h k))))))

(test map-lhash-visits-all-pairs
  (with-temp-lhash (h)
    (let ((entries (loop repeat 20 collect (cons (gen-id) (random 1000)))))
      (loop for (k . v) in entries do (lhash-insert h k v))
      (let ((seen (make-hash-table :test 'equalp)))
        (map-lhash (lambda (pair)
                     (setf (gethash (car pair) seen) (cdr pair)))
                   h)
        (is (= 20 (hash-table-count seen)))
        (loop for (k . v) in entries
              do (is (= v (gethash k seen))))))))

(test many-inserts-survive-splits
  "Inserting well past the load-factor threshold forces bucket splits;
every key must remain retrievable with its original value and the count
must be exact."
  (with-temp-lhash (h :buckets 4)
    (let ((entries (loop repeat 500 collect (cons (gen-id) (random 100000)))))
      (loop for (k . v) in entries do (lhash-insert h k v))
      (is (= 500 (read-lhash-count h)))
      (loop for (k . v) in entries
            do (is (= v (lhash-get h k))
                   "key lost across splits")))))

(test remove-keeps-count-accurate
  "Regression: removing a key from a bucket that still holds other keys used
to inflate read-lhash-count (the survivors were re-added through the normal
counting path).  With few buckets and many keys, bucket sharing is
guaranteed; the count must still equal the number of remaining keys."
  (with-temp-lhash (h :buckets 2)
    (let ((keys (loop repeat 40 collect (gen-id))))
      (loop for k in keys for v from 1 do (lhash-insert h k v))
      (is (= 40 (read-lhash-count h)))
      (loop for k in keys repeat 15 do (lhash-remove h k))
      (is (= 25 (read-lhash-count h)))
      ;; survivors keep their values
      (loop for k in (nthcdr 15 keys) for v from 16
            do (is (= v (lhash-get h k)))))))

(test reopen-preserves-contents
  (with-temp-directory (dir)
    (let ((entries (loop repeat 30 collect (cons (gen-id) (random 1000))))
          (h (make-lhash :location dir :buckets 4)))
      (loop for (k . v) in entries do (lhash-insert h k v))
      (close-lhash h)
      (let ((h2 (open-lhash dir)))
        (is (= 30 (read-lhash-count h2)))
        (loop for (k . v) in entries
              do (is (= v (lhash-get h2 k))))))))

(test a-copied-lhash-deletes-its-own-files-not-the-originals
  "GH #143: STRUCT.DAT carries the LOCATION the lhash was CREATED at, and
OPEN-LHASH used to leave the restored slot alone -- so DELETE-LHASH on a
cp -a copy deleted the ORIGINAL's files while the copy stayed intact.  The
slot must describe where the lhash was OPENED from."
  (with-temp-directory (root)
    (let* ((orig (merge-pathnames "orig/" root))
           (copy (merge-pathnames "copy/" root))
           (h (make-lhash :location orig :buckets 4))
           (k (gen-id)))
      (lhash-insert h k 7)
      (close-lhash h)
      (ensure-directories-exist copy)
      (dolist (f (uiop:directory-files orig))
        (uiop:copy-file f (merge-pathnames (file-namestring f) copy)))
      (let ((h2 (open-lhash copy)))
        (is (equal (namestring copy)
                   (namestring (graph-db::%lhash-location h2)))
            "the restored struct must record where it was opened from")
        (is (= 7 (lhash-get h2 k)))
        (graph-db::delete-lhash h2))
      (is (not (probe-file (merge-pathnames "table.dat" copy)))
          "the copy's files are gone")
      (is (probe-file (merge-pathnames "table.dat" orig))
          "the original's files remain")
      (let ((h3 (open-lhash orig)))
        (is (= 7 (lhash-get h3 k)) "and the original still reads")
        (close-lhash h3)))))
