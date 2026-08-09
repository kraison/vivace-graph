;;;; Tests for the binned mmap heap allocator (allocator.lisp).

(in-package #:graph-db/test)

(def-suite allocator-suite
  :description "create-memory / allocate / free / map-memory."
  :in graph-db-suite)

(in-suite allocator-suite)

(test normalize-size
  "Sizes <=16 round to 16; <=512 round up to a multiple of 8; larger sizes
pass through unchanged."
  (is (= 16 (normalize-allocation-data-size 1)))
  (is (= 16 (normalize-allocation-data-size 16)))
  (is (= 24 (normalize-allocation-data-size 17)))
  (is (= 24 (normalize-allocation-data-size 24)))
  (is (= 512 (normalize-allocation-data-size 512)))
  (is (= 513 (normalize-allocation-data-size 513)))
  (is (= 4096 (normalize-allocation-data-size 4096))))

(test allocate-returns-usable-offset
  "Allocations land in the usable region (>= data-offset) and report the
normalized size as the second value."
  (with-temp-memory (mem)
    (multiple-value-bind (offset size) (allocate mem 10)
      (is (>= offset (memory-data-offset mem)))
      (is (= 16 size)))))

(test allocations-do-not-overlap
  "Sequential allocations of the same size are spaced by header + data
and never collide."
  (with-temp-memory (mem)
    (let ((offsets (loop repeat 50 collect (allocate mem 64))))
      ;; all distinct
      (is (= 50 (length (remove-duplicates offsets))))
      ;; monotonically increasing, gap = data-size (64) + 8-byte header
      (loop for (a b) on (sort (copy-list offsets) #'<)
            while b do (is (= 72 (- b a)))))))

(test write-then-read-bytes
  "Bytes written into an allocation read back unchanged."
  (with-temp-memory (mem)
    (let ((offset (allocate mem 8))
          (payload #(1 2 3 4 5 6 7 8)))
      (dotimes (i (length payload))
        (set-byte mem (+ offset i) (aref payload i)))
      (is (equalp payload (get-bytes mem offset (length payload)))))))

(test active-serialize-header-preserves-payload
  "serialize-header with :active-p t must preserve user payload bytes and not overwrite them with zeroes."
  (with-temp-memory (mem)
    (let ((p (allocate mem 32))
          (payload #(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255)))
      (dotimes (i (length payload))
        (set-byte mem (+ p i) (aref payload i)))
      (graph-db::serialize-header (memory-mmap mem) (- p 8) 32 :active-p t)
      (is (equalp payload (get-bytes mem p (length payload)))))))

(test free-then-reallocate-reuses-block
  "Freeing an allocation returns its block to the size's free list, and the
next same-size allocation reuses that exact offset."
  (with-temp-memory (mem)
    (let ((p1 (allocate mem 32)))
      (free mem p1)
      (let ((p2 (allocate mem 32)))
        (is (= p1 p2))))))

(test free-rejects-double-free
  "Freeing an already-free block signals an error."
  (with-temp-memory (mem)
    (let ((p (allocate mem 32)))
      (free mem p)
      (signals error (free mem p)))))

(test free-rejects-unsafe-pointer
  "Freeing a pointer below the usable region signals an error."
  (with-temp-memory (mem)
    (signals error (free mem 0))))

(test map-memory-visits-live-allocations
  "map-memory visits each live allocation exactly once (free blocks are
skipped unless include-free-p is set)."
  (with-temp-memory (mem)
    (let ((live (loop repeat 5 collect (allocate mem 100)))
          (doomed (allocate mem 100)))
      (declare (ignore live))
      (free mem doomed)
      (let ((seen (map-memory (lambda (offset size free-p)
                                (declare (ignore size free-p))
                                offset)
                              mem :collect-p t)))
        (is (= 5 (length seen)))
        ;; with include-free-p, the freed block shows up too
        (let ((seen-all (map-memory (lambda (offset size free-p)
                                      (declare (ignore offset size free-p))
                                      t)
                                    mem :collect-p t :include-free-p t)))
          (is (= 6 (length seen-all))))))))

(test free-list-tracks-freed-blocks
  "free-list reports freed blocks as (offset . size) pairs."
  (with-temp-memory (mem)
    (is (null (free-list mem)))
    (let ((p (allocate mem 48)))
      (free mem p)
      (let ((fl (free-list mem)))
        (is (= 1 (length fl)))
        (is (= 48 (cdr (first fl))))))))

(test reopen-memory-preserves-data
  "Data written before close-memory is visible after open-memory."
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "heap.dat" dir)))
           (mem (create-memory path (* 1024 1024)))
           offset)
      (setf offset (allocate mem 8))
      (dotimes (i 8) (set-byte mem (+ offset i) (+ 10 i)))
      (close-memory mem)
      (let ((mem2 (open-memory path)))
        (unwind-protect
             (is (equalp #(10 11 12 13 14 15 16 17)
                         (get-bytes mem2 offset 8)))
          (close-memory mem2))))))
