;;;; Tests for the persistent UUID index-list (index-list.lisp).
;;;;
;;;; An index-list is a heap-backed singly linked list of 16-byte UUID keys
;;;; with lazy (mark-deleted) removal.  Built over a temp heap.

(in-package #:graph-db/test)

(def-suite index-list-suite
  :description "index-list push / member / pushnew / map / remove."
  :in graph-db-suite)

(in-suite index-list-suite)

(test make-empty
  (with-temp-memory (heap)
    (let ((il (make-index-list heap)))
      (is (= 0 (index-list-head il)))
      (is (null (index-list-keys il))))))

(test make-with-keys-preserves-order
  "make-index-list keeps the supplied key order when walked head-to-tail."
  (with-temp-memory (heap)
    (let* ((keys (loop repeat 4 collect (gen-id)))
           (il (apply #'make-index-list heap (mapcar #'copy-seq keys))))
      (is (equalp keys (index-list-keys il)))
      (is (equalp (second keys) (nth 1 (index-list-keys il)))))))

(test push-prepends
  (with-temp-memory (heap)
    (let ((il (make-index-list heap))
          (a (gen-id))
          (b (gen-id)))
      (index-list-push a il)
      (index-list-push b il)
      ;; b was pushed last, so it is at the head
      (let ((order (index-list-keys il)))
        (is (equalp b (first order)))
        (is (equalp a (second order)))
        (is (= 2 (length order)))))))

(test member-present-and-absent
  (with-temp-memory (heap)
    (let ((present (gen-id))
          (absent (gen-id))
          (il (make-index-list heap)))
      (index-list-push present il)
      (is-true (key-in-list-p present il))
      (is-false (key-in-list-p absent il)))))

(test pushnew-deduplicates
  (with-temp-memory (heap)
    (let ((k (gen-id))
          (il (make-index-list heap)))
      (index-list-pushnew k il)
      (index-list-pushnew k il)
      (is (= 1 (length (index-list-keys il)))))))

(test push-allows-duplicates
  "Plain push does not dedupe (that is what pushnew is for)."
  (with-temp-memory (heap)
    (let ((k (gen-id))
          (il (make-index-list heap)))
      (index-list-push k il)
      (index-list-push k il)
      (is (= 2 (length (index-list-keys il)))))))

(test remove-first-only
  (with-temp-memory (heap)
    (let ((k (gen-id))
          (other (gen-id))
          (il (make-index-list heap)))
      (index-list-push other il)
      (index-list-push k il)
      (index-list-push k il)          ; two copies of k
      (remove-from-index-list k il)   ; default: remove just the first
      (is (= 2 (length (index-list-keys il))))
      (is-true (key-in-list-p k il))
      (is-true (key-in-list-p other il)))))

(test remove-all
  (with-temp-memory (heap)
    (let ((k (gen-id))
          (other (gen-id))
          (il (make-index-list heap)))
      (index-list-push other il)
      (index-list-push k il)
      (index-list-push k il)
      (remove-from-index-list k il :remove-all-p t)
      (is-false (key-in-list-p k il))
      (is-true (key-in-list-p other il))
      (is (= 1 (length (index-list-keys il)))))))

(test removed-keys-are-skipped-by-map
  (with-temp-memory (heap)
    (let* ((keys (loop repeat 5 collect (gen-id)))
           (il (apply #'make-index-list heap (mapcar #'copy-seq keys))))
      (remove-from-index-list (third keys) il)
      (let ((live (index-list-keys il)))
        (is (= 4 (length live)))
        (is-false (member (third keys) live :test #'equalp))))))

(test remove-advances-past-deleted-cells
  "Removal must ADVANCE over a lazily-deleted cell, not spin on it:
remove the head key, then remove a key that lies BEYOND the deleted
cell.  Pre-fix, REMOVE-FROM-INDEX-LIST never advanced past a deleted
pcons and looped forever -- first exposed by COMPACT-EDGES' second
removal pass (GH #242, found on #208)."
  (with-temp-memory (heap)
    (let* ((keys (loop repeat 3 collect (gen-id)))
           (il (apply #'make-index-list heap (mapcar #'copy-seq keys))))
      ;; head-to-tail walk order == KEYS order
      (remove-from-index-list (first keys) il)  ; deleted cell at head
      (remove-from-index-list (third keys) il)  ; must walk past it
      (let ((live (index-list-keys il)))
        (is (= 1 (length live)))
        (is (equalp (second keys) (first live)))))))
