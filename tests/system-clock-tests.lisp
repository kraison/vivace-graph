;;;; The image-level epoch clock (GH #168).  See
;;;; docs/superpowers/specs/2026-08-20-namespaces-design.md §6.
(in-package #:graph-db/test)

(def-suite system-clock-suite :in graph-db-suite
  :description "The image-level epoch clock and its journal.")
(in-suite system-clock-suite)

(test clock-issues-monotonic-epochs
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (let ((a (clock-next-epoch c))
                 (b (clock-next-epoch c))
                 (d (clock-next-epoch c)))
             (is (< a b d)))
        (close-system-clock c)))))

(test clock-current-epoch-is-the-next-id
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (let ((seen (clock-current-epoch c)))
             (is (= seen (clock-next-epoch c)))
             (is (= (1+ seen) (clock-current-epoch c))))
        (close-system-clock c)))))

(test clock-observe-epoch-is-a-max
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             (clock-observe-epoch c 5000)
             (is (> (clock-next-epoch c) 5000))
             ;; A lower observation must not move it backwards.
             (let ((before (clock-current-epoch c)))
               (clock-observe-epoch c 10)
               (is (= before (clock-current-epoch c)))))
        (close-system-clock c)))))

(test clock-survives-clean-reopen-without-reissuing
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir)))
           (last (progn (dotimes (i 10) (clock-next-epoch c))
                        (clock-next-epoch c))))
      (close-system-clock c)
      (let ((c2 (open-system-clock (namestring dir))))
        (unwind-protect
             (is (> (clock-next-epoch c2) last))
          (close-system-clock c2))))))

(test clock-survives-crash-without-reissuing
  ;; No CLOSE-SYSTEM-CLOCK: simulates a crash after ids were handed out.  The
  ;; block reservation on disk must already dominate every issued id.
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir) :block-size 8))
           (issued (loop repeat 5 collect (clock-next-epoch c)))
           (highest (reduce #'max issued)))
      (let ((c2 (open-system-clock (namestring dir) :block-size 8)))
        (unwind-protect
             (is (> (clock-next-epoch c2) highest))
          (close-system-clock c2))))))

(test clock-survives-crash-after-refilling-its-block
  ;; The above stays below BLOCK-SIZE, so it never exercises the
  ;; incremental refill in %CLOCK-RESERVE -- OPEN-SYSTEM-CLOCK's one-time
  ;; upfront reservation alone would satisfy it.  This one issues past
  ;; the block boundary (block-size 8, 20 ids) so refill's own disk write
  ;; is what has to be durable, not just the initial one.
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir) :block-size 8))
           (issued (loop repeat 20 collect (clock-next-epoch c)))
           (highest (reduce #'max issued)))
      (let ((c2 (open-system-clock (namestring dir) :block-size 8)))
        (unwind-protect
             (is (> (clock-next-epoch c2) highest))
          (close-system-clock c2))))))

(test clock-lease-is-disjoint-and-advances-the-clock
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (multiple-value-bind (start end) (clock-lease-epochs c 1000)
             (is (= 1000 (- end start)))
             ;; The clock has skipped the whole lease: nothing it issues now
             ;; can collide with an id the lease holder allocates.
             (is (>= (clock-next-epoch c) end)))
        (close-system-clock c)))))

(test journal-appends-and-reads-back
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             (journal-append c :detach :store :alpha
                              :lease-start 10 :lease-end 20)
             (journal-append c :attach :store :alpha)
             (let ((rs (journal-records c)))
               (is (= 2 (length rs)))
               (is (eq :detach (getf (first rs) :kind)))
               (is (eq :alpha (getf (first rs) :store)))
               (is (eq :attach (getf (second rs) :kind)))))
        (close-system-clock c)))))

(test journal-survives-reopen
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (journal-append c :create :store :beta)
      (close-system-clock c))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (is (equal '(:create) (mapcar (lambda (r) (getf r :kind))
                                         (journal-records c2))))
        (close-system-clock c2)))))

(test journal-refuses-to-evaluate-on-read
  ;; A journal is data.  Reading it must never evaluate.
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
      (format s "(:kind :bogus :value #.(error \"evaluated\"))~%"))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (signals error (journal-records c2))
        (close-system-clock c2)))))
