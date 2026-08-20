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
  ;; A journal is data.  Reading it must never evaluate.  Must assert the
  ;; condition TYPE: bare `signals error' passes under *READ-EVAL* T too
  ;; (the crafted form's own ERROR call also signals an ERROR), so it
  ;; proves nothing.  READER-ERROR only comes from the reader refusing.
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
      (format s "(:kind :bogus :value #.(error \"evaluated\"))~%"))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (signals reader-error (journal-records c2))
        (close-system-clock c2)))))

;;; Routing epoch allocation through the clock (GH #168 task 3).

(test two-stores-on-one-clock-get-disjoint-ordered-epochs
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let ((ga (make-graph :sc-alpha (namestring da)
                                     :buffer-pool-size 1000
                                     :system-clock clock))
                     (gb (make-graph :sc-beta (namestring db)
                                     :buffer-pool-size 1000
                                     :system-clock clock)))
                 (unwind-protect
                      (let ((ids '()))
                        (dotimes (i 3)
                          (push (transaction-id
                                 (with-transaction ((graph-db::transaction-manager ga))
                                   *transaction*))
                                ids)
                          (push (transaction-id
                                 (with-transaction ((graph-db::transaction-manager gb))
                                   *transaction*))
                                ids))
                        (let ((sorted (sort (copy-list ids) #'<)))
                          ;; No two transactions anywhere share an epoch.
                          (is (= (length sorted)
                                 (length (remove-duplicates sorted))))))
                   (close-graph ga)
                   (close-graph gb)))))
        (close-system-clock clock)))))

(test no-clock-means-per-store-counters-unchanged
  ;; The backward-compatibility hinge: with *SYSTEM-CLOCK* nil and no
  ;; :SYSTEM-CLOCK argument, two graphs allocate independently, exactly as
  ;; before #168 -- so both start low and their ids DO collide.
  (with-temp-directory (da)
    (with-temp-directory (db)
      (let ((ga (make-graph :sc-gamma (namestring da) :buffer-pool-size 1000))
            (gb (make-graph :sc-delta (namestring db) :buffer-pool-size 1000)))
        (unwind-protect
             (let ((ia (transaction-id (with-transaction ((graph-db::transaction-manager ga))
                                         *transaction*)))
                   (ib (transaction-id (with-transaction ((graph-db::transaction-manager gb))
                                         *transaction*))))
               (is (= ia ib)))
          (close-graph ga)
          (close-graph gb))))))

(test attaching-a-store-raises-the-clock-above-its-history
  ;; The watermark: a store with existing history must not hand the clock a
  ;; reason to reissue an epoch that store already used.
  (with-temp-directory (cdir)
    (with-temp-directory (gdir)
      (let ((g (make-graph :sc-eps (namestring gdir) :buffer-pool-size 1000)))
        (dotimes (i 5) (with-transaction ((graph-db::transaction-manager g)) t))
        (let ((highest (load-highest-transaction-id g)))
          (close-graph g)
          (let ((clock (open-system-clock (namestring cdir))))
            (unwind-protect
                 (let ((g2 (open-graph :sc-eps (namestring gdir)
                                       :buffer-pool-size 1000
                                       :system-clock clock)))
                   (unwind-protect
                        (is (> (clock-current-epoch clock) highest))
                     (close-graph g2)))
              (close-system-clock clock))))))))
