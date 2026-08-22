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

(defun %simulate-crash (clock)
  "Release CLOCK's flock without persisting anything, mirroring what the
kernel does when a process dies mid-flight (GH #182).  Lets crash tests
open a second clock on the same directory without CLOSE-SYSTEM-CLOCK's
clean write masking what's actually durable."
  (graph-db::%posix-close (graph-db::system-clock-lock-fd clock))
  ;; Clear the slot: CLOSE-SYSTEM-CLOCK guards on it, so a later close of
  ;; this struct would otherwise double-close -- and close a reused
  ;; descriptor belonging to something else.
  (setf (graph-db::system-clock-lock-fd clock) nil))

(test clock-survives-crash-without-reissuing
  ;; No CLOSE-SYSTEM-CLOCK: simulates a crash after ids were handed out.  The
  ;; block reservation on disk must already dominate every issued id.
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir) :block-size 8))
           (issued (loop repeat 5 collect (clock-next-epoch c)))
           (highest (reduce #'max issued)))
      (%simulate-crash c)
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
      (%simulate-crash c)
      (let ((c2 (open-system-clock (namestring dir) :block-size 8)))
        (unwind-protect
             (is (> (clock-next-epoch c2) highest))
          (close-system-clock c2))))))

(test clock-observe-epoch-persists-its-jump-across-a-crash
  ;; No coverage before this (GH #168 review): both crash tests above
  ;; exercise CLOCK-NEXT-EPOCH only, never an OBSERVE.  CLOCK-OBSERVE-
  ;; EPOCH's trailing %CLOCK-RESERVE forces the ceiling write that makes
  ;; a large foreign jump durable; without it a crash right after an
  ;; observe loses the jump and reopen resumes below it.
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir) :block-size 8))
           (observed 999999))
      (clock-observe-epoch c observed)
      (%simulate-crash c)
      (let ((c2 (open-system-clock (namestring dir) :block-size 8)))
        (unwind-protect
             (is (> (clock-next-epoch c2) observed))
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
  ;; A journal is data.  Reading it must never evaluate.  The #. bomb sits
  ;; MID-FILE (a good record follows) because a torn TAIL is now dropped,
  ;; not signalled (GH #191).  Must assert the wrapped condition's CAUSE
  ;; is a READER-ERROR: under *READ-EVAL* T the bomb's own ERROR call
  ;; would be caught and wrapped too, so asserting only the wrapper
  ;; proves nothing -- the cause type is what distinguishes the reader
  ;; REFUSING from the form EVALUATING.
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
      (format s "(:kind :bogus :value #.(error \"evaluated\"))~%")
      (format s "(:kind :attach :epoch 3 :store :after-bomb)~%"))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (let ((condition
                   (handler-case (progn (journal-records c2) nil)
                     (graph-db:system-journal-corrupt (e) e))))
             (is-true condition
                      "mid-file unreadable record must signal")
             (is (typep (graph-db:journal-corrupt-cause condition)
                        'reader-error)
                 "the cause must be the READER refusing, not evaluation"))
        (close-system-clock c2)))))

(test torn-final-record-is-dropped-with-a-warning
  "Power loss mid-append tears the LAST record; the intact history must
survive (GH #191).  Nearest wrong implementation: signal on any reader
error (the pre-#191 behaviour).  The non-ASCII store name pins
byte-accurate truncation; the append-after-recovery check pins that the
truncation reopened the writer (a stale stream would append to the old,
renamed-away inode and the record would vanish)."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (journal-append c :create :store :alpha)
      (journal-append c :detach :store "café-store")
      (journal-append c :attach :store :alpha)
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append)
      ;; No closing paren, no newline: a torn tail.
      (write-string "(:kind :retire :epoch 9 :store" s))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             (signals graph-db:system-journal-torn-tail
               (journal-records c2))
             ;; The truncation is durable: a second read is clean.
             (let ((rs nil))
               (is-true (handler-case (progn (setq rs (journal-records c2))
                                             t)
                          (graph-db:system-journal-torn-tail () nil))
                        "second read must not warn -- tail was truncated")
               (is (= 3 (length rs)))
               (is (equal '(:create :detach :attach)
                          (mapcar (lambda (r) (getf r :kind)) rs)))
               (is (equal "café-store" (getf (second rs) :store))
                   "byte-accurate truncation: multibyte record intact"))
             (journal-append c2 :retire :store :alpha)
             (is (= 4 (length (journal-records c2)))
                 "append after recovery must land in the LIVE file"))
        (close-system-clock c2)))))

(test torn-tail-recovery-with-a-live-writer
  "The torn-tail path is usually hit by a FRESH clock reopening after a
crash, but it can also be hit by the SAME clock that still holds its own
append stream open -- e.g. another writer tore the tail while this
process kept running.  That is the case %JOURNAL-TRUNCATE-TO's
close-the-append-stream step exists for (GH #191).  Nearest wrong
implementation: skip that close -- the post-recovery append below then
lands on the renamed-away (unlinked) inode and silently vanishes."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             ;; This APPEND opens and leaves open C's own stream on the
             ;; journal file -- unlike the fresh-reopen tests above.
             (journal-append c :create :store :alpha)
             ;; Tear the tail out-of-band, through a second stream, while
             ;; C's stream is still open on the same file -- the
             ;; power-loss shape: a write in flight, not a fresh process
             ;; discovering old damage.
             (with-open-file (s (merge-pathnames "system-journal.log" dir)
                                :direction :output :if-exists :append)
               (write-string "(:kind :retire :epoch 9 :store" s))
             (signals graph-db:system-journal-torn-tail
               (journal-records c))
             (journal-append c :attach :store :beta)
             (let ((rs (journal-records c)))
               (is (= 2 (length rs))
                   "post-recovery append must land in the LIVE file, ~
not the renamed-away inode")
               (is (equal '(:create :attach)
                          (mapcar (lambda (r) (getf r :kind)) rs)))))
        (close-system-clock c)))))

(test mid-file-corruption-still-signals
  "A bad record with good records AFTER it is not a torn tail -- it is
corruption of a different kind and must signal, and the reader must not
truncate anything (GH #191).  Nearest wrong implementation: treat every
reader error as a torn tail and truncate."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (journal-append c :create :store :alpha)
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append)
      ;; A lone close-paren is a genuine reader error (unmatched close);
      ;; unlike "%%% not a lisp form %%%" -- which the brief originally
      ;; specified but which READs cleanly as bare symbols under CL's
      ;; standard readtable (`%' is symbol-constituent) and so never
      ;; signals at all.  Fixed here; see task-1-report.md (GH #191).
      (format s ")~%")
      (format s "(:kind :attach :epoch 5 :store :alpha)~%"))
    (let ((before (alexandria:read-file-into-string
                   (merge-pathnames "system-journal.log" dir))))
      (let ((c2 (open-system-clock (namestring dir))))
        (unwind-protect
             (progn
               (signals graph-db:system-journal-corrupt
                 (journal-records c2))
               (is (equal before
                          (alexandria:read-file-into-string
                           (merge-pathnames "system-journal.log" dir)))
                   "mid-file corruption must not be truncated away"))
          (close-system-clock c2))))))

(defvar *journal-eval-sentinel* nil)

(test tail-read-eval-form-is-dropped-not-evaluated
  "A #. form AT the tail is dropped like any torn tail -- but it must be
dropped WITHOUT evaluating (GH #191).  The sentinel proves it: under
*READ-EVAL* T the form would set it before the reader error paths could
classify anything."
  (setq *journal-eval-sentinel* nil)
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (journal-append c :create :store :alpha)
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append)
      (format s "(:kind :bogus :value ~
#.(setq graph-db/test::*journal-eval-sentinel* t))~%"))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             (signals graph-db:system-journal-torn-tail
               (journal-records c2))
             (is (null *journal-eval-sentinel*)
                 "the #. form must never evaluate")
             (is (= 1 (length (journal-records c2)))))
        (close-system-clock c2)))))

(test torn-tail-second-value-reports-torn-p
  "TORN-P is FiveAM-blind: every torn-tail test above uses SIGNALS, which
exits non-locally before JOURNAL-RECORDS returns any values, so none of
them ever observed the second value.  Pin it directly on an OWNED clock:
true on the read that truncates, nil on the clean read after (GH #191)."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (journal-append c :create :store :alpha)
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append)
      (write-string "(:kind :retire :epoch 9 :store" s))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect
           (progn
             (multiple-value-bind (rs torn-p)
                 (handler-bind
                     ((graph-db:system-journal-torn-tail #'muffle-warning))
                   (journal-records c2))
               (is-true torn-p "first (owned) read must report torn-p")
               (is (= 1 (length rs))))
             (multiple-value-bind (rs2 torn-p2)
                 (journal-records c2)
               (declare (ignore rs2))
               (is (null torn-p2)
                   "second read after truncation must report torn-p nil")))
        (close-system-clock c2)))))

(test stale-clock-does-not-truncate-a-torn-tail
  "After CLOSE-SYSTEM-CLOCK the directory flock is released
(SYSTEM-CLOCK-LOCK-FD nil), but the struct survives.  JOURNAL-RECORDS on
that stale struct must not rename-truncate the file: another process may
hold the real lock and be mid-append, and truncating out from under it
would silently drop every record it appends after the rename (GH #182,
#191).  An unowned read still warns and reports TORN-P true, but leaves
the bytes alone -- it re-warns on every read until an OWNING clock
truncates it.  Ablation: removing the lock-fd guard in JOURNAL-RECORDS
makes the file-unchanged assertion below fail (see fix-wave-report.md)."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (journal-append c :create :store :alpha)
      (close-system-clock c))
    (with-open-file (s (merge-pathnames "system-journal.log" dir)
                       :direction :output :if-exists :append)
      (write-string "(:kind :retire :epoch 9 :store" s))
    (let ((before (alexandria:read-file-into-string
                   (merge-pathnames "system-journal.log" dir)))
          (stale (open-system-clock (namestring dir))))
      (close-system-clock stale)
      ;; STALE's lock-fd is now nil -- exactly the post-close shape the
      ;; ownership check must catch.
      (multiple-value-bind (rs torn-p)
          (handler-bind
              ((graph-db:system-journal-torn-tail #'muffle-warning))
            (journal-records stale))
        (is-true torn-p "an unowned read must still report torn-p")
        (is (= 1 (length rs))))
      (is (equal before
                 (alexandria:read-file-into-string
                  (merge-pathnames "system-journal.log" dir)))
          "an unowned read must leave the torn bytes untouched")
      ;; A fresh, OWNING clock can still recover it.
      (let ((owner (open-system-clock (namestring dir))))
        (unwind-protect
             (progn
               (signals graph-db:system-journal-torn-tail
                 (journal-records owner))
               (is-true
                (handler-case (progn (journal-records owner) t)
                  (graph-db:system-journal-torn-tail () nil))
                "an owning read truncates -- the second read stays clean"))
          (close-system-clock owner))))))

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
                      (let ((tm-a (graph-db::transaction-manager ga))
                            (tm-b (graph-db::transaction-manager gb))
                            (ids '()))
                        (dotimes (i 3)
                          (push (transaction-id (with-transaction (tm-a)
                                                  *transaction*))
                                ids)
                          (push (transaction-id (with-transaction (tm-b)
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
  ;; before #168 -- so both start low and their ids DO collide.  Assert the
  ;; actual pre-#168 values, not just their equality: an off-by-one NIL
  ;; branch (e.g. a bare INCF instead of PROG1-then-INCF) would still
  ;; satisfy (= IA IB) without this.  *SYSTEM-CLOCK* is bound explicitly
  ;; rather than relied on as a lambda-list default, so this test's premise
  ;; does not depend on run order against any test that SETFs the global.
  (let ((*system-clock* nil))
    (with-temp-directory (da)
      (with-temp-directory (db)
        (let ((ga (make-graph :sc-gamma (namestring da)
                              :buffer-pool-size 1000))
              (gb (make-graph :sc-delta (namestring db)
                              :buffer-pool-size 1000)))
          (unwind-protect
               (let* ((tm-a (graph-db::transaction-manager ga))
                      (tm-b (graph-db::transaction-manager gb))
                      (ia (transaction-id (with-transaction (tm-a)
                                            *transaction*)))
                      (ib (transaction-id (with-transaction (tm-b)
                                            *transaction*)))
                      (ia2 (transaction-id (with-transaction (tm-a)
                                             *transaction*))))
                 (is (= 1 ia ib))
                 (is (= 2 ia2)))
            (close-graph ga)
            (close-graph gb)))))))

(test attaching-a-store-raises-the-clock-above-its-history
  ;; The watermark: a store with existing history must not hand the clock a
  ;; reason to reissue an epoch that store already used.
  (with-temp-directory (cdir)
    (with-temp-directory (gdir)
      (let ((g (make-graph :sc-eps (namestring gdir) :buffer-pool-size 1000)))
        (dotimes (i 5)
          (with-transaction ((graph-db::transaction-manager g)) t))
        (let ((highest (load-highest-transaction-id g)))
          (close-graph g)
          (let ((clock (open-system-clock (namestring cdir))))
            (unwind-protect
                 (let ((g2 (open-graph :sc-eps (namestring gdir)
                                       :buffer-pool-size 1000
                                       :system-clock clock)))
                   (unwind-protect
                        (progn
                          (is (> (clock-current-epoch clock) highest))
                          ;; The property the watermark exists for: the
                          ;; NEXT id this store actually allocates must
                          ;; exceed its own history, not just leave the
                          ;; clock's internal counter above it (which
                          ;; would also catch an attach that raced ahead
                          ;; of its own watermark -- fix 2 in the review).
                          (let ((tm-2 (graph-db::transaction-manager g2)))
                            (is (> (transaction-id (with-transaction (tm-2)
                                                     *transaction*))
                                   highest))))
                     (close-graph g2)))
              (close-system-clock clock))))))))

(test start-and-finish-tx-id-bracket-the-shared-epoch
  ;; The gap that matters most (reviewer finding): an implementation that
  ;; drew only the COMMIT id from the clock while leaving start-tx-id and
  ;; finish-tx-id on the local counter would pass every test above and
  ;; still corrupt this store's overlap window (OVERLAPPING-TRANSACTIONS
  ;; filters committed transactions by (<= START (TRANSACTION-ID TX)
  ;; FINISH)).  Interleave commits across two stores sharing one clock and
  ;; check each transaction's own start/finish bracket its own
  ;; transaction-id -- proving all three come from the same sequence.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let ((ga (make-graph :sc-theta (namestring da)
                                     :buffer-pool-size 1000
                                     :system-clock clock))
                     (gb (make-graph :sc-iota (namestring db)
                                     :buffer-pool-size 1000
                                     :system-clock clock)))
                 (unwind-protect
                      (let ((tm-a (graph-db::transaction-manager ga))
                            (tm-b (graph-db::transaction-manager gb))
                            (txs '()))
                        (dotimes (i 3)
                          (push (with-transaction (tm-a) *transaction*) txs)
                          (push (with-transaction (tm-b) *transaction*) txs))
                        (dolist (tx txs)
                          (is (<= (graph-db::start-tx-id tx)
                                  (transaction-id tx)
                                  (graph-db::finish-tx-id tx)))))
                   (close-graph ga)
                   (close-graph gb)))))
        (close-system-clock clock)))))

(test attach-watermarks-a-peer-graphs-pull-cursor-too
  ;; Correctness (reviewer fix 1): a peer-graph's pull-cursor and its local
  ;; highest-transaction-id are DISTINCT number spaces (see
  ;; PEER-OBSERVE-EPOCH's docstring; tests/peer-lamport-tests.lisp confirms
  ;; they diverge).  ATTACH-TO-SYSTEM-CLOCK must watermark past whichever
  ;; is higher, or a node this device pulled at the hub's epoch becomes
  ;; MVCC-invisible to a subsequent local edit once the clock takes over.
  (with-temp-directory (cdir)
    (with-temp-directory (gdir)
      (let ((origin (make-array 16 :element-type '(unsigned-byte 8)
                                :initial-element 7)))
        (let ((g (make-graph :sc-lambda (namestring gdir)
                             :peer-role :device :origin-id origin
                             :peer-host "localhost" :replication-port 0
                             :buffer-pool-size 1000)))
          (let ((*graph* g))
            ;; Local feed-seq advances to 3 ...
            (dotimes (i 3)
              (with-transaction ((graph-db::transaction-manager g)) t)))
          ;; ... while the pull-cursor (a hub frontier) is far above it.
          (graph-db::persist-peer-pull-cursor 42 g)
          (close-graph g :snapshot-p nil))
        (let ((clock (open-system-clock (namestring cdir))))
          (unwind-protect
               (let ((g2 (open-graph :sc-lambda (namestring gdir)
                                     :peer-role :device :origin-id origin
                                     :peer-host "localhost" :replication-port 0
                                     :buffer-pool-size 1000
                                     :system-clock clock)))
                 (unwind-protect
                      (is (> (clock-current-epoch clock) 42))
                   (close-graph g2 :snapshot-p nil)))
            (close-system-clock clock)))))))

(test attach-does-not-half-apply-when-watermark-computation-fails
  ;; Ordering (reviewer fix 2): ATTACH-TO-SYSTEM-CLOCK must compute and
  ;; raise the watermark BEFORE it sets GRAPH-SYSTEM-CLOCK, so a failure
  ;; mid-attach leaves the graph un-attached rather than attached-but-
  ;; un-watermarked (a store whose epoch source is a clock not yet raised
  ;; above its own history -- and already findable via *GRAPHS* by any
  ;; other thread).  Corrupt the persisted highest-id file so
  ;; LOAD-HIGHEST-TRANSACTION-ID signals, and assert the slot never moved.
  (with-temp-directory (cdir)
    (with-temp-directory (gdir)
      (let ((g (make-graph :sc-kappa (namestring gdir) :buffer-pool-size 1000)))
        (with-transaction ((graph-db::transaction-manager g)) t)
        (unwind-protect
             (let ((clock (open-system-clock (namestring cdir))))
               (unwind-protect
                    (progn
                      (with-open-file
                          (s (graph-db::highest-transaction-id-file g)
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
                        (write-byte 0 s))
                      (signals error (attach-to-system-clock g clock))
                      (is (null (graph-system-clock g))))
                 (close-system-clock clock)))
          (close-graph g :snapshot-p nil))))))

(test attach-refuses-a-store-with-an-active-transaction
  ;; No coverage before this (GH #168 review): the quiescence guard in
  ;; ATTACH-TO-SYSTEM-CLOCK exists to prevent the FINISH-TX-ID skew
  ;; described in its docstring.  Attach from inside an open transaction
  ;; and confirm both the signal and that the graph is left un-attached.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (gdir)
             (let ((g (make-graph :sc-quiescence (namestring gdir)
                                  :buffer-pool-size 1000)))
               (unwind-protect
                    (progn
                      (with-transaction ((graph-db::transaction-manager g))
                        (signals attach-with-active-transactions
                          (attach-to-system-clock g clock)))
                      (is (null (graph-system-clock g))))
                 (close-graph g :snapshot-p nil))))
        (close-system-clock clock)))))

(test recreate-graph-allocates-from-the-image-clock
  ;; The audit finding (GH #168): RECREATE-GRAPH minted ids from a per-store
  ;; scalar, so under a shared clock it reissued epochs another store had
  ;; already used.  Drives the real path: SNAPSHOT then REPLAY.  (The plan's
  ;; sketch reached the source graph via LOOKUP-GRAPH post-close and via
  ;; PERSISTENT-TRANSACTION-DIRECTORY, neither of which line up with how
  ;; CLOSE-GRAPH deregisters or where SNAPSHOT actually writes; and empty
  ;; transaction bodies leave nothing in the snapshot for REPLAY to
  ;; allocate ids against.  Fixed by reusing backup-tests.lisp's txn-log/
  ;; convention and g-person schema, both already loaded by graph-tests.lisp.
  ;; The source and destination graphs share *INTEGRATION-GRAPH-NAME* -- as
  ;; many other tests do sequentially -- because that is the schema key
  ;; G-PERSON is registered under; they never overlap while open.
  ;;
  ;; RECORD-COUNT exceeds *RESTORE-OBJECTS-PER-TRANSACTION* (10) so REPLAY
  ;; spans several batches -- several allocator calls, not one -- and the
  ;; clock's ADVANCE (not just a floor on the persisted id) is asserted:
  ;; a TM-CURRENT-EPOCH-for-TM-NEXT-EPOCH substitution peeks the same
  ;; floor without moving the clock, which a floor-only check cannot see
  ;; but an exact advance-by-BATCHES check does.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir)))
          (record-count 25))
      (unwind-protect
           (with-temp-directory (sdir)
             (with-temp-directory (odir)
               ;; Source store: enough real history to span several REPLAY
               ;; batches, then a snapshot on disk.
               (let ((src (make-graph *integration-graph-name* (namestring sdir)
                                      :buffer-pool-size 1000
                                      :system-clock clock)))
                 (let ((*graph* src))
                   (dotimes (i record-count)
                     (with-transaction ((graph-db::transaction-manager src))
                       (make-g-person :name "restore-probe" :age i)))
                   (graph-db::snapshot src))
                 (close-graph src :snapshot-p nil))
               ;; A second store burns epochs, pushing the clock far past
               ;; anything the restore target's own scalar knows about.
               (let ((other (make-graph :sc-other (namestring odir)
                                        :buffer-pool-size 1000
                                        :system-clock clock)))
                 (dotimes (i 50)
                   (with-transaction ((graph-db::transaction-manager other))
                     t))
                 (let* ((per graph-db::*restore-objects-per-transaction*)
                        (floor-epoch (clock-current-epoch clock))
                        (batches (ceiling record-count per)))
                   (with-temp-directory (rdir)
                     (let ((dst (make-graph *integration-graph-name*
                                            (namestring rdir)
                                            :buffer-pool-size 1000
                                            :system-clock clock)))
                       (graph-db::replay
                        dst
                        (merge-pathnames "txn-log/" sdir)
                        :graph-db/test)
                       ;; Every id the replay issued sits above the clock's
                       ;; position when it started ...
                       (is (>= (graph-db::load-highest-transaction-id dst)
                               floor-epoch))
                       ;; ... and the clock advanced by exactly one epoch
                       ;; per batch -- the property TM-NEXT-EPOCH provides
                       ;; and a peek-only TM-CURRENT-EPOCH does not.
                       (is (= batches
                              (- (clock-current-epoch clock) floor-epoch)))
                       (close-graph dst :snapshot-p nil))))
                 (close-graph other :snapshot-p nil))))
        (close-system-clock clock)))))

(defparameter *observe-epoch-origin*
  (make-array 16 :element-type '(unsigned-byte 8) :initial-element 9)
  "A fixed device origin id for the PEER-OBSERVE-EPOCH tests.")

(test peer-observe-epoch-raises-the-image-clock
  ;; PEER-OBSERVE-EPOCH exists so a pulled node's HUB epoch can't outrun this
  ;; store's next start-tx-id (see PEER-OBSERVE-EPOCH's docstring).  With a
  ;; clock bound, TX-ID-COUNTER is dead -- TM-NEXT-EPOCH/TM-CURRENT-EPOCH
  ;; route to the clock (GH #168) -- so the observation must land there too.
  ;; Also pins monotonicity in both directions: a high epoch raises the
  ;; clock, and a later LOWER epoch is a no-op, not a regression.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (gdir)
             (let ((g (make-graph :sc-peer (namestring gdir)
                                  :buffer-pool-size 1000
                                  :peer-role :device
                                  :origin-id *observe-epoch-origin*
                                  :system-clock clock)))
               (unwind-protect
                    (progn
                      (graph-db::peer-observe-epoch g 999999)
                      (is (> (clock-current-epoch clock) 999999)))
                 (close-graph g :snapshot-p nil))))
        (close-system-clock clock)))))

(test peer-observe-epoch-does-not-drag-the-clock-backwards
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (gdir)
             (let ((g (make-graph :sc-peer-lo (namestring gdir)
                                  :buffer-pool-size 1000
                                  :peer-role :device
                                  :origin-id *observe-epoch-origin*
                                  :system-clock clock)))
               (unwind-protect
                    (progn
                      (graph-db::peer-observe-epoch g 999999)
                      (let ((raised (clock-current-epoch clock)))
                        (graph-db::peer-observe-epoch g 5)
                        (is (= raised (clock-current-epoch clock)))))
                 (close-graph g :snapshot-p nil))))
        (close-system-clock clock)))))

(test peer-observe-epoch-ignores-the-dead-counter
  ;; The nearest wrong implementation: route to the clock but keep the old
  ;; `(>= epoch (tx-id-counter tm))' guard.  Under a clock TX-ID-COUNTER is
  ;; dead -- nothing ever advances it -- so gating on it either always
  ;; passes (masking the bug for a fresh store, where the guard is
  ;; vacuously true) or, once something has left a stale high value in the
  ;; slot, wrongly SKIPS the clock observation.  Force that stale value
  ;; directly and confirm the clock still raises regardless.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (gdir)
             (let ((g (make-graph :sc-peer-stale (namestring gdir)
                                  :buffer-pool-size 1000
                                  :peer-role :device
                                  :origin-id *observe-epoch-origin*
                                  :system-clock clock)))
               (unwind-protect
                    (progn
                      (setf (graph-db::tx-id-counter
                             (graph-db::transaction-manager g))
                            5000000)
                      (graph-db::peer-observe-epoch g 999999)
                      (is (> (clock-current-epoch clock) 999999)))
                 (close-graph g :snapshot-p nil))))
        (close-system-clock clock)))))

;;; Cross-store read snapshots pin every participating store (GH #168 task 6).
;;; See spec sec.6: a long cross-store query delays reaping in EVERY store it
;;; touched -- the intended trade, not a regression.

(test cross-store-snapshot-pins-every-store
  ;; Identity, not just presence (review round 2): a hypothetical swapped
  ;; implementation -- GA's snapshot pinning TM-B, GB's pinning TM-A --
  ;; would still pass a count-only check, since both tables end up with
  ;; one entry each.  Under a SHARED clock the two managers' epochs are
  ;; usually the same number, so a value comparison alone can't tell them
  ;; apart -- unless something advances the clock between the two
  ;; snapshots' establishment.  A real write on GB before GB's own
  ;; snapshot does exactly that, so GA's and GB's own pin-time epochs
  ;; diverge and a swap becomes visible as a value mismatch.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let* ((ga (make-graph :sc-pin-a (namestring da)
                                      :buffer-pool-size 1000
                                      :system-clock clock))
                      (gb (make-graph :sc-pin-b (namestring db)
                                      :buffer-pool-size 1000
                                      :system-clock clock))
                      (tm-a (graph-db::transaction-manager ga))
                      (tm-b (graph-db::transaction-manager gb)))
                 (flet ((pin-values (tm)
                          (loop for v being the hash-values
                                  of (graph-db::read-pins tm)
                                collect v)))
                   (unwind-protect
                        (graph-db:with-read-snapshot (ga)
                          (let ((epoch-a (graph-db::tm-peek-epoch tm-a)))
                            ;; Bumps the shared clock before GB pins.
                            (with-transaction (tm-b) t)
                            (graph-db:with-read-snapshot (gb)
                              (let ((epoch-b
                                      (graph-db::tm-peek-epoch tm-b)))
                                ;; Both managers hold a pin.
                                (is (plusp (hash-table-count
                                            (graph-db::read-pins tm-a))))
                                (is (plusp (hash-table-count
                                            (graph-db::read-pins tm-b))))
                                ;; Each table's value is its OWN
                                ;; manager's pin-time epoch -- not the
                                ;; other store's.
                                (is (equal (list epoch-a)
                                           (pin-values tm-a)))
                                (is (equal (list epoch-b)
                                           (pin-values tm-b)))))))
                     (close-graph ga)
                     (close-graph gb))))))
        (close-system-clock clock)))))

(test cross-store-snapshot-releases-pins-after-normal-return
  ;; A pin taken and never released wedges the reaper forever in the store it
  ;; leaked on -- worse than the visibility bug this unit fixes.  Assert both
  ;; stores' pins are gone once the composed snapshot exits normally.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let* ((ga (make-graph :sc-pin-c (namestring da)
                                      :buffer-pool-size 1000
                                      :system-clock clock))
                      (gb (make-graph :sc-pin-d (namestring db)
                                      :buffer-pool-size 1000
                                      :system-clock clock))
                      (tm-a (graph-db::transaction-manager ga))
                      (tm-b (graph-db::transaction-manager gb)))
                 (unwind-protect
                      (progn
                        (graph-db:with-read-snapshot (ga)
                          (graph-db:with-read-snapshot (gb) t))
                        (is (zerop (hash-table-count
                                    (graph-db::read-pins tm-a))))
                        (is (zerop (hash-table-count
                                    (graph-db::read-pins tm-b)))))
                   (close-graph ga)
                   (close-graph gb)))))
        (close-system-clock clock)))))

(test cross-store-snapshot-releases-pins-on-non-local-exit
  ;; Same property, but the body THROWs out instead of returning -- this is
  ;; the whole point of putting the release in UNWIND-PROTECT.
  (with-temp-directory (cdir)
    (let ((clock (open-system-clock (namestring cdir))))
      (unwind-protect
           (with-temp-directory (da)
             (with-temp-directory (db)
               (let* ((ga (make-graph :sc-pin-e (namestring da)
                                      :buffer-pool-size 1000
                                      :system-clock clock))
                      (gb (make-graph :sc-pin-f (namestring db)
                                      :buffer-pool-size 1000
                                      :system-clock clock))
                      (tm-a (graph-db::transaction-manager ga))
                      (tm-b (graph-db::transaction-manager gb)))
                 (unwind-protect
                      (progn
                        (catch 'bail-out
                          (graph-db:with-read-snapshot (ga)
                            (graph-db:with-read-snapshot (gb)
                              (throw 'bail-out nil))))
                        (is (zerop (hash-table-count
                                    (graph-db::read-pins tm-a))))
                        (is (zerop (hash-table-count
                                    (graph-db::read-pins tm-b)))))
                   (close-graph ga)
                   (close-graph gb)))))
        (close-system-clock clock)))))

;;; GH #182: two images on one clock directory both issued epochs, silently.

(test second-open-of-a-held-clock-signals
  "The whole point: a second allocator on one system directory destroys the
single property the clock provides."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (signals graph-db:system-clock-in-use
             (open-system-clock (namestring dir)))
        (close-system-clock c)))))

(test a-refusal-names-the-directory-it-refused
  "The operator needs to know WHICH system directory is held.

Named for what it asserts.  It does not measure timing: LOCK_NB is what stops
a second open blocking forever, and that is proven by this suite terminating
at all -- a blocking flock would hang here rather than fail.  What is NOT
test-enforced is *immediate* versus merely bounded: a short retry loop would
still pass.  Catching that needs a bound tight enough to be flaky on a loaded
machine, which costs more than it buys (GH #182)."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (let ((loc (handler-case
                          (progn (open-system-clock (namestring dir)) nil)
                        (graph-db:system-clock-in-use (e)
                          (graph-db:system-clock-in-use-location e)))))
             (is (equal (namestring dir) loc)
                 "the refusal names the directory it refused"))
        (close-system-clock c)))))

(test a-closed-clock-can-be-reopened
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (close-system-clock c))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect (is-true c2 "the lock was released by the clean close")
        (close-system-clock c2)))))

(test leasing-works-while-the-lock-is-held
  "A lease-holder is inside the owning image (spec §8.1), so the guard must not
break the #170 path."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (multiple-value-bind (start end) (clock-lease-epochs c 100)
             (is (= 100 (- end start)))
             (is (>= (clock-current-epoch c) end)
                 "the clock skipped past the leased range"))
        (close-system-clock c)))))

(test epochs-stay-monotonic-across-a-close-and-reopen
  "The lock must not disturb the ceiling protocol."
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir)))
           (a (clock-next-epoch c)))
      (close-system-clock c)
      (let ((c2 (open-system-clock (namestring dir))))
        (unwind-protect
             (is (> (clock-next-epoch c2) a)
                 "a reopened clock never reissues")
          (close-system-clock c2))))))
