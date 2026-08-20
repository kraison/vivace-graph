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
