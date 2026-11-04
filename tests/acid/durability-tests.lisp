;;;; ACID-DURABILITY-SUITE
;;;;
;;;; Verifies that committed data survives close/reopen cycles and that the
;;;; .dirty crash sentinel is set and checked correctly.  The crash-after-
;;;; partial-apply test uses *after-apply-tx-writes-hook* to simulate a process
;;;; death between the lhash write and the view update, then verifies that
;;;; open-graph's automatic recover-transactions call makes the graph consistent.

(in-package #:graph-db/acid-test)

(def-suite acid-durability-suite
  :description "Durability: crash sentinel, close/reopen, and WAL recovery."
  :in acid-suite)

(in-suite acid-durability-suite)

;;; ---------------------------------------------------------------------------
;;; Test 1: dirty flag detected on reopen
;;;
;;; make-graph writes a .dirty marker.  While the graph is still open (not
;;; closed), calling open-graph on the same path must signal an error — the
;;; system correctly refuses to open an unclean graph.
;;; ---------------------------------------------------------------------------

(test dirty-flag-detected-on-reopen
  "open-graph must signal an error when a .dirty marker is present."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          g)
      (unwind-protect
           (progn
             (setq g (make-graph *acid-graph-name* path :buffer-pool-size 1000))
             ;; Graph is open; .dirty exists.  Attempting to open the same path
             ;; must fail.
             (let ((*graph* g))
               (signals error
                 (open-graph *acid-graph-name* path))))
        (when g
          (ignore-errors (close-graph g :snapshot-p nil)))
        (collect-garbage)))))

;;; ---------------------------------------------------------------------------
;;; Test 2: data survives clean close and reopen
;;;
;;; Commit a vertex and an edge, close the graph cleanly, then reopen it.
;;; Both the vertex (with its slot values) and the edge (with its adjacency
;;; information) must be present and correct after the round-trip.
;;; ---------------------------------------------------------------------------

(test data-survives-close-reopen
  "Committed data (vertex slot values, edge adjacency) survives close-graph + open-graph."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          src-id dst-id)
      ;; --- populate and close ---
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (let ((*graph* g))
          (with-transaction ()
            (let ((src (make-ac-item :value 42 :label "src"))
                  (dst (make-ac-item :value 7  :label "dst")))
              (setq src-id (id src)
                    dst-id (id dst))
              (make-ac-link :from src :to dst)))
          (close-graph g :snapshot-p nil)))
      ;; --- reopen and verify ---
      (let ((g2 (open-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               ;; Vertex slot values
               (let ((src (lookup-vertex src-id)))
                 (is-true src "Source vertex must be present after reopen")
                 (is (= 42 (slot-value src 'value)) "src.value must be 42")
                 (is (string= "src" (slot-value src 'label)) "src.label must be \"src\""))
               (let ((dst (lookup-vertex dst-id)))
                 (is-true dst "Destination vertex must be present after reopen")
                 (is (= 7 (slot-value dst 'value)) "dst.value must be 7"))
               ;; Edge adjacency
               (is (= 1 (length (map-edges #'identity g2
                                           :collect-p t :edge-type 'ac-link)))
                   "Edge must survive reopen"))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Test 3: crash after partial apply — recovery makes graph consistent
;;;
;;; *after-apply-tx-writes-hook* is set to signal an error after apply-tx-writes
;;; runs (lhash updated, heap written, type-index updated) but before
;;; apply-tx-writes-to-views (view index not updated).  The .txn file is left
;;; on disk because cleanup-transaction sees state :committing (not :committed)
;;; and calls remove-transaction instead of mark-as-committed.
;;;
;;; The graph is then closed cleanly (flushing on-disk structures, removing
;;; .dirty).  open-graph automatically calls recover-transactions, which replays
;;; the .txn file: apply-tx-writes is idempotent (duplicate-key-error handled),
;;; and apply-tx-writes-to-views finally updates the view.
;;;
;;; After recovery the vertex must be in the lhash AND the view must be
;;; consistent (vertex appears in the view).
;;; ---------------------------------------------------------------------------

(test crash-after-partial-apply
  "Recovery via recover-transactions makes a partial-apply consistent: lhash and views agree."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          vid)
      ;; --- Phase 1: create graph, arm the crash hook, commit ---
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (define-acid-views)
               ;; Set the hook: fires once after apply-tx-writes, before apply-tx-writes-to-views
               (setf *after-apply-tx-writes-hook*
                     (lambda () (error "simulated crash after lhash write")))
               ;; The transaction will succeed at the lhash level but error at the view level.
               ;; The error propagates to the caller; the .txn file is NOT deleted.
               (handler-case
                   (with-transaction ()
                     (let ((v (make-ac-item :value 77 :label "durable")))
                       (setq vid (id v))))
                 (error () nil))
               ;; Verify the hook self-cleared
               (is (null *after-apply-tx-writes-hook*)
                   "Hook must have self-cleared after firing")
               ;; The crash fired inside the lock, after the rename and
               ;; before the post-lock id patch: the window recovery
               ;; must accept (#422).
               (let ((files (directory (merge-pathnames "tx/*.txn" path))))
                 (is (= 1 (length files)) "one .txn file left behind")
                 (when files
                   (is (= 0 (graph-db::transaction-id
                             (graph-db::load-tx-file (first files))))
                       "a crash inside the lock leaves the header at 0; ~
recovery trusts the filename (#422)"))))
          ;; Close the graph cleanly: flushes on-disk structures, removes .dirty
          ;; The .txn file in tx/ is NOT affected by close-graph.
          (ignore-errors (close-graph g :snapshot-p nil))
          ;; Clear the hook in case of unexpected error paths
          (setf *after-apply-tx-writes-hook* nil)))
      ;; --- Phase 2: open-graph runs recover-transactions automatically ---
      (let ((g2 (open-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               ;; The vertex must be in the lhash (was there before crash, and replay is idempotent)
               (let ((v (lookup-vertex vid)))
                 (is-true v "Vertex must be in lhash after recovery")
                 (when v
                   (is (= 77 (slot-value v 'value))
                       "Vertex slot value must be 77 after recovery")))
               ;; The view must now be consistent (replay applied apply-tx-writes-to-views)
               (let ((view-hits (map-view #'list 'ac-item 'ac-item-by-value
                                         :graph g2
                                         :collect-p t
                                         :start-key 77 :end-key 77)))
                 (is (= 1 (length view-hits))
                     "View must contain the recovered vertex (got ~D entries)" (length view-hits))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; Test 4: an UPDATE's .txn log carries the NEW data, so recovery restores it.
;;;
;;; update-node serializes the write from the copy's BYTES; mutating a slot
;;; updates DATA but not BYTES, so without refreshing bytes the logged write
;;; carried the OLD value.  The live graph hid this (apply-tx-write
;;; re-serializes its own copy), but replaying the .txn during recovery would
;;; restore the stale value.  Crash mid-update (after apply-tx-writes, before
;;; the .txn is cleaned), then recover and confirm the NEW value survives.
;;; ---------------------------------------------------------------------------

(test update-survives-crash-recovery
  "Recovery replays an update's .txn with the NEW slot values, not the stale
copy bytes (regression for update-node serializing a copy's pre-modification
bytes)."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          vid)
      ;; --- Phase 1: commit an insert cleanly, then crash mid-UPDATE ---
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (setq vid (id (make-ac-item :value 1 :label "before"))))
               ;; Crash after apply-tx-writes so the update's .txn is left
               ;; pending for recovery to replay.
               (setf *after-apply-tx-writes-hook*
                     (lambda () (error "simulated crash after lhash write")))
               (handler-case
                   (with-transaction ()
                     (let ((v (copy (lookup-vertex vid))))
                       (setf (slot-value v 'value) 2)
                       (setf (slot-value v 'label) "after")
                       (save v)))
                 (error () nil))
               (is (null *after-apply-tx-writes-hook*)
                   "Hook must have self-cleared after firing"))
          (ignore-errors (close-graph g :snapshot-p nil))
          (setf *after-apply-tx-writes-hook* nil)))
      ;; --- Phase 2: reopen -> recover-transactions replays the update .txn ---
      (let ((g2 (open-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (let ((v (lookup-vertex vid)))
                 (is-true v "Updated vertex must be present after recovery")
                 (when v
                   (is (= 2 (slot-value v 'value))
                       "Recovered update must hold the NEW value 2, not the stale 1")
                   (is (string= "after" (slot-value v 'label))
                       "Recovered update must hold the NEW label"))))
          (ignore-errors (close-graph g2 :snapshot-p nil))
          (collect-garbage))))))

;;; ---------------------------------------------------------------------------
;;; #422: a .txn file's header carries its id once the commit lock is
;;; released; recovery cross-checks it against the filename.
;;; ---------------------------------------------------------------------------

(test recovery-refuses-a-renamed-transaction-file
  "A .txn file whose nonzero header id differs from its filename is
tampered with or misplaced; recovery refuses it by name rather than
replaying it under the filename's id (#422)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (close-graph g :snapshot-p nil))
      ;; A hand-made file: header id 7, filename says 9, no writes.
      ;; Layout per SERIALIZE-TX-HEADER: size at 0, flags at 8, type at 9,
      ;; id at 10, write count at 18, write size at 26.
      (let* ((file (make-pathname :name (format nil "~16,'0X" 9)
                                  :type "txn"
                                  :defaults (merge-pathnames "tx/" path)))
             (header (graph-db::make-tx-header-vector))
             (signalled nil))
        (graph-db::serialize-uint64 header graph-db::+tx-header-size+ 0)
        (setf (aref header 9) graph-db::+tx-header-type-code+)
        (graph-db::serialize-uint64 header 7 graph-db::+tx-header-id-offset+)
        (with-open-file (s file :direction :output
                                :element-type '(unsigned-byte 8))
          (write-sequence header s))
        ;; One open only: a refused open may leave .dirty behind, and a
        ;; second attempt would then fail for that reason instead.
        (handler-case
            (let ((g (open-graph *acid-graph-name* path)))
              (close-graph g :snapshot-p nil))
          (graph-db:transaction-file-id-mismatch (c)
            (setf signalled t)
            (is (= 7 (graph-db::header-id c)))
            (is (= 9 (graph-db::filename-id c)))
            (is (search "0000000000000009" (princ-to-string c))
                "the report names the file")))
        (is-true signalled
                 "open-graph must refuse the mismatched file (#422)")))))

(test a-committed-transaction-file-carries-its-id
  "After commit the .txn file's header id equals its filename's id,
written after the manager lock is released; the patch precedes the
rename to .committed (#422)."
  (with-temp-directory (dir)
    (let ((graph-db::*delete-committed-transaction-files* nil)
          (path (namestring dir)))
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (define-acid-views)
               (with-transaction ()
                 (make-ac-item :value 1 :label "patched"))
               (let ((files (directory
                             (merge-pathnames "tx/*.committed" path))))
                 (is (= 1 (length files)) "one .committed file")
                 (when files
                   (let ((file (first files)))
                     (is (= (parse-integer (pathname-name file) :radix 16)
                            (graph-db::transaction-id
                             (graph-db::load-tx-file file)))
                         "header id must equal the filename id")))))
          (close-graph g :snapshot-p nil))))))

(test a-memory-graph-journal-entry-carries-its-id
  "The retained journal of a memory graph is patched too, and the
patched entry still replays on the next open (#422)."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (vid nil))
      (let ((g (graph-db::make-memory-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (setq vid (id (make-ac-item :value 2 :label "journaled"))))
               (let ((files (directory (merge-pathnames "tx/*.txn" path))))
                 (is (= 1 (length files)) "one retained journal entry")
                 (when files
                   (let ((file (first files)))
                     (is (= (parse-integer (pathname-name file) :radix 16)
                            (graph-db::transaction-id
                             (graph-db::load-tx-file file)))
                         "journal header id must equal the filename id")))))
          (ignore-errors (close-graph g :snapshot-p nil))))
      ;; No checkpoint was written, so the open replays the journal.
      (let ((g2 (graph-db::open-memory-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g2))
               (is-true (lookup-vertex vid)
                        "the patched journal entry replays on open"))
          (ignore-errors (close-graph g2 :snapshot-p nil)))))))

(test a-wal-suppressed-commit-patches-nothing
  "No file, no patch: a WAL-suppressed graph commits without a .txn
file and without an error from the patch step (#422)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *acid-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (setf (graph-db::wal-suppressed-p g) t)
               (finishes
                 (with-transaction ()
                   (make-ac-item :value 3 :label "unjournaled")))
               (is (null (directory (merge-pathnames "tx/*.txn" path)))
                   "no .txn file")
               (is (null (directory (merge-pathnames "tx/*.committed" path)))
                   "no .committed file"))
          (setf (graph-db::wal-suppressed-p g) nil)
          (close-graph g :snapshot-p nil))))))

(test a-patch-failure-after-the-commit-point-propagates
  "The file is made read-only inside the lock, after the rename and
before the patch.  The commit is durable, applied and :COMMITTED, the
file is marked committed with header 0, and the caller gets
TRANSACTION-FILE-PATCH-ERROR, which says so (#422)."
  (if (string= "0" (string-trim '(#\Newline #\Space)
                                 (uiop:run-program '("id" "-u")
                                                   :output :string)))
      (skip "root ignores a file's mode; the patch cannot be made to fail")
      (with-temp-directory (dir)
        (let ((graph-db::*delete-committed-transaction-files* nil)
              (path (namestring dir))
              (captured nil)
              (txfile nil)
              (vid nil))
          (let ((g (make-graph *acid-graph-name* path
                               :buffer-pool-size 1000)))
            (unwind-protect
                 (let ((*graph* g))
                   (setf *after-apply-tx-writes-hook*
                         (lambda ()
                           (setf captured graph-db::*transaction*
                                 txfile (graph-db::transaction-pathname
                                         captured))
                           (uiop:run-program
                            (list "chmod" "0400" (namestring txfile)))))
                   (handler-case
                       (progn
                         (with-transaction ()
                           (setq vid (id (make-ac-item :value 4
                                                       :label "ro"))))
                         (fail "a patch failure must signal"))
                     (graph-db:transaction-file-patch-error (c)
                       (is (equal (namestring txfile)
                                  (namestring (graph-db::patch-file c))))
                       (is (search "is committed" (princ-to-string c))
                           "the report says the transaction is committed")))
                   (is (null *after-apply-tx-writes-hook*)
                       "Hook must have self-cleared after firing")
                   (is (eql :committed (graph-db::state captured))
                       "the transaction ends :COMMITTED, so it stays in the ~
manager's table for OCC until pruned")
                   (is-true (lookup-vertex vid)
                            "the transaction was applied before the patch")
                   (let ((committed (directory
                                     (merge-pathnames "tx/*.committed" path))))
                     (is (= 1 (length committed))
                         "cleanup marked the file committed")
                     (is (null (directory (merge-pathnames "tx/*.txn" path)))
                         "no .txn file is left for recovery")
                     (when committed
                       (is (= 0 (graph-db::transaction-id
                                 (graph-db::load-tx-file (first committed))))
                           "the unpatched file keeps header 0"))))
              (setf *after-apply-tx-writes-hook* nil)
              (dolist (f (directory (merge-pathnames "tx/*.*" path)))
                (uiop:run-program (list "chmod" "0600" (namestring f))))
              (ignore-errors (close-graph g :snapshot-p nil))))))))

(test a-journal-entry-removed-before-the-patch-does-not-fail-the-commit
  "CHECKPOINT-MEMORY-GRAPH may clear the journal between the rename and
the patch; a file that is gone has been superseded, and the commit
finishes without an error (#422)."
  (with-temp-directory (dir)
    (let ((path (namestring dir))
          (vid nil))
      (let ((g (graph-db::make-memory-graph *acid-graph-name* path)))
        (unwind-protect
             (let ((*graph* g))
               (setf *after-apply-tx-writes-hook*
                     (lambda ()
                       (delete-file (graph-db::transaction-pathname
                                     graph-db::*transaction*))))
               (finishes
                 (with-transaction ()
                   (setq vid (id (make-ac-item :value 5 :label "gone")))))
               (is (null *after-apply-tx-writes-hook*))
               (is-true (lookup-vertex vid) "applied all the same"))
          (setf *after-apply-tx-writes-hook* nil)
          (ignore-errors (close-graph g :snapshot-p nil)))))))
