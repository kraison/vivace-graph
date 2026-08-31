(in-package :graph-db)

;; CHECK-DATA-INTEGRITY-P defaults to NIL (GH #119): the sweep
;; deserializes every node a SECOND time -- measured as the larger half
;; of a snapshot's allocation -- for a check the snapshot itself does
;; not need.  CHECK-DATA-INTEGRITY is the explicit audit, and REPLAY
;; still verifies restored data by default.
(defmethod snapshot ((graph graph) &key include-deleted-p
                                     check-data-integrity-p)
  (let ((count nil))
    (with-recursive-lock-held ((txn-lock graph))
      (let ((problems (when check-data-integrity-p
                        (check-data-integrity graph
                                              :include-deleted-p
                                              include-deleted-p))))
        (if problems
            (progn
              (log:error "data integrity errors on ~A" graph)
              (dolist (problem problems)
                (log:error "data integrity: ~A" problem))
              (return-from snapshot
                (values :data-integrity-issues
                        problems)))
            (progn
              ;; Uniqueness comes from the UUID, never the clock (GH #100): the
              ;; old sec.usec name was NIL.NIL on ECL -- one constant filename
              ;; whose snapshots silently overwrote each other -- and even where
              ;; the clock works, two closes inside one microsecond collide.
              ;; FIND-NEWEST-SNAPSHOT orders by FILE-WRITE-DATE, not by name, so
              ;; the timestamp here is only for humans reading the directory.
              (let* ((snap-file (format nil "~A/txn-log/snap-~D-~A"
                                        (location graph) (get-universal-time)
                                        (uuid:make-v4-uuid)))
                     ;; Written in an in-progress/ SUBDIRECTORY -- where
                     ;; FIND-NEWEST-SNAPSHOT's scan and the historical
                     ;; "txn-log/snap-*" glob never look -- and renamed up
                     ;; only once BACKUP has written its completion
                     ;; trailer, so an interrupted snapshot never bears
                     ;; the snap- name at all (GH #127).  The partial
                     ;; keeps the SAME dot-free basename: RENAME-FILE
                     ;; merges a missing TYPE from the old pathname, so a
                     ;; dotted temp name ("in-progress.snap-X") comes out
                     ;; the other side as "snap-X.snap-X" -- matching the
                     ;; ^snap- regex but not the type-less glob every
                     ;; operator script uses.  Measured, not guessed.
                     ;; The partial is deleted on the way out; a crash
                     ;; that skips even that leaves it inert in the
                     ;; subdirectory.
                     (tmp-file (format nil "~A/txn-log/in-progress/~A"
                                       (location graph)
                                       (file-namestring snap-file)))
                     (renamed nil))
                (ensure-directories-exist tmp-file)
                (unwind-protect
                     (progn
                       (setq count (backup graph tmp-file
                                           :include-deleted-p
                                           include-deleted-p))
                       (rename-file tmp-file snap-file)
                       (setq renamed t))
                  (when (and (not renamed) (probe-file tmp-file))
                    (ignore-errors (delete-file tmp-file)))))
              count))))))

(define-condition snapshot-refused-warning (warning)
  ((file :initarg :file :reader snapshot-refused-file)
   (reason :initarg :reason :reader snapshot-refused-reason))
  (:report
   (lambda (c s)
     (ecase (snapshot-refused-reason c)
       (:truncated
        (format s "Snapshot ~A carries the format header but no ~
                   completion trailer -- it was cut short and is ~
                   REFUSED; trying the next newest (GH #127)."
                (snapshot-refused-file c)))
       (:legacy
        (format s "Snapshot ~A predates the completion trailer (GH ~
                   #127) and cannot be verified; restoring it anyway.  ~
                   Re-snapshot after restoring to get a verifiable file."
                (snapshot-refused-file c)))))))

(defun %snapshot-completeness (file)
  ":COMPLETE, :TRUNCATED (header but no trailer -- a modern file cut
short) or :LEGACY (no header -- written before GH #127, unverifiable).
Reads the first and last few hundred BYTES only: node data may hold any
characters, but both marker lines are pure ASCII, so a byte search is
safe where a character tail-seek into multi-byte data is not."
  (flet ((ascii-bytes (string)
           (map 'vector #'char-code string))
         (subseq-search (needle hay)
           (search needle hay :test #'=)))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (let* ((len (file-length in))
             (head (make-array (min len 64)
                               :element-type '(unsigned-byte 8)))
             (tail (make-array (min len 512)
                               :element-type '(unsigned-byte 8))))
        (read-sequence head in)
        (file-position in (- len (length tail)))
        (read-sequence tail in)
        (cond ((not (subseq-search (ascii-bytes "(:SNAPSHOT-HEADER")
                                   head))
               :legacy)
              ((subseq-search (ascii-bytes "(:SNAPSHOT-COMPLETE") tail)
               :complete)
              (t :truncated))))))

(defun find-newest-snapshot (dir)
  "The newest COMPLETE snapshot in DIR, by FILE-WRITE-DATE, as
 (values FILE WRITE-DATE).  A modern file cut short -- header but no
completion trailer -- is refused with SNAPSHOT-REFUSED-WARNING and the
next newest tried: restoring it would silently lose everything after
the cut and report success (GH #127, the #146 chain).  A legacy file
(no header) cannot be verified and is accepted with the same warning's
:LEGACY reason."
  (let ((candidates (sort (remove-if-not
                           (lambda (file)
                             (cl-ppcre:scan "^snap-"
                                            (file-namestring file)))
                           (cl-fad:list-directory dir))
                          '> :key 'file-write-date)))
    (dolist (file candidates)
      (ecase (%snapshot-completeness file)
        (:complete
         (return-from find-newest-snapshot
           (values file (file-write-date file))))
        (:legacy
         (warn 'snapshot-refused-warning :file file :reason :legacy)
         (return-from find-newest-snapshot
           (values file (file-write-date file))))
        (:truncated
         (warn 'snapshot-refused-warning :file file
               :reason :truncated))))))

(defmethod replay ((graph graph) txn-dir package-name &key (check-integrity-p t))
  (let ((snapshot (find-newest-snapshot txn-dir)))
    (when snapshot
      (recreate-graph graph snapshot :package-name package-name))
    (log:debug "Generating graph views.")
    (map nil
         (lambda (pair)
           (destructuring-bind (class-name . view-name) pair
             (regenerate-view graph class-name view-name)))
         (all-views graph))
    (log:debug "Checking data integrity.")
    (if check-integrity-p
        (check-data-integrity graph)
        graph)))
