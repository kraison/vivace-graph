(ql:quickload :graph-db)
(in-package :graph-db)

(progn
  (defun profile-lhash ()
    (sb-profile:profile
     make-mpointer
     make-byte-vector
     get-buffer
     release-buffer
     extend-mapped-file
     set-lhash-next-split
     read-lhash-next-split
     incf-lhash-next-split
     set-lhash-next-overflow-pointer
     read-lhash-next-overflow-pointer
     incf-lhash-next-overflow-pointer
     set-lhash-count
     read-lhash-count
     incf-lhash-count
     decf-lhash-count
     set-lhash-level
     read-lhash-level
     incf-lhash-level
     lookup-lhash-lock
     release-lhash-lock
     grab-lhash-lock
     lhash-insert %hash load-factor hash hash0
     serialize-key serialize-value
     deserialize-key deserialize-value
     acquire-overflow-bucket bucket-offset
     read-overflow-offset get-overflow-offset
     add-to-bucket read-from-bucket
     remove-from-bucket read-bucket
     clear-bucket rehash-bucket split-lhash
     lhash-get lhash-remove serialized-equal
     uuid-array-equal))

  (defun unprofile-lhash ()
    (sb-profile:unprofile
     make-mpointer
     make-byte-vector
     get-buffer
     release-buffer
     extend-mapped-file
     set-lhash-next-split
     read-lhash-next-split
     incf-lhash-next-split
     set-lhash-next-overflow-pointer
     read-lhash-next-overflow-pointer
     incf-lhash-next-overflow-pointer
     set-lhash-count
     read-lhash-count
     incf-lhash-count
     decf-lhash-count
     set-lhash-level
     read-lhash-level
     incf-lhash-level
     lookup-lhash-lock
     release-lhash-lock
     grab-lhash-lock
     lhash-insert %hash load-factor hash hash0
     serialize-key serialize-value
     deserialize-key deserialize-value
     acquire-overflow-bucket bucket-offset
     read-overflow-offset get-overflow-offset
     add-to-bucket read-from-bucket
     remove-from-bucket read-bucket
     clear-bucket rehash-bucket split-lhash
     lhash-get lhash-remove serialized-equal
     uuid-array-equal))
  (let ((random-states (list (make-random-state)
                             (progn (sleep 1) (make-random-state)))))
    (defun gen-test-id ()
      ;; GETTIMEOFDAY returns (VALUES SEC USEC) on every implementation since
      ;; GH #100; it used to return a single fractional-second rational on SBCL.
      (let* ((now (format nil "~,6F~6D~' :@/graph-db::print-byte-array/"
                          (multiple-value-bind (sec usec) (gettimeofday)
                            (coerce (+ sec (/ usec 1000000)) 'double-float))
                          (random 1000000 (nth (random 2) random-states))
                          (get-random-bytes 16))))
        now)))

  (defun analyze-lhash-test ()
    (flet ((lnow ()
             (multiple-value-bind (sec msec) (osicat-posix:gettimeofday)
               (+ sec (/ msec 1000000)))))
      (let ((lhash (make-lhash :test 'uuid-array-equal
                               :location "/var/tmp/lhash/"
                               :buckets 8))
            (table nil))
        (dbg "~A" lhash)
        (unwind-protect
             (let* ((start (lnow)) (cycle-start (lnow)))
               (dotimes (i 1000000)
                 (when (zerop (mod i 100))
                   (let ((time (- (lnow) cycle-start)))
                     (dbg "~A (~F)" i time))
                   (setq cycle-start (lnow)))
                 (let ((uuid (gen-vertex-id)) ;;(gen-id))
                       (value (random sb-ext:most-positive-word)))
                   ;;(dbg "~%ADDING ~A / ~A" uuid value)
                   (push (cons uuid value) table)
                   (lhash-insert lhash uuid value)))
               (let ((total-time (- (lnow) start)))
                 (dbg "done in ~F seconds" total-time)
;                 (dolist (pair (nreverse table))
;                   ;;(dbg "~%LOOKING FOR ~A" pair)
;                   (let ((v2 (lhash-get lhash (car pair))))
;                     (unless (eql (cdr pair) v2)
;                       (dbg "GOT WRONG VALUE FOR ~A. GOT ~A, SHOULD BE ~A"
;                            (car pair) v2 (cdr pair)))))
                 (let ((result 0) (bucket-count (bucket-count lhash)))
                   (dotimes (bucket bucket-count)
                     (let* ((offset (bucket-offset lhash bucket))
                            (items (read-bucket lhash (%lhash-table lhash) offset)))
                       (incf result (length items))
                       ;;(dbg "B~6,'0D ITEMS ~S" bucket (length items))
                       ;;(dolist (i items)
                       ;;  (dbg " ~S" i))
                       ))
                   (values
                    (list :counted result
                          :time (coerce total-time 'float)
                          :map-len (length (map-lhash (lambda (i) i) lhash :collect-p t))
                          :bucket-count (bucket-count lhash)
                          :level (read-lhash-level lhash)
                          :next-split (read-lhash-next-split lhash)
                          )
                    (analyze-lhash lhash)))))
          (delete-lhash lhash)))))

  (defun test-lhash ()
    (flet ((lnow ()
             (multiple-value-bind (sec msec) (osicat-posix:gettimeofday)
               (+ sec (/ msec 1000000)))))
      (let ((lhash (make-lhash :test 'uuid-array-equal
                               :location "/var/tmp/lhash/"
                               :buckets 8))
            (table nil))
        (dbg "~A" lhash)
        (unwind-protect
             (let* ((start (lnow)) (cycle-start (lnow)))
               (dotimes (i 10000)
                 (when (zerop (mod i 100))
                   (let ((time (- (lnow) cycle-start)))
                     (dbg "~A (~F)" i time))
                   (setq cycle-start (lnow)))
                 (let ((uuid (gen-id)) (value (random sb-ext:most-positive-word)))
                   ;;(dbg "~%ADDING ~A / ~A" uuid value)
                   (push (cons uuid value) table)
                   (lhash-insert lhash uuid value)
                   ;;(dbg "LOOKING UP ~A" uuid)
                   (let ((v2 (lhash-get lhash uuid)))
                     (if (eql value v2)
                         ;;(dbg "SUCCESSFULLY FOUND ~A / ~A" uuid v2)
                         nil
                         (dbg "VALUES FOR ~A NOT = ~A / ~A" uuid value v2)))
                   ;;(dbg "REMOVING ~A / ~A" uuid value)
                   ;;(lhash-remove lhash uuid)
                   ;;(dbg "REMOVED? ~A" (null (lhash-get lhash uuid)))
                   (let ((l 0))
                     (map-lhash (lambda (i) (declare (ignore i)) (incf l)) lhash :collect-p nil)
                     (unless (= (1+ i) l)
                       (dbg "Length is wrong: ~S != ~S" (1+ i) l)))
                   ))
               (dbg "done in ~F seconds" (- (lnow) start))
               (dbg "LENGTH: ~S" (length (map-lhash (lambda (i) i) lhash :collect-p t)))
               (dolist (pair (nreverse table))
                 ;;(dbg "~%LOOKING FOR ~A" pair)
                 (let ((v2 (lhash-get lhash (car pair))))
                   (unless (eql (cdr pair) v2)
                     (dbg "GOT WRONG VALUE FOR ~A. GOT ~A, SHOULD BE ~A"
                          (car pair) v2 (cdr pair))))))
          (delete-lhash lhash)))))

  (defun test-lhash2 (&key profile-p)
    (init-buffer-pool)
    (when profile-p
      (sb-profile:reset)
      (profile-lhash))
    (flet ((lnow ()
             (multiple-value-bind (sec msec) (osicat-posix:gettimeofday)
               (+ sec (/ msec 1000000)))))
      (let ((uuids (loop for i from 0 below 1000000
                      collecting (cons (gen-id)
                                       (random sb-ext:most-positive-word))))
            (lhash (make-lhash :test 'uuid-array-equal
                               :location "/var/tmp/lhash/"
                               :bucket-size 24
                               :buckets (expt 2 20))))
                               ;;:buckets (expt 2 8))))
        (dbg "~A" lhash)
        (unwind-protect
             (let* ((start (lnow)))
               (dolist (pair uuids)
                 (lhash-insert lhash (car pair) (cdr pair)))
               (dbg "done")
               (dbg "~A INSERTS IN ~F seconds" (length uuids) (- (lnow) start))
               (let* ((start2 (lnow)))
                 (dolist (pair uuids)
                   ;;(dbg "~%LOOKING FOR ~A" pair)
                   (let ((v2 (lhash-get lhash (car pair))))
                     (unless (eql (cdr pair) v2)
                       (error "GOT WRONG VALUE FOR ~A. GOT ~A, SHOULD BE ~A"
                              (car pair) v2 (cdr pair)))))
                 (dbg "~A LOOKUPS IN ~F seconds" (length uuids) (- (lnow) start2)))
#|
                   (let* ((start3 (lnow)) (count 0) (new-uuids nil))
               (loop until (null uuids) do
               (let ((pair (pop uuids)))
               (incf count)
                        ;;(dbg "REMOVING ~A" (car pair))
               (lhash-remove lhash (car pair))
               ))
               (dolist (ids (list uuids new-uuids))
               (dolist (p2 ids)
               (unless (equalp p2 pair)
               (let ((v2 (lhash-get lhash (car p2))))
               (unless (eql (cdr p2) v2)
               (error
               "GOT WRONG VALUE FOR ~A. GOT ~A, SHOULD BE ~A"
               (car p2) v2 (cdr p2))))))))
                   (let ((pair (cons (gen-id)
               (random sb-ext:most-positive-word))))
               (lhash-insert lhash (car pair) (cdr pair))
               (push pair new-uuids)))
                 |#
;;                 (dbg "~A DELETIONS IN ~F seconds" count (- (lnow) start3)))
#|
                 (let* ((start4 (lnow)))
               (dolist (pair new-uuids)
                     ;;(dbg "~%LOOKING FOR ~A" pair)
               (let ((v2 (lhash-get lhash (car pair))))
               (unless (eql v2 (cdr pair))
               (error "GOT WRONG VALUE FOR ~A. GOT ~A, SHOULD BE ~A"
               (car pair) v2 (cdr v2)))))
               (dbg "~A LOOKUPS IN ~F seconds"
               (length new-uuids) (- (lnow) start4))))
               |#
               )
          (progn
            (dbg "~A" lhash)
            (delete-lhash lhash)
            ;;(close-lhash lhash)
            (when profile-p
              (prog1
                  (sb-profile:report)
                (unprofile-lhash))))))))


  (defun test ()
    (handler-case
        (progn
          (test-lhash2 :profile-p nil)
          (dbg "BUFFER POOL SIZE NOW ~A"
               (length (first (gethash 16 *buffer-pool*)))))
      (error (c)
        (dbg "TEST ERRED: ~A" c)))
    (sb-ext:exit :code 0)))

;;(test)

(defun test-reopen ()
  ;;(init-buffer-pool)
  (let ((uuids (loop for x from 0 below 10
                  collecting (gen-id))))
    (let ((lhash (make-lhash :test 'uuid-array-equal
                             :location "/var/tmp/lhash/"
                             :buckets 1024)))
      (unwind-protect
           (progn
             (dolist (id uuids)
               (let ((v (random sb-ext:most-positive-word)))
                 (dbg "~S -> ~S" id v)
                 (lhash-insert lhash id v)))
             (terpri)
             (dolist (id uuids)
               (let ((v (lhash-get lhash id)))
                 (dbg "~S -> ~S" id v))))
        (close-lhash lhash)))
    (terpri)
    (let ((lhash (open-lhash "/var/tmp/lhash/")))
      (unwind-protect
           (dolist (id uuids)
             (let ((v (lhash-get lhash id)))
               (dbg "~S -> ~S" id v)))
        (close-lhash lhash)))))
