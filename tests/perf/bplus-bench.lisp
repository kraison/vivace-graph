(in-package :graph-db)

;;;; Side-by-side perf: mmap B+ tree vs mmap skip-list, on identical key sets.
;;;;
;;;; In-package :graph-db so it can drive both structures' internals and trace
;;;; page touches on BOTH read paths symmetrically.  Entry point: (bplus-bench).
;;;;
;;;; The headline locality metric is DISTINCT PAGES TOUCHED per operation -- the
;;;; hardware-independent predictor of cold page faults (a cold touch of a page
;;;; not in the OS cache = one fault).  We count logical node/page visits along
;;;; each structure's real search path, independent of any in-RAM node cache, so
;;;; it models the cold case without needing to drop the OS cache (which needs
;;;; root).  We pair it with warm in-process throughput (us/op) and structural
;;;; stats (bytes/key, height/levels).

(defparameter *bpt-bench-page-bytes* 4096
  "Page granularity for the distinct-page-touch metric (an OS page).")

;;; ---------------------------------------------------------------------------
;;; Small timing + formatting helpers
;;; ---------------------------------------------------------------------------

(defmacro us-per-op ((ops) &body body)
  "Run BODY, return microseconds per op over OPS ops."
  (let ((s (gensym)) (o (gensym)))
    `(let ((,s (get-internal-real-time)) (,o ,ops))
       (progn ,@body)
       (/ (* 1.0d6 (- (get-internal-real-time) ,s))
          (* ,o (float internal-time-units-per-second 1.0d0))))))

(defun page-of (addr) (floor addr *bpt-bench-page-bytes*))

;;; ---------------------------------------------------------------------------
;;; Structural stats
;;; ---------------------------------------------------------------------------

(defun bpt-walk-stats (tree)
  "Return (values PAGE-COUNT INTERNAL-COUNT LEAF-COUNT) by walking every page."
  (let ((pages 0) (internal 0) (leaves 0))
    (labels ((walk (addr)
               (incf pages)
               (multiple-value-bind (leaf-p link entries)
                   (%bpt-decode-page tree (%bpt-read-page tree addr))
                 (if leaf-p
                     (incf leaves)
                     (progn (incf internal)
                            (walk link)
                            (dolist (e entries) (walk (third e))))))))
      (walk (%bpt-root tree)))
    (values pages internal leaves)))

(defun sl-node-stats (sl)
  "Return (values NODE-COUNT TOTAL-BYTES MAX-LEVEL) for the on-disk skip list."
  (let ((nodes 0) (bytes 0) (maxlvl 0)
        (pred (%sl-head sl)))
    (loop
      (let ((node (read-skip-node sl (aref (%sn-pointers pred) 0))))
        (when (= (%sn-addr node) (%sn-addr (%sl-tail sl))) (return))
        (incf nodes)
        (incf bytes (%sn-size node))
        (setf maxlvl (max maxlvl (%sn-level node)))
        (setf pred node)))
    (values nodes bytes maxlvl)))

;;; ---------------------------------------------------------------------------
;;; Page-touch tracing along each real search path
;;; ---------------------------------------------------------------------------

(defun bpt-lookup-pages (tree dkey)
  "Distinct pages the B+ tree touches to look up DKEY."
  (let ((*bpt-page-trace* (make-hash-table)))
    (%bpt-find tree dkey)
    (hash-table-count *bpt-page-trace*)))

(defun sl-lookup-pages (sl key)
  "Distinct pages the skip list touches to look up KEY -- mirrors
%FIND-IN-SKIP-LIST, recording every node visited (cache-independent)."
  (let ((pages (make-hash-table)) (pred (%sl-head sl)))
    (setf (gethash (page-of (%sn-addr pred)) pages) t)
    (loop for level from (1- (%sl-max-level sl)) downto 0 do
      (let ((curr (read-skip-node sl (aref (%sn-pointers pred) level))))
        (setf (gethash (page-of (%sn-addr curr)) pages) t)
        (loop while (and (/= (%sn-addr curr) (%sn-addr (%sl-tail sl)))
                         (funcall (%sl-comparison sl) (%sn-key curr) key))
              do (setf pred curr
                       curr (read-skip-node sl (aref (%sn-pointers pred) level)))
                 (setf (gethash (page-of (%sn-addr curr)) pages) t))
        (when (funcall (%sl-key-equal sl) key (%sn-key curr)) (return))))
    (hash-table-count pages)))

(defun bpt-range-pages (tree start end)
  "Distinct pages + entries visited scanning [START,END] in the B+ tree."
  (let ((*bpt-page-trace* (make-hash-table)) (count 0))
    (let ((cur (make-range-cursor tree start end)))
      (loop for node = (cursor-next cur :eoc)
            until (eq node :eoc) do (incf count)))
    (values (hash-table-count *bpt-page-trace*) count)))

(defun sl-range-pages (sl start end)
  "Distinct pages + entries visited scanning [START,END] in the skip list --
counts the leaf-chain nodes yielded (the dominant, scattered cost)."
  (let ((pages (make-hash-table)) (count 0)
        (cur (make-range-cursor sl start end)))
    (loop for node = (cursor-next cur :eoc)
          until (eq node :eoc)
          do (incf count)
             (setf (gethash (page-of (%sn-addr node)) pages) t))
    (values (hash-table-count pages) count)))

;;; ---------------------------------------------------------------------------
;;; Builders (identical keys/values into each structure)
;;; ---------------------------------------------------------------------------

(defun make-int-skip-list (heap)
  (make-skip-list :heap heap
                  :head-key most-negative-fixnum :head-value +null-key+
                  :tail-key most-positive-fixnum :tail-value +max-key+
                  :key-equal '= :key-comparison '<
                  :value-equal 'equal
                  :key-serializer 'serialize :key-deserializer 'deserialize
                  :value-serializer 'serialize :value-deserializer 'deserialize))

(defun make-int-bplus-tree (heap page-size)
  (make-bplus-tree :heap heap :page-size page-size
                   :key-comparison '< :key-equal '= :value-equal 'equal
                   :key-serializer 'serialize :key-deserializer 'deserialize
                   :value-serializer 'serialize :value-deserializer 'deserialize))

;;; ---------------------------------------------------------------------------
;;; The benchmark
;;; ---------------------------------------------------------------------------

(defun bplus-bench (&key (sizes '(10000 100000 500000 1000000))
                         (page-size 4096)
                         (lookup-sample 5000)
                         (range-span 1000)
                         (heap-mb 256))
  "Compare the mmap B+ tree and skip list on integer keys at each N in SIZES.
Runs cleanly through N=1e6.  (heap-mb sizes the per-structure heap; at 1e6 the
skip list uses ~52 MB and the B+ tree ~23 MB, so 256 MB is ample.)"
  (format t "~&~%############  B+ TREE  vs  SKIP-LIST  (~A, page ~A B)  ############~%"
          (lisp-implementation-type) page-size)
  (dolist (n sizes)
    (let* ((sl-path (format nil "/var/tmp/bench-sl-~A.dat" n))
           (bp-path (format nil "/var/tmp/bench-bp-~A.dat" n))
           ;; CREATE-MEMORY does NOT truncate an existing file -- it maps it at its
           ;; current size.  A stale file left by a prior crashed/oversized run
           ;; (e.g. a 1 GB file from :heap-mb 1024) is then reused with a
           ;; size/header mismatch and later faults with a wild-address SEGV.
           ;; Always start from a fresh file.
           (sl-heap (progn (ignore-errors (delete-file sl-path))
                           (create-memory sl-path (* 1024 1024 heap-mb))))
           (bp-heap (progn (ignore-errors (delete-file bp-path))
                           (create-memory bp-path (* 1024 1024 heap-mb)))))
      (unwind-protect
           (progn
           (format t "~&[N=~:D] building...~%" n) (finish-output)
           ;; Shuffle a VECTOR (elt is O(1)); nshuffle on a list is O(n^2) and
           ;; effectively hangs at n=1e6.
           (let* ((keys (nshuffle (make-array n :initial-contents
                                              (loop for i below n collect i))))
                  (sample (loop repeat lookup-sample collect (aref keys (random n))))
                  (sl (make-int-skip-list sl-heap))
                  (bp (make-int-bplus-tree bp-heap page-size)))
             ;; ---- insert (build) ----
             (let ((sl-ins (us-per-op (n)
                             (loop for k across keys do (add-to-skip-list sl k (* k 10)))))
                   (bp-ins (progn (finish-output)
                                  (us-per-op (n)
                                    (loop for k across keys do (add-to-skip-list bp k (* k 10)))))))
               (format t "~&[N=~:D] built; measuring...~%" n) (finish-output)
               ;; ---- warm point lookup ----
               (let ((sl-look (us-per-op ((length sample))
                                (dolist (k sample) (find-in-skip-list sl k))))
                     (bp-look (us-per-op ((length sample))
                                (dolist (k sample) (find-in-skip-list bp k)))))
                 ;; ---- warm range scan (average over several windows) ----
                 (let* ((windows (loop repeat 50 collect (random (max 1 (- n range-span)))))
                        (sl-scan (us-per-op ((* (length windows) range-span))
                                   (dolist (w windows)
                                     (let ((cur (make-range-cursor sl w (+ w range-span))))
                                       (loop for x = (cursor-next cur :eoc)
                                             until (eq x :eoc))))))
                        (bp-scan (us-per-op ((* (length windows) range-span))
                                   (dolist (w windows)
                                     (let ((cur (make-range-cursor bp w (+ w range-span))))
                                       (loop for x = (cursor-next cur :eoc)
                                             until (eq x :eoc)))))))
                   ;; ---- cold page-touch metrics ----
                   (let* ((psamp (loop repeat 1000 collect (aref keys (random n))))
                          (sl-lp (/ (reduce #'+ psamp :key (lambda (k) (sl-lookup-pages sl k)))
                                    1000.0))
                          (bp-lp (/ (reduce #'+ psamp :key (lambda (k) (bpt-lookup-pages bp k)))
                                    1000.0))
                          (rw (loop repeat 30 collect (random (max 1 (- n range-span)))))
                          (sl-rp (/ (reduce #'+ rw :key (lambda (w) (sl-range-pages sl w (+ w range-span))))
                                    30.0))
                          (bp-rp (/ (reduce #'+ rw :key (lambda (w) (bpt-range-pages bp w (+ w range-span))))
                                    30.0)))
                     ;; ---- structural stats ----
                     (multiple-value-bind (sl-nodes sl-bytes sl-maxlvl) (sl-node-stats sl)
                       (multiple-value-bind (bp-pages bp-int bp-leaf) (bpt-walk-stats bp)
                         (declare (ignore bp-int bp-leaf))
                         ;; ---- warm remove (mutates; do last) ----
                         (let* ((half (loop for i below n by 2 collect i))
                                (sl-rm (us-per-op ((length half))
                                         (dolist (k half) (remove-from-skip-list sl k))))
                                (bp-rm (us-per-op ((length half))
                                         (dolist (k half) (remove-from-skip-list bp k)))))
                           (bench-report
                            n page-size
                            :sl-ins sl-ins :bp-ins bp-ins
                            :sl-look sl-look :bp-look bp-look
                            :sl-scan sl-scan :bp-scan bp-scan
                            :sl-lp sl-lp :bp-lp bp-lp
                            :sl-rp sl-rp :bp-rp bp-rp
                            :sl-rm sl-rm :bp-rm bp-rm
                            :range-span range-span
                            :sl-nodes sl-nodes :sl-bytes sl-bytes :sl-maxlvl sl-maxlvl
                            :bp-pages bp-pages
                            :bp-height (%bpt-height bp)))))))))))
        (progn (close-memory sl-heap) (close-memory bp-heap)
               (ignore-errors (delete-file sl-path))
               (ignore-errors (delete-file bp-path))))))
  (values))

(defun ratio-str (sl bp)
  "Format BP relative to SL as an x-factor (\"2.1x faster\" style is left to reader)."
  (if (or (zerop bp) (zerop sl)) "n/a"
      (format nil "~,2Fx" (/ (float sl) (float bp)))))

(defun bench-report (n page-size &key sl-ins bp-ins sl-look bp-look sl-scan bp-scan
                                      sl-lp bp-lp sl-rp bp-rp sl-rm bp-rm range-span
                                      sl-nodes sl-bytes sl-maxlvl bp-pages bp-height)
  (declare (ignore page-size))
  (format t "~%--- N = ~:D ---~%" n)
  (format t "~&(ratio column = skip-list / b+tree; >1x means the B+ tree wins)~%")
  (format t "~&~28A ~14A ~14A ~10A~%" "metric" "skip-list" "b+tree" "sl/bp")
  (flet ((row (label sl bp)
           (format t "~&~28A ~14,3F ~14,3F ~10A~%" label sl bp (ratio-str sl bp))))
    (row "insert us/op" sl-ins bp-ins)
    (row "point-lookup us/op" sl-look bp-look)
    (row (format nil "range-scan us/ent (~A)" range-span) sl-scan bp-scan)
    (row "remove us/op" sl-rm bp-rm)
    (format t "~&  -- cold locality (distinct ~A-B pages touched) --~%" *bpt-bench-page-bytes*)
    (format t "~&~28A ~14,2F ~14,2F ~10A~%" "pages / point-lookup" sl-lp bp-lp
            (ratio-str sl-lp bp-lp))
    (format t "~&~28A ~14,2F ~14,2F ~10A~%"
            (format nil "pages / ~A-scan" range-span) sl-rp bp-rp
            (ratio-str sl-rp bp-rp))
    (format t "~&  -- structure --~%")
    (format t "~&~28A ~14:D ~14:D~%" "entries / nodes / pages" sl-nodes bp-pages)
    (format t "~&~28A ~14:D ~14A~%" "skip-list bytes (used)" sl-bytes "-")
    (format t "~&~28A ~14,1F ~14A~%" "skip-list bytes/key"
            (/ sl-bytes (float n)) "-")
    (format t "~&~28A ~14A ~14,1F~%" "b+tree bytes/key" "-"
            (/ (* bp-pages *bpt-bench-page-bytes*) (float n)))
    (format t "~&~28A ~14A ~14:D~%" "b+tree height" "-" bp-height)
    (format t "~&~28A ~14:D ~14A~%" "skip-list max level" sl-maxlvl "-"))
  (finish-output))
