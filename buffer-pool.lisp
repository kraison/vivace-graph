(in-package :graph-db)

(defvar *buffer-pool*
  #+sbcl (make-hash-table :test 'eq :synchronized t)
  #+lispworks (make-hash-table :test 'eq :single-thread nil)
  #+ccl (make-hash-table :test 'eq :shared t)
  #+ecl (make-hash-table :test 'eq))
(defvar *buffer-pool-stats* nil)
(defvar *buffer-pool-thread* nil)
(defvar *stop-buffer-pool* nil)
;; The BUFFER-POOL-SIZE the pool was initialized with.  The monitor's refill
;; targets scale from this (#48): the small/node classes top up to *BUFFER-POOL-
;; SIZE*, the hot pcons/skip-node/byte-16 classes to 10x that -- matching what
;; INIT-BUFFER-POOL pre-fills.  Previously the targets were hardcoded (1M / 100k)
;; regardless of BUFFER-POOL-SIZE, so a pool drained below the low-water mark
;; ballooned to millions of live objects even when the caller asked for a small
;; pool.  Default 100000 reproduces the old 1M / 100k ceilings exactly.
(defvar *buffer-pool-size* 100000)
(defvar *buffer-pool-low-water-mark* 1000)
(defvar *free-memory-low-water-mark* 10485760)
(defparameter *allow-force-gc-p* nil)
#+(or ccl ecl)
(defvar *buffer-pool-lock* (make-lock))

(defstruct (buffer-pool-stats
             (:conc-name bps-))
  (buffer-8 0 :type (UNSIGNED-BYTE 64))
  (buffer-16 0 :type (UNSIGNED-BYTE 64))
  (buffer-18 0 :type (UNSIGNED-BYTE 64))
  (buffer-24 0 :type (UNSIGNED-BYTE 64))
  (buffer-34 0 :type (UNSIGNED-BYTE 64))
  (pcons 0 :type (UNSIGNED-BYTE 64))
  (vertex 0 :type (UNSIGNED-BYTE 64))
  (edge 0 :type (UNSIGNED-BYTE 64))
  (skip-node 0 :type (UNSIGNED-BYTE 64)))

(defun dump-buffer-pool-stats ()
  (let* ((stats
          (list
           (cons :cache-misses
                 (list (cons :BUFFER-8 (bps-buffer-8 *buffer-pool-stats*))
                       (cons :BUFFER-16 (bps-buffer-16 *buffer-pool-stats*))
                       (cons :BUFFER-18 (bps-buffer-18 *buffer-pool-stats*))
                       (cons :BUFFER-24 (bps-buffer-24 *buffer-pool-stats*))
                       (cons :BUFFER-34 (bps-buffer-34 *buffer-pool-stats*))
                       (cons :pcons (bps-pcons *buffer-pool-stats*))
                       (cons :vertex (bps-vertex *buffer-pool-stats*))
                       (cons :edge (bps-edge *buffer-pool-stats*))
                       (cons :skip-node (bps-skip-node *buffer-pool-stats*))))))
           (buffers nil))
    (maphash (lambda (k v)
               (push (cons k (length (first v))) buffers))
             *buffer-pool*)
    (append stats (list (cons :available-buffers buffers)))))

(defun monitor-buffer-pool ()
  (let ((free-memory (free-memory)))
    (if (and *allow-force-gc-p*
             (<= free-memory *free-memory-low-water-mark*))
        (progn
          (log:warn "VG forcing a full GC, ~A bytes dynamic space available"
                    free-memory)
          #+sbcl (sb-ext:gc :full t)
          #+ccl (gc)
          #+lispworks(hcl:gc-generation 2)
          )
        (let ((stats (dump-buffer-pool-stats))
              ;; Refill targets scale with *BUFFER-POOL-SIZE* (#48): the hot
              ;; pcons/skip-node/byte-16 classes to 10x, the rest to 1x -- the
              ;; same ratios INIT-BUFFER-POOL pre-fills.  Default size 100000
              ;; gives the historical 1M / 100k ceilings.
              (big (* 10 *buffer-pool-size*))
              (small *buffer-pool-size*))
          (dolist (stat (cdr (assoc :available-buffers stats)))
            (when (< (cdr stat) *buffer-pool-low-water-mark*)
              (case (car stat)
                (16
                 (log:info "BUFFER-POOL: Refreshing byte-vector-16 buffers (~D)"
                           (- big (cdr stat)))
                 (dotimes (i (- big (cdr stat)))
                   (let ((b (make-byte-vector 16)))
                     #+sbcl
                     (sb-ext:atomic-push b (first (gethash 16 *buffer-pool*)))
                     #+lispworks
                     (sys:atomic-push b (car (gethash 16 *buffer-pool*)))
                     #+ccl
                     (ccl:with-lock-grabbed (*buffer-pool-lock*)
                       (push b (first (gethash 16 *buffer-pool*))))
                     #+ecl
                     (mp:with-lock (*buffer-pool-lock*)
                       (push b (first (gethash 16 *buffer-pool*)))))))
                ((8 18 24 34)
                 (log:info "BUFFER-POOL: Refreshing byte-vector-~D buffers (~D)"
                           (car stat) (- small (cdr stat)))
                 (dotimes (i (- small (cdr stat)))
                   (let ((b (make-byte-vector (car stat))))
                     #+sbcl
                     (sb-ext:atomic-push b (first (gethash (car stat) *buffer-pool*)))
                     #+lispworks
                     (sys:atomic-push b (car (gethash (car stat) *buffer-pool*)))
                     #+ccl
                     (ccl:with-lock-grabbed (*buffer-pool-lock*)
                       (push b (first (gethash (car stat) *buffer-pool*))))
                     #+ecl
                     (mp:with-lock (*buffer-pool-lock*)
                       (push b (first (gethash (car stat) *buffer-pool*)))))))
                (:skip-node
                 (log:info "BUFFER-POOL: Refreshing skip-node buffers (~D)"
                           (- big (cdr stat)))
                 (dotimes (i (- big (cdr stat)))
                   (make-skip-node-buffer)))
                (:pcons
                 (log:info "BUFFER-POOL: Refreshing pcons buffers (~D)"
                           (- big (cdr stat)))
                 (dotimes (i (- big (cdr stat)))
                   (make-pcons-buffer)))
                ;; ECL builds vertices/edges as their subclass directly and never
                ;; pops these pools (#47), so refilling them there is dead weight.
                #-ecl
                (:edge
                 (log:info "BUFFER-POOL: Refreshing edge buffers (~D)"
                           (- small (cdr stat)))
                 (dotimes (i (- small (cdr stat)))
                   (make-edge-buffer)))
                #-ecl
                (:vertex
                 (log:info "BUFFER-POOL: Refreshing vertex buffers (~D)"
                           (- small (cdr stat)))
                 (dotimes (i (- small (cdr stat)))
                   (make-vertex-buffer))))))))))

(defun monitor-buffer-pool-loop ()
  (loop
     until *stop-buffer-pool*
     do
       (monitor-buffer-pool)
       (sleep 1)))

(defun reset-buffer-pool-stats ()
  (setq *buffer-pool-stats* (make-buffer-pool-stats)))

(defun make-pcons-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing pcons buffer")
    (let ((p (%make-pcons)))
      #+sbcl
      (sb-ext:atomic-push p (first (gethash :pcons *buffer-pool*)))
      #+lispworks
      (sys:atomic-push p (car (gethash :pcons *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push p (first (gethash :pcons *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push p (first (gethash :pcons *buffer-pool*)))))))

(defun make-vertex-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing vertex buffer")
    (let ((v (make-instance 'vertex :id (gen-vertex-id))))
      #+sbcl
      (sb-ext:atomic-push v (first (gethash :vertex *buffer-pool*)))
      #+lispworks
      (sys:atomic-push v (car (gethash :vertex *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push v (first (gethash :vertex *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push v (first (gethash :vertex *buffer-pool*)))))))

(defun make-edge-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing edge buffer")
    (let ((e (make-instance 'edge :id (gen-edge-id))))
      #+sbcl
      (sb-ext:atomic-push e (first (gethash :edge *buffer-pool*)))
      #+lispworks
      (sys:atomic-push e (car (gethash :edge *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push e (first (gethash :edge *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push e (first (gethash :edge *buffer-pool*)))))))

(defun make-skip-node-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing skip-node buffer")
    (let ((s (%make-skip-node)))
      #+sbcl
      (sb-ext:atomic-push s (first (gethash :skip-node *buffer-pool*)))
      #+lispworks
      (sys:atomic-push s (car (gethash :skip-node *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push s (first (gethash :skip-node *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push s (first (gethash :skip-node *buffer-pool*)))))))

(defun make-byte-vector-8-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-8")
    (let ((b (make-byte-vector 8)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 8 *buffer-pool*)))
      #+lispworks
      (sys:atomic-push b (car (gethash 8 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 8 *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push b (first (gethash 8 *buffer-pool*)))))))

(defun make-byte-vector-16-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-16")
    (let ((b (make-byte-vector 16)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 16 *buffer-pool*)))
      #+lispworks
      (sys:atomic-push b (car (gethash 16 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 16 *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push b (first (gethash 16 *buffer-pool*)))))))

(defun make-byte-vector-18-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-18")
    (let ((b (make-byte-vector 18)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 18 *buffer-pool*)))
      #+lispworks
      (sys:atomic-push b (car (gethash 18 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 18 *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push b (first (gethash 18 *buffer-pool*)))))))

(defun make-byte-vector-24-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-24")
    (let ((b (make-byte-vector 24)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 24 *buffer-pool*)))
      #+lispworks
      (sys:atomic-push b (car (gethash 24 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 24 *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push b (first (gethash 24 *buffer-pool*)))))))

(defun make-byte-vector-34-buffer ()
  (let ((*package* (find-package :graph-db)))
    ;;(log:debug "refreshing byte-vector-buffer-34")
    (let ((b (make-byte-vector 34)))
      #+sbcl
      (sb-ext:atomic-push b (first (gethash 34 *buffer-pool*)))
      #+lispworks
      (sys:atomic-push b (car (gethash 34 *buffer-pool*)))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push b (first (gethash 34 *buffer-pool*))))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push b (first (gethash 34 *buffer-pool*)))))))

(defun buffer-pool-running-p ()
  (and (threadp *buffer-pool-thread*)
       (thread-alive-p *buffer-pool-thread*)))

(defun init-buffer-pool (buffer-pool-size)
  (when (buffer-pool-running-p)
    (error "~A is already running. Cannot init-buffer-pool"
           *buffer-pool-thread*))
  (setq *stop-buffer-pool* nil)
  ;; Remember the size so the monitor's refill targets scale to it (#48).
  (setq *buffer-pool-size* buffer-pool-size)
  (reset-buffer-pool-stats)
  (setq *buffer-pool*
        #+sbcl (make-hash-table :test 'eq :synchronized t)
        #+lispworks (make-hash-table :test 'eq :single-thread nil)
        #+ccl (make-hash-table :test 'eq :shared t)
        #+ecl (make-hash-table :test 'eq))
  (setf (gethash :vertex *buffer-pool*)
        (list nil)
        (gethash :edge *buffer-pool*)
        (list nil)
        (gethash :pcons *buffer-pool*)
        (list nil)
        (gethash :skip-node *buffer-pool*)
        (list nil))
  (dolist (num '(8 16 18 24 34))
    (setf (gethash num *buffer-pool*)
          (list nil)))
  (dotimes (i buffer-pool-size)
    ;; ECL constructs vertices/edges as their subclass directly (#47) and never
    ;; pops these pools, so pre-filling them on ECL is pure dead weight.
    #-ecl (make-vertex-buffer)
    #-ecl (make-edge-buffer)
    (make-byte-vector-8-buffer)
    (make-byte-vector-18-buffer)
    (make-byte-vector-24-buffer)
    (make-byte-vector-34-buffer))
  (dotimes (i (* 10 buffer-pool-size))
    (make-pcons-buffer)
    (make-skip-node-buffer)
    (make-byte-vector-16-buffer))
  (setq *buffer-pool-thread*
        (make-thread 'monitor-buffer-pool-loop :name "buffer-pool-thread"))
  *buffer-pool*)

(defun ensure-buffer-pool (buffer-pool-size)
  (or (buffer-pool-running-p)
      (init-buffer-pool buffer-pool-size)))

(defun stop-buffer-pool ()
  (setq *stop-buffer-pool* t)
  (when (and (threadp *buffer-pool-thread*)
             (thread-alive-p *buffer-pool-thread*))
    (join-thread *buffer-pool-thread*)))

;;; ---------------------------------------------------------------------------
;;; Runtime resize (#48)
;;;
;;; The pool is a single process-wide resource shared by every open graph.
;;; SET-BUFFER-POOL-SIZE changes *BUFFER-POOL-SIZE* and converges the live pool
;;; to the new per-class targets NOW -- trimming excess buffers so a shrink
;;; actually releases memory (not just lowers the ceiling), and topping up a
;;; grow.  The monitor thread keeps using the new size for subsequent refills.

(defparameter +buffer-pool-classes+
  ;; ECL builds vertices/edges directly and never pops those pools (#47).
  '(:pcons :skip-node 16 8 18 24 34 #-ecl :vertex #-ecl :edge)
  "The buffer classes the pool maintains, keyed as in *BUFFER-POOL*.")

(defun buffer-pool-class-target (class)
  "Steady-state target count the monitor maintains for CLASS at the current
*BUFFER-POOL-SIZE*: 10x for the hot pcons/skip-node/byte-16 classes, 1x otherwise
-- the same ratios INIT-BUFFER-POOL pre-fills."
  (if (member class '(:pcons :skip-node 16))
      (* 10 *buffer-pool-size*)
      *buffer-pool-size*))

(defun %refill-pool-class (class n)
  "Add N fresh buffers of CLASS via the normal push path."
  (dotimes (i n)
    (case class
      (:pcons (make-pcons-buffer))
      (:skip-node (make-skip-node-buffer))
      #-ecl (:vertex (make-vertex-buffer))
      #-ecl (:edge (make-edge-buffer))
      (8 (make-byte-vector-8-buffer))
      (16 (make-byte-vector-16-buffer))
      (18 (make-byte-vector-18-buffer))
      (24 (make-byte-vector-24-buffer))
      (34 (make-byte-vector-34-buffer)))))

(defun %trim-pool-class (class n)
  "Drop N buffers of CLASS (they become garbage), using the pool's own pop
synchronization so it is safe against concurrent GET-*-BUFFER."
  (when (plusp n)
    #+(or ccl ecl)
    (with-lock (*buffer-pool-lock*)
      (setf (first (gethash class *buffer-pool*))
            (nthcdr n (first (gethash class *buffer-pool*)))))
    #+sbcl
    (dotimes (i n) (sb-ext:atomic-pop (first (gethash class *buffer-pool*))))
    #+lispworks
    (dotimes (i n) (sys:atomic-pop (car (gethash class *buffer-pool*))))))

(defun set-buffer-pool-size (new-size &key (gc t))
  "Resize the live, process-wide buffer pool to NEW-SIZE (a positive integer) and
converge every buffer class to its new target NOW -- trimming excess (so a shrink
releases memory) and topping up shortfalls.  Subsequent monitor refills use
NEW-SIZE.  With :GC (the default) forces a full collection afterward so trimmed
buffers are reclaimed promptly.  Returns NEW-SIZE.

NOTE: the pool is global (shared by all open graphs), so this affects the whole
process.  If the pool isn't running this only records the size for the next
INIT-BUFFER-POOL."
  (check-type new-size (integer 1))
  (setf *buffer-pool-size* new-size)
  (when (buffer-pool-running-p)
    (dolist (class +buffer-pool-classes+)
      (let ((delta (- (length (first (gethash class *buffer-pool*)))
                      (buffer-pool-class-target class))))
        (cond ((plusp delta)  (%trim-pool-class class delta))
              ((minusp delta) (%refill-pool-class class (- delta))))))
    (when gc
      #+sbcl (sb-ext:gc :full t)
      #+ccl (ccl:gc)
      #+ecl (ext:gc t)
      #+lispworks (hcl:gc-generation 2)))
  new-size)

(defun buffer-incf-stat (size)
  (when (buffer-pool-running-p)
    (case size
      (8
       #+ecl
       (mp:with-lock (*buffer-pool-lock*)
         (incf (bps-buffer-8 *buffer-pool-stats*)))
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-8 *buffer-pool-stats*)))
       #+lispworks
       (sys:atomic-incf (bps-buffer-8 *buffer-pool-stats*))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-8 *buffer-pool-stats*)))
      (16
       #+ecl
       (mp:with-lock (*buffer-pool-lock*)
         (incf (bps-buffer-16 *buffer-pool-stats*)))
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-16 *buffer-pool-stats*)))
       #+lispworks
       (sys:atomic-incf (bps-buffer-16 *buffer-pool-stats*))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-16 *buffer-pool-stats*)))
      (18
       #+ecl
       (mp:with-lock (*buffer-pool-lock*)
         (incf (bps-buffer-18 *buffer-pool-stats*)))
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-18 *buffer-pool-stats*)))
       #+lispworks
       (sys:atomic-incf (bps-buffer-18 *buffer-pool-stats*))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-18 *buffer-pool-stats*)))
      (24
       #+ecl
       (mp:with-lock (*buffer-pool-lock*)
         (incf (bps-buffer-24 *buffer-pool-stats*)))
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-24 *buffer-pool-stats*)))
       #+lispworks
       (sys:atomic-incf (bps-buffer-24 *buffer-pool-stats*))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-24 *buffer-pool-stats*)))
      (34
       #+ecl
       (mp:with-lock (*buffer-pool-lock*)
         (incf (bps-buffer-34 *buffer-pool-stats*)))
       #+ccl
       (ccl:with-lock-grabbed (*buffer-pool-lock*)
         (incf (bps-buffer-34 *buffer-pool-stats*)))
       #+lispworks
       (sys:atomic-incf (bps-buffer-34 *buffer-pool-stats*))
       #+sbcl
       (sb-ext:atomic-incf (bps-buffer-34 *buffer-pool-stats*))))))

(defun get-buffer (size)
  (or (and (buffer-pool-running-p)
           #+sbcl
           (sb-ext:atomic-pop (first (gethash size *buffer-pool*)))
           #+lispworks
           (sys:atomic-pop (car (gethash size *buffer-pool*)))
           #+ecl
           (mp:with-lock (*buffer-pool-lock*)
             (pop (first (gethash size *buffer-pool*))))
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash size *buffer-pool*)))))
      (progn
        (buffer-incf-stat size)
        (make-byte-vector size))))

(defun release-buffer (buffer)
  (when (buffer-pool-running-p)
    (let ((size (length buffer)))
      (dotimes (i size)
        (setf (aref buffer i) 0))
      #+ecl
      (mp:with-lock (*buffer-pool-lock*)
        (push buffer (first (gethash size *buffer-pool*))))
      #+ccl
      (ccl:with-lock-grabbed (*buffer-pool-lock*)
        (push buffer (first (gethash size *buffer-pool*))))
      #+lispworks
      (sys:atomic-push buffer (car (gethash size *buffer-pool*)))
      #+sbcl
      (sb-ext:atomic-push buffer (first (gethash size *buffer-pool*)))
      nil)))

(defun get-vertex-buffer ()
  (or (and (buffer-pool-running-p)
           #+ecl
           (mp:with-lock (*buffer-pool-lock*)
             (pop (first (gethash :vertex *buffer-pool*))))
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :vertex *buffer-pool*))))
           #+lispworks
           (sys:atomic-pop (car (gethash :vertex *buffer-pool*)))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :vertex *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ecl
          (mp:with-lock (*buffer-pool-lock*)
            (incf (bps-vertex *buffer-pool-stats*)))
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-vertex *buffer-pool-stats*)))
          #+lispworks
          (sys:atomic-incf (bps-vertex *buffer-pool-stats*))
          #+sbcl
          (sb-ext:atomic-incf (bps-vertex *buffer-pool-stats*)))
        (make-instance 'vertex))))

(defun get-edge-buffer ()
  (or (and (buffer-pool-running-p)
           #+ecl
           (mp:with-lock (*buffer-pool-lock*)
             (pop (first (gethash :edge *buffer-pool*))))
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :edge *buffer-pool*))))
           #+lispworks
           (sys:atomic-pop (car (gethash :edge *buffer-pool*)))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :edge *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ecl
          (mp:with-lock (*buffer-pool-lock*)
            (incf (bps-edge *buffer-pool-stats*)))
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-edge *buffer-pool-stats*)))
          #+lispworks
          (sys:atomic-incf (bps-edge *buffer-pool-stats*))
          #+sbcl
          (sb-ext:atomic-incf (bps-edge *buffer-pool-stats*)))
        (make-instance 'edge))))

(defun get-skip-node-buffer ()
  (or (and (buffer-pool-running-p)
           #+ecl
           (mp:with-lock (*buffer-pool-lock*)
             (pop (first (gethash :skip-node *buffer-pool*))))
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :skip-node *buffer-pool*))))
           #+lispworks
           (sys:atomic-pop (car (gethash :skip-node *buffer-pool*)))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :skip-node *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ecl
          (mp:with-lock (*buffer-pool-lock*)
            (incf (bps-skip-node *buffer-pool-stats*)))
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-skip-node *buffer-pool-stats*)))
          #+lispworks
          (sys:atomic-incf (bps-skip-node *buffer-pool-stats*))
          #+sbcl
          (sb-ext:atomic-incf (bps-skip-node *buffer-pool-stats*)))
        (%make-skip-node))))

(defun get-pcons-buffer ()
  (or (and (buffer-pool-running-p)
           #+ecl
           (mp:with-lock (*buffer-pool-lock*)
             (pop (first (gethash :pcons *buffer-pool*))))
           #+ccl
           (ccl:with-lock-grabbed (*buffer-pool-lock*)
             (pop (first (gethash :pcons *buffer-pool*))))
           #+lispworks
           (sys:atomic-pop (car (gethash :pcons *buffer-pool*)))
           #+sbcl
           (sb-ext:atomic-pop (first (gethash :pcons *buffer-pool*))))
      (progn
        (when (buffer-pool-running-p)
          #+ecl
          (mp:with-lock (*buffer-pool-lock*)
            (incf (bps-pcons *buffer-pool-stats*)))
          #+ccl
          (ccl:with-lock-grabbed (*buffer-pool-lock*)
            (incf (bps-pcons *buffer-pool-stats*)))
          #+lispworks
          (sys:atomic-incf (bps-pcons *buffer-pool-stats*))
          #+sbcl
          (sb-ext:atomic-incf (bps-pcons *buffer-pool-stats*)))
        (%make-pcons))))
