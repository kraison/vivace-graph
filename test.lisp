(in-package :graph-db)

(defun test-bins ()
  (let ((mem (create-memory "/var/tmp/testbin.dat" (expt 2 12))))
    (unwind-protect
         (let ((pointers nil)
               (sizes '(10 16 22 24 32 64 512 550 576 1024 2048)))
           (loop for size in sizes do
                (dotimes (i 100)
                  (push (allocate mem size) pointers)))
           (map-memory #'(lambda (pointer size free-p)
                           (dbg "GOT P~20,'0D OF SIZE ~D (FREE-P ~A)"
                                pointer size free-p))
                       mem)
           (dolist (pointer pointers)
             ;;(dbg "FREEING ~S" pointer)
             (free mem pointer))
           (sleep 1)
           (map-memory #'(lambda (pointer size free-p)
                           (dbg "GOT P~20,'0D OF SIZE ~D (FREE-P ~A)"
                                pointer size free-p))
                       mem :include-free-p nil)
           (analyze-bins mem)
           ;;(allocate mem 512)
           (loop for size in sizes do
                (dotimes (i 100)
                  (dbg "ALLOCATING CHUNK ~S OF SIZE ~S" i size)
                  (let ((p (allocate mem size)))
                    (dbg "ALLOCATED ~S OF SIZE ~S" p size)
                    ;;(analyze-bins mem)
                    (unless (member p pointers)
                      (dbg "~A (size ~A) not found in pointers" p size)))))
           ;;(analyze-bins mem)
           (map-memory #'(lambda (pointer size free-p)
                           (dbg "GOT P~20,'0D OF SIZE ~D (FREE-P ~A)"
                                pointer size free-p))
                       mem :include-free-p nil)
           )
      (progn
        (close-memory mem)
        (delete-file "/var/tmp/testbin.dat")))))

(defun test-alloc ()
  (let ((mem (create-memory "/var/tmp/testmem.dat" (expt 2 12))))
    (unwind-protect
         (let ((o1 (serialize (list 0 1 2 3)))
               (o2 (serialize #(0 1 2 3)))
               (o3 (serialize "0123")))
           (let ((p1 (allocate mem (length o1)))
                 (p2 (allocate mem (length o2)))
                 (p3 (allocate mem (length o3))))
             (dotimes (i (length o1))
               (set-byte (memory-mmap mem) (+ p1 i) (aref o1 i)))
             (dotimes (i (length o2))
               (set-byte (memory-mmap mem) (+ p2 i) (aref o2 i)))
             (dotimes (i (length o3))
               (set-byte (memory-mmap mem) (+ p3 i) (aref o3 i)))
             (map-memory
              #'(lambda (offset len free-p)
                  (format t "~A / ~A / ~A~%" offset len free-p)
                  (let ((bytes (get-bytes (memory-mmap mem) offset len)))
                    (format t "~A~%" bytes)))
              mem)
             (free mem p2)
             (map-memory
              #'(lambda (offset len free-p)
                  (format t "~A / ~A / ~A~%" offset len free-p)
                  (let ((bytes (get-bytes (memory-mmap mem) offset len)))
                    (format t "~A~%" bytes)))
              mem)))
      (close-memory mem))))



(progn
  (defvar *v1* nil)
  (defvar *v2* nil)
  (defvar *e1* nil)
  (defvar *vertices* nil)
  (defvar *edges* nil)

  (defun test-create ()
    ;; A store needs a system directory to take type-ids from (GH #186).
    (setf *system-directory*
          (namestring (ensure-directories-exist "/var/tmp/graph-system/")))
    (setq *graph* (make-graph :test "/var/tmp/graph/"))
#|
    (setq *v1* (make-vertex :generic
                            '((:int . 1123213)
                              (:float . 1.0201023923452345)
                              (:float . 1.0201023111111111)
                              (:list . (1 a #\c "a" 1.0))
                              (:true . t)
                              (:nil . nil)
                              (:string . "barbar"))))
    (setq *v2* (make-vertex :generic
                            '((:blah . 9)
                              (:foo . "bar"))))
    (setq *e1* (make-edge :generic
                          (id *v1*)
                          (id *v2*)
                          2.33
                          '((:label . "test"))))
|#
;    (values (save-vertex *graph* *v1*)
;            (save-vertex *graph* *v2*)
;            (save-edge *graph* *e1*))
    ;;(close-graph *graph*)
    (values *v1* *v2* *e1*)
    )

  (defun change-test ()
    (let ((v (copy-vertex *v1*)))
      (setf (cdr (assoc :int (data v))) 1)
      (save-vertex v)))

  (defun vertex-test ()
    (setq *vertices* nil)
    (format t "Vertices")
    (dotimes (i 10000)
      (when (= 0 (mod i 1000))
        (format t ".")
        (force-output))
      (push (make-vertex :generic `((:count . ,(format nil "~8,'0D" i))))
            *vertices*))
    (terpri))

  (defun edge-test ()
    (setq *edges* nil)
    (format t "Edges")
    (let ((i 0))
      (loop until (= i 100000) do
           (when (= 0 (mod i 1000))
             (format t ".")
             (force-output))
           (let ((in (id (nth (random 5000) *vertices*)))
                 (out (id (nth (+ 5000 (random 5000)) *vertices*))))
             (unless (equalp in out)
               (incf i)
               (make-edge :generic
                          in out
                          (random 1.0)
                          `((:label . ,(format nil "~8,'0D" i))))))))
    (terpri))

  (defun stress-test ()
    (format t "Vertices")
    (dotimes (i 10000)
      (when (= 0 (mod i 1000))
        (format t ".")
        (force-output))
      (let ((v (make-vertex :generic `((:count . ,(format nil "~6,'0D" i))))))
        (push v *vertices*)
        (let ((v-count (length (map-vertices (lambda (v) v) *graph* :collect-p t))))
          (unless (= (1+ i) v-count)
            (dbg "Vertex count is off ~S != ~S~%~S" (1+ i) v-count v)))))
    (terpri)
    (format t "Edges")
    (let ((i 0))
      (loop until (= i 100000) do
           (when (= 0 (mod i 1000))
             (format t ".")
             (force-output))
           (let ((in (id (nth (random 5000) *vertices*)))
                 (out (id (nth (+ 5000 (random 5000)) *vertices*))))
             (unless (equalp in out)
               (incf i)
               (push (make-edge :generic
                                in out
                                (random 1.0)
                                `((:label . ,(format nil "~6,'0D" i))))
                     *edges*)))))
    (terpri))

;  (defun edge-test ()
;    (setq *vertices* nil)
;    (dotimes (i 10)
;      (push (make-vertex :generic `((:count . ,(format nil "~6,'0D" i))))
;            *vertices*))
;    (dotimes (i 100)
;      (push (make-edge :generic
;                       (id (nth (random 10) *vertices*))
;                       (id (nth (random 10) *vertices*))
;                       (random 1.0)
;                       `((:label . ,(format nil "~6,'0D" i))))
;            *edges*)))

  (defun test-open ()
    (setq *graph* (open-graph :test "/var/tmp/graph/")))

  (defun test-close ()
    (close-graph *graph*)))

(defun prof-graph ()
  (dotimes (i 1000)
    (incoming-edges *v2*)))
