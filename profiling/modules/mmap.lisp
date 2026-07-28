;;;; MMap & Low-Level Memory Subsystem Profiler Module
(in-package #:graph-db/profiler)

(defun profile-mmap-subsystem (&key (iterations 10000) (sprof-mode :cpu))
  "Profile low-level memory allocation, byte access, and mmap operations."
  (let* ((temp-dir #P"/tmp/vg-profiler-mmap-test/")
         (_ (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))
         (graph (graph-db:make-graph :mmap-prof-graph temp-dir)))
    (declare (ignore _))
    (unwind-protect
         (let ((heap (graph-db::heap graph)))
           (profile-block (:name (format nil "MMap & Memory Storage Profiling (~:D operations)" iterations)
                           :subsystems '(:mmap-storage)
                           :sprof-mode sprof-mode)
             ;; 1. Single byte reads/writes
             (dotimes (i iterations)
               (let ((addr (+ 1034 (* (mod i 1000) 8))))
                 (graph-db::set-byte heap addr (mod i 256))
                 (graph-db::get-byte heap addr)))
             
             ;; 2. Multi-byte slice reads/writes
             (let ((buf (make-array 64 :element-type '(unsigned-byte 8) :initial-element 42)))
               (dotimes (i (floor iterations 10))
                 (let ((addr (+ 1034 (* (mod i 500) 64))))
                   (graph-db::set-bytes heap buf addr 64)
                   (graph-db::get-bytes heap addr 64))))

             
             ;; 3. Heap block allocations and frees
             (dotimes (i (floor iterations 100))
               (let ((ptr (graph-db::allocate heap 128)))
                 (when (plusp ptr)
                   (graph-db::free heap ptr))))))


      (graph-db:close-graph graph)
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore)))))
