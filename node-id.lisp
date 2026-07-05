(in-package :graph-db)

;; NOTE: (make-random-state) with a NIL arg copies the *current* *random-state*; it does
;; NOT reseed from the clock, so both elements below are identical regardless of any delay
;; between them.  A former (sleep 1) between the two calls therefore bought nothing but a
;; full second of load-time latency -- run at module-init, it added ~1 s to every image
;; boot (device AND hub).  Removed.  (If genuinely-distinct entropy is ever wanted here,
;; use (make-random-state t) per element -- a behavioural change, tracked separately.)
(let ((random-states (list (make-random-state t)
                           (make-random-state t))))

  (defun generate-uuid-name ()
    "Generate a byte array for V5 UUID generation using time and random bytes"
    (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (sec msec)
        #+sbcl (sb-ext:get-time-of-day)
        #-sbcl (%posix-gettimeofday)
        (let* ((total-bytes 40)
               (vec (make-array total-bytes :element-type '(unsigned-byte 8)))
               (offset 0))
          (let ((n-bytes (ceiling (integer-length sec) 8)))
            (dotimes (i n-bytes)
              (setf (aref vec offset) (ldb (byte 8 (* i 8)) sec))
              (incf offset)))
          (let ((n-bytes (ceiling (integer-length msec) 8)))
            (dotimes (i n-bytes)
              (setf (aref vec offset) (ldb (byte 8 (* i 8)) msec))
              (incf offset)))
          (loop for i from offset below total-bytes do
               (setf (aref vec i) (random 256 (nth (random 2) random-states))))
          vec))))

(defun gen-v5-uuid (namespace)
  "Generates a version 5 (name based SHA1) uuid.  Code stolen from the UUID library."
  (declare (optimize (speed 3) (safety 0)))
  (let ((name (generate-uuid-name))
        (digester (ironclad:make-digest :sha1)))
    (ironclad:update-digest digester namespace)
    (ironclad:update-digest digester name)
    (let ((hash (ironclad:produce-digest digester)))
      (let ((id (subseq hash 0 16)))
        (let ((time-high
               (dpb #b0101
                    (byte 4 12)
                    (logior (ash (aref hash 6) 8)
                            (aref hash 7)))))
          (loop for i from 7 downto 6
             do (setf (aref id i)
                      (ldb (byte 8 (* 8 (- 7 i))) time-high)))
          (setf (aref id 8)
                (dpb #b10 (byte 2 6) (aref hash 8)))
          id)))))

(declaim (inline gen-edge-id))
(defun gen-edge-id ()
  (gen-v5-uuid *edge-namespace*))

(declaim (inline gen-vertex-id))
(defun gen-vertex-id ()
  (gen-v5-uuid *vertex-namespace*))
