;;;; Serialization & Binary Codec Subsystem Profiler Module
(in-package #:graph-db/profiler)

(defun profile-serialization-subsystem (&key (iterations 10000) (sprof-mode :cpu))
  "Profile object serialization, type encoding/decoding, and string fast paths."
  (let ((test-strings '("Short ASCII string"
                        "A slightly longer string for testing serialization performance"
                        "Very Long String with numbers 1234567890 and symbols !@#$%^&*()_+"))
        (test-objects (list 123456789
                            3.141592653589793d0
                            '(:symbol-key "string-value" 98765)
                            (vector 1 2 3 4 5 6 7 8 9 10))))
    (profile-block (:name (format nil "Serialization & Binary Codecs (~:D ops)" iterations)
                    :subsystems '(:serialization)
                    :sprof-mode sprof-mode)
      ;; 1. Primitive & compound object serialization / deserialization
      (dotimes (i iterations)
        (let ((obj (nth (mod i (length test-objects)) test-objects)))
          (let ((bytes (graph-db::serialize obj)))
            (graph-db::deserialize bytes))))

      ;; 2. Fast ASCII string encoding / decoding
      (dotimes (i iterations)
        (let* ((str (nth (mod i (length test-strings)) test-strings))
               (bytes (sb-ext:string-to-octets str :external-format :utf-8)))
          (graph-db::%octets-to-string-fast bytes)))

      ;; 3. View key serialization / deserialization
      (let ((node-id (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))
        (dotimes (i iterations)
          (let* ((key-list (list "view-key-name" node-id))
                 (seq (graph-db::view-key-serialize key-list))
                 (byte-seq (coerce seq '(simple-array (unsigned-byte 8) (*)))))
            (graph-db::view-key-deserialize byte-seq)))))))



