;;;; Tests for the binary (de)serialization layer (serialize.lisp).

(in-package #:graph-db/test)

(def-suite serialize-suite
  :description "serialize / deserialize round trips."
  :in graph-db-suite)

(in-suite serialize-suite)

(defun deserialized (object)
  "Serialize OBJECT and deserialize the result, returning the round-tripped
value (DESERIALIZE's primary value only)."
  (values (deserialize (serialize object))))

(test integers
  "Positive, negative, zero, byte boundaries and bignums round-trip."
  (dolist (n (list 0 1 -1 42 -42 127 128 255 256 -256 65535 65536
                   1099511627775
                   12345678901234567890
                   -98765432109876543210))
    (is (eql n (deserialized n))
        "integer ~A did not round-trip (got ~A)" n (deserialized n))))

(test single-floats
  (dolist (f (list 0.0f0 1.0f0 -1.0f0 3.14159f0 -2.71828f0
                   most-positive-single-float most-negative-single-float))
    (is (= f (deserialized f)))))

(test double-floats
  (dolist (f (list 0.0d0 1.0d0 -1.0d0 3.141592653589793d0
                   most-positive-double-float most-negative-double-float))
    (is (= f (deserialized f)))))

(test characters
  (dolist (c (list #\a #\Z #\Space #\Newline (code-char 955) (code-char 0)))
    (is (eql c (deserialized c)))))

(test strings
  (dolist (s (list "" "hello" "with spaces and 123"
                   "unicode: héllo café ☕ λ"))
    (is (string= s (deserialized s)))))

(test booleans-and-nil
  (is (eq t (deserialized t)))
  (is (eq nil (deserialized nil)))
  (is (eq nil (deserialized '()))))

(test keywords
  (dolist (k (list :foo :bar :|Mixed Case| :||))
    (is (eq k (deserialized k)))))

(test symbols
  (dolist (s (list 'cl:list 'cl-user::some-symbol 'graph-db/test::another))
    (is (eq s (deserialized s)))))

(test proper-lists
  (dolist (l (list '() '(1) '(1 2 3)
                   '(1 "two" :three #\4 5.0d0)
                   '(1 (2 (3 (4))) 5)))
    (is (equal l (deserialized l)))))

(test dotted-lists
  (is (equal (cons 1 2) (deserialized (cons 1 2))))
  (is (equal '(1 2 . 3) (deserialized '(1 2 . 3)))))

(test general-vectors
  (dolist (v (list #() #(1 2 3) #(:a "b" 3)))
    (is (equalp v (deserialized v)))))

(test bit-vectors
  (dolist (bv (list #*1 #*0 #*101010 #*1111111100000000))
    (is (equal bv (deserialized bv)))))

(test uuids
  "A UUID object round-trips: serialize -> deserialize yields an equivalent
UUID (compared by string form, since uuid objects aren't EQUAL)."
  (let ((u (uuid:make-v4-uuid)))
    (is (string= (princ-to-string u)
                 (princ-to-string (deserialized u))))
    ;; and the serialization is stable / byte-identical on a second pass
    (is-true (serialized-equal (serialize u)
                               (serialize (deserialized u))))))

(test timestamps
  (let ((ts (local-time:now)))
    (is-true (local-time:timestamp= ts (deserialized ts)))))

(test nested-heterogeneous
  "A realistic mixed structure round-trips intact."
  (let ((obj (list :id 12345
                   :name "café"
                   :tags #(:a :b :c)
                   :scores '(1.5d0 -2.0d0)
                   :flag t
                   :nada nil
                   :pair (cons "k" 99))))
    ;; equalp (not equal) so the nested #(:a :b :c) vector compares elementwise
    (is (equalp obj (deserialized obj)))))

(test serialize-returns-octet-vector
  (let ((bytes (serialize "anything")))
    (is-true (typep bytes '(vector (unsigned-byte 8))))))

(defun fv (&rest floats)
  "A (simple-array single-float (*)) built from FLOATS."
  (make-array (length floats) :element-type 'single-float
                              :initial-contents (mapcar (lambda (x) (coerce x 'single-float))
                                                        floats)))

(test float-vector-header
  "A serialized float vector carries the float-vector tag and a correct payload length."
  (let ((bytes (serialize (fv 1.0 2.0 3.0))))
    (is (= +float-vector+ (aref bytes 0)))
    ;; header is [tag][n][length bytes...]; payload is 1 type byte + 3*4 float bytes
    (let* ((n (aref bytes 1))
           (header-length (+ 2 n))
           (payload-length (decode-length (subseq bytes 2 header-length))))
      (is (= 13 payload-length))
      (is (= (+ header-length 13) (length bytes)))
      (is (= +fv-single-float+ (aref bytes header-length))))))

(test float-vector-round-trip
  "Float vectors round-trip exactly, preserving element type and dimension."
  (dolist (v (list (fv)
                   (fv 0.0)
                   (fv 1.0 -1.0 0.5 -0.5)
                   (fv 3.14159 -2.71828 1.0e10 -1.0e-10)))
    (let ((back (deserialized v)))
      (is (typep back '(simple-array single-float (*)))
          "round-tripped value has the wrong type: ~S" (type-of back))
      (is (= (length v) (length back)))
      (dotimes (i (length v))
        (is (= (aref v i) (aref back i))
            "element ~A differs: ~A vs ~A" i (aref v i) (aref back i))))))

(test float-vector-extremes
  "Boundary float32 values survive the round trip bit-exactly."
  (let ((v (fv most-positive-single-float most-negative-single-float
               least-positive-single-float least-negative-single-float)))
    (let ((back (deserialized v)))
      (dotimes (i (length v))
        (is (= (aref v i) (aref back i)))))))

(test float-vector-large-dimension
  "A realistic embedding dimension round-trips (exercises multi-byte lengths)."
  (let ((v (make-array 1536 :element-type 'single-float)))
    (dotimes (i 1536)
      (setf (aref v i) (coerce (/ (- i 768) 768.0) 'single-float)))
    (let ((back (deserialized v)))
      (is (= 1536 (length back)))
      (is (every #'= v back)))))

(test float-vector-rejects-misaligned-payload
  "A corrupt payload errors rather than silently decoding to a short vector."
  (let ((bytes (serialize (fv 1.0 2.0))))
    ;; drop one trailing byte: payload is now 8 bytes after the type byte, not 9
    (let ((truncated (subseq bytes 0 (1- (length bytes)))))
      (signals error (deserialize truncated)))))

(test float-vector-nan-and-infinity-behaviour
  "Pin whatever ieee-floats does with non-finite values, so a later change to
that library cannot alter stored data silently. Embeddings must never contain
these -- AS-EMBEDDING rejects them at ingest (Task 4) -- but the codec's
behaviour should still be known rather than assumed.

Probed directly against this repo's live serialize/deserialize path (SBCL
2.5.5, 2026-07-20): IEEE-FLOATS:ENCODE-FLOAT32 signals a SIMPLE-ERROR (\"Can't
decode NaN or infinity: ...\") for both single-float-positive-infinity and a
quiet NaN.  That happens at the ENCODE step inside SERIALIZE -- never even
reaching the DESERIALIZE-HELP decoder added in this task -- and SERIALIZE's
:AROUND method wraps it into a GRAPH-DB:SERIALIZATION-ERROR.  So the codec does
not silently corrupt non-finite data: it refuses to store it at all."
  (let ((inf #+sbcl sb-ext:single-float-positive-infinity
             #+ecl si:single-float-positive-infinity))
    (let ((v (make-array 1 :element-type 'single-float :initial-element inf)))
      (signals serialization-error (deserialized v)))))

(test generic-vectors-unaffected
  "Non-single-float vectors still take their existing paths."
  (let ((tv (vector 1 "two" :three)))
    (is (equalp tv (deserialized tv))))
  ;; A (unsigned-byte 8) vector is NOT round-tripped through SERIALIZE/DESERIALIZE
  ;; -- that is not a bug, it's the contract.  SERIALIZE treats such a vector as
  ;; "these are already the bytes to store" and returns it unchanged (EQ, not a
  ;; copy): it has no tag/length header of its own, so DESERIALIZE cannot be
  ;; called on it directly.  Two independent call sites already depend on this
  ;; identity behaviour: memory-graph.lisp:472-485 (NI-VAL/RI-VAL) stores byte
  ;; arrays raw under its own tag rather than routing them through SERIALIZE's
  ;; header format, and edge.lisp:225 serializes already-byte data and writes
  ;; the (identical) result straight to the heap.  There is no wrapped-blob
  ;; codec to round-trip against: +blob+ (globals.lisp) has no encoder or
  ;; decoder anywhere in the codebase.
  (let ((bv (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3))))
    (is (eq bv (serialize bv)))))
