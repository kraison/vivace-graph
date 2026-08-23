;;;; The wire-v2 version gate, hub side (GH #206).
;;;;
;;;; *PEER-PROTOCOL-VERSION* bumps 1 -> 2.  The reference DEVICE already refused
;;;; a mismatched hub pre-auth (peer-streaming.lisp, PEER-SYNC) -- that half of
;;;; the gate needed no change.  What #206 actually reports missing is the other
;;;; direction: the HUB never checked the device's protocol version at all, so a
;;;; stale v1 device could push v1-shaped heads at a v2 hub and get misparsed
;;;; instead of refused.  PEER-AUTHENTICATE-DEVICE now checks first, before the
;;;; schema gate and before any device-registration side effect.
;;;;
;;;; Drives PEER-AUTHENTICATE-DEVICE directly with hand-built auth plists --
;;;; no socket needed, since the question is purely "does this function refuse
;;;; before it mutates," not a wire-encoding question.

(in-package #:graph-db/test)

(def-suite peer-wire-v2-suite
  :description "The hub-side protocol-version gate (GH #206)."
  :in graph-db-suite)

(in-suite peer-wire-v2-suite)

(defparameter *wv2-hub-origin* (id16 41))
(defparameter *wv2-dev-origin* (id16 42))

(defmacro with-wv2-hub ((g) &body body)
  "A hub peer-graph with shared REPLICATION-KEY \"k\" and one registered device
at *WV2-DEV-ORIGIN* (no per-device key, so it falls back to the hub's)."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *integration-graph-name* (namestring dir)
                           :peer-role :hub :origin-id *wv2-hub-origin*
                           :replication-port 0 :replication-key "k"
                           :buffer-pool-size 1000)))
       (unwind-protect
            (let ((*graph* ,g))
              (graph-db::register-peer-device ,g :origin-id *wv2-dev-origin*)
              ,@body)
         (close-graph ,g :snapshot-p nil)))))

(defun %wv2-plist (g &key (protocol graph-db::*peer-protocol-version*)
                       (omit-protocol nil)
                       (schema-major
                        (first (graph-db::peer-schema-version g)))
                       (schema-minor
                        (second (graph-db::peer-schema-version g))))
  "A device auth plist against hub G: good by default (matches *PEER-PROTOCOL-
VERSION* and G's own schema version); callers pass OMIT-PROTOCOL or PROTOCOL
to build the bad variants under test."
  (append
   (list :origin-id (graph-db::peer-id->hex *wv2-dev-origin*)
         :replication-key "k"
         :schema-major schema-major
         :schema-minor schema-minor)
   (unless omit-protocol (list :peer-protocol-version protocol))))

(test hub-authenticates-a-good-v2-plist
  "Control: a plist with the current protocol version and a matching schema
version authenticates -- a guard that refused everything would pass the
refusal tests below for the wrong reason."
  (with-wv2-hub (g)
    (is (graph-db::peer-device-p
         (graph-db::peer-authenticate-device g (%wv2-plist g))))))

(test hub-refuses-auth-plist-without-protocol-key
  "A v1 device never sends :PEER-PROTOCOL-VERSION at all.  Absent must refuse,
not default to compatible -- the #206 misparse this gate exists to prevent."
  (with-wv2-hub (g)
    (let ((c (handler-case
                 (progn (graph-db::peer-authenticate-device
                         g (%wv2-plist g :omit-protocol t))
                        nil)
               (error (e) e))))
      (is (typep c 'graph-db::peer-protocol-mismatch-error)
          "expected PEER-PROTOCOL-MISMATCH-ERROR, got ~S" c)
      (when (typep c 'graph-db::peer-protocol-mismatch-error)
        (is (eql 2 (graph-db::peer-protocol-mismatch-hub c)))
        (is (null (graph-db::peer-protocol-mismatch-device c))
            "absent key reads back as NIL, not 0 or some other sentinel")))))

(test hub-refuses-a-wrong-protocol-version
  "A device that sends a stale (or too-new) version number, present but wrong."
  (with-wv2-hub (g)
    (let ((c (handler-case
                 (progn (graph-db::peer-authenticate-device
                         g (%wv2-plist g :protocol 1))
                        nil)
               (error (e) e))))
      (is (typep c 'graph-db::peer-protocol-mismatch-error)
          "expected PEER-PROTOCOL-MISMATCH-ERROR, got ~S" c)
      (when (typep c 'graph-db::peer-protocol-mismatch-error)
        (is (eql 1 (graph-db::peer-protocol-mismatch-device c)))))))

(test hub-orders-protocol-gate-before-schema-gate
  "Ordering pin: a BAD protocol + a GOOD schema version must fail as a protocol
error, not a schema error -- proving the protocol check runs first."
  (with-wv2-hub (g)
    (let ((c (handler-case
                 (progn (graph-db::peer-authenticate-device
                         g (%wv2-plist g :protocol 1))
                        nil)
               (error (e) e))))
      (is (typep c 'graph-db::peer-protocol-mismatch-error)
          "bad protocol + good schema must surface the PROTOCOL error, got ~S"
          c))))

(test hub-still-runs-schema-gate-behind-a-good-protocol
  "Ordering's other half: a GOOD protocol + a BAD schema major must still reach
and fail the existing schema gate -- the new check does not swallow it."
  (with-wv2-hub (g)
    (let ((c (handler-case
                 (progn (graph-db::peer-authenticate-device
                         g (%wv2-plist g :schema-major 99))
                        nil)
               (error (e) e))))
      (is (typep c 'graph-db::peer-schema-incompatible-error)
          "bad schema + good protocol must surface the SCHEMA error, got ~S"
          c))))

;;; ---------------------------------------------------------------------------
;;; #206 gap 2: the envelope's own header-size byte is checked against this
;;; image's current head layout, BEFORE any head byte is interpreted.  The
;;; protocol/schema gates above are the primary contract; this is defense in
;;; depth for a gate bypass (or a stale on-disk .txn file predating a head-size
;;; change) so it is refused, not misparsed. See DESERIALIZE-TRANSACTION-NODE-
;;; VECTOR in transactions.lisp.
;;; ---------------------------------------------------------------------------

(defun %wv2-fake-node-vector (kind header-size)
  "A hand-built transaction-node-vector envelope: KIND (:VERTEX or :EDGE), a
HEADER-SIZE byte, and exactly HEADER-SIZE zero bytes of 'head' -- no data.
Enough to drive DESERIALIZE-TRANSACTION-NODE-VECTOR's own size check without a
real node.  A vector this short cannot hold a current-size head, so a skipped
check would run GET-BYTE past the array end, not just decode wrong fields --
that is the ablation evidence for GH #206."
  (let* ((type-code (ecase kind
                      (:vertex graph-db::+transaction-node-vertex-code+)
                      (:edge   graph-db::+transaction-node-edge-code+)))
         (total (+ graph-db::+transaction-node-base-header-size+ header-size))
         (vec (make-array total :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (graph-db::serialize-uint64 vec total 0)
    (setf (aref vec 9) type-code)
    (setf (aref vec 26) header-size)
    vec))

(test hub-refuses-old-vertex-head-size
  "A hand-built envelope carrying the OLD (31-byte) vertex head size is
refused with NODE-HEAD-SIZE-MISMATCH-ERROR before any head byte is read."
  (let ((c (handler-case
               (progn (graph-db::deserialize-transaction-node-vector
                       (%wv2-fake-node-vector :vertex 31))
                      nil)
             (error (e) e))))
    (is (typep c 'graph-db::node-head-size-mismatch-error)
        "expected NODE-HEAD-SIZE-MISMATCH-ERROR, got ~S" c)
    (when (typep c 'graph-db::node-head-size-mismatch-error)
      (is (eql graph-db::+node-header-size+
               (graph-db::node-head-size-mismatch-expected c)))
      (is (eql 31 (graph-db::node-head-size-mismatch-actual c)))
      (is (eq :vertex (graph-db::node-head-size-mismatch-kind c))))))

(test hub-refuses-old-edge-head-size
  "Same check, edge side: the OLD (71-byte) edge head size is refused."
  (let ((c (handler-case
               (progn (graph-db::deserialize-transaction-node-vector
                       (%wv2-fake-node-vector :edge 71))
                      nil)
             (error (e) e))))
    (is (typep c 'graph-db::node-head-size-mismatch-error)
        "expected NODE-HEAD-SIZE-MISMATCH-ERROR, got ~S" c)
    (when (typep c 'graph-db::node-head-size-mismatch-error)
      (is (eql graph-db::+edge-header-size+
               (graph-db::node-head-size-mismatch-expected c)))
      (is (eql 71 (graph-db::node-head-size-mismatch-actual c)))
      (is (eq :edge (graph-db::node-head-size-mismatch-kind c))))))

(test hub-refuses-before-reading-any-head-byte
  "Ordering pin: the fake vertex envelope has zero bytes past its declared
(old) header -- if the size check ran AFTER any head interpretation started
(or not at all), reading the current 33-byte head layout from a 31-byte
buffer would run GET-BYTE off the end of the array, not signal our named
condition. It must be OUR condition, not an array-bounds error."
  (let ((c (handler-case
               (progn (graph-db::deserialize-transaction-node-vector
                       (%wv2-fake-node-vector :vertex 31))
                      nil)
             (error (e) e))))
    (is (typep c 'graph-db::node-head-size-mismatch-error)
        "check must fire before any head byte is read; got ~S" c)))

(test hub-applies-current-size-vertex-vector
  "Control: a REAL vertex, freshly loaded from disk (so BYTES reflects the
image's current head layout), round-trips through DESERIALIZE-TRANSACTION-
NODE-VECTOR untouched -- the new check does not refuse legitimate current
traffic."
  (with-test-graph (g)
    (let (id)
      (with-transaction ((graph-db::transaction-manager g))
        (setq id (id (make-g-person :name "X"))))
      (let* ((node (lookup-vertex id))
             (vector (graph-db::transaction-node-vector node))
             (decoded (graph-db::deserialize-transaction-node-vector vector)))
        (is (equalp id (id decoded)))))))

(test hub-applies-current-size-edge-vector
  "Control, edge side: a REAL edge round-trips through DESERIALIZE-
TRANSACTION-NODE-VECTOR untouched."
  (with-test-graph (g)
    (let (a b eid)
      (with-transaction ((graph-db::transaction-manager g))
        (setq a (id (make-g-person :name "A")))
        (setq b (id (make-g-person :name "B")))
        (setq eid (id (make-g-knows :from a :to b))))
      (let* ((edge (lookup-edge eid))
             (vector (graph-db::transaction-node-vector edge))
             (decoded (graph-db::deserialize-transaction-node-vector vector)))
        (is (equalp eid (id decoded)))))))
