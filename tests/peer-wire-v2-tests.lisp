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

(defun %wv2-plist (g &key (protocol graph-db::*peer-protocol-version* protocol-p)
                        (omit-protocol nil)
                        (schema-major (first (graph-db::peer-schema-version g)))
                        (schema-minor (second (graph-db::peer-schema-version g))))
  "A device auth plist against hub G: good by default (matches *PEER-PROTOCOL-
VERSION* and G's own schema version); callers pass OMIT-PROTOCOL or PROTOCOL
to build the bad variants under test."
  (declare (ignore protocol-p))
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
          "bad protocol + good schema must surface the PROTOCOL error, got ~S" c))))

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
          "bad schema + good protocol must surface the SCHEMA error, got ~S" c))))
