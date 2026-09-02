;;;; Refused pushes (GH #151): an authored op the hub refuses DETERMINISTICALLY
;;;; -- a CONSTRAINT-VIOLATION -- is rejected and acknowledged past, never
;;;; re-streamed; a transient failure still propagates and is retried.  The
;;;; hub half drives %REHOME-OR-REJECT directly on a hub peer-graph; the
;;;; device half drives PEER-PUSH-PHASE over a real socket against a fake hub
;;;; that answers the ack with :REJECTED.  Reuses the PU-USER schema (a global
;;;; :UNIQUE email) and fixtures from peer-unique-tests.lisp.

(in-package #:graph-db/test)

(def-suite peer-rejection-suite
  :description "GH #151: deterministic refusals on the peer push path."
  :in graph-db-suite)

(in-suite peer-rejection-suite)

(defmacro with-pu-hub ((g) &body body)
  "A HUB peer-graph on the PU schema, bound to G and *graph*."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *pu-graph-name* (namestring dir)
                           :peer-role :hub :origin-id *pu-hub-origin*
                           :replication-port 0 :replication-key "k"
                           :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(test constraint-violations-share-one-superclass
  "The contract the push path relies on: every deterministic refusal is a
CONSTRAINT-VIOLATION; the transaction-level conflict is not."
  (dolist (c '(graph-db:unique-constraint-violation
               graph-db:value-constraint-violation
               graph-db:cardinality-violation
               graph-db:domain-range-violation
               graph-db:vector-dimension-violation))
    (is (subtypep c 'graph-db:constraint-violation) "~S" c))
  (is-false (subtypep 'graph-db::validation-conflict
                      'graph-db:constraint-violation)))

(test a-deterministically-refused-push-is-rejected-not-retried
  (with-pu-hub (g)
    (with-transaction () (make-pu-user :code "h1" :email "taken@x.com"))
    (let* ((op (pu-authored-create g 'pu-user
                                   '((:code . "d1") (:email . "taken@x.com"))
                                   *pu-dev-origin*))
           (opid (graph-db::peer-op-op-id op)))
      ;; The refusal itself is unchanged: REHOME-AUTHORED-OP still signals.
      (signals graph-db:constraint-violation
        (graph-db::rehome-authored-op g op))
      (multiple-value-bind (applied rejection)
          (graph-db::%rehome-or-reject g op 42)
        (is-false applied)
        (is-true rejection "the op is rejected, not propagated")
        (when rejection
          (is (equalp opid (graph-db:peer-rejection-op-id rejection)))
          (is (equalp *pu-dev-origin*
                      (graph-db:peer-rejection-origin rejection)))
          (is (= 42 (graph-db:peer-rejection-device-seq rejection)))
          (is (search "UNIQUE-CONSTRAINT-VIOLATION"
                      (graph-db:peer-rejection-condition rejection)))
          (is (search "taken@x.com" (graph-db:peer-rejection-message rejection))
              "the report names the offending value")))
      (is (= 1 (length (graph-db:get-peer-rejections g))))
      (is (= 1 (length (graph-db:get-peer-rejections
                        g :origin *pu-dev-origin*))))
      ;; The device re-streams before it has read the ack: recorded once.
      (graph-db::%rehome-or-reject g op 42)
      (is (= 1 (length (graph-db:get-peer-rejections g))))
      (is-false (graph-db::op-applied-p g opid)
                "a refused op is not marked applied")
      (is (probe-file (graph-db::peer-rejections-file g))
          "the record is durable on the hub"))))

(test a-transient-failure-still-propagates
  "Only a CONSTRAINT-VIOLATION is a rejection.  Anything else leaves PUSH-ACK
below the op, as before, so the device retries it."
  (with-pu-hub (g)
    (let ((op (pu-authored-create g 'pu-user
                                  '((:code . "d2") (:email . "e2@x.com"))
                                  *pu-dev-origin*))
          (orig (fdefinition 'graph-db::rehome-authored-op)))
      (setf (fdefinition 'graph-db::rehome-authored-op)
            (lambda (&rest args)
              (declare (ignore args))
              (error "simulated transient failure")))
      (unwind-protect
           (signals error (graph-db::%rehome-or-reject g op 7))
        (setf (fdefinition 'graph-db::rehome-authored-op) orig))
      (is (null (graph-db:get-peer-rejections g))))))

(test rejections-survive-a-reopen
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (opid (gen-id)))
      (let ((g (make-graph *pu-graph-name* path
                           :peer-role :device :origin-id *pu-dev-origin*
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
        (graph-db:record-peer-rejection
         g (graph-db::plist->peer-rejection
            (list :op-id (graph-db::peer-id->hex opid)
                  :origin (graph-db::peer-id->hex *pu-dev-origin*)
                  :lamport 3 :device-seq 11
                  :condition "UNIQUE-CONSTRAINT-VIOLATION"
                  :message "m" :at 1)))
        (close-graph g :snapshot-p nil))
      (let ((g (open-graph *pu-graph-name* path
                           :peer-role :device :origin-id *pu-dev-origin*
                           :peer-host "localhost" :replication-port 0)))
        (unwind-protect
             (let ((rs (graph-db:get-peer-rejections g)))
               (is (= 1 (length rs)))
               (is (equalp opid (graph-db:peer-rejection-op-id (first rs))))
               (is (= 11 (graph-db:peer-rejection-device-seq (first rs))))
               (is (equal "m" (graph-db:peer-rejection-message (first rs))))
               (graph-db:clear-peer-rejections g)
               (is (null (graph-db:get-peer-rejections g))))
          (close-graph g :snapshot-p nil))))))

;;; --- the device half, over a real socket ---------------------------------

(defun %rj-fake-hub (listener rejected)
  "Accept ONE connection on LISTENER, read the device's push up to its
:PUSH-END, and answer :PUSH-ACK for the highest seq seen with REJECTED
attached -- the hub's half of GH #151 without a hub in this image."
  (bordeaux-threads:make-thread
   (lambda ()
     (ignore-errors
      (unwind-protect
           (when (usocket:wait-for-input listener :timeout 20 :ready-only t)
             (let ((socket (usocket:socket-accept
                            listener :element-type '(unsigned-byte 8)))
                   (high 0))
               (unwind-protect
                    (progn
                      (loop
                        (let* ((packet (graph-db::read-packet socket))
                               (type (aref packet 9)))
                          (cond
                            ((= type graph-db::+peer-meta-type-code+)
                             (let ((txh (graph-db::deserialize-tx-header-vector
                                         (graph-db::read-packet socket))))
                               (dotimes (i (graph-db::write-count txh))
                                 (graph-db::read-packet socket))
                               (setf high
                                     (max high
                                          (graph-db::transaction-id txh)))))
                            ((= type graph-db::+plist-packet-type-code+)
                             (return)))))
                      (graph-db::peer-write-plist
                       (list :peer-control :push-ack :push-ack high
                             :rejected (graph-db::peer-rejections->string
                                        rejected))
                       socket))
                 (ignore-errors (usocket:socket-close socket)))))
        (ignore-errors (usocket:socket-close listener)))))
   :name "rj fake hub"))

(test the-device-records-rejections-from-the-push-ack
  "PEER-PUSH-PHASE over a socket: the hub's :REJECTED entries are recorded
on the device and PUSH-ACK still advances past them."
  (with-pu-device (g)
    (with-transaction () (make-pu-user :code "w1" :email "w1@x.com"))
    (let* ((seq (graph-db::load-highest-transaction-id g))
           (opid (gen-id))
           (rejected (list (list :op-id (graph-db::peer-id->hex opid)
                                 :origin (graph-db::peer-id->hex
                                          *pu-dev-origin*)
                                 :lamport 1 :device-seq seq
                                 :condition "UNIQUE-CONSTRAINT-VIOLATION"
                                 :message "simulated" :at 1)))
           (listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                            :element-type '(unsigned-byte 8)))
           (port (usocket:get-local-port listener)))
      (%rj-fake-hub listener rejected)
      (let ((socket (usocket:socket-connect "127.0.0.1" port
                                            :element-type '(unsigned-byte 8))))
        (unwind-protect
             (is (= seq (graph-db::peer-push-phase g socket))
                 "the ack advances PUSH-ACK past the refused op")
          (ignore-errors (usocket:socket-close socket))))
      (is (= seq (graph-db::load-peer-push-ack g)) "and it is persisted")
      (let ((rs (graph-db:get-peer-rejections g)))
        (is (= 1 (length rs)))
        (is (equalp opid (graph-db:peer-rejection-op-id (first rs))))
        (is (= seq (graph-db:peer-rejection-device-seq (first rs))))))))
