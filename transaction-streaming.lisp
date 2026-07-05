(in-package :graph-db)

(defvar *replication-protocol-version* 2)

;;; Catching EPIPE: ugh. But I don't think there's a better way on
;;; SBCL
(defun broken-pipe-error-p (condition)
  (and (typep condition 'simple-condition)
       (equalp (last (simple-condition-format-arguments condition))
               '("Broken pipe"))))

(deftype broken-pipe-error ()
  '(satisfies broken-pipe-error-p))

(defun check-packet-type (packet type-code)
  (let ((type-code-offset 9))
    (assert (<= type-code-offset (length packet)))
    (assert (= (aref packet type-code-offset)
               type-code))))

(define-condition replication-network-error (error) ())
(define-condition short-socket-read-error (replication-network-error) ())
(define-condition eof-socket-error (replication-network-error) ())

(alexandria:define-constant +replication-buffer-size+ 4096)

(defun simple-socket-read (socket buffer length &key (eof-error-p t))
  "Read LENGTH octets from SOCKET into BUFFER starting at index 0. If
  the number of bytes read is zero, signals an error, if EOF-ERROR-P
  is true, otherwise returns NIL. If more than zero are read, but
  fewer than LENGTH, signals a SHORT-SOCKET-READ-ERROR error. Returns
  the number of octets read."
  (let ((bytes-read (read-sequence buffer (usocket:socket-stream socket)
                                   :end length)))
    (cond ((zerop bytes-read)
           (if eof-error-p
               (error 'eof-socket-error)
               nil))
          ((< bytes-read length)
           (error 'short-socket-read-error))
          ((= bytes-read length)
           bytes-read)
          (t
           (error "Unexpected result from socket read-sequence -- ~
                   ~A bytes read, wanted ~A" bytes-read length)))))

(defun read-packet (socket &key (eof-error-p t))
  ;; TODO: Accept a buffer argument to avoid consing?
  (let* ((buffer (make-byte-vector +replication-buffer-size+)))
    (simple-socket-read socket buffer 8 :eof-error-p eof-error-p)
    ;; TODO: Sanity check packet size?
    (let* ((packet-size (deserialize-uint64 buffer 0))
           (packet (make-byte-vector packet-size))
           (packet-offset 0))
      (flet ((add-to-packet (buffer packet length)
               (replace packet buffer :start1 packet-offset
                        :end2 length)
               (incf packet-offset length)))
        (add-to-packet buffer packet 8)
        (multiple-value-bind (whole-buffers remainder)
            (truncate (- packet-size 8) +replication-buffer-size+)
          (dotimes (i whole-buffers)
            (simple-socket-read socket buffer +replication-buffer-size+)
            (add-to-packet buffer packet +replication-buffer-size+))
          (when (plusp remainder)
            (simple-socket-read socket buffer remainder)
            (add-to-packet buffer packet remainder))))
      packet)))

(defun write-packet (packet socket &key (start 0) end)
  (write-sequence packet (usocket:socket-stream socket)
                  :start start
                  :end end)
  (force-output (usocket:socket-stream socket)))


;;; Plist packets (for auth and whatever else)

(define-condition plist-too-fancy-error (error)
  ((plist
    :initarg :plist
    :reader plist-to-fancy-plist)))

(deftype unfancy-plist-element ()
  '(or number string keyword boolean))

(defun unfancy-plist-element-p (object)
  (typep object 'unfancy-plist-element))

(defun check-packet-plist (plist)
  (unless (every #'unfancy-plist-element-p plist)
    (error 'plist-too-fancy-error)))

(defun serialize-packet-plist (plist)
  (check-packet-plist plist)
  (with-standard-io-syntax
    (babel:string-to-octets (write-to-string plist)
                            :encoding :utf-8)))

(defun deserialize-packet-plist (octets &key (offset 0))
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (values (read-from-string (babel:octets-to-string octets
                                                        :encoding :utf-8
                                                        :start offset))))))

;;; 8 bytes of size, 1 byte flag, 1 byte type, payload.

(alexandria:define-constant +plist-packet-header-size+ (+ 8 1 1))
(alexandria:define-constant +plist-packet-type-code+
    (char-code #\p))

(defun serialize-plist-packet (plist)
  (let* ((payload (serialize-packet-plist plist))
         (size (+ (length payload) +plist-packet-header-size+))
         (packet (make-byte-vector size))
         (offset 0))
    (serialize-uint64 packet size offset)
    (incf offset 8)
    ;; Flags (unused)
    (setf (aref packet offset) 0)
    (incf offset)
    ;; Type
    (setf (aref packet offset) +plist-packet-type-code+)
    (incf offset)
    ;; Payload
    (replace packet payload :start1 offset)
    packet))

(defun deserialize-plist-packet (packet)
  (check-packet-type packet +plist-packet-type-code+)
  (deserialize-packet-plist packet :offset 10))

(defun read-plist-packet (socket &key (eof-error-p t))
  (let ((packet (read-packet socket :eof-error-p eof-error-p)))
    (deserialize-plist-packet packet)))

(defun write-plist-packet (plist socket)
  (write-packet (serialize-plist-packet plist) socket))

;;;
;;; Slave/master communication
;;;
;;; The master accepts new connections on the replication port.
;;;
;;; An incoming slave connection gets an initial packet from the
;;; master that provides the master's protocol version, graph name,
;;; and schema digest.
;;;
;;; If the slave can't match all three, the slave disconnects
;;; immediately.
;;;
;;; Otherwise, the slave sends a reply packet with the replication key
;;; and highest transaction id it has. This is the last communication
;;; from the slave to the master.
;;;
;;; If the master accepts the replication key, it first instantly
;;; streams every transaction in its replication logs higher than the
;;; slave highest transaction id.
;;;
;;; Then the master waits for new transactions and sends them as they
;;; happen to the slave.
;;;
;;; The master and slave may disconnect from each other without
;;; communication at any time. Each communication loop only continues
;;; as long as STOP-REPLICATION-P is false.
;;;
;;; The master accept loop, the master transaction streaming loop, and
;;; the slave receiving loop all run in their own threads. The slave
;;; thread runs in its own separate process.

(defgeneric handshake-plist (graph)
  (:documentation "Return the plist of handshake info for GRAPH.")
  (:method (graph)
    (list :name (symbol-name (graph-name graph))
          :schema-digest (schema-digest
                          (schema graph))
          :protocol-version
          *replication-protocol-version*)))

(define-condition invalid-auth-data-error (error) ())

(define-condition invalid-handshake-data-error (error)
  ((field
    :initarg :field
    :reader invalid-handshake-data-error-field)
   (expected
    :initarg :expected
    :reader invalid-handshake-data-error-expected)
   (actual
    :initarg :actual
    :reader invalid-handshake-data-error-actual))
  (:report
   (lambda (condition stream)
     (format stream "Invalid handshake value ~S for ~S, expected ~S"
             (invalid-handshake-data-error-actual condition)
             (invalid-handshake-data-error-field condition)
             (invalid-handshake-data-error-expected condition)))))

(defclass slave-session ()
  ((socket
    :initarg :socket
    :reader socket)
   (mailbox
    :initarg :mailbox
    :reader mailbox)
   (highest-transaction-id
    :initarg :highest-transaction-id
    :accessor highest-transaction-id))
  (:default-initargs
   :socket nil
    :mailbox (make-mailbox)
    :highest-transaction-id 0))

(defgeneric add-slave-session (session graph)
  (:method (session graph)
    (with-recursive-lock-held ((slaves-lock graph))
      (pushnew session (slaves graph)))))

(defgeneric remove-slave-session (session graph)
  (:method (session graph)
    (with-recursive-lock-held ((slaves-lock graph))
      (setf (slaves graph) (remove session (slaves graph))))))

(defgeneric broadcast-to-slaves (object graph)
  (:method (object graph)
    (dolist (session (slaves graph))
      (send-message (mailbox session) object))))

(defmethod replicate-transaction (tx graph)
  (broadcast-to-slaves tx graph))

(defgeneric end-all-sessions (tx graph)
  (:method (tx graph)
    (broadcast-to-slaves :shutdown graph)))

(defgeneric check-client-auth-plist (plist graph)
  (:documentation "Check the validity of the client
  authentication/metadata plist PLIST against GRAPH.")
  (:method (plist graph)
    (unless (equal (getf plist :replication-key)
                   (replication-key graph))
      (error 'invalid-auth-data-error))))

(defgeneric initiate-slave-session (graph socket)
  (:method (graph socket)
    (write-plist-packet (handshake-plist graph) socket)
    (let ((auth-plist (read-plist-packet socket)))
      (check-client-auth-plist auth-plist graph)
      (make-instance 'slave-session
                     :socket socket
                     :highest-transaction-id
                     (getf auth-plist :highest-transaction-id)))))

(defgeneric stream-transaction-to-slave (tx session)
  (:method (tx session)
    (log:debug "Writing ~D bytes to slave" (length (bytes tx)))
    (write-packet (bytes tx) (socket session))))

(defun make-slave-session-handler (graph socket)
  "Returns a function that can be called to handle a new slave session
on SOCKET."
  (lambda ()
    (log:info "Starting slave session")
    (let* ((session (initiate-slave-session graph socket))
           (mailbox (mailbox session)))
      (log:info "Slave session ~A initiated" session)
      (add-slave-session session graph)
      (handler-case
          (unwind-protect
               (progn
                 (stream-logged-transactions graph session)
                 (loop
                    (when (stop-replication-p graph)
                      (return))
                    (let ((tx (receive-message mailbox :timeout 1)))
                      (case tx
                        ((nil)
                         ;; Hit timeout, do nothing
                         )
                        ((:shutdown)
                         (log:info "Shutting down slave ~A" session)
                         (return))
                        (t
                         (handler-case
                             (stream-transaction-to-slave tx session)
                           (broken-pipe-error ()
                             (return))))))))
            (log:info "Cleaning up slave session ~A" session)
            (remove-slave-session session graph)
            (ignore-errors (usocket:socket-close (socket session))))
        (error (c)
          (log:error "~A erred: ~A" session c))))))

(defun server-accept-loop (graph)
  (let ((port (replication-port graph))
        (address usocket:*wildcard-host*))
    ;; :element-type on the LISTENER (not just socket-accept): on CCL the
    ;; accepted socket inherits the listener's element-type, so without this the
    ;; accepted stream is a character stream and binary read-sequence on the
    ;; replication packets signals a type error.
    (usocket:with-socket-listener (listener address port :reuse-address t
                                            :element-type '(unsigned-byte 8))
      (loop
         (when (stop-replication-p graph)
           (return))
         (when (usocket:wait-for-input listener :timeout 1 :ready-only t)
           (let ((socket (usocket:socket-accept listener
                                                :element-type '(unsigned-byte 8))))
             (log:debug "Got a connection: ~A" socket)
             (bordeaux-threads:make-thread
              (make-slave-session-handler graph
                                          socket)
              :name "slave session")))))))


(defgeneric check-handshake-plist (plist graph)
  (:documentation "Check the validity of the handshake plist for
  GRAPH. Called by the slave.")
  (:method (plist graph)
    (let ((valid-plist (handshake-plist graph)))
      (dolist (field '(:name :schema-digest :protocol-version) t)
        (let ((expected (getf valid-plist field))
              (actual (getf plist field)))
          (unless (equal expected actual)
            (error 'invalid-handshake-data-error
                   :field field
                   :expected expected
                   :actual actual)))))))


;;; Slave connecting to master

(defgeneric auth-plist (graph)
  (:documentation "Return the authentication/metadata plist for slave
  graph GRAPH.")
  (:method (graph)
    (list :replication-key (replication-key graph)
          :highest-transaction-id
          (load-highest-transaction-id graph))))

(defgeneric initiate-master-session (graph socket)
  (:method (graph socket)
    (let ((handshake-plist (read-plist-packet socket)))
      (log:debug "Got handshake plist ~A" handshake-plist)
      (check-handshake-plist handshake-plist graph)
      (write-plist-packet (auth-plist graph) socket))))

(defgeneric stream-transaction-to-disk (tx-header-vector socket graph)
  (:method (tx-header-vector socket graph)
    (let* ((tx-header (deserialize-tx-header-vector tx-header-vector))
           (transaction (make-instance 'replicated-transaction
                                       :graph graph
                                       :transaction-id
                                       (transaction-id tx-header))))
      (let* ((permanent-name (transaction-pathname transaction))
             (temporary-name (make-pathname :type "txn-replica-tmp"
                                            :defaults permanent-name)))
        (log:debug "Saving ~A to ~A" tx-header permanent-name)
        (with-open-file (stream temporary-name
                                :direction :output
                                :if-exists :error
                                :element-type '(unsigned-byte 8))
          (write-sequence tx-header-vector stream)
          (dotimes (i (write-count tx-header))
            (let ((packet (read-packet socket)))
              (write-sequence packet stream)
              (push (deserialize-tx-write-vector packet)
                    (writes transaction)))))
        (rename-file temporary-name permanent-name)
        (setf (writes transaction) (nreverse (writes transaction)))
        transaction))))

(deftype retryable-network-error ()
  "A network connection attempt that signals one of these errors may
  be retried."
  '(or
    usocket:ns-host-not-found-error
    usocket:connection-refused-error usocket:network-unreachable-error
    usocket:host-unreachable-error usocket:network-down-error
    usocket:host-down-error usocket:timeout-error))

(defun connect-slave-to-master (host port &key (attempts 10))
  "Connect to the master server for GRAPH. On successful network
connection, authenticates and returns the socket. In the event of a
retryable network error, retries with an exponential retry timeout."
  (let ((timeout 1)
        (attempt 1))
    (loop
      (when (<= attempts attempt)
        (error "Failed to connect to master ~A:~A after ~A attempts"
               host port attempts))
      (handler-case
          (let* ((socket (usocket:socket-connect host port
                                                 :element-type '(unsigned-byte 8)))
                 (stream (usocket:socket-stream socket)))
            (unless (open-stream-p stream)
              (error "Unable to connect"))
            (return socket))
        (retryable-network-error (condition)
          (log:info "Slave connection attempt ~A failed with ~A, ~
                     retrying in ~A second~:P"
                    attempt
                    condition
                    timeout)
          (sleep timeout)
          (incf timeout timeout)
          (incf attempt))
        (error (condition)
          (error "Failed to connect to master ~A:~A: ~A"
                 host port condition))))))

(defgeneric slave-loop (graph)
  (:documentation
   "This function is called in a slave to connect to a master and
   receive transaction packets.")
  (:method (graph)
    ;; Bind *graph* for this slave thread: the transaction deserializers
    ;; (deserialize-edge-head etc.) default their :graph arg to *graph* when
    ;; resolving node type-ids against the schema, and a fresh thread has it
    ;; unbound.
    (let ((*graph* graph)
          (host (master-host graph))
          (port (replication-port graph))
          socket)
      (flet ((connect ()
               (setf socket (connect-slave-to-master host port))
               (initiate-master-session graph socket)
               (log:info "Master session initiated")))
        (connect)
        (loop
           (when (stop-replication-p graph)
             (ignore-errors (usocket:socket-close socket))
             (log:info "Replication stopping")
             (return))
           (tagbody
            retry
              (when (usocket:wait-for-input socket :timeout 1 :ready-only t)
                (let ((packet (read-packet socket :eof-error-p nil)))
                  (when (null packet)
                    ;; On EOF, try to reconnect socket
                    (connect)
                    (go retry))
                  (check-packet-type packet +tx-header-type-code+)
                  (let ((transaction (stream-transaction-to-disk packet
                                                                 socket
                                                                 graph)))
                    (apply-transaction transaction graph)
                    (mark-as-committed (transaction-pathname transaction)))))))))))


;;; Starting/stopping replication

;; Base no-op start/stop-replication methods on the plain GRAPH class moved to
;; graph-class.lisp (graph-db/core) so the embeddable core can open a graph
;; without this network-transport file.  Only the master/slave methods remain.
(defmethod start-replication ((graph master-graph) &key (package :graph-db))
  (declare (ignore package))
  (setf (stop-replication-p graph) nil)
  (setf (replication-listener graph)
        (bordeaux-threads:make-thread
         ;; A closure, not 'server-accept-loop + :arguments: bordeaux-threads
         ;; make-thread takes only :name, not the sb-thread :arguments key.
         (lambda () (server-accept-loop graph))
         :name (format nil "server listener for ~A"
                       (graph-name graph)))))

(defmethod stop-replication ((graph master-graph))
  (setf (stop-replication-p graph) t)
  (broadcast-to-slaves :shutdown graph)
  (let ((listener (replication-listener graph)))
    (when (and (threadp listener)
               (thread-alive-p listener))
      (join-thread listener))))

(defmethod start-replication ((graph slave-graph) &key package)
  (declare (ignore package))
  (setf (stop-replication-p graph) nil)
  (setf (slave-thread graph)
        (bordeaux-threads:make-thread
         ;; A closure, not 'slave-loop + :arguments (see start-replication
         ;; master): bordeaux-threads make-thread takes only :name.
         (lambda () (slave-loop graph))
         :name (format nil "~A-repl-slave-thread" (graph-name graph)))))

(defmethod stop-replication ((graph slave-graph))
  (setf (stop-replication-p graph) t)
  (when (and (threadp (slave-thread graph))
             (thread-alive-p (slave-thread graph)))
    (join-thread (slave-thread graph))))
