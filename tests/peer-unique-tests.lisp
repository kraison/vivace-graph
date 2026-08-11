;;;; Unique constraints under peer replication (#6): the device pull-apply now
;;;; maintains the unique index (gap 1) and :ORIGIN-scoped values partition by the
;;;; node's authoring origin (gap 2).  These drive the real device path --
;;;; APPLY-PEER-AUTHORED-OP for a hand-built create -- against an on-disk peer graph
;;;; and assert: a pulled global-unique value is enforced against a later LOCAL
;;;; commit; an :ORIGIN value collides only WITHIN an origin (two devices may mint
;;;; the same value); and the per-node origin store survives close/open so RELEASE
;;;; recomputes the same key.

(in-package #:graph-db/test)

(def-suite peer-unique-suite
  :description "Unique constraints across peer replication (#6 gaps 1+2)."
  :in graph-db-suite)

(in-suite peer-unique-suite)

(defparameter *pu-graph-name* :graph-db-peer-unique-test)
(defparameter *pu-dev-origin* (id16 3) "This device's origin.")
(defparameter *pu-hub-origin* (id16 8) "A remote (other-device/hub) origin.")

(eval-when (:load-toplevel :execute)
  (setf (gethash *pu-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex pu-user ()
  ((code  :initarg :code  :accessor pu-code  :unique t :scope :origin)  ; partition by author
   (email :initarg :email :accessor pu-email :unique t)                 ; :local (global)
   (note  :initarg :note  :accessor pu-note))
  :graph-db-peer-unique-test)

;; Multi-slot: mirrors UQ-CLAIM's (ns ky) shape from
;; unique-constraint-tests.lisp, proving the pull-apply paths enforce a
;; tuple constraint too, not only a single-slot one (#107).
(def-vertex pu-claim ()
  ((ns :initarg :ns :accessor pu-claim-ns)
   (ky :initarg :ky :accessor pu-claim-ky :initform nil))
  :graph-db-peer-unique-test)

(def-unique pu-claim (ns ky) :graph-db-peer-unique-test)

(defmacro with-pu-device ((g) &body body)
  "An on-disk DEVICE peer-graph named *PU-GRAPH-NAME* bound to G and *graph*."
  `(with-temp-directory (dir)
     (let ((,g (make-graph *pu-graph-name* (namestring dir)
                           :peer-role :device :origin-id *pu-dev-origin*
                           :peer-host "localhost" :replication-port 0
                           :buffer-pool-size 1000)))
       (unwind-protect (let ((*graph* ,g)) ,@body)
         (close-graph ,g :snapshot-p nil)))))

(defun pu-authored-create (graph type data origin &key (lamport 5) (tx-id 9000))
  "An AUTHORED peer-op that CREATEs a fresh TYPE node with DATA, attributed to ORIGIN
(as if pulled from that author).  Returns (values op new-id).

Built via %MAKE-VERTEX, the engine's own node-construction primitive
(GH #135) -- not MAKE-<type>, which would CREATE-NODE the node into GRAPH via
its own implicit transaction (ENSURE-TRANSACTION) before
APPLY-PEER-AUTHORED-OP ever ran, defeating the point of these tests: proving
the peer-apply path itself populates the unique index, not the ordinary
local-commit path it's standing in for."
  (let* ((tid (graph-db::node-type-id
               (graph-db::lookup-node-type-by-name type :vertex :graph graph)))
         (nid (gen-id))
         (n (graph-db::%make-vertex :class type :id nid
                                    :type-id tid :revision 0)))
    (setf (graph-db::data n) data)
    (values (graph-db::make-peer-op
             :kind :authored :op-id (graph-db::gen-op-id) :origin origin
             :lamport lamport :tx-id tx-id
             :writes (list (make-instance 'graph-db::tx-create :node n)))
            nid)))

(test pulled-global-unique-is-enforced-locally
  "GAP 1: a pulled node's global (:LOCAL-scope) unique value lands in the device
index, so a later LOCAL commit reusing it is rejected -- enforcement is no longer
blind to replicated nodes."
  (with-pu-device (g)
    (is (graph-db::apply-peer-authored-op
         g (pu-authored-create g 'pu-user '((:code . "c1") (:email . "x@y.com"))
                               *pu-hub-origin*))
        "the pulled create applies")
    ;; email is global: the pulled x@y.com must now block a local duplicate.
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-pu-user :code "c2" :email "x@y.com")))
    ;; a distinct email still commits fine.
    (finishes (with-transaction () (make-pu-user :code "c3" :email "ok@y.com")))))

(test state-sync-create-maintains-index
  "GAP 1 via the STATE-SYNC path (APPLY-PEER-CREATE-WRITES, not the authored path):
a pulled node's global-unique value lands in the device index, so a later LOCAL
duplicate is rejected."
  (with-pu-device (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name 'pu-user :vertex :graph g)))
           ;; %MAKE-VERTEX, not a bare (MAKE-INSTANCE 'pu-user ...): see
           ;; PU-AUTHORED-CREATE's docstring above (GH #135).
           (n (graph-db::%make-vertex :class 'pu-user :id (gen-id)
                                      :type-id tid :revision 0)))
      (setf (graph-db::data n) '((:code . "s1") (:email . "sync@x.com")))
      (graph-db::apply-peer-create-writes
       g 7777 (list (make-instance 'graph-db::tx-create :node n)) *pu-hub-origin*))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-pu-user :code "s2" :email "sync@x.com")))))

(test purge-releases-unique-keys
  "PEER-PURGE-NODE releases a purged node's unique keys, so its value frees up -- the
index must not keep a stale holder that would falsely reject a reuse."
  (with-pu-device (g)
    (let ((vid (id (with-transaction () (make-pu-user :code "p1" :email "p@x.com")))))
      ;; while the holder is live, reusing its (global) email is rejected.
      (signals graph-db:unique-constraint-violation
        (with-transaction () (make-pu-user :code "p2" :email "p@x.com")))
      (graph-db::apply-peer-purge g (list vid))
      ;; after the purge the email is free again.
      (finishes (with-transaction () (make-pu-user :code "p3" :email "p@x.com"))))))

(test origin-scope-partitions-by-author
  "GAP 2: an :ORIGIN-scoped value collides only WITHIN an origin.  A pulled node and
a local node may share a code (different authors -> different partitions), but two
local nodes may not."
  (with-pu-device (g)
    (is (graph-db::apply-peer-authored-op
         g (pu-authored-create g 'pu-user '((:code . "shared") (:email . "e@x.com"))
                               *pu-hub-origin*))
        "pull a node with code=shared authored by the remote origin")
    ;; same code, but locally authored (different origin) -> allowed.
    (finishes (with-transaction () (make-pu-user :code "shared" :email "d@x.com")))
    ;; a second LOCAL code=shared (same origin as the first local) -> rejected.
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-pu-user :code "shared" :email "d2@x.com")))))

;;; --- multi-slot tuple constraint, same call sites (GH #107) ----------------

(test pulled-multi-slot-unique-is-enforced-locally
  "GAP 1 for a tuple constraint: a pulled (ns, ky) tuple, via the AUTHORED
pull-apply path (peer-streaming.lisp:818), lands in the device index, so a
later LOCAL commit reusing the same tuple is rejected."
  (with-pu-device (g)
    (is (graph-db::apply-peer-authored-op
         g (pu-authored-create g 'pu-claim '((:ns . "ops") (:ky . "c1"))
                               *pu-hub-origin*))
        "the pulled create applies")
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-pu-claim :ns "ops" :ky "c1")))
    ;; a distinct tuple still commits fine.
    (finishes (with-transaction () (make-pu-claim :ns "ops" :ky "c2")))))

(test state-sync-multi-slot-unique-maintains-index
  "The state-sync pull-apply path (peer-streaming.lisp:781) also enforces a
pulled tuple's constraint against a later local duplicate."
  (with-pu-device (g)
    (let* ((tid (graph-db::node-type-id
                 (graph-db::lookup-node-type-by-name
                  'pu-claim :vertex :graph g)))
           ;; %MAKE-VERTEX, not a bare (MAKE-INSTANCE 'pu-claim ...): see
           ;; PU-AUTHORED-CREATE's docstring above (GH #135).
           (n (graph-db::%make-vertex :class 'pu-claim :id (gen-id)
                                      :type-id tid :revision 0)))
      (setf (graph-db::data n) '((:ns . "sync") (:ky . "s1")))
      (graph-db::apply-peer-create-writes
       g 7777 (list (make-instance 'graph-db::tx-create :node n))
       *pu-hub-origin*))
    (signals graph-db:unique-constraint-violation
      (with-transaction () (make-pu-claim :ns "sync" :ky "s1")))))

(test purge-releases-multi-slot-unique-keys
  "PEER-PURGE-NODE releases a purged node's multi-slot tuple key too."
  (with-pu-device (g)
    (let ((vid (id (with-transaction () (make-pu-claim :ns "ops" :ky "p1")))))
      (signals graph-db:unique-constraint-violation
        (with-transaction () (make-pu-claim :ns "ops" :ky "p1")))
      (graph-db::apply-peer-purge g (list vid))
      (finishes (with-transaction () (make-pu-claim :ns "ops" :ky "p1"))))))

(test node-origins-persist-and-release-across-reopen
  "The per-node :ORIGIN partition survives close/open (so enforcement AND release
still compute the right key on a reopened graph)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) (vid nil))
      (let ((g (make-graph *pu-graph-name* path :peer-role :device
                           :origin-id *pu-dev-origin* :peer-host "localhost"
                           :replication-port 0 :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (setf vid (id (with-transaction () (make-pu-user :code "k1" :email "k@x.com")))))
          (close-graph g :snapshot-p nil)))
      (let ((g (open-graph *pu-graph-name* path :peer-role :device
                           :origin-id *pu-dev-origin* :peer-host "localhost"
                           :replication-port 0 :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (is (equalp *pu-dev-origin* (graph-db::get-node-origin g vid))
                   "the node's creation origin was restored")
               ;; same-origin duplicate still rejected (index + origin both restored).
               (signals graph-db:unique-constraint-violation
                 (with-transaction () (make-pu-user :code "k1" :email "other@x.com")))
               ;; re-key the node, then the OLD code frees up -> release used the
               ;; restored origin to remove the right key.
               (with-transaction ()
                 (let ((v (copy (lookup-vertex vid)))) (setf (pu-code v) "k2") (save v)))
               (finishes (with-transaction () (make-pu-user :code "k1" :email "reuse@x.com"))))
          (close-graph g :snapshot-p nil))))))
