;;;; Linking a claim to its endpoint nodes (GH #369, spec sec.4).
;;;;
;;;; Write-time here; the idempotent sweep (U2, spec sec.5) joins this
;;;; file.  Edges are derived: the (namespace, key) slots are the truth,
;;;; and nothing in this file may fail a claim write except a caller's
;;;; own mismatch (sec.4.2).

(in-package #:graph-db.spacetime)

(defun %binary-claim-p (claim)
  "True when CLAIM's class carries the binary arity's OBJECT-KEY slot."
  (slot-exists-p claim 'object-key))

(defun %identity-slot (class)
  "CLASS's identity key slot symbol (SOURCE-FACETS-IDENTITY :KEY-SLOT)."
  (getf (source-facets-identity (source-contract class)) :key-slot))

(defun %uncommitted-creates (graph class slot key)
  "Nodes of CLASS this transaction creates whose SLOT is KEY -- what
INDEX-LOOKUP cannot see, because the index is written at commit apply
(spec sec.4.1 'Same-transaction visibility'; the #324 overlay).  An
unbound SLOT is 'no key', not a candidate."
  (let ((tx graph-db::*transaction*))
    (when tx
      (let ((view (graph-db:make-commit-view graph tx))
            (out '()))
        (dolist (w (graph-db:view-writes view) out)
          (let ((n (graph-db:view-node view (graph-db:id w))))
            (when (and n
                       (typep n class)
                       (null (graph-db:view-old-node view n))
                       (slot-boundp n slot)
                       (let ((v (slot-value n slot)))
                         (and (stringp v) (string= v key))))
              (push n out))))))))

(defun %same-store-candidates (graph namespace key)
  "Distinct nodes in GRAPH that are registered sources of NAMESPACE with
identity key KEY: index hits unioned with the open transaction's own
creates.  NIL, never a signal, for an unknown namespace or a class with
no index in GRAPH (spec sec.4.1 steps 1-2).  Reads *NAMESPACE-SOURCES*
directly rather than NAMESPACE-SOURCES: an unregistered namespace is the
common case on every write of an existing (non-EE) claim family, so this
must not signal-and-catch a condition per call; NAMESPACE-SOURCES keeps
its signalling contract for callers who want the typo check (GH #369
review)."
  (let ((hits '()))
    (dolist (class (gethash namespace *namespace-sources*))
      (let ((slot (%identity-slot class)))
        (dolist (n (handler-case
                       (graph-db:index-lookup graph class (list slot) key)
                     (graph-db:query-precondition-error () nil)))
          (push n hits))
        (dolist (n (%uncommitted-creates graph class slot key))
          (push n hits))))
    (remove-duplicates hits :key #'graph-db:id :test #'equalp)))

(defun %verify-endpoint-node (node namespace key)
  "Signal ENDPOINT-MISMATCH unless NODE is a registered source of
NAMESPACE whose identity key is KEY (spec sec.4.2 step 1).  An
UNRESOLVED-NODE marker is never a source, so it is refused too; an
unbound identity slot is a :KEY mismatch, not an error.  Reads
*NAMESPACE-SOURCES* directly -- see %SAME-STORE-CANDIDATES."
  (let* ((classes (gethash namespace *namespace-sources*))
         (class (find-if (lambda (c) (typep node c)) classes)))
    (unless class
      (error 'endpoint-mismatch :node node :namespace namespace :key key
                                :reason :not-a-source))
    (let ((slot (%identity-slot class)))
      (unless (and (slot-boundp node slot)
                   (let ((v (slot-value node slot)))
                     (and (stringp v) (string= v key))))
        (error 'endpoint-mismatch :node node :namespace namespace
                                  :key key :reason :key)))
    node))

(defun %link-endpoint (claim graph ctor namespace key given)
  "Create one CTOR edge from CLAIM to its endpoint in GRAPH.  GIVEN,
verified: a bad node is ENDPOINT-MISMATCH, uncaught here, and fails the
write.  Without GIVEN: the single same-store candidate is linked, several
candidates are ENDPOINT-LINK-SKIPPED, and any failure in that lookup-and-
link path is logged and swallowed rather than failing the write (GH
#161: a worker thread doing automatic linking may lack *SYSTEM-DIRECTORY*
for lazy SUBJECT-OF/OBJECT-OF store adoption)."
  (if given
      (let ((target (%verify-endpoint-node given namespace key)))
        (funcall ctor :from claim :to (graph-db:id target) :graph graph))
      (handler-case
          (let ((cands (%same-store-candidates graph namespace key)))
            (cond ((null cands) nil)
                  ((cdr cands)
                   (warn 'endpoint-link-skipped
                         :claim claim :namespace namespace :key key
                         :classes (remove-duplicates
                                   (mapcar (lambda (n)
                                             (class-name (class-of n)))
                                           cands)))
                   nil)
                  (t (funcall ctor :from claim
                                  :to (graph-db:id (first cands))
                                  :graph graph))))
        ;; GH #161: EIGHT-CONCURRENT-REGISTRATIONS-OF-ONE-SUBJECT-ALL-
        ;; SUCCEED's worker threads bind *GRAPH* but not *SYSTEM-
        ;; DIRECTORY*; never fail the claim write for it.
        (error (c)
          (log:warn "GH #369: endpoint (~S ~S) of claim ~A not linked: ~A"
                    namespace key (graph-db:id claim) c)
          nil))))

(defun %link-claim-at-write (claim &key subject-node object-node)
  "Link CLAIM's endpoints in its own store, inside the caller's open
transaction (spec sec.4.1-4.2).  Returns CLAIM.  Never signals an ERROR
for the derived edge; ENDPOINT-MISMATCH is the caller's, not the edge's."
  ;; NODE-GRAPH is internal to GRAPH-DB (not exported); GRAPH-DB::* is
  ;; the house idiom used elsewhere in this file for such symbols.
  (let ((graph (graph-db::node-graph claim)))
    (%link-endpoint claim graph #'make-subject-of
                    (claim-subject-namespace claim)
                    (claim-subject-key claim) subject-node)
    (when (%binary-claim-p claim)
      (%link-endpoint claim graph #'make-object-of
                      (claim-object-namespace claim)
                      (claim-object-key claim) object-node))
    claim))

(defun %strip-endpoint-node-args (args)
  "ARGS without :SUBJECT-NODE / :OBJECT-NODE, which the raw constructor
must never see."
  (let ((copy (copy-list args)))
    (remf copy :subject-node)
    (remf copy :object-node)
    copy))
