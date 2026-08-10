;;;; Reading claims back: the inverse query, the extent codec, and the
;;;; regeneration sweep (GH #131, design §7-§8).

(in-package #:graph-db.spacetime)

(defun claims-touching (graph claim-class namespace key
                        &key (role :either))
  "Claims in GRAPH naming (NAMESPACE, KEY) as subject, object, or either.
CLAIM-CLASS is the PARENT class name; one call covers both arities.  Answers
from the claim graph's own indexes -- no cross-graph read, no snapshot, which
is what makes it implementable in this unit (design §8).

An out-of-range ROLE signals rather than silently returning NIL -- NIL is
also the correct answer for \"no claims touch this endpoint\", and this
subsystem exists to keep those two cases from being confused."
  (check-type role (member :subject :object :either))
  (let* ((family (claim-family claim-class))
         (want (list namespace key))
         (subjects (when (member role '(:subject :either))
                     (graph-db:index-lookup
                      graph (claim-family-parent family)
                      '(subject-namespace subject-key) want)))
         (objects (when (member role '(:object :either))
                    (graph-db:index-lookup
                     graph (claim-family-binary family)
                     '(object-namespace object-key) want))))
    ;; A claim naming one endpoint as BOTH subject and object appears in both
    ;; lookups; the union must still return it once.
    (if (and subjects objects)
        (remove-duplicates (append subjects objects)
                            :key #'graph-db:id :test #'equalp)
        (or subjects objects))))

(defun claim-extent (claim)
  "CLAIM's TEMPORAL-EXTENT, decoded from the stored sexp, or NIL.  The stored
form is EXTENT-SEXP; the two never share a name so neither is mistaken for
the other (design §7)."
  (let ((s (claim-extent-sexp claim)))
    (when s (sexp->extent s))))

(defun (setf claim-extent) (extent claim)
  "Store EXTENT on CLAIM as its sexp.  Only values GRAPH-DB:SERIALIZE
already handles reach the heap, so no core type byte is reserved.

Also resets CLAIM's cached BYTES to :INIT: a not-yet-committed node's
BYTES is populated eagerly at construction from the constructor's
initial args and never refreshed for a plain post-construction SETF, so
without this the extent survives in-memory but is silently absent after
a close/reopen (core bug, GH #135)."
  (setf (claim-extent-sexp claim) (and extent (extent->sexp extent)))
  (setf (graph-db::bytes claim) :init)
  extent)
