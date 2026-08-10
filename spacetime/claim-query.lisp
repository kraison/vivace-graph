;;;; Reading claims back: the inverse query, the extent codec, and the
;;;; regeneration sweep (GH #131, design §7-§8).

(in-package #:graph-db.spacetime)

(defun claims-touching (graph claim-class namespace key
                        &key (role :either))
  "Claims in GRAPH naming (NAMESPACE, KEY) as subject, object, or either.
CLAIM-CLASS is the PARENT class name; one call covers both arities.  Answers
from the claim graph's own indexes -- no cross-graph read, no snapshot, which
is what makes it implementable in this unit (design §8)."
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
