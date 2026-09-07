;;;; tests/spacetime/vocabulary-tests.lisp -- what a family names
;;;; (GH #350, spec 2026-09-07 §4).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun %ns-u (ns subject &key (relation "r") (producer "rule-a"))
  "A unary CT-CLAIM with an explicit namespace (MAKE-U fixes :NS)."
  (make-ct-claim-unary :subject-namespace ns :subject-key subject
                       :relation relation :producer producer
                       :standing :inferred))

(defun %ns-b (ns subject ons object
              &key (relation "r") (producer "rule-a"))
  "A binary CT-CLAIM with explicit namespaces on both ends."
  (make-ct-claim-binary :subject-namespace ns :subject-key subject
                        :relation relation
                        :object-namespace ons :object-key object
                        :producer producer :standing :inferred))

(test claim-namespaces-lists-by-role-in-index-order
  "Spec §4.1-4.2: subject names from the parent's index, object names
from the binary's, :EITHER merged and de-duplicated, in index order
(keywords by SYMBOL-NAME); :COUNTS sums over roles."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-b :ns "b" :other "x")
      (%ns-b :region "c" :ns "d"))
    (is (equal '(:ns :region) (claim-namespaces g 'ct-claim :role :subject)))
    (is (equal '(:ns :other) (claim-namespaces g 'ct-claim :role :object)))
    (is (equal '(:ns :other :region) (claim-namespaces g 'ct-claim)))
    (is (equal '((:ns . 2) (:region . 1))
               (claim-namespaces g 'ct-claim :role :subject :counts t)))
    (is (equal '((:ns . 3) (:other . 1) (:region . 1))
               (claim-namespaces g 'ct-claim :counts t))
        ":either sums the two roles")))

(test claim-relations-lists-distinct-relations-with-counts
  "Spec §4.1: relations from the family's CLAIM-RELATION index, once
each, in index order, with counts."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a" :relation "r")
      (%ns-u :ns "b" :relation "r")
      (%ns-b :ns "c" :ns "d" :relation "knows"))
    (is (equal '("knows" "r") (claim-relations g 'ct-claim)))
    (is (equal '(("knows" . 1) ("r" . 2))
               (claim-relations g 'ct-claim :counts t)))))

(test vocabulary-current-drops-retracted-only-names
  "Spec §4.3, R4: the default lists a name whose claims are all
retracted (the record of what was believed); :CURRENT T drops it and
counts only current claims."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :other "b")
      (%ns-u :other "c"))
    (retract-claim (first (claims-touching g 'ct-claim :other "b")))
    (is (equal '(:ns :other) (claim-namespaces g 'ct-claim)))
    (is (equal '((:ns . 1) (:other . 2))
               (claim-namespaces g 'ct-claim :counts t)))
    (is (equal '((:ns . 1) (:other . 1))
               (claim-namespaces g 'ct-claim :counts t :current t))
        "the retracted claim is not counted")
    (retract-claim (first (claims-touching g 'ct-claim :other "c")))
    (is (equal '(:ns) (claim-namespaces g 'ct-claim :current t))
        "every claim under :other is retracted")
    (is (equal '(:ns :other) (claim-namespaces g 'ct-claim)))))

(test vocabulary-drops-a-name-whose-claims-were-deleted
  "Spec §2.3, §5: a deleted claim's index entries go at commit, so its
name disappears without :CURRENT."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :gone "z"))
    (is (equal '(:gone :ns) (claim-namespaces g 'ct-claim)))
    (with-transaction ()
      (graph-db:mark-deleted (first (claims-touching g 'ct-claim :gone "z"))))
    (is (equal '(:ns) (claim-namespaces g 'ct-claim)))
    (is (equal '((:ns . 1)) (claim-namespaces g 'ct-claim :counts t)))))

(test vocabulary-refuses-the-epoch-axes-and-an-unknown-family
  "Spec §4.5, R6: :AS-OF and :AS-OF-EPOCH signal a typed refusal;
an unknown family signals as CLAIMS-TOUCHING does."
  (with-claim-graph (g)
    (with-transaction () (%ns-u :ns "a"))
    (signals graph-db:query-precondition-error
      (claim-namespaces g 'ct-claim :as-of 1))
    (signals graph-db:query-precondition-error
      (claim-relations g 'ct-claim :as-of-epoch 1))
    (signals unknown-claim-family (claim-namespaces g 'no-such-family))
    (is (equal '(:ns) (claim-namespaces g 'ct-claim))
        "control: the same call without the axis answers")))
