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

(test vocabulary-under-an-as-of-extent-confirms-names-at-that-epoch
  "Spec §4.5, R7: an open WITH-AS-OF extent is not refused; index
membership is live, so a claim created after E still has an entry, but
it resolves to nothing at E and its name is not listed.  The same call
outside the extent lists it (control)."
  (with-claim-graph (g)
    (with-transaction () (%ns-u :ns "a"))
    (let ((e (graph-db:latest-epoch g)))
      (with-transaction () (%ns-u :later "z"))
      (is (equal '(:later :ns) (claim-namespaces g 'ct-claim)) "control")
      (graph-db:with-as-of ((g) e)
        (is (equal '(:ns) (claim-namespaces g 'ct-claim))
            "the entry is live, the claim is absent at E: dropped")
        (is (equal '(("r" . 2)) (claim-relations g 'ct-claim :counts t))
            "counts are entry counts, the documented live-membership
bound; :current t resolves")
        (is (equal '(("r" . 1))
                   (claim-relations g 'ct-claim :counts t :current t))
            "under :current each entry is resolved; the absent one drops")))))

(test claim-keys-lists-keys-under-a-namespace-merged-and-paged
  "Spec §4.1-4.2: keys under NAMESPACE from both roles, merged and
de-duplicated in index order, counted per role or summed; :LIMIT /
:OFFSET page the merged list and the second value says whether more
existed; nothing filed there answers NIL."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-b :ns "c" :ns "b")
      (%ns-b :other "x" :ns "c")
      (%ns-u :other "y"))
    (is (equal '("a" "b" "c") (claim-keys g 'ct-claim :ns)))
    (is (equal '("a" "c") (claim-keys g 'ct-claim :ns :role :subject)))
    (is (equal '("b" "c") (claim-keys g 'ct-claim :ns :role :object)))
    (is (equal '(("a" . 1) ("b" . 1) ("c" . 2))
               (claim-keys g 'ct-claim :ns :counts t))
        "c is a subject once and an object once")
    (is (equal '("x" "y") (claim-keys g 'ct-claim :other)))
    (is (null (claim-keys g 'ct-claim :nowhere)))
    (multiple-value-bind (page more) (claim-keys g 'ct-claim :ns :limit 2)
      (is (equal '("a" "b") page))
      (is (eq t more)))
    (multiple-value-bind (page more)
        (claim-keys g 'ct-claim :ns :limit 2 :offset 2)
      (is (equal '("c") page))
      (is (null more)))
    (signals graph-db:query-precondition-error
      (claim-keys g 'ct-claim :ns :as-of 1))))

(test vocabulary-inside-a-transaction-is-what-it-will-commit
  "Spec §4.4, R5 (the GH #324 rule): a name asserted in the open
transaction is listed and counted; one whose only claim the
transaction deleted is gone; a retraction in the transaction moves the
:CURRENT answer; nothing of it is visible to the index until commit."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :other "b"))
    (with-transaction ()
      (%ns-u :fresh "n")
      (is (equal '(:fresh :ns :other) (claim-namespaces g 'ct-claim))
          "a created claim's namespace is listed before commit")
      (is (equal '((:fresh . 1) (:ns . 1) (:other . 1))
                 (claim-namespaces g 'ct-claim :counts t)))
      (is (equal '("n") (claim-keys g 'ct-claim :fresh)))
      (graph-db:mark-deleted (first (claims-touching g 'ct-claim :ns "a")))
      (is (equal '(:fresh :other) (claim-namespaces g 'ct-claim))
          "a name whose only claim the transaction deleted drops")
      (retract-claim (first (claims-touching g 'ct-claim :other "b")))
      (is (equal '(:fresh :other) (claim-namespaces g 'ct-claim))
          "retracted, still believed once: listed by default")
      (is (equal '(:fresh) (claim-namespaces g 'ct-claim :current t))
          "the retraction is seen by :current before commit")
      (is (equal '((:fresh . 1) (:other . 1))
                 (claim-namespaces g 'ct-claim :counts t))))
    (is (equal '(:fresh :other) (claim-namespaces g 'ct-claim))
        "after commit the index agrees")
    (is (equal '(:fresh) (claim-namespaces g 'ct-claim :current t)))))

(test vocabulary-in-a-transaction-counts-created-and-retracted-claims
  "Spec §4.4: inside a transaction a count is the committed count
adjusted by the transaction's own writes, under the default and under
:CURRENT."
  (with-claim-graph (g)
    (with-transaction ()
      (%ns-u :ns "a")
      (%ns-u :ns "b"))
    (with-transaction ()
      (%ns-u :ns "c")
      (retract-claim (first (claims-touching g 'ct-claim :ns "a")))
      (is (equal '((:ns . 3)) (claim-namespaces g 'ct-claim :counts t)))
      (is (equal '((:ns . 2))
                 (claim-namespaces g 'ct-claim :counts t :current t)))
      (is (equal '(("b" . 1) ("c" . 1))
                 (claim-keys g 'ct-claim :ns :counts t :current t)))
      (is (equal '(("r" . 3)) (claim-relations g 'ct-claim :counts t))
          "the created claim's relation counts before commit"))))

(test vocabulary-in-a-transaction-sees-a-created-binary-claim-s-object
  "Spec §4.4, R5, the object role: a binary claim created in the open
transaction contributes its object endpoint to the object-role listing
and its subject to the subject role; under :EITHER a claim whose two
namespaces are equal is counted once per role."
  (with-claim-graph (g)
    (with-transaction () (%ns-u :ns "a"))
    (with-transaction ()
      (%ns-b :ns "s" :other "o")
      (%ns-b :same "x" :same "y")
      (is (equal '(:other :same)
                 (claim-namespaces g 'ct-claim :role :object))
          "object namespaces of created binary claims are listed")
      (is (equal '("o") (claim-keys g 'ct-claim :other))
          "the object key under :other comes from the created pass")
      (is (equal '("x" "y") (claim-keys g 'ct-claim :same))
          "subject and object keys under one namespace, merged")
      (is (equal '((:ns . 2) (:other . 1) (:same . 2))
                 (claim-namespaces g 'ct-claim :counts t))
          ":either sums subject and object roles of created claims"))
    (is (equal '((:ns . 2) (:other . 1) (:same . 2))
               (claim-namespaces g 'ct-claim :counts t))
        "after commit the index agrees")))
