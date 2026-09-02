;;;; The claim class hierarchy and its macro (GH #131, design §3-§5).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defparameter *claim-graph-name* :graph-db-claim-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *claim-graph-name* graph-db::*schema-node-metadata*) nil))

(def-claim-classes ct-claim :graph-db-claim-test
  :extra-slots ((weight :initarg :weight :accessor ct-weight
                        :initform nil)))

(defmacro with-claim-graph ((g) &body body)
  "A fresh on-disk graph named *CLAIM-GRAPH-NAME* in a temp dir."
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *claim-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect (let ((graph-db:*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(test the-macro-defines-a-three-class-hierarchy
  (is-true (find-class 'ct-claim nil))
  (is-true (find-class 'ct-claim-unary nil))
  (is-true (find-class 'ct-claim-binary nil))
  (is-true (subtypep 'ct-claim-unary 'ct-claim))
  (is-true (subtypep 'ct-claim-binary 'ct-claim))
  (is-false (subtypep 'ct-claim-binary 'ct-claim-unary)))

(test the-parent-gets-no-constructor
  "Design §3.3: non-instantiability is signalled by not generating a
constructor.  MAKE-INSTANCE still works; nothing invites it."
  (is-true (fboundp 'make-ct-claim-unary))
  (is-true (fboundp 'make-ct-claim-binary))
  (is-false (fboundp 'make-ct-claim)))

(test object-slots-exist-only-on-the-binary-class
  "This IS the arity-as-a-type property (design §3.1): a unary claim cannot
carry an object because the slot does not exist.

Uses CL's SLOT-EXISTS-P on instances rather than a MOP call on classes:
graph-db :USEs SB-MOP on SBCL and CLOSER-MOP only on CCL/LispWorks, so
CLOSER-MOP is not loaded here and this test package sees neither."
  (with-claim-graph (g)
    (declare (ignorable g))
    (with-transaction ()
      (let ((u (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                                    :relation "r" :producer "p"
                                    :standing :inferred))
            (b (make-ct-claim-binary :subject-namespace :ns :subject-key "s"
                                     :relation "r" :object-namespace :ns
                                     :object-key "o" :producer "p"
                                     :standing :inferred)))
        (is-true (slot-exists-p b 'graph-db.spacetime::object-key))
        (is-false (slot-exists-p u 'graph-db.spacetime::object-key))))))

(test extra-slots-land-on-the-parent-so-both-arities-inherit-them
  (with-claim-graph (g)
    (declare (ignorable g))
    (let (u b)
      (with-transaction ()
        (setq u (make-ct-claim-unary :subject-namespace :ns
                                     :subject-key "s1" :relation "r"
                                     :producer "rule-a" :standing :inferred
                                     :weight 1.5d0))
        (setq b (make-ct-claim-binary :subject-namespace :ns
                                      :subject-key "s1" :relation "r"
                                      :object-namespace :ns :object-key "o1"
                                      :producer "rule-a" :standing :inferred
                                      :weight 2.5d0)))
      (is (= 1.5d0 (ct-weight u)))
      (is (= 2.5d0 (ct-weight b))))))

(test the-registry-maps-a-parent-to-its-arity-subclasses
  (let ((f (claim-family 'ct-claim)))
    (is (eq 'ct-claim (claim-family-parent f)))
    (is (eq 'ct-claim-unary (claim-family-unary f)))
    (is (eq 'ct-claim-binary (claim-family-binary f)))
    (signals unknown-claim-family (claim-family 'no-such-claim))))

(test standing-is-validated-at-construction
  "A claim cannot be built with a standing outside the vocabulary."
  (with-claim-graph (g)
    (declare (ignorable g))
    (signals invalid-standing
      (with-transaction ()
        (make-ct-claim-unary :subject-namespace :ns :subject-key "s"
                             :relation "r" :producer "p"
                             :standing :probably)))))

(test a-graph-holding-claims-closes-and-reopens-cleanly
  "Regression test for GH #131: an earlier STANDING check hooked
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, which also fires while an existing node
is deserialized (its DATA alist not populated yet) -- CLOSE-GRAPH's snapshot
silently failed for any graph holding claim data.  The wrapped-constructor
approach must not reintroduce that (design §5)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)) uid bid)
      (let ((g (make-graph *claim-graph-name* path :buffer-pool-size 1000)))
        (let ((graph-db:*graph* g))
          (with-transaction ()
            (setq uid (id (make-ct-claim-unary
                           :subject-namespace :ns :subject-key "s"
                           :relation "r" :producer "p" :standing :observed)))
            (setq bid (id (make-ct-claim-binary
                           :subject-namespace :ns :subject-key "s"
                           :relation "r" :object-namespace :ns
                           :object-key "o" :producer "p"
                           :standing :observed))))
          (close-graph g)))              ; :snapshot-p t (default)
      (let ((g2 (open-graph *claim-graph-name* path)))
        (unwind-protect
             (let ((graph-db:*graph* g2))
               (let ((u (lookup-ct-claim-unary uid))
                     (b (lookup-ct-claim-binary bid)))
                 (is-true u)
                 (is-true b)
                 (is (eq :observed (claim-standing u)))
                 (is (eq :observed (claim-standing b)))))
          (ignore-errors (close-graph g2))
          (collect-garbage))))))

(test a-claim-carries-registration-outputs-with-defaults
  "On the SHARED slots, not a tenant's :EXTRA-SLOTS: unit 3's traversal
weights by fraction without knowing which tenant wrote the claim, so it
must read one accessor (design §2, cl-llm#13)."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let ((c (make-ct-claim-binary :subject-namespace "s" :subject-key "k"
                                   :object-namespace "o" :object-key "ok"
                                   :relation "r" :producer "p"
                                   :standing :asserted)))
      (is (null (claim-precision-m c)))
      (is (= 1.0d0 (claim-fraction c))))))

(test registration-outputs-survive-a-round-trip
  (with-claim-graph (g)
    (let ((key "rt-1"))
      (make-ct-claim-binary :subject-namespace "s" :subject-key key
                            :object-namespace "o" :object-key "ok"
                            :relation "r" :producer "p"
                            :standing :asserted
                            :precision-m 12.5d0 :fraction 0.25d0)
      (let ((c (first (graph-db:index-lookup
                        g 'ct-claim
                        '(graph-db.spacetime::subject-namespace
                          graph-db.spacetime::subject-key)
                        (list "s" key)))))
        (is (= 12.5d0 (claim-precision-m c)))
        (is (= 0.25d0 (claim-fraction c)))))))

(test claim-identity-key-is-canonical-and-arity-aware
  "GH #303: equal identity tuples render STRING= keys; a unary key has no
object segment; the key names identity, not location."
  (with-claim-graph (g)
    (declare (ignorable g))
    (let (b1 b2 u1)
      (with-transaction ()
        (setq b1 (make-ct-claim-binary
                  :subject-namespace :region :subject-key "r1"
                  :relation "borders"
                  :object-namespace :region :object-key "r2"
                  :producer "ingest" :standing :observed)))
      (with-transaction ()
        (setq u1 (make-ct-claim-unary
                  :subject-namespace :region :subject-key "r1"
                  :relation "verified"
                  :producer "ingest" :standing :observed)))
      (setq b2 (first (claims-touching g 'ct-claim :region "r2"
                                       :role :object)))
      (is (string= (claim-identity-key b1) (claim-identity-key b2))
          "same claim read back renders the same key")
      (is (not (string= (claim-identity-key b1) (claim-identity-key u1))))
      (is (search "|:region|r2|" (claim-identity-key b1))
          "binary key carries the object segment")
      (is (null (search "|:region|r2|" (claim-identity-key u1)))))))

(test claims-touching-paginates-with-a-truncated-flag
  "GH #302: :limit/:offset cut the final result; the second value is T
exactly when more existed past the cut."
  (with-claim-graph (g)
    (with-transaction ()
      (dotimes (k 8)
        (make-ct-claim-binary
         :subject-namespace :region :subject-key "pg"
         :relation "contains"
         :object-namespace :item :object-key (format nil "i~d" k)
         :producer "ingest" :standing :observed)))
    (multiple-value-bind (page truncated)
        (claims-touching g 'ct-claim :region "pg" :role :subject :limit 5)
      (is (= 5 (length page)))
      (is-true truncated))
    (multiple-value-bind (page truncated)
        (claims-touching g 'ct-claim :region "pg" :role :subject :limit 8)
      (is (= 8 (length page)))
      (is-false truncated))
    (multiple-value-bind (page truncated)
        (claims-touching g 'ct-claim :region "pg" :role :subject
                         :limit 5 :offset 5)
      (is (= 3 (length page)))
      (is-false truncated))
    (multiple-value-bind (page truncated)
        (claims-by-producer g 'ct-claim "ingest" :limit 3)
      (is (= 3 (length page)))
      (is-true truncated))))

(test claims-touching-relation-filter-rides-the-index
  "GH #302: :relation restricts to one relation on both roles; the
subject side answers from the CLAIM-SUBJECT-RELATION index."
  (with-claim-graph (g)
    (with-transaction ()
      (dotimes (k 4)
        (make-ct-claim-binary
         :subject-namespace :region :subject-key "rel"
         :relation (if (evenp k) "contains" "borders")
         :object-namespace :item :object-key (format nil "i~d" k)
         :producer "ingest" :standing :observed)))
    (is (= 2 (length (claims-touching g 'ct-claim :region "rel"
                                      :role :subject
                                      :relation "contains"))))
    (is (= 4 (length (claims-touching g 'ct-claim :region "rel"
                                      :role :subject))))
    (let ((via-object (claims-touching g 'ct-claim :item "i1"
                                       :role :object
                                       :relation "borders")))
      (is (= 1 (length via-object))))))

(def-claim-classes kr-claim :graph-db-claim-test :keep-revisions 1)

(test as-of-unwinds-an-in-place-update
  "GH #300: a claim updated in place, read :as-of an instant before the
update, returns the earlier version; :at composes against THAT version's
validity, not the current one."
  (with-claim-graph (g)
    (let (c t0 t1)
      (with-transaction ()
        (setq c (make-ct-claim-binary
                 :subject-namespace :region :subject-key "ao"
                 :relation "in-state"
                 :object-namespace :state :object-key "a"
                 :producer "series" :standing :observed
                 :extent (exact-interval (ts 2022 1 1) (ts 2022 3 31)))))
      (setq t0 (graph-db.spacetime::%st-now))
      (sleep 0.01)
      (with-transaction ()
        (let ((k (graph-db:copy c)))
          (setf (claim-extent k)
                (exact-interval (ts 2022 1 1) (ts 2022 6 30)))
          (graph-db:save k)))
      (setq t1 (graph-db.spacetime::%st-now))
      (let ((then (claims-touching g 'ct-claim :region "ao"
                                   :role :subject :as-of t0))
            (now (claims-touching g 'ct-claim :region "ao"
                                  :role :subject :as-of t1)))
        (is (= 1 (length then)))
        (is (= 1 (length now)))
        (is (extent-equals-p (claim-extent (first then))
                             (exact-interval (ts 2022 1 1)
                                             (ts 2022 3 31)))
            "as-of before the update sees the old validity")
        (is (extent-equals-p (claim-extent (first now))
                             (exact-interval (ts 2022 1 1)
                                             (ts 2022 6 30)))))
      ;; :at composes with the RESOLVED version: May is outside the old
      ;; validity but inside the new.
      (is (null (claims-touching g 'ct-claim :region "ao" :role :subject
                                 :as-of t0 :at (ts 2022 5 15))))
      (is (= 1 (length (claims-touching g 'ct-claim :region "ao"
                                        :role :subject
                                        :as-of t1 :at (ts 2022 5 15))))))))

(test as-of-honors-the-retraction-window
  "GH #300: inside the transaction period the claim is returned; after
retraction it is not; before creation it is not."
  (with-claim-graph (g)
    (let (c before-create mid after)
      (setq before-create (graph-db.spacetime::%st-now))
      (sleep 0.01)
      (with-transaction ()
        (setq c (make-ct-claim-unary
                 :subject-namespace :region :subject-key "rw"
                 :relation "verified"
                 :producer "audit" :standing :observed)))
      (setq mid (graph-db.spacetime::%st-now))
      (sleep 0.01)
      (retract-claim (first (claims-touching g 'ct-claim :region "rw"
                                             :role :subject)))
      (setq after (graph-db.spacetime::%st-now))
      (is (null (claims-touching g 'ct-claim :region "rw" :role :subject
                                 :as-of before-create)))
      (is (= 1 (length (claims-touching g 'ct-claim :region "rw"
                                        :role :subject :as-of mid))))
      (is (null (claims-touching g 'ct-claim :region "rw" :role :subject
                                 :as-of after))))))

(test as-of-reports-reaped-not-a-lie
  "GH #300: with :keep-revisions 1, a version older than the window
resolves to a REAPED-CLAIM, never to a silently-substituted newer one;
and a claim swept by delete-claims-by-producer is invisible."
  (with-claim-graph (g)
    (let (c t0)
      (with-transaction ()
        (setq c (make-kr-claim-unary
                 :subject-namespace :region :subject-key "kr"
                 :relation "verified"
                 :producer "audit" :standing :observed)))
      (setq t0 (graph-db.spacetime::%st-now))
      (sleep 0.01)
      ;; Two more versions; window of 1 reaps the t0-era version.
      (dotimes (i 2)
        (with-transaction ()
          (let ((k (graph-db:copy
                    (first (claims-touching g 'kr-claim :region "kr"
                                            :role :subject)))))
            (setf (claim-confidence k) (* 0.1 (1+ i)))
            (graph-db:save k))))
      (let ((then (claims-touching g 'kr-claim :region "kr"
                                   :role :subject :as-of t0)))
        (is (= 1 (length then)))
        (is (reaped-claim-p (first then))
            "the t0 version is past the window: reaped, not substituted"))
      ;; Swept: absent from :as-of entirely.
      (delete-claims-by-producer g 'kr-claim "audit")
      (is (null (claims-touching g 'kr-claim :region "kr" :role :subject
                                 :as-of t0))))))
