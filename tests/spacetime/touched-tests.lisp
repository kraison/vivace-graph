;;;; The touched-at index and its reader (GH #392, design D1/D2).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(defun tt-three-claims ()
  "Three claims, subjects k1 k2 k3, each recorded in its own transaction,
so their touched instants differ (%ST-NOW is monotonic)."
  (loop for key in '("k1" "k2" "k3")
        collect (with-transaction () (make-u :subject key))))

(test tt-key-of-an-extent-is-its-touched-instant
  (let* ((ts (graph-db.spacetime::%st-now))
         (end (local-time:timestamp+ ts 1 :sec)))
    (is (equal (graph-db.spacetime::%timestamp-key ts)
               (graph-db.spacetime::extent-sexp-touched-key
                (extent->sexp
                 (graph-db.spacetime::%open-transaction-extent ts)))))
    (is (equal (graph-db.spacetime::%timestamp-key end)
               (graph-db.spacetime::extent-sexp-touched-key
                (extent->sexp (make-interval (exact-bound ts)
                                             (exact-bound end)
                                             :semantics :transaction
                                             :standing :asserted)))))
    (is (null (graph-db.spacetime::extent-sexp-touched-key nil)))
    (is (eq :unbounded
            (graph-db.spacetime::extent-sexp-touched-key
             (extent->sexp (make-interval (unknown-bound) (unknown-bound)
                                          :semantics :transaction
                                          :standing :asserted)))))))

(test tt-every-claim-sits-in-the-index-in-touched-order
  (with-claim-graph (g)
    (let ((claims (tt-three-claims)))
      (is (equalp (mapcar #'id claims)
                  (mapcar #'id
                          (graph-db:index-range
                           g 'ct-claim
                           'graph-db.spacetime::transaction-extent-sexp))))
      (loop for c in claims
            for touched = (claim-touched-at c)
            do (is (typep touched 'local-time:timestamp))
               (is (and (typep touched 'local-time:timestamp)
                        (local-time:timestamp=
                         touched (claim-recorded-at c))))))))

(test tt-a-retraction-moves-the-row
  (with-claim-graph (g)
    (let* ((claims (tt-three-claims))
           (k1 (first claims)) (k2 (second claims)) (k3 (third claims))
           (at (local-time:timestamp+ (graph-db.spacetime::%st-now) 1 :sec)))
      (retract-claim k1 :at at)
      (is (equalp (list (id k2) (id k3) (id k1))
                  (mapcar #'id
                          (graph-db:index-range
                           g 'ct-claim
                           'graph-db.spacetime::transaction-extent-sexp))))
      (is (equalp (list (id k3) (id k1))
                  (mapcar #'id (claims-touched-after
                                g 'ct-claim (claim-touched-at k2)))))
      (let ((fresh (find (id k1)
                         (graph-db:index-range
                          g 'ct-claim
                          'graph-db.spacetime::transaction-extent-sexp)
                         :key #'id :test #'equalp)))
        (is (local-time:timestamp= at (claim-touched-at fresh)))))))

(test tt-after-is-strict-and-accepts-a-string
  (with-claim-graph (g)
    (let* ((claims (tt-three-claims))
           (k2 (second claims))
           (k3 (third claims))
           (cursor (claim-touched-at k2)))
      (is (equalp (list (id k3))
                  (mapcar #'id (claims-touched-after g 'ct-claim cursor))))
      (let ((text (local-time:format-timestring
                   nil cursor
                   :format local-time:+rfc3339-format+
                   :timezone local-time:+utc-zone+)))
        (is (equalp (list (id k3))
                    (mapcar #'id (claims-touched-after
                                  g 'ct-claim text))))))))

(test tt-the-open-transactions-own-write-is-seen
  (with-claim-graph (g)
    (let* ((claims (tt-three-claims))
           (k3 (third claims)))
      (with-transaction ()
        (make-u :subject "k4")
        (let ((found (claims-touched-after
                      g 'ct-claim (claim-touched-at k3))))
          (is (= 1 (length found)))
          (is (equal "k4" (claim-subject-key (first found)))))))))

(test tt-limit-and-offset-page
  (with-claim-graph (g)
    (let* ((claims (tt-three-claims))
           (k1 (first claims))
           (k3 (third claims))
           (cursor (local-time:timestamp- (claim-touched-at k1) 1 :sec)))
      (multiple-value-bind (page more)
          (claims-touched-after g 'ct-claim cursor :limit 1)
        (is (equalp (list (id k1)) (mapcar #'id page)))
        (is-true more))
      (multiple-value-bind (page more)
          (claims-touched-after g 'ct-claim cursor :limit 1 :offset 2)
        (is (equalp (list (id k3)) (mapcar #'id page)))
        (is (null more))))))

(test tt-an-unknown-family-signals
  (with-claim-graph (g)
    (signals graph-db.spacetime:unknown-claim-family
      (graph-db.spacetime:claims-touched-after g 'no-such-family
                                               (local-time:now)))))
