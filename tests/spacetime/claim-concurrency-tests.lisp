;;;; The phantom the commit lock defeats, on a claim (design §6.3, §10).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test concurrent-identical-claims-exactly-one-wins
  "Eight threads racing the same claim: one commits, seven are rejected."
  (with-claim-graph (g)
    (let ((oks 0) (rejects 0) (lock (bt:make-lock)) (threads nil))
      (dotimes (i 8)
        (push (bt:make-thread
               (lambda ()
                 (let ((graph-db:*graph* g))
                   (handler-case
                       (progn (with-transaction ()
                                (make-b :producer "race" :object "one"))
                              (bt:with-lock-held (lock) (incf oks)))
                     (graph-db:unique-constraint-violation ()
                       (bt:with-lock-held (lock) (incf rejects)))))))
              threads))
      (mapc #'bt:join-thread threads)
      (is (= 1 oks) "exactly one thread committed (got ~D)" oks)
      (is (= 7 rejects) "the other seven were rejected (got ~D)" rejects)
      (is (= 1 (length (claims-touching g 'ct-claim :ns "s1")))
          "one claim exists"))))

(test concurrent-unary-claims-exactly-one-wins
  "The same gate on the unary arity -- its constraint is a different index,
so passing on binary claims proves nothing about it."
  (with-claim-graph (g)
    (let ((oks 0) (rejects 0) (lock (bt:make-lock)) (threads nil))
      (dotimes (i 8)
        (push (bt:make-thread
               (lambda ()
                 (let ((graph-db:*graph* g))
                   (handler-case
                       (progn (with-transaction ()
                                (make-u :producer "race-u"))
                              (bt:with-lock-held (lock) (incf oks)))
                     (graph-db:unique-constraint-violation ()
                       (bt:with-lock-held (lock) (incf rejects)))))))
              threads))
      (mapc #'bt:join-thread threads)
      (is (= 1 oks) "exactly one thread committed (got ~D)" oks)
      (is (= 7 rejects) "the other seven were rejected (got ~D)" rejects))))
