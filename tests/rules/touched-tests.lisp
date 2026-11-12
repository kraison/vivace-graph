;;;; tests/rules/touched-tests.lisp -- CLAIM-TOUCHED-AFTER/4, the
;;;; touched-at index as a generator, and the rule-body refusals
;;;; (GH #392, touched-at index design D3).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

(defparameter *tta-head*
  "(claim ?z rt-claim \"app\" \"web\" \"hosted-on\" \"host\" \"h1\")"
  "A head whose claim variable is not the body's ?c: %PARSE-HEAD refuses
a head ?c the body also names, and a body key must be bound there.")

(test tta-filters-a-bound-claim
  "With ?c bound the goal filters: ?t is the claim's touched instant
string, and a cursor past every row answers none."
  (with-rules-graph (g)
    (seed g)
    (let ((ts (select-flat (?t)
                (claim ?c rt-claim "host" "h1" ?r ?ons ?o)
                (claim-touched-after ?c ?t "2026-09-01" rt-claim))))
      (is (= 2 (length ts)))
      (is (every (lambda (x) (and (stringp x) (= 30 (length x)))) ts)))
    (is (null (select-flat (?t)
                (claim ?c rt-claim "host" "h1" ?r ?ons ?o)
                (claim-touched-after ?c ?t "2999-01-01" rt-claim))))))

(test tta-generates-a-familys-claims-after-the-cursor-in-order
  "With ?c unbound and a cursor, the generator answers the family's
claims in ascending touched order; a retraction is one of them."
  (with-rules-graph (g)
    (seed-keys g 5)
    (let* ((*cursor* (graph-db.spacetime::%st-now))
           (k03 (first (claims-touching g 'rt-claim :host "k03"
                                        :role :subject)))
           (k01 (first (claims-touching g 'rt-claim :host "k01"
                                        :role :subject))))
      (retract-claim k03 :at (local-time:timestamp+ *cursor* 1 :sec))
      (retract-claim k01 :at (local-time:timestamp+ *cursor* 2 :sec))
      (let ((rows (select () (?k ?t)
                    (lisp ?cur *cursor*)
                    (claim-touched-after ?c ?t ?cur rt-claim)
                    (claim ?c rt-claim ?sns ?k ?r ?ons ?o))))
        (is (equal '("k03" "k01") (mapcar #'first rows)))
        (let ((ts (mapcar #'second rows)))
          (is (every (lambda (x) (and (stringp x) (= 30 (length x))))
                     ts))
          (is (every #'string< ts (rest ts))))))))

(test tta-every-family-skips-what-the-store-lacks
  "With ?family unbound it reads every registered family and skips the
ones this store does not declare instead of signalling."
  (with-rules-graph (g)
    (let ((*cursor* (graph-db.spacetime::%st-now)))
      (seed g)
      (seed-temporal g)
      (is (= 9 (select-count (?c)
                 (lisp ?cur *cursor*)
                 (claim-touched-after ?c ?t ?cur nil)))))))

(test tta-a-bound-family-the-store-lacks-signals
  "A named family the own store does not declare is the index's own
QUERY-PRECONDITION-ERROR, as CLAIMS-TOUCHED-AFTER signals it."
  (with-rules-graph (g)
    (signals graph-db:query-precondition-error
      (select-flat (?c)
        (claim-touched-after ?c ?t "2026-01-01" rtn-claim)))))

(test tta-an-unbound-cursor-answers-nothing
  "A free ?after is the unfused pair's answer: no solutions, no error."
  (with-rules-graph (g)
    (seed g)
    (is (null (select-flat (?c)
                (claim-touched-after ?c ?t ?x rt-claim))))))

(test tta-excluded-producers-are-not-generated
  "*CLAIM-EXCLUDE-PRODUCERS* removes a producer's relation from the
generator, as it does from CLAIM/7's routes (GH #333)."
  (with-rules-graph (g)
    (let ((*cursor* (graph-db.spacetime::%st-now)))
      (seed g)
      (let ((graph-db:*claim-exclude-producers* '(("scan-a" . "runs"))))
        (is (= 2 (select-count (?c)
                   (lisp ?cur *cursor*)
                   (claim-touched-after ?c ?t ?cur rt-claim)))))
      (is (= 4 (select-count (?c)
                 (lisp ?cur *cursor*)
                 (claim-touched-after ?c ?t ?cur rt-claim)))))))

(test tta-two-stores-merge-into-one-ascending-sequence
  "Every store in scope contributes to one ascending sequence; a
retraction stamped past the other store's rows sorts last."
  (with-two-stores (a b)
    (let ((*cursor* (graph-db.spacetime::%st-now)))
      (seed a)
      (seed-b b)
      (let* ((c (first (claims-touching a 'rt-claim :host "h1"
                                        :role :subject
                                        :relation "runs")))
             (retracted
               (retract-claim c :at (local-time:timestamp+ *cursor*
                                                          1 :sec))))
        (let ((graph-db::*claim-scope* (list a b)))
          (let* ((rows (select () (?t ?k)
                          (lisp ?cur *cursor*)
                          (claim-touched-after ?c ?t ?cur rt-claim)
                          (claim ?c rt-claim ?sns ?k ?r ?ons ?o)))
                 (ts (mapcar #'first rows)))
            (is (= 6 (length rows)))
            (is (every #'string< ts (rest ts)))
            (is (string=
                 (graph-db::%instant-string
                  (graph-db.spacetime:claim-touched-at retracted))
                 (car (last ts))))))))))

(test tta-standing-class-is-withheld
  "The compiler writes the goal; free text may not name it in a
standing body (#470)."
  (is (eq :withheld
          (cdr (assoc "claim-touched-after/4"
                      graph-db.rules::*standing-query-functor-classes*
                      :test #'string=)))))

(test tta-explain-names-the-touched-route
  "EXPLAIN-GOALS reports the touched-at route from what is bound."
  (with-rules-graph (g)
    (is (search "touched-at index of RT-CLAIM from the cursor"
                (ter-path '(claim-touched-after ?c ?t "2026-01-01"
                                                rt-claim))))
    (is (search "every family"
                (ter-path '(claim-touched-after ?c ?t "2026-01-01" nil))))
    (is (search "that claim"
                (ter-path '(claim-touched-after ?c ?t "2026-01-01"
                                                rt-claim)
                          '((claim ?c rt-claim "host" "h1" ?r
                                   ?ons ?o)))))))

(test tta-compile-rule-refuses-the-hand-written-functor
  "GH #392: the compiler writes it for the feed's text; a rule body
names claim/7 and the comparison, which the rewrite fuses."
  (refuses "not a rule premise" :name "r" :version "1"
           :family "rt-claim"
           :head *web-hosts-head*
           :body "(claim-touched-after ?c ?t \"2020-01-01\" rt-claim)"))

(test tta-compile-rule-refuses-a-pair-no-premise-binds
  "GH #392: a CLAIM-TOUCHED-AT goal whose claim no claim/7 premise
binds is a dead filter today and an unanalysed generator after D4."
  (refuses "bind ?c with claim/7 first"
           :name "r" :version "1" :family "rt-claim"
           :head *tta-head*
           :body "(claim-touched-at ?c ?t) (instant> ?t \"2020-01-01\")")
  (with-rules-graph (g)
    (is (graph-db.rules:compiled-rule-p
         (graph-db.rules:compile-rule
          g (spec :name "r" :version "1" :family "rt-claim"
                  :head *tta-head*
                  :body (concatenate
                         'string
                         "(claim ?c rt-claim \"host\" ?k \"runs\" "
                         "?ons ?o) (claim-touched-at ?c ?t) "
                         "(instant> ?t \"2020-01-01\")")))))))
