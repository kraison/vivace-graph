;;;; The inverse query (design §8).

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test claims-touching-finds-a-subject-across-both-arities
  (with-claim-graph (g)
    (with-transaction ()
      (make-u :subject "alpha")
      (make-b :subject "alpha" :object "beta"))
    (is (= 2 (length (claims-touching g 'ct-claim :ns "alpha"))))))

(test claims-touching-finds-an-object
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha" :object "beta"))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "beta"))))
    (is (= 1 (length (claims-touching g 'ct-claim :ns "beta"
                                      :role :object))))
    (is (= 0 (length (claims-touching g 'ct-claim :ns "beta"
                                      :role :subject))))))

(test claims-touching-does-not-cross-namespaces
  "The namespace is part of the key, so the same string in another namespace
is a different endpoint."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha"))
    (is (= 0 (length (claims-touching g 'ct-claim :other "alpha"))))))

(test claims-touching-returns-each-claim-once
  "A claim naming the same endpoint as BOTH subject and object must not be
returned twice by the :EITHER union.  The node cache would hand back one EQ
instance for both lookups, which is deduplicated by EQL alone and would let
a wrong (plain REMOVE-DUPLICATES) implementation pass; disabling it forces
two distinct instances for the same node, so the :KEY/:TEST choice is what
is actually under test."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "self" :object "self"))
    (let ((graph-db::*cache-enabled* nil))
      (is (= 1 (length (claims-touching g 'ct-claim :ns "self")))))))

(test claims-touching-signals-on-an-unregistered-parent
  (with-claim-graph (g)
    (signals unknown-claim-family
      (claims-touching g 'no-such-claim :ns "x"))))

(test claims-touching-signals-on-a-bad-role
  "An out-of-range ROLE must signal, not silently return NIL -- NIL is also
the correct answer for \"nothing touches this endpoint\"."
  (with-claim-graph (g)
    (with-transaction () (make-b :subject "alpha"))
    (signals error
      (claims-touching g 'ct-claim :ns "alpha" :role :subjet))))
