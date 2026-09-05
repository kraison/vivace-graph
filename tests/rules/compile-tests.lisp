;;;; tests/rules/compile-tests.lisp -- compile-rule (spec §6, GH #331).

(in-package #:graph-db/rules-test)

(in-suite rules-suite)

;;; *WEB-HOSTS-HEAD* and *WEB-HOSTS-BODY* come from schema-tests.lisp.
;;; Every other rule text is named here rather than written inline: a
;;; claim/7 pattern plus a keyword argument runs past 80 columns.

(defparameter *head-x*
  "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
(defparameter *head-y*
  "(claim ?c rt-claim \"app\" \"web\" \"y\" \"host\" ?h)")
(defparameter *body-x*
  "(claim ?p rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
(defparameter *body-y*
  "(claim ?p rt-claim \"app\" \"web\" \"y\" \"host\" ?h)")
(defparameter *body-z*
  "(claim ?p rt-claim \"app\" \"web\" \"z\" \"host\" ?h)")

(defparameter *head-two-goals*
  (concatenate 'string *head-x* " (claim-current ?c)"))
(defparameter *head-not-claim*
  "(claim-producer ?c \"p\")")
(defparameter *head-c-in-body*
  "(claim ?p rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")
(defparameter *head-relation-var*
  "(claim ?c rt-claim \"app\" \"web\" ?r \"host\" ?h)")
(defparameter *head-free-object*
  "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" ?z)")
(defparameter *head-half-object*
  "(claim ?c rt-claim \"app\" \"web\" \"x\" \"host\" nil)")
(defparameter *head-bad-namespace*
  "(claim ?c rt-claim \"App\" \"web\" \"x\" \"host\" ?h)")
(defparameter *head-runs*
  "(claim ?c rt-claim \"app\" \"web\" \"runs\" \"host\" ?h)")
(defparameter *head-scanned*
  "(claim ?c rt-claim \"host\" ?h \"scanned\" \"app\" ?a)")
(defparameter *head-bare-c*
  "(claim ? rt-claim \"app\" \"web\" \"x\" \"host\" ?h)")

(defparameter *body-no-functor* "(no-such-functor ?p)")
(defparameter *body-unregistered*
  (concatenate 'string *web-hosts-body* " " *body-no-functor*))
(defparameter *body-bare-question*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?)")
(defparameter *body-qualified*
  "(claim ?p graph-db::rt-claim \"host\" ?h \"runs\" \"app\" \"web\")")
(defparameter *body-relation-var*
  "(claim ?p rt-claim \"host\" ?h ?r \"app\" ?a)")
(defparameter *body-runs-any-app*
  "(claim ?p rt-claim \"host\" ?h \"runs\" \"app\" ?a)")
(defparameter *body-scanned*
  (concatenate 'string *body-runs-any-app*
               " (claim-producer ?p \"scan-a\")"))
(defparameter *body-producer-var*
  (concatenate 'string *web-hosts-body* " (claim-producer ?p ?who)"))

(defun spec (&rest args)
  "A def-rule-shaped RULE-SPEC without registering it."
  (apply #'graph-db.rules::%make-rule-spec :source :def-rule args))

(test compile-rule-reads-the-head-and-the-body
  (with-rules-graph (g)
    (let ((c (graph-db.rules:compile-rule
              g (spec :name "web-hosts" :version "1" :family "rt-claim"
                      :head *web-hosts-head* :body *web-hosts-body*))))
      (is (graph-db.rules:compiled-rule-p c))
      (is (string= "hosted-on" (graph-db.rules:compiled-rule-relation c)))
      (is (equal '("runs") (graph-db.rules:compiled-rule-reads c)))
      (is (eq (claim-family 'rt-claim)
              (graph-db.rules::compiled-rule-family c)))
      ;; One premise variable, ?P; the head's ?H is a body variable.
      (is (= 1 (length (graph-db.rules::compiled-rule-premise-vars c))))
      (is (member (graph-db.rules::compiled-rule-head-okey c)
                  (graph-db.rules::compiled-rule-vars c)))
      ;; A literal namespace in the head is interned now, not at run.
      (is (eq :app (graph-db.rules::compiled-rule-head-sns c))))))

(defmacro refuses (reason-substring &rest spec-args)
  "The compile is refused and the reason names REASON-SUBSTRING."
  `(with-rules-graph (g)
     (let ((c (handler-case
                  (progn (graph-db.rules:compile-rule g (spec ,@spec-args))
                         nil)
                (graph-db.rules:rule-compile-error (c) c))))
       (is-true c "compiled when it should have been refused")
       (when c
         (is (search ,reason-substring
                     (graph-db.rules:rule-compile-error-reason c)
                     :test #'char-equal)
             "reason ~S does not mention ~S"
             (graph-db.rules:rule-compile-error-reason c)
             ,reason-substring)))))

(test the-head-must-be-one-claim-pattern
  (refuses "exactly one" :name "r" :version "1" :family "rt-claim"
           :head *head-two-goals* :body *web-hosts-body*)
  (refuses "claim/7" :name "r" :version "1" :family "rt-claim"
           :head *head-not-claim* :body *web-hosts-body*)
  (refuses "?c" :name "r" :version "1" :family "rt-claim"
           :head *head-c-in-body* :body *web-hosts-body*)
  (refuses "family" :name "r" :version "1" :family "rtt-claim"
           :head *web-hosts-head* :body *web-hosts-body*)
  (refuses "relation" :name "r" :version "1" :family "rt-claim"
           :head *head-relation-var* :body *web-hosts-body*)
  (refuses "?z" :name "r" :version "1" :family "rt-claim"
           :head *head-free-object* :body *web-hosts-body*)
  (refuses "object" :name "r" :version "1" :family "rt-claim"
           :head *head-half-object* :body *web-hosts-body*)
  (refuses "namespace" :name "r" :version "1" :family "rt-claim"
           :head *head-bad-namespace* :body *web-hosts-body*))

(test the-body-goes-through-the-guard
  ;; An effecting functor, a package-qualified name, an unknown name.
  (refuses "not a registered" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body *body-unregistered*)
  (refuses "bare ?" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body *body-bare-question*)
  ;; The head is scanned separately: %BODY-VARIABLES sees the body
  ;; alone, so a bare ? in the head's ?c position reached no check.
  (refuses "bare ?" :name "r" :version "1" :family "rt-claim"
           :head *head-bare-c* :body *web-hosts-body*)
  (refuses "package-qualified" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body *body-qualified*)
  (refuses "empty" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body "   "))

(test a-rule-that-reads-its-own-relation-is-refused
  (refuses "cycle" :name "r" :version "1" :family "rt-claim"
           :head *head-runs* :body *web-hosts-body*)
  ;; An unbound relation reads everything, its own included (P6).
  (refuses "bind the relation" :name "r" :version "1" :family "rt-claim"
           :head *web-hosts-head* :body *body-relation-var*))

(test a-cycle-across-two-rules-is-refused-and-named
  (with-rules-graph (g)
    (write-rule g :name "a" :version "1" :family "rt-claim"
                :head *head-x* :body *body-y*)
    (let ((c (handler-case
                 (progn
                   (write-rule g :name "b" :version "1"
                               :family "rt-claim"
                               :head *head-y* :body *body-x*)
                   nil)
               (graph-db.rules:rule-compile-error (c) c))))
      (is-true c)
      (when c
        (is (search "x" (graph-db.rules:rule-compile-error-reason c)
                    :test #'char-equal))
        (is (search "y" (graph-db.rules:rule-compile-error-reason c)
                    :test #'char-equal))))
    ;; The refused write left nothing behind.
    (is (null (graph-db:index-lookup g 'graph-db.rules:rule
                                     '(graph-db.rules::name) "b")))
    ;; Control: b reading a third relation is not a cycle.
    (finishes
      (write-rule g :name "b" :version "1" :family "rt-claim"
                  :head *head-y* :body *body-z*))))

(test a-def-rule-joins-the-cycle-graph-and-collides-by-name
  (with-rules-graph (g)
    (graph-db.rules:def-rule "b" :version "1" :family rt-claim
      :head *head-y* :body *body-x*)
    (unwind-protect
         (progn
           (is (graph-db.rules:rule-spec-p
                (graph-db.rules:find-def-rule "b")))
           (signals graph-db.rules:rule-compile-error
             (write-rule g :name "a" :version "1" :family "rt-claim"
                         :head *head-x* :body *body-y*))
           ;; Same name as a def-rule: a collision, whatever the text.
           (signals graph-db.rules:rule-compile-error
             (write-rule g :name "b" :version "1" :family "rt-claim"
                         :head *web-hosts-head*
                         :body *web-hosts-body*)))
      (graph-db.rules:undef-rule "b"))
    (is (null (graph-db.rules:find-def-rule "b")))
    ;; Control: with the def-rule gone both writes commit.
    (finishes
      (write-rule g :name "a" :version "1" :family "rt-claim"
                  :head *head-x* :body *body-y*))))

(test a-stored-rule-that-does-not-compile-is-refused-at-write
  (with-rules-graph (g)
    (signals graph-db.rules:rule-compile-error
      (write-rule g :name "bad" :version "1" :family "rt-claim"
                  :head *web-hosts-head* :body *body-no-functor*))
    (is (null (graph-db:index-lookup g 'graph-db.rules:rule
                                     '(graph-db.rules::name) "bad")))
    ;; A disabled rule still has to compile (spec §6: compiled, not run).
    (signals graph-db.rules:rule-compile-error
      (write-rule g :name "bad" :version "1" :family "rt-claim"
                  :enabled nil :head *web-hosts-head*
                  :body *body-no-functor*))))

(test claim-producer-generators-move-to-the-front
  "Ruling P5: (claim-producer ?v \"p\") is a generator and runs first."
  (with-rules-graph (g)
    (let* ((c (graph-db.rules:compile-rule
               g (spec :name "r" :version "1" :family "rt-claim"
                       :head *head-scanned* :body *body-scanned*)))
           (goals (graph-db.rules::compiled-rule-goals c)))
      (is (string= "CLAIM-PRODUCER" (symbol-name (first (first goals)))))
      (is (string= "CLAIM" (symbol-name (first (second goals)))))
      ;; The filter direction is left where it was: a bound ?P is not
      ;; a generator.
      (let* ((d (graph-db.rules:compile-rule
                 g (spec :name "r" :version "1" :family "rt-claim"
                         :head *head-x* :body *body-producer-var*)))
             (goals (graph-db.rules::compiled-rule-goals d)))
        (is (string= "CLAIM" (symbol-name (first (first goals)))))))))

(test two-mutually-cyclic-rules-in-one-transaction-are-refused
  "The create-through-the-view branch of %STORED-RULES: neither record
is committed, so only VIEW-WRITES can show one rule the other."
  (with-rules-graph (g)
    (signals graph-db.rules:rule-compile-error
      (with-transaction ((graph-db::transaction-manager g))
        (graph-db.rules:make-rule :graph g :name "a" :version "1"
                                  :family "rt-claim"
                                  :head *head-x* :body *body-y*)
        (graph-db.rules:make-rule :graph g :name "b" :version "1"
                                  :family "rt-claim"
                                  :head *head-y* :body *body-x*)))
    (is (null (graph-db:index-lookup g 'graph-db.rules:rule
                                     '(graph-db.rules::name) "a")))
    (is (null (graph-db:index-lookup g 'graph-db.rules:rule
                                     '(graph-db.rules::name) "b")))
    ;; Control: the same pair with b reading a third relation commits.
    (finishes
      (with-transaction ((graph-db::transaction-manager g))
        (graph-db.rules:make-rule :graph g :name "a" :version "1"
                                  :family "rt-claim"
                                  :head *head-x* :body *body-y*)
        (graph-db.rules:make-rule :graph g :name "b" :version "1"
                                  :family "rt-claim"
                                  :head *head-y* :body *body-z*)))
    (is (= 2 (length (graph-db:map-vertices #'identity g
                                            :vertex-type
                                            'graph-db.rules:rule
                                            :collect-p t))))))
