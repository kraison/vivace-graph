;;;; Tests for the general ordered secondary index (:INDEX slot option / DEF-INDEX).
;;;; See docs/general-index-design.md.  The index is "\:unique minus enforcement":
;;;; equality lookup + ascending range, NIL-exempt, class-scoped (spans subclasses),
;;;; maintained on the commit apply path, durable (on-disk sidecar), backend-agnostic.

(in-package #:graph-db/test)

(defparameter *ix-graph-name* :graph-db-index-test)

(eval-when (:load-toplevel :execute)
  (setf (gethash *ix-graph-name* graph-db::*schema-node-metadata*) nil))

(def-vertex ix-person ()
  ((name  :initarg :name  :accessor ix-name  :index t)              ; plain index
   (age   :initarg :age   :accessor ix-age   :index t)              ; numeric range
   (email :initarg :email :accessor ix-email :index string-downcase) ; canonicalized
   (note  :initarg :note  :accessor ix-note))                       ; NOT indexed
  :graph-db-index-test)

;; A subclass: an :INDEX slot on the parent is one shared index across subclasses.
(def-vertex ix-employee (ix-person)
  ((title :initarg :title :accessor ix-title))
  :graph-db-index-test)

;; The standalone declaration surface: index NOTE, which is NOT marked :index t on
;; the slot, with a canonicalizer.  Declared before any graph is open -> registered,
;; then built at open (or maintained on apply for a fresh make-graph).
(def-index ix-person note :graph-db-index-test :canonicalize string-downcase)

;; Multi-slot: the endpoint-identity shape (namespace, external-key) (GH #107).
(def-vertex ix-claim ()
  ((ns  :initarg :ns  :accessor ix-ns)
   (key :initarg :key :accessor ix-key)
   (rel :initarg :rel :accessor ix-rel))
  :graph-db-index-test)

(def-index ix-claim (ns key rel) :graph-db-index-test)

;; The ambiguous shape from code review: slot A independently marked :index
;; with ITS OWN canonicalizer, AND the first slot of an unrelated multi-slot
;; def-index (A B) with a DIFFERENT canonicalizer.  On reopen, resolving
;; (A B)'s canonicalizer must not accidentally match A's own single-slot
;; :INDEX declaration just because they share a name -- string-upcase (A's)
;; vs string-downcase ((A B)'s) are deliberately conflicting transforms, so a
;; wrong match is observable, not just "no transform happened to still work"
;; (GH #107).
(def-vertex ix-dual ()
  ((a :initarg :a :accessor ix-dual-a :index string-upcase)
   (b :initarg :b :accessor ix-dual-b))
  :graph-db-index-test)

(def-index ix-dual (a b) :graph-db-index-test :canonicalize string-downcase)

;; Multi-slot with a POSITIONAL :canonicalize -- component 0 downcased,
;; component 1 (NIL entry) left as identity (GH #107, Task 6).
(def-index ix-claim (ns key) :graph-db-index-test
  :canonicalize (string-downcase nil))

;; Single-slot backward-compatibility gate: :canonicalize as a #'FN function
;; designator, not a bare symbol -- #'FN reads as (FUNCTION FN), a cons, so
;; this pins that a single-slot spec is not misread as a positional list
;; (GH #107, Task 6).
(def-vertex ix-solo ()
  ((tag :initarg :tag :accessor ix-solo-tag))
  :graph-db-index-test)

(def-index ix-solo tag :graph-db-index-test :canonicalize #'string-downcase)

(def-suite index-suite
  :description "General ordered secondary index (:INDEX / def-index)."
  :in graph-db-suite)

(in-suite index-suite)

(defmacro with-ix-graph ((g &key (backend :skip-list)) &body body)
  "A fresh on-disk graph named *IX-GRAPH-NAME* on BACKEND, in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph *ix-graph-name* (namestring ,dir)
                             :buffer-pool-size 1000 :index-backend ,backend)))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g))
           (collect-garbage))))))

(defun ix-names (nodes) (sort (mapcar #'ix-name nodes) #'string<))

;;; --- characterisation: single-slot behaviour must survive Task 3 (GH #107) --

(test characterise-single-slot-equality-and-range
  "Pins the caller-visible single-slot contract across the slot-list refactor."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)
      (make-ix-person :name "b" :age 40)
      (make-ix-person :name "c" :age 50))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'name "a"))))
    (is (equal '("a" "b") (ix-names (index-range g 'ix-person 'age
                                                 :start 30 :end 40))))
    (is (null (index-lookup g 'ix-person 'name "nope")))))

(test characterise-single-slot-canonicalizer-and-unindexed
  "Pins canonicalized lookup and the error on a genuinely unindexed slot."
  (with-ix-graph (g)
    (with-transaction () (make-ix-person :name "d" :email "D@X.COM"))
    (is (equal '("d") (ix-names (index-lookup g 'ix-person 'email "d@x.com"))))
    (signals error (index-lookup g 'ix-person 'title "x"))))

;;; --- the generalised composite comparator (GH #107) -------------------------

(test index-comparator-matches-reduce-comp-at-arity-2
  "%INDEX-COMP-LESSP must order 2-element keys exactly as REDUCE-COMP-LESSP, or
every existing single-slot index would need a rebuild."
  (let ((a (list "alice" graph-db::+null-key+))
        (b (list "bob"   graph-db::+null-key+))
        (c (list "alice" graph-db::+max-key+)))
    (is (eq (graph-db::reduce-comp-lessp a b)
            (graph-db::%index-comp-lessp a b)))
    (is (eq (graph-db::reduce-comp-lessp a c)
            (graph-db::%index-comp-lessp a c)))
    (is (eq (graph-db::reduce-comp-lessp b a)
            (graph-db::%index-comp-lessp b a)))
    (is (eq (graph-db::reduce-equal a a) (graph-db::%index-equal a a)))
    (is (eq (graph-db::reduce-equal a b) (graph-db::%index-equal a b)))))

(test index-comparator-orders-tuples-and-prefix-bounds
  "Longer keys order component-wise; a short bound key sorts before any
longer key sharing its prefix, so a prefix range scan terminates correctly."
  (let ((k  (list "a" "b" "c" graph-db::+null-key+))
        (lo (list "a" "b"))
        (hi (list "a" "b" graph-db::+max-sentinel+ graph-db::+max-key+)))
    (is-true  (graph-db::%index-comp-lessp lo k))
    (is-false (graph-db::%index-comp-lessp hi k))
    (is-true  (graph-db::%index-comp-lessp k hi))
    (is-false (graph-db::%index-equal lo k))))

(test null-component-orders-below-real-values
  "+NULL-COMPONENT+ sits above +MIN-SENTINEL+ and below every real value, so a
null-bearing tuple falls inside a prefix scan of its populated parts (#107)."
  (is-true  (less-than graph-db::+min-sentinel+ graph-db::+null-component+))
  (is-false (less-than graph-db::+null-component+ graph-db::+min-sentinel+))
  (is-true  (less-than graph-db::+null-component+ graph-db::+max-sentinel+))
  (is-true  (less-than graph-db::+null-component+ 0))
  (is-true  (less-than graph-db::+null-component+ "a"))
  (is-true  (less-than graph-db::+null-component+ 'zzz))
  (is-false (less-than 0 graph-db::+null-component+))
  (is-false (less-than "a" graph-db::+null-component+))
  (is-false (less-than graph-db::+null-component+ graph-db::+null-component+)))

(test null-component-orders-below-timestamps-and-uuids
  "The +NULL-COMPONENT+ block enumerates concrete types explicitly rather than
falling back to a general catch-all, so TIMESTAMP and UUID:UUID -- which have
their own LESS-THAN methods (utilities.lisp) -- need their own overrides too:
without one, dispatch silently falls through to the generic SYMBOL methods
and inverts the ordering, breaking transitivity with e.g. number (#107)."
  (let ((ts (local-time:now))
        (id (uuid:make-v4-uuid)))
    (is-true  (less-than graph-db::+null-component+ ts))
    (is-false (less-than ts graph-db::+null-component+))
    (is-true  (less-than graph-db::+null-component+ id))
    (is-false (less-than id graph-db::+null-component+))))

;;; --- equality ---------------------------------------------------------------

(test lookup-returns-all-sharing-nodes
  "index-lookup returns every node with the value (a non-unique slot -> many ids)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)
      (make-ix-person :name "b" :age 30)
      (make-ix-person :name "c" :age 40))
    (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
    (is (equal '("c")     (ix-names (index-lookup g 'ix-person 'age 40))))
    (is (null (index-lookup g 'ix-person 'age 99)))))

(test lookup-canonicalized
  "A canonicalizer (string-downcase) index matches case-insensitively; the probe is
canonicalized too."
  (with-ix-graph (g)
    (with-transaction () (make-ix-person :name "a" :email "Alice@X.com"))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'email "alice@x.com"))))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'email "ALICE@X.COM"))))))

(test null-and-unbound-excluded
  "A NULL / unbound indexed slot is not indexed (SQL-style); a declared-but-empty
index is a legitimate empty result, not an error."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)                ; email unbound
      (make-ix-person :name "b" :age 30 :email nil)     ; email NIL
      (make-ix-person :name "c" :age 30 :email "c@x"))  ; email present
    ;; only the node with a real email is indexed
    (is (equal '("c") (ix-names (index-lookup g 'ix-person 'email "c@x"))))
    (is (= 1 (graph-db::ix-count (graph-db::%secondary-index-lookup g 'ix-person 'email))))
    ;; querying a declared index for a value nobody holds -> empty, not an error
    (is (null (index-lookup g 'ix-person 'email "nobody@x")))))

;;; --- multi-slot tuple keys (GH #107) ------------------------------------

(test multi-slot-index-finds-exact-tuple
  "A three-component index resolves an exact tuple (#107)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-claim :ns "ops" :key "e1" :rel "at")
      (make-ix-claim :ns "ops" :key "e2" :rel "at"))
    (let ((hits (index-lookup g 'ix-claim '(ns key rel)
                             (list "ops" "e1" "at"))))
      (is (= 1 (length hits)))
      (is (string= "e1" (ix-key (first hits)))))))

(test multi-slot-index-stores-null-component
  "A tuple with a null component is still indexed, so it stays findable (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key nil :rel "at"))
    (is (= 1 (length (index-lookup g 'ix-claim '(ns key rel)
                                   (list "ops") :prefix t))))))

;;; --- query API: tuple lookup and :prefix (GH #107, Task 7) -----------------

(test short-value-without-prefix-signals
  "A value list shorter than the arity must signal, never silently return a
superset -- silent-wrong-answer is this project's dominant defect shape (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel "at"))
    (signals error (index-lookup g 'ix-claim '(ns key rel) (list "ops")))
    (is (= 1 (length (index-lookup g 'ix-claim '(ns key rel)
                                   (list "ops") :prefix t))))))

(test long-value-signals-even-with-prefix
  "Too MANY values is never a prefix scan -- :PREFIX T does not rescue an
overshoot, only a shortfall (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel "at"))
    (signals error (index-lookup g 'ix-claim '(ns key rel)
                                 (list "ops" "e1" "at" "extra") :prefix t))))

(test prefix-lookup-finds-null-trailing-components
  "A prefix lookup must return a row whose components BEYOND the queried
prefix are null (stored as +NULL-COMPONENT+) -- otherwise storing nulls
rather than skipping them (Task 5) buys nothing (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel nil))
    (let ((hits (index-lookup g 'ix-claim '(ns key rel)
                              (list "ops" "e1") :prefix t)))
      (is (= 1 (length hits)))
      (is (string= "e1" (ix-key (first hits))))
      (is (null (ix-rel (first hits)))))))

(test index-range-multi-slot-full-tuple-bounds
  "INDEX-RANGE on a multi-slot index accepts a full-arity tuple for :START/
:END -- an exact-tuple window, the bounded fast path (#107)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-claim :ns "ops" :key "e1" :rel "at")
      (make-ix-claim :ns "ops" :key "e2" :rel "at")
      (make-ix-claim :ns "sec" :key "e9" :rel "at"))
    (is (equal '("e1")
               (mapcar #'ix-key
                       (index-range g 'ix-claim '(ns key rel)
                                   :start (list "ops" "e1" "at")
                                   :end   (list "ops" "e1" "at")))))))

(test index-range-multi-slot-short-tuple-bounds
  "INDEX-RANGE on a multi-slot index also accepts a SHORT (fewer than arity)
tuple for :START/:END, padded like an equality prefix scan so the bounded
fast path never mis-scopes to an id-position sentinel compared against a
value component (#107)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-claim :ns "ops" :key "e1" :rel "at")
      (make-ix-claim :ns "ops" :key "e2" :rel "at")
      (make-ix-claim :ns "ops" :key "e3" :rel "near")
      (make-ix-claim :ns "sec" :key "e9" :rel "at"))
    (is (equal '("e1" "e2")
               (sort (mapcar #'ix-key
                             (index-range g 'ix-claim '(ns key rel)
                                         :start (list "ops" "e1")
                                         :end   (list "ops" "e2")))
                     #'string<)))))

(test index-range-open-ended-over-arity-start-signals
  "An over-arity :START alone (no :END) must signal -- the open-ended
(single-bound) path needs the same arity ceiling as the bounded one, or it
silently mis-filters rows out via %INDEX-VALUE-LESSP's length tie-break
instead of erroring (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel "at"))
    (signals error
      (index-range g 'ix-claim '(ns key rel)
                  :start (list "ops" "e1" "at" "extra")))))

(test index-range-open-ended-over-arity-end-signals
  "Same defect, mirrored on :END alone (no :START) (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "ops" :key "e1" :rel "at"))
    (signals error
      (index-range g 'ix-claim '(ns key rel)
                  :end (list "ops" "e1" "at" "extra")))))

;;; --- declaration surface: positional :canonicalize (GH #107, Task 6) -------

(test resolve-index-canonicalizers-positional-vs-single-spec
  "White-box: %RESOLVE-INDEX-CANONICALIZERS must tell a positional list apart
from a single spec, even though #'FN and a LAMBDA form are both conses."
  ;; NIL / T -> identity at every position.
  (is (equal '(nil nil) (graph-db::%resolve-index-canonicalizers nil 2)))
  (is (equal '(nil nil) (graph-db::%resolve-index-canonicalizers t 2)))
  ;; A bare symbol is a single spec: component 0 only, the rest identity.
  (let ((cans (graph-db::%resolve-index-canonicalizers 'string-downcase 3)))
    (is (= 3 (length cans)))
    (is (eq (fdefinition 'string-downcase) (first cans)))
    (is (null (second cans)))
    (is (null (third cans))))
  ;; #'FN reads as (FUNCTION FN) -- a cons, but still a single spec.
  (let ((cans (graph-db::%resolve-index-canonicalizers
               '(function string-downcase) 2)))
    (is (= 2 (length cans)))
    (is (eq (fdefinition 'string-downcase) (first cans)))
    (is (null (second cans))))
  ;; A LAMBDA form is also a single spec, not a 3-element positional list.
  (let ((cans (graph-db::%resolve-index-canonicalizers
               '(lambda (x) (string-downcase x)) 1)))
    (is (= 1 (length cans)))
    (is (functionp (first cans))))
  ;; A genuine positional list: one entry per component, NIL = identity.
  (let ((cans (graph-db::%resolve-index-canonicalizers
               '(string-downcase nil) 2)))
    (is (= 2 (length cans)))
    (is (eq (fdefinition 'string-downcase) (first cans)))
    (is (null (second cans))))
  ;; A positional list entry may itself be #'FN or a LAMBDA form -- the
  ;; per-position delegation to %RESOLVE-INDEX-CANONICALIZER must handle
  ;; both, not just a bare symbol, inside the positional branch.
  (let ((cans (graph-db::%resolve-index-canonicalizers
               (list '(lambda (x) (string-upcase x)) 'string-downcase
                     '(function string-upcase))
               3)))
    (is (= 3 (length cans)))
    (is (functionp (first cans)))
    (is (string= "AB" (funcall (first cans) "ab")))
    (is (eq (fdefinition 'string-downcase) (second cans)))
    (is (eq (fdefinition 'string-upcase) (third cans))))
  ;; A positional list whose length does not match ARITY signals -- silent
  ;; truncation/padding would hide a caller's miscounted :CANONICALIZE list.
  (signals error (graph-db::%resolve-index-canonicalizers
                  '(string-downcase) 2))
  (signals error (graph-db::%resolve-index-canonicalizers
                  '(string-downcase nil nil) 2)))

(test multi-slot-canonicalizer-is-positional
  "A per-position canonicalizer applies to its own component only (#107)."
  (with-ix-graph (g)
    (with-transaction () (make-ix-claim :ns "OPS" :key "E1" :rel "at"))
    (is (= 1 (length (index-lookup g 'ix-claim '(ns key) (list "ops" "E1")))))
    (is (= 0 (length (index-lookup g 'ix-claim '(ns key) (list "ops" "e1")))))))

(test single-slot-def-index-function-designator-canonicalizer-unchanged
  "Backward-compatibility gate: a single-slot DEF-INDEX with a single #'FN
:canonicalize keeps behaving exactly as before Task 6's positional split --
applied to the one component, case-insensitive lookup either way."
  (with-ix-graph (g)
    (with-transaction () (make-ix-solo :tag "MixedCase"))
    (is (= 1 (length (index-lookup g 'ix-solo 'tag "mixedcase"))))
    (is (= 1 (length (index-lookup g 'ix-solo 'tag "MIXEDCASE"))))
    (is (= 0 (length (index-lookup g 'ix-solo 'tag "nope"))))))

;;; --- def-index (standalone declaration surface) -----------------------------

(test def-index-maintains-and-queries
  "A def-index on a slot NOT marked :index t is maintained on apply and queryable;
its :canonicalize (string-downcase) makes it case-insensitive."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :note "Hello")
      (make-ix-person :name "b" :note "world")
      (make-ix-person :name "c" :note "hello"))
    (is (equal '("a" "c") (ix-names (index-lookup g 'ix-person 'note "HELLO"))))
    (is (equal '("b")     (ix-names (index-lookup g 'ix-person 'note "world"))))))

(test def-index-empty-is-nil-not-error
  "Querying a declared def-index with no entries yet is an empty result, not an error."
  (with-ix-graph (g)
    (is (null (index-lookup g 'ix-person 'note "nobody")))))

(test def-index-reopen
  "A def-index'd index is durable and reopens from the sidecar."
  (with-temp-directory (dir)
    (let ((g (make-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (make-ix-person :name "a" :note "Hi")))
        (close-graph g)))
    (let ((g (open-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (is (equal '("a") (ix-names (index-lookup g 'ix-person 'note "HI"))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))

;;; --- range ------------------------------------------------------------------

(test range-bounded-and-open
  "index-range returns the ordered subset in [start,end]; open ends work."
  (with-ix-graph (g)
    (with-transaction ()
      (dolist (a '(10 20 30 40 50))
        (make-ix-person :name (format nil "n~D" a) :age a)))
    (is (equal '(20 30 40)
               (mapcar #'ix-age (index-range g 'ix-person 'age :start 20 :end 40))))
    (is (equal '(10 20)
               (mapcar #'ix-age (index-range g 'ix-person 'age :end 20))))
    (is (equal '(40 50)
               (mapcar #'ix-age (index-range g 'ix-person 'age :start 40))))
    (is (equal '(10 20 30 40 50)
               (mapcar #'ix-age (index-range g 'ix-person 'age))))))

;;; --- maintenance: update / delete -------------------------------------------

(test update-moves-node-between-values
  "Updating an indexed slot releases the old value and claims the new one."
  (with-ix-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-ix-person :name "a" :age 30))))
      (with-transaction ()
        (let ((v (copy (lookup-vertex id))))
          (setf (ix-age v) 31)
          (save v)))
      (is (null (index-lookup g 'ix-person 'age 30)) "old value released")
      (is (equal '("a") (ix-names (index-lookup g 'ix-person 'age 31))) "new value claimed"))))

(test delete-removes-from-index
  "A deleted node drops out of the index."
  (with-ix-graph (g)
    (let (id)
      (with-transaction () (setq id (id (make-ix-person :name "a" :age 30))))
      (with-transaction () (mark-deleted (lookup-vertex id)))
      (is (null (index-lookup g 'ix-person 'age 30))))))

;;; --- scope: subclasses ------------------------------------------------------

(test subclasses-share-parent-index
  "An index on a parent slot covers subclass instances (one shared index)."
  (with-ix-graph (g)
    (with-transaction ()
      (make-ix-person   :name "p" :age 30)
      (make-ix-employee :name "e" :age 30 :title "boss"))
    ;; querying via the parent class sees both
    (is (equal '("e" "p") (ix-names (index-lookup g 'ix-person 'age 30))))
    ;; querying via the subclass sees the subclass instance (rooted at ancestor)
    (is (member "e" (mapcar #'ix-name (index-lookup g 'ix-employee 'age 30)) :test #'string=))))

;;; --- reopen (on-disk sidecar) -----------------------------------------------

(test reopen-restores-index
  "The on-disk index reopens from its sidecar (no rebuild needed) and still answers."
  (with-temp-directory (dir)
    (let ((g (make-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction ()
               (make-ix-person :name "a" :age 30)
               (make-ix-person :name "b" :age 30)))
        (close-graph g)))
    (let ((g (open-graph *ix-graph-name* (namestring dir) :buffer-pool-size 1000)))
      (unwind-protect
           (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))

(test reopen-resolves-composite-canonicalizer-not-shared-first-slot
  "A multi-slot DEF-INDEX whose first slot ALSO carries its own independent
:index declaration must resolve ITS OWN canonicalizer on reopen, not the
unrelated single-slot one that merely shares a name (#107).  IX-DUAL.A is
:index STRING-UPCASE; (A B) is STRING-DOWNCASE'd -- deliberately conflicting
transforms.  Before the fix, %OWNER-SLOT-CANONICALIZER's arity-blind FIND
matched A's declaration first and returned STRING-UPCASE for the composite
too, so a reopened lookup -- built by upcasing the query instead of
downcasing it -- would never match the entry the ORIGINAL write-time
(correct) canonicalizer stored downcased."
  (with-temp-directory (dir)
    (let ((g (make-graph *ix-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction () (make-ix-dual :a "MixedCase" :b "y")))
        (close-graph g)))
    (let ((g (open-graph *ix-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (is (= 1 (length (index-lookup g 'ix-dual '(a b)
                                          (list "MixedCase" "y")))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))

(test reopen-resolves-positional-canonicalizer-per-position
  "A multi-slot DEF-INDEX with a POSITIONAL :canonicalize survives a
close/reopen cycle, applying each entry to its own component -- not
signalling on open, and not collapsing to component-0-only (#107, Task 6).
IX-CLAIM's (NS KEY) index is (string-downcase nil): NS downcased, KEY left
alone.  Before the %OWNER-SLOT-CANONICALIZER fix, this reopen path handed
the positional list straight to the SINGULAR resolver, which has no branch
for a list spec and signals \"Invalid :INDEX spec\" -- the graph could not
even finish opening."
  (with-temp-directory (dir)
    (let ((g (make-graph *ix-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction ()
               (make-ix-claim :ns "OPS" :key "E1" :rel "at")))
        (close-graph g)))
    (let ((g (open-graph *ix-graph-name* (namestring dir)
                         :buffer-pool-size 1000)))
      (unwind-protect
           (progn
             ;; component 0 (NS) was canonicalized -> a lowercase query matches.
             (is (= 1 (length (index-lookup g 'ix-claim '(ns key)
                                            (list "ops" "E1")))))
             ;; component 1 (KEY) was NOT canonicalized -> case must match.
             (is (= 0 (length (index-lookup g 'ix-claim '(ns key)
                                            (list "ops" "e1"))))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))

(test legacy-single-slot-sidecar-restores
  "A sidecar record carrying a bare symbol (written before #107) must
restore as a 1-list, with no rebuild."
  (is (equal '(name)
             (graph-db::%normalize-slots 'name)))
  (is (equal '(ns key)
             (graph-db::%normalize-slots '(ns key)))))

(test legacy-sidecar-record-reopens-without-rebuild
  "The pre-Task-3 sidecar format stored a bare symbol in the slot position
(see fb83bf4^:index.lisp's SAVE-SECONDARY-INDEX-ROOTS); Task 3 changed the
write side to a list, but nothing exercised a sidecar written by the OLD
code.  DESTRUCTURING-BIND against that shape with no normalisation would
either error outright or, if masked by a broad handler, silently fall back
to REBUILD-SECONDARY-INDEXES -- which would look identical from the query
answer alone.  This constructs a real sidecar file with a legacy bare-symbol
record, then proves via the same fdefinition-shadow technique as
REOPEN-RESTORES-DURABLE-INDEX-AND-ENFORCES (unique-constraint-tests.lisp)
that reopen restores the persisted skip-list and never calls
REBUILD-SECONDARY-INDEXES (#107)."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-person :name "a" :age 30)
                 (make-ix-person :name "b" :age 30)))
          (close-graph g)))
      (let ((file (graph-db::secondary-index-root-file path)))
        (is-true (probe-file file)
                  "close-graph saved the secondary-index sidecar")
        ;; Downgrade every single-slot record's slot field from a 1-list back
        ;; to the bare symbol pre-Task-3 code wrote -- multi-slot lists did
        ;; not exist yet, so a genuine legacy file has only this shape.
        (let ((records (cl-store:restore file)))
          (graph-db::%atomic-cl-store
           (mapcar (lambda (r)
                     (destructuring-bind (owner slot-names address
                                          &optional (backend :skip-list)) r
                       (if (= 1 (length slot-names))
                           (list owner (first slot-names) address backend)
                           r)))
                   records)
           file)))
      (let ((rebuilt nil)
            (orig (fdefinition 'graph-db::rebuild-secondary-indexes)))
        (unwind-protect
             (progn
               (setf (fdefinition 'graph-db::rebuild-secondary-indexes)
                     (lambda (gr) (setf rebuilt t) (funcall orig gr)))
               (let ((g2 (open-graph *ix-graph-name* path
                                     :buffer-pool-size 1000)))
                 (unwind-protect
                      (progn
                        (is (null rebuilt)
                            "reopen restored the legacy sidecar, no scan")
                        (is (equal '("a")
                                   (ix-names (index-lookup g2 'ix-person
                                                            'name "a"))))
                        (is (equal '("a" "b")
                                   (ix-names (index-lookup g2 'ix-person
                                                            'age 30)))))
                   (ignore-errors (close-graph g2))
                   (collect-garbage))))
          (setf (fdefinition 'graph-db::rebuild-secondary-indexes) orig))))))

(test secondary-sidecar-torn-write-falls-back-to-rebuild
  "GH #63: a truncated secondary-index sidecar must not prevent the graph from
opening.  Before the fix, CL-STORE:RESTORE's error propagated straight out of
OPEN-GRAPH (via RESTORE-SECONDARY-INDEX-ROOTS) and the open itself failed; now
it falls back to REBUILD-SECONDARY-INDEXES, exactly as the spatial sidecar
already does."
  (with-temp-directory (dir)
    (let ((path (namestring dir)))
      (let ((g (make-graph *ix-graph-name* path :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* g))
               (with-transaction ()
                 (make-ix-person :name "a" :age 30)
                 (make-ix-person :name "b" :age 30)))
          (close-graph g)))
      ;; Truncate the sidecar mid-record, as an interrupted write would.
      (let* ((file (graph-db::secondary-index-root-file path))
             (bytes (with-open-file (in file :element-type '(unsigned-byte 8))
                      (let ((b (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                        (read-sequence b in)
                        b))))
        (with-open-file (out file :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
          (write-sequence bytes out :end (floor (length bytes) 2))))
      (handler-bind ((warning #'muffle-warning))    ; the torn-sidecar warning
        (let ((g (open-graph *ix-graph-name* path :buffer-pool-size 1000)))
          (unwind-protect
               (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30)))
                   "the index was rebuilt from the still-intact nodes")
            (ignore-errors (close-graph g))
            (collect-garbage)))))))

;;; --- dual backend -----------------------------------------------------------

(test bplus-backend-equality-and-range
  "The index works identically on the B+ tree backend."
  (with-ix-graph (g :backend :bplus-tree)
    (with-transaction ()
      (dolist (a '(10 20 20 30))
        (make-ix-person :name (format nil "n~D" a) :age a)))
    (is (= 2 (length (index-lookup g 'ix-person 'age 20))))
    (is (equal '(10 20 20)
               (mapcar #'ix-age (index-range g 'ix-person 'age :end 20))))))

;;; --- wrong-graph discipline -------------------------------------------------

(test index-resolves-in-passed-graph
  "index-lookup / index-range resolve ids in the GRAPH argument, not the ambient
*graph* (the wrong-graph audit discipline)."
  (with-ix-graph (b)
    (with-transaction ()
      (make-ix-person :name "a" :age 30)
      (make-ix-person :name "b" :age 30))
    (with-temp-directory (dir-a)
      (let ((a (make-graph :ix-decoy (namestring dir-a) :buffer-pool-size 1000)))
        (unwind-protect
             (let ((*graph* a))            ; ambient graph is the WRONG one
               (is (equal '("a" "b") (ix-names (index-lookup b 'ix-person 'age 30))))
               (is (= 2 (length (index-range b 'ix-person 'age :start 30 :end 30)))))
          (ignore-errors (close-graph a :snapshot-p nil))
          (collect-garbage))))))

;;; --- memory backend ---------------------------------------------------------

(defmacro with-ix-memory-graph ((g) &body body)
  "A fresh in-memory graph named *IX-GRAPH-NAME*, in a temp dir."
  (let ((dir (gensym)))
    `(with-temp-directory (,dir)
       (let ((,g (graph-db::make-memory-graph *ix-graph-name* (namestring ,dir))))
         (unwind-protect (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;;; --- arity-aware skip-list construction (GH #107) ---------------------------

;; Runs on the memory-graph mem-skip-list, not WITH-IX-GRAPH's on-disk
;; :skip-list.  MEM-SKIP-LIST stores keys as plain Lisp objects with no
;; serialization step, so it proves the head/tail-arity property with no
;; codec involved at all.  The on-disk equivalent -- which DOES need a codec
;; that can serialize a 4-element key, the gap Task 4 found and Task 4b
;; closed -- is ON-DISK-ARITY-3-INDEX-CONSTRUCTS-AND-ROUND-TRIPS, below.
(test secondary-skip-list-head-tail-match-arity
  "Head/tail sentinel keys must have arity+1 elements, or a multi-slot index's
bounds sort wrongly against real keys (#107).  Neither skip-list backend has a
%SL-HEAD-KEY / %SL-TAIL-KEY reader: head/tail are SKIP-NODEs, keyed via
%SN-KEY -- MEM-SKIP-LIST-HEAD / MEM-SKIP-LIST-TAIL on the in-RAM backend used
here."
  (with-ix-memory-graph (g)
    (let* ((sl (graph-db::make-secondary-skip-list g 3))
           (head (graph-db::mem-skip-list-head sl))
           (tail (graph-db::mem-skip-list-tail sl)))
      (is (= 4 (length (graph-db::%sn-key head))))
      (is (= 4 (length (graph-db::%sn-key tail)))))))

;;; --- index key codec (GH #107) ------------------------------------------

;; VIEW-KEY-SERIALIZE (views.lisp) does (concatenate 'vector (second key)
;; payload) -- it assumes SECOND is the node id, true only at arity 1.  At
;; arity > 1 SECOND is a value component, not the id, and the CONCATENATE
;; signals a TYPE-ERROR: an on-disk multi-slot index could not be
;; constructed at all (Task 4 could only exercise the in-RAM MEM-SKIP-LIST,
;; above).  %INDEX-KEY-SERIALIZE / %INDEX-KEY-DESERIALIZE (index.lisp) are
;; the index's own codec -- VIEW-KEY-SERIALIZE / VIEW-KEY-DESERIALIZE stay
;; untouched (they are shared with views and :UNIQUE).
;;
;; NOTE ON THE COMPARATOR: EQUAL, not EQUALP, would be the natural choice for
;; comparing two deserialized keys, but EQUAL on a general (non-string,
;; non-bit-vector) array is EQ, not elementwise -- it would report the id
;; byte-array mismatched even when its 16 bytes match, since deserialize
;; always allocates a fresh array.  EQUALP compares array elements, which is
;; what "the same key came back" actually means here.
(test index-key-codec-round-trips-and-matches-at-arity-1
  "Arity 1 must be byte-identical to the view codec (no rebuild), and every
arity must round-trip -- including a single component that is itself a list."
  (let ((id (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))
    ;; requirement 1: byte-identical at arity 1
    (is (equalp (graph-db::view-key-serialize (list "alice" id))
                (graph-db::%index-key-serialize (list "alice" id))))
    (is (equalp (graph-db::view-key-serialize (list 42 id))
                (graph-db::%index-key-serialize (list 42 id))))
    ;; requirement 2: round-trip at several arities
    (dolist (key (list (list "alice" id)
                       (list '("a" "b") id)   ; list-valued single component
                       (list "ops" "e1" id)
                       (list "ops" "e1" "at" id)))
      (is (equalp key (graph-db::%index-key-deserialize
                       (graph-db::%index-key-serialize key)))))))

;; The assertion Task 4 could not make: an on-disk arity-3 index that
;; actually constructs (VIEW-KEY-SERIALIZE would TYPE-ERROR on the first
;; write -- the 4-element head/tail key alone) and round-trips a lookup
;; through real mmap-backed serialize/deserialize, not MEM-SKIP-LIST's plain
;; Lisp objects.
(test on-disk-arity-3-index-constructs-and-round-trips
  "An on-disk (:skip-list) secondary index of arity 3 constructs, and a
range-cursor lookup by its full (v1 v2 v3) prefix returns the id inserted
under it."
  (with-ix-graph (g)
    (let* ((sl (graph-db::make-secondary-skip-list g 3))
           (id (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))
      (add-to-skip-list sl (list "ops" "e1" "at" id) nil)
      (let* ((cur (make-range-cursor
                   sl
                   (list "ops" "e1" "at" graph-db::+null-key+)
                   (list "ops" "e1" "at" graph-db::+max-key+)))
             (node (cursor-next cur :eoc)))
        (is (not (eql node :eoc)))
        (is (equalp id (fourth (graph-db::%sn-key node))))
        (is (eql :eoc (cursor-next cur :eoc)))))))

(test memory-backend-equality-and-range
  "The index works on a memory-graph (mem-skip-list backing), :index t and def-index."
  (with-ix-memory-graph (g)
    (with-transaction ()
      (make-ix-person :name "a" :age 30 :note "Hi")
      (make-ix-person :name "b" :age 30)
      (make-ix-person :name "c" :age 40))
    (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
    (is (equal '(30 30 40) (mapcar #'ix-age (index-range g 'ix-person 'age))))
    (is (equal '("a") (ix-names (index-lookup g 'ix-person 'note "HI"))))))

(test memory-backend-reopen-rebuilds
  "A memory-graph rebuilds its indexes on reopen from the restored nodes."
  (with-temp-directory (dir)
    (let ((g (graph-db::make-memory-graph *ix-graph-name* (namestring dir))))
      (unwind-protect
           (let ((*graph* g))
             (with-transaction ()
               (make-ix-person :name "a" :age 30 :note "Hi")
               (make-ix-person :name "b" :age 30)))
        (close-graph g)))            ; checkpoint image + journal
    (let ((g (graph-db::open-memory-graph *ix-graph-name* (namestring dir))))
      (unwind-protect
           (progn
             (is (equal '("a" "b") (ix-names (index-lookup g 'ix-person 'age 30))))
             (is (equal '("a") (ix-names (index-lookup g 'ix-person 'note "HI")))))
        (ignore-errors (close-graph g))
        (collect-garbage)))))
