;;;; Bare (keyword) type names resolve package-aware or signal (GH #190).
(in-package #:graph-db/test)

(def-suite keyword-alias-suite :in graph-db-suite
  :description "Bare type names: unique match or a loud ambiguity error.")
(in-suite keyword-alias-suite)

;;; Two packages that legitimately share a symbol-name -- the namespaces
;;; design's motivating case (spec 3.4, GH #190).  Nothing imports them;
;;; the qualified reference is the point.
(defpackage #:graph-db-alias-pkg-a
  (:use)
  (:export #:alias-species #:alias-unique))
(defpackage #:graph-db-alias-pkg-b
  (:use)
  (:export #:alias-species))

;;; Both ALIAS-SPECIES types land in ONE store's schema -- that is the
;;; collision, and it is a schema-name collision only.  The generated
;;; MAKE-/LOOKUP-/-P helpers intern in the CLASS SYMBOL's own package
;;; (GH #172), so each ALIAS-SPECIES keeps its own helpers and neither
;;; definition clobbers the other's.  The ambiguity surface these tests
;;; exercise is LOOKUP-NODE-TYPE-BY-NAME; they never call the helpers.
(def-vertex graph-db-alias-pkg-a:alias-species () ((label :type string))
  :alias-two-store)
(def-vertex graph-db-alias-pkg-b:alias-species () ((label :type string))
  :alias-two-store)
(def-vertex graph-db-alias-pkg-a:alias-unique () ((label :type string))
  :alias-solo-store)

(defmacro with-alias-test-graph ((g store-name) &body body)
  "One store under its own system directory, like WITH-TWO-TEST-GRAPHS
(global-type-id-tests) but for a single graph of a chosen name."
  (let ((s (gensym)) (d (gensym)))
    `(with-temp-directory (,s)
       (with-temp-directory (,d)
         (let ((graph-db::*system-directory* (namestring ,s)))
           (let ((,g (make-graph ,store-name (namestring ,d)
                                 :buffer-pool-size 1000)))
             (unwind-protect (progn ,@body)
               (ignore-errors (close-graph ,g :snapshot-p nil))
               (collect-garbage))))))))

(defun %vertex-sub-table (graph)
  (gethash :vertex (graph-db::schema-type-table (graph-db::schema graph))))

(test bare-name-resolves-when-unique
  "A keyword designator still works when exactly one registered type
matches -- the public make-vertex/map-vertices convenience survives.
Nearest wrong implementation: keyword lookups always return NIL."
  (with-alias-test-graph (g :alias-solo-store)
    (let ((meta (lookup-node-type-by-name :alias-unique :vertex :graph g)))
      (is (graph-db::node-type-p meta))
      (is (eq 'graph-db-alias-pkg-a:alias-unique
              (graph-db::node-type-name meta))))))

(test ambiguous-bare-name-signals
  "Two same-named types in different packages: a bare name is genuinely
ambiguous and must ERROR, never resolve to whichever was defined last.
Nearest wrong implementation: return the first (or last) match."
  (with-alias-test-graph (g :alias-two-store)
    (signals graph-db:ambiguous-node-type-name
      (lookup-node-type-by-name :alias-species :vertex :graph g))))

(test qualified-names-resolve-past-the-ambiguity
  "The package-qualified symbols each reach their own type, with distinct
registry ids -- the alias collision never touched the real keys."
  (with-alias-test-graph (g :alias-two-store)
    (let ((meta-a (lookup-node-type-by-name
                   'graph-db-alias-pkg-a:alias-species :vertex :graph g))
          (meta-b (lookup-node-type-by-name
                   'graph-db-alias-pkg-b:alias-species :vertex :graph g)))
      (is (eq 'graph-db-alias-pkg-a:alias-species
              (graph-db::node-type-name meta-a)))
      (is (eq 'graph-db-alias-pkg-b:alias-species
              (graph-db::node-type-name meta-b)))
      (is (/= (graph-db::node-type-id meta-a)
              (graph-db::node-type-id meta-b))))))

(test keyword-alias-no-longer-written
  "UPDATE-NODE-TYPE stops writing the third (keyword) key.  Fails against
the pre-#190 code, which stored :ALIAS-UNIQUE -> id here."
  (with-alias-test-graph (g :alias-solo-store)
    (is (null (gethash :alias-unique (%vertex-sub-table g))))))

(test stale-persisted-alias-is-ignored
  "Old schema.dat files carry alias entries written by the old code, and
after the very collision this fixes they can point at the WRONG id.  The
keyword path must resolve by scanning the real symbol keys, never by
GETHASH on the keyword.  Nearest wrong implementation: try GETHASH first
and only scan on a miss."
  (with-alias-test-graph (g :alias-solo-store)
    (setf (gethash :alias-unique (%vertex-sub-table g)) 999999)
    (let ((meta (lookup-node-type-by-name :alias-unique :vertex :graph g)))
      (is (graph-db::node-type-p meta))
      (is (/= 999999 (graph-db::node-type-id meta))))))
