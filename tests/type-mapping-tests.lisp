;;;; Tests for the type-filtering of MAP-EDGES / MAP-VERTICES: subtype inclusion
;;;; (the new edge-subtype support + the existing vertex-subtype support), the
;;;; INCLUDE-/EXCLUDE- type-list params, and -- critically -- that an all-types
;;;; scan visits a subtype edge exactly once (no double-count).
;;;;
;;;; Uses its OWN graph (:graph-db-type-mapping-test) and the first edge-subtype
;;;; in the codebase, isolated from the other suites' schemas.

(in-package #:graph-db/test)

(eval-when (:load-toplevel :execute)
  (setf (gethash :graph-db-type-mapping-test *schema-node-metadata*) nil))

(def-vertex tm-animal ()
  ((name :type string))
  :graph-db-type-mapping-test)

(def-vertex tm-dog (tm-animal)            ; vertex subtype
  ()
  :graph-db-type-mapping-test)

(def-edge tm-rel ()                       ; parent edge type
  ()
  :graph-db-type-mapping-test)

(def-edge tm-friend (tm-rel)              ; EDGE SUBTYPE (first in the codebase)
  ()
  :graph-db-type-mapping-test)

(def-edge tm-other ()                     ; unrelated edge type
  ()
  :graph-db-type-mapping-test)

(def-suite type-mapping-suite
  :description "MAP-EDGES / MAP-VERTICES subtype + include/exclude type lists."
  :in graph-db-suite)

(in-suite type-mapping-suite)

(defmacro with-tm-graph ((g) &body body)
  (let ((dir (gensym "DIR")))
    `(with-temp-directory (,dir)
       (let ((,g (make-graph :graph-db-type-mapping-test (namestring ,dir)
                             :buffer-pool-size 1000)))
         (unwind-protect
              (let ((*graph* ,g)) ,@body)
           (ignore-errors (close-graph ,g :snapshot-p nil))
           (collect-garbage))))))

;; a (tm-animal) -tm-rel-> b (tm-animal); a -tm-friend-> c (tm-dog);
;; b -tm-other-> c.  Bind A/B/C in the lexical environment of BODY.
(defmacro with-tm-data ((g a b c) &body body)
  `(with-tm-graph (,g)
     (let (,a ,b ,c)
       (declare (ignorable ,a ,b ,c))
       (with-transaction ()
         (setf ,a (make-tm-animal :name "a")
               ,b (make-tm-animal :name "b")
               ,c (make-tm-dog :name "c"))
         (make-tm-rel :from ,a :to ,b)
         (make-tm-friend :from ,a :to ,c)
         (make-tm-other :from ,b :to ,c))
       ,@body)))

(defun edge-count-of (&rest map-edges-args)
  (length (apply #'map-edges 'identity (first map-edges-args) :collect-p t
                 (rest map-edges-args))))

;;; ---- edge subtype inclusion -------------------------------------

(test map-edges-parent-type-includes-subtype
  "A parent-edge-type scan returns the parent's edges AND its subtype's edges."
  (with-tm-data (g a b c)
    (is (= 2 (edge-count-of g :edge-type 'tm-rel)))   ; tm-rel + tm-friend
    (is (= 1 (edge-count-of g :edge-type 'tm-friend))) ; the subtype alone
    (is (= 1 (edge-count-of g :edge-type 'tm-other)))))

(test map-edges-include-subclasses-nil-is-exact
  ":include-subclasses-p nil restricts a parent-type scan to the exact type."
  (with-tm-data (g a b c)
    (is (= 1 (edge-count-of g :edge-type 'tm-rel :include-subclasses-p nil)))))

;;; ---- include / exclude edge-type lists --------------------------

(test map-edges-include-edge-types-list-union
  ":include-edge-types unions the listed types (with their subtypes)."
  (with-tm-data (g a b c)
    ;; tm-rel(+tm-friend subtype) + tm-other = 3
    (is (= 3 (edge-count-of g :include-edge-types '(tm-rel tm-other))))))

(test map-edges-list-dedups-overlapping-subtree
  "Listing a type and its own subtype does NOT double-count the subtype."
  (with-tm-data (g a b c)
    ;; tm-rel expands to {tm-rel,tm-friend}; tm-friend expands to {tm-friend};
    ;; the union is {tm-rel,tm-friend} -> 2 edges, tm-friend counted once.
    (is (= 2 (edge-count-of g :include-edge-types '(tm-rel tm-friend))))))

(test map-edges-exclude-edge-types
  ":exclude-edge-types removes a type from an all-types scan."
  (with-tm-data (g a b c)
    (is (= 3 (edge-count-of g)))                         ; all three
    (is (= 2 (edge-count-of g :exclude-edge-types '(tm-other))))))

;;; ---- the no-double-count guard (all-types scans) ----------------

(test map-edges-all-types-visits-subtype-once
  "An all-types scan visits the subtype edge exactly once (not under its parent
too).  This is the double-count regression guard."
  (with-tm-data (g a b c)
    (is (= 3 (edge-count-of g)))))                        ; not 4

(test outgoing-edges-all-types-no-double-count
  "A vertex's all-types adjacency scan visits each adjacent edge once, including
a subtype edge."
  (with-tm-data (g a b c)
    ;; a has tm-rel->b and tm-friend->c : two out-edges, each once.
    (is (= 2 (length (outgoing-edges a :graph g))))))

;;; ---- outgoing-edges subtype + list ------------------------------

(test outgoing-edges-parent-type-includes-subtype
  "outgoing-edges :edge-type parent includes the subtype edge; nil excludes it."
  (with-tm-data (g a b c)
    (is (= 2 (length (outgoing-edges a :graph g :edge-type 'tm-rel))))
    (is (= 1 (length (outgoing-edges a :graph g :edge-type 'tm-rel
                                       :include-subclasses-p nil))))
    (is (= 1 (length (outgoing-edges a :graph g
                                       :include-edge-types '(tm-friend)))))))

;;; ---- map-vertices: subtype + include/exclude vertex-type lists ----

(test map-vertices-parent-type-includes-subtype
  "A parent-vertex-type scan includes its subtype (tm-dog is a tm-animal)."
  (with-tm-data (g a b c)
    (is (= 3 (length (map-vertices 'identity g :vertex-type 'tm-animal
                                              :collect-p t))))
    (is (= 2 (length (map-vertices 'identity g :vertex-type 'tm-animal
                                              :include-subclasses-p nil
                                              :collect-p t))))
    (is (= 1 (length (map-vertices 'identity g :vertex-type 'tm-dog
                                              :collect-p t))))))

(test map-vertices-include-and-exclude-vertex-types
  ":include-vertex-types selects a list; :exclude-vertex-types removes a subtype."
  (with-tm-data (g a b c)
    (is (= 1 (length (map-vertices 'identity g :include-vertex-types '(tm-dog)
                                              :collect-p t))))
    ;; tm-animal (incl. tm-dog subtype) minus tm-dog = the two plain animals
    (is (= 2 (length (map-vertices 'identity g :vertex-type 'tm-animal
                                              :exclude-vertex-types '(tm-dog)
                                              :collect-p t))))))
