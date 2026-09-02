(ql:quickload :graph-db)
(in-package :graph-db)

(defvar *graph-name* :test-graph)
(defvar *graph-path* "/var/tmp/test-graph/")
;; Type-ids come from the registry in *SYSTEM-DIRECTORY*; a store cannot be
;; opened without one (GH #186).
(setf *system-directory*
      (namestring (ensure-directories-exist "/var/tmp/test-graph-system/")))
(log:config :all :sane :d :nopretty :thread :daily "/var/tmp/graph.log")

;;; Types
(defun email-p (x)
  (and (stringp x)
       (find #\@ x)
       x))
(deftype email () `(satisfies email-p))

;;; Schema.  ⚠ These forms must be LOADED before make-graph/open-graph:
;;; schema.dat persists type metadata for id stability, never the CLOS
;;; classes themselves (GH #144).
(def-vertex person ()
  ((first-name :type string)
   (middle-name :type string)
   (last-name :type string))
  :test-graph)

;; :unique t puts the slot under a uniqueness constraint, enforced at the
;; commit boundary (NULL-exempt): two customers with one email cannot both
;; commit.  (To QUERY a slot by value, give it a DEF-INDEX or a view, as
;; below -- :unique enforces; it is not a lookup surface.)
(def-vertex customer (person)
  ((email :type email :unique t))
  :test-graph)

;; :vector-index t gives the slot a dedicated mmap vector segment,
;; maintained by the transaction apply path; VECTOR-SEARCH below does
;; cosine kNN over it.  (Toy 3-dim embeddings; real ones are model-sized.)
(def-vertex product ()
  ((name :type string)
   (upc :type string)
   (embedding :vector-index t))
  :test-graph)

;; A geometry slot marked :index t opts the type into the spatial index: every
;; merchant is automatically (re)indexed by its LOCATION on commit, with no
;; hand-written node-geometry method.  (make-point takes lon, lat in WGS84.)
(def-vertex merchant ()
  ((name :type string)
   (location :type geometry :index t))
  :test-graph)

(def-edge likes ()
 ()
 :test-graph)

(def-edge sells ()
 ()
 :test-graph)

(setq *graph* (make-graph *graph-name* *graph-path* :buffer-pool-size 10000))

;;; Indexes
;; This will index both customers and people
(def-view last-name :lessp (person :test-graph)
  (:map
   (lambda (person)
     (when (slot-value person 'last-name)
       (yield (slot-value person 'last-name) nil)))))

;; This will only index customers
(def-view email :lessp (customer :test-graph)
  (:map
   (lambda (customer)
     (when (slot-value customer 'email)
       (yield (slot-value customer 'email) nil)))))

;; Example of a map-reduce view
(def-view popularity :greaterp (likes :test-graph)
  (:map
   (lambda (like-edge)
     (yield (string-id (to like-edge)) 1)))
  (:reduce
   (lambda (keys values)
     (declare (ignore keys))
     (apply '+ values))))


;; A general ordered index needs no map function: DEF-INDEX indexes a slot
;; by its stored value, for equality and range lookups.  Declarative and
;; idempotent like DEF-VIEW; :CANONICALIZE makes this one case-insensitive.
(def-index product upc :test-graph :canonicalize string-downcase)

(defun lookup-people-by-last-name (last-name)
  (let ((people (invoke-graph-view 'person 'last-name :key last-name)))
    (if people
        (mapcar (lambda (person)
                  (lookup-vertex (cdr (assoc :id person))))
                people)
        nil)))

(defun lookup-customer-by-email (email)
  (let ((customers (invoke-graph-view 'customer 'email :key email)))
    (if customers
        (lookup-vertex (cdr (assoc :id (first customers))))
        nil)))

;;; Add some data
(with-transaction ()
  (let ((c1 (make-customer :first-name "Joe" :last-name "Blow" :email "joe@blow.com"))
        (c2 (make-customer :first-name "Jill" :last-name "Blow" :email "jill@blow.com"))
        ;; m1 carries a LOCATION (lon, lat); committing it indexes it spatially.
        (m1 (make-merchant :name "Snake Oil, Inc."
                           :location (make-point 12.3424d0 45.6720d0)))
        (p1 (make-product :name "Oil of Longevity" :upc "1234567890"
                          :embedding #(0.9 0.1 0.0)))
        (p2 (make-product :name "Oil of Slipperiness" :upc "abcdefghijk"
                          :embedding #(0.7 0.6 0.1))))
    ;; Two more merchants: one ~1 km away, one in another city -- so the
    ;; proximity queries below have something to discriminate.
    (make-merchant :name "Elixir Emporium" :location (make-point 12.3520d0
                                                       45.6780d0))
    (make-merchant :name "Faraway Tonics"  :location (make-point 2.4683d0
                                                       41.7763d0))
    (make-sells :from m1 :to p1)
    ;; The above is equivalent to
    ;; (make-edge 'sells m1 p1 1 nil)
    (make-sells :from m1 :to p2)
    (make-likes :from c1 :to p1 :weight 100.0)
    (make-likes :from c1 :to p2 :weight 20.0)
    (make-likes :from c2 :to p2 :weight 50.0)))

;;; Now run some queries
(lookup-customer-by-email "joe@blow.com")

(lookup-people-by-last-name "Blow")

(select (:flat nil)
        (?liker ?product)
        (likes ?liker ?product))

(select (:flat nil :limit 1 :skip 0)
        (?liker ?product ?how-much)
        (likes ?liker ?product ?how-much))

(select-flat (?customer) (is-a ?customer customer))

(let ((person (select-one (?person) (is-a ?person person))))
  (declare (special person))
  (destructuring-bind (product like-qty)
      (select (:flat t :limit 1 :skip 0)
              (?product ?like-qty)
              (lisp ?person person) ;; Import the person into Prolog
              (likes ?person ?product ?like-qty))
    (format nil "~A likes '~A' with a degree of ~F"
            (slot-value person 'first-name)
            (slot-value product 'name)
            like-qty)))

(map-reduced-view (lambda (key id value)
                    (declare (ignore id))
                    (let ((product (lookup-vertex key)))
                      (cons product value)))
                  'likes
                  'popularity
                  :collect-p t)

(map-vertices (lambda (person)
                (format t "~A is a person~%" person)
                person)
              *graph*
              :collect-p t
              :vertex-type 'person)

(map-edges (lambda (edge)
             (let ((how-much (weight edge))
                   (product (lookup-vertex (to edge))))
               (cons product how-much)))
           *graph*
           :collect-p t
           :edge-type 'likes
           :vertex (lookup-customer-by-email "joe@blow.com")
           :direction :out)

;;; Ordered, unique and vector indexes ----------------------------------
;;;
;;; The DEF-INDEX above and the :UNIQUE slot are both ordered secondary
;;; indexes over stored values -- no view lambda involved.

;; Equality, through the :CANONICALIZE (case does not matter).
(index-lookup *graph* 'product 'upc "ABCdefGHIJK")

;; Ascending range scan over [start, end]; open-ended when NIL.
(index-range *graph* 'product 'upc :start "1" :end "2")

;; MAP-INDEX streams the same range without consing a list.
(map-index (lambda (product) (format t "~a~%" (name product)))
           *graph* 'product 'upc)

;; The :UNIQUE slot refuses a duplicate at commit: nothing of the
;; failed transaction survives.  (Equality lookups on email go through
;; the EMAIL view defined above.)
(handler-case
    (with-transaction ()
      (make-customer :first-name "Imposter" :last-name "Blow"
                     :email "joe@blow.com"))
  (unique-constraint-violation (c)
    (format t "~&Refused, as it should be:~%~a~%" c)))

;; Cosine kNN over the :VECTOR-INDEX slot: the k products most similar
;; to a query vector, best first.
(vector-search *graph* 'product 'embedding #(1.0 0.0 0.0) 2)

;;; Spatial queries
;;;
;;; Because MERCHANT has a (location :type geometry :index t) slot, every
;;; merchant was placed in the graph's spatial index on commit.  No extra
;;; bookkeeping is needed -- the transaction write-path maintains it.

;; Every spatial query takes a SCOPE first: a node-class name, a list of them,
;; or :ALL.  The scope picks which per-class index is scanned AND filters the
;; results by type, so a query for merchants can never return anything else.

;; Merchants within 2 km of a downtown point (lat, lon, radius-metres).
;; Returns (merchant . distance-metres) pairs, nearest first.
(find-nodes-near 'merchant 45.6720d0 12.3424d0 2000d0)
;; => Snake Oil, Inc. (~0 m) and Elixir Emporium (~1 km); Faraway Tonics
;;    (another city) is excluded.

;; The two nearest merchants to that same point, nearest first.
(find-nearest-k 'merchant 45.6720d0 12.3424d0 2)

;; Merchants whose location falls inside an area of interest (a polygon, given
;; as rings of (lon lat) -- the first ring is the outer boundary).
(find-nodes-within
 'merchant
 (make-polygon '(((12.335d0 45.666d0) (12.365d0 45.666d0)
                  (12.365d0 45.682d0) (12.335d0 45.682d0)
                  (12.335d0 45.666d0)))))

;; The same proximity query, composed in Prolog.  find-near/5 takes the scope as
;; its second argument and yields nodes, so it cooperates with the rest of the
;; query language.  (The scope already restricts the answer to merchants, so the
;; is-a goal the pre-scope API needed is no longer required.)
(select-flat (?m)
  (find-near ?m merchant 45.6720d0 12.3424d0 2000d0))

;;; Updating a node ------------------------------------------------------
;;;
;;; LOOKUP-* (and view helpers like LOOKUP-CUSTOMER-BY-EMAIL above) return
;;; the SHARED cached node -- the same object every other reader and thread
;;; holds.  Writing its slots in place would be invisible to disk and
;;; visible to everyone else immediately, with no transaction protecting
;;; it.  So COPY it inside the transaction, modify the copy, and SAVE that.
;;; Writing an uncopied node signals MUTATING-UNREGISTERED-NODE (GH #135).

(with-transaction ()
  (let ((c (copy (lookup-customer-by-email "joe@blow.com"))))
    (setf (email c) "joe@blowfish.com")
    (save c)))

;;; A node created in THIS transaction is different: it has no committed
;;; version to update against, so it needs no copy -- and COPY of it
;;; signals COPYING-UNCOMMITTED-NODE.  Just set its slots directly.

(with-transaction ()
  (let ((c3 (make-customer :first-name "Cara" :last-name "Blow"
                            :email "cara@blow.com")))
    (setf (email c3) "cara.blow@blow.com")))

(close-graph *graph*)
