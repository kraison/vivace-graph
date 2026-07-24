(ql:quickload :graph-db)
(in-package :graph-db)

(defvar *graph-name* :test-graph)
(defvar *graph-path* "/var/tmp/test-graph/")
(log:config :all :sane :d :nopretty :thread :daily "/var/tmp/graph.log")

;;; Types
(defun email-p (x)
  (and (stringp x)
       (find #\@ x)
       x))
(deftype email () `(satisfies email-p))

;;; Schema
(def-vertex person ()
  ((first-name :type string)
   (middle-name :type string)
   (last-name :type string))
  :test-graph)

(def-vertex customer (person)
  ((email :type email))
  :test-graph)

(def-vertex product ()
  ((name :type string)
   (upc :type string))
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
                           :location (make-point 37.1724d0 49.2020d0)))
        (p1 (make-product :name "Oil of Longevity" :upc "1234567890"))
        (p2 (make-product :name "Oil of Slipperiness" :upc "abcdefghijk")))
    ;; Two more merchants: one ~1.5 km away, one in another city -- so the
    ;; proximity queries below have something to discriminate.
    (make-merchant :name "Elixir Emporium" :location (make-point 37.1850d0 49.2080d0))
    (make-merchant :name "Faraway Tonics"  :location (make-point 23.7183d0 50.0263d0))
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
(find-nodes-near 'merchant 49.2020d0 37.1724d0 2000d0)
;; => Snake Oil, Inc. (~0 m) and Elixir Emporium (~1.5 km); Faraway Tonics
;;    (another city) is excluded.

;; The two nearest merchants to that same point, nearest first.
(find-nearest-k 'merchant 49.2020d0 37.1724d0 2)

;; Merchants whose location falls inside an area of interest (a polygon, given
;; as rings of (lon lat) -- the first ring is the outer boundary).
(find-nodes-within
 'merchant
 (make-polygon '(((37.165d0 49.196d0) (37.195d0 49.196d0)
                  (37.195d0 49.212d0) (37.165d0 49.212d0)
                  (37.165d0 49.196d0)))))

;; The same proximity query, composed in Prolog.  find-near/5 takes the scope as
;; its second argument and yields nodes, so it cooperates with the rest of the
;; query language.  (The scope already restricts the answer to merchants, so the
;; is-a goal the pre-scope API needed is no longer required.)
(select-flat (?m)
  (find-near ?m merchant 49.2020d0 37.1724d0 2000d0))

(close-graph *graph*)
