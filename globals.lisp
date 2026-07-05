(in-package :graph-db)

;;; ECL threading-primitive capability gate.
;;;
;;; The custom rw-lock (rw-lock.lisp) uses a `(sleep 0.001)` busy-poll on ECL
;;; instead of condition-variable blocking, a workaround for ECL 21.2.1 bugs
;;; (mp:wait-on-semaphore blocking indefinitely, condition-variable-broadcast
;;; missing waiters, condition-variable-timedwait unreliable before 23.09.09).
;;; Those are fixed in modern ECL, where the poll's ~1 ms/handoff floor is pure
;;; overhead.  Push :GRAPH-DB-ECL-MODERN-MP when running ECL >= 23.9.9 so the
;;; rw-lock can take the blocking path; older ECL keeps the safe poll fallback.
;;;
;;; eval-when so the feature is set before rw-lock.lisp is read/compiled (it
;;; loads after globals per graph-db.asd).  Default-to-safe: any parse failure or
;;; older version leaves the feature absent (poll path).  Validate on the target
;;; ECL: a wrong gate would reintroduce the 21.2.1 hangs.
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((parts (mapcar (lambda (s) (or (parse-integer s :junk-allowed t) 0))
                       (uiop:split-string (lisp-implementation-version)
                                          :separator "."))))
    (destructuring-bind (&optional (major 0) (minor 0) (patch 0) &rest rest) parts
      (declare (ignore rest))
      (when (or (> major 23)
                (and (= major 23) (or (> minor 9)
                                      (and (= minor 9) (>= patch 9)))))
        (pushnew :graph-db-ecl-modern-mp *features*)))))

(defvar *cache-enabled* t)

(defparameter *index-backend* :skip-list
  "DEFAULT ordered-map backend for a NEW graph's heap-backed indexes -- views,
:unique, and the spatial index: :SKIP-LIST (the original; default) or :BPLUS-TREE
(mmap B+ tree -- better cold-cache locality, faster reads AND writes, ~2x smaller;
see docs/bplus-tree-experiment.md).  Both speak the same ordered-map protocol over
the same (payload . node-id) composite keys, so an index behaves identically on
either.  This is only the DEFAULT: MAKE-GRAPH / OPEN-GRAPH take an :INDEX-BACKEND
keyword to choose PER GRAPH (captured in the graph's INDEX-BACKEND slot, which every
index-creation path consults).  The choice is persisted per index (views: the
view-alist :BACKEND key; unique + spatial: their root sidecar), so reopening a graph
uses each index's own written backend -- flipping this never disturbs an existing
graph (a missing tag => :skip-list).  Set this (or pass :INDEX-BACKEND) from your
application's own config; graph-db does not read an ini file itself.")

(alexandria:define-constant +db-version+ 1)

(defvar *graph* nil)
(alexandria:define-constant +main-table-file+ "main.dat" :test 'equal)
(alexandria:define-constant +meta-file+ "meta.dat" :test 'equal)
(alexandria:define-constant +data-file+ "data.dat" :test 'equal)

(defvar *schema-node-metadata* (make-hash-table :test 'equal))
(alexandria:define-constant +max-node-types+ 65536)

;; v2 (2026): MVCC node head grew 15 -> 31 bytes (commit-epoch + prev-pointer).
;; Old (v1) graphs must be migrated via MIGRATE-GRAPH (snapshot + replay).
(alexandria:define-constant +storage-version+     #x02)
(alexandria:define-constant +fixed-integer-64+    #x01)
(alexandria:define-constant +data-magic-byte+     #x17)
(alexandria:define-constant +lhash-magic-byte+    #x18)
(alexandria:define-constant +overflow-magic-byte+ #x19)
(alexandria:define-constant +config-magic-byte+   #x20)
(alexandria:define-constant +null-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 0)
   :test 'equalp)
(alexandria:define-constant +max-key+
  (make-array '(16) :element-type '(unsigned-byte 8) :initial-element 255)
   :test 'equalp)
(alexandria:define-constant +key-bytes+ 16)
(alexandria:define-constant +value-bytes+ 8)
(alexandria:define-constant +bucket-size+ 24)
(alexandria:define-constant +data-extent-size+ (* 1024 1024 100))

;; Initial sizes (in bytes) of the two memory-mapped allocator regions a graph
;; creates: HEAP (node/edge data) and INDEXES.  Both grow on demand via
;; extend-mapped-file, so these are just starting sizes; tune them per workload
;; via MAKE-GRAPH's :heap-size / :index-size, or by rebinding these defaults.
(defparameter *default-heap-size* (* 1024 1024 1000)
  "Initial size, in bytes, of a new graph's heap (node/edge data) region.")
(defparameter *default-index-size* (* 1024 1024 1000)
  "Initial size, in bytes, of a new graph's indexes region.")

;; Each memory-mapped file reserves a virtual-address window up front (PROT_NONE,
;; MAP_NORESERVE — address space only, no committed memory) and maps the file
;; into the head of it.  Growth re-maps more of the file into the reserved window
;; with MAP_FIXED, so the base pointer never moves and concurrent readers never
;; fault or need a lock.  A file may grow up to its reservation; exceeding it
;; signals an error.  The reservation is proportional to the file's initial size
;; (with a floor) rather than a flat huge value: a graph has ~15-20 mapped files,
;; so a flat multi-GB reservation each would reserve enormous VA per graph (which
;; can fail on macOS).  See mmap.lisp.
(defparameter *mmap-reservation-multiplier* 8
  "Growth headroom: a mapped file reserves this multiple of its initial size.")
(defparameter *mmap-min-reservation* (* 1024 1024 1024)
  "Floor, in bytes, for a mapped file's virtual-address reservation.")

;; Key namespaces
(defvar *vertex-namespace* (uuid:uuid-to-byte-array
                            (uuid:make-uuid-from-string "2140DCE1-3208-4354-8696-5DF3076D1CEB")))
(defvar *edge-namespace* (uuid:uuid-to-byte-array
                          (uuid:make-uuid-from-string "0392C7B5-A38B-466F-92E5-5A7493C2775A")))

;; Sentinel values for skip lists
(alexandria:define-constant +min-sentinel+ :gmin)
(alexandria:define-constant +max-sentinel+ :gmax)
;; For views, aggregrate key symbol
(alexandria:define-constant +reduce-master-key+ :gagg)

;; index-lists
(alexandria:define-constant +index-list-bytes+ 17)

;; ve-key / ve-index
(alexandria:define-constant +ve-key-bytes+ 18)
(alexandria:define-constant +null-ve-key+
    (make-array +ve-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp)
(alexandria:define-constant +max-ve-key+
    (make-array +ve-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
  :test 'equalp)

;; vev-key / vev-index
(alexandria:define-constant +vev-key-bytes+ 34)
(alexandria:define-constant +null-vev-key+
    (make-array +vev-key-bytes+ :initial-element 0 :element-type '(unsigned-byte 8))
  :test 'equalp)
(alexandria:define-constant +max-vev-key+
    (make-array +vev-key-bytes+ :initial-element 255 :element-type '(unsigned-byte 8))
   :test 'equalp)

;; Type bytes for serialization
(alexandria:define-constant +needs-lookup+ :needs-lookup)
(alexandria:define-constant +unknown+ 0)
(alexandria:define-constant +negative-integer+ 1)
(alexandria:define-constant +positive-integer+ 2)
(alexandria:define-constant +character+ 3)
(alexandria:define-constant +symbol+ 4)
(alexandria:define-constant +string+ 5)
(alexandria:define-constant +list+ 6)
(alexandria:define-constant +vector+ 7)
(alexandria:define-constant +single-float+ 8)
(alexandria:define-constant +double-float+ 9)
(alexandria:define-constant +ratio+ 10)
(alexandria:define-constant +t+ 11)
(alexandria:define-constant +null+ 12)
(alexandria:define-constant +blob+ 13) ;; Uninterpreted octets
(alexandria:define-constant +dotted-list+ 14)
(alexandria:define-constant +keyword+ 15)
(alexandria:define-constant +slot-key+ 16)
(alexandria:define-constant +id+ 17)
(alexandria:define-constant +vertex+ 18)
(alexandria:define-constant +edge+ 19)
(alexandria:define-constant +skip-list+ 20)
(alexandria:define-constant +ve-index+ 21)
(alexandria:define-constant +type-index+ 22)
(alexandria:define-constant +pcons+ 23)
(alexandria:define-constant +pqueue+ 24)
(alexandria:define-constant +mpointer+ 25)
(alexandria:define-constant +pcell+ 26)
(alexandria:define-constant +index-list+ 27)
(alexandria:define-constant +vev-index+ 28)
(alexandria:define-constant +bit-vector+ 29)
(alexandria:define-constant +bignum+ 30)
;; User-defined type identifiers for serializing. Start at 100
(alexandria:define-constant +uuid+ 100)
(alexandria:define-constant +timestamp+ 101)
(alexandria:define-constant +geometry+ 102) ;; spatial extension (see geometry.lisp)

;; GEOS availability flags.  These are inert in core graph-db (no FFI, no libgeos
;; dependency).  The OPTIONAL `graph-db/geos' add-on system flips them at load
;; time when it successfully binds libgeos_c; the spatial refine seam
;; (geometry-ops.lisp) consults them to decide between exact GEOS topology and
;; the dependency-free fallbacks.  Core stays libgeos-free.
(defvar *geos-available-p* nil
  "True once the graph-db/geos add-on has loaded libgeos_c successfully.")
(defvar *geos-version* nil
  "GEOS C library version as a list (major minor patch), or NIL if unloaded.")
(defvar *geos-makevalid-available-p* nil
  "True when the loaded GEOS is new enough (>= 3.8) for GEOSMakeValid_r.")

(defparameter *initial-extents* 10)
(defparameter *max-locks* 10000)

(defvar *graph-hash* nil)

;; Prolog specials
(defparameter *occurs-check* t)
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *var-counter* 0 "Counter for generating variable names.")
(defvar *functor* nil "The Prolog functor currently being compiled.")
(defvar *select-list* nil "Accumulator for prolog selects.")
(defvar *cont* nil "Continuation container for step-wise queries.")

#+sbcl
(defvar *prolog-global-functors* (make-hash-table :synchronized t))
#+sbcl
(defvar *user-functors* (make-hash-table :synchronized t :test 'eql))

#+lispworks
(defvar *prolog-global-functors* (make-hash-table :single-thread nil))
#+lispworks
(defvar *user-functors* (make-hash-table :single-thread nil :test 'eql))

#+ccl
(defvar *prolog-global-functors* (make-hash-table :shared t))
#+ccl
(defvar *user-functors* (make-hash-table :shared t :test 'eql))

#+ecl
(defvar *prolog-global-functors* (make-hash-table))
#+ecl
(defvar *user-functors* (make-hash-table :test 'eql))

(defparameter *prolog-trace* nil)
(alexandria:define-constant +unbound+ :unbound)
(alexandria:define-constant +no-bindings+ '((t . t)) :test 'equalp)
(alexandria:define-constant +fail+ nil)
