;;;; GUI API endpoints (GH #269).
;;;;
;;;; Every handler resolves its graph BY NAME at request time -- the GUI
;;;; holds no graph state.  Reads call engine internals directly
;;;; (lookup-vertex, outgoing-edges/incoming-edges, the type table);
;;;; nothing proxies through rest.lisp.  Error contract: handler-case ->
;;;; JSON {error, message}; 404 unknown graph/node/type, 409
;;;; dirty/conflict, 400 malformed, 500 with the condition's report.  No
;;;; backtraces to the browser; details go to log4cl.

(in-package #:graph-db.gui)

;;; ---------------------------------------------------------------------
;;; Request/response helpers
;;; ---------------------------------------------------------------------

(defun param (params name)
  "PARAMS value under NAME (a keyword route capture or a string query
parameter), as ningle delivers them."
  (cdr (assoc name params :test #'equalp)))

;;; Responses use cl-json's EXPLICIT encoder: the default guessing
;;; encoder renders an alist as a JSON object only when it stumbles on
;;; a dotted pair, so an alist whose values are all lists/NIL silently
;;; becomes an ARRAY.  Every nested object/array/boolean is tagged
;;; with %OBJ / %ARR / %BOOL below.

(defun %obj (alist) (cons :object alist))
(defun %arr (list) (cons :array list))
(defun %bool (x) (json:json-bool x))
(defun %maybe (x) (json:json-or-null x))

(defun %json-value (v)
  "An arbitrary slot value in explicit-encoder terms: atoms pass (NIL
as null), proper lists become arrays, anything else prints."
  (typecase v
    (null (%maybe nil))
    ((or string number keyword symbol) v)
    (cons (if (ignore-errors (list-length v))
              (%arr (mapcar #'%json-value v))
              (princ-to-string v)))
    (t (princ-to-string v))))

(defun %json-response (alist &key (status 200))
  (setf (lack.response:response-status ningle:*response*) status)
  (setf (lack.response:response-headers ningle:*response*)
        (list* :content-type "application/json; charset=utf-8"
               (lack.response:response-headers ningle:*response*)))
  (json:with-explicit-encoder
    (json:encode-json-to-string (%obj alist))))

(defun gui-error (status code message)
  "The uniform error body: {error, message} with an honest STATUS."
  (%json-response (list (cons :error code) (cons :message message))
                  :status status))

(defmacro def-gui-handler (name (params) &body body)
  "Define ningle handler NAME with the GUI error contract wrapped
around BODY.  A dirty store surfaces its report verbatim as a 409; any
other condition is a 500 whose report reaches the client and whose
details go to the log."
  `(defun ,name (,params)
     (declare (ignorable ,params))
     (handler-case (progn ,@body)
       (graph-db::store-not-closed-cleanly-error (c)
         (gui-error 409 "dirty-store" (princ-to-string c)))
       (error (c)
         (log:error "GUI ~A: ~A" ',name c)
         (gui-error 500 "internal-error" (princ-to-string c))))))

;;; ---------------------------------------------------------------------
;;; Roster: what stores does this system know?
;;; ---------------------------------------------------------------------

(defun %wire-name (name)
  "NAME (symbol or string) as its URL/JSON spelling."
  (if (symbolp name)
      (string-downcase (symbol-name name))
      (princ-to-string name)))

(defun %name-key (name)
  "Dedupe key for NAME across registry/journal/*graphs* spellings."
  (graph-db::%store-name-key name))

(defstruct roster-entry
  name          ; raw name, usable with open-graph
  location      ; namestring or NIL
  open-p)

(defun %journal-attach-locations ()
  "Keyword store name -> latest :ATTACH location from the clock
journal.  Reads via *SYSTEM-CLOCK* when bound, else an unowned clock
over *SYSTEM-DIRECTORY* (JOURNAL-RECORDS never truncates unowned)."
  (let ((table (make-hash-table :test 'equal))
        (clock (or graph-db::*system-clock*
                   (and graph-db::*system-directory*
                        (graph-db::%make-system-clock
                         :location graph-db::*system-directory*)))))
    (when clock
      (handler-case
          (dolist (r (graph-db::journal-records clock))
            (when (and (eq (getf r :kind) :attach) (getf r :location))
              (setf (gethash (%name-key (getf r :store)) table)
                    (getf r :location))))
        (error (c)
          (log:warn "GUI roster: unreadable system journal: ~A" c))))
    table))

(defun gui-roster ()
  "The store roster: registry names + journal :ATTACH locations,
deduped, overlaid with the open graphs in *GRAPHS*.  Falls back to
*GRAPHS* alone when no *SYSTEM-DIRECTORY* is bound.  Cheap by design:
name, location, open/closed -- no stats."
  (let ((entries (make-hash-table :test 'equal)))
    (flet ((entry (name)
             (or (gethash (%name-key name) entries)
                 (setf (gethash (%name-key name) entries)
                       (make-roster-entry :name name)))))
      (when graph-db::*system-directory*
        (handler-case
            (maphash (lambda (name id)
                       (declare (ignore id))
                       (entry name))
                     (graph-db::store-registry-names
                      (graph-db::ensure-store-registry)))
          (error (c)
            (log:warn "GUI roster: store registry unreadable: ~A" c)))
        (maphash (lambda (key location)
                   (setf (roster-entry-location (entry key))
                         (namestring location)))
                 (%journal-attach-locations)))
      (maphash (lambda (name graph)
                 (when (and (typep graph 'graph-db::graph)
                            (graph-db::graph-open-p graph))
                   (let ((e (entry name)))
                     (setf (roster-entry-name e) name
                           (roster-entry-open-p e) t
                           (roster-entry-location e)
                           (namestring (graph-db::location graph))))))
               graph-db::*graphs*))
    (let ((result '()))
      (maphash (lambda (key e) (declare (ignore key)) (push e result))
               entries)
      (sort result #'string< :key (lambda (e)
                                    (%wire-name (roster-entry-name e)))))))

(defun find-roster-entry (wire-name)
  (find wire-name (gui-roster)
        :key (lambda (e) (%wire-name (roster-entry-name e)))
        :test #'string-equal))

(defun find-open-graph (wire-name)
  "The OPEN graph whose name spells WIRE-NAME, or NIL."
  (maphash (lambda (name graph)
             (when (and (typep graph 'graph-db::graph)
                        (graph-db::graph-open-p graph)
                        (string-equal (%wire-name name) wire-name))
               (return-from find-open-graph graph)))
           graph-db::*graphs*)
  nil)

(defun %roster-json (entry)
  (%obj (list (cons :name (%wire-name (roster-entry-name entry)))
              (cons :location (%maybe (roster-entry-location entry)))
              (cons :open (%bool (roster-entry-open-p entry))))))

(def-gui-handler api-graphs (params)
  (%json-response
   (list (cons :graphs (%arr (mapcar #'%roster-json (gui-roster)))))))

;;; ---------------------------------------------------------------------
;;; Management verbs: open / close.  Both serialize through the ONE
;;; exclusive side of *GUI-RW-LOCK*, so a race surfaces as a clean 409,
;;; never as two concurrent opens.
;;; ---------------------------------------------------------------------

;; GUI-local reader/writer coordination (GH #269): CLOSE-GRAPH unmaps
;; the store's mmaps, so a read handler racing an in-flight close would
;; fault or read freed bytes.  Every graph-read handler holds the
;; shared side across resolve+read; open/close hold the exclusive side.
;; CLOSE-GRAPH itself is untouched.
(defvar *gui-rw-lock* (graph-db::make-rw-lock))

(def-gui-handler api-open-graph (params)
  (let ((wire (param params :name)))
    (graph-db::with-write-lock (*gui-rw-lock*)
      (let ((open (find-open-graph wire)))
        (if open
            (%json-response (list (cons :name wire) (cons :open t)
                                  (cons :status "already-open")))
            (let ((entry (find-roster-entry wire)))
              (cond
                ((null entry)
                 (gui-error 404 "unknown-graph"
                            (format nil "Unknown graph ~A" wire)))
                ((null (roster-entry-location entry))
                 (gui-error 404 "no-recorded-location"
                            (format nil "No recorded location for ~
graph ~A; the GUI opens stores only at their roster location" wire)))
                ((not (uiop:directory-exists-p
                       (roster-entry-location entry)))
                 (gui-error 404 "location-missing"
                            (format nil "Recorded location ~A for ~
graph ~A no longer exists"
                                    (roster-entry-location entry)
                                    wire)))
                (t
                 ;; Strictly at the recorded location -- no free-form
                 ;; paths.  A dirty store signals STORE-NOT-CLOSED-
                 ;; CLEANLY-ERROR, which DEF-GUI-HANDLER maps to 409.
                 (graph-db:open-graph (roster-entry-name entry)
                                      (roster-entry-location entry))
                 (%json-response
                  (list (cons :name wire) (cons :open t)
                        (cons :status "opened")))))))))))

(def-gui-handler api-close-graph (params)
  (let ((wire (param params :name)))
    (graph-db::with-write-lock (*gui-rw-lock*)
      (let ((graph (find-open-graph wire)))
        (if graph
            (progn
              (graph-db:close-graph graph)
              (%json-response (list (cons :name wire)
                                    (cons :open (%bool nil))
                                    (cons :status "closed"))))
            (gui-error 409 "not-open"
                       (format nil "Graph ~A is not open" wire)))))))

;;; ---------------------------------------------------------------------
;;; Read endpoints.  Each resolves its graph per request; a closed or
;;; unknown graph is a 404.
;;; ---------------------------------------------------------------------

(defmacro with-gui-graph ((var params) &body body)
  "Bind VAR to the open graph named by PARAMS' :name, or answer 404.
Holds *GUI-RW-LOCK*'s shared side across resolve+read so an in-flight
close cannot unmap the store mid-read (GH #269)."
  `(graph-db::with-read-lock (*gui-rw-lock*)
     (let ((,var (find-open-graph (param ,params :name))))
       (if ,var
           (progn ,@body)
           (gui-error 404 "unknown-graph"
                      (format nil "Graph ~A is not open"
                              (param ,params :name)))))))

(defun %schema-type-names (graph parent)
  "Node-type names (class symbols) registered for PARENT (:vertex or
:edge) in GRAPH's schema, sorted by name."
  (let ((names '()))
    (maphash (lambda (key meta)
               (when (numberp key)
                 (push (graph-db::node-type-name meta) names)))
             (gethash parent (graph-db::schema-type-table
                             (graph-db::schema graph))))
    (sort names #'string< :key #'symbol-name)))

(defun %camel (symbol)
  (json:lisp-to-camel-case (symbol-name symbol)))

(defun %per-type-counts (graph)
  "(values vertex-alist edge-alist) of (class-name . count), one sweep
of each table."
  (let ((v (make-hash-table :test 'eq))
        (e (make-hash-table :test 'eq)))
    (graph-db:map-vertices
     (lambda (x) (incf (gethash (class-name (class-of x)) v 0))) graph)
    (graph-db:map-edges
     (lambda (x) (incf (gethash (class-name (class-of x)) e 0))) graph)
    (flet ((as-alist (table)
             (let ((acc '()))
               (maphash (lambda (k n) (push (cons k n) acc)) table)
               (sort acc #'string< :key (lambda (c)
                                          (symbol-name (car c)))))))
      (values (as-alist v) (as-alist e)))))

(defun %on-disk-size (graph)
  "Total bytes of the files under GRAPH's directory; unreadable files
count zero."
  (let ((total 0))
    (fad:walk-directory
     (graph-db::location graph)
     (lambda (file)
       (incf total
             (or (ignore-errors
                  (with-open-file (s file :element-type
                                       '(unsigned-byte 8))
                    (file-length s)))
                 0)))
     :directories nil)
    total))

(defun %schema-summary (graph parent)
  "[{name, slots:[...]}] for PARENT's node types."
  (mapcar (lambda (type-name)
            (let ((meta (graph-db::lookup-node-type-by-name
                         (intern (symbol-name type-name) :keyword)
                         parent :graph graph)))
              (%obj
               (list (cons :name (%camel type-name))
                     (cons :slots
                           (%arr
                            (mapcar (lambda (slot-def)
                                      (%camel (if (consp slot-def)
                                                  (first slot-def)
                                                  slot-def)))
                                    (and meta
                                         (graph-db::node-type-slots
                                          meta)))))))))
          (%schema-type-names graph parent)))

(defun %index-inventory (graph)
  "DEF-INDEX specs + spatial indexes registered for GRAPH."
  (append
   (mapcar (lambda (spec)
             (%obj
              (list (cons :kind "ordered")
                    (cons :owner
                          (%camel
                           (graph-db::index-spec-owner-name spec)))
                    (cons :slots
                          (%arr
                           (mapcar #'%camel
                                   (graph-db::index-spec-slot-names
                                    spec)))))))
           (graph-db::%registered-index-specs graph))
   (let ((acc '()))
     (maphash (lambda (key idx)
                (declare (ignore idx))
                (push (%obj
                       (list (cons :kind "spatial")
                             (cons :owner (%camel (car key)))
                             (cons :slots
                                   (%arr (list (%camel (cdr key)))))))
                      acc))
              (graph-db::spatial-indexes graph))
     (nreverse acc))))

(def-gui-handler api-graph-stats (params)
  (with-gui-graph (graph params)
    (multiple-value-bind (v-counts e-counts) (%per-type-counts graph)
      (%json-response
       (list (cons :name (param params :name))
             (cons :vertex-count
                   (graph-db::read-lhash-count
                    (graph-db::vertex-table graph)))
             (cons :edge-count
                   (graph-db::read-lhash-count
                    (graph-db::edge-table graph)))
             (cons :vertex-counts-by-type (%obj v-counts))
             (cons :edge-counts-by-type (%obj e-counts))
             (cons :views
                   (%arr
                    (mapcar (lambda (pair)
                              (%obj
                               (list (cons :class (%camel (car pair)))
                                     (cons :name (%camel (cdr pair))))))
                            (graph-db::list-views graph))))
             (cons :indexes (%arr (%index-inventory graph)))
             (cons :on-disk-bytes (%on-disk-size graph))
             (cons :schema
                   (%obj
                    (list (cons :vertex-types
                                (%arr (%schema-summary graph :vertex)))
                          (cons :edge-types
                                (%arr (%schema-summary
                                       graph :edge)))))))))))

(def-gui-handler api-graph-types (params)
  (with-gui-graph (graph params)
    (%json-response
     (list (cons :vertex-types
                 (%arr (mapcar #'%camel
                               (%schema-type-names graph :vertex))))
           (cons :edge-types
                 (%arr (mapcar #'%camel
                               (%schema-type-names graph :edge))))))))

;;; ---------------------------------------------------------------------
;;; Node sample, inspection, neighborhood
;;; ---------------------------------------------------------------------

(defparameter *default-node-limit* 50)
(defparameter *default-neighborhood-limit* 100)

(defun %parse-limit (params default)
  "The \"limit\" query parameter as a positive integer, else DEFAULT."
  (let* ((raw (param params "limit"))
         (n (and raw (ignore-errors (parse-integer raw)))))
    (if (and n (plusp n)) n default)))

(defun %resolve-vertex-type (graph wire-type)
  "WIRE-TYPE (camelCase) as a vertex class symbol of GRAPH, or NIL."
  (let ((meta (handler-case
                  (graph-db::lookup-node-type-by-name
                   (intern (json:camel-case-to-lisp wire-type) :keyword)
                   :vertex :graph graph)
                (error () nil))))
    (and meta (graph-db::node-type-name meta))))

(defun %vertex-brief (v)
  (list (cons :id (graph-db:string-id v))
        (cons :type (%camel (class-name (class-of v))))))

(def-gui-handler api-graph-nodes (params)
  (with-gui-graph (graph params)
    (let ((wire-type (param params "type"))
          (limit (%parse-limit params *default-node-limit*)))
      (if (null wire-type)
          (gui-error 400 "missing-type"
                     "The \"type\" query parameter is required")
          (let ((type (%resolve-vertex-type graph wire-type)))
            (if (null type)
                (gui-error 404 "unknown-type"
                           (format nil "Unknown vertex type ~A"
                                   wire-type))
                (let ((nodes '()) (count 0) (truncated nil))
                  (block sample
                    (graph-db:map-vertices
                     (lambda (v)
                       (when (>= count limit)
                         (setq truncated t)
                         (return-from sample))
                       (push (%vertex-brief v) nodes)
                       (incf count))
                     graph :vertex-type type))
                  (%json-response
                   (list (cons :type wire-type)
                         (cons :nodes
                               (%arr (mapcar #'%obj
                                             (nreverse nodes))))
                         (cons :truncated (%bool truncated)))))))))))

(defun %parse-node-id (string)
  "STRING as a 16-byte id array, or NIL when malformed.  Wire ids are
the engine's 32-hex-character STRING-ID form."
  (and (stringp string)
       (= 32 (length string))
       (every (lambda (ch) (digit-char-p ch 16)) string)
       (graph-db::read-id-array-from-string string)))

(defun %node-slots-alist (node)
  "NODE's data slots as an alist keyed by camelCase slot names.  The
underlying node data is an ALIST of (:SLOT . value) conses -- this is
the JSON-object rendering of it."
  (mapcar (lambda (slot-name)
            (cons (%camel slot-name)
                  (%json-value (slot-value node slot-name))))
          (graph-db::data-slots (class-of node))))

(def-gui-handler api-graph-node (params)
  (with-gui-graph (graph params)
    (let ((id (%parse-node-id (param params :id))))
      (if (null id)
          (gui-error 400 "malformed-id"
                     (format nil "Malformed node id ~A"
                             (param params :id)))
          (let ((v (graph-db:lookup-vertex id :graph graph)))
            (cond
              (v (%json-response
                  (list (cons :id (graph-db:string-id v))
                        (cons :type (%camel (class-name (class-of v))))
                        (cons :slots (%obj (%node-slots-alist v)))
                        (cons :in-edge-count
                              (length (graph-db:incoming-edges
                                       v :graph graph)))
                        (cons :out-edge-count
                              (length (graph-db:outgoing-edges
                                       v :graph graph))))))
              (t
               (let ((e (graph-db:lookup-edge id :graph graph)))
                 (if e
                     (%json-response
                      (list (cons :id (graph-db:string-id e))
                            (cons :type (%camel
                                         (class-name (class-of e))))
                            (cons :from (graph-db:string-id
                                         (graph-db::from e)))
                            (cons :to (graph-db:string-id
                                       (graph-db::to e)))
                            (cons :slots
                                  (%obj (%node-slots-alist e)))))
                     (gui-error 404 "unknown-node"
                                (format nil "Unknown node ~A"
                                        (param params :id))))))))))))

(def-gui-handler api-graph-neighborhood (params)
  (with-gui-graph (graph params)
    (let ((id (%parse-node-id (param params :id)))
          (limit (%parse-limit params *default-neighborhood-limit*)))
      (if (null id)
          (gui-error 400 "malformed-id"
                     (format nil "Malformed node id ~A"
                             (param params :id)))
          ;; One consistent snapshot for the whole batch: the node, its
          ;; edges in both directions, and every neighbor endpoint.
          (graph-db:with-read-snapshot (graph)
            (let ((center (graph-db:lookup-vertex id :graph graph)))
              (if (null center)
                  (gui-error 404 "unknown-node"
                             (format nil "Unknown node ~A"
                                     (param params :id)))
                  (let* ((out (graph-db:outgoing-edges
                               center :graph graph))
                         (in (graph-db:incoming-edges
                              center :graph graph))
                         (all (append out in))
                         (truncated (> (length all) limit))
                         (edges (if truncated
                                    (subseq all 0 limit)
                                    all))
                         (nodes (make-hash-table :test 'equal)))
                    (setf (gethash (graph-db:string-id center) nodes)
                          (%vertex-brief center))
                    (dolist (e edges)
                      (dolist (end (list (graph-db::from e)
                                         (graph-db::to e)))
                        (let ((sid (graph-db:string-id end)))
                          (unless (gethash sid nodes)
                            (let ((v (graph-db:lookup-vertex
                                      end :graph graph)))
                              (when v
                                (setf (gethash sid nodes)
                                      (%vertex-brief v))))))))
                    (let ((node-list '()))
                      (maphash (lambda (k v)
                                 (declare (ignore k))
                                 (push v node-list))
                               nodes)
                      (%json-response
                       (list
                        (cons :nodes
                              (%arr
                               (mapcar
                                #'%obj
                                (sort node-list #'string<
                                      :key (lambda (n)
                                             (cdr (assoc :id n)))))))
                        (cons :edges
                              (%arr
                               (mapcar
                                (lambda (e)
                                  (%obj
                                   (list
                                    (cons :id (graph-db:string-id e))
                                    (cons :type
                                          (%camel
                                           (class-name (class-of e))))
                                    (cons :from
                                          (graph-db:string-id
                                           (graph-db::from e)))
                                    (cons :to
                                          (graph-db:string-id
                                           (graph-db::to e))))))
                                edges)))
                        (cons :truncated (%bool truncated)))))))))))))
