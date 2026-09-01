;;;; GUI API endpoints (GH #269).
;;;;
;;;; Every handler resolves its graph BY NAME at request time -- the GUI
;;;; holds no graph state.  Reads call engine internals directly
;;;; (lookup-vertex, outgoing-edges/incoming-edges, the type table);
;;;; nothing proxies through rest.lisp.  Error contract: handler-case ->
;;;; JSON {error, message}; 404 unknown graph/node/type, 409
;;;; dirty/conflict, 400 malformed, 500 with the condition's report.  No
;;;; backtraces to the browser; details go to log4cl.
;;;;
;;;; Naming on the wire (GH #277): JSON KEYS are protocol and stay
;;;; camelCase (cl-json's keyword encoding).  VALUES naming schema
;;;; entities -- types, slots, views, index owners -- ship as the engine
;;;; spells them, lowercase kebab, so what the UI shows is what a query
;;;; types.  See %WIRE-SYMBOL and %NODE-SLOTS-ALIST.

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

(defun %alist-p (v)
  "True when V is a proper list of (KEY . VALUE) conses whose keys are
symbols or strings -- the shape a decoded JSON object is stored as."
  (and (consp v)
       (ignore-errors (list-length v))
       (every (lambda (e)
                (and (consp e) (or (symbolp (car e)) (stringp (car e)))))
              v)))

(defun %json-value (v)
  "An arbitrary slot value in explicit-encoder terms: atoms pass (NIL
as null, T as true), an alist becomes an object keyed by its keys'
wire spelling, other proper lists become arrays, anything else prints.
A stored JSON object is an alist, and before GH #282 the improper-cons
branch printed each (key . value) pair as Lisp text."
  (typecase v
    (null (%maybe nil))
    ((eql t) (%bool t))
    ((or string number keyword symbol) v)
    (cons (cond ((%alist-p v)
                 (%obj (mapcar (lambda (e)
                                 (cons (if (stringp (car e))
                                           (car e)
                                           (%wire-symbol (car e)))
                                       (%json-value (cdr e))))
                               v)))
                ((ignore-errors (list-length v))
                 (%arr (mapcar #'%json-value v)))
                (t (princ-to-string v))))
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

(defun %wire-symbol (symbol)
  "SYMBOL as its wire spelling: the engine's own name, downcased kebab
\(GUI-PERSON -> \"gui-person\").  Schema names are NOT camelized -- a
query author types them as the engine spells them (GH #277)."
  (string-downcase (symbol-name symbol)))

(defun %per-type-counts (graph)
  "(values vertex-alist edge-alist) of (wire-type-name . count), one
sweep of each table."
  (let ((v (make-hash-table :test 'eq))
        (e (make-hash-table :test 'eq)))
    (graph-db:map-vertices
     (lambda (x) (incf (gethash (class-name (class-of x)) v 0))) graph)
    (graph-db:map-edges
     (lambda (x) (incf (gethash (class-name (class-of x)) e 0))) graph)
    (flet ((as-alist (table)
             ;; Keys here are type names, not protocol -- string keys
             ;; so cl-json emits them verbatim (GH #277).
             (let ((acc '()))
               (maphash (lambda (k n)
                          (push (cons (%wire-symbol k) n) acc))
                        table)
               (sort acc #'string< :key #'car))))
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
               (list (cons :name (%wire-symbol type-name))
                     (cons :slots
                           (%arr
                            (mapcar (lambda (slot-def)
                                      (%wire-symbol
                                       (if (consp slot-def)
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
                          (%wire-symbol
                           (graph-db::index-spec-owner-name spec)))
                    (cons :slots
                          (%arr
                           (mapcar #'%wire-symbol
                                   (graph-db::index-spec-slot-names
                                    spec)))))))
           (graph-db::%registered-index-specs graph))
   (let ((acc '()))
     (maphash (lambda (key idx)
                (declare (ignore idx))
                (push (%obj
                       (list (cons :kind "spatial")
                             (cons :owner (%wire-symbol (car key)))
                             (cons :slots
                                   (%arr
                                    (list (%wire-symbol (cdr key)))))))
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
                               (list (cons :class
                                           (%wire-symbol (car pair)))
                                     (cons :name
                                           (%wire-symbol (cdr pair))))))
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
                 (%arr (mapcar #'%wire-symbol
                               (%schema-type-names graph :vertex))))
           (cons :edge-types
                 (%arr (mapcar #'%wire-symbol
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
  "WIRE-TYPE (the engine's own kebab spelling) as a vertex class symbol
of GRAPH, or NIL.  Interned directly -- the old camel-case round trip
was not bijective (GH #277)."
  (let ((meta (handler-case
                  (graph-db::lookup-node-type-by-name
                   (intern (string-upcase wire-type) :keyword)
                   :vertex :graph graph)
                (error () nil))))
    (and meta (graph-db::node-type-name meta))))

(defun %vertex-brief (v)
  (list (cons :id (graph-db:string-id v))
        (cons :type (%wire-symbol (class-name (class-of v))))))

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
                ;; Echo the canonical spelling, not the raw parameter:
                ;; the response's type must be re-usable as ?type=.
                (let ((nodes '()) (count 0) (truncated nil)
                      (canonical (%wire-symbol type)))
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
                   (list (cons :type canonical)
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
  "NODE's data slots as an alist keyed by the engine's slot names.  The
underlying node data is an ALIST of (:SLOT . value) conses -- this is
the JSON-object rendering of it.

The one place the keys-stay-camelCase rule is overridden: these keys
are domain identifiers a query author types, not protocol (GH #277)."
  (mapcar (lambda (slot-name)
            (cons (%wire-symbol slot-name)
                  (%json-value (slot-value node slot-name))))
          (graph-db::data-slots (class-of node))))

(def-gui-handler api-graph-node (params)
  (with-gui-graph (graph params)
    (let ((id (%parse-node-id (param params :id))))
      (if (null id)
          (gui-error 400 "malformed-id"
                     (format nil "Malformed node id ~A"
                             (param params :id)))
          ;; The per-graph node cache is keyed by id alone, so
          ;; LOOKUP-VERTEX can return a cached EDGE for an edge id.
          ;; Branch on the object's class, never on which lookup
          ;; found it (GH #271).
          (let ((v (graph-db:lookup-vertex id :graph graph)))
            (cond
              ((typep v 'graph-db::vertex)
               (%json-response
                (list (cons :id (graph-db:string-id v))
                      (cons :type
                            (%wire-symbol (class-name (class-of v))))
                      (cons :slots (%obj (%node-slots-alist v)))
                      (cons :in-edge-count
                            (length (graph-db:incoming-edges
                                     v :graph graph)))
                      (cons :out-edge-count
                            (length (graph-db:outgoing-edges
                                     v :graph graph))))))
              (t
               (let ((e (if (typep v 'graph-db::edge)
                            v
                            (graph-db:lookup-edge id :graph graph))))
                 (if (typep e 'graph-db::edge)
                     (%json-response
                      (list (cons :id (graph-db:string-id e))
                            (cons :type
                                  (%wire-symbol
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

;;; ---------------------------------------------------------------------
;;; Query workbench (GH #278).  The body is the same structured DSL
;;; rest.lisp's /graph/:g/query route accepts, compiled and run by the
;;; SHARED implementation in query-dsl.lisp -- read-only, snapshot-
;;; isolated and capped, exactly as REST gets it.  Nothing here parses
;;; or reads user text into symbols beyond what that compiler does.
;;; ---------------------------------------------------------------------

(defun %request-json-body (&key (intern-dsl-keys t))
  "The request body decoded as JSON, or :MALFORMED.  The GUI's own
seam onto the DSL -- it does not call through rest.lisp.  With
INTERN-DSL-KEYS (the default) only the DSL's own keys become keywords
(DECODE-DSL-JSON, GH #284); NIL leaves every key a string, for an
endpoint that reads its fields by string (the Prolog editor).

The :MALFORMED arm covers only bodies sent under a content type lack
does NOT pre-parse.  Under application/json lack parses the body when
it builds the request, before ningle dispatches, so a JSON syntax
error is ITS plain-text 400 and this handler never runs (GH #278)."
  ;; Neither decoder interns a client-supplied key; the default decoder
  ;; interned every object key a client sent (GH #284).
  (handler-case
      (let* ((raw (lack/request:request-content ningle:*request*))
             (string (if (stringp raw)
                         raw
                         (flexi-streams:octets-to-string
                          raw :external-format :utf-8))))
        (if intern-dsl-keys
            (graph-db:decode-dsl-json string)
            (graph-db:decode-json-string-keys string)))
    (error () :malformed)))

(defun %clamp-row-cap (n)
  "N clamped by *QUERY-DEFAULT-LIMIT*.  Mirrors RUN-QUERY-GOALS' own
rule so a response can report the bound it was actually run under."
  (if (and (integerp n) (plusp n))
      (min n graph-db::*query-default-limit*)
      graph-db::*query-default-limit*))

(defun %query-row-cap (dsl)
  "The row cap RUN-PATTERN-QUERY will apply to DSL: its \"limit\"
clamped by *QUERY-DEFAULT-LIMIT*."
  (%clamp-row-cap (cdr (assoc :limit dsl))))

(defun %query-probe-limit (cap)
  "Rows to ask the runner for when the answer must show CAP of them.
One past the cap tells a truncated result from an exactly-full page.
At *QUERY-DEFAULT-LIMIT* there is no room to ask for one more -- the
runner clamps there -- so the probe is the cap itself and an
exactly-full page reads as truncated (GH #278)."
  (if (< cap graph-db::*query-default-limit*) (1+ cap) cap))

(defun %query-dsl-for-gui (dsl limit)
  "DSL with its \"format\" field dropped and \"limit\" forced to LIMIT.
The GUI answers one JSON object; the DSL's REST-only NDJSON streaming
arm would both break that envelope and push a second content-type
header (GH #278)."
  (cons (cons :limit limit)
        (remove-if (lambda (cell) (member (car cell) '(:format :limit)))
                   dsl)))

(defun %decode-query-rows (json-string)
  "RUN-PATTERN-QUERY's JSON array decoded back to alists with the key
STRINGS intact -- cl-json's default readers fold them to keywords and
lose the spelling the DSL chose for each result variable."
  (let ((json:*json-identifier-name-to-lisp* #'identity)
        (json:*identifier-name-to-key* #'identity))
    (json:decode-json-from-string json-string)))

(defun %query-row-json (row)
  (%obj (mapcar (lambda (cell)
                  (cons (car cell) (%json-value (cdr cell))))
                row)))

(defun %query-envelope (json-string cap probe)
  "The workbench result envelope -- {columns, rows, rowCount, limit,
truncated} -- for RUN-QUERY-GOALS' JSON-STRING, run under PROBE rows
and answered under CAP.  Shared by the builder endpoint and the
free-text Prolog one so one results table renders both (GH #278, #279)."
  (let* ((rows (%decode-query-rows json-string))
         (n (length rows))
         ;; The probe row proves there was more; it is never shown.
         ;; Without room for it, >= is the best the runner's own clamp
         ;; allows.
         (truncated (if (> probe cap) (> n cap) (>= n cap)))
         (shown (if (> n cap) (subseq rows 0 cap) rows)))
    (%json-response
     (list
      ;; QUERY-ROW->ALIST builds every row in select order, so the
      ;; first row names the columns.
      (cons :columns (%arr (mapcar #'car (first shown))))
      (cons :rows (%arr (mapcar #'%query-row-json shown)))
      (cons :row-count (length shown))
      (cons :limit cap)
      (cons :truncated (%bool truncated))))))

;; The whole query runs under WITH-GUI-GRAPH's read side, so a slow one
;; delays open/close for up to the DSL's *QUERY-DEFAULT-TIMEOUT*.  That
;; is deliberate: resolving under the lock and running outside it would
;; restore exactly the hazard the lock exists for -- CLOSE-GRAPH unmaps
;; the store's mmaps under a live reader (GH #269).
(def-gui-handler api-graph-query (params)
  (with-gui-graph (graph params)
    (let ((dsl (%request-json-body)))
      (cond
        ((eq dsl :malformed)
         (gui-error 400 "malformed-json"
                    "Request body is not valid JSON"))
        ;; A decoded JSON object is an alist; anything else (an array,
        ;; a bare scalar) would make ASSOC signal deep in the compiler.
        ((not (and (listp dsl) (every #'consp dsl)))
         (gui-error 400 "malformed-query"
                    "Request body must be a JSON query object"))
        (t
         (handler-case
             (let* ((cap (%query-row-cap dsl))
                    (probe (%query-probe-limit cap)))
               (%query-envelope
                (graph-db::run-pattern-query
                 (%query-dsl-for-gui dsl probe) graph)
                cap probe))
           ;; Same mapping REST uses for the same conditions: one DSL,
           ;; one meaning per failure.  A resource-budget breach is 400,
           ;; not 413 -- 413 is about the size of the request entity,
           ;; and the request here is tiny; what blew the budget is the
           ;; query the client wrote.
           (graph-db:query-param-error (c)
             (gui-error 400 "bad-query"
                        (graph-db::query-param-error-reason c)))
           (graph-db:prolog-resource-error (c)
             (declare (ignore c))
             (gui-error 400 "query-too-expensive"
                        "Query exceeded its resource limits"))
           (graph-db:prolog-permission-error (c)
             (declare (ignore c))
             (gui-error 403 "forbidden-operation"
                        "Query attempted a forbidden operation"))))))))

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
              ;; Same id-keyed-cache gotcha as API-GRAPH-NODE: an edge
              ;; id can come back from LOOKUP-VERTEX.  A neighborhood
              ;; is defined on vertices only, so an edge center is a
              ;; 404 by decision (GH #271).
              (if (not (typep center 'graph-db::vertex))
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
                                          (%wire-symbol
                                           (class-name (class-of e))))
                                    (cons :from
                                          (graph-db:string-id
                                           (graph-db::from e)))
                                    (cons :to
                                          (graph-db:string-id
                                           (graph-db::to e))))))
                                edges)))
                        (cons :truncated (%bool truncated)))))))))))))
