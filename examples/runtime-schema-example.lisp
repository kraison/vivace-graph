;;;; Runtime schema: the intended developer experience (GH #172).
;;;;
;;;; This file is DOCUMENTATION, not loadable code.  It is the example
;;;; application written before implementation as the design artifact
;;;; (Kevin-approved 2026-08-24) and reconciled here, after #172
;;;; shipped, against the actual signatures and output shapes in
;;;; `runtime-schema.lisp` and `schema-tools.lisp` -- every form below
;;;; is checked against the source, and the three open points (A/B/C)
;;;; the original draft flagged are resolved below, not left hanging.
;;;; Read it top to bottom as three Lisp sessions separated by process
;;;; restarts; Kevin reads this file, so the design commentary and the
;;;; session structure are kept, not stripped to a bare reference.
;;;;
;;;; The two problems this design must not dodge (Kevin, 2026-08-24):
;;;;
;;;;   1. OPACITY.  A runtime-created class has no source file.  The
;;;;      developer who sits down tomorrow cannot grep for it, cannot
;;;;      read a def-vertex form, cannot see the schema as Lisp.  VG's
;;;;      founding bargain -- "write mostly ordinary CLOS and it
;;;;      persists" -- cuts both ways: the schema has always BEEN the
;;;;      source.  #172 breaks that unless the schema can be turned
;;;;      back into source on demand.
;;;;
;;;;   2. LOAD ORDER.  Methods need their classes at compile time.  A
;;;;      class that exists only as persisted metadata does not exist
;;;;      when compile-file reaches (defmethod ... ((x sensor:reading))
;;;;      in the next Lisp invocation.  This is the reason the feature
;;;;      has waited twenty years.
;;;;
;;;; The design answers both with the same move: METADATA IS THE
;;;; DURABLE TRUTH, AND SOURCE IS A VIEW OF IT -- derivable at any
;;;; moment, in two directions:
;;;;
;;;;   - MATERIALIZE: metadata -> live classes, at load time, before
;;;;     any method file compiles.  No eval, no load of runtime-written
;;;;     code; the invariant ("restart never evaluates data") holds
;;;;     because building a class from a slots list is a mechanical MOP
;;;;     operation on data.
;;;;
;;;;   - EXPORT: metadata -> def-vertex/def-edge text, for the human.
;;;;     The developer can read it, diff it, and -- when a runtime type
;;;;     has earned permanence -- check it in, at which point it stops
;;;;     being a runtime type at all.  Loading exported source over an
;;;;     existing runtime type is idempotent: same names, same registry
;;;;     ids, same slots.  Promotion to source is a one-way door the
;;;;     DEVELOPER walks through deliberately; the ENGINE never
;;;;     persists source or loads it.

;;;; ===================================================================
;;;; SESSION 1 -- a running system grows a type at runtime
;;;; ===================================================================

(in-package :cl-user)

;;; The application's static schema, exactly as today.  Nothing about
;;; def-vertex changes; compiled files with methods on TICKET work as
;;; they always have.  (:SUPPORT is TICKET's default store per #167.)

(defpackage #:helpdesk (:use #:cl #:graph-db))
(in-package #:helpdesk)

(def-vertex ticket () ((title :type string)
                       (severity :type integer))
  :support)

;;; ... the app runs; an operator (or the app itself, e.g. an ETL that
;;; discovers a new record shape) needs a new node type WITHOUT a
;;; deploy.  Two new functions -- functions, not macros, because the
;;; type name and shape arrive as data at runtime:

;; A runtime namespace is a package plus, later, manifest rows.  This
;; allocates NO files and creates NO store (spec: schema creation must
;; be cheap, store creation must not be).  Idempotent.
(graph-db:ensure-namespace "TELEMETRY" :nicknames '("TLM"))

;; The runtime twin of def-vertex.  NAME may be a symbol or a
;; "PACKAGE:NAME" string (the caller may only have strings -- this is
;; runtime); the package must already exist (ENSURE-NAMESPACE first --
;; a missing package is an error, not an implicit creation).  Slot
;; specs are the same data def-vertex takes.  :DEFAULT-STORE mirrors
;; def-vertex's trailing store argument, but as a keyword, and it
;; defaults to NIL: a runtime type need not commit to placement at
;; creation, at the cost that its generated constructor then requires
;; an explicit :GRAPH.
;;
;; Returns the finalized CLOS class.  Under the hood this drives the
;; SAME installation path def-vertex's expansion uses
;; (%INSTALL-NODE-TYPE) -- one meta, one registry id keyed on the
;; interned symbol, instantiated into the default store if open -- via
;; the one new primitive the spec names: class-from-metadata (a
;; DEFCLASS built through the MOP, with accessors and MAKE-/LOOKUP-/-P
;; helpers interned in the symbol's own package).
(graph-db:create-vertex-type "TELEMETRY:READING"
  '((sensor-id :type string)
    (value     :type double-float)
    (taken-at  :type integer))
  :default-store :support)

;; Behaviour cannot be data (a closure does not serialize).  A runtime
;; type that wants a constraint NAMES a function registered by code
;; that shipped in the image, via a slot option: :CHECK NAME (not
;; :VALUE-CONSTRAINT -- that is a different, existing mechanism,
;; DEF-VALUE-CONSTRAINT; :CHECK is the new, minimal, function-by-name
;; seam #172 adds beside it).  Registration is code-side and explicit;
;; the metadata stores only the NAME.  Presence is verified twice: at
;; CREATE-VERTEX-TYPE time (this call would itself signal
;; SCHEMA-FUNCTION-UNRESOLVED if the name were not yet registered) and
;; again at MATERIALIZE-SCHEMA time in a later session; resolution
;; itself happens at each check, so a re-registration takes effect
;; immediately -- restart still never evaluates data.
(graph-db:register-schema-function 'telemetry-plausible-p
  (lambda (v) (< -1d6 v 1d6)))

(graph-db:create-vertex-type "TELEMETRY:CALIBRATION"
  '((value :type double-float
           :check telemetry-plausible-p))  ; a NAME, not a #'
  :default-store :support)

;;; From here the runtime type is indistinguishable from a static one.
;;; The generated constructor exists, placement follows #167's rules,
;;; transactions and views and indexes see an ordinary node-class:

(with-transaction ((transaction-manager (lookup-graph :support)))
  (funcall (intern "MAKE-READING" :telemetry)
           :sensor-id "s-17" :value 22.5d0 :taken-at 3600))

;;; ------------------------------------------------------------------
;;; The opacity answer, part 1: the schema is always readable as text.
;;; ------------------------------------------------------------------
;;;
;;; DESCRIBE-SCHEMA is the REPL/Emacs tool.  Plain text on a stream, so
;;; it works today in SLIME (C-c C-d equivalent: just call it), and a
;;; ten-line schema.el could later put it in a dedicated buffer -- out
;;; of scope for #172 itself (R7), but the text dump is SLIME-usable
;;; as-is.  Every type row says WHERE IT CAME FROM -- source or
;;; runtime -- because that provenance is precisely what the developer
;;; cannot currently see:

(graph-db:describe-schema :namespace :telemetry)
;; =>
;; Namespace TELEMETRY
;;   CALIBRATION (vertex) default-store :support   [runtime 2026-08-24]
;;     VALUE  DOUBLE-FLOAT  :check TELEMETRY-PLAUSIBLE-P
;;   READING (vertex) default-store :support   [runtime 2026-08-24]
;;     SENSOR-ID  STRING
;;     VALUE  DOUBLE-FLOAT
;;     TAKEN-AT  INTEGER
;;
;; (types within a namespace print sorted by name, hence CALIBRATION
;; before READING here.)
;;
;; (describe-schema)                 -- everything, grouped by namespace
;; (describe-schema :store :support) -- what one store's schema holds
;; Static types print the same way tagged [source], so ONE tool shows
;; the whole schema, not only the runtime part.  :SINCE (a universal
;; time, or a "YYYY-MM-DD" string) filters by each row's recorded
;; time, so the dump doubles as a change log -- see session 3.

;;; ------------------------------------------------------------------
;;; The opacity answer, part 2: the schema is exportable as SOURCE.
;;; ------------------------------------------------------------------

(graph-db:export-schema-source "src/generated-schema.lisp"
                               :namespace :telemetry)
;; Writes, verbatim def-vertex syntax, with a provenance header:
;;
;;   ;;; Generated by graph-db:export-schema-source 2026-08-24.
;;   ;;; Source of truth remains the persisted metadata until this
;;   ;;; file is loaded as part of the system; loading it is
;;   ;;; idempotent (same names -> same registry ids).
;;
;;   (defpackage #:telemetry (:use #:cl #:graph-db)
;;     (:nicknames #:tlm)
;;     (:export
;;      #:calibration
;;      #:reading
;;      ))
;;   (in-package #:telemetry)
;;
;;   (def-vertex calibration ()
;;       ((value :type double-float :check helpdesk::telemetry-plausible-p))
;;       :support)
;;
;;   (def-vertex reading ()
;;       ((sensor-id :type string)
;;        (value :type double-float)
;;        (taken-at :type integer))
;;       :support)
;;
;; Two things worth noticing that only show up once real metadata goes
;; through the exporter:
;;
;;   - The :EXPORT list is one name per line.  A ten-plus-type
;;     namespace's export clause is the known column-80 offender when
;;     run together on one line, so EXPORT-SCHEMA-SOURCE always breaks
;;     it out.
;;
;;   - TELEMETRY-PLAUSIBLE-P prints package-qualified
;;     (HELPDESK::TELEMETRY-PLAUSIBLE-P), not bare, even though this
;;     file's IN-PACKAGE is :TELEMETRY.  The symbol was read where
;;     REGISTER-SCHEMA-FUNCTION was called above -- ambient package
;;     :HELPDESK, not :TELEMETRY -- and the exporter qualifies any
;;     symbol whose home package a bare token would NOT resolve back
;;     to: printing it bare here would, on load, INTERN A NEW SYMBOL
;;     under :TELEMETRY instead of finding the original one, silently
;;     breaking the EQ-keyed lookup against *SCHEMA-FUNCTIONS*.  A
;;     symbol homed in the exported namespace itself, or in
;;     COMMON-LISP or GRAPH-DB (which the generated DEFPACKAGE always
;;     :USEs), prints bare -- see SENSOR-ID/VALUE/TAKEN-AT and the
;;     type names above.
;;
;; This is how a runtime type is PROMOTED: the developer reviews the
;; file, adds it to the .asd before their method files, commits.  The
;; type is now source-defined; the persisted metadata agrees with it
;; and stays agreeing (def-vertex re-registration is the existing
;; replace-in-place).  The engine itself NEVER loads this file --
;; export is for humans and their build systems only.

;;; Session 1 ends: the process exits.  Everything above survives as
;;; metadata: node-type rows (name, parents, slots, package, default
;;; store, constraint NAMES) in each holding store's schema.dat, plus
;;; -- new in #172 -- a system-level namespace+type manifest
;;; (schema-manifest.dat) beside the type registry, so the full schema
;;; is enumerable WITHOUT opening every store.  No source anywhere.

;;; ===================================================================
;;; SESSION 2 -- a fresh Lisp: the load-order problem, solved at load
;;; ===================================================================

;;; The developer writes ordinary methods against the runtime class.
;;; THE PROBLEM in one line: compile-file cannot compile
;;;
;;;   (defmethod plot ((r telemetry:reading)) ...)
;;;
;;; when TELEMETRY:READING exists only in schema.dat -- the package
;;; does not even exist to read the symbol.  The answer is one form,
;;; placed where schema has always had to live: BEFORE the code that
;;; uses it.  A second constraint follows from the same load-order
;;; discipline: MATERIALIZE-SCHEMA fails fast
;;; (MATERIALIZE-UNRESOLVED-FUNCTIONS) on any :CHECK name the image
;;; does not yet provide, so REGISTER-SCHEMA-FUNCTION must run BEFORE
;;; it, not after -- the earlier draft of this example registered the
;;; function in plotting.lisp, loaded AFTER runtime-schema.lisp; under
;;; the shipped fail-fast behaviour that load order aborts the build.

;;; --- helpdesk.asd ---------------------------------------------------
;; (defsystem "helpdesk"
;;   :depends-on ("graph-db")
;;   :components
;;   ((:file "package")
;;    ;; Static schema, as always:
;;    (:file "schema"        :depends-on ("package"))
;;    ;; NEW: registers the functions runtime :CHECK slots name, then
;;    ;; materializes every runtime-defined namespace and type from the
;;    ;; persisted metadata -- packages interned, classes built via the
;;    ;; MOP, accessors and constructors generated -- exactly as if a
;;    ;; schema.lisp had been loaded, except nothing is evaluated: the
;;    ;; input is the metadata rows, pure data.
;;    (:file "runtime-schema" :depends-on ("schema"))
;;    ;; Methods compile AFTER both, so every class -- source-defined
;;    ;; or runtime-defined -- exists at compile time:
;;    (:file "plotting"      :depends-on ("runtime-schema"))))

;;; --- runtime-schema.lisp -------------------------------------------
(in-package :helpdesk)

;; The constraint function MATERIALIZE-SCHEMA's :CHECK rows will name,
;; provided by code that ships in the image.  Registering it here,
;; before the MATERIALIZE-SCHEMA call below, is not a style choice: a
;; missing name at materialize time aborts the WHOLE call before
;; anything is built (one condition naming every unresolved name), by
;; design (approved point C, R3/R5) -- fail fast at load, not at first
;; write three files later.
(graph-db:register-schema-function 'telemetry-plausible-p
  (lambda (v) (< -1d6 v 1d6)))

;; Reads the system manifest at LOAD TIME (including during
;; compile-file's load of this file), creates packages and classes for
;; every runtime-defined type.  Idempotent; a type whose class already
;; exists (e.g. defined by schema.lisp above) is left alone -- source
;; wins, and a slot-set disagreement between source and metadata
;; signals the #196 divergence warning rather than silently picking.
;;
;; EVAL-WHEN matters and the macro carries it so users cannot get it
;; wrong: (:compile-toplevel :load-toplevel :execute).
;;
;; Returns (:NAMESPACES n :MATERIALIZED n :SKIPPED-EXISTING n) -- the
;; REPL summary; not used here, MATERIALIZE-SCHEMA is a top-level form
;; run for effect.
(graph-db:materialize-schema "/var/db/helpdesk-system/")

;; A closed variant for deployments that fix their schema at build
;; time exists as an option rather than a second function:
;;   (graph-db:materialize-schema DIR :namespaces '(:telemetry))

;;; --- plotting.lisp -------------------------------------------------
(in-package :helpdesk)

;; Compiles because materialize-schema ran first, and TELEMETRY-
;; PLAUSIBLE-P was already registered before it ran.  This is ordinary
;; CLOS on an ordinary class; the founding bargain holds again.
(defmethod plot ((r telemetry:reading))
  (draw-point (telemetry:taken-at r) (telemetry:value r)))

;;; Runtime use, session 2 -- note ordinary reader syntax now works,
;;; no intern/funcall gymnastics, because the package and accessors
;;; exist before this file was even compiled:

(open-graph :support "/var/db/helpdesk/support/")
(with-transaction ((transaction-manager (lookup-graph :support)))
  (telemetry:make-reading :sensor-id "s-17" :value 21.9d0
                          :taken-at 7200))

(mapcar #'plot
        (map-vertices #'identity (lookup-graph :support)
                      :collect-p t
                      :vertex-type 'telemetry:reading))

;;; ===================================================================
;;; SESSION 3 -- schema evolution at runtime, still visible
;;; ===================================================================

(in-package :helpdesk)

;; Adding a slot to a live runtime type is the same function again --
;; CREATE-VERTEX-TYPE on an existing name is redefinition, exactly as
;; re-evaluating a def-vertex form is today (same replace-in-place,
;; same CLOS class-redefinition semantics for live instances), whether
;; the existing name was itself runtime- or source-defined (approved
;; point B):
(graph-db:create-vertex-type "TELEMETRY:READING"
  '((sensor-id :type string)
    (value     :type double-float)
    (taken-at  :type integer)
    (unit      :type keyword))       ; new slot
  :default-store :support)

;; The developer arriving after that change asks the schema, not grep:
(graph-db:describe-schema :namespace :telemetry :since "2026-08-25")
;; => shows READING with UNIT, tagged [runtime 2026-08-25]: the text
;;    dump doubles as a change log because every meta row carries its
;;    definition time, and :SINCE filters rows recorded at or after
;;    it (a "YYYY-MM-DD" string is parsed as local midnight).

;; What is DELIBERATELY NOT PROVIDED, so the boundary stays sharp:
;;  - no runtime defmethod, no persisted functions, no lambda in any
;;    slot option: behaviour ships in the image, structure in the data;
;;  - no automatic loading of exported source by the engine, ever;
;;  - no runtime DELETION of a type in this unit (retraction interacts
;;    with on-disk instances and stays out of scope, as today);
;;  - no runtime def-view/index/unique definition (those macros stay
;;    code-side; R7) and no Emacs mode (the text dump is SLIME-usable
;;    as-is, R6).

;;; ===================================================================
;;; Summary of the API this example commits #172 to, as shipped
;;; ===================================================================
;;;
;;;   ensure-namespace          name &key nicknames
;;;                                  record-p (T)          -> package
;;;     (RECORD-P NIL is MATERIALIZE-SCHEMA's own internal replay
;;;      knob, suppressing a manifest re-append for an unchanged row;
;;;      an ordinary caller never passes it.)
;;;   create-vertex-type        name slot-specs
;;;                             &key parents default-store
;;;                                  keep-revisions        -> class
;;;   create-edge-type          name slot-specs &key ...   -> class
;;;   register-schema-function  name function              -> name
;;;   find-schema-function      name                       -> function or NIL
;;;   materialize-schema        system-dir &key namespaces
;;;     -> (:namespaces n :materialized n :skipped-existing n)
;;;     (load-time safe; :compile-toplevel; idempotent; source wins;
;;;      an unresolved :CHECK function, or an unbuildable parent, each
;;;      abort the WHOLE call with one condition naming every offender
;;;      -- MATERIALIZE-UNRESOLVED-FUNCTIONS / MATERIALIZE-UNRESOLVED-
;;;      PARENTS -- before anything is built)
;;;   describe-schema           &key namespace store since stream
;;;   export-schema-source      path &key namespace store  -> truename
;;;
;;; The three open points the original draft flagged, as resolved by
;;; the unit spec's rulings (docs/superpowers/specs/2026-08-24-runtime-
;;; schema-172-design.md):
;;;
;;;   A. Where MATERIALIZE-SCHEMA reads from: a system-level manifest,
;;;      schema-manifest.dat, beside the type registry under
;;;      *SYSTEM-DIRECTORY* -- append-only, enumerable without opening
;;;      any store, fail-safe (no system directory or a torn/damaged
;;;      file degrades to in-image-only, never aborts) (R2).
;;;
;;;   B. CREATE-*-TYPE on an existing name, source- or runtime-defined,
;;;      IS allowed: ordinary CLOS class redefinition, with the #196
;;;      divergence warning on slot disagreement, exactly like
;;;      re-evaluating DEF-VERTEX (R4) -- session 3 above.
;;;
;;;   C. Constraint-name resolution: PRESENCE is verified fail-fast, at
;;;      CREATE-*-TYPE time and again at MATERIALIZE-SCHEMA time;
;;;      RESOLUTION to the actual function happens at each check, so a
;;;      re-registration takes effect immediately (R5).
