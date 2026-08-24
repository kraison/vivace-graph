;;;; Runtime schema: the intended developer experience (GH #172).
;;;;
;;;; This file is a DESIGN ARTIFACT, not loadable code.  It is the
;;;; example application requested before implementation: every form
;;;; below is written against the API #172 intends to build, and the
;;;; commentary states the design decision each form embodies.  Read it
;;;; top to bottom as three Lisp sessions separated by process restarts.
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
;;;;     code; the spec's invariant ("restart never evaluates data")
;;;;     holds because building a class from a slots list is a
;;;;     mechanical MOP operation on data.
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

;; A runtime namespace is a package plus, later, metadata rows.  This
;; allocates NO files and creates NO store (spec: schema creation must
;; be cheap, store creation must not be).  Idempotent.
(graph-db:ensure-namespace "TELEMETRY" :nicknames '("TLM"))

;; The runtime twin of def-vertex.  NAME may be a symbol or a
;; "PACKAGE:NAME" string (the caller may only have strings -- this is
;; runtime).  Slot specs are the same data def-vertex takes.  The
;; trailing :DEFAULT-STORE mirrors def-vertex's trailing argument.
;;
;; Returns the finalized CLOS class.  Under the hood this drives the
;; SAME registration path def-vertex expands into -- one meta, one
;; registry id keyed on the interned symbol, instantiated into the
;; default store if open -- plus the one new primitive the spec names:
;; class-from-metadata (defclass via the MOP, accessors and
;; MAKE-/LOOKUP-/-P helpers interned in the symbol's package).
(graph-db:create-vertex-type "TELEMETRY:READING"
  '((sensor-id :type string)
    (value     :type double-float)
    (taken-at  :type integer))
  :default-store :support)

;; Behaviour cannot be data (a closure does not serialize).  A runtime
;; type that wants a constraint NAMES a function registered by code
;; that shipped in the image.  Registration is code-side and explicit;
;; the metadata stores only the NAME.  At restart the name is resolved
;; against this registry and materialization FAILS CLEANLY (a named
;; condition listing the missing function) if the image no longer
;; provides it -- restart still never evaluates data.
(graph-db:register-schema-function 'telemetry-plausible-p
  (lambda (v) (< -1d6 v 1d6)))

(graph-db:create-vertex-type "TELEMETRY:CALIBRATION"
  '((value :type double-float
           :value-constraint telemetry-plausible-p))  ; a NAME, not a #'
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
;;; ten-line schema.el can later put it in a dedicated buffer.  Every
;;; type row says WHERE IT CAME FROM -- source or runtime -- because
;;; that provenance is precisely what the developer cannot currently
;;; see:

(graph-db:describe-schema :namespace :telemetry)
;; =>
;; Namespace TELEMETRY (runtime, created 2026-08-24T18:02:11Z)
;;   READING (vertex) default-store :SUPPORT   [runtime 2026-08-24]
;;     SENSOR-ID  string
;;     VALUE      double-float
;;     TAKEN-AT   integer
;;   CALIBRATION (vertex) default-store :SUPPORT [runtime 2026-08-24]
;;     VALUE      double-float  :value-constraint TELEMETRY-PLAUSIBLE-P
;;
;; (describe-schema)               -- everything, grouped by namespace
;; (describe-schema :store :support) -- what one store's schema holds
;; Static types print the same way tagged [source], so ONE tool shows
;; the whole schema, not only the runtime part.

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
;;   (defpackage #:telemetry (:use #:cl #:graph-db)
;;     (:nicknames #:tlm)
;;     (:export #:reading #:calibration))
;;   (in-package #:telemetry)
;;   (def-vertex reading ()
;;     ((sensor-id :type string)
;;      (value     :type double-float)
;;      (taken-at  :type integer))
;;     :support)
;;   (def-vertex calibration ()
;;     ((value :type double-float
;;             :value-constraint telemetry-plausible-p))
;;     :support)
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
;;; -- new in #172 -- a system-level namespace+type manifest beside the
;;; type registry, so the full schema is enumerable WITHOUT opening
;;; every store.  No source anywhere.

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
;;; uses it.

;;; --- helpdesk.asd ---------------------------------------------------
;; (defsystem "helpdesk"
;;   :depends-on ("graph-db")
;;   :components
;;   ((:file "package")
;;    ;; Static schema, as always:
;;    (:file "schema"        :depends-on ("package"))
;;    ;; NEW: materialize every runtime-defined namespace and type from
;;    ;; the persisted metadata -- packages interned, classes built via
;;    ;; the MOP, accessors and constructors generated -- exactly as if
;;    ;; a schema.lisp had been loaded, except nothing is evaluated:
;;    ;; the input is the metadata rows, pure data.
;;    (:file "runtime-schema" :depends-on ("schema"))
;;    ;; Methods compile AFTER both, so every class -- source-defined
;;    ;; or runtime-defined -- exists at compile time:
;;    (:file "plotting"      :depends-on ("runtime-schema"))))

;;; --- runtime-schema.lisp -------------------------------------------
(in-package :helpdesk)

;; Reads the system manifest at LOAD TIME (including during
;; compile-file's load of this file), creates packages and classes for
;; every runtime-defined type.  Idempotent; a type whose class already
;; exists (e.g. defined by schema.lisp above) is left alone -- source
;; wins, and a slot-set disagreement between source and metadata
;; signals the #196 divergence warning rather than silently picking.
;;
;; EVAL-WHEN matters and the macro carries it so users cannot get it
;; wrong: (:compile-toplevel :load-toplevel :execute).
(graph-db:materialize-schema "/var/db/helpdesk-system/")

;; A closed variant for deployments that fix their schema at build
;; time exists as an option rather than a second function:
;;   (graph-db:materialize-schema DIR :namespaces '(:telemetry))

;;; --- plotting.lisp -------------------------------------------------
(in-package :helpdesk)

;; Compiles because materialize-schema ran first.  This is ordinary
;; CLOS on an ordinary class; the founding bargain holds again.
(defmethod plot ((r telemetry:reading))
  (draw-point (telemetry:taken-at r) (telemetry:value r)))

;; And the constraint function the metadata names must be provided by
;; the image before any write touches CALIBRATION; a missing name is a
;; clean, named error at materialize time (listing every unresolved
;; function), not a mystery at first write:
(graph-db:register-schema-function 'telemetry-plausible-p
  (lambda (v) (< -1d6 v 1d6)))

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
;; same CLOS class-redefinition semantics for live instances):
(graph-db:create-vertex-type "TELEMETRY:READING"
  '((sensor-id :type string)
    (value     :type double-float)
    (taken-at  :type integer)
    (unit      :type keyword))       ; new slot
  :default-store :support)

;; The developer arriving after that change asks the schema, not grep:
(graph-db:describe-schema :namespace :telemetry :since "2026-08-24")
;; => shows READING with UNIT marked [runtime 2026-08-25], i.e. the
;;    text dump doubles as a change log because every meta row carries
;;    its definition timestamp.

;; What is DELIBERATELY NOT PROVIDED, so the boundary stays sharp:
;;  - no runtime defmethod, no persisted functions, no lambda in any
;;    slot option: behaviour ships in the image, structure in the data;
;;  - no automatic loading of exported source by the engine, ever;
;;  - no runtime DELETION of a type in this unit (retraction interacts
;;    with on-disk instances and stays out of scope, as today).

;;; ===================================================================
;;; Summary of the API this example commits #172 to
;;; ===================================================================
;;;
;;;   ensure-namespace          name &key nicknames        -> package
;;;   create-vertex-type        name slot-specs
;;;                             &key parents default-store
;;;                                  keep-revisions        -> class
;;;   create-edge-type          name slot-specs &key ...   -> class
;;;   register-schema-function  name function              -> name
;;;   materialize-schema        system-dir &key namespaces -> summary
;;;     (load-time safe; :compile-toplevel; idempotent; source wins;
;;;      unresolved function names -> one named condition, clean)
;;;   describe-schema           &key namespace store since stream
;;;   export-schema-source      path &key namespace store  -> truename
;;;
;;; Open design points to settle in the unit spec (flagged, not hidden):
;;;   A. Where materialize-schema reads from: a new system-level
;;;      manifest beside the type registry (proposed -- enumerable
;;;      without opening stores, appended on every registration), vs
;;;      scanning registered stores' schema.dat files.
;;;   B. Whether create-*-type on a SOURCE-defined name is allowed
;;;      (proposed: yes with the #196 divergence warning, since
;;;      re-evaluating def-vertex already is) or refused.
;;;   C. Constraint-name resolution time: at materialize (fail fast,
;;;      proposed) vs at first write (lazier, mirrors adoption).
