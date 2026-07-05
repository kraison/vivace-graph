;;;; Shared schema for the 3-process (1 hub + 2 device) peer-replication test.
;;;;
;;;; Loaded IDENTICALLY by the hub and both devices (same schema-digest, same
;;;; node type-ids on the wire).  Models authority-scoped disclosure across MORE
;;;; THAN ONE device, to exercise what the 2-process test cannot: scope isolation
;;;; (A sees data B doesn't, and vice versa), overlap (a node both devices hold),
;;;; per-device re-task purge, and purge -> re-entry (PT-2).
;;;;
;;;; Disclosure is keyed by a per-node TEAM and a per-device SCOPE: a node is
;;;; disclosable to a device iff its TEAM is "both" or equals the device's SCOPE.
;;;; The graph is a closed subgraph rooted at the shared SITE:
;;;;
;;;;            site (both)
;;;;          /     |      \
;;;;   survey-a   survey-b   survey-s        survey-a: team "alpha"
;;;;   (alpha)    (bravo)    (both)          survey-b: team "bravo"
;;;;    /  \        |          |             survey-s: team "both"  (shared)
;;;;  fa1  fa2     fb1        fs1
;;;; (alpha)(alpha)(bravo)   (both)
;;;;
;;;; Device A (scope "alpha") holds: site, survey-a, fa1, fa2, survey-s, fs1.
;;;; Device B (scope "bravo") holds: site, survey-b, fb1,        survey-s, fs1.
;;;; Overlap (both hold): site, survey-s, fs1.  A never sees fb1; B never sees fa*.
;;;;
;;;; Note the closed-subgraph reachability: fa* sit UNDER survey-a (alpha), which
;;;; B cannot traverse -- so even re-teaming fa1 to "bravo" does NOT leak it to B
;;;; (the only path runs through an undisclosed survey).  That's the engine doing
;;;; the right thing, and the re-task phases below assert it.

(in-package :graph-db)

(def-vertex m-site ()
  ((name :type string)
   (team :type string))
  :peer-multi-app)

(def-vertex m-survey ()
  ((name :type string)
   (team :type string))
  :peer-multi-app)

(def-vertex m-find ()
  ((name   :type string)
   (team   :type string)
   (status :type string))
  :peer-multi-app)

(def-edge m-has-survey () () :peer-multi-app)   ; site   -> survey
(def-edge m-has-find   () () :peer-multi-app)    ; survey -> find

;;; Fixed device origin ids known to all three processes (the hub mints them for
;;; real; here constants avoid a side channel).
(defparameter *device-a-origin*
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(10 10 10 10 0 0 0 0 0 0 0 0 0 0 0 1)))
(defparameter *device-b-origin*
  (make-array 16 :element-type '(unsigned-byte 8)
                 :initial-contents '(11 11 11 11 0 0 0 0 0 0 0 0 0 0 0 2)))

;;; The disclosure seam (design §7): disclosable to SCOPE iff team "both" or =SCOPE.
;;; Downward-closed by construction (a child's team is visible wherever its parent
;;; is: alpha/bravo finds under same-team surveys under the "both" site).
(defun peer-multi-disclosable (vertex graph scope)
  (declare (ignore graph))
  (let ((team (slot-value vertex 'team)))
    (or (string= team "both")
        (string= team scope))))
