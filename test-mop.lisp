(in-package :graph-db)

(cl-fad:delete-directory-and-files "/var/tmp/graph/")
;; A store needs a system directory to take type-ids from (GH #186).
(setf *system-directory*
      (namestring (ensure-directories-exist "/var/tmp/graph-system/")))
(setq *graph* (make-graph :test "/var/tmp/graph/"))
(def-vertex tv ()
  ((ts1 :type string) (ts2 :type integer))
  :test)
(make-tv :ts1 "Test string 1" :ts2 1)
(make-tv :ts1 "Test string 2" :ts2 2)
(map-vertices 'identity *graph* :vertex-type 'tv :collect-p t)
(map-vertices (lambda (v)
                (list :ts1 (slot-value v 'ts1)
                      :ts1 (ts1 v)))
              *graph* :vertex-type 'tv :collect-p t)

(def-vertex tv2 (tv)
  ((ts3 :type string) (ts4 :type integer))
  :test)

(make-instance 'tv2 :ts1 "Test string 1" :ts2 1 :ts3 "Test string" :ts4 1)
(make-tv2 :ts1 "Test string 2" :ts2 2)
