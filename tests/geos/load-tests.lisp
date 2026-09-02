;;;; S0: the add-on loads without error and reports availability sanely.

(in-package #:graph-db/geos-test)

(def-suite geos-load-suite
  :description "graph-db/geos loads and reports availability without crashing."
  :in geos-suite)

(in-suite geos-load-suite)

(test availability-flag-is-boolean
  "Loading the add-on leaves *geos-available-p* a definite boolean (T or NIL),
never unbound or an error."
  (is (typep *geos-available-p* 'boolean)))

(test version-consistent-with-availability
  "When GEOS is available, the version parsed to a (maj min patch) integer list
and the makeValid flag is a boolean; when not, all are NIL."
  (if *geos-available-p*
      (progn
        (is (consp *geos-version*))
        (is (= 3 (length *geos-version*)))
        (is (every #'integerp *geos-version*))
        (is (typep *geos-makevalid-available-p* 'boolean)))
      (progn
        (is (null *geos-version*))
        (is (null *geos-makevalid-available-p*)))))

(test load-geos-is-idempotent-and-returns-boolean
  "Calling load-geos again is safe and returns a boolean matching availability."
  (let ((result (load-geos)))
    (is (typep result 'boolean))
    (is (eq result *geos-available-p*))))

(test modern-geos-here-has-makevalid
  "On this dev/CI machine GEOS is installed and >= 3.8, so makeValid is available.
Skipped automatically where GEOS is absent or older."
  (if (and *geos-available-p* (>= (first *geos-version*) 3)
           (>= (second *geos-version*) 8))
      (is-true *geos-makevalid-available-p*)
      (skip "GEOS absent or < 3.8 on this machine")))

;;; ---- version-string parsing (pure) -------------------------------------

(test parse-geos-version-variants
  "The version parser handles the CAPI-suffixed string and bare versions, and
rejects junk."
  (is (equal '(3 13 1) (graph-db::%parse-geos-version "3.13.1-CAPI-1.19.2")))
  (is (equal '(3 8 0)  (graph-db::%parse-geos-version "3.8.0")))
  (is (equal '(3 11 0) (graph-db::%parse-geos-version "3.11")))   ; missing patch -> 0
  (is (null (graph-db::%parse-geos-version "not-a-version")))
  (is (null (graph-db::%parse-geos-version nil))))

(test geos-version>=-boundaries
  "The >= comparator is correct around the 3.8 makeValid boundary."
  (is-true  (graph-db::%geos-version>= '(3 13 1) 3 8))
  (is-true  (graph-db::%geos-version>= '(3 8 0) 3 8))
  (is-false (graph-db::%geos-version>= '(3 7 9) 3 8))
  (is-true  (graph-db::%geos-version>= '(4 0 0) 3 8))
  (is-false (graph-db::%geos-version>= nil 3 8)))

;;; ---- graceful degradation when the library is missing ------------------

(test missing-library-is-caught-gracefully
  "A failed foreign-library load surfaces as a CFFI error that load-geos's
handler-case swallows -- proving the 'GEOS absent' path leaves the image alive
rather than crashing.  We exercise the same handler shape against a deliberately
bogus library so the test is meaningful even on a machine that HAS GEOS."
  (cffi:define-foreign-library %geos-bogus-lib
    (t (:default "definitely-not-a-real-library-xyzzy")))
  (let ((survived
          (handler-case
              (progn (cffi:use-foreign-library %geos-bogus-lib) :loaded)
            (error () :handled))))
    (is (eq :handled survived)
        "missing library must raise an error our handler catches, not crash")))

(test search-dirs-keep-their-declared-priority-order
  "%ADD-GEOS-LIBRARY-DIRECTORIES must leave CFFI's search list in the SAME order
as the dirs it was given -- highest priority first.

Asserted against dirs that certainly exist, on a fresh binding, so the result
does not depend on which library dirs this particular host happens to have.
That matters: on an Apple-Silicon Mac /usr/local/lib does not exist at all, so a
test phrased against the real list silently asserts nothing and passes on the
BUGGY code.  PUSHNEW prepends, so pushing a priority-ordered list forward
reverses it; this is the regression that behaviour would represent."
  (let* ((cffi:*foreign-library-directories* '())
         (dirs '("/usr" "/tmp"))
         (added (graph-db::%add-geos-library-directories dirs)))
    (is (equal '("/usr" "/tmp") added)
        "returns the dirs it added, in priority order")
    (is (equal (mapcar (lambda (d) (pathname (concatenate 'string d "/"))) dirs)
               cffi:*foreign-library-directories*)
        "search list must read highest-priority-first, got ~S"
        cffi:*foreign-library-directories*)))

(test nonexistent-search-dirs-are-skipped-not-pushed
  "A dir that does not exist is not added -- CFFI would otherwise stat a bogus
path on every library load."
  (let* ((cffi:*foreign-library-directories* '())
         (added (graph-db::%add-geos-library-directories
                 '("/usr" "/no/such/dir/ever" "/tmp"))))
    (is (equal '("/usr" "/tmp") added))
    (is (= 2 (length cffi:*foreign-library-directories*)))))
