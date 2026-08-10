;;;; Temporal extents: construction, granules, and the sexp codec.

(in-package #:graph-db/spacetime-test)

(in-suite spacetime-suite)

(test granule-bounds-cover-the-whole-granule
  (multiple-value-bind (start end) (granule-bounds (ts 2026 1 15 14 30) :month)
    (is-true (timestamp= start (ts 2026 1 1)))
    (is-true (timestamp= end (timestamp- (ts 2026 2 1) 1 :nsec))))
  (multiple-value-bind (start end) (granule-bounds (ts 2026 2 9) :year)
    (is-true (timestamp= start (ts 2026 1 1)))
    (is-true (timestamp= end (timestamp- (ts 2027 1 1) 1 :nsec)))))

(test granule-bounds-handle-february-without-a-table
  "Leap-year correctness comes from LOCAL-TIME's month arithmetic, not from
a days-per-month table (design §3.5)."
  (multiple-value-bind (start end) (granule-bounds (ts 2024 2 10) :month)
    (declare (ignore start))
    (is-true (timestamp= end (timestamp- (ts 2024 3 1) 1 :nsec)))))

(test granules-land-on-absolute-utc-instants
  "Design §3.5.  Asserted against hard-coded Unix seconds, which no timezone
setting can move: 2026-01-01T00:00:00Z is 1767225600 and 2026-02-01T00:00:00Z
is 1769904000.  An unpinned constructor lands 7200 seconds early on a host in
EET, and this is what catches that.

LOCAL-TIME:FIND-TIMEZONE-BY-LOCATION-NAME is deliberately NOT used here: the
timezone repository is not loaded by default and it returns NIL, which would
have made this test vacuous."
  (multiple-value-bind (start end) (granule-bounds (ts 2026 1 15 14 30) :month)
    (is (= 1767225600 (local-time:timestamp-to-unix start)))
    (is (= 1769903999 (local-time:timestamp-to-unix end)))
    (is (= 999999999 (local-time:nsec-of end)))))

(test granule-end-does-not-shift-across-a-dst-transition-month
  "Design §3.5, GH #134.  LOCAL-TIME:TIMESTAMP+ takes no :TIMEZONE and does
:MONTH arithmetic in *DEFAULT-TIMEZONE*, so on a DST-observing host March's
granule end used to land an hour short of 23:59:59Z.  Asserted against a
hard-coded Unix second, which no host timezone can move."
  (multiple-value-bind (start end) (granule-bounds (ts 2026 3 15) :month)
    (declare (ignore start))
    (is (= 1775001599 (local-time:timestamp-to-unix end)))
    (is (= 999999999 (local-time:nsec-of end)))))

(test granule-end-does-not-spill-into-the-next-month
  "Design §3.5, GH #134.  October's granule end used to compute as
2026-11-01T00:59:59Z on a DST-observing host -- inside November, so two
adjacent month granules overlapped instead of meeting.  This asserts both
the exact correct end and, directly, that it falls strictly before
November 1st."
  (multiple-value-bind (start end) (granule-bounds (ts 2026 10 15) :month)
    (declare (ignore start))
    (is (= 1793491199 (local-time:timestamp-to-unix end)))
    (is-true (timestamp< end (ts 2026 11 1))
             "October's granule end reaches into November")))

(test a-day-granule-spans-exactly-86400-seconds-across-dst
  "Design §3.5, GH #134.  A :DAY granule crossing the EU spring-forward
transition used to be 82800s wide instead of a fixed UTC day."
  (multiple-value-bind (start end) (granule-bounds (ts 2026 3 29) :day)
    (is (= 86399 (- (local-time:timestamp-to-unix end)
                    (local-time:timestamp-to-unix start))))
    (is (= 999999999 (local-time:nsec-of end)))))

(test granule-construction-ignores-the-ambient-timezone
  "The other half: rebinding *DEFAULT-TIMEZONE* must not move a granule.
Strong on a non-UTC host, trivially true on a UTC one -- which is why the
absolute-instant test above carries the real weight."
  (let ((ambient (multiple-value-list (granule-bounds (ts 2026 1 15) :month))))
    (let ((*default-timezone* +utc-zone+))
      (let ((pinned (multiple-value-list
                     (granule-bounds (ts 2026 1 15) :month))))
        (is-true (timestamp= (first ambient) (first pinned)))
        (is-true (timestamp= (second ambient) (second pinned)))))))

(test a-granule-interval-is-not-a-granule-instant
  "The §3.3 distinction: \"January 2026\" has exact endpoints; \"sometime in
January 2026\" is one uncertain timestamp."
  (let ((month (make-granule-interval (ts 2026 1 15) :month))
        (point (make-granule-instant (ts 2026 1 15) :month)))
    (is (eq :interval (extent-kind month)))
    (is (eq :instant (extent-kind point)))
    (is-true (bound-exact-p (extent-start month)))
    (is-false (bound-exact-p (extent-start point)))
    (is-true (extent-instant-p point))
    (is-false (extent-instant-p month))))

(test an-instant-couples-its-two-endpoints
  "START and END must be the SAME bound, which is what makes the endpoints
move together (design §3.3)."
  (let ((point (make-granule-instant (ts 2026 1 15) :month)))
    (is (eq (extent-start point) (extent-end point)))))

(test extent-round-trips-through-the-sexp-codec
  (let ((e (make-granule-interval (ts 2026 1 15) :month
                                  :semantics :validity :standing :inferred)))
    (let ((back (sexp->extent (extent->sexp e))))
      (is (eq (extent-kind e) (extent-kind back)))
      (is (eq (extent-precision e) (extent-precision back)))
      (is (eq (extent-semantics e) (extent-semantics back)))
      (is (eq (extent-standing e) (extent-standing back)))
      (is-true (timestamp= (bound-earliest (extent-start e))
                           (bound-earliest (extent-start back))))
      (is-true (timestamp= (bound-latest (extent-end e))
                           (bound-latest (extent-end back)))))))

(test the-codec-preserves-instant-coupling
  "A round-tripped instant must come back coupled, or the algebra would
silently start over-reporting uncertainty for it."
  (let ((back (sexp->extent (extent->sexp
                             (make-granule-instant (ts 2026 1 15) :month)))))
    (is (eq :instant (extent-kind back)))
    (is (eq (extent-start back) (extent-end back)))))

(test the-codec-emits-only-values-core-can-serialize
  "Design §6: no serialize type byte is reserved, so every leaf must already
be a keyword, an integer, or a LOCAL-TIME:TIMESTAMP."
  (labels ((ok (x)
             (or (keywordp x) (integerp x) (null x)
                 (typep x 'local-time:timestamp)
                 (and (listp x) (every #'ok x)))))
    (is-true (ok (extent->sexp (make-granule-instant (ts 2026 1 15) :month))))
    (is-true (ok (extent->sexp
                  (make-interval (unknown-bound) (unknown-bound)
                                 :standing :indeterminate))))))

(test an-extent-rejects-a-bad-standing-or-precision
  (signals invalid-standing
    (make-granule-interval (ts 2026 1 15) :month :standing :probably))
  (signals invalid-extent (make-granule-interval (ts 2026 1 15) :fortnight)))

(test make-interval-rejects-only-a-value-degenerate-pair
  "Design §3.2.  START = END exactly is a point in time -- MAKE-INSTANT's
job -- so MAKE-INTERVAL must reject it.  Bounds that merely overlap are
uncertain, not collapsed, and must still construct (GH #130)."
  (signals invalid-extent
    (make-interval (exact-bound (ts 2026 1 15)) (exact-bound (ts 2026 1 15))))
  (let ((e (make-interval (make-bound (ts 2026 1 10) (ts 2026 1 20))
                          (make-bound (ts 2026 1 15) (ts 2026 1 25)))))
    (is (eq :interval (extent-kind e)))))
