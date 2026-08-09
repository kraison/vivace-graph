# ECL bug report (paste target) — `change-class` retains memory per call on a user-metaclass node model

**Where to file:** https://gitlab.com/embeddable-common-lisp/ecl/-/work_items
**Filed by:** (needs a GitLab account — Claude Code could not authenticate to GitLab from the dev session)

---

## Title

`change-class` on instances of a user-defined metaclass retains memory on every call (unbounded growth under repeated class changes) — ECL 26.5.5

## Environment

- ECL 26.5.5 (Homebrew, macOS arm64) **and** AOT-cross-compiled `aarch64-linux-android` (Android, Boehm GC). Same behavior on both.
- Reproduced against **VivaceGraph** (pure-Common-Lisp graph database), which uses a MOP object model: https://github.com/kraison/vivace-graph-v3
- SBCL 2.5.5 and CCL do **not** exhibit this — their `change-class` reclaims normally.

## Summary

VivaceGraph materializes each persistent node by making a base instance of a metaclass `node-class`
(a `standard-class` subclass) and `change-class`-ing it to the concrete node subtype. Under a
read-heavy workload (each query materializes hundreds–thousands of nodes), **live heap grows roughly
linearly and is never reclaimed**, climbing until OOM. Replacing the single per-read `change-class`
with a direct `make-instance` of the target subclass eliminates the growth entirely — so the retained
memory is attributable to `change-class`.

## Impact (measured, Android field client, Galaxy S24 Ultra, ECL 26.5.5 AOT)

Metric is **LIVE = `GC_get_heap_size()` − `GC_get_free_bytes()`** (i.e. genuinely reachable bytes
after a full `(ext:gc t)`, not high-water):

| | with per-read `change-class` | after removing it |
|---|---|---|
| LIVE growth per query | **+120–155 MB, steady, never plateaus** | ~0 (flat) |
| LIVE over 8 identical queries | 896 → **1999 MB** | 268 → 267 MB |
| open-graph baseline LIVE | ~**750 MB** | **269 MB** |
| heap high-water over the run | **2386 MB** | 462 MB |

The graph is tiny (≈928 nodes / 1847 edges); the memory is entirely per-`change-class` retention.

## Reproduction (against VivaceGraph)

Desktop ECL 26.5.5 reproduces the same growth. LIVE probe via cffi to Boehm:

```lisp
;; (ql:quickload :graph-db)  ;; commit BEFORE the fix, e.g. 9c4956d
;; (ql:quickload :cffi)
(in-package :graph-db)
(defun live-mb ()
  (/ (- (cffi:foreign-funcall "GC_get_heap_size" :unsigned-long)
        (cffi:foreign-funcall "GC_get_free_bytes" :unsigned-long)) 1048576.0))
(def-vertex lv () ((n :type string) (loc :type geometry :index t)) :leak)
(defvar *g* (make-graph :leak "/var/tmp/leak/" :buffer-pool-size 2000))
(setq *graph* *g*)
(let ((prev nil))
  (dotimes (i 800)
    (with-transaction ()
      (let ((v (make-lv :n (format nil "n~A" i)
                        :loc (make-point (+ 37d0 (* i 1d-3)) (+ 49d0 (* i 1d-3))))))
        (when prev (make-le :from prev :to v)) (setq prev v)))))
;; Repeatedly materialize every node.  Each pass runs deserialize -> change-class per node.
(dotimes (r 8)
  (map-vertices (lambda (v) (data v)) *g*)
  (map-edges    (lambda (e) (data e)) *g*)
  (dotimes (k 4) (ext:gc t))
  (format t "~&round ~A  LIVE=~,1F MB~%" r (live-mb)))
```

Observed on ECL 26.5.5 (pre-fix): LIVE climbs ~**+105 MB every round** (460 → 1201 MB over 8 rounds),
never reclaimed. On **SBCL the same code holds LIVE dead flat**. Bisecting shows the growth tracks
the per-node `change-class` in the deserializer, independent of I/O, the MVCC read pin, or JSON
encoding.

## Isolation notes for the ECL team

- The node metaclass `node-class` (a `standard-class` subclass) defines custom
  `node-slot-definition` classes and `slot-boundp-using-class` / `slot-makunbound-using-class`
  `:around` methods, and the subtypes add slots (persistent slots) not present on the base class, so
  `change-class` reallocates instance storage and runs `shared-initialize` for the added slots.
- A **trivial standalone `change-class`** (plain `standard-class`, or a bare custom metaclass, with or
  without added slots) did **not** reproduce the retention in our hands — so the trigger appears to be
  `change-class` interacting with the full user-metaclass setup (custom slot-definition metaobjects +
  `slot-*-using-class` methods) under repeated calls, rather than `change-class` in the abstract. The
  reliable reproduction is via the VivaceGraph node model above. We're happy to help narrow it further.

## Root cause / workaround in VivaceGraph

Constructing the node as its final subclass directly (`make-instance <subclass>`) instead of
base-instance-then-`change-class` removes the leak completely (and is faster). Fix commit:
`30d1837` on branch `peer-replication` (issue #47) — https://github.com/kraison/vivace-graph-v3.
The `change-class` path is retained on SBCL/CCL/LispWorks (no leak there) and only bypassed on ECL.

## Ask

- Confirm whether `change-class` on user-metaclass instances retains per call under Boehm on ECL
  26.5.5, and whether it's fixable in the `change-class` / `update-instance-for-different-class`
  implementation. We can provide a live image, additional traces, or a trimmed reproduction on
  request.
