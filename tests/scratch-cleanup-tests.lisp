;;;; Scratch-space manager tests (GH #214).
;;;;
;;;; The sweep tests operate ONLY on fabricated roots the test owns --
;;;; never on the real temp root, whose live entries belong to concurrent
;;;; runs on this shared host.

(in-package #:graph-db/test)

(def-suite scratch-cleanup-suite
  :description "Per-run scratch parent + stale sweep (GH #214)."
  :in graph-db-suite)

(in-suite scratch-cleanup-suite)

(test scratch-lives-under-run-parent
  "Fixture scratch (dirs and file names) lands under the per-run parent."
  (let ((parent (namestring (graph-db-test-scratch:scratch-run-directory)))
        (dir (make-temp-directory))
        (file (make-temp-file-name "sc214" "dat")))
    (unwind-protect
         (progn
           (is (uiop:string-prefix-p parent (namestring dir)))
           (is (uiop:string-prefix-p parent (namestring file))))
      (uiop:delete-directory-tree dir :validate t
                                      :if-does-not-exist :ignore))))

(defun make-fake-scratch-entry (root name)
  "Create directory or plain-file NAME under ROOT; returns its pathname.
NAME ending in / makes a directory."
  (let ((path (merge-pathnames name root)))
    (if (uiop:directory-pathname-p path)
        (ensure-directories-exist path)
        (with-open-file (s path :direction :output
                                :if-does-not-exist :create)
          (declare (ignorable s))))
    path))

;; MOST-NEGATIVE-FIXNUM (not 0) so any age qualifies even when a
;; filesystem timestamp runs ahead of GET-UNIVERSAL-TIME.
(test sweep-deletes-stale-matching-entries
  "With every age qualifying, the sweep removes every matching entry in
a fabricated root -- legacy flat dirs, old run parents, loose files --
and never touches non-matching names."
  (with-temp-directory (root)
    (let ((stale-dir (make-fake-scratch-entry root "graph-db-test-old1/"))
          (stale-run (make-fake-scratch-entry root "graph-db-test-run-old2/"))
          (stale-file (make-fake-scratch-entry root "graph-db-test-old3.dat"))
          (other-dir (make-fake-scratch-entry root "keepme-dir/"))
          (other-file (make-fake-scratch-entry root "keepme.dat"))
          ;; Near-miss: shares "graph-db-test" but not the "-" boundary.
          (near-miss (make-fake-scratch-entry root "graph-db-testx/")))
      (is (= 3 (graph-db-test-scratch:sweep-stale-scratch
                :root root :max-age-seconds most-negative-fixnum)))
      (is (null (uiop:directory-exists-p stale-dir)))
      (is (null (uiop:directory-exists-p stale-run)))
      (is (null (probe-file stale-file)))
      (is (not (null (uiop:directory-exists-p other-dir))))
      (is (not (null (probe-file other-file))))
      (is (not (null (uiop:directory-exists-p near-miss)))))))

(defun make-symlink (target link)
  "POSIX symlink LINK -> TARGET via ln(1); NIL where that isn't possible."
  (ignore-errors
    (uiop:run-program
     (list "ln" "-s"
           (string-right-trim "/" (namestring target))
           (string-right-trim "/" (namestring link))))
    t))

(test sweep-skips-symlinks
  "Symlinks with matching names are SKIPPED, never followed: deleting
through a dir link would empty its target (GH #214).  Pinned behavior:
the link itself survives too."
  (with-temp-directory (root)
    (let* ((victim (merge-pathnames "victim-dir/" root))
           (victim-file (merge-pathnames "victim.dat" victim))
           (dir-link (merge-pathnames "graph-db-test-evil/" root))
           (file-link (merge-pathnames "graph-db-test-evil.dat" root)))
      (ensure-directories-exist victim)
      (with-open-file (s victim-file :direction :output
                                     :if-does-not-exist :create)
        (write-string "keep" s))
      (if (not (and (make-symlink victim dir-link)
                    (make-symlink victim-file file-link)))
          (skip "no symlink support on this host")
          (progn
            (is (= 0 (graph-db-test-scratch:sweep-stale-scratch
                      :root root
                      :max-age-seconds most-negative-fixnum)))
            (is (not (null (uiop:directory-exists-p victim))))
            (is (not (null (probe-file victim-file))))
            ;; The links resolve to intact targets, so they still exist.
            (is (not (null (uiop:directory-exists-p dir-link))))
            (is (not (null (probe-file file-link)))))))))

(test sweep-keeps-fresh-matching-entries
  "Entries younger than the age threshold survive the sweep, matching
name or not -- the guard that makes the sweep safe beside live runs."
  (with-temp-directory (root)
    (let ((fresh-dir (make-fake-scratch-entry root "graph-db-test-fresh/"))
          (fresh-file (make-fake-scratch-entry root "graph-db-conc-f.dat")))
      (is (= 0 (graph-db-test-scratch:sweep-stale-scratch
                :root root :max-age-seconds most-positive-fixnum)))
      (is (not (null (uiop:directory-exists-p fresh-dir))))
      (is (not (null (probe-file fresh-file)))))))
