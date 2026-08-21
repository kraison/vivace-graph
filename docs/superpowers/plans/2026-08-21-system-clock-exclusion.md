# System Clock Cross-Process Exclusion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `open-system-clock` refuses, immediately and by name, when another live process
holds the clock directory — instead of silently issuing a second stream of epochs.

**Architecture:** An advisory `flock(2)` lock on a file in the clock directory, taken
`LOCK_EX | LOCK_NB` at open and held for the clock's lifetime via an fd on the
`system-clock` struct. The kernel releases it on process death, so a stale lock is
impossible by construction and no recovery step is needed. Two tasks: the FFI wrapper, then
the clock change that uses it.

**Tech Stack:** Common Lisp (SBCL 2.6.6 primary, ECL conditionally supported), CFFI,
FiveAM, ASDF.

**Spec:** `docs/superpowers/specs/2026-08-21-system-clock-exclusion-design.md`

## Global Constraints

- **Lisp: spaces only, never tabs. Hard 80-column limit** — code, comments, docstrings and
  strings alike. A 96-column line is a defect.
- **Comments are terse and point elsewhere.** State the non-obvious fact in a line or two
  and reference an issue or a doc. Do not narrate reasoning in source.
- **Scope is exclusion only.** The clock counter is already crash-safe (`%write-clock-ceiling`
  persists `ceiling + block-size` ahead of issuing). Do **not** add a clean/unclean flag, a
  recovery path, or a `.dirty`-style marker.
- **No new arguments or modes on any public entry point.** No `:read-only`, no `LOCK_SH`.
- **Do not touch** `clock-next-epoch`, `clock-lease-epochs`, `clock-observe-epoch`,
  `journal-append`, `journal-records`, or `attach-to-system-clock`.
- **#191 (torn journal tail) is out of scope.** Same file, unrelated failure.
- Every push that changes source changes docs too; a `PreToolUse` hook enforces it.
- Run tests in the **foreground**. Never two SBCL builds at once — they share one FASL cache
  and concurrent builds corrupt it.

---

### Task 1: `%posix-flock`

**Files:**
- Modify: `posix.lisp` (constants near the existing `+o-*+` block ~line 33-48; wrapper in
  the syscall-wrapper section after `%posix-open`, ~line 100)
- Create: `tests/posix-tests.lisp`
- Modify: `graph-db.asd` (register the new test file)

**Interfaces:**
- Produces: `%posix-flock (fd operation)` → `T` when taken, `NIL` when held elsewhere,
  signals otherwise. Constants `+lock-ex+`, `+lock-nb+`. Helper `%errno`.
- Consumes: `%posix-open`, `%posix-close`, `+o-creat+`, `+o-rdwr+` (all existing).

**Context the brief cannot give you:** nothing in this tree reads `errno` today, and
`graph-db.asd` still carries `#+ecl` conditionals, so ECL is not hypothetical. The `#+ecl`
branch below is a starting point, not a verified one — **verify it or replace it**, and if
ECL cannot report `errno`, make `%errno` return `NIL` there and say so in the docstring
rather than guessing a value.

- [ ] **Step 1: Write the failing tests**

Create `tests/posix-tests.lisp`:

```lisp
;;;; POSIX syscall wrappers.
(in-package #:graph-db/test)

(def-suite posix-suite :in graph-db-suite
  :description "Thin syscall wrappers over CFFI.")
(in-suite posix-suite)

(defun %open-rw (path)
  (graph-db::%posix-open path (logior graph-db::+o-creat+
                                      graph-db::+o-rdwr+)))

(test flock-denies-a-second-open-file-description
  "flock locks attach to the open file description, not the process, so two
OPEN(2) calls in one image contend exactly as two processes would.  That is
what makes GH #182's guard testable without spawning a child."
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "lockme" dir)))
           (a (%open-rw path))
           (b (%open-rw path))
           (op (logior graph-db::+lock-ex+ graph-db::+lock-nb+)))
      (unwind-protect
           (progn
             (is-true (graph-db::%posix-flock a op)
                      "the first descriptor takes the lock")
             (is (null (graph-db::%posix-flock b op))
                 "the second is denied, and denial is NIL, not an error"))
        (graph-db::%posix-close a)
        (graph-db::%posix-close b)))))

(test flock-signals-rather-than-reporting-held-on-a-real-error
  "Acceptance criterion: a genuine failure must stay distinguishable from a
held lock.  EBADF on an invalid descriptor is the cheapest way to reach the
signalling branch -- if %ERRNO cannot report on this implementation, this test
is what catches the resulting misreport."
  (signals error (graph-db::%posix-flock -1 (logior graph-db::+lock-ex+
                                                    graph-db::+lock-nb+))))

(test flock-releases-on-close
  "Close is the only release path the clock uses -- CLOSE-SYSTEM-CLOCK closes
the fd rather than calling LOCK_UN -- so this is the release semantics that
matter."
  (with-temp-directory (dir)
    (let* ((path (namestring (merge-pathnames "lockme" dir)))
           (a (%open-rw path))
           (op (logior graph-db::+lock-ex+ graph-db::+lock-nb+)))
      (is-true (graph-db::%posix-flock a op))
      (graph-db::%posix-close a)
      (let ((b (%open-rw path)))
        (unwind-protect
             (is-true (graph-db::%posix-flock b op)
                      "closing the holder frees the lock")
          (graph-db::%posix-close b))))))
```

Register it in `graph-db.asd` beside `system-clock-tests`:

```lisp
               (:file "posix-tests")             ; GH #182
```

- [ ] **Step 2: Run them and watch them fail**

Run the `posix-suite` only. Expected: failure naming `%POSIX-FLOCK` as undefined. If you
see a *reader* error instead, you have a paren or package problem — fix that first; an
undefined-function failure is the one that means "feature missing".

- [ ] **Step 3: Add the constants**

In `posix.lisp`, with the other flag constants:

```lisp
;;; flock(2).  Same values on Linux and Darwin, so unlike the mmap and open
;;; flags above these need no platform conditional.  No +LOCK-UN+: the clock
;;; releases by closing the fd (GH #182).
(defconstant +lock-ex+ 2)
(defconstant +lock-nb+ 4)

;;; EWOULDBLOCK == EAGAIN on both targets, but the value differs.
(defconstant +eagain+ #+graph-db-posix-linux 11 #-graph-db-posix-linux 35)
```

- [ ] **Step 4: Add `%errno` and `%posix-flock`**

```lisp
(defun %errno ()
  "The current errno, or NIL where this implementation cannot report one.
NIL is honest: a caller must not mistake an unknown failure for a held lock."
  #+sbcl (sb-alien:get-errno)
  #+ecl  (ffi:c-inline () () :int "errno" :one-liner t)
  #-(or sbcl ecl) nil)

(defun %posix-flock (fd operation)
  "flock(2).  Returns T when the lock was taken, NIL when OPERATION included
+LOCK-NB+ and the lock is held elsewhere.  Signals on any other failure: a
caller must not report 'another process holds this' for EBADF or ENOLCK."
  (let* ((r (cffi:foreign-funcall "flock" :int fd :int operation :int))
         (e (unless (zerop r) (%errno))))
    (cond ((zerop r) t)
          ((eql e +eagain+) nil)
          (t (error "posix flock failed for fd ~D (operation ~D, errno ~A)"
                    fd operation e)))))
```

`e` is captured on the line after the call, before anything else can clobber `errno`.

- [ ] **Step 5: Run the tests and watch them pass**

Both green, output pristine. Then run the **whole** suite once to confirm the new ASDF
component did not disturb load order.

- [ ] **Step 6: Ablation**

Change `%posix-flock` to `(declare (ignore operation)) t` — always claim success. Confirm
`flock-denies-a-second-open-file-description` **fails**. Restore, confirm it passes. Record
both numbers in the report. A guard test that passes with the guard removed proves nothing.

- [ ] **Step 7: Commit**

```bash
git add posix.lisp tests/posix-tests.lisp graph-db.asd
git commit -m "feat(posix): flock(2) wrapper distinguishing held from failed (#182)"
```

---

### Task 2: Exclusion in `open-system-clock`

**Files:**
- Modify: `system-clock.lisp` (struct ~line 12; `open-system-clock` ~line 57;
  `close-system-clock` ~line 69; new condition and lock-file helper)
- Modify: `package.lisp` (export the condition, beside `#:open-system-clock` ~line 28)
- Modify: `tests/system-clock-tests.lisp` (append)
- Modify: `CHANGELOG.md`, `docs/vivace-graph-v3-doc.org`

**Interfaces:**
- Consumes: `%posix-flock`, `%posix-open`, `%posix-close`, `+lock-ex+`, `+lock-nb+`,
  `+o-creat+`, `+o-rdwr+` from Task 1.
- Produces: condition `system-clock-in-use` with reader
  `system-clock-in-use-location`; new struct slot `lock-fd`.

**Context the brief cannot give you:** the lock file is
`system-clock.lock`, named to match its siblings `system-clock.dat` and
`system-journal.log`. The spec writes it illustratively as `.lock`; the sibling naming
wins. No ASDF change is needed for load order — `system-clock` depends on `utilities`,
which already depends on `posix`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/system-clock-tests.lisp`:

```lisp
;;; GH #182: two images on one clock directory both issued epochs, silently.

(test second-open-of-a-held-clock-signals
  "The whole point: a second allocator on one system directory destroys the
single property the clock provides."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (signals graph-db:system-clock-in-use
             (open-system-clock (namestring dir)))
        (close-system-clock c)))))

(test a-held-clock-refuses-without-blocking
  "LOCK_NB is deliberate: blocking would present as a startup hang with no
diagnostic.  Proven by the refusal returning at all -- a blocking flock here
would never reach the assertion."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (let ((loc (handler-case
                          (progn (open-system-clock (namestring dir)) nil)
                        (graph-db:system-clock-in-use (e)
                          (graph-db:system-clock-in-use-location e)))))
             (is-true loc "the refusal names the directory it refused"))
        (close-system-clock c)))))

(test a-closed-clock-can-be-reopened
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (close-system-clock c))
    (let ((c2 (open-system-clock (namestring dir))))
      (unwind-protect (is-true c2 "the lock was released by the clean close")
        (close-system-clock c2)))))

(test leasing-works-while-the-lock-is-held
  "A lease-holder is inside the owning image (spec §8.1), so the guard must not
break the #170 path."
  (with-temp-directory (dir)
    (let ((c (open-system-clock (namestring dir))))
      (unwind-protect
           (multiple-value-bind (start end) (clock-lease-epochs c 100)
             (is (= 100 (- end start)))
             (is (>= (clock-current-epoch c) end)
                 "the clock skipped past the leased range"))
        (close-system-clock c)))))

(test epochs-stay-monotonic-across-a-close-and-reopen
  "The lock must not disturb the ceiling protocol."
  (with-temp-directory (dir)
    (let* ((c (open-system-clock (namestring dir)))
           (a (clock-next-epoch c)))
      (close-system-clock c)
      (let ((c2 (open-system-clock (namestring dir))))
        (unwind-protect
             (is (> (clock-next-epoch c2) a)
                 "a reopened clock never reissues")
          (close-system-clock c2))))))
```

- [ ] **Step 2: Run them and watch them fail**

Run `system-clock-suite` only. Expected: `SYSTEM-CLOCK-IN-USE` undefined, and
`second-open-of-a-held-clock-signals` failing because the second open **succeeds**. That
second failure is the defect itself — confirm you see it before fixing anything.

- [ ] **Step 3: Add the condition and the lock-file helper**

In `system-clock.lisp`, after the struct:

```lisp
(define-condition system-clock-in-use (error)
  ((location :initarg :location :reader system-clock-in-use-location))
  (:report
   (lambda (c s)
     (format s "The system clock at ~A is held by another process.  Only one ~
image may allocate epochs for a system; a second would issue epochs colliding ~
with the holder's (GH #182).  A lease-holding process must not open the clock ~
directory -- see the design's §8.1."
             (system-clock-in-use-location c)))))

(defun %clock-lock-file (location)
  (make-pathname :name "system-clock" :type "lock" :defaults location))
```

Add the slot to `system-clock`, after `journal`:

```lisp
  ;; Held open for the clock's lifetime; the kernel releases it on process
  ;; death, so a stale lock cannot happen (GH #182).
  (lock-fd nil))
```

Export from `package.lisp`, beside `#:open-system-clock`:

```lisp
           #:system-clock-in-use
           #:system-clock-in-use-location
```

- [ ] **Step 4: Take the lock in `open-system-clock`**

Replace the body. The `unwind-protect` matters: without it a failure in
`%write-clock-ceiling` leaks the fd, and the leaked fd holds the lock for the life of the
process — turning a transient error into a permanently unopenable clock.

```lisp
(defun open-system-clock (location &key (block-size 4096))
  "Open or create the system clock in directory LOCATION.  Ids resume above
the persisted ceiling, so a crash never reissues one.  Signals
SYSTEM-CLOCK-IN-USE if another live process holds LOCATION (GH #182)."
  (ensure-directories-exist location)
  (let ((fd (%posix-open (%clock-lock-file location)
                         (logior +o-creat+ +o-rdwr+)))
        (opened nil))
    (unwind-protect
         (progn
           (unless (%posix-flock fd (logior +lock-ex+ +lock-nb+))
             (error 'system-clock-in-use :location location))
           (let* ((ceiling (%read-clock-ceiling location))
                  (clock (%make-system-clock :location location
                                             :counter ceiling
                                             :ceiling ceiling
                                             :block-size block-size
                                             :lock-fd fd)))
             (%write-clock-ceiling clock (+ ceiling block-size))
             (setf opened t)
             clock))
      (unless opened (%posix-close fd)))))
```

- [ ] **Step 5: Release it in `close-system-clock`**

Add to the existing `with-recursive-lock-held` body, after the journal clause:

```lisp
    (when (system-clock-lock-fd clock)
      ;; Closing the fd is the release; there is no LOCK_UN path.
      (%posix-close (system-clock-lock-fd clock))
      (setf (system-clock-lock-fd clock) nil))
```

- [ ] **Step 6: Run the tests and watch them pass**

`system-clock-suite` green, then the **full** suite. Report both counts. The full suite is
~15 minutes and exceeds the default Bash timeout — run it in the background and poll, or
raise the timeout. Do not skip it: `open-system-clock` is called from graph open paths.

**Known coverage gap, stated deliberately.** The spec's acceptance criterion *"a holder
that dies leaves no residue"* is **not** covered by a test. It is a kernel guarantee of
`flock`, and reaching it needs a child process that dies holding the lock — machinery worth
more than it buys here, since `flock-releases-on-close` already pins the release path the
clock actually uses. Do not add a subprocess test for it; do not claim it is covered.

- [ ] **Step 7: Ablation**

Delete the `unless (%posix-flock ...)` form from `open-system-clock` — leaving the fd open
so only the *refusal* is gone. Confirm `second-open-of-a-held-clock-signals` **fails**.
Restore, confirm it passes. Record both numbers; state explicitly that you verified the
edit landed before running.

- [ ] **Step 8: Docs**

`CHANGELOG.md`, under `## [Unreleased]` → `### Fixed`: what the hole was (two images both
issuing), the mechanism (`flock`, kernel-released), and **why there is no recovery step** —
the counter is already crash-safe, so a `.dirty`-style marker was rejected.

`docs/vivace-graph-v3-doc.org`, Chapter 17's clock section: one image per system directory;
the refusal is immediate and names the directory; a crashed holder needs no operator action;
`flock` over NFS is unreliable and the design assumes one host; the guard is advisory, so a
process bypassing `open-system-clock` is not stopped.

Wrap Org prose at 79 columns.

- [ ] **Step 9: Commit**

```bash
git add system-clock.lisp package.lisp tests/system-clock-tests.lisp \
        CHANGELOG.md docs/vivace-graph-v3-doc.org
git commit -m "fix(clock): refuse a second process on the clock directory (#182)"
```
