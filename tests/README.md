# Running the `graph-db` test suite

Two things about this environment cost real time during the multi-slot
index work (`#107`) and are recorded here so they don't cost it again.

## The default heap is too small for a full run

```
sbcl --dynamic-space-size 12288 --non-interactive \
  --eval '(asdf:test-system :graph-db)'
```

SBCL's default heap (1 GiB) is exhausted partway through a full run —
`make-graph` eagerly builds 131,072 index-lists per graph, and the suite
opens many graphs in one image. `--dynamic-space-size 12288` has been
sufficient; the default has not.

A full run also exceeds most shells' default command timeout (well past
two minutes). Run it as a single backgroundable/long-timeout invocation
that writes to a file, then read the file back — don't let a wrapper
auto-background and silently lose the result.

## `graph-db/test` does not `:use #:graph-db`

The test package (`tests/package.lisp`) curates an explicit
`:import-from` list rather than `:use`-ing the whole `graph-db` package.
A newly exported symbol used unqualified in a test therefore does **not**
fail at compile time if you forget to add it to that list — it silently
interns a *fresh* symbol in `graph-db/test`, and the failure only
surfaces later as an undefined-function or undefined-variable error at
fasl-load time (or a mysteriously-never-matching `eq`/`find-class`).
This bit two separate implementers during this work. When a test needs a
symbol newly exported from `graph-db`, add it to the `:import-from` list
in `tests/package.lisp` explicitly — don't assume `:use`-style visibility.

## Individual suites

```lisp
(asdf:load-system :graph-db/test)
(fiveam:run! 'graph-db/test::index-suite)
(fiveam:run! 'graph-db/test::unique-constraint-suite)
(fiveam:run! 'graph-db/test::peer-index-suite)
(fiveam:run! 'graph-db/test::peer-unique-suite)
```

## The replication harness is outside the FiveAM matrix

`tests/replication/run-replication-test.sh` is a shell script, not a
FiveAM suite — `asdf:test-system` never runs it. Run it deliberately.
It spawns master and slave as two separate OS processes (replication
cannot be tested in one Lisp image; they'd share process-global state
such as `*buffer-pool*` and `*graphs*`) and loads `graph-db` via
`ql:quickload`, which resolves through Quicklisp's `local-projects`
registry — if that's a symlink to a *different* checkout than the one
you're testing, the harness silently tests the wrong code. Point it at
the checkout you mean with `CL_SOURCE_REGISTRY`, e.g.:

```sh
CL_SOURCE_REGISTRY='(:source-registry (:tree "/path/to/your/checkout/") :inherit-configuration)' \
  tests/replication/run-replication-test.sh
```

## Running on ECL and CCL

Neither is release-gating (CLAUDE.md); both are run in dedicated
sessions. Recorded 2026-10-07 from those runs (GH #403):

- **ECL.** Warm the FASL cache once from a real TTY (`ssh -tt`, or
  `script`) before any redirected or `nohup` run: a cold build under
  redirect fails in `run-program` with ":OUTPUT argument ... does not
  have a file handle". Run under a pty. ECL buffers output, so a frozen
  log does not show the current test; attach `gdb` to find it. Raise the
  GC heap cap first: `(ext:set-limit 'ext:heap-size 0)`.
- **CCL (Linux only).** Run with `< /dev/null` and a hard `timeout`: on
  error CCL drops to a stdin listener, and an EOF'd listener exits 0, so
  a crash would read as success. Keep driver scripts out of `/tmp`
  (unverified: this looks like the `TEMPORARY-FILES:` logical pathname).
