# Hot-swap drift detector — making stale-JVM failures loud

**Status:** specification (not yet implemented)
**Pattern:** `agency/loud-failure`
**Originating session:** zai-ab5c5f8ce5bd4679afebb303f990d23c, 2026-08-04
**PSR:** psr-8c5b6c79-b454-47a0-8f7a-da5af70087f5

## The silent failure this addresses

During the session above, a fix (`1e366ef8 "Surface ex-data details on
PSR/PUR/PAR shape failures"`) was committed to git and verified in a
freshly-spawned JVM, but never hot-swapped into the long-running serving
JVM. The result: `psr_select` against the live server returned the *old*
bare `"Invalid evidence shape"` error with no `:details`, while the same
call against a fresh JVM returned the field-level diagnostic. An entire
before/after measurement was nearly invalidated because nothing reported
that the running code differed from HEAD.

This is the loud-failure pattern in pure form:
- the divergence was **detectable** (source mtime > namespace load time)
- detection was **cheap** (a handful of `File.lastModified` calls)
- the cost of **not** detecting was real wasted work (a runner two days
  earlier had abandoned a recording under the same stale error)

## Why boot-context is the right home

`boot-context` is the orientation tool agents call to ground themselves.
It already reports `:git {:branch :dirty-count :dirty-files}`. The
natural extension is a `:namespace-drift` key that flags any loaded
namespace whose on-disk source file is newer than the namespace's load
time.

An agent calling `boot_context` at the start of a turn would see:

```
:git {:branch "master" :dirty-count 3 :dirty-files [...]}
:namespace-drift {:stale-count 1
                  :stale-namespaces ["futon3c.peripheral.real-backend"]
                  :note "source file newer than loaded namespace; hot-swap likely needed"}
```

That single line would have prevented this session's silent failure.

## Detection mechanism

Clojure does not stamp namespace load time directly, but two practical
approaches work:

### Approach A: source mtime vs. a load-time snapshot (preferred)

On JVM start (or on first `boot-context` call), snapshot the mtime of
each `.clj` source file for every loaded namespace under
`futon3c.*`. On subsequent `boot-context` calls, re-stat those files. A
file whose mtime increased is a drifted namespace.

Tradeoff: requires a baseline snapshot. The snapshot itself is cheap
(one `File.lastModified` per loaded ns, ~50-200 ns).

### Approach B: content hash (fallback, more expensive)

Hash the on-disk source of each loaded namespace and compare to a
baseline hash captured at load time. Catches edits that don't change
mtime (rare: `touch -t` recovery, VCS operations that rewrite mtimes).

Tradeoff: hashing is ~1000x more expensive than mtime and rarely adds
signal over Approach A.

**Recommendation:** Approach A, with the baseline captured lazily on
first `boot-context` call (so the detector is self-initializing and
needs no JVM-start wiring).

## Scope of namespaces to check

Restrict to namespaces whose source lives under the project's `src/`
tree (specifically `futon3c.*` and `futon.*`). This avoids noise from
third-party libraries in `~/.m2` whose source is never hot-swapped.

Implementation: walk `(all-ns)`, filter by `(.getName (ns-name ns))`
starting with `"futon"`, resolve the source file via the var metadata
`:file` of any public var in the ns, stat it.

## What the detector must NOT do

- **Must not auto-reload.** Loud-failure means *surface*, not *act*.
  Auto-reload in a tool that agents call for orientation would have
  surprising side effects (re-evaluating defs, resetting atoms). The
  detector reports; the operator (or a dedicated hot-swap tool) reloads.
- **Must not block.** Orientation tools must stay fast. Cap the check at
  the `futon*` namespace set and time-bound it (it will be sub-ms in
  practice, but a hard ceiling prevents pathological cases).
- **Must not report transient dirty working-tree files as drift.** The
  `:git` key already covers dirty files. This detector is specifically
  about *loaded code vs. on-disk source*, which is a different question
  from "is the worktree clean."

## Relationship to the 2026-08-03 `tool-error` defect

The `tool-error` fix in `1e366ef8` (surfacing `ex-data :details` on
shape-validation failures) is a loud-failure intervention at the
*error-reporting* layer. This detector is a loud-failure intervention at
the *orientation* layer. They address the same failure class
(silent-loss-of-signal) at different points in the agent loop. Both are
needed: `tool-error` makes individual errors actionable; the drift
detector makes the *absence* of a fix visible.

## Open questions for implementation

1. Should the baseline snapshot persist across `boot-context` calls (in
   an atom in `memory_backend.clj`) or be recomputed each call?
   **Lean:** persist in an atom, initialized on first call. A
   per-call baseline would make the detector a no-op (everything looks
   fresh against a baseline taken milliseconds ago).
2. Should this also cover `.cljc` files? **Lean:** yes, but v1 can be
   `.clj`-only since that covers the hot-swap surface.
3. Should the detector live in `boot-context` or be a separate tool?
   **Lean:** `boot-context`, because that is the tool agents already
   call for orientation and the instruction "refresh with boot_context
   when you need current state" already primes them to expect liveness
   signals there.
