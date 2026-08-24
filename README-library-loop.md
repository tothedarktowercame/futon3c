# Files-only Library Loop

The Library Loop runs one persistent Codex session against one persistent
`apm-lean` Git worktree. It does not use the futon3c JVM, frames, leases,
roles, or a coordinator. State and immutable receipts live under
`data/apm-lane/runs/PROBLEM/`.

## Environment

Run from the canonical futon3c checkout after these commits have landed:

```sh
export LIBRARY_LOOP_ROOT=/absolute/path/to/futon3c
export LIBRARY_LOOP_ADAPTER_NS=futon3c.apm.library-loop-adapter
```

The adapter never invokes a shell command string and never pushes a remote.
Every process receives an argv vector and explicit cwd.

## Initialize

The solver workspace must be an absolute path to the root of an `apm-lean`
Git worktree. `BASE` is the configured trunk head and `HEAD` is the initial
solver-worktree head.

```sh
scripts/library-loop init t00J02 /absolute/apm-lean-solver BASE HEAD
```

Create `data/apm-lane/runs/t00J02/standing-goal.md`, then create
`config.edn`:

```clojure
{:schema 1
 :trunk-worktree "/absolute/apm-lean-trunk"
 :trunk-branch "repair/m97A06-energy-regularity"
 :codex-command ["/absolute/bin/codex" "exec" "{prompt-text}"]
 :lake-executable "/absolute/bin/lake"
 :audit-command ["/absolute/futon3c/scripts/library-loop-audit"
                 "{base}" "{head}" "{run-dir}"]
 :status-command ["/absolute/futon3c/scripts/library-loop-status"
                  "{head}" "{run-dir}"]
 :checkpoint-cadence 20
 :slate-path "/absolute/futon3c/data/apm-lane/demonstrators.edn"}
```

The audit command runs in the solver workspace and must atomically create
`RUN-DIR/audits/HEAD.edn`:

```clojure
{:schema 1
 :head-sha "HEAD"
 :modules
 {"ConstructionTargets.Module"
  {:ok? true :head-sha "HEAD" :declarations [declaration.names]}}}
```

The status command runs in the trunk worktree after the exact fast-forward and
must atomically create `RUN-DIR/status/HEAD.edn`:

```clojure
{:schema 1
 :candidate-sha "HEAD"
 :ruling :partial-banked             ; or :closed
 :status-sha "digest-or-commit"}
```

Problem registration is read from
`WORKSPACE/problems/PROBLEM/targets.edn`. It is a vector of unique records:

```clojure
[{:module "ConstructionTargets.Module"
  :created-turn 20
  :status :active
  :obligation :problem/stable-obligation
  :declarations [ConstructionTargets.Module.headlineTheorem]}]
```

`:declarations` is the exact, nonempty set of promoted declarations owned by
the ledger row. For every changed module, the production audit executable
first runs `lake build MODULE` in the solver workspace, then imports it and
runs Lean `#print axioms` for every listed declaration. This ordering makes a
first-time target auditable without relying on a pre-existing olean; the
ordinary gate still performs its deterministic rebuild closure afterward. A
missing/duplicate list, failed target build, unknown declaration, failed audit
elaboration, or output containing `sorryAx` prevents green evidence. The audit
is bound to the run state's exact workspace, base, and HEAD, records complete
argv/cwd/exit/stdout/stderr evidence for both build and audit per module, and
writes `audits/HEAD.edn` atomically.

For a newly added target, `:created-turn` records the mathematical creation
turn and is never rewritten merely because a later gate retries registration.
Same-turn registration is direct. A delayed correction is accepted only when
the adapter proves from the module's first-add Git commit and the canonical
successful turn receipts that the commit was present at the claimed turn and
absent from every earlier observed turn HEAD. The bounded proof (problem,
module, claimed turn, receipt id, turn HEAD, creation commit, and first-seen
flag) is part of the registration snapshot digest. Future turns, missing or
malformed proofs, and unsupported backdating fail closed.

The production status executable runs only after the exact candidate is the
configured trunk HEAD. It requires a clean trunk, elaborates the canonical
problem `Main.lean`, checks the observed sorry count against the committed
`status.json`, and maps only a consistent `partial` result to
`:partial-banked` or a consistent `solved` result with zero sorries to
`:closed`. It writes `status/HEAD.edn` atomically. Zero sorries alone never
manufacture closure, and neither executable edits apm-lean.

Missing or stale audit evidence, missing ledgers, dirty/racing worktrees, and
divergent SHAs fail closed.

After a gate intent is durable, typed fail-closed refusals from repository
observation or rebuild planning are themselves settled as append-only red gate
receipts. Their fingerprint covers only the finding, an allowlisted bounded
diagnostic, and the exact problem/turn/base/HEAD/intent binding; exception
text, command output, temporary paths, timestamps, snapshots, and environment
data are excluded. Restart first reconciles an existing receipt without
re-observing. Exceptions without the typed `:finding` contract still surface
as programmer failures and never masquerade as gate results.

`{prompt-text}` begins with the exact contents of `standing-goal.md` as one
argv item. When the immediately preceding transition was a gate, the adapter
derives that gate's canonical intent id from durable state and appends a small
feedback record containing only problem, turn, intent, candidate HEAD,
outcome, finding, failure fingerprint, and registration outcome. It never
copies command output, repository snapshots, or environment data into the
prompt. A receipt whose problem, turn, intent, or HEAD does not match fails
closed; directory ordering cannot select feedback. Thus a red gate is visible
to the next persistent turn exactly once and cannot be replayed as a later
turn's authority.

`{prompt}` is also available when a vetted Codex wrapper accepts a file path.
To retain one Codex session, configure the argv with the explicit durable
session id, for example
`["codex" "exec" "resume" "SESSION-ID" "{prompt-text}"]`; never use a global
`--last` selector in an unattended multi-run environment.

Before a qualification begins in a fresh apm-lean worktree, install the pinned
Mathlib binary cache with the repository toolchain and manifest unchanged:

```sh
lake exe cache get
```

This populates `.lake` only. Verify `lean-toolchain`, `lake-manifest.json`, and
`git status --porcelain=v1` before and after; cache installation is not a gate
retry and must not change tracked source.

## Operate one transition at a time

```sh
scripts/library-loop status t00J02
scripts/library-loop resume t00J02
```

`resume` performs at most one turn or gate. At checkpoint cadence it returns
`:checkpoint-required`; it never spins or approves its own work.

Write the structured checkpoint EDN and emit the independent-review request:

```sh
scripts/library-loop checkpoint t00J02 /absolute/strategy-01.edn
```

The request appears under `RUN-DIR/review-requests/`. A different reviewer
writes a review EDN containing the exact request digest, obligation id,
`:ruling`, nonempty `:rationale`, and `:approved?`. Apply both files:

```sh
scripts/library-loop apply-review t00J02 \
  /absolute/strategy-01.edn /absolute/review-01.edn
```

Only an exact state-bound approval opens banking:

```sh
scripts/library-loop bank t00J02
```

Banking rebuilds the base-to-candidate ConstructionTargets closure and problem
`Main.lean`, verifies clean worktrees and ancestry, and executes only
`git merge --ff-only CANDIDATE` in the configured trunk. It never pushes.

## Interrupted turns

`codex exec` has no reliable external job query in this adapter. If the runner
dies while `:turn-running` and no durable turn receipt exists, `resume` does
not launch another turn. It records `:turn-observation-unavailable` and pauses.
An operator must inspect the Codex output/worktree and record an explicit
recovery disposition; Git cleanliness is never treated as completion.
