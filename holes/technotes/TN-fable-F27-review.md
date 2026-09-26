# TN: F27 / M-apm-demonstration review — why runs still need repairs

Author: Claude (Fable 5), 2026-08-23. Read-only review of futon3c `master`
at 57f6e4f5, prompted by Joe: "I've validated, run, tested, and banked and we
are still seeing irregularities — any advice?"

## Method

Mined the ~150 APM commits of 2026-08-17..23 (`git log -- src/futon3c/apm
scripts/library-lane-run.sh`) and read the bodies of the ones with repair
verbs. The week's repairs are not a scatter of unrelated bugs: they fall into
four clusters, and in three of them the *same* defect is repaired at a
different site each time. Evidence below is by commit sha so it can be
checked.

Context numbers: 634 commits in 7 days repo-wide, ~112 with repair verbs
(Qualify / Fix / Enforce / Repair / Re-pin / Recover). Registry holds
`jit-m94A03-retry-v1/-v2/-v3` — same problem, same revision `9b57394`, same
blob, each relaunched as a new coordinator id rather than the one coordinator
recovering.

## Cluster 1 — "How does a job actually get run?" (5 commits + 1 revert, 25 h)

- `67010bc7` (08-22 13:34): `/api/alpha/invoke/activate` 404s, "the per-agent
  drainer dispatches from that queue", stop calling activate.
- `f981f441` (08-22 14:14): the drainer does NOT adopt announced jobs; every
  announced frame job has sat queued forever since 67010bc7. `/activate`
  existed only on branch d6f9ec2c; :7070 runs master.
- `d915cce0` (08-22 14:48): third theory; reverted one minute later by
  `cbb431dd`.
- `c04b5773`, `f17976fa`, `5d8769cd` (08-23): reconcile the fallout in the
  durable layer (lost activation receipt, unaccepted cancellation, awaiting
  terminal jobs).

Eight APM files call these routes directly:
`live_proof_phases` (6 sites), `live_preflight_runtime` (5),
`live_learning_phases` (4), `live_promotion` (3), `library_lane_phases` (2),
`conductor_open`, `countdown_control`, `frame18_control` (1 each). The four
tests that mention `announce` mock it; nothing exercises the real route
table. Each driver therefore encodes its author's *belief* about
announce/invoke semantics, and the beliefs disagree. This is the cluster
behind the "job orphaned 45+ minutes" stalls.

**Fix (highest priority — this is what stops runs completing):**
1. One contract test against the real `futon3c.transport.http` handler (no
   mock): announce → state `queued`, nothing runs; `/invoke` with the same
   job-id → runs; `GET /invoke/jobs/<id>` → state sequence.
2. One function (home: `live_preflight_runtime`) that does
   announce+invoke+poll; the other ~21 direct call sites go through it.
   Semantics are already recorded in memory as "announce reserves, /invoke
   runs" — the code must stop re-deriving them per file.

## Cluster 2 — "Relative to which checkout?" (~9 commits)

`cc753705`, `7d9af67f`, `c899f291`, `a640348a`, `6ef41088`, `b7b36218`,
`b95dff66`, `30995c8e`, `11a2f940`. Every one resolves a path or revision
against the wrong root: control checkout vs. campaign root vs. qualification
worktree vs. the solver's apm-lean worktree.

`b95dff66` is the purest case and the most dangerous: the solver was told to
read its role card at a control-repo-relative path; its workspace has no
`holes/`; it proceeded *silently* without its contract. This does not stall
the run — it degrades the evidence. For the whitepaper, silent failures of
this kind matter more than stalls.

**Fix:**
1. One resolver: `{:control-root :campaign-root :workspace}` × path-kind →
   absolute path. Grep `src/futon3c/apm/` for `(str root "/`, `io/file`,
   `fs/path` and route them through it.
2. Before any dispatch, `Files/exists` every path that will be handed to an
   agent; refuse the dispatch if one is missing. Converts the silent mode
   into a loud one.

## Cluster 3 — "What's a valid preflight baseline?" (4 commits)

`2cabe328`, `204c7cdf`, `58085cfd`, `1e4aba80`. The validator required
exactly `{:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}` because that is
what the hand-built countdown fixture produced. t00J02 has one sorry plus a
`simpa` linter hint → warnings 2 → ruled `:blocked` by a style lint. Each
commit widened the acceptance for the next real problem; there will be a
fifth.

**Fix:** run the validator over the whole corpus once, offline
(`lake env lean` each `problems/*/lean/Main.lean`, collect
`(exit warnings sorry-warnings errors)`), and write the accepted set from the
distribution rather than from the next failing problem.

## Cluster 4 — step-machine vs. driver semantics (converging; leave alone)

`225f9eda`, `fe0f89a0`, `1f7dc5a0`, `cc8439cf`, `57f6e4f5`. `drive!` is a
durable step machine returning `:awaiting-terminal` after one step; callers
treated that as "phase done". Today's durable-coordinator work (intents bound
to phase, `postcondition-satisfied?`, `:library-lane-phase-intent-drift`) is
the right shape of fix. Not recommending changes here.

## Shared-JVM note

`30995c8e` / `11a2f940` are the 2026-08-23 "one JVM per repo, pin to
qualified master" policy arriving in code. But `16058df7 Recover library
coordinator in the shared JVM` shows a stale `futon3c.apm.*` namespace was
still live. `scripts/restore-http-routes.sh` reloads only `transport.http`;
extend it to reload `futon3c.apm.*`, since those are what this week's
reloads actually clobbered.

## Summary

Not many bugs: three boundaries without a contract (Agency job lifecycle,
checkout roots, Lean baseline), each re-learned by every new file that
touches it. Fix the boundaries, not the files. Cluster 1 is what makes runs
complete; cluster 2 is what makes completed runs trustworthy.

Things I suggested and withdrew: a commit freeze to collect N comparable runs
(Joe: impossible — repairs are needed to complete a run at all). The
clusters above are what the repair log says instead.

## Addendum — how we got here (the Lean model vs. the leaks)

`mathlib4-apm-validation/DarkTower/APMCycleMachine.lean` has 37 theorems; 24
are named after an incident or mutant (`f25_reused_student_session_refused`,
`stale_base_cannot_substitute_for_terminal_head`,
`three_sorries_are_a_valid_nonvacuous_preflight_baseline`, …). It is a
regression suite in theorem form — the same reactive pattern as the Clojure
guards. It was grown by incidents, not derived from requirements, so it
verifies the failures that already happened.

The model is about **rulings given observations**. `DispatchObservation`
(11 fields) and `validDispatch` are correct — but nothing says how
`activatedJobId` is obtained from the world (cluster 1), the model has no
notion of a filesystem root (cluster 2), and `validPreflightSorryBaseline`
is a predicate over three Nats with no theory of how a Lean toolchain
produces them (cluster 3). The second validation was half: decision layer
verified, observation layer left to the shell. Every leak this week was in
the unmodelled half.

"Loop through problems, solve, share with a student" hides three effects:
*solve* (external process via a job lifecycle whose semantics live in
another repo), *in order* (durable resume across JVM reload — now done by
the durable coordinator), *share* (cross-agent memory through futon1b with
its own failure modes). The phase loop is ten lines; the effects are the
system.

### Proposed `futon3/library/cycle-machine` content

Not the phase loop (exists twice already). The **effect ports**, each with
one contract and one contract test against the real counterpart:

1. `job-port` — announce / invoke / poll-terminal against the real
   `transport.http` handler; the *sole* producer of `DispatchObservation`.
2. `authority-port` — `{control-root campaign-root workspace}` × path-kind →
   absolute path; existence check before dispatch.
3. `toolchain-port` — Lean file → `(exit warnings sorry-warnings errors)`;
   accepted baseline derived from the corpus distribution.
4. `step-machine` — the durable coordinator, already written.

Principle for the library header: **every structure the verified model
consumes has exactly one producer, and that producer has a contract test
against the real counterpart.** This week is what happens when a verified
structure has eight producers and zero contract tests. Incident question
becomes "which port lied?" (converges) rather than "which guard is
missing?" (does not).
