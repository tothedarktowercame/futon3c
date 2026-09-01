# Codex 17 watcher-efficiency review

Joe requested this review on 2026-09-01 after an `apm-watcher-codex` bell
occupied Codex 17 for more than 25 minutes while an operator turn waited behind
it.  The question is not whether each local repair was defensible.  It is why
ordinary campaign progress generates so much agent activity, and which small
structural changes would stop it.

## Verdict

The projection watcher does not currently distinguish "the machine needs an
agent" from "the machine has not reached the next stable projection yet."
It has acquired representations for some legitimate waits, but the bell
contract still demands `:healthy`, the babysitter does not treat `:waiting` as
a completed healthy observation, and a two-poll alert can turn a normal
transition into an open-ended repair turn.

That creates a costly loop:

1. a transitional projection is classified as an alert;
2. the babysitter bells Codex with "repair ... and verify ... healthy";
3. Codex follows the transition until a stable state rather than returning
   after classifying it;
4. while waiting, Codex finds another observer mismatch and modifies the
   watcher;
5. tests, commits, live reloads, coordinator resumes, and more polling all
   occur inside the same bell turn;
6. the next previously unseen transition repeats the process.

The result is locally tested watcher code but poor end-to-end operation.  A
watcher invocation has no latency bound, a normal wait is not a successful
terminal outcome for the caller, and repair authority is broader in practice
than the written watcher contract.

## Evidence from the Codex 17 buffer

At 18:43 UTC the buffer contained 494,793 characters and 139 watcher turns.
Textual activity records in that buffer included:

| record | occurrences |
|---|---:|
| watcher turn started | 139 |
| shell tool call | 1,351 |
| explicit `sleep N` | 139 |
| `apm-watch-projection.sh --once` | 234 |
| `proof-eval.sh` | 92 |
| focused Clojure test invocation | 45 |
| clj-kondo invocation | 32 |
| Git commit command | 16 |
| durable coordinator resume reference | 11 |
| final `:watch/status :healthy` report | 94 |
| final `:watch/status :waiting` report | 2 |
| "No repair was needed" | 6 |

These are buffer occurrences, not billing-quality execution metrics, but the
ratio is unambiguous: the watcher path performs much more polling and repair
work than classification work.  Ninety-four healthy conclusions required 234
explicit projection checks and 139 sleeps.

The turn that prompted this review began at 18:13:29 and was still running at
18:43.  Its repeated command was a 50-second sleep followed by file/projection
inspection.  During the same turn Codex:

- recognized a declared substrate backoff and committed `0df3f3f0`;
- found the canonical checkout on `codex/persona-registry-aliases`, stashed two
  edits, switched it to `master`, and resumed the coordinator;
- changed guide retry observation and committed `d7277df1`;
- changed heartbeat treatment during retry and committed `1c8f3bd5`;
- repeatedly waited for a visibility operation reported to take about five
  minutes.

This was not one repair taking 30 minutes.  It was state classification,
source repair, checkout administration, live loading, coordinator control,
retry backoff, and workflow monitoring collapsed into one request.

The churn is recent as well as cumulative.  Since 2026-08-25, the watchdog,
babysitter, or shell wrapper has been touched by many commits for individual
observed states: terminal recognition, heartbeat wakeups, scheduled transport
retries, campaign pinning, fault deduplication, memory cascades, substrate
backoff, nested guide retry, and retry-window heartbeat treatment.  The
watchdog source alone gained 140 lines and lost 28 over that interval.  These
changes describe missing state-machine coverage one incident at a time.

## Where the present control contracts disagree

### `:waiting` exists but is not accepted end to end

`projection-watchdog/evaluate` can now return `:waiting` for a transport
retry, substrate wait, or bounded cascade.  But:

- `scripts/apm-watch-projection.sh --once` exits successfully only when its
  output contains `:watch/status :healthy`;
- the babysitter has explicit branches for `:alert` and `:healthy`, but none
  for `:waiting`;
- the repair bell explicitly says the command "should report
  `:watch/status :healthy`."

Consequently the data model says a declared wait is legitimate while the
caller still treats it as unfinished.  Codex then remains attached through the
deadline and subsequent phase transition to manufacture a `:healthy` result.

### The alert threshold is unrelated to the operation's declared budget

The frame watcher polls every ten seconds.  The babysitter bells after the
same reason appears twice.  This gives a transition roughly twenty seconds to
settle even when its durable operation declares a multi-minute deadline.

There are special-case suppressions for particular combinations such as a
reachable role job inside its timeout.  Each new legitimate operation shape
therefore requires another watcher exception.  The invariant should instead
be general: a declared operation or retry is progressing until its own
deadline passes without the progress evidence required by its contract.

### One bell combines incompatible jobs

The generated packet asks the recipient to investigate, repair, keep the
campaign progressing, and prove a globally healthy watcher result.  That
combines four different completion conditions:

- classify the observation;
- repair code;
- operate the live coordinator;
- wait for the campaign to reach another stable projection.

There is no point at which the recipient is required to stop after satisfying
the original finding.  This is why a small false alert grows into a long
sequence of opportunistic repairs.

### Written authority and observed authority differ

`TN-apm-watcher.md` permits read-only inspection, dispatching a fix, fixing
watcher instruments, and restarting the babysitter.  It explicitly forbids
hand-resuming the regulator and touching workspaces.  The Codex 17 history
contains coordinator resumes, a branch switch, a stash, live namespace
reloads, and source commits within watcher-generated turns.

Some of those changes repaired real faults, but the discrepancy means the
system cannot determine from the job type which mutations are possible.  A
review of an individual diff cannot repair that authority ambiguity.

## Recommended changes

The first three changes are the smallest coherent repair.  Applying only a
new suppression for the latest f78 state would continue the existing pattern.

### 1. Make `:waiting` an operationally successful terminal result

A `:waiting` observation must include durable evidence: wait kind, wake or
deadline, attempt/budget where applicable, and the state artifact from which
it was derived.  When that evidence is valid:

- `apm-watch-projection.sh --once` exits zero;
- the babysitter clears pending alert debounce state;
- an existing alert incident is closed as "classified waiting," not retained
  until a later `:healthy` sample;
- a bell recipient reports the classification and returns immediately.

This does not weaken liveness.  A later watcher tick must alert if the recorded
deadline passes without the required state change.  It removes agent waiting
before the deadline while preserving detection after it.

Acceptance tests should cover transport retry, substrate wait, bounded memory
cascade, and a live role job.  Each must produce zero bells before its durable
deadline and one alert after a deliberately expired deadline with no progress.

### 2. Replace two-poll staleness with contract-derived incident predicates

Do not infer a failure merely because a transition or coordinator file is 120
seconds old.  For every active state, derive one of:

- a job deadline;
- a retry wake time;
- a bounded-operation deadline;
- a next-tick/heartbeat obligation when no longer operation exists.

Bell only when the applicable obligation is violated.  A failed durable last
result can alert immediately; an active declared operation cannot alert for
generic staleness before its deadline.  Unknown state shapes should produce a
single `:watch-contract-unknown` incident rather than being guessed healthy or
expanded into several evolving finding codes.

The test should replay the recorded f75--f78 state sequences through the pure
watchdog evaluator.  Valid sequences must emit no incident.  Injected stopped,
expired, mismatched, and unreachable states must each emit one stable incident
identity.

### 3. Give a watcher bell one bounded completion condition

Change the packet from "repair until this frame is healthy" to:

> Resolve incident ID X.  Return after (a) showing it is a valid declared
> wait, (b) committing and validating a repair that eliminates finding X on
> the same captured observation, or (c) recording a typed invariant conflict.
> Do not wait for the next phase.

The packet should include the captured observation and finding identity.  Its
acceptance check must replay that observation, not demand whatever happens to
be globally healthy several minutes later.  A source repair and a live
coordinator recovery are separate durable actions; neither silently expands
into the other.

Set and enforce a watcher-response budget.  Five minutes is sufficient for
classification and a focused observer repair; exceeding it should end with a
durable incident report, not another sleep.  A repair that genuinely requires
longer remains visible as repair work without occupying the original watcher
call.

### 4. Enforce mutation authority by job type

Watcher-classification jobs should be read-only except for their incident
artifact.  Observer-repair jobs may edit, test, and commit the watcher but may
not switch the canonical checkout, stash unrelated work, or resume the
coordinator.  Coordinator recovery must be performed through its declared
recovery transition and receipt, with the preconditions in
`TN-apm-watcher.md` enforced by code rather than prose.

Routing watcher work to a different agent would keep Joe's REPL responsive,
but it would only hide the excess work.  Apply the semantic and authority
changes first; routing is useful afterward for operator responsiveness.

## Measures that should become gates

Record these per frame and fail the operational qualification when their
budgets are exceeded:

- watcher polls and bells;
- distinct incident identities;
- time from first finding to classification;
- agent-active time spent waiting for a future deadline;
- live reloads and coordinator recovery actions;
- observer source changes made while a campaign is active;
- phase wall time separated into declared compute, declared waiting, repair,
  and unclassified delay.

Initial acceptance for a clean frame should be: zero bells, zero agent-active
waiting, zero live repairs, and one bounded watcher observation per sampling
interval.  A fault-injected frame should produce one incident, one bounded
classification turn, and either one validated repair or one typed stop.

## Review conclusion

Codex 17 is not slow because shell polling itself is computationally
expensive.  It is slow because the watcher asks an interactive agent to remain
responsible until the whole machine returns to a globally healthy projection,
even when the machine has already recorded a legitimate wait.  The repeated
watchdog fixes are evidence that the observer is learning the production
state machine from live incidents rather than being qualified against its
state sequences.

Accepting evidence-backed `:waiting`, deriving incidents from declared
deadlines, and bounding each bell to one captured finding would remove most of
the visible activity without weakening any workflow invariant.  They also
make subsequent reviews meaningful: the reviewer can test a finite incident
contract instead of approving another special case in an open-ended loop.

## Repair outcome (2026-09-01)

The live recovery exposed two coordinator/watchdog lifecycle races beyond the
original watcher-noise problem.

First, after a repair resume, the semantic watchdog treated the previous
generation's `:live-supervisor-launch-audit-failed` result as a current fatal
result even though the resumed generation held a valid reconciliation tick
claim.  It disabled the coordinator at its first ten-second observation.  The
watchdog now lets that claimed reconciliation settle under its declared
deadline; a repeated failure still halts after the claim settles.  This is
commit `fb8e2ce5`.

Second, an older watchdog thread could survive a coordinator stop/resume and
call its captured stop function against the new registry entry.  Watchdog
identity alone was insufficient because it did not establish authority over
the current coordinator generation.  Each armed watchdog now captures the
registry entry digest and revalidates both the digest and enabled state before
observation and again immediately before a durable stop.  A stale watcher
returns `:durable-coordinator-watchdog-authority-superseded` instead of
mutating the new generation.  This is commit `648da15b`.

Both repairs have focused regression tests.  The durable-coordinator tests
pass with 27 tests and 168 assertions; the semantic-progress-watchdog tests
pass with 13 tests and 35 assertions.  `clj-kondo` and the workspace
parenthesis checker also pass.  After live recovery, coordinator epoch 135
remained enabled across the watchdog interval and durably completed successive
ticks, demonstrating that the stale watcher can no longer stop current work.
