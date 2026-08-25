# TN — APM launch countdown

**Date:** 2026-08-20  
**Status:** pre-launch operating proposal; decisions below are pinned where
marked and otherwise remain gates to clear  
**Scope:** transition from interactive burn-in to approximately ten deliberate
interactive frames, followed by a resumable fifty-problem autonomous campaign

## Launch thesis

The burn-in has established that APM can close real Lean problems and can expose
defects in its own instruments. The next objective is not simply to run more
frames. It is to demonstrate cumulative, independently witnessed learning while
moving routine orchestration and verification away from immortal Claude
sessions and onto bounded Codex work.

The immediate cast should be conservative. Zai GLM-5.2 remains the measured
student because that arm is central to the research programme. Zai GLM-5.3 is
not yet promoted into Guide, Scribe, or Ground Control merely because those
roles are technically reachable. Its present value is different and unusually
useful: as a fresh operator it reverse-engineers underspecified commands and
thereby reveals assumptions that Claude's accumulated memory silently supplies.

Accordingly, GLM-5.3 should first be used as a **tester of the apparatus**, not
as an inhabitant of the measured frame.

## Evidence input now in flight

Let the current `*zai-repl:zai-3*` Ground Control run finish without redirecting
it. Its transcript is evidence about:

- which instructions a fresh capable agent can execute directly;
- which operational steps it has to reconstruct from source;
- where it chooses an unsafe or incoherent interpretation;
- how many turns and tool calls each ambiguity costs;
- which explanations, commands, response shapes, and stop conditions are
  missing from the Ground Control card;
- whether its eventual execution is correct despite the extra search.

Do not evaluate this run solely by whether frame 16 succeeds. A successful run
that required extensive reverse engineering is evidence that the interface is
underspecified. A failed action followed by a correct diagnosis can be more
valuable than a fluent action supported by private conversational memory.

After the run closes, extract an evidence table with one row per hesitation or
wrong turn:

| Field | Meaning |
|-------|---------|
| task | What Zai was trying to accomplish |
| card text available | What the role card actually said |
| action taken | Command, search, or inference Zai chose |
| missing contract | Information that would have made the action direct |
| consequence | Extra turns, failed command, unsafe action, or harmless delay |
| correction source | Joe, code inspection, machine refusal, or independent check |
| v3 disposition | Card change, command change, test, or deliberately retained research challenge |

That table, not recollection of the session, is the input to Ground Control v3.
Do not draft v3 before the run has finished; doing so would erase the last part
of the usability trace.

## Cast for the next interactive phase

The initial default is:

| Function | Agent | Status |
|----------|-------|--------|
| Ground Control | bounded Codex agent, likely Terra/high | proposed default; validate in first two frames |
| Solver | Codex Sol Max (`gpt-5.6-sol`, reasoning `max`) | pinned |
| Student | Zai GLM-5.2, fresh session | pinned research arm |
| Proctor | Codex, one batched gate dispatch | activate; previously declared but unused |
| Guide | Codex initially | proposed; keep bounded and separate from Solver |
| Scribe/reviewer | Codex initially, different agent from Solver | proposed; author must not review own memory |
| Analyst | fresh Codex, tenure at most two frames | proposed |
| Apparatus tester | Zai GLM-5.3, outside the measured frame | pinned initial use |
| Exceptional referee | fresh bounded Claude | only by sampling, disagreement, or operator request |

This is intentionally not a full Zai recast. The observed need for guidance is
part of the research question, but putting the same uncertain operator into
Ground Control, Guide, and Scribe at once confounds three questions and enlarges
the blast radius. Change one role at a time after its interface has survived a
fresh-agent test.

Ground Control should probably move to Codex because the role combines codebase
navigation, shell work, exact state tracking, review, and stop-line discipline.
The hypothesis still needs measurement: use a fresh bounded Codex Ground Control
for one frame, then another fresh Codex Ground Control for the next. Do not
replace the immortal Claude session with an immortal Codex session.

## GLM-5.3 apparatus-tester contract

The tester is a naïve but capable consumer of programmatic commands. It does not
conduct, solve, guide, review, or write measured frame data.

Give it one bounded task at a time, such as:

- open a disposable frame from a fixture registration;
- mint a declared cast and report the effective cast;
- dispatch a mock job and park on it;
- resume after a synthetic completion;
- run the complete close gate against a fixture;
- generate a successor registration and trigger each stale-token guard;
- distinguish a reload-safe `load-file` from a restart-required change;
- recover from deliberately malformed, missing, or contradictory responses.

The task packet should name the desired outcome but should not explain hidden
implementation details. The point is to discover whether the command surface
does. Every test runs with:

- a fresh Zai session;
- fixture or disposable ids that cannot be confused with a live frame;
- no access to solver or student worktrees from a measured cycle;
- no memory channel carrying current problem content;
- explicit cleanup and postcondition checks;
- a turn/tool count and a transcript retained as usability evidence.

Findings feed documentation, error messages, command design, and tests at the
next regime boundary. They do not cause mid-frame harness changes. A tester
finding may stop a launch, but it may not silently repair the live instrument.

Success means more than eventual completion. Record:

1. time and turns to first correct command;
2. number of source-code searches required;
3. rejected commands and whether the rejection was explanatory;
4. unstated values the tester had to infer;
5. whether the tester verified the serving path independently;
6. whether a second fresh tester can complete the revised task directly.

This use of Zai replaces an inefficient pattern in which Claude's long private
history compensates for an unclear interface. The resulting commands should be
usable by any fresh agent, including future autonomous conductors.

## Ten interactive frames before autonomy

Treat the next ten frames as a countdown, not ten more burn-in repetitions.

### Countdown 10–9: finish the Ground Control interface

- Complete the Zai-3 run and evidence table.
- Produce Ground Control v3 from observed gaps.
- Run every v3 command through a fresh GLM-5.3 apparatus test.
- Put Ground Control on a fresh bounded Codex agent for two frames.
- Activate the Codex proctor as one batched verification job per frame.

Exit criterion: a second fresh Codex Ground Control can open, run, and close a
frame without searching HTTP handlers or reconstructing command syntax from
source.

### Countdown 8–7: establish the student counterfactual

For the same sound problem, run two fresh GLM-5.2 student attempts:

- treatment: reviewed prior-frame memories enabled;
- control: memory channel disabled.

Randomize arm order. Use isolated sessions, worktrees, and Git object reachability.
Neither arm may receive a solver commit, a commit SHA in a memory, or direct
guidance. This prevents the frame-9 artifact-transfer contamination from being
misreported as learning.

Exit criterion: both arms produce complete traces and the comparison can be
reconstructed from frozen receipts without agent testimony.

### Countdown 6–5: establish the learning chain

Require at least one fully witnessed chain:

```text
prior problem failure or proof step
  → candidate memory
  → independent review
  → reviewed attachment
  → eligible on a later problem
  → retrieved
  → explicitly USED or IGNORED
  → treatment/control behavioral comparison
```

Do not call production, promotion, eligibility, retrieval, or use alone
"learning." The claimed effect is downstream behavior on a later problem.

Exit criterion: at least one positive chain or one fully instrumented negative
result in which a reviewed relevant memory was eligible but failed at a named
later link.

### Countdown 4–3: unattended single frames

Joe authorizes the problem pool and registration, then does not intervene unless
a stop rule fires. The Codex Ground Control may execute preregistered transitions
but may not reinterpret a failed invariant, repair a frozen problem, change the
harness/store mid-frame, approve its own authored work, or retry a void frame
under the same id.

Exit criterion: two consecutive frames resume correctly across every park/wake
boundary and close without discretionary operator repair.

### Countdown 2–1: two five-problem rehearsals

Run two sequential blocks of five. Store changes become visible only between
problems, never between paired student arms. Checkpoint after every transition
and produce a block report automatically.

Exit criterion: both blocks recover from process/session interruption, preserve
the frozen comparison, and stop correctly on injected failures.

## Fifty-problem campaign shape

Run ten sequential blocks of five problems. Sequentiality is load-bearing: the
campaign is meant to test whether reviewed experience from earlier problems
helps on later ones. Fifty simultaneous jobs would demonstrate throughput, not
cumulative learning.

For each problem:

1. Select from a pre-approved manifest.
2. Run an independent statement-soundness probe.
3. Freeze problem, harness revision, store revision, role-card hashes, cast, and
   provider model attribution.
4. Let Codex Sol Max establish the ground-truth solution or exact obstruction.
5. Run the paired fresh GLM-5.2 student arms.
6. Have Codex Proctor run the full verification and contamination audit in one
   batch.
7. Mine both successful and failed trajectories into candidate memories.
8. Have an agent other than the depositor approve, reassign, or reject each
   candidate.
9. Close and publish receipts.
10. Admit approved memories to the snapshot for the next problem only.

At least one problem per block should be a preregistered holdout. The report
must distinguish general harness/model improvement from effects attributable to
the reviewed memory channel.

## Primary learning evidence

Report each layer separately:

1. candidate memory produced;
2. independently reviewed;
3. available to the executed retrieval path;
4. retrieved after ranking/cutoff;
5. dispositioned `USED` or `IGNORED` for every surfaced id;
6. followed by a behavioral difference;
7. generalized to a later non-identical problem;
8. accumulated across ordered blocks.

Primary behavioral measures:

- axiom-clean closure;
- residual `sorry` count;
- attempts or elapsed time to first compiled boundary artifact;
- exact obstruction count and severity;
- memory-on minus memory-off result within the same problem;
- reviewed-memory conversion rate from production through later beneficial use.

Raw offer counts are diagnostic, not a learning outcome. Repeated offers of one
memory count once at the memory level.

## Stop rules for autonomous operation

Stop the affected frame on:

- possible statement defect;
- cast, roster, readiness, or provider-attribution disagreement;
- missing or failed durable receipt;
- malformed trace;
- harness and store both varying inside one comparison;
- author equal to reviewer;
- unreviewed memory becoming retrievable;
- direct traffic to the student;
- commit/artifact leakage between arms;
- unexpected axioms or failed direct compile;
- three bounded student attempts without improvement;
- off-conductor action;
- repeated-tool/stuck guard without recovery;
- runtime coherence disagreement.

Stop the campaign at the next checkpoint on:

- a review backlog larger than one block;
- failure to reconstruct a treatment/control comparison;
- quota exhaustion that would change a pinned model;
- the same infrastructure failure in three consecutive frames;
- any proposal to change what the campaign measures.

A defective problem is marked `VOID`, retains its instrument findings, and is
replaced from the manifest. It is never silently repaired and counted as the
same trial.

## Claude boundary

No default APM seat and no standing APM orchestration session should require
Claude during this countdown. Claude remains available as a fresh bounded
referee for:

- a genuine Codex/reviewer disagreement;
- a sampled blind audit, initially no more than one per five interactive frames
  and one per ten autonomous problems;
- a proposed measurement or protocol change;
- final review of the campaign's learning claim.

This is not a claim that Claude lacks value. It reserves Claude for judgments
where vendor diversity or demonstrated comparative strength supplies evidence,
rather than spending its long context on shell work and remembered command
syntax.

## Launch gates

The fifty-problem campaign is not authorized until all are green:

- [ ] Zai-3 Ground Control transcript complete and evidence table reviewed.
- [ ] Ground Control v3 frozen and hashable.
- [ ] Every v3 programmatic command passed by a second fresh GLM-5.3 tester.
- [ ] Two bounded Codex Ground Control frames completed.
- [ ] Codex Proctor used in batched mode and its report independently checked.
- [ ] Paired GLM-5.2 memory-on/off student arms run without contamination.
- [ ] Retrieval availability is recorded non-tautologically at recall time.
- [ ] Per-id surfaced-memory disposition is complete.
- [ ] Provider model attribution is durable for every seat.
- [ ] Author/reviewer separation is machine-checked.
- [ ] Store and harness revisions are frozen per comparison.
- [ ] Stop rules have been mutation/injection tested.
- [ ] Two five-problem rehearsals complete from durable checkpoints.
- [ ] Block report distinguishes production, review, retrieval, use, and effect.
- [ ] Joe explicitly authorizes the fifty-problem manifest and quota envelope.

## Immediate next move

Do nothing to the live Zai-3 run. When it finishes:

1. export the complete transcript;
2. build the hesitation/wrong-turn evidence table;
3. compare it with Ground Control v2 line by line;
4. draft Ground Control v3 from those observed gaps;
5. give the v3 commands, without hidden implementation hints, to a fresh
   GLM-5.3 apparatus tester;
6. only then seat a fresh Codex Ground Control for the first countdown frame.
