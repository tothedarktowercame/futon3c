# APMV3 F200–F218 reliability audit

2026-09-11. Requested by Joe after deliberately stopping the loops and restarting
the server. **V2 is retired for this work and must remain disabled. V3 remains
disabled pending repair and qualification.** No coordinator, topology loop,
agent campaign, or shared service was started by this audit.

## Scope and evidence

`census.clj` read 224 ledger, problem-transition, and live EDN files across all
19 frames, including multiple EDN forms per ledger; all parsed. `census.edn`
contains file SHA256s, exact nested locations of retained error fields,
transitions, queue dispositions and existing adjudications. It also extracts
retained coordinator errors, with a source hash. Raw finding counts are NOT
incident counts: prior attempts are embedded repeatedly in terminal histories.
Proof sources and sealed evaluation contents were not read. Error fields inside
free-text reports/prompts are deliberately excluded. This is a retained-record
audit, not a claim that every transient event survived persistence.

Queue dispositions in this interval: 7 completed (5 partial, 2 closed), 5 parked,
6 statement-refuted/void, and F218 still active in its frame ledger but stopped
at the coordinator. Completed does not mean uninterrupted. A partial proof or
valid statement refutation is not an infrastructure failure.

Source inspection additionally covered the coordinator, promotion driver,
evidence transport, Guide validation, report parser, and Voxterm projection and
rendering. Historical adjudications were consulted for external job failures;
their detailed attribution is not independently re-established by replay here.

## Every frame

| Frame | Recorded disposition | Faults/observations and interpretation |
|---|---|---|
| F200 | Parked | Successor solver dispatched into a still-running session; existing adjudication identifies thread-writer conflict mislabelled session mismatch. 35 retained proof-invalid records are intermediate validation records, not 35 independent crashes. |
| F201 | Completed, partial | No selected error fields in the inspected live/ledger records. This is not proof of zero intervention. |
| F202 | Parked | Same overlap class as F200; failed reportless dispatches fed repeated-artifact/remediation logic. Three retained job-terminal-failure fields, with nested duplication possible. |
| F203 | Statement refuted | Legitimate statement outcome, followed by apparatus trouble repairing its slot. Coordinator history retains persisted-manifest mismatch: revised proof identity was compared against the original F203 manifest. Plan/state and repair-receipt failures also survive in coordinator history. |
| F204 | Completed, partial | Guide first failed mode-authority/candidates-outside-store-mode validation, then repaired. In sampled retained original submission, top-level `mode` was absent. |
| F205 | Statement refuted | Mathematical/statement disposition. Intermediate proof-invalid record alone does not establish an apparatus fault. |
| F206 | Parked | Student non-compilation was treated as candidate-preservation/terminal failure (`workspace-probe-failed`); this should be a measured Student outcome. Also Guide mode repair and missing-submission diagnostics. Solver success and subsequent learning-arm park are distinct. |
| F207 | Completed, closed | Guide mode repair despite eventual workflow success. Intermediate proof-invalid records precede closure. |
| F208 | Parked | Existing adjudication identifies provider quota outage consuming solver rounds and producing a strategy checkpoint. Retained job-terminal-failure records support dispatch failure, not mathematical exhaustion. |
| F209 | Parked | Existing adjudication identifies dead preflight job, poll/backoff vs watchdog deadline conflict, then manual pointer clearing and job identity collision. Ledger itself barely advanced; final park alone loses most of the sequence. |
| F210 | Statement refuted | Proof checker records mutation-outside-problem-file as well as proof-invalid. Preserve this authority violation separately from the valid refutation outcome; not enough here to blame a machine race. |
| F211 | Completed, partial | Guide mode repair and a retained solver-job-terminal-failure, despite eventual completion. Root cause of that individual job failure is not established by the selected record fields. |
| F212 | Statement refuted | Multiple incomplete proof/axiom checks, then refutation. No independent transport crash established in frame-local records. |
| F213 | Statement refuted | Frame ledger spans roughly six hours; elapsed time alone is not six hours of agent execution. Coordinator plan/watchdog faults cannot all be assigned to this frame solely by current-file timestamps. |
| F214 | Completed, partial | Snapshot visibility timeout: retained bounded retry attempt 0 failed, attempt 1 succeeded about 17m39s later. Concrete recovered infrastructure failure. |
| F215 | Completed, closed | Cascade unreachable, snapshot visibility unavailable (retry success about 17m56s later), and missing-submission diagnostics. Proof closure does not erase a degraded learning/retrieval measurement. |
| F216 | Statement refuted | Mutation-outside-problem-file finding alongside proof-invalid, then refutation and successor slot. Same distinction as F210. |
| F217 | Completed, partial | Guide mode repair plus report parser/linter failure. Linter message names an abbreviated hex identifier and backtick syntax from prose. It does not prove that the typed JSON receipt itself was malformed. |
| F218 | Operator-stopped, incomplete | Guide mode repair plus two apparatus repair attempts; parser/linter failure; candidate/review edge write transport failures; timed-out evidence read escaped a coordinator tick and eventually caused watchdog halt. Resume restored progress but another hyperedge write timed out at 00:42:08Z. Operator stopped at 00:44:21Z with promotion in bounded transport retry. |

## What is recurring, and what has already changed

1. **Job completion, submission collection and session release disagree.**
   F200/F202 directly demonstrate premature successor dispatch. Commits
   `076cbb6e` and `a6967bba` add the terminal wait and its provider on resumed
   paths; `78226361` changes classification; `d4ec8ee0` prevents reportless
   failures from counting as repeated proof artifacts. These are source repairs,
   not evidence that every post-restart path has been exercised successfully.

2. **Expected mathematical incompleteness and unavailable machinery share fault
   budgets.** F206 and F208 demonstrate different versions of this. Source
   repairs include `94a5c218` (preserve Student compile failures as observations)
   and `52f0aa75` (substrate build failure is not Student failure). A restarted
   loop must demonstrate both directions, not just accept a passing proof.

3. **The Guide contract repeatedly needs an avoidable repair turn.** F204, F206,
   F207, F211, F217 and F218 record the same mode findings. All six requests
   authorize store-mode. Sampled original payloads in F204/F217/F218 omit mode;
   F218's repaired payload includes `"mode":"store-mode"`. The validator
   correctly requires an explicit matching declaration; do not relax it.
   Audit the actual dispatch template, inherited role-card schema and submit
   helper together so a compliant first submission supplies required fields.

4. **Transport classification does not cover thrown failures consistently.**
   `live-promotion/drive!` classifies returned failure maps after `drive-step!`,
   but does not catch its thrown exceptions. `futon1b-backend/get-edn` explicitly
   throws typed `:futon1b-read-timeout`. F218's retained stop demonstrates such
   an exception reached the regulator. The exact call stack was not retained;
   identify and replay the failing read at that boundary before patching it.
   F214/F215 prove bounded retry can work. F218's later hyperedge error identifies
   `org.httpkit.client.TimeoutException: idle timeout: 30000ms`. Neither error
   establishes whether storage, queueing, resource pressure or networking caused
   the original delay. Increasing deadlines would conceal this uncertainty.

5. **Statement repair changes identities used by multiple durable records.**
   F203's old manifest vs new statement mismatch is concrete. Repairs
   `2364e217`, `970ba41a`, `47bc692d`, `7195f7d0`, and `dc9a95f2` address new
   frame identity, numbering, plan persistence, pins and current registered
   launch lookup. Coordinator history also retains invalid-state, invalid
   Guide-repair receipt and park-decision-read failures. Those need targeted
   replay and timestamp attribution; none should be silently dismissed because
   later frames completed.

6. **Watchdog lifecycle has its own defects and incomplete historical evidence.**
   Retained coordinator history includes rearm-limit-exceeded and watchdog
   repair-failed. The canonical checkout still has pre-existing uncommitted
   coordinator/tests changes addressing stale observer/current-registration
   mismatch. They were neither authored nor loaded by this audit. Review and
   commit ownership must be resolved before declaring a qualified runtime.

7. **Report-parser diagnostics can be mistaken for receipt corruption.**
   F217/F218 retain abbreviated-identifier and syntax-quote lint errors.
   `parse-report-diagnostic` falls back to strict linting when it cannot extract
   a map from text. A prose completion can therefore produce this error even
   alongside a typed submission. Replay selection between typed payload and
   completion text; never ask an agent to remove mathematical content merely
   because a prose fallback was parsed as EDN.

## Voxterm: reproduced incorrect activity claim

`display_probe.py` calls the actual `voxterm/server.py` projection against the
stopped campaign, with a readable empty job feed and network helpers disabled.
`display-observed.json` retains the result and source hashes. It reproduces
F218 `guide-intervention-1`, actor `in-process`, an increasing phase duration,
and `watchdog silent ... — loop supervisor gone` despite deliberate shutdown.

The exact mechanism:

- `_apm_frame_timeline` labels the last ledger phase current until a successor
  phase appears. It computes `now - phase_start`, including downtime.
- It selects `in-process` whenever the feed is readable and no matching job
  is running. No coordinator enablement or active tick is consulted.
- `apm_status` judges watchdog silence from its old observation. It does not
  consult the durable operator stop/quiescence record for this branch.
- Both collapsed and expanded `index.html` rendering have an `in-process`
  fallback. The expanded current phase remains green even when overall state
  is stopped/stalled.

Required semantics: separate unfinished phase, actual active execution,
scheduled retry, operator hold, and unknown observation. Report wall-clock
phase age as age, not runtime. An operator-stopped coordinator must not become
an unexplained silent-watchdog alarm. If a role is still draining, show it
explicitly rather than asserting whole-system quiescence. Genuine in-process
work requires positive tick/operation evidence. Do not merely freeze a bogus
timer or hide the warning.

The existing `test_apm_timeline_feed.py` and `test_apm_watchdog_halt_strip.py`
both pass on the current source. The former explicitly expects readable empty
feed => in-process. Thus this is a coverage/specification gap, not a failure
fixed by rerunning existing tests.

## Repair order and evidence required before V3 restart

1. Correct the durable lifecycle projection and both display branches. Exercise
   operator stop, restart while disabled, draining role, active internal tick,
   scheduled retry, unknown feed, genuine watchdog loss and closed frame.
2. Replay typed submission vs prose selection and the six Guide mode cases;
   fix the producer contract without weakening consumer validation.
3. Replay transport exceptions and returned failures at all promotion stages.
   Prove bounded retry, preservation of accepted work, and no role redispatch
   merely for publication retry. Investigate store-side timing/resource logs
   separately; there is no demonstrated permanent store-latency repair yet.
4. Re-run the job-ordering, Student measurement, quota, statement identity and
   watchdog regressions through the actual coordinator composition. Check
   canonical source/loaded identity after resolving existing dirty changes.
5. Only after review, authorize a V3 canary and measure frames without manual
   rescue, recovered failures, and legitimate mathematical dispositions
   separately. V2 is not part of the canary or any automatic resume manifest.

## Validation and limits

Audit script passes clj-kondo and the workspace parentheses check. All 224
selected frame EDN files parsed. Display diagnostic is offline and reproducible;
it is not a screenshot or live job-feed assertion. Existing display tests passed
while the new diagnostic reproduced the defect. No production code was changed,
no source was live-loaded, and no repairs or experiments were dispatched.
Both coordinator registrations were verified disabled after server restart.

This packet finishes the bounded historical/source audit. Remaining work is
explicit: exact attribution of retained campaign-level errors, store-side root
cause, composed fault replay, fixes and qualification. It does not certify that
the loop is ready to resume.
