# TN-fable-F25-review — why F25 ended "partial" and how to keep F26+ from doing the same

Author: Claude Fable 5 (review seat), 2026-08-23.
Status: recommendation; no code changed.

## 1. What F25 actually produced

"Partial" applies to the learning frame, not the mathematical problem.

| Layer | Outcome |
|---|---|
| Problem (m94A02) | **Solved.** Clean, axiom-acceptable Lean proof from the Codex Solver. |
| Solver-memory | **Succeeded.** Scribe proposed memories; independent Promotion Proctor approved two; exact content-addressed snapshot created. |
| Student attempt 1 | **Incomplete.** Student accessed the snapshot and worked on the proof, but its final response was conversational text, not the typed receipt. |
| Frame | **Partial.** With no Student receipt the controller could not certify the Student phase or advance through Guide / Student×2 / Scribe / Close. |

Demonstrated end-to-end: solver proof → Scribe extraction → independent review →
frozen snapshot admission → Student dispatch bound to the snapshot → Student
retrieval and attempted use.

Not certified: the Student's exact outcome and failure account; whether each
admitted memory was useful / misleading / unused; Guide interventions and the
two remaining Student attempts; the Scribe comparison across trajectories;
terminal closure and Analyst input.

The Student's ordinary job record contains useful narrative (it read the
memories and described concrete proof obstacles). The model correctly refuses
to reinterpret conversation as a receipt, so that narrative can inform a human
postmortem but cannot count as learning-loop data.

Decision (Joe, 2026-08-23): **do not re-run F25.** 200 problems remain; the
learning loop can be revisited later after a successful Codex solve. The
question is the design, so F26+ do better than "done partial".

## 2. Where "partial" comes from in the code

- `src/futon3c/apm/live_job_driver.clj:196-207` — job reaches `:done` with no
  typed submission → `:typed-submission-missing` → exactly one repair dispatch
  (`live_learning_phases.clj:262 terminal-repair-request`) → if that fails too,
  `:live-job-terminal-repair-exhausted`.
- `src/futon3c/apm/series_terminal.clj:70-80` — a failure in any
  `:student-attempt-N` phase can only close the frame as `:partial`
  (reasons `:promotion-review-apparatus-invalid`,
  `:student-dispatch-apparatus-invalid`). There is **no path where the frame
  closes with the learning loop merely unobserved.** One missing Student
  receipt therefore kills Guide, Student×2, Scribe-reduce, Close, and the frame.
- `src/futon3c/apm/live_learning_phases.clj:228-260 prompt` — the Student
  prompt is `pr-str` of the whole request, then "attempt the problem
  independently", then — at the very end — a two-step shell ritual (run
  template command → fill every null → run submit command). A Student that
  spends its context on Lean naturally ends its turn before that step. The
  repair dispatch asks for the same thing in the same shape, so it fails the
  same way.
- `src/futon3c/apm/generated_contract.clj:8,45` — `:student-attempts 3` is
  hardcoded into the contract; every frame carries the full learning loop.

Net: the learning loop's measurement failure is conflated with frame
completion, and "no measurement" is not representable as a value.

## 3. Recommended design changes (priority order)

### 3.1 Decouple frame closure from learning observation  (stops the bleed)

The frame terminal should carry two independent results:

- `:problem/outcome` — `:solved | :partial | :invalid` (exists; driven by the
  Lean verdict and the verify receipt).
- `:learning/outcome` — **new**: `:observed | :partially-observed |
  :unobserved | :skipped`.

A frame whose proof is verified and banked closes `:closed` regardless of the
learning result. The bank advances on the Lean verdict alone; the learning loop
is a trailer, never a gate. Touch point: `series_terminal.clj` partial rule and
the `:frame-terminal` receipt shape in `queued_frame_terminal.clj`.

### 3.2 Make "receipt missing" a typed receipt, not an error

When repair is exhausted, the controller **synthesizes** a `:student-attempt`
receipt: `{:outcome :unobserved :evidence {:job-id … :reason
:typed-submission-missing :repair-attempts n}}`, content-addressed like any
other receipt. `frame_cycle_handlers` can then certify the phase; Guide and
Scribe phases run against what exists; the Scribe comparison says "attempt 1
unobserved" honestly. Attach the Student transcript as `:evidence/uncertified`
— useful to humans, never counted.

### 3.3 Move the submit step out of the prompt and into the harness

Submission today depends on the agent remembering a shell command at the end
of a long turn. Instead:

- The job runner (the wrapper around `scripts/apm-submit-role.py`) checks for
  the submission record when the agent's turn ends. If absent, it re-prompts
  the **same session** with one line — "Call the submit command now; nothing
  else." — context intact, no re-solve. Allow 2–3 of these; they are cheap.
  Reserve the fresh-session repair dispatch for contract migration only.
- Pre-fill the JSON template server-side with every authority field
  (`:receipt-id`, `:snapshot-id`, `:snapshot-digest`, frame/problem/job ids).
  The Student fills only `:outcome`, `:failure-account`, `:used-ids`. Fewer
  nulls, fewer malformed receipts.

### 3.4 Derive what can be derived — a `:harness-observed` receipt tier

The system is Lean-backed, so most of the receipt is objectively checkable:

| Field | Source |
|---|---|
| `:outcome` | Lean check of the Student workspace head (`:workspace/terminal-heads` already exists) |
| `:surfaced-ids`, `:queries` | memory-retrieval log, server side |
| `:used-ids`, `:failure-account` | genuinely subjective — Student only |

Build a second receipt tier from the derivable fields. Then "unobserved"
becomes rare (you always get outcome + surfaced set), and "Student claims X
about memories but the harness shows Y" becomes detectable rather than
invisible.

### 3.5 Give the Student a hard stop budget

"Stop proving after N compile cycles / T minutes and submit whatever you
have." Without it the Student runs to context exhaustion and the turn ends
wherever the context died — which is exactly F25's failure.

### 3.6 Make learning phases per-block configurable

Replace the hardcoded `:student-attempts 3` with a per-block setting. For the
200-problem run default to 0 (or 1) and run the full loop only on frames you
pick. Combined with 3.1 that is a `:learning/outcome :skipped` close, not a
partial.

## 4. Suggested order of work

1. **3.1 + 3.2** — controller-only, two small Codex handoffs
   (a) `series_terminal` partial rule + `:learning/outcome` field;
   (b) synthesized `:unobserved` receipt in `live_learning_phases` on
   repair-exhausted. These guarantee F26+ close on the Lean verdict.
2. **3.6** — config flip; can be done immediately.
3. **3.3 / 3.4 / 3.5** — quality of the learning measurement, for when the
   loop is switched back on.

## 5. F25 disposition

Leave F25 recorded as `partial` with reason `student-receipt-missing`; do not
backfill. The snapshot is content-addressed, so a later F25b bound to the same
snapshot hash is well-defined if the learning experiment is ever resumed.
