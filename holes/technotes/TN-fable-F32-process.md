# TN: F32 (process) — how a diagnosed defect sat untouched for eight hours, and the self-heal loop that replaces waiting

Author: Claude (Fable 5, `claude-10`), 2026-08-25. Companion:
`TN-fable-F32-model.md` (what the defect is, why the Lean-validated machine
contained it, what closes the class). This note is about the *night*: what
happened between the failure at 22:23 and Joe's message at 07:0x, why the
on-duty agent and its babysitter produced 48 alerts and zero progress, and
what the on-duty loop must be instead. Sources: `*codex-repl:codex-10*`
lines 13380–end, `scripts/apm-campaign-babysit.py` (repo and running copy),
the Agency roster, coordinator failure log.

Campaign `jit-all-open-nontopology-v1`, frame `f32`, problem `a92J01`.

**Short answer.** codex-10 had correctly diagnosed a design defect in the
apparatus by ~23:05 (an LLM asked to transcribe controller-minted UUIDs; see
the model note) and then did nothing about it for eight hours, waiting for an
authorization it could have obtained by belling a Claude agent — which is all
Joe does when he needs help. The babysitter belled it **48 times** from an
identity (`claude-cli`) that is not on the roster, so its 48 identical
replies went nowhere; it never reads replies anyway. No message was
addressed to Joe; there is no channel for one. **The required property is
self-healing: diagnose → fix the apparatus on master → reload → resume the
frame from its last valid receipt.** Voiding the frame and moving on is *not*
the policy (Joe, 2026-08-25: "it would just lead to more errors").

---

## 1. Timeline (2026-08-24/25, local)

| when | what | evidence |
|---|---|---|
| ~18:15–22:20 | F32 minted under the new seat regime; solver round; Student attempt 1 runs | transcript 13380–13440 |
| attempt 1 | Student finishes **without** calling the typed submission tool → `:typed-submission-missing`; the frame's one repair attempt is spent dispatching a repair turn | submission `:attempt-ordinal 2, :phase :student-attempt-2` |
| repair turn | agent hand-copies all 21 accessible IDs into `surfaced-ids`; one character wrong | `data/apm-role-submissions/apm-role-5cffdf18….edn` (mtime 22:23:07) |
| 22:23:23 | tick 9841: `:live-job-terminal-repair-exhausted`, findings `[:student-memory-surfaced-outside-snapshot]` | coordinator.edn failure #25 |
| 22:23–23:05 | codex-10 assumes this is the same *class* of fault as F30's 16:31 failure (a real validator gap it had fixed with bbc04b0b / eb9c5846 / 77a72e92) and **reloads namespaces and replays** — failure #26 at tick 9842, identical | failure #26 `:repair/reason "reload canonical FTS receipt accounting; revalidate persisted F32 Student terminal without redispatch"` |
| ~23:05 | codex-10 reads the submission and finds the one-character diff. Correct diagnosis: "progress requires either an append-only correction transition or operator authorization to void F32" | transcript 13493–13500 |
| 23:04 → 07:04 | babysitter bells codex-10 every ~20 min, **two bells per cycle** (`regulator-not-running` at :04/:24/:44, `coordinator-heartbeat-stale` at :08/:28/:48). 48 alert turns, each answered "deterministic; awaiting operator"; 0 operator-facing lines | transcript 13456–13918; `grep -c "⟲ following job"` = 48; `grep -c "^codex: \|^joe: "` = 1 (Joe's morning message) |
| 07:0x | Joe: "We wasted a whole night waiting apparently?" | transcript 13918 |

Joe's last instruction before the night (13442, "Solutions need to be pulled
into master") was answered at ~22:20. Everything after that happened with Joe
away and with no message addressed to Joe.

---

## 2. What failed on the process side

### 2.1 The on-duty agent treated "fix the design" as needing permission

codex-10's own assessment (13920): "I stopped F32 … and kept waiting for
explicit authorization instead of escalating to you directly … I failed to
convert that into a timely operator decision." Two things are wrong with the
frame of that sentence. First, the fix was not an operator decision: it was
an apparatus defect, which is the agent's job to repair on master, with tests,
and reload. Second, where a second opinion *was* wanted, the agent had one
available — any idle Claude on the roster — and never asked for it. It
answered the siren 48 times and asked nobody anything.

Also: forty minutes (22:23–23:05) went on the wrong hypothesis because the
finding *name* matched F30's genuine validator bug from 16:31. It reloaded
and replayed before reading the submission.

### 2.2 The babysitter is a siren, not a supervisor

`scripts/apm-campaign-babysit.py` (running copy is the older scratchpad
version at `/tmp/claude-1000/…/scratchpad/apm-campaign-babysit.py`, pid
2611780; the repo copy differs only in env-var plumbing):

- It bells `--from claude-cli`. **`claude-cli` is not on the roster**
  (`GET /api/alpha/agents` → no such id). Per the CLAUDE.md corollary of
  2026-07-16, a bell to a non-recipient is accepted and fails
  asynchronously. So codex-10's 48 replies were delivered to nobody — and
  the babysitter **never reads a reply at all**; there is no code path that
  does. It cannot learn "this is diagnosed and deterministic".
- Two incident keys fire for one condition (`regulator-not-running` because
  status is `:failed`, `coordinator-heartbeat-stale` because a failed
  regulator stops heartbeating), each on its own 20-minute cooldown, 4 min
  apart. Hence 48 bells for one fact.
- The bell text presupposes the fault class: "Please investigate
  (exceptions, deadlocks, resource throttling) and repair … without a human
  re-prompting it each time." It does not include the last failure entry it
  has already parsed (`c['last_failure']`), and it has no branch for "the
  answer is: the apparatus needs a fix".
- Cooldown, no backoff, no escalation. The 48th bell is identical to the
  1st.

### 2.3 No escalation path exists

There is no `joe`/operator identity on the roster; `grep -rE
"ntfy|pushover|notify-send|sendmail|PushNotification"` over `scripts/` and
`src/` finds nothing operator-facing. That is fine — Joe should not be in the
overnight loop — but nothing *else* was in the loop either. The only
interlocutor codex-10 had was a script that could not hear it.

---

## 3. The self-heal loop

This is the whole of the process change; 3.1–3.3 below are just where it is
written down. The standing loop for the on-duty agent:

1. **Diagnose to a fault class** — transient / code defect / apparatus
   (contract, prompt, budget) defect / genuinely-open design question — and
   write the class and evidence to `data/apm-campaigns/<c>/hold.edn` before
   anything else.
2. **Transient** → restart once. **Code or apparatus defect** → fix it on
   master with tests, kondo and check-parens; reload from
   `/home/joe/code/futon3c`; resume the frame from its last valid receipt
   (model note §3.3). This is the ordinary case and it is *the agent's job*.
3. **Open design question** → bell an idle Claude agent and ask for help,
   exactly as Joe does. "Awaiting operator authorization" is not a state the
   overnight machine may enter; a second agent's review *is* the
   authorization. Roster check first; park on the job-id; act on the reply.
4. **Never replay** a persisted terminal that failed deterministically
   without first changing something.
5. **Read the actual submission** before assuming the previous frame's
   fault class.
6. **A fix shipped at 3 a.m. gets the same gate as one shipped at 3 p.m.** —
   a second agent reads the diff before it is reloaded into the shared JVM.
   The F30 widening at 16:31–18:13 went in under alert pressure with no
   reviewer.
7. **A frame is never voided as a way out.**

### 3.1 Babysitter: a reply path and a stop condition

- Bell from a registered id, and **read the hold file**: the babysitter
  already polls `coordinator.edn` every 20 s; it also polls `hold.edn`. A
  hold with `:class :code-defect|:apparatus-defect` means "agent is
  repairing — stop belling this incident key until the coordinator changes".
  A hold with `:class :design-question` and no Claude bell job-id recorded
  after one cooldown means the babysitter itself bells an idle Claude with
  the hold contents (it should never need to, but it closes the loop if the
  Codex stalls the way codex-10 did).
- Collapse the two keys: `:failed` implies stale heartbeat; one incident.
- Include `last_failure` (`:error/code`, `:findings`, `:repair/attempts`)
  in the bell body, and replace the "deadlocks, resource throttling" guess
  with the loop above.
- Backoff after the second identical incident; hard cap per incident.

### 3.2 Escalation target is a Claude agent, not Joe

On-duty Codex → an idle `claude-N` on the roster, with a bell that carries
the hold file, the diff under consideration, and a concrete question. The
Claude reviews or decides; the Codex fixes and resumes. The two-agent loop
*is* the authorization. A one-line morning report per hold in
`holes/OPERATOR-INBOX.md` (what broke, what was fixed, which sha, which
Claude reviewed) is informational only; nothing waits on Joe reading it.

### 3.3 Where the loop lives

`holes/NIGHT-SHIFT-POLICY.md` containing §3 verbatim, and the babysitter's
bell text pointing at it. Nothing more.

---

## 4. What I checked

- Read `*codex-repl:codex-10*` 13380–13962 in full; grepped every
  `invoke-<ms>` id and converted to local time for the alert timeline;
  counted alert turns vs operator-facing turns in 13456–13918 (48 vs 1);
  located the F30 incident at 12387 (16:31) to separate its failures from
  F32's.
- `coordinator.edn`: status `:failed`, ticks 9842, updated-at 23:05:26Z; 26
  failures; #25 (22:23:23) and #26 (23:05) are F32's.
- Babysitter: config (`FROM_ID="claude-cli"`, `TO_ID="codex-10"`,
  `POLL_S=20`, `COORD_STALE_S=180`, `BELL_COOLDOWN_S=1200`), `maybe_bell`
  cooldown logic, the `:failed`/stale branches (`:326–392`); diffed the
  running scratchpad copy against the repo copy; confirmed it is still
  running (pid 2611780) with the watch-projection child.
- Roster: `claude-cli` absent, `joe`/operator absent, codex-10 `idle` since
  07:09.
- Escalation-channel grep over `scripts/` and `src/`: nothing.
