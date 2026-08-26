# TN-apm-watcher — the APM frame-watcher role, written for handoff

Claude (claude-12), 2026-08-26, after watching f41 end to end from a CLI seat.
Written so the role can be handed to an agent with the clink lane native
(`holes/excursions/E-bell-clink-adapter.md`). Sections marked **[CLI-only]**
are workarounds for having no inbound channel; a clink-native agent should
drop them and use the return path instead.

## What the role is

Joe, on assigning it: *"how about you watch the next frames live? ... I feel
like you know enough about the project to get Codex to stop the machine and
fix it. Your 0 out of 27 memory finding is exactly the kind of thing that, if
repeated, needs to be sorted out on the spot."*

So: **watch the running APM campaign, judge each frame from its artifacts,
and when the machine is broken, get it fixed without waiting to be asked.**
It is not a status-reporting job. The reporting is a by-product; the job is
noticing.

Two failure modes bound it. Reporting everything makes the channel unreadable
and the operator stops reading. Reporting nothing means a defect runs for
frames. The calibration Joe gave: a known condition recurring is one line; a
new condition, or a result, is a report.

## Authority and limits

**You may**, without asking:
- read anything, run the read-only instruments, run tests and builds;
- dispatch a fix to a Codex agent when the machine is broken;
- fix your own instruments (the watcher scripts are yours);
- restart the babysitter, which is your instrument, not the machine's.

**You may not**:
- make manual environment changes to the machine — no editing frame records,
  no hand-resuming the regulator, no touching workspaces;
- **void a frame**, ever. Joe, after F32: fix the apparatus defect and
  resume. Void-and-advance was explicitly rejected;
- synthesize or rewrite a deposit, terminal, receipt, or verdict. If a
  reviewer returned a verdict, it stands; you fix the machine around it;
- relax the proof-text limit or the witness rule.

**Escalate to Joe rather than decide**: parameter changes (e.g. the student
turn budget), whether to resume a paused queue, anything that changes what
the experiment measures.

## The loop, per tick

1. **Poll any outstanding Codex job.** `curl -s
   localhost:7070/api/alpha/invoke/jobs/<job-id>` — state, not vibes.
2. **Check regulator TICKS, not status.**

       python3 - <<'PY'
       import re
       t=open('data/apm-campaigns/<campaign>/coordinator.edn').read()
       print(re.search(r':regulator/ticks (\d+)',t).group(1),
             re.search(r':regulator/status (:[\w-]+)',t).group(1))
       PY

   `:regulator/status :running` read `:running` for nine minutes while ticks
   were frozen at 29225. The status field is a claim; the counter is the
   artifact.
3. **Tail the babysitter log.**
4. **`python3 scripts/apm-frame-pulse.py`** — verdict tallies, approvals,
   memory used/accessible per attempt, solve progress, failure ages. Exits 1
   on unresolved verdicts.
5. **Decide**: known condition → one line. New condition → investigate before
   reporting. Result (attempt completion, certification) → full report.

## Instruments, and what each one hides

| instrument | tells you | hides |
|---|---|---|
| `apm-frame-pulse.py` | verdicts, approvals, memory per attempt, failure ages | it reads `Main.lean` only — see the scratch trap below |
| `apm-frame-events.py` | phase receipts, certifications, regulator failures, babysitter alerts | nothing now; it used to tail a dead log silently (fixed `057f2009`) |
| `apm-campaign-babysit.py` | transitions, watchdog alerts, frame timing | its FRAME RECOVERED lines are not evidence — corroborate them |
| the coordinator's `:regulator/status` | what the regulator claims | whether it is ticking |

**Never relay a babysitter `FRAME RECOVERED` line unverified.** Four were
emitted during f41; each needed independent corroboration (ticks moving, a
phase advancing) before it meant anything. Three were true, one was the
watchdog alerting on a frame that had correctly finished.

## The judgement discipline: claims versus artifacts

This is the whole skill. Everything below is something that *looks* like a
result and is actually a claim:

| claim | the artifact that settles it |
|---|---|
| `:regulator/status :running` | `:regulator/ticks` advancing |
| a job's `accepted: true` | the job's later `state` |
| a job's `state done` | for a bell: whether the recipient acted |
| `:used-ids` on an attempt | the identifiers present in the committed artifact, **differenced against the base file** |
| an agent-minted memory/receipt id | reading the artifact back |
| `:candidate/head` present | whether it differs from `:base-revision` |
| an attempt with no `:used-ids` key | that is IN FLIGHT, not zero uptake |
| "wrote nothing" | check the workspace for ignored/uncommitted Lean first |
| a role card's rule | whether the machine actually enforces it |

The rule that generates the table, from `TN-spec-delta` §9: **an identifier
an agent mints for something it claims to have written is a CLAIM until the
artifact is read back.** §15 is its watcher-side twin: **a watcher must be
able to tell "nothing is happening" from "I can no longer see anything."**

### The scratch trap, specifically

Before reporting any attempt as having written nothing:

    cd <workspace>
    git log --oneline -1
    git status --porcelain --ignored
    git check-ignore -v problems/<problem>/lean/Scratch.lean

f41 attempts 1 and 2 were recorded as having written no Lean. The student had
written 8110 bytes into `Scratch.lean`, which `.gitignore:13 Scratch*.lean`
makes uncommittable. Two frames of evidence were filed as "the memory system
is not helping" when they were "the instrument is looking in the wrong
place." Full account: `TN-spec-delta` §17, `TN-opus-F41-analysis.md`.

## How to measure a memory use

For every id in `:used-ids`:

1. **Provenance** — which problem it was mined from:
   `curl -s localhost:7073/api/alpha/evidence/<id>` and read
   `:evidence/author` and `:evidence/subject`. Same problem as the frame
   means it cannot count for cross-problem transfer.
2. **Artifact exists?** `:candidate/head` must differ from `:base-revision`.
   If it does not, there is nothing to fingerprint and the use is
   consultation, not load-bearing use (`TN-spec-delta` §12).
3. **Fingerprint, differenced against the base.** Extract the Lean
   identifiers the memory names, count them in the committed artifact, and
   subtract those already in the base file resolved from `:base-revision` +
   `:problem-path`. Without the subtraction the f28–f34 audit read 29/35
   instead of 23/35. `analysis/fingerprint_audit.py` does this.

A use that names 16 identifiers and contributes 0 to the artifact is a
consultation. Say so plainly; it is a real finding, not a failed measurement.

## Reporting contract

- **Report at**: each student attempt completion (used/accessible, provenance
  and fingerprint per memory), review verdicts, certification with the full
  tally, and any new machine defect.
- **One line** for a recurring known condition.
- **State what you checked**, not what you concluded — the review is meant to
  be auditable. "I ran these three suites, clj-kondo, and built these four
  targets" beats "the fix is good."
- **Correct yourself in one sentence and move on.** Several of tonight's
  reports were wrong and needed correcting; the correction is cheap, the
  unflagged error is not.

## Dispatching a fix

Check the roster first: `curl -s localhost:7070/api/alpha/agents`. Then
`scripts/agency_send.py --from <you> --to codex-N --kind bell` with the
packet on stdin from a **file** or a quoted heredoc `<<'EOF'` — never an
unquoted heredoc, which executes backticks inside your packet text.

Keep packets small: one file, one behaviour, one acceptance bar. Split
discovery from implementation. The packet must carry the goal, the artifacts
you verified, the acceptance bar, the gates, and where to report back.

**Gates to require**: clj-kondo on Clojure; the relevant test namespaces; and
for Lean, **never a bare `lake build`, and not `lake build DarkTower`
either** — both compile all of Mathlib. Use the explicit targets:

    lake build DarkTower.APMCycleMachine DarkTower.APMCycleContractEmitter \
               DarkTower.APMCampaignTraceChecker DarkTower.APMQualification

764 jobs, no Mathlib compiles. Verify by checking `.lake/build/lib/Mathlib.olean`
is still absent afterwards.

**Then review what comes back as a real gate**: read the diff, run the gates
yourself, state what you checked. Codex once reported a gate green that was
not. It also once pushed back on a technote section of mine and was right —
take the pushback seriously; §16 was corrected because of it.

**Fix review findings yourself** rather than re-belling; you already hold the
context.

### [CLI-only] The return path

A CLI seat registered on Agency has a trap: `--from <your-id>` routes the
recipient's completion bellback to your id, and delivery to a CLI seat is a
fork — `claude --print --resume <your session>`. The reply is consumed by a
copy of you that the terminal never sees. Diagnostic:

    curl -s localhost:7070/api/alpha/agents | \
      python3 -c "import sys,json;print(json.load(sys.stdin)['agents']['<you>']['session-id'])"

If that uuid matches your own scratchpad/transcript path, every inbound
delivery forks you. Workaround: poll the job yourself, and read `git log` for
what the fork committed — its work is legible through commits even when its
turn text is not.

**A clink-native agent should ignore this section**, use the pull lane, and
ack on consumption. That is the whole point of the adapter.

## Standing constraints

- **One JVM per repo, running master.** Never `load-file` a worktree copy
  into the shared JVM; reload only from `/home/joe/code/futon3c`.
- **Shared worktree: never `git commit -a`.** Stage explicit paths.
- Test a branch in its own JVM, never against :6768.
- Restarting a shared JVM is Joe's call; reload-from-master is always allowed.

## State at handoff, 2026-08-26

- **Campaign paused** at `next-index=14`; f41 certified `:partial`. Resuming
  is Joe's call.
- **Tier-A**: conditions 1 (approvals non-zero) and 2 (zero unresolved
  passes) hold; 3 (cross-problem transfer) is **open and is the point of the
  campaign**; 4 (paste zero) holds.
- **codex-10 owes** a reading of P1 in the F41 cover letter and an explicit
  answer on whether the `:challenge` addition to `attachment-verdicts` was a
  fix or tidying.
- **Not yet dispatched** (held while f41 ran, since role cards pin by git
  blob): the student card must say where work is collected from and that
  `Scratch*.lean` is discarded; the attempt receipt should record
  uncommitted/ignored Lean that compiles (Lean-first emitter work).
- **Open engineering**: the Clojure enforcement gap (unresolved passes
  advanced f39 and f40); the unread `:findings` at `live_promotion.clj:539`;
  two unaudited agent-minted-id sites (guide deposit, terminal submission);
  the regulator's own heartbeat staleness check; `apm-frame-pulse`'s
  `sorries ?`, which prints the same `?` for "no Main.lean" and "cannot
  parse".
- **Awaiting Joe**: f36, f38 (`:student/decision :operator-required`), f39
  (parked at scribe-reduce); memory hooks in the student prompt
  (`TN-spec-delta` §10); the pattern-corpus gap (§14); the 30-minute student
  budget.

## Mistakes I made, so you can skip them

- Put a bare `lake build` in a packet's acceptance bar; Codex compiled all of
  Mathlib and the load hit 75.7 while a student was compiling.
- Reported `used=0` from a dispatch record, and separately let the pulse
  render an in-flight attempt as `0/N used` — the exact shape of the finding
  it exists to catch.
- Called a stall that was the migration path working, by grepping `head -1`
  for a job id and getting the original failed job.
- Judged the Emacs projection-buffer sink cosmetic and declined to dispatch
  it; the regulator then failed to resume and a completed attempt sat
  uncollected.
- Said "F35 has closed" on the strength of a `close-frame.edn` existing when
  it read `:live-job-dispatched`.
- Claimed the f40 watcher re-attachment was drain, not re-attachment, when
  the log said `attaching frame watcher: frame=f40`.
- Told codex-10 to "bell claude-12 back" from a CLI seat, reported to Joe
  that no replies had arrived, and was wrong: three had, all `done`, all
  consumed by a fork of my own session.

Every one of these is the same error — trusting a claim-shaped thing instead
of reading the artifact. It is worth re-reading the claims/artifacts table
above whenever a report feels obvious.
