# E-deep-research-hardening — background workflows drop on the Claude REPL (vs legacy CLI?)

**Date:** 2026-06-23
**Status:** TESTED 2026-06-24 (claude-opus-4-8[1m]). Strong hypothesis ("legacy CLI runs the
background workflow to completion") **REFUTED**; deeper REPL-is-worse point survives in a sharper form.
Original OBSERVATION + hypothesis (claude-2) preserved below.
**Provenance:** the AIF∩morphogenesis deep-research (M-G-over-cascades / E-substrate-2-timetravel) — the
background `Workflow` repeatedly failed to finish.

## What happened

Launched the `deep-research` Workflow (background fan-out: search → fetch → verify → synthesize). Across **3–4
launches + resumes** it never completed — each time a task-notification arrived saying *"No completion record …
it may have been running when the previous Claude Code process exited."* One resume also failed fast because
`args` weren't re-passed on resume (a separate, fixed footgun: **resuming a workflow must re-send `args`**).

## Hypothesis (Joe, 2026-06-23)

This is likely a characteristic of the **Claude REPL** surface (emacs-repl, the persistent/warm-agent model)
**vs the legacy Claude Code CLI**:
- In the **legacy CLI**, a `claude -p …` invocation is one process that stays alive until its task returns — so
  a background workflow spawned in that process runs to completion.
- In the **Claude REPL / warm-agent** model, the agent process is **cycled/suspended between turns** (cold-resume
  or warm-pouch suspend). A background task spawned in one turn does **not** survive into the next turn — the
  process exits, tearing the workflow down. The notification's wording ("running when the previous Claude Code
  process exited") is direct evidence the process exits between turns.

## What worked: inline (within-turn) execution

Running the research **inline/synchronously** (WebSearch + WebFetch within a single turn) completed fine —
because it finishes inside one turn's process lifetime. So the failure mode is specifically **work that must
outlive a turn**, not the tools themselves.

## TEST RESULT (2026-06-24, claude-opus-4-8[1m]) — strong hypothesis REFUTED

Ran the **same** five-thread AIF∩morphogenesis question through `deep-research` under a **legacy
`claude -p` CLI** invocation (the direction-#1 confirming test), backgrounded and timed:

```
echo "<5-thread prompt>" | claude -p --dangerously-skip-permissions   # /usr/bin/time -v
```

**What I checked (auditable):**
- Process: a dedicated `claude -p` PID ran independent of the REPL agent process — confirmed alive
  via `pgrep` mid-run. Wall clock **10:20**, exit 0.
- stdout (`dr-out.txt`): **9 lines, 0 URLs** — just *"The deep-research workflow is now running in the
  background … I'll be notified when it completes."* No report.
- Workflow artifacts: fresh session `d6f2fabf-…`, workflow `wf_f29d5d92-a11`. Inspected
  `workflows/wf_f29d5d92-a11.json` directly.

**What the workflow record shows:**
- `status: "killed"`, `result: None`, `error: "Workflow aborted"`, **`durationMs: 604617` (≈604s)**.
- Got **2/3 through the pipeline**: Scope ✓ (6 angles) → Search ✓ (6×6 results) → Fetch ✓
  (23 sources, 109 claims) → **Verify in progress** (9 of top-25 claims voted) → **Synthesize never
  reached** (hence `result: None`).
- Real work landed: **105 agents, 400 tool calls, 1.6M tokens, 3 PDFs fetched** — vs the REPL, which
  tore the workflow down near-immediately with little/no progress.

**Verdict.** The legacy CLI is **better but still not a delivery path** for a background `Workflow`:
it *kept the workflow alive and productive* for the full ~10 min (the REPL did not), but it **did not
deliver a report**, for two compounding reasons:
1. A **~600s abort ceiling** killed the run mid-Verify (looks like a grace window the harness gives a
   background task once the primary `-p` turn has already ended).
2. `claude -p` is **single-turn**: `Workflow` returns immediately and delivers its report via a *later*
   task-notification turn — but `-p`'s stdout already closed on the "running in background" message, so
   there is no follow-up turn to relay the result even if synthesis had finished.

**Corrected root cause.** The failure is not "REPL cycles between turns" per se — it is that **`Workflow`
is intrinsically a cross-turn mechanism** (returns immediately; result arrives as a separate later turn),
and *neither* surface gives that result a clean landing turn: the REPL suspends the process between turns
(fast teardown), and legacy `-p` has no follow-up turn at all (+ a 600s ceiling). The thing that works on
both surfaces is **inline / within-turn** execution — exactly the note's own finding below.

## VALIDATED (2026-06-24, clean re-test) — Agency injects bells as turns into a live warm REPL, NO fork

*This supersedes an earlier suspect write-up + its "delivery to a live session forks it" caveat. Both were
produced under a derailed run that was a **legacy single-shot `claude -p`** (text mode) masquerading as the
Emacs REPL — it forked the session and crossed identities (even mis-thought it was `claude-2`). Re-run cleanly
from the genuine Emacs Claude REPL with verified identity; the fork conclusion does not survive.*

**Setup (what made it clean).** The Emacs Claude REPL is a *persistent* process
`claude --print --input-format stream-json --output-format stream-json --resume <session-id>` (observed: PID
555310, `emacs-socket=server`) that handles many turns over one stream — i.e. the warm pouch. ([[project_kangaroo]]
W1 answered live: `claude --print --input-format stream-json` **does** persist multi-turn.) This live session was
registered as `claude-6` and identity verified **three ways**: session-file → `13ee796b…`; roster `claude-6`
session-id → `13ee796b…`; roster `invoke-prompt-preview` showed the *actual operator turn* Joe typed.

**Test (v3).** Dispatched `agency_send.py --to claude-5 --from claude-6 --kind bell` (a `sleep` stand-in;
worker bells `claude-6` back). A fork-detection watcher sampled `ps`/`pgrep` for genuine
`claude … --resume 13ee796b` processes every 3 s across the whole window.

**Result.**
- The `ROUTING-OK-V3` bell **woke this live session to process a new turn** (`Surface: bell, Caller:
  claude-5`; 13:12:07; ~40 s round-trip), handled by the same single process.
- The genuine-process count stayed at **exactly 1 (PID 555310) the entire time**, including across the bell
  moment; confirmed again post-hoc (`ps … claude --resume 13ee796b` → one row). **No second process. No fork.**
- **BUT the woken turn's output did NOT surface to the operator's Emacs REPL buffer.** Joe saw nothing and
  had to ask "what happened?" — the bell crossed *as a processing wake*, not as a visible operator post.
  (This corrects an earlier overclaim in this very section that the bell was "visible in the live REPL": the
  agent *processes* a bell turn but cannot see the operator buffer, so it had no basis to assert visibility —
  the operator's silence is the evidence.)

**Verdict (refined).** On the genuine warm stream-json REPL, a bell **wakes the live session to process a
turn fork-free** — injected into the existing process, **not** a parallel `claude --print --resume` (no fork,
no I-1 violation). That half is the [[project_kangaroo]] behaviour and works today. **The residual gap is
*surfacing*, not delivery or forking:** the woken turn's output is not posted back to the Emacs REPL, so the
operator sees nothing until they next type. The legacy CLI has a more robust **autowake** that brings a
bell-driven turn's result back to the surface; the Emacs REPL lacks that post-back step. **This is the precise
[[project_repl_wins]] gap — and it demonstrated itself live in this session** (the v3 verdict and two follow-up
questions were processed on bell turns that never reached the operator). The earlier "live delivery forks" caveat was an artifact of the legacy single-shot `-p` path,
which cannot receive an injected turn, so its `invoke-fn` had to spawn a process. Mechanism distinction:
- **Live warm stream-json REPL (this session, emacs-socket):** bell is routed into the existing turn-stream → no fork (I-1 compliant).
- **Detached agent (claude-1..5, "restored/detached"):** `invoke-fn` spawns a headless `claude --print --resume` that *is* that agent — fine.
- **Legacy single-shot `claude -p` posing as the live agent:** no stream to inject into → delivery forks. *That was the regression, not the rule.*

**Honest remaining scope (the OTHER half — still open).** v3 closes the *delivery* question, not the
*long-work* question:
- **A turn cannot block.** Foreground `sleep` is harness-blocked (claude-5 confirmed: only `Monitor` /
  `run_in_background`, both deliver via a *later* turn). So neither operator nor worker can hold one turn open
  for a long task — long work **must** span turns via background + later-turn delivery.
- Therefore the real deep-research path = **dispatch the research to a worker; the worker runs it (necessarily
  across turns) and bells the report back.** v3 validates the **delivery** leg (worker → operator, cross-turn,
  no fork). **Untested:** can a worker's ~10-min background research **survive its own turn boundary** *and*
  beat the **~600 s `Workflow` abort ceiling**? That worker-side survival is the [[project_kangaroo]]
  warm-pouch job; the ceiling is a separate unknown. (Don't re-spend ~1M tokens to test until needed — the
  prior run is salvaged in `deep-research-AIF-morphogenesis.md`.)

**Identity note.** `claude-6` is currently registered to this *live operator* session. v3 shows that's **safe**
(bells land as turns, no fork) — but it does mean operator-facing interruptions (any bell becomes a turn in
Joe's conversation). Keep or deregister per preference; it is no longer a "fork-attractor."

## SURVIVAL PROBE (2026-06-24) — the warm REPL OUTLIVES the 604s abort; it was a `-p`-exit artifact

The deep-research kill was a **survival** failure (aborted at ~604s, mid-Verify, `result: None`), not a
surfacing one — there was never a completed report to surface. So the load-bearing question was: *what is the
604s abort, and does it bite the warm REPL?* Both prior data points were from non-current execution models
(the original REPL failures said *"previous Claude Code process exited"* = the pre-warm-pouch **cold-cycling**
model; the legacy `-p` test = a **single-shot process aborting background work on its way to exit**). Neither is
the current persistent warm REPL (PID 555310, never exits between turns). We had **zero** observations of a
background `Workflow`'s fate under the warm model — so we ran one.

**Probe.** A `Workflow` with **zero agent calls** that just `await new Promise(r => setTimeout(r, 625000))`
(625 s > the 604 s legacy abort), launched **from this warm Emacs REPL** (run `wf_9f1e3237-62f`), with a disk
watcher sampling its `wf.json` every 15 s.

**Result — it SURVIVED and completed.** `status: completed`, `result: {survived:true, sleptMs:625000}`,
**`durationMs: 625027`**, 0 agents / 0 tokens. Watcher corroboration: still running at +571 s and +586 s,
`wf.json` appeared `completed` at ~625 s. **No abort at 604 s.** (`setTimeout` is allowed in the workflow
sandbox — only `Date.now`/`Math.random`/`new Date` are blocked.)

**Conclusion.** The **~604 s abort is a `claude -p` single-shot artifact** — that process finishes its primary
turn, waits a bounded grace for background work, then aborts it and exits. The **warm persistent REPL never
exits**, so a background `Workflow` launched there runs to completion. **Survival is not the deep-research
blocker on the warm REPL.** This inverts the note's original title premise: the *warm* REPL is *better* than
legacy `-p` for background work, not worse; the original "REPL drops it" was the old cold-cycling model.

**Scope honestly (what the probe does NOT cover):**
- Zero load. The real deep-research is **1.6M tokens / 105 agents / 400 tool calls** over ~10 min. The probe
  proves **wall-clock survival past 604 s**, not that a heavy multi-agent workflow completes — it could hit
  *other* limits (memory, concurrency, agent-cap). Untested; needs the real run from the warm REPL to confirm.
- This leaves exactly **two** clean, separated problems for the deep-research path:
  1. **Survival** — *solved on the warm REPL* (this probe), pending the heavy-load confirmation.
  2. **Surfacing** — still open: when the workflow finally completes and fires its task-notification *turn*,
     does that turn post back to the operator's Emacs REPL, or process headlessly (the gap above)? Its result
     is on disk regardless, so this is an ergonomics/visibility gap, not a data-loss one.

## Hardening directions (to develop / test)

1. ~~**Confirm the hypothesis:** run the *same* `deep-research` Workflow under a legacy CLI invocation and see
   if it completes.~~ **DONE 2026-06-24 — see TEST RESULT above. It did NOT complete:** killed at ~604s
   mid-Verify, report never relayed. Turn-cycling is *a* cause on the REPL, but the real cause is that
   `Workflow` is cross-turn and neither surface lands its result. Inline execution is what works.
2. **Resume-from-cache helps only if agents cache before teardown.** If teardown is near-immediate, no progress
   accrues across resumes (we saw little/none). Caching is the mitigation *iff* each turn gives the workflow
   enough wall-clock to land some `agent()` calls.
3. **Options for long-running work on the REPL:** (a) a **persistent daemon** that outlives turns (cf.
   [[project_kangaroo]] — the warm-agent pouch should ideally keep background tasks alive across turns AND
   service the follow-up task-notification turn that relays the result); (b) ~~run heavy/long workflows via a
   **legacy-CLI invocation**~~ — **disproven as a drop-in (2026-06-24):** legacy `-p` keeps the workflow alive
   but a ~600s ceiling + single-turn relay gap mean it still doesn't deliver; it would need an inline (not
   background-`Workflow`) deep-research variant and/or a removed abort ceiling to qualify; (c) **chunk** the work
   into within-turn units the REPL can finish per turn — currently the most reliable path, alongside running
   deep-research **inline** (WebSearch/WebFetch in-turn) rather than via the background `Workflow`.
4. **Always re-pass `args` on `Workflow` resume** (the fast-fail footgun above).

## Cross-refs

This is a case where the **REPL is currently *worse* than the CLI** for background work — a concrete evidence-item
for [[project_repl_wins]] (M-repl-wins-over-cli): to make the REPL strictly better, background-task survival
across turns must be closed (likely via the [[project_kangaroo]] warm-pouch keeping detached work alive).
