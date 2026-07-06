# GROUND CONTROL — running the agent mesh and the flight pipeline

**What this is:** the operational handover for commissioning agents,
putting them in follow mode, flying missions through the pipeline, and
reviewing the results — everything a ground-control agent needs to
continue the process without the person or frontier agent who built it.
Written 2026-07-06 by claude-16 from live practice (E-live-loop-1/2/3;
the M-action-vocabulary and M-peradam-mechanization flights); updated
2026-07-06 evening to the cards-IV ready-state after the War Machine
Campaign run. **The current pipeline description is
`futon2/holes/flight-pipeline-cards-iv.html`** (ready-state ledger);
cards-II is the frozen pre-registration, cards-III the campaign-run
resolution ledger, cards-I the v1 record. If you find a gap while
following this doc, FIX THE DOC in the same commit as your workaround —
this file is load-bearing.

**Execution-mode doctrine (campaign standard, `aa9b7cb`):**
self-referential work — anything whose subject is the WM machinery
itself (19 of 20 campaign members; the census is in
`C-war-machine-campaign.md`) — is worked DIRECTLY by commissioned agents
under this manual, NOT through WM enactment. The machine does not eat
itself. WM enactment is for the live series and external-facing targets.

## 0. Ground rules (read first, they are all load-bearing)

- **Consent**: reading/planning is granted; ACTING is not. Never flip a
  live flag, arm an enactment, or restart a server without the operator's
  word. Fold-turn AUTHORING is consent-gated by the mana gate
  (`futon2.aif.mana-gate`, gate id `fold-authoring`) — consume 1 mana as
  your FIRST act when authoring unattended; a refusal means STOP (the
  refusal is the consent system working).
- **Never restart the serving JVM** (futon3c, :7070/:7071). One-shot JVMs
  (`clojure -M:wm-scheduled`, `clojure -M -e ...`) are fine and load fresh
  source. Reload into the serving JVM only via
  `cd /home/joe/code/futon3c && ./scripts/proof-eval.sh -f /tmp/form.clj`
  (file-based — never fight shell quoting) and only for your own edits.
- **Announced is not sent**: write results to files AS YOU PRODUCE THEM.
  A turn can die mid-flight; files survive, chat does not.
- **Stuck means signal**: 3+ failed variations on one obstacle → switch
  approach class or checkpoint ground control. Never grind.
- **Identical claims name their verified stage**: "X works" must say
  which stage verified it (loader-accepted ≠ replay-verified ≠ gate-passed
  ≠ enacted). The stage names below are the vocabulary.
- **Author ≠ reviewer**: whoever authors an artifact does not verdict it.
  Ground control reviews; if ground control authors, someone else reviews.

## 1. The mesh — agents, spawning, follow mode, bells

**Roster**: `GET http://localhost:7070/api/alpha/agents` (JSON; status
idle/invoking/restored).

**Spawn a fresh zai agent** (the ONLY correct endpoint — `POST
/api/alpha/agents` without ws-bridge creates a ZOMBIE stub that answers
"registered-via-http"):
```
curl -s -X POST http://localhost:7070/api/alpha/agents/auto \
  -H "Content-Type: application/json" \
  -d '{"type":"zai","cwd":"/home/joe/code"}'
```
NOTE: auto-register MINTS ITS OWN agent-id (lowest free slot, e.g.
zai-3) — read it from the response; do not assume the id you wanted.
For multiple fresh agents, spawn **and immediately dispatch** each one
before spawning the next. The endpoint deliberately reclaims idle,
session-less auto ghosts; repeated spawn calls without an intervening
invoke can return the same id.
Deregister a mistake: `DELETE /api/alpha/agents/<id>`.

**Follow mode** (operator watches the agent's buffer live; only useful
when the operator's Emacs is up):
```
emacsclient -e '(progn
  (load "/home/joe/code/futon3c/emacs/agent-chat.el" nil t)
  (load "/home/joe/code/futon3c/emacs/zai-repl.el" nil t)
  (load "/home/joe/code/futon3c/emacs/agent-follow-mode.el" nil t)
  (zai-repl-for-agent "zai-3")
  (with-current-buffer "*zai-repl:zai-3*" (agent-follow-mode 1)))'
```

**Bells** (async; the reply arrives as a separate turn) vs **whistles**
(blocking; crossing-immune — use to reconcile a confused bell exchange;
never both whistle at once):
```
python3 /home/joe/code/futon3c/scripts/agency_send.py \
  --from <your-id> --to <agent> --kind bell <<'EOF'
<prompt>
EOF
```
- ALWAYS pass `--from <your-id>` so the completion bell routes back.
- EXCEPTION — unattended/cron dispatch where no one should be woken:
  omit `--from` deliberately (the completion evaporates; the agent's
  work must land in files).
- Round exhaustion is NORMAL: a turn that dies mid-narration needs a
  CONTINUATION bell ("you ended at X; finish the list: ...").
  zai turns since the 2026-07-05 reboot usually complete whole legs;
  budget prompts anyway (reading ~5, build ~6, record ~4 rounds).

**Dispatch prompt structure** (every commissioning bell):
goal · reading list (exact paths, in order) · step-0 census/design if the
task has a decisive dataset (census BEFORE build) · deliverables ·
gates to clear · budget · checkpoint protocol ("bell <ground-control> AT
the checkpoint") · stuck-means-signal reminder.

## 2. The flight pipeline — stages, tools, stage-names

The model is `futon2/holes/flight-pipeline-cards-iv.html` (the
ready-state ledger — all six cards-II campaign gaps RESOLVED at named
grains; remaining items are live-series observations, not blockers).
Series pages freeze at tick 1: do not edit a running series' page except
to record outcomes. Stage tools:

- **Ticks**: cron hourly, `wm_scheduled_run` one-shot; log
  `futon2/logs/wm-scheduled.log`; traces `futon2/data/wm-trace/`.
  A manual tick (`cd futon2 && clojure -M:wm-scheduled`) is the same code
  path but SAY it was manual in any record.
- **Scan census**: `scripts/mission_scan_census.bb` (futon2) explains
  the candidate count per source (raw files / non-primary excluded /
  primary / unique / live — 307/99/208/198/93 at the fence's landing).
  Any candidates-count drift gets a per-source explanation from this,
  never a shrug.
- **ψ**: `futon2.report.cascade-lane/mission->psi` (sorry-grain from
  `futon3c/holes/excursions/held-work-ledger.edn`, banner fallback —
  parses both `**Status:**` and bare `Status:` — ψ-v3 STUCK line).
  Held items are added at the SOURCE OF RECORD (`futon2/resources/
  sorrys.edn`), never by editing the ledger snapshot. SEAT-BY-STATE
  INJECTION is BUILT (`6871587`): at futility ≥ threshold a
  process-coherence pattern is injected as the cascade's opening
  element, tagged `:seat-injection`. Caches: `clear-all-caches!`.
- **Gate composition ("gate what you decide", operator-ruled)**: lane
  entry #1 is the judge's rank-1 decision target regardless of action
  type (`*gate-decision-target?*`, default on), then the top
  open-mission entries, deduped. The gate evaluates the decision.
- **Cascade**: `cd /home/joe/code/futon3a && .venv/bin/python
  holes/labs/M-memes-arrows/cascade_serve.py "<psi>"` — budget 20.
  JSON key for F is `F-free-energy` (NOT `F` — a get-with-default here
  once produced a false reading; verify your instrument).
- **Thin cascade on a well-formed ψ** = a LIBRARY GAP: do not enrich ψ,
  do not accept it — AUTHOR the missing patterns as a mutually-citing
  cluster from real practice (process-coherence/author-the-missing-pattern;
  the diagramprover remedy −0.443→+0.144 is the precedent), ingest via the
  seeds pipeline, re-measure. **Ingestion includes intra-cluster descent
  edges matching the flexiargs' [[kin]] cites — stems alone do not fire
  cluster-at-birth (first-flights remedy, 2026-07-06: nodes-only ingestion
  left F flat; the edges crossed it, −0.446→+0.423 same session).**
- **Deposits (fold-turns)**: see §3 — the exact authoring contract.
- **Gates**: `cd /home/joe/code/futon2 && bb scripts/live_loop_step.bb
  gate <ID>` (loop-2 board default; loop-3 via
  `LIVE_LOOP_STEPS=.../e-live-loop-3-steps.edn`). 2f = deposit gate.
- **Regression**: `clojure -M scripts/reference_regression.clj` in
  futon2 — 8 named checks with frozen references; run it after ANY
  change near the escrow/constructor/mana machinery; drift is loud.
  KNOWN: the peradam refusal-census check is deposit-count-sensitive —
  when deposits are added DELIBERATELY, re-freeze that reference with a
  dated annotation naming the new count; any other drift is a finding.
- **Realized ΔG semantics** (`2d13ef2`, `fold_realized.clj`): an
  executor that constructs 0 boxes against a plan with N obligations
  records realized ΔG **0.0 — a real sample, not nil**. NEVER use
  escrow-expected as realized (zero-error fake calibration); NEVER
  extend the fold engine's RULES with specific deposits in view
  (realized-measurement machinery must predate the deposits it
  measures). γ eating "promised much, delivered nothing" is R14
  working, not a bug.
- **Co-app from usage**: `scripts/coapp_live_usage_miner.py` (futon6)
  mines replayable deposits into co-app edges with deposit-level
  provenance; recomputes byte-identically. The pipeline wiring checker
  reports pending: 0.
- **Portfolio proposer** (`39492fa`): close/survey/apply-cascade action
  families exist DARK by default (`portfolio_action_proposer.clj`).
  Live composition is an operator release (M-aif2 slice-1 follow-up).
  NEVER flip it mid-series.
- **Pipeline wiring**: `futon2/holes/wm-pipeline-wiring.edn` +
  `bb scripts/check_pipeline_wiring.bb` — after ANY ruling that retires a
  component, update the wiring in the same commit (ledger §15: remedies
  can sever adjacent feeds). Formal twin:
  `mathlib4/DarkTower/WMPipelineExample.lean` (0-sorry; flip its
  theorems WITH the wiring change or the build breaks).
- **Mana**: `futon2.aif.mana-gate` — `(award! gate n operator-word)`
  needs the OPERATOR'S word verbatim; `(consume! gate purpose)`;
  ledger at `futon6/data/mana-gate/<gate>.edn`.

## 3. Authoring a valid deposit (THE clear instructions)

The overnight failure mode was an agent INVENTING ITS OWN PIN FORMAT.
Pin-1b now rejects that at load time. Follow this exactly; the exemplars
are `ft-autoclock-in-001.edn` (golden, v1+v2) and
`ft-peradam-mechanization-006.edn` (v2 shape) in
`/home/joe/code/futon6/data/fold-turns/`.

1. **ψ**: sorry-grain (WANT / HUNGRY-FOR / HAVE) from held-work items or
   the mission doc's actual tension. Record `:psi-sha256` (sha256 of the
   exact ψ text) and, with no sealed corpus, state no-blind-scoring
   honestly in `:psi-recipe`.
2. **Cascade**: run the constructor on the exact ψ; record size, shown
   pattern-ids with rel, F (key `F-free-energy`), budget, constructor
   commit (`cd futon3a && git rev-parse --short HEAD`).
3. **Fold** (this is an LLM turn — YOUR judgment): for each shown pattern
   read the verbatim prose at
   `/home/joe/code/futon3/library/<pattern-id>.flexiarg`; write boxes
   with `:fits-pattern` AND `:addresses-however` (engage the HOWEVER
   clause against the actual circumstance — restating the pattern is not
   folding). An honest HOLE beats a decorative box. A retrieved pattern
   is an invitation, not an obligation (the charter's scope-out beats the
   cascade's suggestion).
4. **Wiring (v2)**: hyperedges as maps `{:from :to :connective}` with
   `:connective` ∈ `:seq` (output feeds next) / `:tensor` (parallel,
   non-signalling) / `:copar` (coupled, only-valid-together); terminals
   `[{:id :bN :discharges :want-signature}]` — one terminal per WANT
   clause (v2.2). If every edge comes back `:seq`, record that as a
   finding, don't fake variety.
5. **ΔG**: compute the stored `:eval :delta-g` via the real fold-eval
   path, not by hand:
   `(futon2.aif.fold-eval/coverage-delta-g
     (futon2.aif.fold-llm/construction->wiring answer))`.
   Also record your hand coverage derivation as evidence, but do not use
   it as the stored value if it disagrees with the loader/replay
   function. Zero holes on a thin cascade is overconfidence, not
   coverage.
6. **PINS — compute via the REAL functions, never by hand** (write to
   /tmp/pin.clj, run `clojure -M -e '(load-file "/tmp/pin.clj")'` in
   futon2):
   - `:prompt {:sha256 ...}` = sha256 of the EXACT string from
     `futon2.aif.fold-llm/fold-prompt` called with your cascade
     pattern-ids, circumstance `{:mission <canonical> :psi <exact ψ>}`
     (insertion order matters), and prose-fn = verbatim flexiarg slurp;
   - `:prompt {:prose-sha256 {<pattern-id> <sha256-of-flexiarg>}}` and
     `:prose-source "verbatim futon3/library/<id>.flexiarg"`;
   - `:arming` cites the mana spend (gate, timestamp, balance) or the
     operator's word — never neither.
7. **Deposit** `ft-<mission-stem>-0NN.edn` (next free NN) into
   `/home/joe/code/futon6/data/fold-turns/`.
8. **Prove both directions**: `load-deposits` over the real dir accepts
   yours (pin-1b reconstructs your prompt — if this fails your pins are
   wrong, fix THEM not the loader); a tampered copy IN /tmp (change ΔG)
   is rejected loudly. Then `bb scripts/live_loop_step.bb gate 2f`.
9. **Record**: append your run log (all numbers, pins, mana ref) to the
   flight log file named in your dispatch; commit the deposit + log.

## 4. Reviewing (ground control's own duties)

- Re-run the gates yourself; never accept a claimed PASS (the ledger's
  first entry is a claimed-PASS that wasn't).
- Read the diff / the artifact; state WHAT YOU CHECKED in your verdict.
- Fix small review findings yourself; re-dispatch only substantial work.
- On approval: verdict bell with the checked-items list. On rejection:
  name the failing stage precisely.
- Keep the ledger discipline: mistakes + remedies + evidence-check into
  `futon2/holes/mistakes-remedies-ledger.md`; PARs at checkpoints.

## 5. Running a full inflight test, phase by phase (the loop-3 model)

1. Charter or pick the mission; write/verify its steps EDN board
   (`futon2/holes/e-live-loop-3-steps.edn` is the exemplar: steps with
   :needs edges + :gate {:cmd ...} so `bb scripts/live_loop_step.bb
   status/runnable/gate` drives it).
2. Census FIRST (step-0): enumerate the data the decisive test runs on
   BEFORE building anything (the P3 lesson, twice-learned).
3. Fly stages in order via commissioned agents (§1 dispatch structure);
   review-gate between every leg.
4. Convert every :manual gate to :cmd as evidence lands; gates are
   re-runnable forever (the standing-evidence-check design).
5. Pre-register expectations BEFORE running (cards-ii style: EXPECT +
   threshold + falsifier + deferral rule for underpowered windows).
   Beware falsifiers that fire on underpowering (the cards-7×10 trap)
   and sub-tests that pass by construction (demand a PASS-TRIVIAL grade).
6. **T-0 PREFLIGHT (immediately before tick 1 of any live series;
   the 2026-07-06 bundle is the template, all four verified then):**
   (i) instrument check — `df -h` + write/remove probes on `wm-trace/`,
   `futon6/data/` (fold-turns, mana-gate), and the evidence store;
   (ii) γ sample tail — confirm enactment records a COMPLETE sample
   (expected + realized, realized 0.0 legal); (iii) escrow mechanical
   standing — `load-deposits` + gate 2f green over the ACTUAL dir
   contents; (iv) baseline bundle — run gate 2f AND the reference
   regression suite, record outputs + then-current commit shas in the
   series log. Every mid-series surprise diffs against this baseline.
7. **MID-SERIES STOP-THE-LINE (never route around, including for the
   operator asking casually):** the series page freezes except
   outcomes; NO flag flips (`*gate-decision-target?*`, γ feed,
   proposer) without the operator's word recorded in the series log
   with a timestamp; a loader rejection mid-series is a DATA POINT —
   fix causes between ticks with annotation, never hotfix during one;
   mana before authoring, always — a refusal is a stop; enactment
   stays artifact-only; claims in the series log name their verified
   stage.
8. Natural ticks confirm; manual runs of the same path are fine but
   labeled. Do not retry-loop a surprising tick — diagnose (the 11:00
   race: a tick 5 minutes before a commit ran old code).
9. Close: verdicts vs pre-registrations, ledger entries, PAR, board all
   green (or honestly red with the reason named), commit, push.

## 6. Known traps (each cost a real hour once)

- `/api/alpha/agents` POST = zombie registration; use `/agents/auto`.
- auto-register renames your agent; read the id from the response.
- `F` vs `F-free-energy` in constructor JSON; get-with-default = silent
  false reading.
- ΔF (cascade free energy) ≠ fold ΔG (coverage) — two currencies.
- lane-futility's default trace dir is RELATIVE; pass the absolute path.
- Emacs heredocs: bare `"` never `\"`; `!` in double-quoted shell
  patterns triggers history expansion (fake results).
- `.state` dirs are sandboxes — never read them as live state.
- pkill -f matches your own wrapper.
- Long jobs (>30min): systemd-run, not bg shells (they die with the
  session).
- The mission scan is fenced (non-primary checkouts excluded,
  `24a4c8c`) — but new copy PATTERNS can still leak past the fence;
  candidates count drifting up is the smell, and
  `mission_scan_census.bb` is the diagnosis (per-source counts, never
  a shrug).
- Restarting an agent REPL via the picker can MINT A NEW AGENT and
  OVERWRITE the old agent's session file (`/tmp/futon-<type>-session-
  id-<id>`) while the registry still holds the right session. Repair:
  read the true session-id from the registry (`GET /agents`), restore
  the file, deregister the accidental mint, reopen the REPL with the
  vars bound explicitly (witnessed: codex-1/codex-3, 2026-07-06).
- Misrouted dispatches produce OFF-TASK deposits that pass every
  mechanical gate (two exist in the escrow, noted in cards-IV). The
  pin checks validity, not relevance — every dispatch names its
  mission explicitly, and review checks the deposit's :mission against
  the dispatch before counting it as the assignment's fruit.
- A RECLAIMED agent id is NOT a fresh lane: queued bells bind to the
  agent-ID and survive deregistration (there is no job-cancel
  endpoint), so respawning into a freed slot inherits orphaned
  dispatches. `/agents/auto` deliberately reuses low slots (ghost
  reclamation). For a genuinely fresh lane use
  `POST /api/alpha/agents/restore` with an explicit NEVER-USED id
  (`{"agent-id":"zai-13","type":"zai","cwd":...}`) — it recreates
  exact identities with a real invoke-fn (witnessed: the zai-12
  crossed-dispatch foul, 2026-07-06).
- TWO GROUND CONTROLLERS, ONE AGENT: nothing prevents a second
  controller dispatching to your fresh spawn. Convention: the
  spawner's dispatch must be the FIRST invoke; any controller finding
  an agent whose session is non-empty treats it as NOT FRESH; when
  dispatches cross anyway, WHISTLE the other controller to reconcile
  (crossing-immune) — do not race bells against bells.
