# M-the-perfect-crime

Status: HEAD

**Type**: Mission
**Lifecycle**: HEAD (drafted 2026-05-27) → first IDENTIFY artifact landed 2026-06-07 (corpus-audit pass; see §IDENTIFY). Operator HEAD-verify still pending.  Checkpoint 2026-09-24: no real progress on the overt question (see §IDENTIFY, Checkpoint).
**Owner**: claude-1 (pending operator-direction)
**Pairing**: TBD

---

## HEAD (preserved from the operator-shape intake, 2026-05-27)

### The question

**Overt**: Can the futon stack acquire Tornhill-style static-analysis instruments (churn × complexity, temporal coupling, hotspot mapping) in a form that is actually live — and if it could, would such instruments have detected the kind of futonic-discipline failure that occurred today (mission prose routinely claiming "subsumes Tornhill" / "Tornhill hotspots become free" while neither the subsumer nor the subsumed was live)?

**Covert** (the harder question): If no amount of instrument-cloning would have caught today's failure — because the failure was at the *prose* layer, not the *instrument* layer — what *would* have caught it? What is the failure mode for which the very existence of high-resolution instruments could be a *cover*, rather than a corrective?

### The Tornhill thread (detective archaeology)

Adam Tornhill, *Your Code as a Crime Scene* (2015) + *Software Design X-Rays* (2018): treat the codebase as a crime scene, git history as forensic trace, churn × complexity as the indicator that points to where the trouble actually lives. Today (2026-05-27) we audited the corpus and found:

- **Zero** `.clj` matches for `tornhill | hotspot | temporal-coupling | change-frequency | code-maat`.
- The `cljs` "hotspot" in WM client is a pheromone-decay visit counter — a naming collision, not Tornhill.
- `futon2/scripts/futon2/report/war_machine_visual.clj` uses commit-count as activity proxy with the explicit comment *"cf. code-maat: hotspot = churn × complexity / here: hotspot = commit count"* — a single-axis approximation.
- `futon3/scripts/geometric_layer_phase2.clj` (offline `bb` script; futon3 CLAUDE.md flags futon3 scripts as "source material, not running infrastructure") computes T(var) = `1 if no incident :coverage edge else 0` — the indicator-function T, not Tornhill.
- But mission prose in `M-pattern-application-diagnostic`, `M-futon-enrichment`, `E-cross-prototype-geometry`, and `M-action-cost-modelling §3.8` (and likely others) routinely claims *"Tornhill's hotspots become free"* / *"subsumes code-maat"* — as if subsumption were live, when neither side was.

Joe's framing (verbatim, 2026-05-27 emacs-repl): *"no amount of code-maat cloning would have allowed us to detect this."* That observation is the seed of the mission.

### The Baudrillard thread (murder of reality)

Operator seed — publisher's cover-jacket text for *The Perfect Crime* (1995):

> In his new book, perhaps the most cogent expression of his mature thought, Jean Baudrillard turns detective in order to investigate a crime which he hopes may yet be solved: the "murder" of reality. To solve the crime would be to unravel the social and technological processes by which reality has quite simply vanished under the deadly glare of media "real time."
>
> But Baudrillard is not merely intending to lament the disappearance of the real, an occurrence he recently described as "the most important event of modern history," nor even to meditate upon the paradoxes of reality and illusion, truth and its masks. *The Perfect Crime* is also the work of a great *moraliste*: a penetrating examination of vital aspects of the social, political and cultural life of the "advanced democracies" in the (very) late twentieth century. Where critics like McLuhan once exposed the alienating consequences of "the medium," Baudrillard lays bare the depredatory effects of an oppressive transparency on our social lives, of a relentless positivity on our critical faculties, and of a withering 'high definition' on our very sense of reality.

The translation we owe: Tornhill's instruments produce *high-definition* maps of the codebase. Baudrillard's worry is that high-definition *itself* murders the real — the more transparent the system gets, the more its instruments substitute for the thing they measure. The perfect crime that occurred today is that the *prose-claim* of having Tornhill substituted for the *reality* of having Tornhill, and futonic discipline slept through it because the prose looked rigorous — well-cited, internally consistent, structured. Real-time, high-resolution, eminently quotable prose. That is the perfect crime.

### The mashup

The mission's working hypothesis: futonic discipline needs a **forensic** mode whose *target* is not the code (Tornhill's target) but the *authoring practice* — specifically the structural form of chained-claim verbs ("subsumes" / "becomes free" / "falls out of" / "gives us X as a bonus" / "layer-0 byproduct"). Where Tornhill asks *"where in the code is the trouble actually living?"*, the perfect-crime detective asks *"where in our authoring is reality being subtly murdered by claim?"*

Concretely: where in the mission corpus does "X subsumes Y" / "Y becomes free given X" / "naturally falls out of X" appear with neither X nor Y actually live? Today's audit found four such locations; a systematic detective pass would likely find others.

### The sub-question (named in operator intake)

**Should we add Tornhill features anyway?** The mission should NOT pre-decide. The detective archaeology may conclude:

- (a) **Yes** — Tornhill outputs are load-bearing for some real downstream consumer (e.g. providing the *complementary* signal to sorry-T-projection that the indicator-function approach can't supply), and the corpus's lean-on of them was anticipatory rather than illusory.
- (b) **No** — the corpus has gotten by without them, the prose-claims were the actual signal-source for the missions that referenced them, and adding instruments would only deepen the high-definition crime (more transparent surfaces, more substitution).
- (c) **Yes-but-with-discipline** — add them only after a perfect-crime audit pass clarifies what we would actually do with them, with explicit success criteria that route around the Baudrillardian substitution failure mode.

### Prior art consulted (HEAD-stage)

- Adam Tornhill, *Your Code as a Crime Scene* (2015); *Software Design X-Rays* (2018) — referenced in M-futon-enrichment, M-pattern-application-diagnostic, E-cross-prototype-geometry.
- Jean Baudrillard, *The Perfect Crime* (1995) — operator seed; jacket text above.
- Today's audit (2026-05-27 emacs-repl, claude-1 + Joe) — confirmation that Tornhill claim is corpus-wide and prose-only.
- `[[feedback_we_do_discipline]]` — the general "claim before evidence" memory; this mission proposes a sharper sibling.
- `[[feedback_subsumption_claim_discipline]]` (saved this session) — when prose says "X subsumes Y", BOTH must be live or both deferred-with-flag.

### Design space (sketch — NOT commitments; IDENTIFY shapes which are real)

- A pattern in `library/` capturing the chained-claim discipline rule (sibling to existing futon-theory entries).
- A corpus-audit pass that mechanically searches for chained-claim verbs across `**/holes/missions/*.md` + `**/holes/excursions/*.md` and produces a punch list of audit candidates.
- A separate **perfect-crime register** (distinct from `sorrys.edn`): these are sorrys-on-authoring, not sorrys-on-code; the entity-of-record is the prose location, not the code artifact.
- A FUTON-NATIVE Tornhill: churn × complexity over the futon1a `code/v05` hyperedges. The watcher already does commit-vertex catch-up; complexity proxy is the open design question. (Belongs to sub-question (a)/(c).)
- A diagnostic mode for M-pattern-application-diagnostic where prose-claims about pattern-subsumption get round-tripped against the live pattern library.

### HEAD exit criteria (operator-verified)

- Operator confirms the Tornhill-as-failure-case + Baudrillard-as-moraliste framing captures the intended tension.
- Operator decides whether the sub-question ("add Tornhill anyway?") is part of this mission or forks as a parallel mission.
- Operator names initial owner(s) / pairings, or explicitly leaves owner open for later.
- Operator signs off on the prior-art list, or names additions (candidates: Christopher Alexander on aliveness-as-honesty; Salingaros on structural tension; possibly McLuhan since Baudrillard explicitly riffs on him).

### Carried-forward tensions (for IDENTIFY)

- Whether this mission is *primarily* a detective excursion (one-shot corpus audit) or a *durable* discipline (recurring pattern-library entry + ongoing register).
- Whether the perfect-crime detection is itself instrumentable, or whether instrumenting it would reproduce the Baudrillardian crime (the detective becomes another instrument, becomes another substitute for the real).
- Whether the corpus audit produces a clean punch list or a more diffuse moralisation — depends on how strictly the chained-claim pattern can be specified.
- Whether `[[feedback_subsumption_claim_discipline]]` is memory-only or graduates to pattern-library-grade material (`library/futon-theory/` or `library/code-coherence/`).
- The sorry-T vs Tornhill complementarity question (today's framing): they measure orthogonal surfaces — declared anamnesis vs emergent churn × complexity. The mission should not silently re-collapse them into "two paths to the same signal."

---

## IDENTIFY

**Operator HEAD-verify still pending** (the HEAD exit criteria need Joe — see above). What *has*
landed (WM watched-cycle 1, 2026-06-07, pilot-driven under the guardrailed loop) is the first concrete
IDENTIFY artifact — the one named in the HEAD design-space sketch:

### The corpus-audit pass (first detective sweep)

A mechanical pass over `*/holes/{missions,excursions}/{M,E}-*.md` (excl. `<repo>/.state`) for the
chained-claim / subsumption verbs — `subsumes` · `subsumption` · `becomes free` · `for free` ·
`falls out of` · `by-product` · `as a (free) bonus`. Result:

- **84 raw hits across 39 files**; **74 candidates across 38 files** after excluding the mission's
  own definitional uses. The HEAD audit's original **4** confirmed sites were the tip — a systematic
  pass finds an order of magnitude more *candidates*.
- Top candidate-density files: `M-bounded-disposition` (8), `M-archaeology-control` (8),
  `M-single-locus` (5), `M-war-machine-wiring` (4), `E-interest-mining` (4).
- Artifact: **`M-the-perfect-crime.audit.edn`** (this directory) — the punch list (per-file,
  per-line, per-verb).

**The discipline (load-bearing, per the Baudrillard thread):** these are audit **CANDIDATES — a verb
is present — NOT confirmed perfect-crimes.** Most uses are legitimate. The crime is specifically
*"X subsumes Y with neither X nor Y live"*; deciding that needs the per-candidate **live-check** (is
the subsumer AND the subsumed actually live?). That check is the next IDENTIFY step — and it must not
itself become the high-definition instrument that substitutes for the judgement (the detective must
not become another cover for the real).

### Carried into the live-check step

- The **perfect-crime register** (sorrys-on-authoring, distinct from `sorrys.edn` — entity-of-record
  is the prose location), seeded from the punch list once candidates are live-checked.
- ~~Graduation of `[[feedback_subsumption_claim_discipline]]` to a `library/` pattern.~~ **DONE 2026-06-11** (WM-pilot arc-2 cycle 12): minted `library/code-coherence/subsumption-claim-discipline.flexiarg` (futon3 `6e76dc8`) — two-sided live-check (both X and Y live or both flagged), canonical instance = the Tornhill-subsumption audit.
- The sub-question ("add Tornhill anyway?") stays open — the audit does **not** pre-decide it.

### Second sweep — a perfect crime in the calibration instrument (2026-06-11, WM-pilot)

The first sweep hunted chained-claim verbs in mission *prose*. A second instance surfaced
today in a different layer — a measurement *instrument* — confirming the covert question
(§HEAD: "the failure mode for which the very existence of high-resolution instruments could
be a *cover*, rather than a corrective").

**The crime.** The WM-pilot loop built a G-SIM calibration harness to measure the forward
model against realised field outcomes — with high-definition rigor: independence tags,
measured-vs-fallback, settled-vs-transient reads, anti-laundering throughout. It accrued
"measured pairs" with near-zero error and looked calibrated. A source-check (fable-1, of
`compute-efe`) found the instrument was measuring the model against *itself*: the field's
per-target G is composed from the *same* constant predict-effects the model predicts with —
`prior == value`, one level down. The near-zero errors were not calibration; they were a
tautology. The apparatus's very rigor was the cover that made a vacuous measurement look real
— the Baudrillardian crime (§Baudrillard: "the more transparent the system gets, the more its
instruments substitute for the thing they measure") in the calibration domain rather than the
prose domain.

**A smaller same-day instance (transient-as-durable).** A pilot cycle reported error 0.277 as
"a real-content hole closure moves the target G substantially." A settled re-read showed the
realised had been caught as a transient post-edit spike before settling to baseline; the claim
was retracted. Prose-claim (durable movement) substituting for reality (a scan-timing artifact).

**What it adds to the mission.** The first sweep's thesis was that no Tornhill-instrument
cloning would catch a *prose*-layer crime. This sharpens it: an instrument can commit the crime
against *itself* — a measurement whose rigor is the cover for measuring nothing. The forensic
mode this mission seeks therefore extends past a chained-claim-verb scan over prose to a
*self-reference check*: does the instrument's "measurement" reduce to the instrument grading its
own homework? The same-day fix (target-sensitive predict-effects, breaking `prior == value`,
operator-consented) is what made the calibration measure something that can be *wrong*.
Provenance: WM-pilot session 2026-06-11 (claude-3 + fable-1), Pilot's-Log Turns 4–7. Cross-ref
`[[feedback_subsumption_claim_discipline]]` (prose chained-claims) and this new self-reference
failure mode.

**The crime relocates; it is never killed (the pudding-prover rationale, derived from below).**
A forensic corollary surfaced when the `prior == value` finding was fixed the same day
(target-sensitive predict-effects). The fix did not abolish the crime — it pushed it into a
*scarcer witness*. The constant model's **state-blindness** was exposed (it predicts the same
regardless of world-change); fixing that exposed the scaled model's **increment-circularity**
(G-vs-G tests whether the world *moved*, never the per-hole *coefficient* — the agreement scale
is still model-priced). The next witness is the **outcome**: did real functioning result? —
where a three-witness certificate ([[project_pudding_peradams]]) lives. So the discipline this
mission seeks is not a single detector but a **ladder of scarcer witnesses**: high-definition
instruments substitute for the real until the witness becomes too expensive to fake. That
laddering, reached here from the calibration domain, *is* the pudding-prover's rationale arrived
at from below. Named with ground control: **two-layer calibration** — L1 G-vs-G = dynamics /
consistency (cheap, every cycle, never value evidence); L2 outcome-vs-prediction = value, gated
at pudding G1 (the arrow-witness binding, registry sorry #2). Each relocation costs the launderer
more — which is the point.

### Third sweep — the agent chat as crime scene (Joe, 2026-09-21)

Joe (emacs-repl, 2026-09-21): *"I still haven't gotten into the 'Your Code as a Crime Scene'
stuff that we keep circling, but now I am wondering if those ideas could be reapplied to
'your agent chat as a crime scene'."*

This answers the covert question from HEAD more directly than code instruments could. The
original failure was at the prose layer, and the conversation is where that prose is written.
Tornhill's instruments, pointed at the chat record instead of git, give the forensic mode aimed
at authoring practice that §"The mashup" asked for.

| Tornhill (code) | Agent chat | Existing data (2026-09-21) |
|---|---|---|
| Hotspot = churn × complexity | topics the operator keeps correcting: redirect/reject rate × turns, per mission or pattern | stance pilot (`futon0/analysis/audits/operator-reply-pilot-2026-09-21/`), pattern stages (`PATTERN-STAGES.md`); both provisional pending Joe's blind labels |
| Temporal coupling | missions, patterns or agents recurring in the same turns; hidden coupling between lanes | pattern retrievals per turn; mesh edges (durable since futon3c eca529f7) |
| Code age | claim age: a statement repeated after the system changed | casebook A2 (`futon0/analysis/business-models/CASEBOOK-agent-failure-modes.md`) |
| Knowledge map / main developer | which seat holds the context for what | roster, session logs |
| Knowledge loss (developer leaves) | compaction, or a seat retired mid-thread | `.pre-compact-*` transcript snapshots |
| Conway / coordination needs | agent-to-agent traffic per mission | park/wake pilot (casebook E1), call graph |
| Offender profiling | trace bad output to the instruction that started it | `futon0/analysis/audits/FORENSIC-autopilot-2026-09-21.md` (codex-26, 2026-09-13) |

**Chained-claim verbs in chat.** The first sweep's verbs ("subsumes", "falls out of", "for
free") occur far more in agent turns than in mission docs, and the chat record also shows whether
the operator let them pass. A passed claim later shown false is a perfect crime with a timestamp
and a witness. Candidate next step: run the first sweep's verb list over agent final turns, join
each hit to the operator's next-turn stance, and live-check the accepted ones.

**First two cases (both 2026-09-21, both perfect crimes in the Baudrillard sense — the evidence
looked complete):**
1. *Mesh edges looked recorded; they lived in an atom.* Live endpoint 23 edges, durable count 0
   (`futon3c/holes/NOTE-agency-accounting-gaps-2026-09-21.md`, cfa2e00c). Casebook A5.
2. *The Minard figure said Joe stopped working after 09-13; the instrument had stopped.* futon1b
   pagination broke its newest-first contract (XTDB sort spill); caught only against an
   independent transcript census (`futon0/analysis/audits/claude_operator_census.py`). Casebook C3.

**Discipline carried over.** Chat instruments produce more high-definition pictures of the
operator's work, and a stage-share table can stand in for understanding it. Every chat metric
therefore needs a check against a source outside itself, as the transcript census checked the
Minard figure — the same two-sided live-check as `subsumption-claim-discipline.flexiarg`.

### Checkpoint 2026-09-24 — four months on, still no real progress on the overt question

The mission opened on 2026-05-27. Since then it has collected three sweeps of *examples* of the
crime, but the overt question — are Tornhill-style instruments live in the stack? — has the same
answer as on day one: **no**. The follow-through steps named above were never taken:

- Operator HEAD-verify: still pending.
- Per-candidate live-check of the 74 punch-list candidates (`M-the-perfect-crime.audit.edn`,
  `:audit/next`): not started; no candidate has a verdict.
- Perfect-crime register: never seeded; no file exists.
- Sub-question "add Tornhill anyway?": still open, with no one working on it.

**What has moved (Joe, 2026-09-24).** The movement is on the third sweep's side — analytics over
the agent chat and the stack's commit record — not on code instruments:

- The Minard figure (casebook C3, above), now regenerable from
  `marimo-zone/notebooks/minard-operator-work-20260921.py` via
  `futon0/analysis/audits/minard_operator_work.py`. Its failure was caught by the kind of
  independent check this mission asks for (the transcript census).
- Marimo notebooks in `marimo-zone/` (all 2026-09-21 unless noted): blind pattern-stage
  labelling with an agreement check (`pattern-stages-20260921.py`), the operator-reply stance
  pilot (`chat-operator-reply-pilot-20260921.py`), park/wake usage cost
  (`chat-park-wake-pilot-20260921.py`), a commit-activity audit in
  `chat-business-ideas-20260921.py` (`futon0/analysis/audits/commit_timeseries.py`), and
  earlier the WM closure view (`wm-closure.py`, 2026-09-17).

None of these is at the Tornhill level yet. The nearest is the commit-activity audit, which
counts commits per repo per day: churn at repo grain, with no file or var breakdown, no
complexity axis and no coupling. The chat-side notebooks are early versions of the third
sweep's table (hotspot ≈ correction rate, coordination cost ≈ park/wake usage), and their
labels are provisional until Joe's blind labels are in.

**A correction to the HEAD audit.** "Zero `.clj` matches" was true only for the words that audit
searched for. A churn/complexity pipeline was built on 2026-03-06 (futon4 `b47a852`), before the
mission opened, and the audit did not find it:

- producer: `futon4/scripts/ingest-three-columns.py` — `ingest_file_churn` writes `code/file-churn`
  (commits all-time / last 90 days / last touch per file); `ingest_indentation_complexity` writes
  `code/indentation-complexity` (Tornhill's own complexity proxy);
- consumer: `futon3c/src/futon3c/enrichment/query.clj` (`classify-hyperedge` → `:churn`,
  `:complexity`; property keys match the producer);
- display: `futon4/dev/arxana-browser-enrich.el` "Churn / Complexity" panel.

**It is not live.** futon1b census (`/api/alpha/census?type=…`, 2026-09-24): `code/file-churn` 0,
`code/indentation` 0, `code/indentation-complexity` 0 (control: `code/v05/commit` 17,850). Two
defects are visible in the code:

1. The futon1b migration candidate list (`futon1b/migration/export.clj`,
   `futon1b/hx-backfill-per-type.bb`) names `code/indentation`, which nothing writes, and omits
   `code/indentation-complexity`, which the producer does write. Export only probes listed types, so
   complexity data could not have survived migration. `code/file-churn` is listed and still reads 0;
   whether it was ever ingested into futon1a is unknown (futon1a :7071 is down).
2. The Arxana panel renders only `(when (or churn complexity) …)`, so an empty store shows as a file
   with no churn, not as a dead instrument. This is the mission's own crime in miniature: consumer
   code present and correct, no data behind it, nothing on screen to say so.

**Raw material that is live but unused.** futon1b holds `code/v05/edits` 542,936 (commit → var),
`code/v05/var` 47,827, `code/v05/calls` 39,364. Var-level churn and temporal coupling (vars edited
in the same commits) could be computed from these directly, from one source instead of a second
`git log` pass. Nothing computes them. No complexity data is live in any form, and churn × complexity
is not computed anywhere. (Not checked: whether the v05 commit ingest is current, and how much of it
comes from worktree repos such as `futon3c-d`.)

**What would count as progress.** Smallest step: fix the type name in the migration list, re-run the
L0 ingest into futon1b, make the Arxana panel say "no churn data in store" when it has none, and pass
a census check with non-zero counts. Any later churn/coupling metric derived from `code/v05/edits`
needs a check against a source outside itself (e.g. `git log --numstat` on one repo), per
`subsumption-claim-discipline.flexiarg`.

### Integration plan — Tornhill features into the EFE field page (2026-09-25)

Warrant: claude-12-turn-91 (Joe), fragment s1 — "mission-efe-field.html shows no evidence of
including the Tornhill features … I would like to get a plan back (with details in the mission)
about how the integration will be effected" (pattern: orchestration/recorded-handoff — the plan
lives here, not only in the message channel).

Joe's evidence: `https://zone.hyperreal.enterprises/wip/mission-efe-field.html` — generated by
`futon6/scripts/mission_efe_field.py` from `futon6/data/efe-scopes.json` + carpet positions;
confirmed 2026-09-25 that no churn or complexity input feeds it.

The plan has four layers, in dependency order. Layers 1–2 are the mission's own 2026-09-24
"What would count as progress" step made concrete; layers 3–4 carry the data to the two display
surfaces. Each layer has an acceptance check against a source outside itself, per
`library/code-coherence/subsumption-claim-discipline.flexiarg` (both sides live, or flagged).

1. **Data layer — make the existing ingest actually land.** Fix the futon1b migration type
   list (`futon1b/migration/export.clj`, `futon1b/hx-backfill-per-type.bb`): the list names
   `code/indentation` (which nothing writes) and omits `code/indentation-complexity` (which
   `futon4/scripts/ingest-three-columns.py` writes). Re-run the L0 ingest
   (`ingest_file_churn` + `ingest_indentation_complexity`) into futon1b. Also establish whether
   `code/file-churn` was ever ingested into futon1a (futon1a :7071 was down at the 2026-09-24
   census — re-check).
   *Acceptance:* futon1b census (`/api/alpha/census?type=…`) returns non-zero for
   `code/file-churn` and `code/indentation-complexity`, with `code/v05/commit` as control.

2. **Derived layer — compute the Tornhill metrics from live data.** Churn × complexity per
   file, and var-level churn + temporal coupling (vars edited in the same commits) computed
   from the already-live `code/v05/edits` (542,936), `code/v05/var` (47,827), `code/v05/calls`
   (39,364) — one source, no second `git log` pass. Complexity axis from the layer-1
   indentation ingest (Tornhill's own proxy). Before trusting v05-edits churn, verify the v05
   commit ingest is current and establish how much of it comes from worktree repos
   (`futon3c-d` etc.) — explicitly unchecked at the checkpoint.
   *Acceptance:* derived churn for one repo cross-checked against `git log --numstat` on that
   repo (independent source, per the discipline).

3. **Display layer A — fix the silent instrument.** `futon4/dev/arxana-browser-enrich.el`
   renders the Churn/Complexity panel only `(when (or churn complexity) …)`; an empty store
   shows as a file with no churn. Make the panel say "no churn data in store" when it has
   none — absence of data must be visible, not indistinguishable from a clean file.
   *Acceptance:* with the store empty the panel shows the no-data message; after layer 1 it
   shows real values for a known-churny file.

4. **Display layer B — the EFE field page (Joe's named surface).** Add a Tornhill overlay to
   `futon6/scripts/mission_efe_field.py`: a per-mission hotspot metric (churn × complexity
   aggregated over the files/vars in each mission's scopes) rendered on the district hubs —
   e.g. hub ring width/colour intensity ∝ hotspot score — alongside the existing Salingaros
   class colour, with the metric's source and as-of timestamp in the page legend. Data path:
   a new dump script (sibling to `scripts/mission_efe_scope_dump.py`) queries futon1b and
   writes a JSON the field script reads, keeping the page reproducible and static. The
   overlay must have an explicit no-data state per district (grey ring + legend note), never
   silent absence — the same defect class as layer 3.
   *Acceptance:* the rendered page shows the Tornhill layer with a data timestamp; a spot
   check of one high-churn mission against `git log` on its files agrees on the ranking
   direction; districts with no data are marked as such.

**Sequencing.** 1 → 2 → {3, 4} (3 and 4 are independent). Layer 1 is the smallest step and
unblocks everything; it is also the checkpoint's own named next step, unchanged.

**Landed 2026-09-25 (claude-12-turn-95):** a minimal layer-4 form — before layers 1–2, since
Joe asked for the smallest visible change. `futon6/scripts/mission_efe_field.py` now draws a
Tornhill change-frequency ring per district (commits touching the mission's own doc, 180-day
window, thickness ∝ log churn, deduped across worktree repos by commit hash) with a thin
dashed grey ring as the explicit zero state, and legend/tooltip stating the complexity axis
is pending. Cross-checked: M-the-perfect-crime ring reads 9, `git log` on futon3c reads 9.
This is change-frequency only — churn × complexity still awaits layers 1–2.

**Relabelled same day (claude-12-turn-100, reviewer feedback relayed by Joe):** the first
version's label ("Tornhill change-frequency" / hover "Tornhill churn") overstated the
measurement — the ring counts commits to the mission's *own doc*, and Tornhill churn is
change-frequency in the *code under study*. The reviewer named it correctly: the same
overclaim class this mission exists to catch, caused by the plan's own gap (nothing links a
mission to its code yet, so the one linkable thing got measured). The ring is now labelled
"mission-doc activity" in legend and hover, and both state what is *not* claimed. This
instance is logged here as the register's first entry would have been — a perfect crime
caught in review *before* it could settle into prose.

**Code churn landed (claude-12-turn-104, same day):** the reviewer's named root cause — no
mission→code link — turned out to be already closed in data: `fold-embed/edges.jsonl`
carries `touches` edges (mission-doc → code vars), and `futon6/scripts/mission_activity.py`
(packet from claude-12) resolves them to files and computes per-file git churn + indent
complexity into `data/mission-activity.json`. The EFE page now draws that as a second ring:
code churn 90d, hotspot = churn × complexity on hover, temporal coupling, and three explicit
coverage states (28 measured / 7 link-but-unresolved / 289 no-link). The doc-activity ring
stays, labelled as the proxy it is. Layers 1–2 of the plan above are partially pre-empted by
this pipeline — its source of record is the touches edges + git log, not the futon1b
churn/complexity types (still 0); reconciling the two stores remains open.

**Discipline carried.** Every metric above gets its check against a source outside itself
(census, `git log --numstat`, transcript census precedent) — the two-sided live-check; the
detective must not become another high-definition cover. The sub-question ("add Tornhill
anyway?", HEAD) is answered conditionally by this plan: this is branch (c)
yes-but-with-discipline, because Joe has now named a real downstream consumer (the EFE field
page) with explicit acceptance criteria.

### Checkpoint 2026-09-26 — Tornhill at file grain, joined to the agent chat

Joe (2026-09-26): the Tornhill work "doesn't seem to have been sorted out very well yet", and
the third sweep ("your agent chat as a crime scene") should be built together with the classic
version. Diagnosis: the work so far measured a layer above Tornhill's unit. The store route
(futon1b churn/complexity types) is still empty, and the EFE ring reaches code only through
mission→code `touches` edges, which cover 28 of 348 carpet missions. Tornhill's own analyses —
hotspots, coupling, age, knowledge map over files — had not been run at all. They need only git.

**What was done**

- `futon0/analysis/audits/tornhill.py` (collect + check): per repository, over the last 90
  days of HEAD history (no merges, as code-maat), for code files only:
  hotspots (revisions × indentation complexity), complexity trend for each repo's top 10
  (sampled at the commits that touched the file), cross-module change coupling (code-maat
  defaults: ≥5 shared, ≥30%, commits of >30 files skipped), code age, and the model named in
  `Co-Authored-By`. 16 repositories, 3,717 code files, about 20 s.
- `futon0/analysis/audits/tornhill_chat.py` (collect + check): joins each commit to its session
  through futon6 `data/session-commit-index.json`, and each session to its transcript. Per
  file: the seats that changed it, Joe's turns in those sessions (census rule; raw, and shared
  out across the code files each session touched), compacted sessions, and pilot stance labels.
  Also coupling at session grain, compared with commit grain on the same commits.
- `futon0/analysis/audits/test_tornhill.py`: 13 tests on a synthetic repo and transcripts,
  including the bad cases each check exists for (tampered report fails `check`; side-branch
  commit counted; merge not counted; a row written after `as_of` not counted).
- `marimo-zone/notebooks/tornhill-crime-scene-20260926.py`: hotspot scatter and table,
  coupling tables (commit grain; session grain never in one commit), knowledge-map bars by
  agent kind, hotspot × operator-attention scatter, seat table, coverage and check results. A
  button re-runs all four steps.
- Outputs: JSON in `~/.local/share/futon-audits/tornhill/` (session ids and seat names, no turn
  text; kept out of the repos per `[[run-data-is-data]]`); the two `.check.txt` files are
  committed beside the scripts.

**Design decisions** (settled while building; recorded here in place of a separate PSR)

- Pattern: `library/code-coherence/subsumption-claim-discipline.flexiarg` — every number has a
  `check` that re-reads the source by a different path; a repo that cannot be read is listed
  as missing, never zero (as `commit_timeseries.py`).
- Complexity is indentation, with the unit per language (2 for Lisp/JS, 4 for Python/Rust/sh).
- A file created inside the window gets no trend ratio: its first sample is its birth, so the
  ratio would restate its size. It is reported as "new: N→M lines".
- Quarto `*_files/` bundles and build output are not code (futon7a's top "hotspots" were
  bundled Vega/KaTeX).
- One commit claimed by two sessions (24 cases): the session that printed the sha beats a
  subject+time match; then the earlier session. The ambiguous commits are listed in the output.
- Sessions without an Agency envelope are labelled `claude:unrouted` / `codex:unrouted`. The
  sampled ones are build-loop invocations ("ROW TO DO THIS INVOCATION …") and codex exec jobs,
  with no operator. Joe typing into an interactive CLI session has no envelope either; those
  rows are counted separately as direct CLI turns.
- Transcript reads stop at an `as_of` time fixed at the start of the run; live seats keep
  writing while it runs.

**Findings (as of 2026-09-26)**

- Hotspots: `futon3c/src/futon3c/transport/http.clj` (189 revisions, 9,130 lines, grew ×1.74
  in complexity over the window) and `futon2/src/futon2/aif/full_loop_runner.clj` (236
  revisions, created in the window, now 5,492 lines) are far ahead; then `war_machine.clj`
  (futon2) and the `futon3c/apm/live_*` family.
- Trailers leave most authorship blank: 8,804 of 12,915 file revisions carry no
  `Co-Authored-By`. The session join names the seat: `full_loop_runner.clj` was changed by 28
  seats in 3½ weeks, the busiest (codex-10) making 17% of the changes; `http.clj` and
  `war_machine.clj` by 23 each.
- Coupling: 73 cross-module pairs at commit grain, led by `war_machine.clj` ↔ `aif/trace.clj`
  (futon2, 38 commits) and the futon1b graph/server/test triangle. At session grain, 38 of 97
  cross-module pairs never share a commit — mostly the futon3c APM cluster
  (`live_job_driver`, `countdown_control`, `queued_frame_adapter`, `problem_queue_supervisor`),
  which agents change together and commit separately.
- Operator attention: 1,203 of 1,945 attributed files drew any of Joe's turns. Highest
  apportioned: `apm/workspace_lifecycle.clj` (3 sessions), then `war_machine.clj`,
  `aif/trace.clj`, `transport/http.clj`, `full_loop_runner.clj` — the code hotspots again.
- Revisions and line churn agree only moderately as rankings (Spearman 0.4–0.7 per repo);
  agent commits are small and frequent. The notebook shows both.

**Checks**

- `tornhill-2026-09-26.check.txt`: PASS (43 hotspot files re-read with
  `git log --full-history`). The first run failed on futon6 `scripts/linode_stepper.py`: path
  simplification drops side-branch commit 304beb6, which really changed the file. The check
  now uses `--full-history`.
- `tornhill-chat-2026-09-26.check.txt`: PASS (census recount per session; commit found in its
  transcript; file lists by `git show`). Earlier failures and their causes: a merge commit
  listed by `git show` (merges excluded, as in the classic half); a commit attributed twice
  (rule above); a turn that arrived during the run (`as_of`).
- Tests: 13 tests, 0 failures.

**Limits**

- The session index covers 2026-09-01 → 2026-09-25, so the chat side sees 3½ weeks of the
  90-day window. 2,086 index rows have no repo (commits on branches that no longer exist).
- Kimi and Zai sessions have no local transcripts: seat and operator counts are absent, not 0.
- Stance: 100 pilot-labelled replies from three seats. Counts are shown and not turned into
  rates.

**Next**

- Rebuild the EFE ring on this file-level data, so the mission view is a lens over real
  hotspots.
- The chained-claim sweep over agent final turns joined to Joe's next-turn stance (third
  sweep, "Candidate next step"). The join to sessions now exists for it.
- The futon1b churn/complexity types (plan layer 1): this pipeline reads git directly, so the
  store route is optional. Joe to say whether futon1b should hold these metrics.

**Review of `tornhill.py` / `tornhill_chat.py` (claude-12, 2026-09-26; handoff packet R)**

Reviewed at futon0 fa0546b against the report `tornhill-2026-09-26.json` (generated
00:48:42Z). No warrant exists, and the tests were not rerun. The handoff records
"13 tests, 0 failures".

Spot-checks, made by a route different from the script's:
- `futon0 scripts/futon-sync.clj`: complexity recomputed with awk from `git show <head>:`
  gives total 4968.5 and loc 888, the same as the report.
- The top futon3c coupling pair, `emacs/session-mode.el` ↔ `test/session-mode-test.el`:
  10 revisions each and 10 shared commits by `git log --full-history`. No shared commit
  touches more than 7 files, so no sweep is in it. Report: shared 10, degree 100.

Verdict: the counts can be used, for the EFE ring (packet 1b) among others. Tests and
behaviour have the following gaps:

1. **`tornhill_chat.collect` has no test.** The tests cover `read_claude` (uuid dedup
   across `.pre-compact` snapshots, `as_of`, the census rule) and `attribution_rank` on
   its own. The per-file join, the ambiguous-commit path in `resolve_commits`,
   `operator_share` apportioning, and session-grain coupling (`SESSION_MAX_FILES`) are
   all untested. The rank tests cannot catch a slip in the `prev`/`c` comparison inside
   `resolve_commits`, which is where a double-attributed commit would come from. This
   is the gap to close first.
2. **Check (1) of `tornhill_chat check` re-runs the collector's own rule.** It is a
   second copy of the same loop, not a second reading. It catches a counting slip. It
   cannot catch a wrong rule, for example a new kind of harness text in Joe's name. (I
   looked for clock-reminder turns, "You requisitioned …", passing the rule: 0 found.)
3. **Complexity is read from the working tree, and revisions from HEAD.**
   `analyse_repo` reads `Path(repo) / f`. A file with uncommitted edits gets HEAD's
   revisions and the working copy's complexity. The trend samples use `git show
   <sha>:`. Reading `git show HEAD:<path>` for the current value too would make the
   report reproducible from its recorded `head`.
4. **`trend_ratio` starts after the first change in the window, not at the window's
   start.** `series[0]` is the file as it stood after its first in-window commit. For a
   file that predates the window, growth made by that first commit is left out, so the
   ratio understates growth. The fixture pins this behaviour ("1 indent at the first
   touch"): `old.clj` was 0 indents before the window, so a ratio from the window's
   start would be undefined. The docstring's "plus HEAD" is not implemented.
5. **The sweep threshold counts code files only.** `MAX_CHANGESET` is compared with the
   number of the commit's files that are code files changed in the window. code-maat
   counts the whole change set. A commit with 20 code files and 60 data files counts
   toward coupling here. The fixture's sweep is all code, so the tests do not decide
   between the two readings.
6. Complexity units are pinned for `.clj` and `.py` only. A wrong `INDENT_UNIT` entry for
   `.el`, `.js` or `.lean` would pass.

None of these changes a number in the 2026-09-26 check files. Items 1 and 3 are one small
packet each, when wanted.

---

## Appendix A. Cross-references

- `M-action-cost-modelling.md` §3.8 — the aliveness synthesis paragraph where Mana / Anamnesis / Alexander / Salingaros / EOI / T were unified; one of the four sites where Tornhill-subsumption claims live.
- `M-pattern-application-diagnostic.md` — multiple Tornhill references; would be a natural early audit target for any detective pass.
- `M-futon-enrichment.md` (futon4) — original locus of "Code as a Crime Scene" citation and "Tornhill-style hotspots as a free bonus" framing.
- `E-cross-prototype-geometry.md` (futon3) — ΔT-equals-Tornhill-hotspot identification.
- `E-substrate-2-sorry-typing.md` — landed today; the v0 indicator-function T over sorries. Complementary to (not subsumed by) any future Tornhill work.
- `futon2/scripts/futon2/report/war_machine_visual.clj` — only live trace of code-maat-adjacent computation in the stack (commit-count proxy).
- `futon3/scripts/geometric_layer_phase2.clj` — offline ΔT computation; reference impl for any future port to the JVM.

## Appendix B. Provenance

- Operator seed: emacs-repl, 2026-05-27, Joe (verbatim Tornhill+Baudrillard mashup with jacket-text quote).
- Audit data: 2026-05-27 emacs-repl session, claude-1 (grep over `/home/joe/code/**` excluding `.md`).
- Spawned from: discipline-failure observation during M-action-cost-modelling INSTANTIATE-adjacent work (E-substrate-2-sorry-typing live-write trigger verification + Joe's "Tornhill-as-load-bearing" concern).
