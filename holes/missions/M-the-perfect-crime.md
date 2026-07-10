# M-the-perfect-crime

Status: HEAD

**Type**: Mission
**Lifecycle**: HEAD (drafted 2026-05-27) → first IDENTIFY artifact landed 2026-06-07 (corpus-audit pass; see §IDENTIFY). Operator HEAD-verify still pending.
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
