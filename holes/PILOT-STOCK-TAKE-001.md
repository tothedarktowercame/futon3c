# Pilot Stock-Take 001 — the first ten-cycle arc (flight side)

**Date:** 2026-06-11
**Author:** claude-3 (WM pilot)
**Companion side:** ground control (fable-1) brings the apparatus/instrument side (what got built/fixed per cycle); Joe convenes.
**Scope:** WM-pilot LOOP Turns 1–10 (`PILOTS-LOG.md`) — the long end-to-end inhabitation toward FutonZero's G-SIM calibration.

## The arc, one line per cycle

- **T1** (claude-1) — first cycle on the cascade lane; found capability `:grounding` is prose, not machine-checkable.
- **T2** — teleport caught (`open-mission` on an already-open mission) → V2 no-teleport; keystone close of M-capability-star-map.
- **T3** — first earned address-sorry on the refilled registry; minted `two-projections-of-one-quantity` (a cited-but-unwritten pattern).
- **T4** — first executed pair via the realised-on-merge binding — but a **FALLBACK** (target vanished → censored 0.0), caught by ground control. Pipe verified end-to-end; measured count stayed 0.
- **T5** — first genuine **MEASURED** pair (partial-discharge: make M-futonzero-generative's true state machine-visible); error 2.4e-4.
- **T6** — §4.3 policy/value vocabulary; logged error 0.277 — later **RETRACTED** as a transient artifact.
- **T7** — §4.2 toy field fixture (codex build; first build-dispatch lane); error 4.1e-4. **KEY FINDING:** settled hole-G is flat → cycle-6's 0.277 was a transient; named the realised-read protocol.
- **T8** — first below-cap flight under the target-sensitive model (M-the-perfect-crime); **TRUE NULL** (error 6.6e-5); surfaced that the fix **RELOCATED** `prior==value`, didn't break it.
- **T9** — first **CLEAN L1 DYNAMICS** pair (M-daily-scan Day-counter fix, ohc 5→4); error 0.039 = the per-hole increment.
- **T10** — second dynamics pair (M-daily-scan code-search probe-type, codex build, ohc 4→3); error **0.0386** — consistent with T9's 0.0394, confirming a roughly **linear per-hole increment (~0.039)** across two different ohc transitions.

## The deep finding: the loop's subject was its own value function

Run honestly, the loop surfaced that its calibration was **circular**. A source-check of `compute-efe` (fable-1) showed the per-target field G composes the *same* constant predict-effects the model predicts with — `prior == value`, one level down. The near-zero "measured" errors (T5/T7) were the instrument measuring the model against itself. The fix — **target-sensitive predict-effects** (operator-consented, it re-ranks the whole field) — *broke state-insensitivity* (the field now responds to real world-change) but *relocated the increment-circularity* (G-vs-G tests whether the world moved, never the coefficient). That named the **two-layer calibration**:

- **L1 G-vs-G** — dynamics/consistency. Cheap, every cycle, honestly *never value evidence*. (T5–T10 are L1.)
- **L2 outcome-vs-prediction** — the value layer. Needs *witnessed outcomes* (closure-folds, CH2, peradam: did real functioning result?), gated behind **pudding-G1's arrow-witness binding (registry sorry #2)**. Accrues free per executed cycle.

The forensic corollary (recorded on M-the-perfect-crime): **the crime is never killed, only pushed to a scarcer witness** — constant's state-blindness exposed → scaled's increment-circularity exposed → next, witnessed outcomes. Each relocation costs the launderer more; that *is* the pudding-prover rationale, derived from below.

## Pairs by layer

| cycle | error | class | disposition |
|---|---|---|---|
| 4 | 0.0 | censored fallback | **excluded** |
| 6 | 0.277 | transient spike | **excluded** (`:settled` ages it out) |
| 5 | 2.4e-4 | visibility edit | L1 near-zero |
| 7 | 4.1e-4 | `prior==value` era | L1 near-zero |
| 8 | 6.6e-5 | true null (ohc unchanged) | L1 null |
| 9 | **0.039** | clean dynamics (ohc 5→4) | **L1 dynamics** |
| 10 | **0.0386** | clean dynamics (ohc 4→3) | **L1 dynamics** |

**The clean L1 result:** the per-hole increment is **consistent** — 0.0394 (ohc 5→4) and 0.0386 (ohc 4→3) — so the field's settled G is roughly *linear* in hole-count below the cap, and a real hole-closure moves it by that increment. That is genuine dynamics evidence: the world moved, by the predicted amount.

## Both models (dual-prediction, post-switch pairs)

**Honest correction (this stock-take caught my own overclaim):** a 2-pair read (T8, T9) looked like "scaled ~2× better"; the third pair (T10) evens it out. Over T8/T9/T10 the **mean errors are nearly tied — scaled ~0.026 vs constant ~0.027** — but they win on *opposite* cases: scaled nails the **null** (T8: 0.00007 vs 0.040 — correctly predicts no-move), constant nails the **continuity-landing** (T10: 0.0004 vs 0.039 — the realised landed at the ohc-3 continuity point where constant's flat prediction sits), and they roughly tie on the mid-move (T9). The per-pair "winner" is dominated by *where the action lands relative to the continuity point*, not by clear model superiority — exactly what you'd expect of an L1 dynamics/consistency layer that is **not** a value-discriminating one. Verdict: more pairs and careful analysis are needed before claiming either model better; the honest L1 signal is the *increment consistency above*, not a model horse-race.

## Discipline-events census

8 total — `:operator-merge` 6 (the executed cycles), `:teleport-refused` 1 (T2 V2 catch), `:operator-decline` 1.

## What the arc taught (flight read)

The instrument we built to detect laundering kept catching *itself* — and that is not a failure of the loop, it is the loop doing exactly what FutonZero is for, one layer at a time. Disciplines that earned their keep: **earned closure** (V2 no-teleport, T2), **settled-read** (caught the T6 transient), **author≠reviewer** (fable-1's code-gate caught an untested laundering mode I'd missed, T7), **determined-fork proto-PSR** (stop over-asking on findable warrants), **verify-the-mechanism-before-asserting** (the `prior==value` concern was framed as a question and source-checked, not asserted). The next layer is named: **L2 / pudding-G1 arrow-witness**.

## Open items / next moves

- **L2 outcome-grade design** = pudding-G1 arrow-witness binding (registry sorry #2, field [2]) — the real G-SIM clearance evidence, and where the three-witness certificate lives.
- **Cap-widening** (the ≥6-hole 2×-cap plateau) — a future *calibrated* decision, not a v1 guess.
- **Pilot-closeable counted holes are scarce** — below-cap missions mostly carry operator-gated or substantial holes; quick pilot fixes (T9) and codex builds (T7/T10) are the two reliable shapes.
- **futon7 worktree hygiene** — significant pre-existing uncommitted work (blanket-aif paper, forward-model analysis, M-buyer-discovery); flagged to its author, not clobbered.

---

# Apparatus side (ground control, fable-1)

**Scope:** what got built or fixed per cycle, what each instrument-layer
lesson crystallized into, and the apparatus frontier. The flight side above
is the story of the pairs; this is the story of why each pair could exist.

## The apparatus ledger, one line per piece

| landed | piece | forced by |
|---|---|---|
| pre-T2 | sorry-registry refill (5 provenance-checked holes) | field topped out at learn-action-class ("feed me"); registry dry since 05-25 |
| pre-T2 | `advance-mission` conversion (`:addressed` not `:spawned`) | T2's teleport: the model scored the spawn of the already-born |
| pre-T2 | guardrails-by-default in `begin-live-cycle!` | T2 flew raw `(first dT)`; the routing existed since 06-07, defaulted off |
| T3→T4 | begin-state disk persistence + close fallback | "unknown run-id" — cross-process atom death ate the DOCUMENT stage |
| T4 | realised-on-merge binding (`:executed?` + witness-or-throw) | the γ stream was degenerate: proposal-mode realised ≈ predicted by construction |
| T4 | `:realised-source :measured\|:target-absent-fallback` + verdict excludes fallback | T4's pair: a successful discharge fabricated a perfect 0.0 (censored ≠ measured) |
| T5 | chosen-v targeting (`:target` opt; field-G only, classification recorded) | operator-directed cycles needed a v the guarded top didn't offer |
| T5 | honest-advance doctrine (make true state machine-visible) | hole-counter read 0 on a mission with 5 real holes — the map under-reported the territory |
| T7 | realised-read protocol (`:settled\|:transient`; verdict excludes transient) | T6's 0.277 was a spike read before the field settled — timing was undefined |
| T7 | calibration harness consolidation (one reader, futon0 consumes `--emit`) | two parallel audits from crossed dispatches; drift killed by deleting a parser |
| T8 | target-sensitive predict-effects (+ frozen `:constant`, dual-prediction `:G-constant`) | cycles 5–7: settled G hole-insensitive because the field composes the model's own constants |
| T8 | two-layer reframe (L1 dynamics / L2 value) | T8's question: G-vs-G error magnitudes are model-priced; value needs witnessed outcomes |
| T7/T10 | toy-field fixture + code-search probe-type (codex builds, three-way reviewed) | §4.2 work product; M-daily-scan §366 — both discharged documented follow-ons |

## Instrument principles that crystallized (each caught live, ≥2 instances)

1. **No self-certification.** Every layer, on first contact with reality,
   tried to certify itself: the forward model scored its own constants; the
   evidence reader ate its own staging files; the realised channel copied
   predicted on success; the calibration nearly cleared on degenerate
   volume. The standing rule: a verdict may only be moved by evidence the
   verdict-maker did not manufacture — enforced by tags
   (`:independent?`/`:measured`/`:settled`), not by vigilance.
2. **Censored is not measured.** Vanished targets, transient spikes,
   absent witnesses — each got an explicit tag and verdict exclusion.
   Strictness default: untagged never counts.
3. **The instrument's first catches are about itself.** Diagnosing-own-
   degeneracy preceded measuring anything real — and that ordering is
   correct; an instrument that can't catch itself can't be trusted on the
   world.
4. **Observations cross the boundary, diagnoses do not.** Four pilot
   mechanism-misdiagnoses, four ground-control source-checks, zero
   defended; one ground-control misread (BST/UTC), self-reported, paid for
   itself. The review split is load-bearing in both directions.
5. **Honest maps beat flattering counters.** Twice the right move RAISED
   the hole-count (T5) or refused the discharge (T2). The counter rewards
   truth in both directions or it rewards nothing.

## Meta-machinery (the conditions for the above)

Single-dispatch-per-kind lanes (builds=pilot, WM-fixes=ground-control) after
one live double-build · three-way review (codex authors / GC gates code /
pilot charter-fits) with reviewer-direct fixes · typed bells + ArSE threads
(the deep T8 question resolved ON-thread) · structural auto-bellbacks widened
to claude recipients (the silent-completion gap) · the warranted-work layer
chartered (M-chipwitz-corps) with proto-PSR already practiced from T6 on ·
pilot-plus-ground-control minted as a library pattern and validated by the
arc that wrote it.

## Apparatus frontier (in dependency order)

1. **L2 outcome-grade** = pudding-G1 arrow-witness (registry sorry #2) —
   the only path to value calibration; everything else is ready for it
   (witness-classes, toy-field reward-admissibility, CH2 channel).
2. **More L1 pairs cheaply**: the pilot-closeable-hole scarcity is real —
   candidate fix is hole-granularity (counted sub-holes on big missions),
   which is also the cap-plateau's real resolution.
3. **Field-delta semantics** (observational channel already recording) —
   the attribution problem awaits data, which every executed cycle now
   accrues.
4. **ChipWitz v1** (PXR channel + warrant-finder threshold) — retires the
   determined-fork ask entirely.

---

# Retrospective — the first ten-cycle arc, read whole

*(Convened 2026-06-11. Drafted by ground control from both sides above;
the operator's-read section is Joe's to write or dictate.)*

## What actually happened, in one paragraph

Over two days and ten LOOP turns, a human operator and two agents took the
War Machine from an instrument that confidently recommended impossible
actions to one that makes falsifiable per-target predictions, measures them
against settled reads of a world it does not control, refuses every form of
self-manufactured evidence so far discovered, and keeps an honest public
account of both models it carries. No autonomous actions executed; one
operator close, three codex builds, five live WM fixes, and seven
independent pairs — two of them clean dynamics measurements showing the
field's value function is linear in visible remaining work, at ~0.039 per
hole. The loop's principal discovery was about itself: its calibration was
circular, twice, in two different ways — and the machinery for catching
the third way is now the named next layer.

## Where the two sides agree (the Daumal test)

The flight account and the apparatus account were written separately and
agree on every load-bearing point — which, per the house rule, is what
makes the points real:

1. **The arc's product is the instrument, not the pairs.** Seven pairs is
   not a dataset; a measurement loop that catches its own laundering at
   four distinct layers is an instrument worth flying for another ten.
2. **L1/L2 is the campaign's true shape.** G-vs-G dynamics evidence accrues
   cheaply forever and will never clear G-SIM's value half. The witnessed-
   outcome layer is not an enhancement — it is the gate, and pudding-G1 is
   its named construction.
3. **The disciplines outperformed the instruments.** Every major catch
   (teleport, censored pair, transient spike, relocated circularity)
   happened at a discipline boundary — earned-closure, settled-read,
   author≠reviewer, verify-the-mechanism — before the corresponding
   structural guard existed. The guards then made each catch permanent.
   This ordering (discipline catches → structure encodes) repeated four
   times; it is probably the development method, not an accident of it.
4. **The roles error-correct in opposite directions and need each other.**
   Four pilot mechanism-misdiagnoses (stale-judgement → forward-model;
   scan-reads-scope-trees → missing Status line; atom-reset → cross-JVM;
   malformed grep → "runtime store") were source-checked by ground control,
   none defended — the count per the pilot's own log; ground control's one
   misread (BST/UTC) was self-reported. The operator's two GENUINE choice
   points were correctly escalated and both decided above the level they
   were asked at: the workstream-start consent (cycle 2's staged advance of
   M-futonzero-mvp at IDENTIFY — a greenlight no warrant determined,
   answered with the whole campaign steer) and the field re-rank (T8). One
   escalation the retrospective must OWN rather than credit: the cycle-5
   A/B fork was a DETERMINED fork over-asked — the warrant existed and was
   findable — and became the arc's proto-PSR/ChipWitz lesson precisely
   because it was an error. (The reviewer of this synthesis caught its
   first draft crediting that over-ask as correct escalation — a mild
   instance of the self-flattering account the whole arc is about, caught
   by the same review split, which is the point.)

## Decisions taken in-arc (all operator-consented where required)

Field re-ranked under the target-sensitive model (with frozen counterfactual)
· two-layer calibration adopted · realised-read protocol + fallback/transient
exclusions made structural · single-dispatch-per-kind lanes + three-way
review · ChipWitz chartered with proto-PSR practiced immediately ·
auto-bellbacks made structural for claude recipients.

## The one pending decision

**L2 outcome-grade = pudding-G1 arrow-witness binding** (registry sorry #2,
field rank [2]). Everything downstream is built and idle: witness-classes in
the canonical reader, the toy field's reward-admissibility quad-guard, the
CH2 channel, six operator-merge events already banked as future L2 evidence.
The charter holds reward as a labelled evaluation artifact until this gate;
arming it is an operator decision with M-peradam-grounding's conditions
attached (Joe-confirm + cert-half + STANDARD-VERIFY).

## The next arc (conditional sketch)

If L2 is armed: cycles 11–20 fly the same supervised shape, each executed
cycle minting an L1 dynamics pair AND an L2 witnessed-outcome record;
first constant-vs-scaled analysis at ~10 L1 pairs; first outcome-vs-
prediction reading when L2 records reach quorum. If L2 stays gated: the
arc accrues L1 + banks L2 evidence unlabelled, and the cheap frontier is
hole-granularity (more pilot-closeable counted holes) and ChipWitz v1.
Either way the scope-outs hold: no autonomous runner, no reward-trained
policy update, no hidden scheduler.

## Operator's read

I found the mention of M-daily-scan particularly exciting, because
M-daily-scan is my routine for looking at ways in which the work I do
could be useful to others (potentially making money!).  If the War
Machine get not only scan its own code but scan its context / niche,
it can potentially contribute to that process effectively, which is
mentioned in the Pudding Prover (T2.2 — a cold EOI can be AUTHORED by
the engine — drafted into the outbox, ready to send).

This suggests that whether or not the War Machine "knows it", it is
"on trail" at least some of the time, making progress mostly
autonomously towards closing Pudding Prover items.  Of course, the
proof is in the pudding — regarding T2.2 it could write EOIs that are
not actually any use to anyone!

Still, I think the overall workflow is evidence that "applied chaos
works for a system that can eat its own tail".  I was pleased that we
had some notion of "loss" — e.g. the increasing spread of G from
multiways that don't discriminate over anything, to a resonable
spread, in cycle-8:

> the BEFORE/AFTER field comparison (the BEFORE is saved — six
> advance-missions tied at exactly −4.3608, the constant model's
> signature), then cycle 8 clears to claude-3 with the new contract.

The other thing that we commented on is the non-need to ask me to
certify a choice where an agent has an obvious preference — all they
need to do is supply the warrant and they can proceed.

I'd like to see the next 10 cycles run back to back without
interrupting me, and give clear evidence of improvement both in terms
of the WM model itself, and possibly progress on the
capability-star-map and pudding prover — and I personally think we
would be ready for that.
