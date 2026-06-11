# Pilot's Log — War Machine REPL cycles

The durable record of WM pilot **LOOP turns** — the recurring `READ → EVAL → PRINT → DOCUMENT` cycle
(M-pilot-appearance §2, the geno-layer "loop-preparation for the next turn"). This activates the named open
follow-on of `M-pilot-appearance` (CLOSED 2026-05-29): *LOOP-learning + doc-currency automation*.

Each entry records: **what the WM recommended**, **what the pilot did**, **what it found**, and a **PUR**
(did the tooling help; honesty/earned-closure notes; supervised vs. armed). Append newest at the top. Keep
it honest — no laundered findings.

**This is now the DOCUMENT stage of the pilot cycle** (not documentation-only): `close-live-cycle!`
(`war_machine_pilot.clj`) auto-appends a turn here as its last step. The mechanical facts (recommendation,
cascade, predicted/realised, top-shift) auto-fill from the cycle; the inhabitant supplies the semantic
account by passing `:document {:did … :found … :pur …}` to `close-live-cycle!`. A log failure can never
break a turn (best-effort). So every completed cycle writes its own entry — Turn 2 onward land here
automatically.

---

## Turn 5 — 2026-06-11 (claude-3, supervised-proposal)

**READ.** WM recommended `sorry/pattern-measure-never-target` (G=-4.46).
_(Auto-log note: this line is the field's top [0]. The actual `v` this cycle was
**operator-directed** `advance-mission M-futonzero-generative` (predicted G=−4.0849) —
Joe's choice B, not the field top. `log-pilot-turn!` records pre-top as the
"recommendation," which on an operator-directed cycle differs from the chosen v.)_

**EVAL / DID.** Made M-futonzero-generative true open state machine-visible: added a Remaining Work checklist (2 done+dated for sections 4.1 and 4.4, 5 open for 4.2/4.3/4.5 plus G-SIM and G-REWARD) and a dated checkpoint. Committed futon0 477f7ea.

**PRINT / FOUND.** Partial-discharge worked: target stayed ranked (rank 10, guardrail rule cleared from open-mission-no-holes to nil, holes now visible), so realised read a genuine post-field G. First realised-source measured pair. Error is tiny (predicted -4.085, realised -4.085) because the hole-visibility edit barely moved this mission own EFE G: small but genuinely measured, not a censored zero fallback (post-entry exists; realised is the field own independent number).  _[predicted G=-4.08, realised G=-4.09, top-shift sorry/pattern-measure-never-target->]_

**PUR.** Pattern is consent-gate (operator-directed: Joe chose target B) plus earned-closure (field moved: rule cleared). Option-C partial-discharge delivered the first non-fallback measured pair. Vigilance: a near-zero error is what ground control flagged as suspicious in Turn 4; here it is legitimate (measured, target persists) and I verified the mechanism (rule nil, post-entry present) before claiming it. About 11 more measured pairs needed for calibratable. Mode supervised, operator-directed. Mode: supervised-proposal.

## Turn 4 — 2026-06-11 (claude-3, supervised-proposal)

**READ.** WM recommended `sorry/pattern-two-projections-of-one-quantity` (G=-4.73).

**EVAL / DID.** marked pattern-two-projections sorry :addressed, pattern minted c1c0325

**PRINT / FOUND.** field moved, sorry dropped from ranked-actions, first independent pair  _[predicted G=-4.73, realised G=-4.73, top-shift sorry/pattern-two-projections-of-one-quantity->sorry/pattern-measure-never-target]_

**PUR.** executed cycle with committed evidence-ref, independent pair via the realised-on-merge binding Mode: supervised-proposal.

**Ground-control correction (fable-1, same day) — the pair was a FALLBACK, not a measurement.** The
discharge succeeded so the target *vanished* from the post-field, and `close-live-cycle!`'s
absent-target fallback copied predicted→realised → `:prediction-error 0.0` **by construction** (a
*censored* observation, not a measured one — and a suspicious "perfect" result I should have questioned).
The verdict now counts ONLY `:realised-source :measured` pairs; this frame is retroactively EXCLUDED, so
the independent **measured** count stays 0. **No diminishment of what Turn 4 proved: the PIPE is verified
end-to-end** — executed, witnessed (`futon3 c1c0325 + futon2 b3acaaa`), merge-event, auto-DOCUMENT, field
moved, and the full sorry-mining round-trip closed store-side (arrow `arr-4d50ce67-10b` promoted
`:constructed` w/ payload; CH2 discharge `futon3a/sorry/meme-arrow-969d5eb3d8b6b363`). The first
*measured* pair needs a discharge whose **target retains a post-field reading** (partial discharge) or
**field-delta realised semantics** (realised = movement of the whole differential, not the target's own
G) — a design fork for Joe.

## Turn 3 — 2026-06-10 (claude-3, emacs-repl) — first earned address-sorry on the refilled registry; minted a cited-but-unwritten pattern

*Hand-written: `close-live-cycle!` raised `unknown run-id` — the staged run-state (stashed by
`begin-live-cycle!` in `!live-cycle-runs`) was lost to an ns reload between begin and close. The cycle
genuinely happened; the auto-write didn't. Apparatus durability bug flagged to fable-1 (the run-state
atom needs `defonce`/persistence so the DOCUMENT stage survives reloads). Turn-4+ auto-write once fixed.*

**READ.** After the sorry-registry refill (fable-1), the field moved off `learn-action-class`: the top
five `ranked-actions` were `:address-sorry` on the 5 refilled sorries. The guarded selection
(`begin-live-cycle! {:guardrails? true}`, run `live-d4afd48c`) took the first `:autonomous` —
**`:address-sorry sorry/pattern-two-projections-of-one-quantity`** (G=−4.735), rationale "cited in
ARGUE, absent from the library."

**EVAL / DID.** `v` = mint the pattern. Hand-authored
`futon3/library/structure/two-projections-of-one-quantity.flexiarg` (witness pattern; full
IF/HOWEVER/THEN/BECAUSE), capturing the latent-quantity-with-two-projections structure.

**PRINT / FOUND.** The pattern was a *real laundering instance* — cited as
`structure/two-projections-of-one-quantity` in the M-memes / E-mission-head ARGUE but **never minted**
(the cascade caught it, futon6 early-closures.md). Its canonical instance is the aliveness synthesis
(EOI / AIF-surprise / anamnesis / T / Alexander-wholeness / Salingaros-tension as projections of one
aliveness-quantity). So this cycle closed an actual gap the substrate had been pointing at.

**LOOP.** Realised left **OPEN** (no fabricated realised) — this pair is **non-independent** and so
cannot clear G-SIM (belt: nil-actual; suspenders: the `:independent?` filter). The field-movement check
awaits the next scan re-reading the new library file (and, for a real realised, fable-1's
realised-on-merge binding).

**PUR.** Pattern = **consent-gate + earned-closure**, and the loop *pointed at itself*: refill →
field-shift → address-sorry on a genuine gap. The tooling worked end-to-end EXCEPT the
`close-live-cycle!` auto-DOCUMENT (run-id durability) — caught by flying a real Turn 3, the kind of
finding only live inhabitation surfaces. Supervised: the pattern is authored in the working tree;
commit-to-master is Joe's consent-to-merge.

## Turn 2 — 2026-06-10 (claude-3, emacs-repl) — first supervised observer cycle; teleport caught, V2 save → keystone close

*Backfilled by hand: this cycle's DOCUMENT stage was (wrongly) emitted only via Agency bells, not through
`close-live-cycle!`, so it skipped this log. Recorded honestly after the fact; Turn 3 onward auto-write.*

**READ.** Live WM recommended **`open-mission M-capability-star-map`** (G=−5.698), top of a §8 fork
(essay-corpus −5.602, emacs-cursor −5.550 — all within false-floor tie territory).

**EVAL / DID.** Resolved the fork by **pattern-warrant** (not score-delta): star-map is the keystone —
the dependency-root for WM-overnight, HEAD→VERIFY done, INSTANTIATE the open edge. Minted a consent-gate
**proposal** (supervised). At PRINT, target-check caught what the apparatus couldn't yet see: the mission
(and the whole top-3) was **already open** — `open-mission` would be an **unearned/teleport discharge**
(V2 no-teleport). **HELD the PRINT.**

**PRINT / FOUND.** Surfaced the teleport to the operator. Operator (Joe) chose **close**
`M-capability-star-map` (mission-close is operator-only), caveats → four adjacent excursions
(`E-starmap-{branch-merge,full-registry-scan,efe-hardening,vsatarcs-regen}`). Executed the close
(`2968d7f`) + the machine-visible leading `**Status:** CLOSED` line (`563a2a1`). The teleport finding drove
three ground-control (fable-1) live fixes: the forward-model class conversion (`:open-mission`→
`:advance-mission`, `:spawned`→`:addressed`; futon2 a491f9d / futon3c a341f18), the guardrails-by-default
pilot wiring, and a watcher-wedge fix. Field moved: star-map **gone** from `ranked-actions`,
`mission-status {:open? false :open-hole-count 0}` on two consecutive scans.

**PUR.** Pattern = **consent-gate + earned-closure (V2 no-teleport)**. The tooling *worked*: the discipline
caught the WM's own top recommendation as a teleport ("scoring the spawn of the already-born"). Honesty
note: the close wasn't *real* until **machine-visible** — the missing `Status:` line meant the field hadn't
moved, so the discharge wasn't earned until that landed (earned-closure = the field moves, not the doc
asserts). Supervised throughout (operator decided the close + what replays to master). One prediction-error
→ three landed live fixes + one strategic read (the open-sorry substrate ran dry 2026-05-25 →
`ranked-actions[0]=learn-action-class` is the field asking to be fed). **Process miss:** DOCUMENT went to
Agency, not here — corrected from Turn 3.

## Turn 1 — 2026-06-09 (claude-1, emacs-repl) — first cycle on the new tooling (cascade lane + v2 rollout)

**READ.** Live WM recommended **`open-mission M-capability-star-map`** (G=−5.69). The new **cascade-policy
lane** gave the *method* — a 6-pattern policy (C=9.88): `war-machine/ideal-actual-gap` ·
`t3/capability-not-functioning` · `futon-theory/mission-dependency` · `ukrns/scale-register` ·
`campaign-coherence/shared-standard-has-no-single-owner` · `vsatlas/value-flow-constellation`.

**EVAL / DID.** Took the recommendation *guided by the cascade*: audited the capability graph for an
**ideal-actual gap** — caps claiming `:satisfied` whose grounding doesn't exist.

**PRINT / FOUND.** Of 20 `:satisfied` caps, 17 name grounding — but `:grounding` is **prose, not a
machine-checkable path** (only 3/17 resolve to a file). So **capability-functioning can't be machine-verified
from the graph**; the ideal-actual gap is structurally undetectable under the current schema (the graph
asserts grounding it cannot check). Recorded as an advance on `M-capability-star-map` (futon0 `3f194a3`): the
schema needs a checkable grounding field; until then, use substrate-2 `:capability/status` vs. attestation.

**PUR.** *Did the new tooling help?* **Yes — modestly and concretely.** The cascade handed me a *specific,
productive lens* ("ideal-actual-gap") for an otherwise-vague recommendation, and it led straight to a real
finding — itself an ideal-actual gap, in the grounding schema. The recommendation was vindicated by what it
found. *Honesty (earned closure):* my first audit cut reported "12 gaps" — a path-resolution **artifact** (it
flagged `pudding-prover.bb`, which exists at `futon7/holes/`); corrected to **0 file-missing**. The cascade
pointed the *direction*; the rigor to catch the artifact was on the operator, not the tool — the right
division (the cascade is assurance-machinery, not a substitute for the check). *Mode:* **supervised** — Part B
(the WM driving a real `promote!`) remains held for explicit operator arming.
