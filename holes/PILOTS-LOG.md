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
