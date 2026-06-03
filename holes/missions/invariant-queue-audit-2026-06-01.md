# Invariant Queue Audit — 2026-06-01 (claude-2, read-and-advise)

**Task (Joe):** the Invariant Queue is machine-generated, priority-sorted; which
entries are actually worth devoting cycles to, and have some already been built?
**Source audited:** `futon3c/docs/structural-law-inventory.sexp` (172 entries; the
`futon4/futon-stack-invariant-model.edn` is an Arxana hypergraph view of it).
**Scope:** the **12 `:candidate` families** (the unbuilt backlog). The 37
`:operational` + 16 `:operational-when-enabled` are already built/binding and out
of scope here. **Method:** for each candidate family — (1) is it secretly already
built (`:implemented-in` present)? (2) is it worth cycles, and why/why not?
**Discipline note:** this is a desk audit of the inventory's own claims; I did NOT
re-verify each `:implemented-in` path exists in code — flagged where that matters.

## Headline findings

1. **"36 candidates" is misleading — it's 12 families** (the 36 is `:status
   candidate` *lines*, which include sub-invariants). Audit at family grain.
2. **4 of 12 families are already partly-or-mostly built** but still labelled
   `:candidate` — the inventory is stale, not the work. The single biggest item,
   `cross-store-agreement`, has **23 operational sub-invariants of 48** — it is
   largely built and mis-classified. **Cheapest win: promote, don't build.**
3. The Joe-2026-04-29 reframe (queue = *projection* of an alive structure, not an
   enumeration to grind through) means the right output of this audit is **triage
   into {promote / drop / genuinely-build}, not "do all 12."**

## Per-family verdict

| Family | already built? | worth cycles? | verdict |
|---|---|---|---|
| **cross-store-agreement** (48 subs, 23 operational) | **largely BUILT** | high (it's the registry/ledger/Arxana consistency law — load-bearing) | **PROMOTE** the operational subs; audit the gap (25 non-op subs) — mostly a labelling + finish-the-tail job, not greenfield |
| **archaeology-control** (6 subs, 6 operational) | **BUILT** | high (it's the fatberg-prevention law the unstuck mission is *about*) | **PROMOTE to operational**; it appears done — verify the `:implemented-in` paths then re-status |
| **atomic-inspectable-units** (6 subs, 3 operational) | **half BUILT** | high (checkout/checkin/inspectable-boundary = the work-unit discipline) | **finish + promote** the 3 unbuilt subs; real value, half the work already exists |
| **metabolic-balance** (1 sub, operational) | **BUILT** (working-tree drain, tested, calibrated) | high (operator allostatic load — directly the "don't drown the operator" faculty) | **PROMOTE**; it's coded + tested + calibrated against the 2026-05-03 sweep. Mislabelled candidate. |
| **failure-locality** (3 subs) | no | **high** | **BUILD** — "failures surface near the layer that caused them" is exactly the discipline whose absence caused the bot-evidence fatberg (silent shape-rejected appends in 28 sites). Highest-value *unbuilt* family. |
| **interaction-evidence-continuity** (4 subs) | no | **high** | **BUILD** — "interactive agent work lands in the ledger or is visibly marked lost" — directly the bot-evidence loss class; pairs with failure-locality. |
| **budgeted-action-selection** (6 subs) | no | medium-high | **BUILD (scoped)** — action selection constrained by budget/license. Relevant to the autorunner/consent-gate work; but 6 subs is large — scope to the consent-gate-adjacent ones first. |
| **human-visible-inspectability** (3 subs) | no | medium | **BUILD or fold** — "operator can tell what is happening" overlaps metabolic-balance + the projection apparatus of the unstuck mission. May be a *projection* concern, not a separate invariant. Check for overlap before building. |
| **repo-role-clarity** (4 subs) | no | medium | **defer** — declared-role / root-legibility / main-branch-coherence are hygiene; real but low-urgency, and partly enforced socially already. Good "ratchet rung," not a priority. |
| **artifact-custody** (4 subs) | no | medium | **defer / fold into metabolic-balance** — "outputs land where expected" is adjacent to artifact drain channels already in metabolic-balance. Likely a sibling, not standalone. |
| **peripheral-custody** (4 subs) | no | low-medium | **defer** — peripheral session domain-id discipline; matters only if peripheral drift is an observed problem. No evidence it is right now. |
| **strategic-closure-specification** (2 subs) | no | medium | **BUILD (cheap)** — "strategic recs promoted only when their next closure step is specified" is small (2 subs) and directly anti-chained-claim / anti-fatberg. High value-per-cycle. |

## Tier-A path-verify RESULT (2026-06-01, the caveat discharged)

Verified every `:implemented-in` path cited by the 4 Tier-A families actually
exists on disk (resolving repo-root prefixes: futon1a paths live under
`futon1a/src/futon1a/`, futon-sync under `futon0/scripts/`):

- `archaeology-control` → `futon3c/src/futon3c/logic/archaeology.clj` + test ✅
- `metabolic-balance` → `futon3c/src/futon3c/logic/metabolic_balance.clj` +
  `disposition_edn.clj` + both tests ✅
- `atomic-inspectable-units` → `futon3c/src/futon3c/logic/locus.clj` + test ✅
- `cross-store-agreement` → futon1a `core/pipeline.clj`, `core/entity.clj`,
  `system.clj`, `model/verify.clj`, `auth/penholder.clj`, `api/errors.clj`;
  futon3b `gate/pipeline.clj`, `gate/canon.clj`; futon3c `peripheral/mission*`,
  `structural_law.clj`, `proof_logic.clj` — all present ✅

**Conclusion:** Tier-A code is real, not phantom. The "promote, don't build"
recommendation is verified-sound. The re-status itself (mutating `:status
candidate → :operational` in the source-of-truth `structural-law-inventory.sexp`)
is **deliberately NOT done here** — that file is the shared inventory owned by the
invariant-queue mission line (claude-1); a status promotion is an owner decision,
not something to apply unilaterally in a loop. This audit supplies the verified
evidence FOR the promotion; the promotion is theirs to ratify.

## Recommended triage (the actual answer to "what's worth cycles")

**Tier A — promote, ~no build (cheapest, do first):** `metabolic-balance`,
`archaeology-control`, `cross-store-agreement` (op subset), `atomic-inspectable-units`
(op subset). These are built-but-mislabelled; the cycle cost is *verify the
`:implemented-in` paths still hold + re-status*, not implementation. This alone
shrinks the "queue" by a third and is the single highest ROI.

**Tier B — build, high value (the genuine backlog):** `failure-locality`,
`interaction-evidence-continuity`, `strategic-closure-specification`. All three
target the exact failure class that motivated M-invariant-queue-unstuck (silent
loss, fatberg, chained claims). `strategic-closure-specification` is the cheapest
(2 subs). Build these *with canaries* per the unstuck mission, or they become new
fatberg.

**Tier C — defer or fold (don't spend cycles yet):** `repo-role-clarity`,
`artifact-custody`, `peripheral-custody`, `human-visible-inspectability`,
`budgeted-action-selection` (beyond consent-gate scope). Several are likely
*siblings* of existing families (artifact-custody↔metabolic-balance,
human-visible↔projection apparatus) rather than standalone work — fold-or-drop
candidates, not build candidates.

## Caveats (honest)
- I judged "already built" from the inventory's own `:implemented-in` fields; I
  did **not** open each cited `.clj` to confirm it still implements the claim.
  Tier A's "promote" recommendation should be gated on a 10-min verify of those
  paths (the inventory has been stale before — that staleness is the finding).
- "Worth cycles" is my judgement against the unstuck-mission's stated purpose
  (anti-fatberg, anti-silent-loss). It is advice, not a ruling — ratify or push back.
- Per the queue-as-projection reframe: the deepest move may be that Tier C
  families shouldn't be *enumerated* at all — they should fall out of the
  projection apparatus. Folding > building for those.
