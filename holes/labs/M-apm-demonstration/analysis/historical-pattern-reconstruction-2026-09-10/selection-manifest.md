# Selection and source manifest — first reconstruction packet

Owner: `zai-pattern-reconstruction-20260910`. Commissioned 2026-09-10 by Joe
via codex-16 (see `../COMMISSION.md`). Frozen before detailed reconstruction;
no member changed after this freeze.

- apm-lean source pin (all six): `6da564602379fefb7a9b15a27e4175dadd2e8217`
  (working tree at that commit; the six files below carry no later commits).
- futon3c (this repo) at packet time: `767d7e66f7585826ba42e11a70acc8f96db4abdf`.
- futon3 canonical library census pin (prior work): `a376e4043369167ec8b491a77ddc4f9695a6d991`.

Selection rule from the commission: six unsealed development solves, three
pairs sharing a plausible intermediate construction, at least two
mathematical areas, a93A01 excluded (owned by the ongoing transfer pilot;
this packet made no contact with the pilot's corpus, Student or TA).

## The six, as three pairs

| Pair | Problems | Area | Shared intermediate construction |
|---|---|---|---|
| A | t01A03, t91J02 | algebraic topology | retraction (left inverse) ⇒ functorial-invariant injectivity ⇒ nonexistence |
| B | t93A04, t92J07 | differential topology / measure theory | null critical-value set + positive-measure target ⇒ choose a good value |
| C | a97J04, a94A02 | real analysis (AC / series of monotone functions) | summable/integrable dominator licenses termwise limit interchange; auxiliary object then identified with the target |

Two mathematical areas are covered (topology family A/B; real analysis C),
with B additionally measure-theoretic.

## Per-problem source pins

All paths relative to apm-lean root. `sorry` counts are literal `grep -c
sorry` over the pinned Main.lean at the pin above (source inspection, not a
fresh Lean elaboration).

| Problem | Statement sha256 (problem.md) | Solution sha256 (lean/Main.lean) | sorries | bundle status.json says | note |
|---|---|---|---|---|---|
| t01A03 | 7124f133…33c8984 | aa488276…de26bfee | 0 | solved / 0 | consistent |
| t91J02 | 0164bfb8…b96df5fa2 | c670eeec…11a6782 | 0 | solved / 0 | consistent |
| t93A04 | 2a6c2436…492f9902 | 5baf0e7d…90eb8c2f | 0 | **stale**: partial / 1 | status.json last touched 2026-08-19; solve closed 2026-09-09 (`a49c8476`, `337bdd48`); Main.lean at pin is sorry-free |
| t92J07 | 5ba2ed69…29f02344e | 0e7bf954…bedc943e3 | 0 | **stale**: partial / 2 | same: closed 2026-09-09 (`b364803a`); sorry-free at pin |
| a97J04 | 5e21b952…05b29e8 | 19e2a620…b6dcef881 | 0 | complete / 0 | consistent |
| a94A02 | bbeaf420…e6d2db4a | d86c4062…c550ec6 | 0 | solved / 0 | consistent |

Unsealed status: none of the six appears in any holdout/seal manifest
inspected (apm-lean root listing, M-diagramprover driver surfaces); all six
were worked through the ordinary M-diagramprover campaign ledger
(`holes/labs/M-diagramprover/apm-driver/campaign-ledger.jsonl`, futon3c).

## Author provenance (execution records only)

Git commit authors in apm-lean are uniformly Joe/Joseph Corneli (local seat
commits), so authorship is taken from the campaign ledger's seat fields and,
for a97J04, an in-source memory citation. Never inferred from filename/style.

- **t01A03 — Codex.** Dispatched to `codex-2` (2026-08-06), repaired under
  `ams-codex-1`, closer hops `ams-codex-2`/`ams-codex-1` (2026-08-07/08).
- **t91J02 — Codex.** Statement batch seat `codex-6`; closer hops
  `ams-codex-1`/`ams-codex-2`.
- **t93A04 — Codex.** Statement seat `codex-2`; repair `ams-scribe-1`;
  closer hops `ams-codex-2`/`ams-codex-1`.
- **t92J07 — Codex.** Statement seat `codex-7`; repair `ams-scribe-1`;
  closer hops `ams-codex-1`/`ams-codex-2`.
- **a97J04 — Zai (zai-1), with a Codex-authored reviewed memory.** The only
  closer hop recorded is `zai-1` (2026-08-06T22:46Z). The Lean source
  comments cite memory
  `e-codexpilot-extend-local-absolute-continuity-to-an-endpoint-via-the-derivative-primitive`
  (author claude-6, scribe-author codex-5, runner codex; reviewed/approved),
  i.e. the route was seeded outside zai-1's own work.
- **a94A02 — mixed Zai/Codex.** Closer hops recorded for both `zai-1`
  (2026-08-06T22:41Z, 23:21Z) and `ams-codex-1` (2026-08-07T08:56Z). No
  single-agent attribution is defensible; recorded as mixed rather than
  resolved.

Unknown-author category: not needed for these six, but no inference was made
where the ledger was ambiguous (a94A02 stays "mixed").

## Statement repairs actually present (preserved, not silently fixed)

- t92J07: source typo `f_i : M_x → ℝ^m` repaired to `f_i : M_i → ℝ^m`;
  Hausdorff/second-countability made explicit (required by Sard).
- t93A04: no repair; the "smooth line preimage" is a *defined certificate*
  (transversality to an explicit affine line), not an independently
  constructed submanifold predicate.
- t01A03: pictorial statement concretized; genus pinned by integral homology
  iso conditions; the three obstruction inputs (H₁ of middle circle = 0,
  noninjective wedge π₁ map, connectedness) are hypotheses, so the theorem is
  a conditional reduction — this is recorded as such in the reconstruction.
- t91J02, a97J04, a94A02: no statement repairs; a97J04 encodes `[0,1]`
  functions as ambient `ℝ → ℝ` restricted to the interval (an explicit
  encoding note, and the original informal problem is faithfully represented).
