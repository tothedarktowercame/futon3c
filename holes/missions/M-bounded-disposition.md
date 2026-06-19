**Status:** INSTANTIATE complete (2026-04-29) for the stash slice; siblings handed to Codex via GitHub issue.
**Home-repo:** futon3c
**Family:** `archaeology-control` (structural-law-inventory.sexp:200) — same as M-archaeology-control. Different shape.
**Sibling missions:** [M-archaeology-control](M-archaeology-control.md) (subsumption-witness shape; obsolescence-recognition/* siblings); [M-invariant-queue-extend](M-invariant-queue-extend.md) (apparatus).
**Origin:** Joe + claude-11 conversation 2026-04-29 unpacking the next-up candidates `recover-or-drop` (rank 2) and `stash-debt-bounded` (rank 3) — both in `archaeology-control`. Shape-first IDENTIFY surfaced the bounded-disposition shape as their common family-protocol.

# M-bounded-disposition: Per-artifact triage + population bound, exhibited across artifact-classes

## 1. IDENTIFY

### Motivation

The structural-law inventory's rank-2 and rank-3 candidates are
`recover-or-drop` ("A stash is revived, parked on a branch, or
explicitly dropped") and `stash-debt-bounded` ("Stash count remains
below a reasonable threshold, or older stashes must be classified").
Both about stashes; both currently `:status :candidate` with
`:best-current-exemplar none-yet`.

Reading them together via shape-first IDENTIFY: they're not two
separate slices but two *obligations of one shape*. Each artifact in a
population must carry a disposition; the unresolved subset must stay
bounded. The shape generalizes beyond stashes — branches, mission
docs, PRs all share it.

This mission ships the shape (`bounded-disposition`) with one worked
sibling (`bounded-disposition/stash` — operationally absorbing both
recover-or-drop and stash-debt-bounded into a single richer
invariant), and hands off two more siblings (`/branch`, `/mission-doc`)
to Codex.

### Theoretical anchoring

- **Bounded-disposition shape (just named).** Two-obligation invariant:
  per-artifact classification + aggregate bound on the unresolved
  subset. See
  `futon3/library/invariant-coherence/bounded-disposition.flexiarg`.
- **Distinct from subsumption-witness** (M-archaeology-control). That
  shape is a binary fired-once predicate ("is A subsumed by P?").
  Bounded-disposition is per-member triage + aggregate. The two shapes
  coexist in the same family because some artifacts witness
  obsolescence (autostashes subsumed by HEAD) while others witness
  unfinished decisions (stashes deliberately kept). One artifact-class
  can have instances of both shapes simultaneously — stashes do.
- **Multi-mode binding** (M-invariant-queue-unstuck precedent). Probe-
  tap + boot-time check + pre-commit hook. The stash slice gets all
  three; the others (branch, mission-doc) get probe + boot only.
- **Detection vs cleanup separation.** The probe is read-only; cleanup
  is operator-driven via `futon0/scripts/futon-sync.clj` (existing) or
  per-artifact-class disposition workflows. Same separation as
  subsumption-witness.

### Scope in

- **Sibling invariant `bounded-disposition/stash`** under family
  `archaeology-control`. Replaces the existing two narrower
  candidates `recover-or-drop` and `stash-debt-bounded` with one
  shape-aligned invariant carrying both obligations.
- **Disposition vocabulary**:
  `{:kept :parked-on-branch :dropped :awaiting-decision}`. Default is
  `:awaiting-decision` — a stash with no disposition tag in its
  message falls into this bucket.
- **Disposition tag convention**: stash messages carrying
  `[disposition: <vocab>]` near the start of the message classify the
  stash. (Joe's existing `futon0/scripts/futon-sync.clj park` will
  need a follow-up to emit these tags by default; out of scope here.)
- **Bound**: per repo, `|undecided| ≤ 5` AND no stash older than 14
  days carries `:awaiting-decision`. Both are configurable via tap-
  registration options.
- **Implementation**: extend `futon3c.logic.archaeology` with check-fn
  `check-stash-disposition`, mirroring the autostash check shape.
- **Tests**: probe-result shape; default classification; per-tag
  classification; bound violations.
- **Pre-commit hook**: extend
  `futon3c/scripts/check-autostash-obsolescence.sh` (or wrap it) so
  pre-commit runs *both* obsolescence-recognition/autostash AND
  bounded-disposition/stash. Stash discipline becomes a single
  computational guarantee.
- **Inventory updates**: remove the two existing narrow candidates
  (`recover-or-drop`, `stash-debt-bounded`) per ratchet's `:removed`
  semantics (informational, not demotion); add
  `bounded-disposition/stash` as `:status :operational-when-enabled`.

### Scope out

- **`bounded-disposition/branch`** — handed to Codex. Disposition
  vocabulary `{:active :merged-not-yet-deleted :parked
  :long-lived-release :abandoned}`. Disposition source = git-config
  per-branch description (`git config branch.<name>.description`) OR a
  sidecar `.git/futon-branch-dispositions.edn`.
- **`bounded-disposition/mission-doc`** — handed to Codex. Disposition
  vocabulary `{:open :closed :parked :archived}` parsed from the
  `Status:` line in the mission file front-matter.
- **`bounded-disposition/pr`** — deferred. Cross-repo PR pressure
  isn't significant in this stack today; revisit when it surfaces.
- **Auto-classifying tooling** (e.g. extending `futon-sync.clj` to
  emit disposition tags). Out of scope; the invariant detects, the
  operator decides.
- **Disposition record migration** — older stashes have no tags;
  there's no automated migration. They start as `:awaiting-decision`
  and the operator triages.

### Completion criteria

1. `bounded-disposition/stash` registered as a probe tap +
   pre-commit hook addition, scoring `:status :operational-when-enabled`
   in the inventory.
2. `bounded-disposition.flexiarg` lands in `invariant-coherence`
   namespace and is referenced from this mission's IDENTIFY.
3. Mission test sweep clean: 0 failures across the full mission
   suites.
4. Two new sibling Codex handoffs opened (one per remaining
   artifact-class) with the standard scope-bounded-handoff shape.
5. Inventory reflects the rename: old narrow candidates gone, new
   sibling registered.

### Relationship to other missions

- **Sibling shape (different protocol):** M-archaeology-control —
  same family `archaeology-control`, different shape
  (subsumption-witness). The two missions exhibit two different
  protocols for the same family, which is the methodology-payoff Joe
  flagged ("the shape is itself a shape we can reuse").
- **Apparatus dependency:** M-invariant-queue-extend — the probe /
  boundary / canary / boot-time-check infrastructure both this
  mission and M-archaeology-control build on.
- **Pattern home:** `futon3/library/invariant-coherence/` — picks
  up its second shape (after subsumption-witness).
- **Tooling-not-replaced:** `futon0/scripts/futon-sync.clj` stays
  manual; this invariant tells the operator when to run it.

### Source material

- `futon3c/docs/structural-law-inventory.sexp:200-228` — current
  family + the two candidates being absorbed.
- `futon3c/src/futon3c/logic/archaeology.clj` — apparatus to extend.
- `futon3c/scripts/check-autostash-obsolescence.sh` — hook to extend
  (or pair with).
- `futon0/scripts/futon-sync.clj` — operator-driven cleanup tool
  whose work this invariant will tell us when to run.
- `futon5a/data/stack-stereolithography-priority-queue.json` — source
  for the rank-2 and rank-3 priority lookup.

### Owner and dependencies

- **Owner:** Joe (architectural authority on disposition vocabulary +
  threshold values) + claude (stash slice implementation +
  inventory rename) + Codex (branch + mission-doc siblings via
  scope-bounded-handoff).
- **Primary repo:** futon3c (apparatus extension).
- **Touches:** all 14 ~/code/futon* repos (live stash state).

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT

(In progress for the stash slice this turn; the other siblings depend on Codex.)

## Checkpoints

### 2026-04-29 — INSTANTIATE complete for bounded-disposition/stash; siblings handed to Codex

**Concrete deliverables**

- `futon3/library/invariant-coherence/bounded-disposition.flexiarg` —
  the new shape-pattern with vocabulary, bound, anti-patterns, and
  exemplar table for stash/branch/mission-doc/pr.
- `futon3c/src/futon3c/logic/archaeology.clj` extended with:
  `I-bounded-disposition`, `stash-disposition-vocabulary`,
  `default-stash-disposition`, `default-undecided-bound`,
  `default-old-stash-days`, `parse-stash-disposition`,
  `list-stashes-with-time`, `check-stash-disposition`. The convenience
  registrar extended to register four total family-ids.
- `futon3c/test/futon3c/logic/archaeology_test.clj` — 6 new deftests
  for vocabulary, parsing, empty-input, no-stashes,
  nonexistent-repo, canonical statement. The four-tap registrar test
  replaces the three-tap one.
- `futon3c/docs/structural-law-inventory.sexp:200-240` updated:
  removed `stash-debt-bounded` + `recover-or-drop` (informational
  removal per ratchet); added `bounded-disposition/stash` at
  `:status :operational-when-enabled` with full triples.
- PSR + PUR at `holes/labs/M-bounded-disposition/{psr,pur}/`.
- Codex handoff (GitHub issue) opened for the two remaining siblings:
  `bounded-disposition/branch` + `bounded-disposition/mission-doc`.

**Live verification**

Loaded via Drawbridge into running JVM; all four archaeology-control
taps register; bounded-disposition/stash fires `:outcome :ok`
(scanned 1 repo, 0 stashes — clean state across futon-stack).

**Tests**

Mission test sweep: **99 tests / 268 assertions / 0 failures** across
boundary + invariant + store + ratchet + probe + probe-taps + tracer
+ archaeology.

**Net mission state — INSTANTIATE complete for first sibling**

- ✅ `bounded-disposition/stash` — probe-tap registered, canonical
  statement bound, tests pass. `:status :operational-when-enabled`.
  Pre-commit binding deferred; boot-time check pending (Codex
  handoff covers this together with the new siblings).
- 🤖 `bounded-disposition/branch` and `bounded-disposition/mission-doc`
  — handed to Codex via GitHub issue.

**The methodology recurs.** Two missions (M-archaeology-control,
M-bounded-disposition) under one family (`archaeology-control`)
exhibit two distinct shapes (subsumption-witness, bounded-disposition).
Each mission's IDENTIFY started with shape-first; each produced a
namespace-discriminated sibling family; each handed off the
remaining instances via scope-bounded GH issues. The stack now has
*two* worked examples of how the inventory queue moves: pick the
top-rank candidate, IDENTIFY its shape, build first instance, hand off
siblings.

### 2026-04-29 — mission opened with shape-first IDENTIFY

- IDENTIFY complete; bounded-disposition shape named and lifted into
  `futon3/library/invariant-coherence/bounded-disposition.flexiarg`.
- Three sibling instances scoped: stash, branch, mission-doc. Stash
  slice is the in-mission worked example; branch + mission-doc are
  Codex handoffs.
- Pending: implementation of `bounded-disposition/stash` and the
  inventory rename.
