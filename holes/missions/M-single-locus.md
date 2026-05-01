**Status:** INSTANTIATE complete (2026-04-29) for the mission-home slice; siblings handed to Codex via GitHub issue #64.
**Family:** `atomic-inspectable-units` (structural-law-inventory.sexp:161)
**Sibling missions:** [M-archaeology-control](M-archaeology-control.md) (subsumption-witness shape); [M-bounded-disposition](M-bounded-disposition.md) (bounded-disposition shape); [M-invariant-queue-extend](M-invariant-queue-extend.md) (apparatus).
**Origin:** Joe + claude-11 conversation 2026-04-29 — first execution of `~/code/algorithms/next-invariant.md` ratifies the algorithm by running it on the rank-4 candidate `home-repo`.
**Home-repo:** futon3c

# M-single-locus: Uniqueness of attribute per identity, exhibited across artifact-classes

## 1. IDENTIFY

### Motivation

Rank-4 priority candidate `atomic-inspectable-units / home-repo`
("Every active work item has one canonical repo") is the third candidate
in the inventory queue requiring shape-first IDENTIFY. Reading it
alongside `agency/single-routing-authority` (already operational in
futon3c — one routing entry per agent-id) and the existing
`single-boundary` family (one boundary per write-class) reveals the
common shape: **uniqueness of an attribute mapping per identity**. For
identity I and attribute A, the system carries exactly one record
`(I, A) → R`.

This mission ships the shape (`single-locus`) as a new pattern in
`invariant-coherence/`, builds one worked sibling
(`single-locus/mission-home`), adds at least one concrete annotated
inhabitant per Joe's "future-proofed needs an instance" directive, and
hands off two more siblings (`/agent-routing`, `/artifact-live-copy`)
to Codex.

### Theoretical anchoring

- **Single-locus shape (just named).** Per-(I, A) cardinality-1
  invariant. See
  `futon3/library/invariant-coherence/single-locus.flexiarg`.
- **Distinct from subsumption-witness and bounded-disposition.** The
  three shapes coexist; this is the third.
- **Backwards-applied.** `agency/single-routing-authority` has been a
  single-locus instance since before the shape was named. Marking it
  as a prior exemplar in the new pattern's table makes the lineage
  explicit.
- **Inhabitant-first discipline.** When the live signal would be
  trivially-pass without explicit data (no `Home-repo:` annotations
  exist yet anywhere), at least one mission file gets the annotation
  in this same INSTANTIATE — the apparatus has a non-trivial input
  to verify against.

### Scope in

- **Sibling invariant `single-locus/mission-home`** under family
  `atomic-inspectable-units`. Replaces the narrower candidate
  `home-repo` per ratchet `:removed` semantics.
- **`Home-repo:` annotation convention**: a line in the first 10 lines
  of a mission file with case-insensitive `Home-repo: <repo-name>`
  designates the authoritative locus for the work. Missing line ⇒
  implicit-home (the mission file's own repo).
- **Cardinality-test**: a mission has at most one `Home-repo:`
  annotation. Two or more = violation.
- **At least one concrete inhabitant**: this mission file (`M-single-locus.md`)
  carries `Home-repo: futon3c` as its annotation. M-bounded-disposition
  + M-archaeology-control get annotations too — both have work
  spanning futon3c apparatus + Codex (cross-repo-via-handoff).
- **Implementation**: new namespace `futon3c.logic.locus` with
  `check-mission-home-locus`. Boots-time check + probe-tap, mirroring
  the archaeology pattern.
- **Tests**: parse paths, cardinality cases, default-handling, the
  registrar.
- **Inventory updates**: rename `home-repo` → `single-locus/mission-home`
  with full triples; mark `:status :operational-when-enabled`.

### Scope out

- **`single-locus/agent-routing`** — handed to Codex. Wraps the
  `agency.registry` atom: for each agent-id, |routing-entries| = 1.
- **`single-locus/artifact-live-copy`** — handed to Codex. Subsumes
  the existing `single-live-copy` candidate (in
  atomic-inspectable-units already). Detection: cross-repo grep for
  same-named tool/pattern files; flag identical filenames in
  multiple repos without an explicit canonical-repo annotation.
- **`single-locus/write-class-boundary`** — already operational as
  `single-boundary` family. Re-framing under single-locus is
  documentation-only; do not rename live family.
- **Auto-classifying** missions with implicit homes that should be
  explicit. Out of scope; the invariant detects, the operator
  decides.

### Completion criteria

1. `single-locus/mission-home` registered as probe-tap + boot-time
   check; `:status :operational-when-enabled` in inventory.
2. `single-locus.flexiarg` lands in `invariant-coherence` namespace.
3. ≥ 1 active mission file carries explicit `Home-repo:` annotation
   that the check verifies as cardinality-1.
4. Mission test sweep clean: 0 failures.
5. One Codex handoff issue opened scoping `agent-routing` +
   `artifact-live-copy` siblings.

### Relationship to other missions

- **Sibling shapes (different protocols, different families):**
  M-archaeology-control (subsumption-witness),
  M-bounded-disposition (bounded-disposition). Three shapes now,
  three missions, three pattern files in
  `invariant-coherence/`.
- **First live-execution of `~/code/algorithms/next-invariant.md`** —
  this mission's existence is the algorithm's correctness criterion.

### Source material

- `futon3c/docs/structural-law-inventory.sexp:161-189` — current
  family + the narrower candidate being absorbed.
- `futon3/library/agency/single-routing-authority.flexiarg` — prior
  single-locus exemplar pattern.
- `futon3c/src/futon3c/logic/archaeology.clj` — apparatus to model
  `futon3c.logic.locus` after.
- `~/code/algorithms/next-invariant.md` — the executing algorithm.

### Owner and dependencies

- **Owner:** Joe (architectural authority on annotation convention) +
  claude (mission-home slice + algorithm execution) + Codex
  (agent-routing + artifact-live-copy via handoff).
- **Primary repo:** futon3c (apparatus extension).
- **Touches:** all repos with `holes/missions/` directories
  (see live state: 154 mission files across 12 repo locations).

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT

INSTANTIATE in progress for the mission-home slice; siblings depend on
Codex.

## Checkpoints

### 2026-04-29 — INSTANTIATE complete for single-locus/mission-home; siblings handed to Codex

**Concrete deliverables**

- `futon3/library/invariant-coherence/single-locus.flexiarg` — new
  pattern with cardinality test + exemplar table.
- `futon3c/src/futon3c/logic/locus.clj` — new namespace with
  `I-single-locus`, `parse-home-repo-annotations`,
  `check-mission-home-locus`, `check-mission-home-locus-on-load!`,
  `register-locus-taps!`. Markdown-tolerant regex.
- `futon3c/test/futon3c/logic/locus_test.clj` — 13 deftests / 20
  assertions / 0 failures.
- Three mission files annotated with `Home-repo: futon3c`
  (M-single-locus, M-archaeology-control, M-bounded-disposition) —
  the concrete inhabitants Joe asked for.
- `futon3c/docs/structural-law-inventory.sexp` updated: `home-repo`
  removed, `single-locus/mission-home` added at
  `:status :operational-when-enabled`.
- `futon3c/dev/futon3c/dev/bootstrap.clj` extended: locus require +
  try-wrapped on-load call after the archaeology checks.
- PSR + PUR at `holes/labs/M-single-locus/{psr,pur}/`.
- Codex handoff (GitHub issue #64) for the two remaining siblings:
  `single-locus/agent-routing` + `single-locus/artifact-live-copy`.

**Live verification**

```
{:outcome :ok
 :detail {:scanned-repos 9
          :total-missions 109
          :explicit-home-count 3
          :implicit-home-count 106}}
```

Cardinality-1 holds across all 109 missions; three concrete annotated
inhabitants (the ones added this turn) keep the apparatus from being
trivially-pass.

**Test sweep**

Mission test sweep: **124 tests / 321 assertions / 0 failures** across
boundary + invariant + store + ratchet + probe + probe-taps + tracer +
archaeology + locus.

**Algorithm trial-run conclusion**

The first execution of `~/code/algorithms/next-invariant.md` ran
end-to-end. Its correctness criterion ("after one run, the priority
queue's top-N has shifted AND a Codex handoff is open continuing the
shape") is satisfied: rank-4 `home-repo` is now operational-when-enabled
as `single-locus/mission-home`; issue #64 is open; the next iteration
starts at whatever's now rank-4 (probably one of the other
atomic-inspectable-units candidates).

The algorithm produces traces of a recurring shape:
- M-archaeology-control (subsumption-witness; iteration 0)
- M-bounded-disposition (bounded-disposition; iteration 1)
- M-single-locus (single-locus; iteration 2)

Three iterations, three shapes, three pattern files in
`invariant-coherence/`. The methodology is now self-evidently
recurring — when we draft `next-invariant-from-traces.md` it can read
from these as input.

### 2026-04-29 — mission opened with shape-first IDENTIFY (algorithm trial run)

- IDENTIFY complete; single-locus shape named and lifted into
  `futon3/library/invariant-coherence/single-locus.flexiarg`.
- Three sibling instances scoped: mission-home (in-mission worked
  example), agent-routing (Codex), artifact-live-copy (Codex).
- This mission file carries `Home-repo: futon3c` per the
  inhabitant-first directive — at least one concrete annotated
  artifact exists at the moment the check first fires.
- Pending: implementation, tests, inventory rename, Codex handoff.
