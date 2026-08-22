# README-missions — the Mission Doc format (for pilots)

A mission doc (`futon<N>/holes/missions/M-*.md`) is not free-form prose. Its
**section headers** are parsed into the mission's structure — the 8-phase
lifecycle that the outer-loop tracker (`emit_mission_clean.sh`) types as a
DarkTower comb of holes. If you advance a phase but put the content under the
wrong header, the tracker will NOT see it and the hole will NOT discharge. This
doc is the format so you don't reverse-engineer it from the parser mid-flight.

Canonical parser: `futon6/scripts/mission_scope_detect.py` (`phase_for_title` /
`LOOSE_PHASE_MAP`). The structural counter that the tracker reads:
`futon3c.logic.capability-star-map-extractor/structural-hole-report`.

## The eightfold lifecycle

The 8 phases, in BV-spine order:

```
head → identify → map → derive → argue → verify → instantiate → document
```

`head` is the mission preamble (title + intro before the first `## N.` section)
— it has no section of its own and is auto-discharged on entry. The other seven
are written sections.

## How a section becomes a PHASE (the rule that bit zai-1)

A header creates an **eightfold-phase** frame — the thing that discharges a
tracker hole — iff its **first word, after stripping any `N.` numbering, is a
phase name** (case-insensitive). The number is for humans; the parser drops it.

Discharges the phase (eightfold-phase frame):
- `## 1. IDENTIFY`
- `## 2. MAP`
- `## 3. DERIVE`
- `## 4. ARGUE`
- `## 5. VERIFY`
- `## 6. INSTANTIATE`
- `## 7. DOCUMENT`
- `## Derive — the mining pipeline` (first word still `Derive`) ✓
- `## DERIVE` (no number) ✓

Does NOT discharge the phase (becomes a `loose-section`, not a phase):
- `## Checkpoints` → content here is a loose-section. **This was the trap:**
  writing "Checkpoint 4 — the DERIVE work" under `## Checkpoints` does NOT
  advance `:derive`. You must ALSO add a `## 3. DERIVE` section (it may be a
  short intro that points at the checkpoint — but the header must exist).
- `## Motivation`, `## Scope`, `## Open Questions`, `## Notes` → keyword-mapped
  to a *loose* phase flavour (identify/argue/…), still not an eightfold-phase
  frame. In particular `## Open Questions` with content is tracked as a held
  **open-question** hole (metadata on the CLean), not a lifecycle phase.

Inline closure also works for the "satisfied elsewhere" case — the bold form
`**DOCUMENT phase:** satisfied by README-foo.md` (verbs: satisfied/closed/
complete/done) closes that phase without a full section.

## What makes a phase WRITTEN vs an empty shell

A phase header alone is not enough. The extractor's `substantive-frame?` marks a
phase `:written` only if its scope frame has substance — `sub-count > 0` (real
subsections / nested content) or an attached anchor. An empty `## 3. DERIVE`
with one line and no substructure stays a vacuous hole. **Earn the discharge:**
the section must carry the real work that satisfies the phase's `:produces`
(derive → a construction, verify → a checked witness, …), not a stub to trip the
header detector. The review gate checks this.

## Observing your change immediately (don't sleep/poll)

After you edit the doc, two caches sit between the file and the tracker: the
futon1a scope reingest (async watcher) and a 30s structural-cache in futon3c. So
a naive re-emit right after the edit can still show the OLD holes. Do not
`sleep` and poll. Instead force a synchronous refresh in the LOOP turn:

```
bash scripts/emit_mission_clean.sh <mission-id> --refresh <mission-doc-path>
```

`--refresh` reingests the doc and busts the cache before reading, so a genuine
phase discharge shows up in `holes-at` immediately. Without `--refresh` (STEP 1b,
the before-work read) the cached view is fine.

## Cross-refs

- `README-pilot.md` — the pilot loop (READ→EVAL→PRINT→LOOP→VERIFY).
- `README-drawbridge.md` — how to eval in the live JVM (`emit_mission_clean.sh`
  goes through Drawbridge; its `--refresh` flag refreshes mission data/cache and
  is unrelated to the forbidden `clojure.tools.namespace` namespace refresh).
- `src/futon3c/logic/mission_clean.clj` + `holes/excursions/E-scope-organism-copar.md`
  — the outer-loop tracker itself and its one deferred fidelity gap.
