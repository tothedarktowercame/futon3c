# Excursion: E-r3d-per-entity-attribution

**Date:** 2026-05-30
**Status:** IDENTIFY
**Authored-by:** the War Machine run, **inline** — claude-5 as the in-loop pilot,
during M-war-machine-first-outing INSTANTIATE. This excursion is itself a cycle
*artefact*: the WM ranked `sorry/r3d-per-entity-attribution` at rank-1; it is not
earn-dischargeable in a bounded cycle, so the cycle's move was a **∇-deform**
(niche-construction) — construct the path rather than stall or fake-finish.
Cited consent-gate: see the cycle record (M-war-machine-first-outing §11 car 7).
**Parent sorry:** `sorry/r3d-per-entity-attribution` (futon2 registry, `:open`,
`:kind :prototyping-forward`).
**Template:** `E-support-coverage.md` (the prior likelihood-model excursion that
landed support/attack-coverage by the same shape).
**Owner:** Joe / pilot.

## 1. IDENTIFY — the gap

`futon2.report.war-machine/judge`'s v0.10 R3d wiring synthesises a **single
global** belief-update event from the `:annotation-health` prediction-error and
applies it **uniformly to every entity** in the belief domain (positive error →
`:strengthened` on all; negative → `:foreclosed` on all; weight =
`min(1,|weighted-error|)*0.1`). This is honest as a v1 placeholder — it makes
`:mu-post ≠ :mu-pre` so bilateral drift is witnessable — but it is **not** an
honest per-entity Bayesian update: an aggregate signal cannot legitimately be
attributed equally to every entity.

**Why now (the blocker is gone):** the named prerequisite — a typed per-entity
event vocabulary — landed 2026-05-26 as
`futon3/library/structure/interest-event-vocabulary.flexiarg` (M-INC step (b)).
So the architectural blocker this sorry waited on no longer exists; only the
*rewiring to consume per-entity events* remains. That is what makes r3d scopable
as a bounded excursion today rather than an open-ended wait.

## 2. Bounded steps (MAP seed)

1. **Survey** the exact R3d synthesis site in `judge` (the global-event
   construction + the uniform `apply-to-every-entity` step) and enumerate what
   per-entity state/event streams are actually available from the vocabulary /
   the substrate-2 adapter.
2. **Map** the per-entity events: replace the one global synthetic event with
   per-entity events carrying honest attribution (which entities the
   `:annotation-health` movement actually pertains to).
3. **Rewire** judge's R3d inner loop to update belief per-entity, preserving the
   multi-channel aggregation v0.16 already added (channel-health-signs) — change
   *attribution*, not the channel math.
4. **Verify** (computational-notebook discipline, mirroring E-support-coverage):
   `:mu-post` differs **per entity** (no longer uniform); bilateral drift with
   the VSATARCS side still witnessed; belief/judge tests green.
5. **Close** — flip `sorry/r3d-per-entity-attribution` → `:addressed` with the
   resolution + cg, committed to the registry ledger (R-A.1).

## 3. Success criteria

- Per-entity `:mu-post` divergence demonstrated on a real judge call (not all
  entities move identically).
- No regression in the multi-channel R3 aggregation (v0.16) or bilateral drift.
- The discharge is *earned* (G3: the field moves — r3d rotates off
  `ranked-actions[0]` on the next tick) and *sticks* (no `:sorry-closures-stick`
  regression).

## 4. Provenance

Spawned inline by the WM run as the demonstration that the in-loop agent — being
the *same system* — decomposes a deep sorry into a bounded path as a cycle move,
rather than requiring the operator to pre-seed excursions externally
(operator insight, emacs-repl 2026-05-30). This excursion does **not** discharge
r3d; it makes r3d dischargeable. Executing it autonomously remains gated (R-I);
authoring it inline is the low-blast-radius ∇-deform.
