# Excursion: Cheesemonger

**Type:** Excursion (E-prefix) — not a full mission. A bounded scope-out
owned end-to-end by a single agent, sized to land without growing into its
own mission tree. Sibling of `E-street-sweeper.md` (working-tree hygiene),
`E-night-shift.md` (branch-isolated implementation), and the pattern-mining
work — the "working-class" peripheral series.

**Status:** SCOPED (authored 2026-05-29; not yet built)
**Date:** 2026-05-29
**Author:** claude-2 (inhabiting `:war-machine-pilot`; emacs-repl, paired with Joe)
**End-to-end owner:** unassigned (claude-2 by default)
**Parent mission:** `futon5a/holes/missions/M-a-sorry-enterprise.md` (the mining mission whose runaway this guards) + the REPL LOOP `:autonomy` (`futon3c/holes/specs/repl.spec.edn`)
**Surface that authored this:** emacs-repl 2026-05-29 between claude-2 and Joe

## Why this exists (Joe's reframe, emacs-repl 2026-05-29)

> "if we're mining holes faster than we're closing them, that's … well,
> interesting, I guess, including from an AIF perspective. Maybe we need an
> E-cheesemonger.md excursion alongside E-street-sweeper that helps us keep
> track of whether the auto-generated holes [are] getting out of hand."

Now that the LOOP turn **auto-mines sorries** (`futon3c/src/futon3c/aif/loop_learning.clj`, `:derivation :auto-mined`), the system can generate holes (sorries) autonomously and unattended. If it mines them **faster than it closes them**, the registry diverges.

**AIF framing (load-bearing).** Per the aliveness synthesis (`M-action-cost-modelling §3.8`, `project_aliveness_synthesis`): a sorry is a high-**anamnesis** region — un-discharged tension the system can't forget. **Mining = anamnesis creation; closure = mana discharge.** A sustained `d(open-sorries)/dt > 0` means anamnesis accumulates faster than it discharges — the agent's expected free energy is *not* being reduced; the to-do list diverges. The cheesemonger is the **homeostatic governor on the sorry-budget**: it measures the anamnesis-creation flux against the discharge flux and flags (later: throttles) when they diverge. It is the dual of `E-street-sweeper` — sweeper *closes* a particular hole-class (dirty files); cheesemonger *watches the balance of opening vs closing across all classes*.

## The sharp point: the danger is at the promotion boundary, not the mining

The auto-miner today surfaces candidates **ephemerally**: each cycle recomputes them from the live WM gap-signals and **de-dupes against `sorry-registry/open-sorrys`**, and they live only in that cycle's γ-frame `:learning` block — they are **not persisted** to the registry. So the registry is *not* currently flooded; the same ~10 channel-gap candidates re-surface each cycle until the underlying gap is fixed.

The runaway risk appears at the **candidate → registry promotion** step — the typed-`:sorry`-emission gated on `E-substrate-2-sorry-typing` + `interest-event-vocabulary.flexiarg`, still owned by `M-a-sorry-enterprise`. Once promotion exists, an overnight loop could mint sorries far faster than any closing excursion discharges them. **The cheesemonger sits at that boundary** as the advisory governor: it is the thing that makes auto-promotion safe to turn on.

## What this excursion is for

A monitor (provisionally a report + a WM metabolic-balance channel, NOT a new
write-peripheral) that computes and surfaces the **hole-budget**:

- **mining flux** — distinct candidate sorries surfaced per cycle (from γ-frame `:learning :sorries-mined` counts across `data/repl-traces/`), and the size of the rolling distinct-candidate set.
- **closure flux** — sorries closed per unit time (`:resolved-at` timestamps in the registry; trend of `open-sorrys` count).
- **net hole-flux** = (distinct mined) − (closed) over a window. Sustained positive ⇒ "out of hand."

When net flux crosses a threshold, it raises a signal the WM can act on — ideally a **`:sorry-flux` (anamnesis-balance) metabolic-balance channel** alongside the existing `:working-tree` one, so the WM's own `:stop-the-line` mode fires on hole-overflow exactly as it does on dirty-file pressure. The *response* (close holes) then routes to the existing closing surfaces (street-sweeper / night-shift / the pilot addressing top sorries) — the cheesemonger only **measures + flags**, it does not close.

## Why an excursion, not a mission

- Bounded measurement build: a metric over data we already emit (γ-frames) + the registry, plus one metabolic-balance channel. No new write-capability, no multi-phase lifecycle.
- `M-a-sorry-enterprise` is the mission (the mining + promotion pipeline); the cheesemonger is the bounded *governor* alongside it — the homeostatic check that the mining mission's output doesn't outrun closure.

## Scope

**In:** compute mining-flux / closure-flux / net-hole-flux from γ-frames + registry; surface a reading (report/console); propose the `:sorry-flux` metabolic-balance channel; define the "out of hand" threshold.

**Out:** does **not** close holes (street-sweeper / night-shift / pilot do that); does **not** itself promote candidates to the registry (that's the M-a-sorry-enterprise emission gate); does **not** auto-throttle the miner without operator consent (advisory v0). Closing-rate over time needs a closure log — note that `data/sorrys.edn` is gitignored, so a separate append-only closure ledger (or futon1a evidence) may be a prerequisite, and is itself a mined hole (`sorry-discharge-not-version-controlled`, frame live-2d50834b).

## First deliverable (v0, for the owning agent)

A `bb` reading — `hole-budget` over `data/repl-traces/*.edn` (sum/-distinct of `:learning :sorries-mined`) vs registry open-count + `:resolved-at` events — printed as `{:mined-distinct N :open M :closed-window K :net-flux (N−K) :verdict :balanced|:accumulating}`. Then, if the verdict is useful, lift it into a `:sorry-flux` metabolic-balance channel so the WM surfaces it natively.

## Cross-references

- `futon3c/src/futon3c/aif/loop_learning.clj` — the auto-miner whose output this governs
- `futon3c/holes/specs/repl.spec.edn` — LOOP `:autonomy` + V6 (the surface that emits `:learning`)
- `futon5a/holes/missions/M-a-sorry-enterprise.md` — parent mission (Checkpoint 2026-05-29); owns promotion
- `futon3c/holes/missions/E-street-sweeper.md` — sibling; the dual (closes the working-tree hole-class)
- `project_aliveness_synthesis` — anamnesis (accumulation) vs mana (discharge); the quantity being balanced

## Provenance

Coined by Joe (emacs-repl, 2026-05-29) immediately after the LOOP auto-miner v0 landed, as the homeostatic counterpart: a miner that runs unattended needs a governor that watches whether it mines faster than the stack closes. Named for the Swiss-cheese line earlier in the same session.
