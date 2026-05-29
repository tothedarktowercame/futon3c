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

## Rating, promotion, and the anti-backdoor invariant (Joe, emacs-repl 2026-05-29)

> "the auto-mined candidates should probably be rated and top percentage
> (maybe ≈30%?) should be persisted. Otherwise we'd risk … agent work that
> claimed closure of WM candidates, but left 'ephemeral sorries'. … I don't
> [want] to create a backdoor whereby it could happen."

Ephemerality has a dark side: a gap that is **never persisted has no durable referent**, so a closure *claim* against it can't be audited — the V2/no-teleport laundering failure, reappearing at the candidate level. The fix is a **rating + bounded promotion**, governed here:

- **Rate.** Each mined candidate carries a `:rating` (landed in `loop_learning.clj`: channel-gaps by gap magnitude, structural/missing-heads = 1.0), sorted descending.
- **Promote the top.** The top **≈30%** (starting parameter — really a warrant threshold the cheesemonger tunes against the net-flux) are **persisted** to the registry as durable `:auto-mined` sorries (`:status :open` + provenance, deduped). The long tail stays ephemeral/advisory. This persists the high-warrant holes (auditable, closeable) **without** flooding the registry with the tail — which is exactly the homeostatic balance this excursion governs.

**Anti-backdoor invariant (must hold structurally, not by agent discipline):**

1. **Only PERSISTED registry sorries are actionable/closeable.** Ephemeral candidates are advisory frame-data; they never enter the WM action space.
2. **A substantive closure claim MUST cite a persisted sorry-id.** You cannot "close" an ephemeral candidate — there is nothing durable to close. You can only *fix the underlying gap*, after which it stops being mined (no closure-claim needed, nothing to launder).
3. **Promotion is the ONLY route from candidate → actionable, and it persists by construction** — so a thing becomes closeable exactly when it becomes durable+auditable. No other path may make a candidate actionable.

**Current state (verified 2026-05-29):** the backdoor is **not open today** — WM `address-sorry` ranked-actions come from `sorry-registry/open-sorrys` (the persisted registry), and auto-mined candidates live only in the frame `:learning` (advisory). The invariant above is what keeps it closed when promotion is turned on; it is the cheesemonger's contract.

**Dependency note:** persisting auto-mined sorries needs either the substrate-2 typed-`:sorry` emission (gated on `E-substrate-2-sorry-typing`) or a write to the v1 `sorrys.edn` — and `sorrys.edn` is gitignored, so a version-controlled closure ledger (the `sorry-discharge-not-version-controlled` hole) is a soft prerequisite for *auditable* closure over time.

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

## Charter: the hole lifecycle (governance scope — Joe + claude-2, emacs-repl 2026-05-29)

E-cheesemonger governs the full lifecycle of a hole (sorry): **mine → rate →
promote → work → claim-closure → verify → stays-closed → retire.** The review
below is the agreed scope; **details deferred** ("sort out the details later").
Status tags: DONE / ENFORCED / DESIGNED / PARTIAL / OPEN.

1. **Identify candidate sorries** (new things to work toward). — **DONE**: `loop_learning.clj` auto-miner from WM gap-signals.
2. **No fake-finished** — don't leave big unfinished holes in work we claim is done. — **ENFORCED**: REPL V2 (no-teleport) + earned discharge (the mission-aif-head cycle).
3. **Don't flood** — mining-rate must not outrun closure-rate. — **DESIGNED**: the flux metric (this excursion) + rating + top-≈30% promotion gate.
4. **Closure must STICK** — regression / false-closure watch: if the WM re-detects a gap marked `:addressed`, flag it as regression-or-false-closure, do NOT silently re-mine as new. (Cautionary tale: the `defonce` bug — "head computes locally" was false for ages, unnoticed.) — **OPEN**. *Highest priority; un-detectable without #8.*
5. **Verify the closure, don't self-assert it** — a closure needs evidence the gap is gone, checked by something other than the claimer (the witness / realised-vs-predicted G-shift / adversarial re-read). — **PARTIAL**: the REPL's realised-discharge is one instance; generalise it.
6. **Spurious rejection** — not every candidate is a real hole; a first-class "dismiss as not-a-gap / by-design" disposition (distinct from closed-by-work), and the rejection itself **audited** so inconvenient holes can't just be deleted. — **OPEN** (registry already carries non-`:open` dispositions; use + audit them).
7. **Right holes (anti-Goodhart)** — auto-mining optimises the *measurable* (WM channel-gaps); important-but-unmeasured strategic/architectural holes stay invisible while the flux reads "balanced." Don't let easy-to-mine crowd out important. (= shen-tamkin `anamnesis-is-Goodhart-target`, live.) — **OPEN** (the `:agent-supplied` / transcript rung partially counters).
8. **Provenance + auditable closure ledger** — every hole and closure traceable (who/what/when/against-what-evidence). The substrate that makes #4/#5 and the anti-backdoor *checkable* rather than asserted. — **OPEN** (`sorrys.edn` gitignored → no closure history).
9. **Obsolete-hole GC** — retire holes whose *premise* is gone (referenced code deleted, mission closed) so the flux stays meaningful and the registry doesn't fossilise. — **OPEN**.

Plus the **anti-backdoor invariant** (above): only persisted sorries are actionable/closeable; ephemeral candidates are advisory; closure claims cite a persisted sorry-id; promotion is the only candidate→actionable route and persists by construction.

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
