# E-first-flights-typed-grounds-tail-closure — W2 census

**Excursion:** E-first-flights-transferred-work, workstream W2.
**Date:** 2026-07-06. **Author:** zai-6 (ground-control dispatched).
**Verdict:** **HOLD**

## The question

Does the typed-grounds / return-channel tail around sorry-arrow
`arr-7535a5b6-e59` close from existing evidence, or must it remain open
as an Excursion-owned obligation?

## Census — source-of-record trace

### 1. The sorry-arrow `arr-7535a5b6-e59` (the typed hole)

**Source of record:** `futon3c/holes/missions/M-first-flights.md` §3.4
(DERIVE gate certificate), and `futon3c/holes/flights/F-wm-piloted-2026-06-12.md`
sortie 9.

The arrow was minted into live meme.db during the piloted flight (2026-06-12):

```
have: flight-records-organs-typed-grounds-in-prose
want: flights-as-anatomy-corpus-canonical-organ-order-typed-grounds
status: :construction/:open
construction named: flight-pretty-print (ckpt 21) + typed-grounds migration (unplanned)
```

The arrow's `:construction` names TWO fillers. Only one has been discharged.

### 2. The wiring diagram — `first-flights-wiring.edn`

**Source of record:** `futon3c/holes/flights/first-flights-wiring.edn`.

The wiring diagram (the triple's third term) records two hungry nodes:

| Node | Satiety | Status |
|---|---|---|
| `:pretty-print` | `{:hungry-for :payoff}` | **Partially discharged** — checkpoint 22: flight-pretty-print landed (`.flight.edn` → canonical-organ-order text), witness = pilot review + ckpt 22. The rendering half of the structured-events debt is paid. |
| `:typed-grounds` | `{:hungry-for :design}` | **NOT discharged** — "no plan yet — the second half of structured-events-only's debt" (wiring line 45). |

The `:typed-grounds` node sits in the hyperedge chain between `:pretty-print`
and `:want-port` (lines 61, 64). Its `:satiety {:hungry-for :design}` means the
grounds-migration has no design, no plan, no construction — it is design-hungry,
not merely awaiting execution.

### 3. Checkpoint 22 (M-first-flights §INSTANTIATE)

**Source of record:** `futon3c/holes/missions/M-first-flights.md` lines 946-952.

> "sorry-arrow arr-7535a5b6-e59 stays :open — its construction names BOTH
> pretty-print and the typed-grounds migration, and the latter is still
> :design-hungry (no plan). The cascade's structured-events node flips
> half: rendering debt paid, grounds-in-prose debt remains."

This is the authoritative statement: the sorry-arrow stays open because
its named construction has two halves, and only one half discharged.

### 4. The Phase-B sorry (`:sorry/first-flights-phase-b-policy-grade-G`)

**Source of record:** `futon2/resources/sorrys.edn`, entry updated by
`futon2 2665779`.

The sorry is now `:status :addressed` with `:addressed-by-excursion`
pointing here. The resolution text explicitly says:

> "Residual typed-grounds / return-channel work is tracked inside the
> Excursion until independently closed or held."

So the sorry closure TRANSFERRED the typed-grounds tail to this Excursion —
it did NOT discharge it. The sorry's `:addressed` status is correct for the
Phase-B policy-grade G obligation (which has a named successor in
`fold-realized`), but the typed-grounds / return-channel tail is a distinct
obligation that the sorry text itself names as residual.

### 5. DarkTower / Lean formalization

**Source of record:** `mathlib4/DarkTower/FirstFlightsExample.lean`.

The Lean formalization proves the fold grain (`fold_selected`,
`fold_empty_store_leaves_hungry`) — that a cascade-selected query can fill a
hungry port. But the formalization operates at the type level: it proves the
MECHANISM of filling, not that any specific typed-grounds migration plan exists.
The residual idealization is noted: "fold conditions on selection-existence,
not yet on which pattern — a future faithful step." The typed-grounds migration
is not formalized in Lean because it has no design.

### 6. Ground-control closure demo

**Source of record:** `futon2/holes/ground-control-M-first-flights-closure-2026-07-06.md`.

Task A census finding:

> "The typed-grounds migration / sorry-arrow arr-7535a5b6-e59 remains open."

This was flagged as an open operator question — whether to close the mission
or keep it as a tracking vehicle until typed-grounds migration lands. The
operator chose to close/transfer the mission and home the tail in this Excursion.

## Verdict: HOLD

The typed-grounds / return-channel tail is **NOT closed**. It must remain
open as an Excursion-owned obligation.

### Exact blocker

The `:typed-grounds` node in `first-flights-wiring.edn` has
`:satiety {:hungry-for :design}` — the typed-grounds migration (moving
grounds out of prose into typed structured fields) has no design, no plan,
and no construction. The sorry-arrow `arr-7535a5b6-e59` is `:open` because
its named construction has two halves and only the rendering half (flight-
pretty-print, checkpoint 22) discharged.

### Source-of-record that keeps it open

1. `futon3c/holes/flights/first-flights-wiring.edn` line 44:
   `:satiety {:hungry-for :design}` (the typed-grounds node)
2. `futon3c/holes/missions/M-first-flights.md` lines 949-952:
   "sorry-arrow arr-7535a5b6-e59 stays :open — its construction names BOTH
   pretty-print and the typed-grounds migration, and the latter is still
   :design-hungry (no plan)"
3. `futon2/resources/sorrys.edn` resolution text:
   "Residual typed-grounds / return-channel work is tracked inside the
   Excursion until independently closed or held"

### What does NOT close this tail

- The Phase-B sorry being `:addressed` — that addresses the policy-grade G
  obligation, not the typed-grounds migration.
- The DarkTower Lean fold proof — that proves the filling mechanism, not that
  a typed-grounds migration design exists.
- The flight-pretty-print checkpoint 22 — that discharged the rendering half
  only.

### What WOULD close this tail

A typed-grounds migration design: a plan for how grounds currently living as
prose in flight records become typed structured fields with witness-checkable
discharge. Until that design exists and is witnessed in a flight record where
the `:typed-grounds` node flips from `:hungry-for :design` to `:full`, the
sorry-arrow `arr-7535a5b6-e59` cannot discharge.

## Relationship to M-fold-ansatz

This tail is NOT a dependency of M-fold-ansatz. M-fold-ansatz may benefit from
any typed-grounding representation that falls out of a future design, but this
Excursion holds or closes on its own evidence. The hold does not block
M-fold-ansatz.
