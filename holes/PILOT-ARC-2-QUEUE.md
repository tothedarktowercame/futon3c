# Arc-2 Queue — questions deferred to arc-end review

Per the arc-2 operating protocol (#1): genuinely-undetermined / operator-relevant
forks encountered mid-arc are NOT asked of Joe; they take the safe default (skip
that target / hold) and are logged here for review at STOCK-TAKE-002 (cycle 20).

| logged | cycle | question | safe default taken | warrant status |
|---|---|---|---|---|
| 2026-06-11 | 11 | M-daily-scan §367: rewrite the `pattern-rule-systems` probe as **Alexander-pattern-language** (`topic:design-patterns OR "pattern language" OR alexander-patterns`) or **rename to business-rules**? Decides what Joe's daily intelligence scan *hunts*. | Skipped §367; pivoted cycle 11 to §4.5. | Operator-relevant (Joe's scan intent). The audit leans Alexander (name signals it; query drifted to business-rules), but the choice affects Joe's intelligence priorities — his call, not warrant-determined. |

## Resolutions (operator review, 2026-06-11)

**§367 → 無 (mu).** Joe rejects the fork's premise: "business rules" is
woefully generic; *repeatedly* scanning "Alexander patterns" wastes time
too. The actual requirement dissolves the static-probe model entirely:

> "I'd expect the daily scan to be *fresh* on any given day, with a
> traceable source for all terms that are being scanned and a warrant, as
> well as the evidence retrieved."

So §367 closes not by re-querying or renaming the static probe but by
retiring the static-probe pattern in favour of a **fresh-probes
discipline**: the daily probe set is GENERATED each day, each term carrying
(term ← source artifact ← warrant), with the retrieved evidence ledgered.
Notes for whoever lands it:
- futon7 already has the seam: the `probe-gen` bb task ("generate probes
  from concept graph", f7.probe-gen) — the machinery exists, the
  discipline doesn't yet.
- This is the third instance of the static-probe-misalignment disease
  (xtdb OR-chain, frame-005; pattern-rule-systems drift, §367 audit) — the
  pattern is general: a frozen query is an unwitnessed prior that
  re-certifies itself every scan.
- Per Joe: the daily scan is effectively a PERIPHERAL, and wants its own
  anatomy treatment (envelope: generate-probes → search → enrich →
  signals → brief; term-provenance as a first-class field) — joins the
  anatomy programme alongside the WM-flight anatomy.
