# Technote: Portfolio Inference Live Scan — 2026-02-27

**Date:** 2026-02-27T11:30Z
**Context:** First live run of portfolio inference against production server
**Method:** Drawbridge eval of `portfolio-step!` + HTTP `mc-review`
**Author:** joe + claude (Claude Code session)

## Inference Recommendation

```
Portfolio Inference (step 7)
Mode: :CONSOLIDATE | Urgency: 0.54 | τ: 1.38 | FE: 0.0444
Recommendation: review
Top actions:
  review:       G=-0.520  p=23.7%
  consolidate:  G=-0.340  p=20.8%
  wait:         G=-0.277  p=19.9%
```

**Interpretation:** The loop recommends *review* (take stock) over
starting new work. CONSOLIDATE mode with moderate urgency. The EFE
ranking says the highest information gain right now comes from
reviewing what exists, not from building.

## Belief State (μ)

| Channel | Value | Notes |
|---------|-------|-------|
| mission-complete-ratio | 0.325 | 13/40 missions complete |
| coverage-pct | ~0 | **Anomalous** — devmap coverage reads as zero |
| coverage-trajectory | 0.5 | Neutral (no baseline) |
| mana-available | ~0 | Mana system not initialized |
| blocked-ratio | ~0 | No blocked missions |
| evidence-velocity | 0.946 | High recent evidence activity |
| dependency-depth | ~1.0 | **Saturated** — needs investigation |
| gap-count | ~1.0 | **Saturated** — 79 gaps, clamped to max |
| stall-count | 0.325 | Some missions without recent evidence |
| spinoff-pressure | 1.0 | **Saturated** — missions generating spinoffs |
| pattern-reuse | ~0 | Patterns not tracked in evidence yet |
| review-age | 1.0 | **Saturated** — no recent review snapshots |

### Sensor Issues

Three channels are clamped to boundary values (0 or 1), which reduces
the model's ability to discriminate:

1. **coverage-pct = 0** — mc-coverage reports 0% across all devmaps.
   This is likely a bug in how coverage is computed (missions aren't
   being matched to devmap components). The observation surface
   produces structural data but the mapping is broken.

2. **dependency-depth = 1.0** — saturated. Either the normalization
   range is too narrow or every mission has deep dependency chains.
   Needs investigation.

3. **spinoff-pressure = 1.0** — saturated. The spinoff count exceeds
   the normalization cap. May need a higher clamp range or log scale.

4. **review-age = 1.0** — correct behavior (no prior review snapshots
   existed). Will self-correct after a few `mc-review` cycles.

5. **mana-available = 0** — mana system (nonstarter.db) not wired.
   Expected; not a bug.

## Portfolio Observation (mc-review)

**Summary:** 40 missions (13 complete, 11 in-progress, 0 blocked, 3
ready). 10 devmaps (10 valid). Avg coverage: 0%. Mana not initialized.

### By Status

**Complete (13):** agency-refactor, alfworld-pattern-discovery,
mission-peripheral, mission-control, transport-adapters,
peripheral-model, dispatch-peripheral-bridge, peripheral-behavior,
diagram-composition, sci-detection-pipeline, pattern-exotype-bridge,
futon2-aif-ants, futon3-agent-loop

**In-Progress (11):** portfolio-inference, futon3-last-mile,
tpg-coupling-evolution, xor-coupling-probe,
evidence-landscape-exotype, f6-ingest, f6-eval, social-exotype,
futon1a-rebuild, futon3-coordination, coordination-exotype

**Ready (3):** forum-refactor, coupling-as-constraint,
fulab-wiring-survey

**Unknown (13):** sliding-blackboard, stepper-calibration,
operational-readiness, improve-irc, operational-readiness-traceability,
peripheral-gauntlet, proof-peripheral, futon3c-codex, social-exotype,
psr-pur-mesh-peripheral, IRC-stability, peripheral-phenomenology,
coordination-rewrite

**Blocked (0):** none

### Gaps (79 uncovered devmap components)

Top gap clusters:
- evidence-landscape-exotype: E-store, E-threads, E-validate, E-compact, E-default
- f6-ingest: C-parse, C-embed, C-tag, C-cluster, C-ner
- (67 more across other devmaps)

### Actionable

14 missions flagged as actionable (ready or in-progress with no blockers).

## What This Means for Closing M-portfolio-inference

The loop runs, produces coherent recommendations, and the CONSOLIDATE
mode is defensible given the data. Three issues for the closing agent
to consider:

### 1. Sensor Calibration Needed

The 3 saturated channels (coverage-pct, dependency-depth,
spinoff-pressure) mean the model is operating with reduced
discriminability. These aren't bugs in the inference loop — they're
bugs in the observation surface or normalization ranges.

**Recommendation:** File as known weakness in the closure section.
Sensor calibration is an operational concern, not a structural one.
The inference architecture handles arbitrary channel values correctly;
it's the observation surface that needs tuning.

### 2. Coverage Reporting Broken

mc-coverage reports 0% everywhere. This is the single biggest data
quality issue — coverage is supposed to be the primary signal for
gap-driven policy selection. Without it, the model can't distinguish
"well-covered but stale" from "barely started."

**Recommendation:** Investigate whether mission-to-devmap matching
is working. The devmaps exist (10 valid), missions exist (40), but
the mapping produces zero coverage.

### 3. Mode Selection is Plausible

CONSOLIDATE at urgency 0.54 is the right call given:
- High spinoff pressure (missions generating more missions)
- High gap count (79 uncovered components)
- 11 in-progress missions (lots of WIP)
- 13 unknown-status missions (need triage)

The "review" recommendation (take stock before acting) is coherent.
This is the inference loop doing what it should — recommending
consolidation over expansion when the portfolio is fragmented.

## Relationship to M-self-representing-stack

M-self-representing-stack (DERIVE+ARGUE complete, ready for VERIFY)
consumes portfolio inference as its sensory surface. This scan
confirms the surface is operational. The self-representing stack
would make this data navigable in Arxana (hyperedges from missions
to evidence to code). The CONSOLIDATE recommendation arguably
supports prioritizing the self-representing stack — it's a
consolidation move that makes existing work legible.
