# Slice 1 synthesis

Stage 1 dispatched 97 independent zai-1 reads over all 376 available chunks
for the 12 requested problems. No chunks were down-sampled. The reads produced
238 `MARK` records, 222 `MOVE` phrases, and 34 `NOTHING-SURPRISED` results.

## Semantic clusters

| Cluster | Problems | Read transcripts | Disposition |
|---|---:|---:|---|
| Probe the claimed property, not the acceptance proxy | 7 | 17 | Authored as a technique |
| Separate evidence history from verdict state | 4 | 4 | Authored as a principle |
| Type empty, idle, failed, and retryable outcomes explicitly | 6 | 7 | Reinforces `agency/loud-failure` |
| Measure the discriminating population before prescribing | 8 | 14 | Reinforces `system-coherence/facet-before-aggregating` and `system-coherence/argue-empirically-not-persuasively` |
| Resolve current state before interpreting historical state | 6 | 9 | Reinforces `process-coherence/status-refresh-before-work` and `stack-coherence/staleness-scan` |
| Calibrate a gate on known-good and known-bad artifacts | 6 | 7 | Reinforces `data-mining/gates-as-code` and `data-mining/golden-is-curated-not-raw` |

Counts are distinct read packets containing at least one phrase assigned to
the cluster, and distinct problem IDs represented by those packets. A phrase
was assigned by meaning, not exact-string equality.

## Dedupe decisions

The dedupe surface was the complete `/home/joe/code/futon3/library/` tree plus
the five case-1 staging flexiargs. Two candidates remained gaps.

- Authored `process-coherence/probe-the-claimed-property-not-the-acceptance-proxy`.
  `agent/evidence-over-assertion` requires evidence, and
  `war-machine/operational-not-decorative` requires validation, but neither
  names the recurring proxy-versus-property error or the need to probe a
  transitive trust closure with explicit unknowns.
- Authored `process-coherence/separate-evidence-history-from-verdict-state`.
  `stack-coherence/evidence-ledger` anchors claims and
  `aif/valuation-reads-the-paperwork` versions inputs, but neither separates
  append-only observations from authority-bearing, supersedable verdicts.
- Rejected `type empty outcomes before retrying or settling`: near-duplicate of
  `agency/loud-failure`, whose HOWEVER already names empty-success versus
  swallowed-failure ambiguity and whose THEN requires typed failure layers.
- Rejected `measure before strategic prescription`: the generic move is
  already covered by `system-coherence/facet-before-aggregating` and
  `system-coherence/argue-empirically-not-persuasively`; the slice adds cases,
  not a missing maneuver.
- Rejected `refresh current state before historical analysis`: covered by
  `process-coherence/status-refresh-before-work` and
  `stack-coherence/staleness-scan`.
- Rejected `calibrate gates with positive and negative fixtures`: covered
  directly by `data-mining/gates-as-code` and
  `data-mining/golden-is-curated-not-raw`.

## Optional Tier-0 recognizer

`futon6/scripts/cas_select.py --help` ran successfully, but the recognizer
accepts prepared fixture or `.steps.json` inputs rather than prose directories.
Preparing a new conversion pipeline would exceed the requested best-effort
probe, so no recognizer-derived miss-list was produced.
