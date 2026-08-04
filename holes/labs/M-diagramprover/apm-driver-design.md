# APM driver v0 — design (2026-08-04, claude-10; build = small codex handoffs)

The continuous workflow of capability-proof.md N9, on this box.
Requirements inherited from Joe's 08-03 cron-disable: attributable,
ledgered, parkable/pollable, inspectable; plus the day's findings:
two-part dispatch frame (N4), boundary-artifact relay (N2), mechanical
gates executed not testified (N7), claude review only where judgment
is irreducible (§1b), scribe with hunger audit (N3), capability-doc
updates (the BHK layer), packet templates as versioned artifacts (N8).

## Shape

Python, reusing the existing script idiom (apm_formal_zai_cron.py's
quota gate + candidate queue; agency_send's bell mechanics;
memory_outcome_sweeper's endpoint fields). A persistent
operator-started process (systemd-run or tmux, NEVER crontab), with:

- **Identity:** registered Agency identity `apm-driver` — every
  dispatch attributable in roster/jobs; no http-caller anonymity.
- **Ledger:** append-only JSONL at
  holes/labs/M-diagramprover/apm-driver/ledger.jsonl — one record per
  transition (dispatch, poll-result, gate-result, review-request,
  verdict, scribe, promotion-queued), each with timestamps, job-ids,
  shas. `driver.py status` renders current chains from the ledger.
  The ledger IS the inspectability surface.
- **Polling, not bells-to-driver:** the driver polls
  /api/alpha/invoke/jobs/<id> (bells wake agents, not scripts);
  interval 60s; quota gate (cron's fetch_and_enforce_quota) before
  every dispatch; ≤1 concurrent chain in v0.

## State machine (per problem chain)

```
SELECT (queue order, candidate_problem_ids filter)
 └─ DISPATCH A (starter phase A: recon-only, two-part frame, template)
     └─ DISPATCH B (phase B on A's completion: solve + event-anchored
        interleave)
         └─ MECHANICAL GATE (local): lake build; comment-stripped
            sorry count; #print axioms verbatim; statement extraction
            + normalized hash; boundary-comment conformance check
            (protocol sections present)
             ├─ CLOSED → FIDELITY REVIEW (claude checkpoint: fresh
             │   statement vs informal, §1b) → SCRIBE DISPATCH →
             │   PROMOTION QUEUE → UPDATE capability-proof.md → NEXT
             ├─ PARTIAL + conforming boundary → CLOSER HOP (codex,
             │   template; statement hash FROZEN from first
             │   formalization — mismatch = statement-altered, chain
             │   VOID→review) → gate again; ≤3 hops (§3 continuation
             │   recording per hop, option-independent fields) →
             │   exhausted → class `open-hole`, boundary banked, NEXT
             └─ DEFECTIVE (nonconforming boundary / gate anomaly /
                 statement mismatch) → CLAUDE REVIEW, chain paused
```

## Claude checkpoints (the only non-mechanical steps)

1. Fresh-statement fidelity adjudication (every new formalization).
2. Anomalies (defective class, gate contradictions, void chains).
3. Promotion approvals (author≠reviewer), batched.
Mechanism: driver bells claude-10 with the checkpoint payload and
watches for a verdict file
(apm-driver/verdicts/<chain-id>-<checkpoint>.edn) written by claude;
chain waits, other chains proceed. Timeout 12h → chain parks itself
in the ledger as `awaiting-review`.

## Templates (N8 substrate)

holes/labs/M-diagramprover/apm-driver/templates/{phase-a.md,
phase-b.md, closer.md, scribe.md} — the 08-03 packets, parameterized
({{problem_id}}, {{bundle}}, {{boundary_excerpt}}, {{hop_n}}).
Versioned in git; scribe practice-drafts may propose deltas (reviewed
like spec deltas). Templates carry: two-part frame, real tool names,
ignore-and-move-on license, boundary protocol, desk-research license
(closer), hunger-audit reporting requirement.

## Capability-proof updates

Per chain completion the driver appends to capability-proof.md
§Update-log: chain id, problem, outcome class, hops, gate results,
certificates (commit shas), and increments the relevant node warrants
(N1 closer instances, N2 relay instances, N3 scribe yield, N4
dispatch-frame count). Warrant CLASS changes remain claude decisions
(narrative never upgrades warrants — neither does the driver).

## Build plan (small handoffs, one behavior each)

- H1 ledger + state machine + status (dry-run, fixture jobs)
- H2 dispatch/poll + apm-driver registration + quota gate
- H3 mechanical gate module (build/sorries/axioms/statement-hash/
  boundary-conformance — port of the 08-03 hand-run gates)
- H4 templates + rendering (port the 08-03 packets)
- H5 claude checkpoints (bell + verdict files + timeout)
- H6 scribe dispatch + promotion queue + capability-doc update
Then: supervised trial, 2–3 problems, claude watching every
transition; then widen concurrency.

## Explicitly out of v0

Multi-chain concurrency >1; zone-joe execution (the workflow copies
over once Joe's remote session lands); automatic template deltas
(proposals only); Ψ/arm experimentation (the driver runs the
PRODUCTION frame — cohort arms ride the same ledger later via the
guard).
