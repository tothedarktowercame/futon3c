# Executed TA–Student pattern-transfer pilot

All three commissioned development cases completed, with five real Agency Student turns on 2026-09-10. No Student transcript was fabricated. This is TA-reviewed paper mathematics, not a fresh Lean-checked proof or a teaching-efficacy experiment.

Materials frozen in **42d4b153** before the reserved Student's first exposure; first executed C1 walkthrough committed in **f96a0b8d**. Baseline futon3c source SHA: `703b987655ce0bc81232438579e44c567573d68c`. Prototype **b9e09bb5** reused and verified; canonical patterns pinned to futon3 `a376e4043369167ec8b491a77ddc4f9695a6d991`, with exact text hashes in materials/patterns.json.

| Case | Actual outcome | Student invocation IDs |
|---|---|---|
| C1: finite-net source → uniformly bounded family target | Positive target proved initially. Initial counterexamples dropped required premises. TA lesson/feedback followed by explicit source-to-target mapping and valid triangular spikes on [0,1], repairing the contrast. | `invoke-1789059902616-19036-11a648a1`, `invoke-1789060039883-19041-e098a859` |
| C2: bounded discrete domain | Correctly rejected bounded-domain UC⇒bounded-image claim, locating the missing finite-net prerequisite. Already anticipated during C1; no fresh-test claim. | `invoke-1789060152372-19047-af4590c9` |
| C3: uniformly continuous dense extension | Constructed extension via Cauchy image limits; repaired metric-only well-definedness and epsilon slack. Candidate construction gap only in the supplied corpus/search; no pattern promotion. | `invoke-1789060226855-19051-8dc11a5e`, `invoke-1789060331192-19055-de4ac8b9` |

The Student was `zai-student-transfer-20260910`, model `glm-5.3`, session `zai-1715421a-aa9b-4807-a824-2738669b1b5c` throughout. Five turns took approximately 92, 60, 38, 52 and 45 seconds respectively (job start/finish receipts). Per-round reported token usage, including repeated context and cached-input counts, is in receipt-audit.json; these are not unique exposed-token counts. No cost or comparative-performance estimate is made.

Read **TA-REVIEW.md** for mathematical adjudication and evidence corrections. **exposure-ledger.json** separates retrieval, actual reads, claimed use, proof-supported use and hint exposure. **receipt-audit.json** joins job IDs, model/session, actual commands/results, durable turn IDs and reported usage. Full actual Student content and tool calls/results are in receipts/*-messages.json; private model reasoning fields are excluded. Durable evidence and full job receipts accompany them. Student-authored tool summaries abbreviate or omit content; the actual captured receipts are authoritative.

Student actually read four of five provided pattern bodies across cases and the source lesson. No reviewed memory body was read or claimed used. The TA inspected exact-ID reviewed attachment metadata; the one matched retained row had no body, so it supplies no implementation-read evidence. The library's bounded-only premise is demonstrably invalid; its revision remains unchanged. The dense-extension gap is a bounded search/corpus observation, not absence from all canonical patterns or stores.

The finite-net construction was correctly adapted across problems, but the initial positive target was already correct before source teaching. The observed repair is the premise-preserving counterexample, not a newly enabled positive proof. C3 also needed precision repairs. Same-session learning was intended; no independent-case, memory-only, causal efficacy or prevalence claim follows.

Access was an instructional allowlist on the shared filesystem. The actual boot additionally included Agency system instructions, AGENTS.md and dirty-path metadata; it did not contain the target solution. All observed Student file accesses stayed within the turn's allowed materials/output paths. No TA cascade, withheld reference or future task file was read. No Student network/memory/agent calls occurred. No technical isolation is claimed. No Claude calls, sealed holdout access, RUN4 changes, live role installation, runtime scheduler changes or Lake/Lean invocation occurred.

The queue was empty and held with no session before release. Every subsequent dispatch boundary was checked; no unrelated pending job was observed. Final cleanup explicitly called release again and verified no hold, no pending work, no drain. Student remains registered and idle, with session retained for audit/follow-up. That is not an exclusive scheduler lease; any later reuse must inspect the queue again. See receipts/role-disposition.json.

To verify retained evidence, from futon3c:

```sh
python3 holes/labs/M-apm-demonstration/analysis/ta-transfer-pilot-2026-09-10/audit.py
python3 holes/labs/M-apm-demonstration/analysis/ta-cascade-2026-09-10/verify.py
```

The audit checks hashes and receipt joins, not theorem validity. capture.py/capture-messages.clj document the read-only forensic export of this Student's live message atom used to preserve complete tool outputs beyond normal job truncation. They are not installed runtime features and should not be rerun to overwrite historical snapshots after further Student work.
