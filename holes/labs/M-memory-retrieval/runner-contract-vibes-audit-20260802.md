# Runner-contract audit: which "vibes" must become rules for the cohort

**2026-08-02, claude-7, at Joe's prompt** ("check the runner contract for similar
vibes that should become rules"). Following the receipt-ranking finding, which
showed a prose norm (use-attribution) starves when unenforced. Contract source:
`data/codex-sorry-packet-template.txt` + the dispatch packet
(`dispatch_with_recall.clj`).

## The class, and why the cohort changes the stakes

The contract's load-bearing norms are currently enforced by **human ground-control
review** — a reviewer re-runs the build, re-checks axioms, eyeballs the statement.
That works at low volume. **A cohort generates data faster than any reviewer can
re-verify every run**, so the unreviewed runs drift, and the norm silently
degrades exactly as use-attribution did (`used-count ≈ 0`). For the cohort, each
load-bearing norm must become an **automated gate a run passes before it counts**,
not a courtesy the reviewer catches when they look. The standalone tools already
exist (`vacuity_scan.py`, `queue_audit.py`, `#print axioms`, a statement diff) but
have **no callers** — they are tools, not gates.

## Two enforcement types

**Type A — RE-VERIFY, don't trust the paste.** The runner pastes a claim; the gate
must independently re-run the check. Trusting the pasted output IS the vibe.

| Contract norm | Rule 5/1/6 | Current | Must become |
|---|---|---|---|
| **Statement integrity** — "do not weaken, replace, or silently alter the target statements" | R1 | reviewer eyeballs (the "announcement gate" concept) | automated **statement diff**: frozen original target vs submitted; any change → reject. *A proof of a mutated statement is worthless — this is the highest-value gate.* |
| **Axiom-cleanliness** — "report the VERBATIM `#print axioms`… any `sorryAx` means relocated" | R5 | runner pastes `#print axioms`; reviewer may re-run | harness **re-runs `#print axioms`** on every claimed-complete decl; a pasted-clean report over a `sorryAx` file cannot pass |
| **Non-triviality / no disguise** — "never disguise an obligation with an axiom, weakened theorem, or irrelevant proof" | R6 | `vacuity_scan.py` exists, un-wired | wire vacuity/triviality + the statement diff into acceptance (`f = 0` passes vacuity but is degenerate) |

**Type B — GATE THE COVERAGE.** The runner must emit a structured per-item verdict
against a set the harness knows; missing coverage → `:incomplete`. Same shape as
the use-attribution fix already recorded.

| Contract norm | Rule | Current | Must become |
|---|---|---|---|
| **Dispatch-time use-attribution** — "Memory usage section naming every used/ignored surfaced ID" | R-summary | free prose, ungated (→ inert receipt-ranking) | per-surfaced-id `USED/IGNORED` verdict, gated vs the offered receipt (already recorded: `E-memory-v3-programme.md` cohort requirement) |
| **Error-time memory attribution** — "If a surfaced rule applies, use it and cite its memory id" | R4 | free prose, ungated | the error-recall twin: per-surfaced-error-rule `USED/IGNORED` verdict, gated vs `error_recall.bb`'s surfaced set |
| **Which search arm carried it** — "(a)/(b)/(c)/none, plus arms tried and empty" | R2/summary | free prose; "a vague 'searched everything' is not [valuable]" — but nothing rejects vague | structured arm field ∈ {a,b,c,none} required; a report without it is `:incomplete`. This is a tracked sensor and will starve like use-attribution if ungated. |

**Type C — hygiene (lower value).** Commit scope (R1/R7, "modify only the target
file / commit only scoped files"): gate the commit diff's file set ⊆ the scoped
set. Cheap, mechanical, currently trusted.

## Recommendation

For the cohort, promote Type A and Type B to automated gates in the dispatch
acceptance path, in this priority: **statement diff** (worthless-if-mutated),
**axiom re-run** (trust-the-paste is the hole), then the three coverage sensors
(use / error / arm) so the witness channels the cohort depends on are not starved.
Type C is a cheap add. The principle is one line: **at cohort scale, a norm that
is not a gate is not a norm** — the reviewer who was the enforcement does not scale
with the run volume.
