# capability-proof-arxiv — review checklist

Working document. **Not part of the paper.** Tracks what the independent
adversarial review (`futon6/holes/TN-codex-review-of-capability-proof-arxiv.md`,
codex-5) found, what has been corrected in the paper, and what is still
outstanding. The paper itself states only current fact plus explicit
*pending rerun* markers; the reasoning about how it got there lives here.

Status key: **DONE** corrected in the paper · **PENDING RERUN** needs compute
before the paper can claim it · **OPEN** needs a decision or a fix ·
**WONTFIX** deliberate, with reason.

---

## Verified by the review — no action

The entire object-level census reproduced from artifacts by an independent
agent: 320,337 S1 marks (and every kind sub-count); 883 nodes / 419 edges / 410
holes; 280 scopes over 20 kinds; 358 typed boxes in 8 methods; 198 carried
sorry-holes; missing-warrant 222/383 = 57.96%; 88 typed / 10 cyclic / 0 failed;
entropy gate 0.02 off-diagonal cosine, macro entropy 0.53, PASS.

This is the part of the paper that was checked mechanically as it was produced,
and it held. Everything below was asserted in prose.

---

## Corrections applied to the paper

| # | Finding | Was | Now | Status |
|---|---|---|---|---|
| 1 | `e2e-16` ledger incomplete | "S1–S12 ledgered 12/12" | A12 marked **partial**: 12/12 ledgered, 9 executed under this corpus id, 3 pending rerun | **DONE** |
| 2 | A10 id collision | "10 paper signatures", twin-sim 0.75 | 9 signatures (papers with ≥2 proofs); twin similarity marked *pending recomputation* | **DONE** |
| 3 | A6 contradicted by its own producer | "ledgered; orphan check clean" | **weak**: 12 objects for 16 papers, `wellformed:false` ×2, 33 orphans, output outside the RETRIEVE path | **DONE** |
| 4 | A3 retry rate | 48% | 45.9% (45/98 finals carry a retry), definition stated | **DONE** |
| 5 | A3 gate selection | "98/98 gated PASS" | qualified: passes *when finals are selected explicitly*; the directory gate also sees 98 `.rung2` reports and fails on them | **DONE** |
| 6 | A8 "95 theorems" not derivable | asserted alongside 446/286 | export stated as 446 nodes / 286 edges (88 Proof + 358 Step); XTDB figures cited to their separate artifact | **DONE** |
| 7 | A2 substrate wording | "substrate-corpus match measured" | adds that the stage *checks* a shipped substrate rather than rebuilding per corpus; corpus-fresh rebuild is H1 tier 2 | **DONE** |
| 8 | S4 filled slots composition | 222 listed as 83+34+22+13+11 = 163 | shows the omitted `slot: 59` generic category | **DONE** |
| 9 | A1/A2 paper counts | 12/12 | 16/16 | **DONE** |
| 10 | S1 census not reproducible as shipped | implied byte-stable | notes the frozen manifest is required; default scan reads a mutable directory | **DONE** |

---

## Pending rerun — the paper marks these, the compute is outstanding

| # | Item | What is needed | Cost |
|---|---|---|---|
| R1 | **Clean single-corpus execution of S3/S4/S7** under `math-ct-e2e-16` | The artifacts are the same 98 graphs from the same 16 papers; only the label differs. A clean run removes the caveat from A12 entirely. | ~2 days CPU on Zone (LLM stages), or hours on any GPU host |
| R2 | **Paper-twin similarity recomputation** | The prior 0.75 was computed over a partition in which legacy ids collapsed. Recompute after the id fix. | Seconds — needs only `clean_paper_signature` re-run, but wants R1's clean embed to be quotable |
| R3 | **A3′ anchor validation at volume** | H21's fix is validated at n=3 (3/3 exact vs 41% baseline). Needs enough re-mined proofs to warrant. | Hours CPU; re-mine ~30 proofs under the numbered prompt |

---

## Open — needs a fix or a decision

| # | Item | Note |
|---|---|---|
| O1 | **S3 directory gate treats `.rung2` reports as graphs** | The stage's own gate fails on its own sidecar files; the component result is sound only because finals are selected by hand. Fix the gate's glob. |
| O2 | **S5 output contains 98 spurious `no-structure` rows** | Same root cause as O1 — rung-2 reports processed as graphs. The paper's 6/82/10 distribution silently selects the valid half. |
| O3 | **S9 masks its first sub-command's failure with `;`** | Same defect class as the S11 one already fixed (H22). Audit every stage command for `;` between sub-steps. |
| O4 | **Stepper can exit 0 after reporting a refusal or gate failure** | Process exit code does not reflect stage outcome, so a caller cannot detect failure without parsing stdout. |
| O5 | **A6: S6 writes outside the RETRIEVE path** | `paper_graph_assemble` defaults to `data/paper-graphs`; RETRIEVE collects `data/iatc-paper-graphs/$RUN_ID`. Also: 2 objects `wellformed:false`, 33 orphans, and the promised expository-edge pass does not exist in S6. |
| O6 | **Hazard close-count not mechanically derivable** | `E-superpod-hardening.md` is prose with sub-hazards (H11b, H12b, H19b/c) and stale status headings. Either make it a state table with one row per hazard, or stop quoting a count in the paper. |
| O7 | **DAG contract describes an older stage numbering** | The contract and the runnable `OPS` dict disagree; the paper cites the contract. |
| O8 | **Source/PDF drift** | The reviewed PDF was built before the reviewed source. Any third-party handoff should record source and PDF hashes together. |

---

## Method note worth keeping

Every error the review found was in the direction of the claim the paper wanted
to make, and none was in the mechanically-checked counts. The numbers were
verified as they were produced; the integration claims were prose about how the
pieces fit, and prose was the unchecked surface. The operational consequence is
to run the replay harness *before* asserting an integration claim rather than
after — when codex-5 ran it, it reported the ledger gap and recommended abort,
which is exactly what it exists to do.
