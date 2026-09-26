# P2 V2-6 candidate assembly report

- Population: 49 recorded uses from 45 dispatch rows.
- Source split: 43 `used-ids`; 6 `prose-only`.
- Prose-only uses: job sequences 455 (1), 467 (1), 468 (2), 471 (1), and 474 (1).
- Candidate: `load-bearing-candidates-20260731.jsonl`.
- Candidate SHA-256 / deterministic rerun SHA-256: `1a4e0ee9b3349fbe15f0d24c17cd33f5551d12232f1deac1607c2c2473c788c8`.
- Stable order: receipt/job ID, then memory ID.
- Input hash before and after assembly, `coding-sections-20260731.json`: `ef1258ef9fe3c691e8a54edbf0dcbda787094d098d4a935de1959c9a3cbcc552`.
- Input hash before and after assembly, `receipts-export-20260731-all-authors.edn`: `0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3`.
- No adjudication was performed. The candidate has no category, label, score, evaluative ordering, summary judgement, or recommendation.

Found but deliberately not fixed:

- Sparse `used-ids`: the six prose-reported uses remain marked `source: prose-only`; they were not reconciled into the receipt field.
- Statement-order term selection and absent lane-scoped rarity ranking were not repaired.
- Job 448 has two outcome receipts in the frozen export. The export is newest-first, so the deterministic assembler retains the newest recorded classification (`partial-now-unblocked-by-infrastructure-fix`) without modifying either receipt.
- The early job 167 usage report names its two memories by title rather than evidence ID. The deterministic assembler maps those two frozen titles to the IDs already present in that row's `used-ids`; it does not alter the source report.
