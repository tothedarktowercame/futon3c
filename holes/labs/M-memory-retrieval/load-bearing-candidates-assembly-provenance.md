# P2 load-bearing candidate assembly provenance

Recorded on 2026-08-02 on `lucy-joe` to close the assembly leg of the P2
adjudication provenance chain.

## Original assembly artifacts

The assembly ran on 2026-08-01 from 10:20:20Z to 10:23:50Z (11:20–11:23
Europe/London). The `20260731` suffix names the frozen input corpus; no P2
assembly artifact found on this host has a 2026-07-31 mtime.

| artifact | role | mtime (Europe/London) | bytes | SHA-256 |
|---|---|---|---:|---|
| `assemble-load-bearing-candidates.clj` | deterministic assembler | 2026-08-01 11:23:06 | 4,176 | `b06bb3d52317574e1b424709d289be77d0db8678b8ef7432a0b7463128591a97` |
| `coding-sections-20260731.json` | 45-row coding input: job/problem IDs, `used-ids`, and verbatim runner sections | 2026-08-01 11:19:44 | 66,607 | `ef1258ef9fe3c691e8a54edbf0dcbda787094d098d4a935de1959c9a3cbcc552` |
| `receipts-export-20260731-all-authors.edn` | frozen receipt corpus used to obtain each job's recorded outcome | 2026-08-01 11:15:55 | 883,207 | `0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3` |
| `load-bearing-candidates-20260731.jsonl` | frozen candidate population | 2026-08-01 11:23:24 | 20,752 | `1a4e0ee9b3349fbe15f0d24c17cd33f5551d12232f1deac1607c2c2473c788c8` |
| `load-bearing-candidates-20260731-report.md` | original assembly report | 2026-08-01 11:23:39 | 1,610 | `1eb5cf97c2f40121f5e56734294b31d00594d49bb641d3c80cd7504b08a2c573` |
| `load-bearing-candidates-20260731-completion.txt` | exact durable copy of the original completion artifact recovered from `/tmp` | 2026-08-01 11:23:50 | 764 | `d9a11f015d55655352154e0a0d6865968ac45c17fd888adcf0f8a45196bb50b1` |

The population is derived from `coding-sections-20260731.json`: every ID in a
row's `used-ids`, plus every memory explicitly reported as used in prose when
that field is empty. The five known prose-only jobs are encoded explicitly so
the defect remains visible. The receipts export is not used to add or remove
candidates; it supplies the recorded outcome field. The output contains 49
rows: 43 from `used-ids` and 6 marked `prose-only`.

## Reproduction on 2026-08-02

The committed assembler was copied unchanged into an isolated directory under
`/tmp`, together with byte copies of the two frozen inputs, and run there. This
prevented its fixed relative output path from reaching or rewriting the frozen
candidate.

| output | SHA-256 |
|---|---|
| frozen `load-bearing-candidates-20260731.jsonl` | `1a4e0ee9b3349fbe15f0d24c17cd33f5551d12232f1deac1607c2c2473c788c8` |
| isolated rerun output | `1a4e0ee9b3349fbe15f0d24c17cd33f5551d12232f1deac1607c2c2473c788c8` |

`cmp -s` returned 0: the reproduction is byte-identical. The run reported 49
rows with the same 43 `used-ids` / 6 `prose-only` split. The frozen candidate
was not opened for writing and retained its original mtime.

## `/tmp` and working-directory sweep

A sweep covered every `/tmp` file with an mtime from 2026-07-31 through
2026-08-02, then separately searched all `/tmp` names and text for `p2`,
`adjudicat`, `load-bearing`, `candidate`, `coding-sections`, `receipt`,
`prose-only`, and the candidate filename. There were 52 files in the time
window. The P2-relevant findings were:

| path | mtime (Europe/London) | bytes | SHA-256 | disposition |
|---|---|---:|---|---|
| `/tmp/load-bearing-candidates.first.jsonl` | 2026-08-01 11:23:24 | 20,752 | `1a4e0ee9b3349fbe15f0d24c17cd33f5551d12232f1deac1607c2c2473c788c8` | original pre-rerun comparison copy; already preserved byte-for-byte as the frozen candidate |
| `/tmp/futon-invoke-artifacts/codex-2-019fbccf-1785579830658.txt` | 2026-08-01 11:23:50 | 764 | `d9a11f015d55655352154e0a0d6865968ac45c17fd888adcf0f8a45196bb50b1` | copied byte-for-byte to durable `load-bearing-candidates-20260731-completion.txt` |
| `/tmp/futon3c-invoke-jobs.edn` | 2026-08-02 09:48:06 | 21,316 | `f6b2915af1f56553092e9d2a60efe388e8c447e119ba1f7d6511a4bc1f126908` | live multi-job registry containing the original result text, not an assembly input; relevant facts frozen in this record and the completion copy |
| `/tmp/p2-assembly-repro-20260802.cVsgma/holes/labs/M-memory-retrieval/load-bearing-candidates-20260731.jsonl` | 2026-08-02 09:49:23 | 20,752 | `1a4e0ee9b3349fbe15f0d24c17cd33f5551d12232f1deac1607c2c2473c788c8` | isolated rerun output; already preserved byte-for-byte as the frozen candidate |

The isolated rerun directory also contains copies of the assembler and both
inputs, each identical to the durable artifact listed above. No relevant
2026-07-31-mtime file and no adjudication verdict file were found on this
divergent checkout. Nothing was deleted.

## Role boundary

I assembled the full 49-row candidate population without seeing any
adjudication verdicts. My inputs contained receipt fields and runners' own
memory-usage prose, but no load-bearing category, score, ranking, or judgement;
the task explicitly prohibited adjudication, and I neither applied nor hinted
at the later rubric. I sorted only by job ID and memory ID. The adjudicator's
verdict file did not exist in my assembly materials and is still absent from
this divergent `lucy-joe` checkout, so the candidate selection could not have
been steered by the eventual verdicts.

## Git anchors

- `a5d5443` — freezes `assemble-load-bearing-candidates.clj`.
- `def3f20` — preserves the exact original completion artifact recovered from
  `/tmp`.
