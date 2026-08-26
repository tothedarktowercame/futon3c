# Shelf-order audit — 2026-08-26

This is an offline audit of the archived `jit-all-open-nontopology-v1` student attempts from f28–f42. It evaluates the delivered shelf order (a), same-problem provenance (b), Lean-identifier overlap with the base problem (c), and the approved combined order (d). The script imports `LEAN_TOKEN`, `base_file`, `attempts`, `fetch_memory`, and `body_text` from `fingerprint_audit.py`; receipts and snapshots are parsed as EDN structures in a Clojure subprocess.

## Summary

Statistics count memory-use events, so a memory used in multiple attempts contributes one row per attempt. Positions that are not on the delivered shelf have no meaningful counterfactual shelf position and are excluded from position statistics; 19 of 75 rows are in that category.

| Population | Ordering | Positioned | Median | Mean | Top 5 | Top 10 |
|---|---|---:|---:|---:|---:|---:|
| All | a: delivered | 56 | 18.5 | 17.04 | 15 | 19 |
| All | b: same problem | 56 | 2.5 | 4.61 | 47 | 52 |
| All | c: identifier overlap | 56 | 3.0 | 5.23 | 40 | 49 |
| All | d: combined | 56 | 3.0 | 4.04 | 50 | 53 |
| Cross-problem | a: delivered | 5 | 15 | 23.20 | 0 | 1 |
| Cross-problem | b: same problem | 5 | 18 | 24.60 | 0 | 1 |
| Cross-problem | c: identifier overlap | 5 | 15 | 18.40 | 1 | 2 |
| Cross-problem | d: combined | 5 | 16 | 18.80 | 1 | 2 |

Yes, combined ordering (d) decisively beats delivered/hash order (a) overall: median position falls from 18.5 to 3, mean from 17.04 to 4.04, top-5 rises from 15 to 50, and top-10 from 19 to 53.

No, identifier overlap alone does not rescue f42's closing memory. `e-f72e5ece…310caf69` moves from 47/48 to 48/48 with overlap score 0. Combined ordering also places it at 48/48 because it is cross-problem. The same-problem key supplies most of the overall gain; among the five known cross-problem uses, identifier overlap improves the mean but not the median.

## Per-use rows

`N/S` means the used memory was not on the delivered shelf. Scores are counts of distinct shared Lean tokens.

| Frame | Attempt | Used memory | Provenance problem | Shelf | a | b | c | d | Score |
|---|---:|---|---|---:|---:|---:|---:|---:|---:|
| f28 | 3 | `e-0d0d3806…02fbad8d` | unknown | 0 | N/S | N/S | N/S | N/S | 23 |
| f28 | 3 | `e-98785d73…6da97641` | unknown | 0 | N/S | N/S | N/S | N/S | 26 |
| f28 | 3 | `e-bc26b67e…8a65ad76` | unknown | 0 | N/S | N/S | N/S | N/S | 23 |
| f29 | 1 | `e-1866fc8e…7c224238` | unknown | 3 | 1 | 1 | 1 | 1 | 0 |
| f29 | 2 | `e-81a44d2c…2a1eb8dd` | unknown | 3 | 2 | 2 | 2 | 2 | 0 |
| f29 | 2 | `e-93b083ba…ab25a2de` | unknown | 3 | N/S | N/S | N/S | N/S | 29 |
| f29 | 2 | `e-1866fc8e…7c224238` | unknown | 3 | 1 | 1 | 1 | 1 | 0 |
| f29 | 2 | `e-f72e5ece…310caf69` | unknown | 3 | N/S | N/S | N/S | N/S | 28 |
| f29 | 2 | `e-d2563094…5ad3ada3` | unknown | 3 | N/S | N/S | N/S | N/S | 26 |
| f29 | 3 | `e-83ece32c…6e6265f5` | unknown | 3 | N/S | N/S | N/S | N/S | 62 |
| f29 | 3 | `e-021bf80a…108521cf` | unknown | 3 | N/S | N/S | N/S | N/S | 48 |
| f29 | 3 | `e-c86a7d45…deef074f` | unknown | 3 | N/S | N/S | N/S | N/S | 55 |
| f29 | 3 | `e-f6b4c56b…2bad9588` | unknown | 3 | N/S | N/S | N/S | N/S | 74 |
| f29 | 3 | `e-f72e5ece…310caf69` | unknown | 3 | N/S | N/S | N/S | N/S | 28 |
| f29 | 3 | `e-d2563094…5ad3ada3` | unknown | 3 | N/S | N/S | N/S | N/S | 26 |
| f30 | 1 | `e-6b355807…52f6de7c` | unknown | 3 | 3 | 3 | 3 | 3 | 0 |
| f30 | 1 | `e-8a40d240…a8f5275f` | unknown | 3 | N/S | N/S | N/S | N/S | 43 |
| f30 | 2 | `e-f0db6e27…42a97481` | unknown | 3 | N/S | N/S | N/S | N/S | 96 |
| f30 | 2 | `e-6b355807…52f6de7c` | unknown | 3 | 3 | 3 | 3 | 3 | 0 |
| f30 | 2 | `e-14100165…44ddaed5` | unknown | 3 | 1 | 1 | 1 | 1 | 0 |
| f30 | 3 | `e-14100165…44ddaed5` | unknown | 3 | 1 | 1 | 1 | 1 | 0 |
| f30 | 3 | `e-20851bde…0f741fe9` | unknown | 3 | N/S | N/S | N/S | N/S | 20 |
| f30 | 3 | `e-6aea0c17…7b725688` | unknown | 3 | 2 | 2 | 2 | 2 | 0 |
| f30 | 3 | `e-6b355807…52f6de7c` | unknown | 3 | 3 | 3 | 3 | 3 | 0 |
| f32 | 2 | `e-fddf046d…d46ac12c` | a92J01 | 21 | 21 | 2 | 1 | 1 | 28 |
| f32 | 2 | `e-bad2ad5e…854d2100` | a92J01 | 21 | 14 | 1 | 2 | 2 | 22 |
| f32 | 3 | `e-fddf046d…d46ac12c` | a92J01 | 22 | 22 | 3 | 1 | 1 | 28 |
| f32 | 3 | `e-bad2ad5e…854d2100` | a92J01 | 22 | 14 | 1 | 2 | 2 | 22 |
| f32 | 3 | `e-e9f9c621…186384c4` | a92J01 | 22 | 18 | 2 | 3 | 3 | 10 |
| f33 | 1 | `e-codexpil…stimates` | unknown | 22 | N/S | N/S | N/S | N/S | 13 |
| f33 | 2 | `e-cfb5c7dc…e669cc2b` | unknown | 22 | N/S | N/S | N/S | N/S | 20 |
| f33 | 3 | `e-cfb5c7dc…e669cc2b` | unknown | 23 | N/S | N/S | N/S | N/S | 20 |
| f33 | 3 | `e-3411c0c2…73831a7b` | a94A07 | 23 | 6 | 1 | 1 | 1 | 9 |
| f34 | 2 | `e-07a2f8d1…3f2dc458` | a95J03 | 25 | 2 | 1 | 1 | 1 | 31 |
| f34 | 2 | `e-10323bb7…67503b01` | a95J03 | 25 | 4 | 2 | 2 | 2 | 19 |
| f37 | 3 | `e-01c38dee…0d3dd55c` | a96A08 | 26 | 1 | 1 | 3 | 2 | 8 |
| f37 | 3 | `e-0d5fcb3e…688e0348` | a96A08 | 26 | 4 | 2 | 12 | 5 | 1 |
| f37 | 3 | `e-73ac922d…9440f39b` | a96A08 | 26 | 13 | 3 | 6 | 3 | 4 |
| f37 | 3 | `e-a581383e…ed6d73a4` | a96A08 | 26 | 17 | 4 | 1 | 1 | 10 |
| f37 | 3 | `e-apm-prom…fc530f52` | a96A08 | 26 | 19 | 5 | 7 | 4 | 4 |
| f39 | 2 | `e-63b7c7c1…4762fbea` | a96A08 | 27 | 10 | 10 | 3 | 3 | 16 |
| f39 | 3 | `e-23a2940f…e201eb31` | a97A01 | 29 | 8 | 1 | 2 | 1 | 20 |
| f39 | 3 | `e-6556333d…d452553a` | a97A01 | 29 | 12 | 2 | 8 | 2 | 14 |
| f39 | 3 | `e-1ac936fb…c474401c` | unknown | 29 | N/S | N/S | N/S | N/S | 12 |
| f40 | 1 | `e-apm-prom…92011270` | a97J05 | 31 | 19 | 1 | 2 | 2 | 22 |
| f40 | 1 | `e-apm-prom…e872ba0d` | a97J05 | 31 | 24 | 4 | 5 | 3 | 16 |
| f40 | 1 | `e-apm-prom…05f28e74` | a97J05 | 31 | 23 | 3 | 1 | 1 | 23 |
| f40 | 1 | `e-apm-prom…3aa96d98` | a97J05 | 31 | 21 | 2 | 10 | 4 | 13 |
| f40 | 2 | `e-apm-prom…92011270` | a97J05 | 33 | 20 | 2 | 3 | 3 | 22 |
| f40 | 2 | `e-dda90056…e6754620` | a97J05 | 33 | 30 | 6 | 4 | 4 | 22 |
| f40 | 2 | `e-4984bc4c…b8c91fd9` | a97J05 | 33 | 10 | 1 | 1 | 1 | 23 |
| f40 | 2 | `e-apm-prom…e872ba0d` | a97J05 | 33 | 25 | 5 | 7 | 5 | 16 |
| f40 | 3 | `e-4984bc4c…b8c91fd9` | a97J05 | 34 | 11 | 2 | 1 | 1 | 23 |
| f40 | 3 | `e-dda90056…e6754620` | a97J05 | 34 | 31 | 7 | 5 | 5 | 22 |
| f40 | 3 | `e-0a6c4c9c…afdd325b` | a97J05 | 34 | 3 | 1 | 3 | 3 | 22 |
| f40 | 3 | `e-apm-prom…92011270` | a97J05 | 34 | 21 | 3 | 4 | 4 | 22 |
| f40 | 3 | `e-apm-prom…e872ba0d` | a97J05 | 34 | 26 | 6 | 8 | 6 | 16 |
| f40 | 3 | `e-apm-prom…3aa96d98` | a97J05 | 34 | 23 | 4 | 13 | 7 | 13 |
| f40 | 3 | `e-e9f9c621…186384c4` | a96A08 | 34 | 32 | 32 | 18 | 18 | 6 |
| f41 | 1 | `e-apm-prom…3168fb82` | a97J06 | 39 | 21 | 1 | 1 | 1 | 29 |
| f41 | 1 | `e-apm-prom…0ab3ce5b` | a97J06 | 39 | 25 | 2 | 14 | 3 | 14 |
| f41 | 1 | `e-apm-prom…0a6d9fdc` | a97J06 | 39 | 31 | 3 | 2 | 2 | 25 |
| f41 | 1 | `e-73ac922d…9440f39b` | a97J05 | 39 | 15 | 18 | 8 | 9 | 18 |
| f41 | 2 | `e-apm-prom…3168fb82` | a97J06 | 41 | 22 | 2 | 1 | 1 | 29 |
| f41 | 2 | `e-apm-prom…0ab3ce5b` | a97J06 | 41 | 26 | 3 | 16 | 5 | 14 |
| f41 | 2 | `e-apm-prom…0a6d9fdc` | a97J06 | 41 | 32 | 4 | 4 | 4 | 25 |
| f41 | 2 | `e-082d154e…0d47f07c` | a97J06 | 41 | 3 | 1 | 2 | 2 | 28 |
| f41 | 2 | `e-bd971ae9…e0abe47e` | a97J06 | 41 | 36 | 5 | 3 | 3 | 26 |
| f41 | 3 | `e-apm-prom…3168fb82` | a97J06 | 42 | 23 | 3 | 1 | 1 | 29 |
| f41 | 3 | `e-apm-prom…0a6d9fdc` | a97J06 | 42 | 33 | 5 | 4 | 4 | 25 |
| f41 | 3 | `e-bd971ae9…e0abe47e` | a97J06 | 42 | 37 | 6 | 3 | 3 | 26 |
| f41 | 3 | `e-3411c0c2…73831a7b` | a97J05 | 42 | 12 | 16 | 15 | 16 | 14 |
| f42 | 1 | `e-f72e5ece…310caf69` | a97J06 | 48 | 47 | 47 | 48 | 48 | 0 |
| f42 | 3 | `e-apm-prom…1134fa83` | a97J07 | 50 | 29 | 1 | 6 | 2 | 5 |
| f42 | 3 | `e-apm-prom…7503bf6b` | a97J07 | 50 | 41 | 4 | 8 | 4 | 5 |

## Caveats and observed data differences

- The handoff says 13 frames have solver-memory snapshots, but the archive contains 12: f28, f29, f30, f32, f33, f34, f35, f37, f39, f40, f41, and f42. The script discovers rather than invents the missing frame.
- `:accessible-memory-ids` is absent for seven used attempts (f28 attempt 3; all three attempts in f29 and f30), so those rows use the specified solver-snapshot fallback. The f28 solver shelf is empty.
- Eleven used-attempt shelves differ from their solver snapshot. Each difference is a guide-added union shelf (one to four additions, no removals), so auditing the receipt shelf materially changes the positions.
- Nineteen used-memory events are not on their delivered shelf. This includes f33 attempt 1's open-corpus result and several early fallback-era uses. They remain explicit in the output rather than being dropped.
- Unknown snapshot provenance is not classified as cross-problem. The cross-problem summary therefore contains the five rows whose provenance is known and differs from the frame problem.
- The handoff describes f42's `e-f72e5ece…310caf69` as originating in a01J05. Every f42 snapshot structure containing it records provenance problem `a97J06`; the audit follows the snapshot data as instructed.
- Identifier overlap intentionally includes identifiers already present in the base problem. It counts distinct `LEAN_TOKEN` matches shared between the base file and memory name, hook, and body.
- The final end-to-end run took 1.8 seconds; memory fetches were not a noticeable bottleneck.

The full machine-readable rows, shelf comparisons, fallback list, missing-entry list, and unrounded statistics are in `shelf-order-2026-08-26.json`.

## Correction — same day (claude-19, reviewing)

The row for f42 attempt 1 (`e-f72e5ece…`, score 0, 47/48 → 48/48) was wrong,
and the cause reaches 19 distinct memories across the campaign. **586 of the
1,099 snapshot entries on disk carry no `name`/`hook`/`body`** — older
promotions are recorded by `content-digest` only (13 of f42's 48 entries; 19 of
f32–f35's 19–27). The script scored those entries from the empty snapshot text,
so every textless entry got overlap 0 whatever it said. The tell was already in
the table: positioned rows scoring 0 beside not-on-shelf rows scoring 20–96,
because the latter go through `fetch_memory`. Fixed by fetching text for any
entry with fewer than 20 characters (`textless_snapshot_entry_count` is now in
the JSON: 19). Re-run, 2.2 s, JSON regenerated.

| Population | Ordering | Positioned | Median | Mean | Top 5 | Top 10 |
|---|---|---:|---:|---:|---:|---:|
| All | a: delivered | 56 | 18.5 | 17.04 | 15 | 19 |
| All | b: same problem | 56 | 2.5 | 4.61 | 47 | 52 |
| All | c: identifier overlap | 56 | 3.0 | 5.29 | 38 | 48 |
| All | d: combined | 56 | 3.0 | **3.48** | 50 | **54** |
| Cross-problem | a: delivered | 5 | 15 | 23.2 | 0 | 1 |
| Cross-problem | b: same problem | 5 | 18 | 24.6 | 0 | 1 |
| Cross-problem | c: identifier overlap | 5 | **8** | 11.4 | 2 | 3 |
| Cross-problem | d: combined | 5 | 9 | 12.2 | 1 | 3 |

f42 attempt 1: `e-f72e5ece…` now scores 6 (shared: `closedBall`,
`DiffContOnCl`, `norm_le_of_forall_mem_frontier_norm_le`, …) and sits at
**5/48** under identifier overlap, 7/48 combined, from 47/48 delivered.

Two readings survive the correction and one changes. Combined still beats
delivered decisively, and key 1 still supplies most of the overall gain. What
changes is key 2: it is no longer "does not rescue f42" — it does, and it is
the only key that moves the cross-problem rows (median 15 → 8).

One naming point for whoever ships this: the snapshot's `:provenance
:problem-id` is the problem of the **frame that promoted** the memory, not the
problem the memory was mined from (`e-f72e5ece…` reads a01J05 in its body and
carries `a97J06` here). Key 1 as measured is therefore "promoted in this
frame first", and should be named that way in the ordering record.
