# BPM evaluation batch 0 — preregistration

Registered: 2026-07-25, claude-4 (ground control), casting per Joe: runner zai-4 (fresh seat).
Charter: holes/missions/M-zai-learning-loop.md (held-out evaluation section).

- **Population:** the 10 bpm-starter problems (bpm-starter/README.md), stubs at apm-lean e40ebd9.
- **Order (fixed):** 1-1-1, 1-2-2, 1-3-1, 1-4-1, 1-5-1, 1-6-2, 1-7-1, 1-8-1, 1-1-2, 1-3-2.
- **One session per problem, sequential.** No retries in-batch; the session's terminal state is the attempt's outcome. Every outcome stays in the denominator.
- **Baseline condition:** near-empty store, NO recall injection (S0 not built) — batch 0 measures runner-alone capability; later batches measure runner+loop.
- **Outcomes per problem** (mechanically witnessed): solved = lake exit 0, 0 sorries, 0 errors, commit sha; partial = compiles with sorries remaining; failed = does not compile / no commit. Turn count = turn-round evidence entries for the session (Evidence Landscape).
- **Quarantine:** runner instructed not to call memory_record in these sessions; sessions excluded from all scribe mining; any breach = held-out-contamination, recorded, voids the affected problem's row (not the batch).
- **Review:** ground control re-runs every compile receipt; tally reported to operator with per-problem rows.
