# Cohort 1 — first recall-live training cohort (preregistration)

Registered 2026-07-26, claude-4 (ground control). Casting per Joe: runner
zai-5 (fresh seat; APM cron quota-gated off at 54% usage, no contention).
Charter: holes/missions/M-zai-learning-loop.md.

- **Population (fixed, deterministic — first five unattempted APM problems in
  id order):** a92J02, a93A01, a93A03, a93J02, a93J05. One session each,
  sequential. Session shape: formalize (create lean/Main.lean per a95-series
  conventions) + prove; mechanical witness = lake exit/sorry count/commit.
- **Recall LIVE:** every session dispatched via scripts/dispatch_with_recall.clj
  (offered-receipts recorded; recall timeout 30s bounded; recall-empty
  degrades gracefully and is a typed row condition, not a failure).
- **Scribe pass per session:** codex-2 three-lane mining after each session;
  drafts to operator review; promotion via the proven path. Training
  problems are mineable (no quarantine).
- **Sub-arm (labeled, not counted with the fresh five): one revisit session
  on a96A04** — the Young-blocker problem — testing whether the recalled
  missing-dependency-protocol memory changes behavior at the site it was
  mined from. Confounded by prior repo work; reported as its own row.
- **Counting rule:** batch-0 discipline verbatim. No retries; every outcome
  in the denominator; pacing note (edit early, commit honest partial ~20m)
  in every packet from session 1.
- **Observables:** all four levels. Leaf level live for the first time:
  surfaced-ids vs used-ids in receipts + arc evidence of effect. Zero
  leaf-hits remains publishable.
