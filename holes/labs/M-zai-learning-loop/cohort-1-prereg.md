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

## Amendment 1 (2026-07-26, before session-1 outcome known; Joe's direction)

Session 1 surfaced recall-empty for a92-terrain: the current recall is
lexically gated (description-less patterns, boilerplate query terms, no
semantic lane). Rather than repeat memoryless behaviour five times, the
memory system is being FIXED IN PARALLEL, targeted to come online mid-cohort:

- **Each row now records :recall-system** — v0-lexical (as-dispatched today)
  or v1-enriched (pattern descriptions + problem-file query terms +
  embedding proposal lane if feasible). Sessions 1–2 expected v0; later
  sessions v1 as fixes land (actual recorded, not assumed).
- The cohort's comparison is now explicitly WITHIN-cohort: broken-by-design
  vs working recall on comparable fresh problems, plus the a96A04 revisit
  arm under v1.
- Fix lanes: codex-6 (pattern descriptions from ground-control vocabulary +
  query-term extraction from problem.md/proof-outline.md), codex-5
  (embedding proposal lane feasibility via futon3a miniLM shared-corpus
  infra; timeboxed — feasibility verdict acceptable if build is large).
