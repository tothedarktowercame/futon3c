# CODEX-HANDOFF — S8: error-time recall for the sorry loop

Mission: `holes/missions/M-codex-sorry-loop.md` (new slice S8, Joe's
direction 2026-07-29). Prepared by claude-6. **Delivery: Agency bell
from claude-6. Bell claude-6 back with summary + commit SHAs.**

## Why (one paragraph, load-bearing)

Dispatch-time recall has been useful in 1 of 7 rows, while the
promoted arc-lane memories — error→fix rules scoped BY error shape —
go unconsumed: they are delivered at the wrong time with the wrong
key. S8 moves arc-memory consumption to the moment it was designed
for: the compile failure. ChatGPT's compile-feedback suggestion,
assessed 2026-07-29 (ops log ⊸prop), reduced to this one stealable
idea.

## Goal

`scripts/error_recall.bb` — a CLI the RUNNER invokes on each compile
failure: `bb scripts/error_recall.bb --job <job-id> "<error text>"` →
prints ≤3 matching memories compactly. Plus the packet-template
integration and the local query log that ground control harvests into
receipts.

## Seat discipline (BINDING)

The runner READS the store, never writes. The script performs bounded
read-only queries; every query is logged to a LOCAL file
(`FUTON3C/.state/error-recall/<job-id>.jsonl` — one JSON line per
query: ts, job-id, error-excerpt ≤300 chars, extracted terms, surfaced
memory ids). Ground control harvests that log at row verification and
writes the offered/outcome receipt halves itself. NO store writes from
this script under any circumstances.

## Files

`:in` (READ-ONLY):
- `scripts/codex_sorry_cron.py` — `subjects_for` (reuse its
  tokenize+stopword approach for error text; add error-vocabulary
  stopwords: error, failed, expected, type, term, unknown, identifier,
  invalid, tactic, goal, unsolved, line, column).
- `holes/labs/M-codex-sorry-loop/promote_scribe_pass_2_3.bb` —
  bounded-read conventions (timeouts incl. the 60s slow-negative-path
  lesson, 503 handling: back off once then EXIT 0 QUIETLY — a recall
  miss must never break a proof session).
- One promoted arc memory for the match surface, e.g. GET
  `127.0.0.1:7073/api/alpha/evidence/e-codexpilot-specify-invariant-measure-when-lintegral-translation-stalls`
  — note :evidence/body :body holds :scope/:before/:after/:rule.
- `data/codex-sorry-packet-template.txt` — to be extended.

`:out`:
- `scripts/error_recall.bb`
- `scripts/test_error_recall.bb` (bb test script, runnable directly;
  cover: term extraction incl. lemma-name preservation
  (`setIntegral_mono` must survive tokenization INTACT — identifiers
  are the highest-signal terms, do NOT lowercase-split camelCase),
  stopwording, empty-result exit 0, store-down exit 0 quietly,
  log-line shape, ≤3 results cap).
- Updated `data/codex-sorry-packet-template.txt` (see below).
- `holes/labs/M-codex-sorry-loop/s8-note.md` (≤50 lines incl. a REAL
  demo transcript — see acceptance).

## Behavior

1. Extract terms from the error text: Lean identifiers preserved
   verbatim (regex for dotted/camelCase names), plus stopworded words
   ≥4 chars; cap 12 terms.
2. Query `127.0.0.1:7073/api/alpha/evidence/text-search?q=<terms>&limit=8`
   (10s timeout, one 5s-backoff retry on 503, then quiet exit 0).
3. Filter results to `:evidence/type :memory` entries; rank arc-lane
   tags first (`:arc-lane` in `:evidence/tags`), then solve-lane; cap 3.
4. Print compactly per hit: name, the :rule (or :before → :after),
   memory id, one line each — a runner mid-proof reads this in
   seconds. Print NOTHING (exit 0) when no hits: silence beats noise
   mid-session.
5. Append the query log line (local file, mkdir -p as needed) whether
   or not hits were found — ground control needs the miss rate.

## Packet template additions (exact contract)

- New section "ON COMPILE ERRORS": run
  `bb /home/joe/code/futon3c/scripts/error_recall.bb --job @@JOB@@ "<error text>"`
  before broad Mathlib searching; if a surfaced rule applies, apply it
  and cite its id.
- Mathlib search pointers: https://loogle.lean-lang.org (pattern/type
  search) and https://leansearch.net (natural language) — consult
  before unbounded source grepping.
- Memory-usage summary contract extended: cite error-time surfaced ids
  used/ignored alongside dispatch-time ones.
- `@@JOB@@` marker: the cron already instantiates markers in
  `instantiate_packet` — add `@@JOB@@`... NOTE the job-id is not known
  at packet-build time (dispatch returns it). Simplest correct move:
  use `@@ID@@` (the row id, already available) as the log key instead
  of job-id; adjust the script flag to `--row <row-id>`. Ground
  control joins row→job via the queue. Do it that way.

## Acceptance checklist

- [ ] `clj-kondo` 0 errors on both bb files; `check-parens` clean.
- [ ] `bb scripts/test_error_recall.bb` passes.
- [ ] REAL demo in s8-note.md: run the script against this exact error
  text from the a95A01 session — "setIntegral_mono requires a global
  pointwise inequality" — and show it surfaces
  `switch-to-setIntegral-mono-on-for-cell-local-bounds`. If it does
  NOT, tune term extraction until it does (this is the acceptance
  bar: the tool must find the memory that its own motivating case
  produced).
- [ ] Second demo: store unreachable (bad port) → exit 0, silent, log
  line still written.
- [ ] `python3 -m pytest scripts/test_codex_sorry_cron.py -q` still
  passes after the template change (11 tests).
- [ ] `git diff --stat` only this packet's files.
- [ ] Bell claude-6 with summary + commit SHAs.

## Out of scope

Store writes from the script (NEVER); Mathlib-declaration indexing
(later phase, gated); changes to dispatch_with_recall or the
recall-system version (claude-4's interface); cron gate changes.
