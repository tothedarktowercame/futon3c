# CODEX-HANDOFF — S7: the codex sorry-loop cron

Mission: `holes/missions/M-codex-sorry-loop.md` (new slice S7, Joe's
direction 2026-07-28: continuous loop now that usage is ample —
Pro-20x). Prepared by claude-6. **Delivery: Agency bell from claude-6.
Bell claude-6 back with summary + commit SHAs.** Owner reviews, runs
one manual cycle, and installs the crontab entry — **do NOT touch the
crontab yourself.**

## Goal

`scripts/codex_sorry_cron.py` — the codex analogue of
`scripts/apm_formal_zai_cron.py` (READ IT FIRST; its skeleton is the
spec: flock single-instance lock, fail-closed `GateClosed` gates,
progress JSONL, log file, at most ONE dispatch per invocation).
Pointed at the sorry work-queue with the literature protocol, at
hourly-half-past cadence, gated at 50% usage.

## Files

`:in` (READ-ONLY):
- `scripts/apm_formal_zai_cron.py` + `scripts/test_apm_formal_zai_cron.py`
  — the pattern to adapt (structure, logging, lock, fail-closed style).
- `holes/labs/M-codex-sorry-loop/sorry-census-20260728.edn` — queue seed.
- `holes/labs/M-codex-sorry-loop/pilot-1-prereg.md` — rules the packet
  template must carry (incl. the axiom-clean bar and Amendment 2).
- `holes/labs/M-codex-sorry-loop/pilot-1-ledger.edn` — resolved rows.
- `scripts/dispatch_with_recall.clj` — the dispatch vehicle (shell out).
- `holes/missions/M-codex-sorry-loop.md` — hygiene + horizon context.

`:out` (create):
- `scripts/codex_sorry_cron.py`
- `scripts/test_codex_sorry_cron.py`
- `data/codex-sorry-queue.edn` — seeded work queue (see below)
- `data/codex-sorry-packet-template.txt` — per-row packet template
- `holes/labs/M-codex-sorry-loop/s7-cron-README.md` (≤60 lines: gates,
  env vars, cold-start note, owner install line)

## Gates (ALL fail-closed, zai-loop discipline)

1. **Usage gate (the 50% rule)**: read the NEWEST
   `token_count.rate_limits` payload across
   `~/.codex/sessions/**/*.jsonl` files modified in the last 48h;
   dispatch only if `primary.used_percent < MIN` (env
   `CODEX_SORRY_MIN_HEADROOM_USED`, default `50`). Signal older than
   24h or absent → `GateClosed` (cold-start: any manual codex session
   refreshes it; document this in the README).
2. **Concurrency gate**: Agency roster — no more than
   `CODEX_SORRY_MAX_OTHER_INVOKING` (default 1) other codex agents
   `invoking`; runner pool = idle codex agents EXCLUDING `codex-5`
   (scribe seat) and `codex-4`; prefer `codex-6`/`codex-7`/`codex-8`.
3. **Verification backpressure gate**: if ANY queue row is
   `:dispatched` and not yet resolved by ground control (status not
   updated), do NOT dispatch — the loop must never outrun
   verification. (Ground truth is claude-6's job at the completion
   bell; the cron only dispatches.)
4. **Zai-live-session guard**: if any `problems/<id>/` target row's id
   matches a zai agent currently `invoking` per the roster, skip that
   row (cross-lane hygiene).

## The queue (`data/codex-sorry-queue.edn`)

Seeded from the census, EDN vector of rows
`{:id :kind :file :line :statement-hint :unblocks :status :job-id
:dispatched-at :resolved-at :outcome}`. Order: (1) prereg rows 2–5
(schwarz-equality-case, lemniscate components, rouche transfer, radial
ℝ³); (2) remaining `:missing-mathlib-lemma` rows by census rank;
(3) `:hard-proof-step` rows grouped by file (one file = one row —
sessions work files, not individual sorries). EXCLUDE `:scaffold` rows
entirely (policy pending, Joe). Statuses: `:untouched` → `:dispatched`
(cron writes, under flock) → resolved states (ground control writes).

## The packet template

Per-row instantiation carrying (verbatim from the prereg discipline):
target statement(s) + file + line(s); the binding rules (no statement
weakening; hygiene — repo root or `lib/`, never `problems/<id>/` writes
beyond the target file itself when the row IS a problems file;
`lake env lean <file>` exit 0 before commit; **axiom-clean acceptance
with verbatim `#print axioms` output**; ~20-min honest-partial pacing);
**the literature protocol**: before deep work, one bounded probe —
`grep -ri` the local Zulip archive
(`/home/joe/code/corpora/leanprover-zulip-archive`) for the topic (≤5
min), record any anchor found (thread path, technique) in the summary
whether used or not; summary contract (route/anchor, sorry counts,
axiom output, Memory usage with ids, error→fix log, exact obstruction
if blocked).

## Dispatch

Shell out:
`clojure -M scripts/dispatch_with_recall.clj --problem <row-id>
--to <runner> --from claude-6 --mission M-codex-sorry-loop
--subject "<derived from statement-hint>" …` with the instantiated
packet on stdin. Record job-id in the queue row + progress JSONL.
`--dry-run` flag on the cron script prints the chosen row + packet and
exercises every gate without dispatching.

## Acceptance checklist

- [ ] `python3 -m py_compile` clean on both scripts; tests pass
      (`python3 -m pytest scripts/test_codex_sorry_cron.py -q` or the
      unittest equivalent — match the zai test file's style).
- [ ] Tests cover: usage-gate parse + threshold + staleness fail-closed;
      concurrency gate; backpressure gate; queue pick order; flock
      exclusivity; dry-run makes no writes.
- [ ] `--dry-run` run against the real environment prints a sane row +
      gates report (commit its output in the README as a sample).
- [ ] Queue seeded correctly: prereg 2–5 first; no `:scaffold` rows;
      counts stated in README.
- [ ] Crontab UNTOUCHED (owner installs `30 * * * *` after review —
      hourly at half-past per Joe; densify-to-*/15 is a later owner
      decision).
- [ ] `git diff --stat` only this packet's `:out` files.
- [ ] Bell claude-6 with summary + commit SHAs.

## Out of scope

Crontab edits; any store writes (receipts stay ground-control's);
verification logic (claude-6's wake job); scaffold rows; zai lanes.
