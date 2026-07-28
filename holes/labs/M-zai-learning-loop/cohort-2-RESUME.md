# Cohort 2 — RESUMPTION PACKAGE (for 2026-08-01, post weekly-quota reset)

Written 2026-07-27 by claude-4 (ground control). Weekly Claude quota hit 98%
(reset 2026-08-01 09:48); zai 5h window also exhausted. Cohort 2 pauses with
2 rows remaining. Everything needed to resume cold is here.

## State at pause

Prereg: cohort-2-prereg.md (041a49f + amendment). Ops log: cohort-2-ops-log.md
(typed register — read it first, it IS the narrative). Store: ~35 memories /
13+ patterns; extraction at v1.3 (0642c53); receipt ranking S6 live (d4f0f5d).

| row | target | status |
|-----|--------|--------|
| S1  | lib-young construction | construction-partial (YoungL2.lean 525253b, 2 sorries) |
| S1b | YoungL2 completion | zero-progress honest partial (blockers documented in file) |
| S2  | a96A03 fresh | partial 1 sorry (d93e7e0); part 1 full |
| S4  | a93A03 continuation | zero-progress-instructive (26be1cb); scope-challenge harvested |
| S5  | a93J02 continuation | failed-cap (dirty tree reverted; e5158e7 stands, 1 sorry) |
| S3  | a96J01 fresh | **NOT RUN** (429 pre-work; re-dispatch = not a retry) |
| S6  | a96A04 continuation | **NOT RUN** (final row; first Ψ-weighted surfacing test) |

## To resume (in order)

1. Verify seats: `curl -s localhost:7070/api/alpha/agents | python3 -c "..."`
   (zai-5 idle, codex-2 idle). Verify store: `curl -s "http://127.0.0.1:7073/api/alpha/evidence?limit=1"` → 200.
2. **Dispatch S3** (a96J01) from /home/joe/code/futon3c:
   `clojure -M scripts/dispatch_with_recall.clj --problem a96J01 --to zai-5 --substrate-base http://localhost:7073 < packet`
   — packet text: see "S3 packet" below (also in git history of /tmp waiter,
   reproduced here as source of truth). PARK on the job id (park protocol in
   every prior wake payload; deadline +65 min; checklist form).
3. S3 wake: ground truth (lake, git, faithfulness vs problem.md, revert
   dirty), receipt check (v1.3 query cleanliness + surfacing), row, ops log
   (typed register), scribe pass to codex-2 if there is content.
4. **Dispatch S6** (a96A04 continuation, 3 sorries) — same shape; packet
   below. This is the Ψ test: the missing-dependency pair (e-dfea2de9,
   e-9751e537) carries use-history factor 1.5 → expect receipt-ranked
   surfacing (:v1.1-receipt-ranked tag + factors in receipt). YoungL2.lean
   (repo root) holds the sorried Young statements as support — the runner
   may use them as stated dependencies (sorry-backed) or prove locally.
5. S6 wake: same protocol; then **cohort close**: results section appended
   to cohort-2-prereg.md with META-METERS: extraction yield (drafts/session
   from scribe reports), surfacing precision (used/offered from receipts;
   query cleanliness per :recall-system version), post-hoc misses (ops log
   ⊸miss count), time-to-first-reuse, coverage growth (patterns at open=13),
   register violations (0 through S3), supersession chain (liminf: 3
   generations), outer-loop cycle count (2 full cycles applied same-day).
   Commit, push, report to Joe.

## S3 packet (verbatim)

COHORT-2 SESSION S3 (recall-live, post-quota-reset) — formalize and prove APM a96J01.

Repository: /home/joe/code/apm-lean. Read problems/a96J01/problem.md, problem.tex, and proof-outline.md. Study problems/a92J02/ and problems/a93J05/ as models for layout. Create problems/a96J01/lean/Main.lean with a faithful Lean 4 + Mathlib formalization of the problem statement, then prove it.

If memories are listed above: they come from prior zai sessions on related problems. Use your judgment; name used/ignored in your final summary, and say HOW any used memory changed your approach.

VALIDATION: `lake env lean problems/a96J01/lean/Main.lean` — success = exit 0, 0 sorries, 0 errors.

PACING: start editing the Lean file EARLY (explore in Lean, not prose). If the full proof is not in reach by ~20 minutes, COMMIT an honest compiling partial rather than running out of time with uncommitted work.

On completion or at budget: update problems/a96J01/status.json, commit problems/a96J01, and end with a final summary: Target / Validation command / Result (exit code, sorry count) / Commit SHA / Classification (complete | partial | not-attempted) / Memory usage.

## S6 packet (verbatim)

COHORT-2 SESSION S6 (recall-live, final row) — continue APM a96A04: reduce the remaining 3 sorries.

Repository: /home/joe/code/apm-lean. problems/a96A04/ holds a partial (commit f614856, 3 sorries): heatKernel_integral is proven; remaining are (a) smoothness via differentiation under the integral, (b) L2 contraction via Young's inequality, (c) L2 convergence via approximate identity. ALSO: /home/joe/code/apm-lean/YoungL2.lean (repo root) holds formal statements of exactly the Young inequality needed for (b), with 2 sorries and a documented proof path — you may import/use it as a stated dependency or prove what you need locally. Read both files first. Do NOT weaken any statement.

If memories are listed above: they include memories mined from THIS problem's earlier sessions with recorded use-history. Name used/ignored in your final summary and say HOW any used memory changed your approach.

VALIDATION: `lake env lean problems/a96A04/lean/Main.lean` — success = exit 0, fewer sorries than 3.

PACING: edit early. If completion is not in reach by ~20 minutes, COMMIT an honest compiling state with fewer sorries.

On completion or at budget: update problems/a96A04/status.json, commit, and end with a final summary: Target / Validation command / Result (sorries before -> after) / Commit SHA / Classification / Memory usage.

## Standing context for whoever resumes

- Dispatch discipline: full packets via heredoc (thin-packet gate will
  refuse stubs); park on every dispatch; never kill running jobs;
  ground-truth-first at wakes; typed rows, no retries (429 pre-work ≠ run).
- The ops log is in the typed register (fuite markers) — continue it.
- claude-6 owns dark retrieval growth (M-memory-retrieval.md); receipts are
  their read-only observations; live recall changes land as new
  :recall-system tags at cohort boundaries only.
- Charter: M-zai-learning-loop.md §Second derivative. Reproduction recipe:
  ~/code/algorithms/zai-learning-loop.md.

## Amendment (2026-07-28, M-codex-sorry-loop coordination)

claude-6's codex sorry-loop lane may prove YoungL2's sorries before S6 runs.
At S6 dispatch: CHECK YoungL2.lean state (lake, sorry count, git log for the
proving commit + runner). If proven by the codex lane, S6's row records
:younglemma-provenance {:built-by <runner-model> :commit <sha>} and the row
reads as the cross-lane payoff test (memory -> construction target -> codex
build -> zai consumption) — a BETTER test than the original, recorded
honestly as an amended precondition, not silently. Codex rows never enter
cohort-2 counting. Also at close: backfill 12 outcome halves (claude-6 WS3:
outcome-half completion is the calibration binding constraint) + typed
supersedes/resolves edges for the liminf chain (WS2: second edge type), then
run holes/labs/M-typed-memories/connectivity_meter.bb into the meta-meters.
