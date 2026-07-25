# BPM batch 0 — results (2026-07-25/26)

Prereg: bpm-batch-0-prereg.md (d487047). Runner zai-4, sequential, no recall
injection (baseline). Stubs at apm-lean e40ebd9; all solve commits pushed.

## Tally

| # | problem | outcome | commit | wall-clock | notes |
|---|---------|---------|--------|-----------|-------|
| 1 | 1.1.1 trig inequality | **SOLVED** | ab8ee4a | ~18m | monotone f via derivative sign |
| 2 | 1.2.2 IVP+closed ⟹ cont | **SOLVED** | ebc68dd | ~20m | semicontinuity via closed level sets |
| 3 | 1.3.1 power means → A₁ | **SOLVED** | 5bfa490 | ~26m | squeeze; (k+1)^{1/n}→1 |
| 4 | 1.4.1 f'=0 ⟹ const | **SOLVED** | e2f2f12 | ~7m | Cauchy MVT |
| 5 | 1.5.1 ∫f=0 ⟹ f≡0 | **SOLVED** | 9ab89c4 | ~3m | Mathlib integral_pos |
| 6 | 1.6.2 ptwise lim Lipschitz | **SOLVED** | fd76fed | ~7m | Lipschitz transfer |
| 7 | 1.7.1 sawtooth Fourier | FAILED | — | ~42m cap | died mid-formalization, 8 errors, dirty tree reverted |
| 8 | 1.8.1 concave majorant | FAILED | — | ~3m | died in prose math exploration, zero file edits |
| 9 | 1.1.2 sup ≤ ‖f'‖₂ | FAILED | — | ~31m cap | died mid-formalization (FTC+Cauchy–Schwarz API), reverted |
| 10 | 1.3.2 (nⁿ/n!)^{1/n}→e | NOT-RUN | — | — | zai 5h quota exhausted (HTTP 429; resets 07-26 11:00) |

**Solved with zero sorries: 6/10 (6/9 attempted).** All six solves verified by
ground control: lake exit 0 / 0 sorries / 0 errors re-run independently;
statement integrity checked against e40ebd9 per session.

## Quarantine

zai-4 has **zero** memory-type evidence entries (checked end-of-batch). No
memory_record calls in any session. Held-out discipline held.

## Findings

1. **The binding constraint is time-under-cap, not mathematical reach.** No
   session failed on a wrong proof. Everything solvable inside ~25 minutes
   was solved; all failures are cap/limit deaths on problems needing heavy
   Mathlib API navigation (Fourier integrals; FTC+Cauchy–Schwarz chains) or
   subtle construction search (least concave majorant). This is the best
   possible shape for the learning-loop experiment: accumulated memory (API
   maps, "check Mathlib for X" rules, construction templates) attacks
   exactly this constraint, measurably (turns-to-green, cap-survival).
2. **Dispatch-prompt evolution during the batch** (recorded, not hidden):
   sessions 8–10 added a pacing note (commit honest partial by ~20m; edit
   Lean early). It did not rescue sessions 8–9. Future batches should carry
   the same note from session 1 for comparability.
3. **Quota contention**: concurrent a95-series cron sessions (a95J01–J03,
   a95A08 partials landed in the same window) shared the 5-hour zai quota;
   the batch's tail sessions likely ran under rate pressure (session 8 died
   in ~3 minutes) and session 10 never ran. Future batches: pause other zai
   consumers or schedule against a fresh window.
4. **Session-7/9 leftovers**: cap-death leaves uncommitted non-compiling
   trees; ground control reverted to stubs both times. The runner-side fix
   (commit-early discipline) belongs in the harness, not just the prompt.

## Batch-0 status of bpm-1-3-2

Not run (quota). The dispatch was refused before any zai contact with the
problem — no attempt occurred. Options for the operator: (a) count the row
as not-run and close the batch at 9 attempts; (b) run session 10 after quota
reset as the same batch (defensible: no attempt was started, so it is not a
retry). Ground control recommends (b) with this note preserved.
