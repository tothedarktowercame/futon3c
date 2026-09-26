# CODEX HANDOFF — S9: statement-vs-informal-solution reconciliation

**From:** claude-9 (ground control, M-codex-sorry-loop)
**Authorised by:** Joe, 2026-07-30
**Kind:** audit / census. **You will not write a single line of Lean.**

## Why this exists

The cron lane has attempted 8 rows of the `:hard-proof-step` tail. **Two
were solved; six turned out to have defective statements.** Every one of
those six was discovered the expensive way — by a runner spending a slot
trying to prove something unprovable, then ground control spending a
verification cycle confirming it.

Tracing all six back through `problem.md` (the original exam TeX) and
`informal-solution.md` showed that **four of the six were pure
translation defects: the mathematics was already correct and already
written down, and the formaliser broke it in the Lean.** Those four are
catchable by *reading two documents side by side* — which is this job.

**This is deliberately primed:** you are not being asked whether defects
exist. They do, and six are already confirmed. You are being asked to
find the rest before runners waste slots on them.

## The confirmed six (your priming set — read these first)

Each is fully written up in
`holes/labs/M-codex-sorry-loop/faithfulness-ledger.md` (now tracked in
git), entries #3–#8 plus the section **§WHERE THE DEFECTS COME FROM**.
Read that section before starting; it is the model for your output.

**Class A — translation defects (formaliser broke correct mathematics). 4:**
| Row | Mechanism |
|---|---|
| a00J05 | `variable (K) (hK_compact) (hK_measure)` declared in the section but **never included** in the declarations — only `K` is mentioned, so both hypotheses silently vanish. Statements became false. |
| a01A02 | Informal solution states the hypothesis as the **measure** bound `m({\|f\|>λ}) ≤ Cλ⁻²` and requires `0 < m(E) < ∞`. Formaliser wrote `(volume …).toReal ≤ C/t^2`, and **`ENNReal.toReal ⊤ = 0`** makes the hypothesis vacuously satisfiable → theorem false. |
| a01A03 | Informal solution says `g = χ_{[-h,h]}` **a.e.**, twice, explicitly (lines 11 and 21). Formaliser wrote `∀ x, g x = …`. Pointwise conclusion over a.e.-only constraints. |
| a01A04 | `Fin n → ℝ` used where a Euclidean space was meant. It carries Mathlib's **Pi sup-norm**, so `Metric.ball 0 1` is the cube `(-1,1)ⁿ` (volume `2ⁿ`), not the Euclidean ball. NB `problems/a95A03` already documents this exact repair — swap to `EuclideanSpace ℝ (Fin n)`. |

**Class B — the informal solution itself is wrong. 1:**
`a01A06`. Its informal solution derives `O(1/λ)`, then `O(1/λ²)`, states
outright *"O(1/λ²) … is **not** enough for L²!"*, then overrides itself
with an invalid step (it lower-bounds a quantity and uses that as an
upper bound on a log). It concludes exponential decay and "Yes, g ∈ L²".
Both are wrong; correct answers are an entropy/`L log L` bound and
**no**, with `g = x^{-3/4}` on `(0,1]` as witness.

**Class C — the original exam asks for something false. 1:**
`a01A05`. `problem.md` itself says *"Show that |fₘ−eₘ| ≤ 2⁻ᵐ"*, which is
narrowly false (margin ≈ 0.018). No `informal-solution.md` exists.

**Consequence you must internalise: the informal solution is NOT the
denominator.** The original exam TeX in `problem.md` is. The informal
solution is strong evidence of intent and usually correct (4 of the 5
that exist were), but a01A06 proves it can be confidently wrong.

## Scope

Every row in `futon3c/data/codex-sorry-queue.edn` whose `:status` is
`:untouched` — 66 rows at time of writing. Derive the list from the
queue; do not hardcode it. Each row's `:file` points into
`/home/joe/code/apm-lean/problems/<id>/lean/Main.lean`, and the bundle
beside it has `problem.md`, `problem.tex`, `informal-solution.md`
(usually) and `proof-outline.md`.

Skip the 6 rows marked `:blocked-owner-decision` — already done.

## What to do per row

1. Read the Lean target statement(s) at the `:line` numbers.
2. Read `problem.md` for the original exam text, and
   `informal-solution.md` for the intended theorem.
3. Ask, in this order:
   - Does the Lean statement say **the same thing** as the informal
     solution's stated theorem? Differences in quantifier strength
     (`∀ x` vs a.e.), in ambient type, in `ENNReal`-vs-`toReal`
     encoding, and in which hypotheses actually reach the declaration
     are exactly where the four known defects live.
   - Are all section `variable` hypotheses actually **included** in the
     declaration? (Lean 4 does not auto-include unmentioned Prop
     binders. This is mechanical — check every row.)
   - Is any measure `.toReal` used in a **hypothesis** without an
     accompanying `≠ ⊤` / `< ⊤`?
   - Is the ambient type right — `Fin n → ℝ` where Euclidean geometry
     (`Metric.ball`, `dist`, `‖x‖` on a domain point) is intended?
   - Is the conclusion **the same strength** as what the hypothesis
     actually controls? (This is the a01A06/a01A05 family and is NOT
     mechanical — flag suspicion, do not over-claim.)
4. Classify: `:ok` | `:suspect-translation` | `:suspect-informal` |
   `:suspect-exam` | `:cannot-assess`.
5. Cite `file:line` for BOTH sides of every claim you make.

## Hard constraints

- **Do not edit any file under `/home/joe/code/apm-lean/`.** Statement
  repairs need Joe's authorisation; you are producing a triage report,
  nothing else.
- **Do not attempt any proof.** If you can cheaply confirm a defect with
  a counterexample *sketch*, note it — but no Lean writing.
- **No store writes.** No `curl` to `:7073`. Ground control holds the pen.
- `:ok` is a fine and expected answer. **Do not manufacture findings** —
  a row you cannot assess is `:cannot-assess` with a reason, and that is
  more useful than a guess.

## Output — INCREMENTAL, this is mandatory

The Agency job cap is ~30 minutes and **will** kill you mid-census.
Therefore:

- Append **one JSON object per row, immediately after assessing it**, to
  `futon3c/holes/labs/M-codex-sorry-loop/s9-reconciliation.jsonl`.
  Never buffer results in memory. If you die at row 20, rows 1–19 must
  already be on disk and valid.
- Each line: `{"row-id":…, "problem":…, "verdict":…, "mechanism":…,
  "formal-cite":"path:line", "informal-cite":"path:line",
  "evidence":"…", "confidence":"high|medium|low"}`
- At the end (or when you sense you are near the cap), write/refresh
  `futon3c/holes/labs/M-codex-sorry-loop/s9-reconciliation-note.md`:
  rows covered, counts per verdict, the rows you consider highest-risk,
  and **explicitly what you did NOT cover** so I can re-dispatch the
  remainder. Silent partial coverage is the one unacceptable outcome.

## Gates

- The JSONL must parse — validate it before finishing.
- If you write any Clojure/Lisp: `emacs -Q --batch -l
  futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" --
  --no-defaults <files>` must print OK.
- `git diff --check` clean. Commit your report files (they are tracked
  now); do not commit anything under `apm-lean`.

## When done

**Bell `claude-9` back** with: rows covered / not covered, counts per
verdict, your top suspects, and commit shas. Use
`--from codex-4`.
