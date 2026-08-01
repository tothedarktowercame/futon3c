# S4 scribe pass 36

Written by claude-9 (ground control), 2026-07-31, covering the runs owed since
pass 35: a95J05, a95J06, a95A02 attempt 4, a96A01, a95J04, a95J08 attempt 3,
a96A02, a96A04.

- Mode: mixed — one solve-lane close, two pre-dispatch blocks, one frontier
  confirmation, one statement defect, one metric caveat.
- Drafts only. No store-write endpoint called for memory promotion; the
  amendments below are **hand-applicable** and marked as such.

Receipts covered:

| problem | receipt | disposition |
|---|---|---|
| a95J05 | (pass-35 receipt) | closed axiom-clean |
| a95J08 attempt 3 | `f2333f84-a915-40d3-b099-332a44e2ab6f` | `:blocked-mathlib-frontier` |
| a95A02 attempt 4 | (pass-35 receipt) | partial, long assembly |
| a96A01 | (pass-35 receipt) | partial, 3 declarations clean |
| a95J06 | (pass-35 receipt) | `:blocked-vacuous-targets` (pre-dispatch) |
| a95J04 | (pass-35 receipt) | `:blocked-statement-false-placeholder-hypothesis` |
| a96A02 | `087f0667-a5c0-4878-81c8-4e50840879e8` | `:blocked-placeholder-subject` (pre-dispatch) |
| a96A04 | `e9d008be-b34f-40a4-a00e-e6a16d245122` | `:blocked-statement-defect-api-drift` |

---

## Finding 1 — a fifth defect mechanism: silent API-drift strengthening

The taxonomy fixed in pass 33 was: one umbrella (*missing semantic-domain guards
admit degenerate witnesses*, with the `integral_undef` / totalized-`deriv` /
extended-codomain-`⊤` mechanisms), plus **constant placeholder** and
**pre-formal elaboration failure** outside it.

a96A04 does not fit any of them, and it should be added as its own class.

`heatConv_contDiff` asks for `ContDiff ℝ ⊤ (heatConv t f)`. In the current
smoothness-index API the index type is `WithTop ℕ∞`, and `⊤` is notation for
`ω` — **real analytic**, strictly stronger than `∞` (smooth). Verified against
Mathlib source directly, not taken from the runner:

- `Mathlib/Analysis/Calculus/ContDiff/FTaylorSeries.lean:112` —
  `scoped[ContDiff] notation3 "ω" => (⊤ : WithTop ℕ∞)`
- `Mathlib/Analysis/Calculus/ContDiff/Defs.lean:90` — "we denote `(⊤ : ℕ∞) :
  WithTop ℕ∞` with `∞`, and `⊤ : WithTop ℕ∞` with `ω`"
- `Defs.lean:1165` — `contDiff_omega_iff_analyticOnNhd : ContDiff 𝕜 ω f ↔
  AnalyticOnNhd 𝕜 f univ`

The problem asks for **smooth**, in four independent places: `problem.md` ("u is
smooth in the variable x"), `informal-solution.md` ("(a) u is smooth in x for
t > 0", route: "smoothness via DCT"), `status.json` ("part a: smoothness via
DCT"), and the theorem's *own docstring* on the line above it.

**What makes this its own class is the causation.** No one edited the file. The
token `⊤` meant *smooth* when the index type was `ℕ∞`; after Mathlib moved to
`WithTop ℕ∞` the identical token means *analytic*. The statement got strictly
harder with no diff to review. Every gate this loop runs is blind to it:

- sorry count — unchanged;
- `#print axioms` — clean;
- statement-integrity diffing — **nothing changed**, which is precisely the problem;
- the vacuity scan (0a/0b) — the conclusion is neither `True` nor about a placeholder.

Proposed name: `:silent-api-drift-strengthening`. Distinguishing test: *the
statement is unchanged and still elaborates, but a symbol in it has been
redefined upstream.*

**Corpus sweep:** `ContDiff … ⊤` occurs in exactly two of 145 problem files —
a01A03 and a96A04. The drift is contained; there is no third instance. I ran
the sweep specifically to bound the class rather than leaving it open-ended.

a01A03 is the precedent and it is worse there: it was repaired to `⊤` *intending*
smooth, and since every compactly supported real-analytic function on ℝ is
identically zero, the hypothesis then quantified over `f = 0` alone and the
statement became **false**. It was corrected to `ContDiff ℝ ∞`. So the same drift
produced a *vacuity* in one file and a *strengthening* in the other.

The a96A04 repair (`⊤` → `∞`) is `:decision-owner "joe"`. Precedent exists and is
same-class; I have **not** treated it as standing authorisation.

---

## Finding 2 — a memory can be used correctly and still cause the wrong decision

This is the pass's most important result and it cuts against our own headline
metric, which is why it leads the amendments.

`e-codexpilot-distinguish-ContDiff-top-analytic-from-ContDiff-infinity-smooth`
was promoted from the a01A03 diagnosis earlier the same day. It **surfaced** for
a96A04 and the runner **used** it — correctly — to establish that `⊤` means
analytic. The runner then drew the *opposite operational conclusion* from
a01A03's:

- a01A03: "⊤ means analytic ⟹ **the statement is mis-stated; repair it to `∞`**."
- a96A04: "⊤ means analytic ⟹ **this is a hard analytic construction**", and
  declared an analytic-parameter-integral frontier.

The memory carries the **fact** but not its **consequent**. It says what `⊤`
means. It does not say that when the informal problem asks for smoothness, `⊤` is
a *mis-statement to repair* rather than a *frontier to assault*. Nor does it
carry the cheap discriminator that settles the question: **grep `problem.md` for
`smooth`.** One grep decides it.

**Metric consequence, recorded honestly.** metric-3 counts this as a memory used,
and by any mechanical definition it was. But the run it informed reached the
wrong conclusion and spent a runner slot on a frontier the problem never asked
anyone to cross. **"Memories used" is not "memories used well," and our metric-3
number does not currently distinguish them.** Every prior pass has reported
metric-3 as an unqualified good; this is the first counter-instance, and it
should be visible in the whitepaper rather than smoothed over. It does not
retract the memory-loop result — the loop has compounded genuinely at least four
times — but it bounds the claim: *surfacing and consumption are measured;
correctness of the resulting decision is not.*

Suggested follow-on (not done here): for each memory recording a *distinction*,
check whether it also records the *action* the distinction licenses. A
distinction without a consequent is a fact the runner must re-reason from, and
a96A04 shows it can re-reason wrongly.

---

## Finding 3 — the pre-dispatch vacuity preflight is now load-bearing

Two rows were blocked *before* a runner slot was spent, one row apart:

- **a95J06** — conclusions `∀ ε > 0, ∃ δ > 0, True` and literally `True`. Either
  closes with `trivial`.
- **a96A02** — conclusions are substantive-looking quantified inequalities, so
  the `True`-conclusion check (0a) passes. But their **subject** is
  `spikeFunction := 0`. Under the constant zero, `spikeFunction_holderWith`
  (`|f x − f y| ≤ A|x−y|^(1/2)`) is **trivially true** and
  `spikeFunction_not_absolutelyContinuous` (`ε ≤ ∑|f(bᵢ) − f(aᵢ)|`) is **false** —
  the constant function is absolutely continuous.

**The generalisation a95J03 could not have taught:** a placeholder subject makes
positive statements *trivial* **and** negative statements *false*. A row can be
simultaneously vacuous and un-dischargeable, and a runner attacking the false
target would burn a full slot reporting a "statement defect" whose actual cause
is a placeholder three declarations above it.

This forced the preflight's step 0 to split:

    0a. is the CONCLUSION itself `True`?                        [a95J06]
    0b. does the conclusion's SUBJECT unfold to a bare constant? [a95J03, a96A02]

**Corpus closure.** `scripts/vacuity_scan.py` (written this pass, documented,
sanity-checked against the known positives) reports: of 145 problem files,
**exactly one CLEAN problem carries a vacuity signal — a95J03, already known and
flagged `:counts-toward-clean :disputed`.** No second vacuous clean exists. Among
sorried problems the flagged set is a01J06, a95J04, a95J06, a96A02 — all already
blocked. A narrow independent detector (prose admitting a generated sorried
definition was replaced) returns the same three files and separates them
usefully: a95J03 and a96A02 substituted `:= 0`, whereas **a95J06 substituted
Mathlib's real `cantorSet` — a good repair** whose vacuity is a different defect.

The scan's own history is worth keeping: its **first version returned a false
all-clear across all 145 files**, twice over. A `def (\w+)[^:=]*:=` regex cannot
match a definition with typed binders (they contain `:`), and classifying files
with `(?<![\w.])sorry(?![\w])` matches the word in *prose* — a95J03's docstring
says "had a sorry in its definition", so the one known vacuous clean problem was
filed under "sorried" and the headline "0 clean problems flagged" was itself
vacuously true. **Both traps were caught only by asserting against known
positives before believing the result.** That assertion is now in the script.

---

## Finding 4 — the YoungL2 lakefile fix paid within hours

a96A04's runner reports that the repository's `YoungL2` module closed
`heatConv_L2_le` outright. `YoungL2` held an axiom-clean
`convolution_L2_contraction_of_probability_kernel` that was **unreachable** until
commit `7958c53` added its missing `[[lean_lib]]` stanza tonight — the same
defect `ConstructionTargets` had. This is the first consumption of that module by
another problem, and it discharged a declaration rather than merely informing
one. Worth recording because the fix was infrastructure, not mathematics, and its
payoff is directly attributable.

---

## Hand-applicable amendments

### A. Attach the consequent to the ContDiff distinction

Target:
`e-codexpilot-distinguish-ContDiff-top-analytic-from-ContDiff-infinity-smooth`

Append:

- **consequent (this is what was missing):** when the Lean asks for
  `ContDiff 𝕜 ⊤` and the informal problem asks for *smooth*, the statement is
  **mis-stated**, not merely hard. The repair is `⊤` → `∞`. Treat an analytic
  frontier as the conclusion only when the informal problem actually asks for
  analyticity;
- **discriminator, cheap and decisive:** grep the problem's `problem.md` /
  `informal-solution.md` for `smooth` vs `analytic` before concluding anything
  about difficulty;
- **why the drift happens:** the index type moved from `ℕ∞` to `WithTop ℕ∞`; the
  unchanged token `⊤` changed meaning from smooth to analytic. The file has no
  diff, so review-based gates cannot see it;
- **two outcomes of the same drift:** a01A03 — became **false** (compactly
  supported real-analytic ⇒ zero, so the hypothesis quantified over `f = 0`
  alone), repaired to `∞`; a96A04 — became **strictly stronger** than the
  problem, repair pending Joe;
- evidence: a01A03 (in-file correction note, machine-checked refutation
  committed); a96A04 receipt `e9d008be-b34f-40a4-a00e-e6a16d245122`;
- **use-quality note:** this memory was surfaced and used in a96A04 and still
  produced the wrong decision, because it carried the fact without the
  consequent. Retain this note — it is the evidence for the amendment.

Suggested confidence:
`:two-problem-machine-checked-distinction-with-repair-consequent`.

### B. Extend the placeholder memory with the falsity half

Target:
`e-codexpilot-inspect-placeholder-definitions-before-claiming-mathematical-content`

Append:

- a placeholder subject does not only make positive statements trivial, it makes
  **negative statements false**. a96A02 exhibits both halves from one `:= 0`:
  `spikeFunction_holderWith` is trivially true, `spikeFunction_not_absolutelyContinuous`
  is false;
- therefore a "statement defect" reported by a runner may have its actual cause
  in a placeholder definition elsewhere in the file — check the subjects before
  accepting the diagnosis;
- **prose is not evidence of repair:** a96A02's header claims the function is
  left "opaque … with a precise specification of its values", but the definition
  is `:= 0` and no specification appears anywhere in the file. Same shape as
  a95J06, whose docstring says "blocked" while the Lean says "provable". Scaffold
  prose describes the *intended* repair; only the Lean is checked;
- evidence: a96A02 receipt `087f0667-a5c0-4878-81c8-4e50840879e8`;
- retain the existing a95J03 challenge attachment.

### C. New memory — silent API-drift strengthening

Only if the scribe judges it distinct from A rather than a generalisation of it;
I lean **distinct**, because A is about one API and this is about the failure
*mode*:

- a statement can go stale, and get strictly harder or become false, **with no
  edit to the file**, when an upstream definition changes meaning;
- sorry counts, axiom censuses and signature diffs are all blind to it — the
  diff is empty by construction;
- detection: when a target looks disproportionately hard for its stated informal
  content, check whether a symbol in it has been redefined upstream, before
  declaring a frontier;
- instances: a01A03, a96A04. Corpus sweep bounds the `ContDiff` case at exactly
  these two files.

---

## Prior-art check (per pass-33 correction)

Before proposing A–C I checked for existing coverage, since three earlier
proposals of mine turned out to duplicate existing memories:

- the ContDiff distinction **already exists** — hence A is an *amendment*, not a
  new memory. This is the same correction the scribe made about YoungL2 in
  pass 34;
- the placeholder memory **already exists** — hence B is an amendment;
- I found no existing memory for the *drift mechanism itself* as opposed to the
  specific API, which is why C is proposed as new. If the scribe finds one,
  fold C into it.
