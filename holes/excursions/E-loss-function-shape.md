# E-loss-function-shape — the general shape, with a slot for scoped df

**Opened 2026-08-19 by claude-2 (ground control) at Joe's direction**, after a
day in which the same measurement error appeared in six different places. Joe:
*"maybe we can work to a general shape and slot that in when it is ready."*

Written to disk deliberately. Three artefacts today existed only in an agent's
context or an uncommitted working tree — a corpus survey, a correctness fix, and
a set of stop-reports living in bell threads. Two of those were mine.

---

## The governing rule

> **A number and its basis are one object.** A `df` without its population, a
> guidance count without its subtrahend definition, a `used-ids` without whether
> the channel was invoked — each is not a weak measurement, it is not a
> measurement. If the basis changed between frames, the comparison is void and
> the display must SAY SO rather than plot.

Every failure compared across 2026-08-18/19 — `:recall-empty` on an anchor absent
from the store, an honest forward scan over a stale checkpoint, an absolute
`df` band calibrated on a corpus 700× smaller, a silently dropped filter, a
survey counting paper-proof mentions as encoding dependencies — is *a
computation that succeeded against a population nobody had stated* (claude-11's
formulation). The basis is not documentation. It is the missing operand.

## Four bands, kept separate

Deliberately not one scalar. Collapsing them lets a harness repair read as a
mathematical gain, which is precisely what would have happened at f12.

### 1. COST — what was spent
```clojure
{:cost {:commits 5 :dispatches 2 :wall-clock-min 47
        :provenance :measured}}          ; :measured | :estimated — NEVER averaged
```
`t01A07` closed in 5 commits (measured). Singular homology is "1–3 weeks"
(estimated). Averaging those silently is how a plan becomes fiction. The
reusability premium — general vs sphere-specific two-open simply-connected —
is **~1 commit, measured against the alternative**, which is the only
coefficient we have that was measured against a counterfactual rather than
estimated against nothing.

### 2. CAPABILITY — did the problem close
```clojure
{:capability {:disposition :closed          ; :closed :tier-a :tier-b :defective
              :axiom-clean? true
              :statement-unchanged? true
              :residual-sorries 0}}
```
The objective. A frame that repairs its own plumbing and does not solve its
problem has failed, however green the other bands read.

### 3. TRANSFER — did accumulated knowledge shape the work
```clojure
{:transfer {:surfaced 4 :used 1 :channel :pull   ; :push | :pull | :none
            :cost-delta {:milestone :first-per-clause-split
                         :solver 3 :student 1 :provenance :measured}
            :basis {:retrieval :scoped-df-pending   ; <-- THE SLOT
                    :anchor {:term "computes" :satisfied? false
                             :population :whole-index-unfiltered :indexed 149766}}}}
```
**The slot.** Retrieval quality is currently unmeasurable: `?df=` reports
whole-index frequencies and silently ignores every filter, so an anchor is
chosen against 149,766 documents when the discriminating population is the ~770
memories. Until claude-11's scoped `df` lands, `:retrieval` reads
`:scoped-df-pending` and any cross-frame transfer comparison is **void by
declaration**, not merely noisy.

Two channel facts the band must carry, both measured: transfer occurred twice
through the **pull** channel while the metric watched push, and once through a
memory that had been **rejected** at review. A transfer metric keyed on
"offered then used" reads zero in all three cases.

**Cost-delta needs a fixed milestone.** f10 and f11 both show the student
spending less and reaching less — 7 commits/0 residual vs 4/5, and 5/2 vs 2/3.
No ratio between "less time" and "less result" is interpretable. Compare at
*cost to first per-clause split* or *to first closed conjunct*, not at the end,
because the arms have different budgets.

### 4. ACCUMULATION — did the store gain reusable knowledge
```clojure
{:accumulation {:deposits 6 :reviewed-attachments 0
                :reviewer-independent? false   ; guide-reviewed != independent
                :basis {:store-total 211 :from-frame-seats 1}}}
```
Of 211 reviewed attachments in the store, **exactly one** came from an APM frame
seat. The rest were deposited by the operator and ad-hoc sessions. So this band
has never yet measured a working machine, and a zero here is currently
uninformative rather than negative.

`:reviewer-independent?` is load-bearing: attach-then-review with the guide as
reviewer WORKS and is not independent; the independent path is blocked at the
transport. A frame that gains one has not demonstrated separation of powers.

## Three values, never conflated

```clojure
:confirmed | :refuted | :inapplicable | :vacuous | :unmeasured
```

- **`:inapplicable`** — the precondition never occurred. Half of f11's
  predictions: there were no offers to disposition, so an empty disposition list
  is not a failure.
- **`:vacuous`** — it passed, but could not have failed. f11's `F3` passed
  because zero offers means no undispositioned offer exists.
- **`:unmeasured`** — the instrument did not run, or ran against the wrong
  population. Distinct from a measured zero, and today's most common state.

Rendering all of these as `0` is the thing that will mislead us in six frames'
time when nobody remembers which was which.

## Rate of return, and its denominator

Under **completion** the breakdown governs ORDER, not selection — we need all of
it. The quantity that matters is the cost of the covering set, where shared
infrastructure pays back superlinearly.

```clojure
{:rate {:area "singular-homology"
        :blocked-consumers 34        ; NOT references — see below
        :cost-estimate-commits 20
        :basis {:method :source-closure-448-of-448
                :vintage "2026-08-19"
                :coverage 0.62        ; obstruction records exist for 62% of open
                :supersedes ["marker-survey" "mentions-only"]}}}
```

**The denominator is blocked-consumers, not references.** Corrected three times
in one day: 52 (paper-proof mentions) → 16 (statement mentions) → 34 (transitive
closure), and then the metric itself corrected — `measure-integral` shows 100
consumers and 41 open, but a spot-check found **22 of 26** recorded obstructions
name something other than measure theory. References are not gaps. Mathlib is
adequate in an area where problems close at a normal rate, however many mention
it.

**Compounding is currently zero.** The finance framing assumes reinvestment: a
closed problem deposits knowledge that makes the next one cheaper. That term is
the accumulation band, and it has produced one attachment ever. Until it works,
an ROI model must carry a compounding rate of **0** and say so, or the first
plausible projection will assume a yield that is not happening.

## What to build first

The **receipt**, before the metric. A basis reading
`population :whole-index-unfiltered, indexed 149766` beside a band of `[3 150]`
makes the mismatch self-evident on sight; the band is the fix, the receipt is
what makes the fix checkable. Landed for the recall anchor
(futon3c `10937528`); the same treatment is owed to the guidance count, the
transfer channel, and the accumulation denominator.
