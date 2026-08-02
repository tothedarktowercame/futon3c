# Receipt-ranking is inert in the current corpus — a read, not a measurement

**2026-08-02, claude-7.** Settling "does the recall receipt-ranking boost help?"
by reading the code and the logged receipts, per Joe's rule that a *does-it-help*
sub-question which the data already answers should not buy a cohort arm.
Sharpened form due to claude-4 (the ant-excursion analog: a live-looking term
that is causally dead).

## The mechanism

`rank-memories` (`src/futon3c/dispatch_with_recall.clj:553`) multiplies each
candidate's base score `1/(1+0.05*index)` by a receipt factor
`1.0 + alpha*use-rate` (`alpha=0.5`, so factor ∈ [1.0, 1.5]), re-sorts, and the
result flows to `(take limit)` at line 716 (`default-limit = 5`).

**Structurally it is wired to be load-bearing** — the boost re-orders the *full*
candidate list *before* truncation, so a boosted memory can cross the top-k cut
and change which memories surface. This is **not** the ants' bug (there the
variance was computed and then discarded by the observation function; here the
ranked order feeds the cut directly).

## But it is inert in practice

For the boost to change the surfaced set, one dispatch must satisfy BOTH:
- **(T) truncation binds**: `|candidates| > 5` (else `take limit` keeps all,
  and order among a wholesale-consumed set is irrelevant to what the runner sees);
- **(V) use-rates vary**: some candidate has `use-rate > 0`, else every factor is
  `1.0` and the ranked order equals the pre-receipt order.
Plus gate **(S)**: `stats-found?` — at least one candidate carries a prior receipt.

Against the frozen receipts export (`0cc527e2…`, 64 logged dispatches),
reproduced by `scripts/receipt_ranking_inertness_check.py`:

- **(S) fails in 51/64** — no candidate had a prior receipt; the boost was never
  even computed.
- Of the **13** that computed it: blocks 1–10 have 8–9 candidates (**T holds**)
  but **use-rate = 0 for every memory** (**V fails**) → uniform factor 1.0 → the
  ranked top-5 equals the pre-receipt top-5.
- Blocks 11–13 have **use-rates up to 1.0** (**V holds**) but only **3 candidates**
  (**T fails**) → truncation never binds.
- **(T) and (V) co-occur in 0 dispatches.**

## Verdict and consequence

Receipt-ranking is **causally inert across the entire logged corpus** — correctly
built, starved of signal. The root cause is the same sparse use-attribution V2
documented: memories are *offered* but almost never marked *used*, so use-rates
sit at 0 exactly where the candidate sets are large enough to truncate.

**A receipt-ranking arm in the V3 cohort would measure nothing** on the current
corpus. It should not be registered until a **corpus-maturity precondition** is
met: dispatches with `used-count > 0` on candidate sets larger than the limit, so
(T) and (V) can co-occur. That precondition is checkable by **re-running this
read** — it is not an experiment. This is the read-vs-measure rule paying out:
one code trace plus one pass over the logs de-scoped a cohort arm that would have
spent tokens confirming a null the data already carries.
