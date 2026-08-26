# E-R8-red-ring-fill — filling the placeholder at step ⑨

**Opened:** 2026-08-26 · claude-13 at Joe's direction.
**Scope:** three slices. `G(π)` is explicitly **out** — see "Deliberately out of
scope" below.

## The premise, restated as Joe put it

> *a loop is NOT born instrumented for its gain — but does have a placeholder
> where it can gain that instrumentation.*

This is a correction to how R8 has been read. WR-27 says a loop is *born*
instrumented; the WM loop was born with a **placeholder**, `sec-system` step ⑨:

    ⑨ Fold any new realized fold outcome into the engineering selection gain g

It is a good placeholder — typed, correctly positioned between ④ (load the
previous trace) and ㉞ (persist it), and **conditional**, so with no outcome it
is a silent no-op rather than a crash. **R8 is red because the placeholder has
never been filled, not because the instrument is missing.**

## What is already true (verified 2026-08-26; do not re-derive)

**The producer exists and is armed.** `futon2/src/futon2/aif/fold_realized.clj`:
both `G` legs are the same coverage→rollout ΔG, expected over the *predicted*
wiring against realized over the *enacted* wiring, so γ's relative error is
apples-to-apples. Independently verified and closed 2026-07-06 by zai-5
(`E-first-flights-policy-grade-G-closure.md`): 18/18 tests, scale match
confirmed at the REPL (expected ΔG −0.6 with matching enacted coverage →
realized −0.6; zero coverage → 0.0, not nil). `*live-wire?*` defaults **true as
of 2026-07-08 (Joe-directed)**, alongside `*selection-gain-grounded-feed?*`.

**And it has never fired.** Its own docstring says why: *"LATENT until R10 (the
live loop) runs — arming this makes enactment live-CAPABLE, not live-RUNNING."*
Across the 62 archived attempts in `wm-outer-loop-40..46-v1`, all dated
2026-07-24 and **sixteen days after arming**:

    :realized-outcome 0 · :realized-score 0 · :expected-score 0
    fold-realized-outcome 0 · gamma 0

**So `g` has never moved.** `:selection-gain` is `1.0` in all 65 occurrences.
That constant *is* `g`, and step ⑯ selects with `τ_eff = 1/g`, so the selection
temperature has been pinned at 1.0 for the entire archive. `:predicted-g-s` is
likewise a fixture: value 0.4, occurring once across 58 attempts.

**Timeline.** Instrument closed 07-06 → armed 07-08 → daily cadence stopped
07-14 → 62 attempts on 07-24 emitting nothing → last run 07-27 → R10 currently
off for tuning.

## Slice 1 — DISCOVERY: does the wiring reach the producer?

**One run, no code changes.** Bring R10 up with the existing arms on for a run of
consecutive ticks and report whether any `:realized-outcome` appears.

**This is discovery, not implementation, and it can falsify the rest.** Two
outcomes, both informative:

- **Readings appear** → R8's `:built` gate is met on the spot, and slice 2 is a
  replay exercise against fresh data.
- **Nothing flows** → the defect is a **wiring gap** between the
  `wm-outer-loop-*` path and `fold-realized`'s enactment path. That is a seam to
  locate, findable in one run, and a different task from anything below.

**Acceptance:** a report naming, for one tick, whether `with-realized-outcome`
was reached; if not, the last function on the path that was. Cite file:line.

**Do not:** modify `fold_realized.clj`, change either arm's default, or touch
the serving JVM's loaded namespaces.

## Slice 2 — the null control, which slice 1 makes testable

Only after slice 1 yields readings. R8's promotion test states it:

> the per-tick mismatch is recorded at every tick for a run of consecutive
> ticks, routed into the outer-loop update, and the subsequent reward action
> **differs** from the action the same tick sequence produces with the mismatch
> held constant.

> **null control:** replay the same ticks with the mismatch frozen; if the
> action sequence is identical, the mismatch is being recorded and not read —
> *the current state wearing an instrument.*

**Why slice 1 is a strict prerequisite**, in the promotion test's own words:
*"Runnable only AFTER the mismatch is emitted… the archive has no mismatch to
freeze."*

**And what makes it bite:** with `g` moving, `τ_eff = 1/g` changes, so step ⑯'s
selection differs between the two replays *by construction* — or does not, which
is the finding. Today that comparison is vacuous because `g` is a constant.

**Acceptance:** two action sequences from the same ticks, one live and one
frozen, plus their diff. An identical diff is a **pass for the experiment and a
fail for the ring** — report it as such rather than retrying.

## Slice 3 — two recording holes on the operator side (independent)

From the 34-step comparison, the operator loop's two cheapest failures. Both are
**recording changes, not measurement design**, and neither depends on slices 1–2.

**⑧ per-channel precision.** The operator loop keeps none. A concrete instance
from 2026-08-26: codex-12's completion report stated *"I left `Step.switch`
unchanged"* while its own commit `81325aaf4b` had corrected it. The report was
wrong about its own work, in the direction of understating it. Had the review
trusted the report, a fix already applied would have been re-dispatched. **That
is a precision signal on the agent-self-report channel, and nothing recorded
it.** Minimum viable: per dispatch, record whether the completion summary
matched the commits, as a two-valued observation.

**㉛ Morning Brief QA queue.** Outcomes of operator-loop work are not queued for
adjudication. Minimum viable: on review completion, emit one Morning-Brief item
carrying the job id, the shas, and the review verdict.

**Acceptance:** for ⑧, one recorded observation per dispatch over ≥3 dispatches,
including at least one mismatch if one occurs. For ㉛, one queued item per
completed review, readable by the existing Morning Brief surface.

## Deliberately out of scope

**`G(π)`.** Steps ⑬⑭⑮ compute a `G`, and policy-grade `G(π)` has no producer:
`fold-eval` #1 is *"deliberately not passed through the rollout/EFE vocabulary:
no predicted outcome distribution is present"*, evaluation (b) is unbuilt, and
codex-8's audit found the archive holds no `P(s′ | π, s)` and no state-indexed
`G(s′)` (`futon2/holes/NOTE-d3-computability.md`). Filling ⑨ moves the
*engineering* gain `g`; it does not produce `G(π)`. Conflating the two is the
error this excursion exists to avoid.

## Stopping conditions

- Slice 1 returns no readings **and** names no wiring seam → stop and report;
  the premise that the producer is reachable is wrong.
- Slice 2's diff is identical → the ring stays red **with an instrument
  attached**, which is a better-evidenced red than today's and should be
  recorded as the result, not retried.

## Related

- `p4ng/sec-system.tex` ⑨ ⑯ ㉞ — the placeholder and what reads it.
- `p4ng/empirics-futon/NOTE-thirtyfour-steps-both-levels.md` — the asymmetry that puts ⑧ and ㉛ in slice 3.
- `p4ng/empirics-futon/promotion-tests.edn` — R8's gates, null control, retro-trip.
- `futon2/src/futon2/aif/fold_realized.clj` — the producer.
- `futon3c/holes/excursions/E-first-flights-policy-grade-G-closure.md` — its closure.
