# T-exogenous-evidence-update-rule — an emission-driven estimator cannot carry an exogenous class

Raised by joe + claude-13, 2026-08-24, out of the futon-2026 paper work.
Sibling of `T-forward-model-vs-active-work.md` (same series, same root cause:
the loop has no representation of the participant outside it).

**Status: DESIGN CONSTRAINT. Blocked on a prior question — see Blocker.**

## The run record this falls out of

Last live run, 2026-07-13:

```
:address-sorry   108 emissions, 1 followthrough → Beta(2, 108), intrinsic-value 0.009
                 interp: "operator rarely follows; class is overactive"
:open-mission    no emissions in window         → Beta(1, 1),   intrinsic-value 0.500
:fire-pattern    168 emissions observed, 0 applied (no substrate) → 0.500
```

Two of three classes are on the uniform prior. The credit signal is one
confounded number and two non-numbers. (`0.009` is the *mode*, 1/108 = 0.00926;
the mean would be 2/110 = 0.018.)

## The constraint (joe)

The estimator is **emission-driven**: β increments every time a recommendation
goes unanswered. Emissions run at ~108 per window; external events arrive maybe
monthly. So an exogenous class would be **crushed to zero by construction**, and
we would have measured our own emission rate a second time — the identical
failure the `0.009` already represents, imported into the new register.

External evidence therefore needs to be **event-driven**: α increments on the
rare outside event, β does *not* tick per unanswered emission. That is a
different update rule, not just a different data source.

## The refinement (claude-13)

If β never ticks, the class becomes **unfalsifiable**: one external event and it
sits high indefinitely with nothing able to bring it down. That trades *crushed
to zero by construction* for *inflated by construction* — the same error with
the sign flipped.

The principled form is that **silence updates neither α nor β**. Non-response is
missing data, not evidence. β should tick only on an **observed decline**: the
counterparty verifiably had the opportunity and did not take it.

This is the paper's ⟨1.2.2⟩ written as an update rule — a model with no
representation of the counterparty's alternative actions cannot distinguish
*considered and declined* from *was elsewhere*, so it must not score the second
as the first.

## Why this makes the operator work load-bearing rather than decorative

Separating *missing* from *negative* requires knowing the counterparty was
present. That observable already exists: `GET /api/alpha/evidence/sessions`
returns one row per session with `:count`, `:authors`, `:first-at`,
`:latest-at`; at time of writing, 348 sessions the operator has ever attended,
nine active within a day.

So closing the loop over the operator is not only de-confounding the existing
signal — **it is what makes β legitimate at all**. Without presence you cannot
separate missing from negative and must not tick; with it you can, and the class
stays falsifiable.

(Caveat already recorded in the paper: the endpoint accepts an `author`
parameter and does not honour it — the response echoes `:author-filter null` and
the same 1,025-session total for every value. The filter is client-side.)

## The socket that already exists

`code/v05/wm-hyperparameter-update` carries `:evidence-refs`, and today every ref
is a git sha. The business-model job is **not** a parallel register: it is
producing evidence-refs that point *outside the repo*, in a shape the existing
learner already consumes. Small, checkable target.

## Interim honesty

§4.3 gives the precedent: declare a class **substrate-unavailable and hold it at
prior** rather than let agents score themselves. `:fire-pattern` already does
exactly this — 168 emissions, 0 applied, held at 0.500. An exogenous class
should sit there until real outside events arrive, not be scored on emissions.

## Blocker (settle first)

**The loop has been dead since 2026-07-14.** Daily from 05-22, then one
`ERROR nil` and stop; `futon2/data/wm-full-loop/archives/stop-line-2026-07-15/`.
Five weeks. Calibrating a policy that is not running is a paper exercise, so
whether the loop comes back — and why it stopped — is prior to any of the above.

Note also what the death forecloses: it is *not* an absorbing state in the
dynamical sense, and cannot be read as one. The dynamics did not continue and
fail to leave; they halted. Whether `:address-sorry` would have recovered is
unanswerable, not answered.

## Related

- `T-devmap-forward-model-calibration.md` — same series.
- `T-forward-model-vs-active-work.md` — the cascade/clock mismatch, same series.
- Paper: `p4ng/sec-operator.tex` (`fig:wr-overlay` caption carries the death
  date; the sessions query and its broken `author` filter are in the body),
  `p4ng/app-argument-outline.tex` steps ⟨1.2.1⟩, ⟨1.2.2⟩, ⟨1.2.7⟩.
