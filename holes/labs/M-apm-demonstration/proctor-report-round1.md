# M-apm-demonstration round 1 — retroactive proctor report

Date: 2026-08-16

Proctor: ams-codex-2

Evidence: frozen `role-cards/proctor.md`; the six committed frame packets;
`M-apm-demonstration.md` W.5–W.14; and the Agency jobs endpoint queried with
`limit=500`.

## Method and limitation

I applied the frozen card literally. I counted every Agency row addressed to
`codex-4` inside the reconstructed frame interval without using `caller` to
decide whether it counted. I used `caller` only for the separate exclusivity
test. Ambiguity was resolved toward guidance. I inspected prompt and result
payloads to assess uptake and residual movement.

This is a retroactive audit, not the open-and-close live witness required by
job 1 of the card. The card is not wrong, but its required live observation
was not staffed during the round. Frames 1 and 2 preserve cycle-open and
sentinel times in W.6 and W.7. Frames 3–6 do not preserve exact
`:cycle/opened-at` / `:cycle/closed-at` instants in the cited frame files or
W.10–W.14. For those frames I used the conservative observable interval from
the first measured-seat job's Agency `created-at` through the final such job's
`finished-at`; these are bounds on the active dispatch interval, not invented
cycle timestamps. Consequently, exclusivity is established for the observable
dispatch interval only in frames 3–6. A foreign seat row in an unrecorded gap
between true cycle open/close and these bounds would not be detectable as
in-window from the prescribed surviving record.

## Windows used

| frame | interval used (UTC, 2026-08-16) | basis |
|---|---:|---|
| 1 | 08:48:34–~09:14:00 | W.6 explicit open and sentinel |
| 2 | 09:20:12–~09:59:00 | W.7 explicit open and sentinel |
| 3 | 10:30:06.547–10:58:11.994 | first/last `zai-1` Agency job; W.10 says close at trial cap |
| 4 | 11:15:28.987–11:58:44.699 | first/last `codex-4` Agency job; W.12 says four sieges, and W.12-post confirms premature close |
| 5 | 12:04:25.898–13:15:15.597 | first/last `codex-4` Agency job; W.13 says close at registered cap |
| 6 | 13:53:36.427–14:29:24.690 | first/last `codex-4` Agency job; W.14 records closure |

The unrelated machinery job to `codex-4` at 10:03:40 is between frames 2 and
3 and is therefore `not this cycle`, not silently absorbed into either count.

## Seat exclusivity

| frame | solver-seat callers in interval | student-seat rows in interval | verdict |
|---|---|---|---|
| 1 | two `codex-4` rows, both caller `claude-7` | one `zai-1` row, caller `claude-7`; no other `zai-*` | no stranger traffic; the `claude-*`→`zai-1` row trips the card's direct-channel signature, but its payload is the cycle's own student dispatch |
| 2 | ten `codex-4` rows, all caller `claude-7` | none; no other `zai-*` | exclusive |
| 3 | no solver rows | three `zai-1` rows, all caller `claude-7`; no other `zai-*` | no stranger traffic; all three trip the direct-channel signature but are the cycle's three student trials |
| 4 | four `codex-4` rows, all caller `claude-7` | none; no other `zai-*` | exclusive over the observable interval |
| 5 | ten `codex-4` rows, all caller `claude-7` | none; no other `zai-*` | exclusive over the observable interval |
| 6 | twelve `codex-4` rows, all caller `claude-7` | none; no other `zai-*` | exclusive over the observable interval |

Thus there is no observed stranger contamination. The direct-channel flags in
frames 1 and 3 are the known caller-routing false-positive shape, not evidence
of an additional guide-to-student message: each row is itself a full cycle
student task/trial packet. I have not adjusted any count on that basis.

## Sender-blind solver-row classification

| frame | all solver rows | guidance | administrative | not-this-cycle |
|---|---:|---:|---:|---:|
| 1 | 2 | 0 | 2 | 0 |
| 2 | 10 | 9 | 1 | 0 |
| 3 | 0 | 0 | 0 | 0 |
| 4 | 4 | 4 | 0 | 0 |
| 5 | 10 | 10 | 0 | 0 |
| 6 | 12 | 12 | 0 | 0 |

Classification notes:

- Frame 1's original dispatch and repaired-statement redispatch assign the
  task/state but provide no proof hint, lemma, technique, or proof direction;
  I classify both as administrative task dispatches. This reproduces zero
  guidance without relying on the prompt's claimed sender.
- Frame 2 has one initial task dispatch and nine guidance rows. The latter are
  packets labelled guidance 1–8 plus two Agency rows for guidance 2
  (`invoke-1786872588379-4568-6dbfc7e4` and
  `invoke-1786872591094-4569-c9544691`). The duplicate counts twice under the
  card. Therefore the independent raw guidance count is **9**, not 8.
- Frame 4's initial packet changes solver process (the sustained-siege card and
  exact route/frontier material), so under the card's ambiguity rule it is
  guidance, as are the two process continuations and final content hint.
- Frame 5's initial plan-first intervention and all nine continuations move the
  proof process, so all ten are guidance. The first carries the plan/path-lift
  direction; the remaining nine are process-only continuations.
- Frame 6's initial assembly-close direction and eleven process continuations
  all move the proof process and therefore count as twelve guidance rows.

This exposes two distinct counting vocabularies in the existing narrative.
W.7's “guidance x8, raw rows 9” distinguishes eight intended guide actions
from nine Agency rows; the proctor card explicitly measures rows, so its number
is 9. W.13-post later correctly notes that dispatch steps are not necessarily
distinct plan-level attempts; that semantic correction does not change the
row count.

## Uptake and effect

### Frame 1

No guidance was given. The first solver row detected the inverted topology
statement. After an operator-authorized statement repair, the administrative
redispatch closed the theorem: residual `1 → 0`. This is repair uptake, not
proof guidance uptake.

### Frame 2

Guidance was visibly taken up. The route progressed through projected-curve
infrastructure, convexity/star-shapedness, angular primitives, the gauge
rescaling model, the no-simple-loop lemma, and the square-perimeter decoder.
The payloads report 15 compiling commits. Residual sorries remained `1 → 1`.
The duplicated guidance-2 row made no separately determinable difference: one
duplicate result reports work already completed, while the companion row
carries the actual two landed increments. The content intervention advanced a
large toolkit but did not close the target.

### Frame 3

There were no solver rows and therefore no solver guidance to assess. The
student-only negative belongs to the transfer measurement, not this guidance
count.

### Frame 4

The process guidance was taken up and accompanied substantial movement: the
solver built the global topology bridge, crossings, FTC machinery, and an
unwrapped-angle construction while the residual stayed `1 → 1`. The sole
content hint in siege 4 did not help the proposed route. The solver proved the
suggested direct composition periodic and therefore incapable of the required
endpoint gain (`ce6f1ac`). This is a clear “given and made no difference to
closure” finding, with the stronger result that the hint was formally refuted.

### Frame 5

The plan-first/process intervention was taken up: the solver wrote and revised
its own plan, then landed the seam-corrected lift, orientation proof, all four
cut orders, range lifting, and seam lemmas. The payloads report 55 commits.
Residual sorries remained `1 → 1`; guidance produced major frontier movement
but no closure. After the first packet, there was no conductor mathematical
content, so the later mathematical route cannot be credited to content hints.

### Frame 6

All guidance was process-only and the solver followed the continuation
structure. The first eleven rows successively landed the two telescopes, seam
welds, routing prerequisites, and three of four assembly branches while the
single residual persisted. The twelfth row completed the last branch and final
dispatch, moving residual `1 → 0`. Uptake is observable; causal attribution to
conductor mathematics is not, because the packets explicitly supplied zero
content and the assembly plan was already solver-authored.

## Round-level finding

The independent sender-blind row series is **0, 9, 0, 4, 10, 12** guidance
rows for frames 1–6 respectively, under the frozen card and the windows stated
above. It is not a declining series. The later-frame increase is largely
process continuation traffic, and frame 3 is student-primary with no solver
leg, so this raw series should not be read as a homogeneous treatment-dose
trend without those design facts. This report records the measurement; it does
not redesign it.

No memory was surfaced in any inspected solver dispatch: the dispatch receipts
were `completed-empty`, except one frame-5 row marked `store-unavailable`.
Accordingly there is no surfaced-memory uptake to credit or ignore in these
solver rows.

## Proctor conduct

I did not guide the solver, write to the substrate, or contact the student. I
used no cycle lane because the exercise was already complete; this report is
delivered to the operator through git as required.
