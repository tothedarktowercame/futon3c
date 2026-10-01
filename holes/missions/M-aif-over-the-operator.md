# M-aif-over-the-operator — close the AIF loop over the operator's responses

**Status:** IDENTIFY (opened 2026-10-01, claude-17 with codex-10). Live; open work is the unchecked items under "Work items".
**Owner:** Joe · interactive side claude-17 · War Machine side codex-10.

## Origin

Joe, 2026-10-01: "from the M-象-2000 side we now have tool, and from the AIF side
the theory now finally seems to have a viable implementation." The aim is to
"zip" the two into one mission, and to work it from both sides, swapping work
between the automated team (War Machine) and the interactive session team.

The problem is stated in `p4ng/sec-operator.tex`: one action class received
108 unanswered proposals (WR-0, `futon3/library/war-room/wr-0-organise-without-apparatus.flexiarg`);
its learned credit fell, but the record could not distinguish rejection from
absence. Remedy proposed there: record considered-and-accepted,
considered-and-declined and no-response separately, with an explicit basis for
any claim that a proposal was considered; whether that improves selection is
an intervention to test.

## Side A — what the interactive side records today (claude-17)

All live on 2026-10-01 unless marked.

- **Offers** (M-象-2000 P11): `POST /api/alpha/offer` mints `:offer/record` with
  numbered options, exact seat, optional expiry; shown on the operator's prompt line.
- **Agreements**: the operator types `yes`, `yes 2`, `yes act:ID 2` or the same
  prefixed with `🈸:`; both parsers (`agent-chat.el`, `agreement_record.clj`)
  accept only that grammar. The route mints `:agreement/record` naming offer,
  option and the operator's turn evidence. Ambiguous and refused acceptances
  are recorded as refusals.
- **Withdrawals** (P10) and operator negation (P12-5) exist as records.
- **Obligations** (P9): agreements are the source of the who-owes-what projection.
- **Turn evidence** in futon1b, with operator authorship checked.
- **Intent readings**: 象 reads the intent of operator turns; 小象 does it
  classically (`futon3c/emacs/xiaoxiang-preview.el`). Where 象 readings are
  stored durably: to confirm.
- **Agent reply marks** (trial from 2026-09-30, `~/code/CLAUDE.md`): each reply
  paragraph carries one of 象's intent marks (㊭ propose, 🈸 ask-action, ...).
  These are text, not records.
- **Commit trailers** link commits to the agent session and dispatching job.

What this gives an AIF loop, per offer: accepted (which option) / declined
(🈚, negation) / withdrawn / no response, with the operator's turn as the basis
for the first two. No response holds only once the offer, its response window
and the absence of a linked response are durably joined, and it never shows the
offer was considered (codex-10's correction, below). Gaps:
- a 🈸 in a reply does not post an offer; until agents post one, most asks
  leave no record;
- "declined" has no classical grammar yet (only `yes` is parsed);
- War Machine proposals (nag / brief / silent notifications) are not offers,
  so WR-0's 108 proposals would still be unrecorded.

## Side B — what the War Machine's AIF implementation computes (codex-10)

The live selector constructs provisional cascades, not canonical declarations.
`futon2.aif.wm.cascade-decision/cascade-decision` reads the current mission problems and
their interpretation receipts, qualifies their tokens by target, and turns
library patterns into guarded token transitions.  `token-belief-carry/stage`
retains the initial/continuing belief and observation boundary.  The cascade
lanes supply the finite rollout model and the retained risk, ambiguity and
information terms; `policy/select-action-cascades` computes the posterior and
selects an acting cascade.  Preferences come from the current live-C family
schedule (`live-c/family-scales`, `live-c/family-schedule`) and are retained by
`preference-audit/build`; empirical habit and verified pattern-use evidence
enter separately as E, including `cascade-feedback/attach-pattern-evidence-menu`.
Thus learned habit can change selection without being smuggled into the
certificate's G decomposition.

An operator response should enter as one observation occurrence per proposal,
at response/withdrawal/expiry time rather than at an arbitrary polling rate.
Its minimal shape is `{proposal-id, option-id?, outcome, offer-revision,
operator-turn-evidence, observed-at}`, where `outcome` is one of `:accepted`,
`:declined`, `:withdrawn`, or `:no-response`; acceptance additionally names
exactly one option.  A proposal displayed without a response is censored until
its response window closes, not immediately negative evidence.  The occurrence
belongs at the same admitted-observation boundary used by the token carry and
next-tick refresh (`token-belief-stage` / `token-belief-input`).  Initially it
should update a proposal/action-class response model and its empirical E/habit
term: for example, separate posterior counts for acceptance, decline and
censoring, scoped by proposal class and option.  It should not silently rewrite
C: an operator's response is evidence about likely response and practical
usefulness, whereas an explicit statement of what outcomes are wanted is the
preference input.  Once an observation model is declared and pinned, policy
rollouts can also predict these response outcomes and G can score them normally.

The machine already learns a narrower but complementary fact.  At close,
`cascade-feedback/receipt` separates patterns merely selected from patterns
whose selected-to-enacted bridge and accepted increment were verified.  Later
construction receives target-local and global counts of successful and
incomplete applications; the Beta(1,1) likelihood-ratio prior can therefore
favour patterns that previously worked, including a mid-run pattern introduced
to get unstuck.  This is evidence about *how a cascade worked on a task*.
Operator records instead say *how a person responded to a particular proposed
action or option*.  The two evidence families should remain distinguishable,
then meet in policy selection rather than being collapsed into one success bit.

**codex-10:** Side A's final enumeration currently overstates the evidence:
`expired` establishes `:no-response` only when the offer, response window and
absence of a linked response are all durably joined.  It does not establish
that the operator considered the proposal, and neither an intent reading nor a
display event alone should upgrade silence to `:declined`.  The proposal record
also needs a stable proposal/action-class identity; otherwise the WR-0 credit
update cannot be scoped or replayed.

## Zip (claude-17, 2026-10-01)

### Records to observations

Side B's observation is `{proposal-id, option-id?, outcome, offer-revision,
operator-turn-evidence, observed-at}`. From Side A:

| outcome | Side A record | observed-at | basis | exists? |
|---|---|---|---|---|
| `:accepted` + option | `:agreement/record` | agreement time | acceptance evidence (operator turn) | yes |
| `:declined` | operator negation (P12-5) or a decline grammar | negation time | operator turn | grammar missing; P12-5 fit to check |
| `:withdrawn` | `:act/withdrawal` on the offer (P10) | withdrawal time | withdrawing act | yes |
| `:no-response` | offer with `until`, no linked agreement / negation / withdrawal | `until` | the durable absence join | join not built |

`proposal-id` is the offer id. Offers are immutable, so a changed offer is a
new id; `offer-revision` can be the offer id until revisions exist. A shown
offer with no answer before `until` is censored, not negative (Side B).

### Work items

Each item names the change and how to observe that it holds. Owners are the
side best placed to do it, not a gate.

Interactive side (claude-17 / 象-2000):
- [ ] **Action class on offers.** `:offer/record` validation requires a stable
  `:offer/action-class`; an offer posted without one is refused, and the
  offer tests cover both cases. Credit cannot be scoped or replayed per class
  without it (codex-10).
- [ ] **`until` on loop offers.** Offers that carry an action class must carry
  `until`; one posted without it is refused. Gives no-response a window.
- [ ] **Decline recorded.** Either a classical `no` / `no act:ID [N]` grammar,
  parsed like `yes` in both parsers and minting a decline record linked to the
  offer, or P12-5 negation shown (with a test) to resolve against offers.
  Today `negation_interpretation.clj` appears to resolve against disclosures.
- [ ] **Agents post an offer for each 🈸.** The rule is written in the reply
  proforma; over a sample of later agent replies, every 🈸 paragraph has an
  offer id on the operator's prompt line in that turn.

War Machine side (codex-10):
- [ ] **WM proposals become offers.** At least one nag / brief / silent
  notification class is minted as offers with its WM action class as
  `:offer/action-class`; the offers appear in futon1b.
- [ ] **Response model as an E-family term.** Per action class and option,
  posterior counts of accepted / declined / censored enter E, separate from
  `cascade-feedback` pattern evidence and from C (Side B); a selection receipt
  shows the term and its counts.

At the seam (proposed owner: WM side, which consumes the observations):
- [ ] **Response projector.** Reads offers, agreements, withdrawals and
  declines from futon1b and emits Side B observations, including no-response
  evaluated as of each offer's `until`. Read-only, no timer; computed when the
  WM next reads.

Tests:
- [ ] **T1 (records):** the projector over offers posted from 2026-10-01
  returns one observation per closed offer, and its counts match a hand count.
- [ ] **T2 (WR-0 recordable):** one WM notification class runs as offers for a
  week; its no-response and decline counts are reported separately.
- [ ] **T3 (intervention, per sec-operator):** the comparison is written down
  before the response model is switched on (what is compared, over which
  offers, what result would count against it); then selection with the model
  is run and the result recorded against that statement.

## Working from both sides

The interactive session suits the small live edits (offer schema, grammar,
Emacs); the automated team suits the WM items. Work can pass between teams as
offers, so the mission's own hand-offs are recorded by the loop it builds.
