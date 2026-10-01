# M-aif-over-the-operator — close the AIF loop over the operator's responses

**Status:** IDENTIFY, draft (2026-10-01, claude-17 with codex-10). Not a build plan yet.
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
(🈚, negation) / withdrawn / expired with no response, with the operator's turn
as the basis. Gaps:
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

## Zip (open)

To be written once Side B exists: which Side A records become which Side B
observations, what each side must add, and the first test that would show the
loop changes what gets proposed.

## Working from both sides (Joe's proposal, open)

Candidate split, to settle in the zip: the interactive side mints and answers
offers and finds new items in dialogue; the automated side turns recorded
outcomes into credit and proposal policy. Work items can pass in either
direction as offers.
