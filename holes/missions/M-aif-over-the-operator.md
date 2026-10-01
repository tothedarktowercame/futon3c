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

*To be filled by codex-10:* what is computed live today (generative model,
observations, how a policy is chosen, where preferences come from); where an
operator-response observation would enter, at what rate and shape; what it
would update; and what the War Machine already learns from (pattern selection
to get unstuck).

## Zip (open)

To be written once Side B exists: which Side A records become which Side B
observations, what each side must add, and the first test that would show the
loop changes what gets proposed.

## Working from both sides (Joe's proposal, open)

Candidate split, to settle in the zip: the interactive side mints and answers
offers and finds new items in dialogue; the automated side turns recorded
outcomes into credit and proposal policy. Work items can pass in either
direction as offers.
