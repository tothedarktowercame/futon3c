# Excursion: E-elephant-2000-feature-completeness-demo

**Date:** 2026-11-07
**Status:** SKETCH. Nothing run. The demo is to be run once the Agency is safe to
use again; Codex is doing the E-evidence-cleanup cutover. (Joe, 2026-11-07.)
**Attached to:** [M-象-2000](../missions/M-象-2000.md). This excursion supplies
the live demonstration its acceptance list asks for. Its DOCUMENT section has not
been started, and Joe's user-side demonstration (DERIVE-2 item 17.4) is deferred
"until we see the core system working".

## HEAD

Joe, 2026-11-07: "Xiang is supposed to at least support Elephant-2000 semantics.
But this hasn't been tested or demonstrated live to my knowledge!" … "a separate
excursion … referring back to the original paper, any extensions we made, and
outlining a demo script that's needed for acceptance."

## The paper

John McCarthy, *Elephant 2000: A Programming Language Based on Speech Acts*.
Local copy: `storage/references/mccarthy-elephant-2000/elephant.tex`. The main
body is lines 130–1530; the rest is McCarthy's appended notes. Its claims, in
the terms used here:

- **Programs act.** Inputs and outputs are speech acts: requests, questions,
  offers, acceptances, permissions, answers, assertions, promises, commitments
  (l.151–158).
- **Programs remember.** Programs refer to the past directly. There is one
  history, and a commitment exists at time t if it arose before t and has not
  been revoked since (l.431–551).
- **Correctness comes from the acts.** Answers must be truthful and responsive,
  promises kept, commitments fulfilled, and only authorised commands obeyed
  (l.172–178).

[DERIVE-1](../labs/M-象-2000/DERIVE-1-mccarthy-requirements.md) translates the
paper into 32 requirements, R1–R32, in three groups:
- **A. Acts** (R1–R16);
- **B. Reference to the past** (R17–R23);
- **C. Program as logic, and operation** (R24–R32).

Each row also records what our translation leaves out. The demo below is scored
against those rows.

## What we added beyond the paper

From M-象-2000 DERIVE-2 and INSTANTIATE. All of it is built; most of it has been
checked only piece by piece.

- **Act stamp.** Each act records its executor, signer and authority. An act
  made without a covering grant is refused with 403, and nothing is written
  (象/名分有据; R10).
- **Grants.** Joe's go-aheads are recorded as permissions; an agent's offers were
  granted on 09-28 (R11).
- **Promise lifecycle.** A promise is a durable record with a beneficiary, a
  deadline and a test. It ends at release or lapse, not when its wake is
  delivered (诺必践; R6, R28). Pending history rows go through an outbox, with
  kill tests (P2c).
- **Obligation verdicts.** At a deadline a promise is judged fulfilled or
  unfulfilled (P8, P9). An unknown job state is never treated as terminal.
- **Agreements as joint acts** (P11; R12).
- **Blocker escalation.** One `escalation/blocker` act, then one bell to the
  orchestrator (P12).
- **Attestation of selections** (P21).
- **As-of view.** The 09-24 Kimi red-tape incident was reconstructed from the
  stores, with no row of the acceptance case filled by hand (P14).
- **Checks over a session.** `scripts/xiang_acceptance.py <session>` runs the
  McCarthy checks R17, R18, R16/R29, R3/R4, R6/R19 and R19-rewind.
- **A tension we kept open.** McCarthy's history is a finished past. An agent's
  promise, or a parked job, names a future that may never arrive (IDENTIFY,
  §Theoretical anchoring).

## What has already been shown

- **2026-09-29:** live confirmations of P11 (agreement `act:ba40d9aa…`) and
  P21 (attestation `act:442ac499…`), and a scripted live check of P12 (P12-6-4).
  An unscripted escalation has not been observed.
- **2026-10-16:** [DEMO-2026-10-16](../labs/M-象-2000/DEMO-2026-10-16-what-runs-today.md),
  §B3, ran `xiang_acceptance.py` on a real session. R18, R16/R29, R3/R4 and
  R6/R19 passed. R17 failed on one row. R19-rewind was pending, because
  turn-commit rows lack start heads.

Missing: the whole chain, from operator request to agent promise to deadline to
verdict to query, run once on a live conversation and shown to Joe as it
happens. Also missing: acts of the kinds the confirmations above did not cover
(question and answer, permission, withdrawal, refusal for lack of authority).

## Preconditions

- E-evidence-cleanup cutover finished, so that turns, replies and dispatches are
  findable as evidence (M-repl-circus's gate).
- 象 readings go to `象-sonnet` (primary since `fc32f09dc`, 2026-11-06), so that
  every demo turn is read without depending on Kimi quota.
- A fresh session, so the checks run over the demo alone.
- **Pre-flight (reading only, before the run):** for each step below, find in
  the code the route or query that shows its result, and mark the step *live*,
  *tests only* or *missing*. Steps marked *missing* are recorded as gaps, not
  improvised during the demo.

## Demo script

Joe speaks the operator turns; one agent seat (call it A) answers. Each step
lists what is said, the record that should appear, and how to show it. Routes
are to be filled in at pre-flight, not guessed now.

| # | Act (R) | Joe / agent A | Expected record | Shown by | Passes when |
|---|---|---|---|---|---|
| 1 | Question → answer (R5, R4) | Joe asks a factual question with a checkable answer (for example the head sha of a repo). A answers. | Question act; answer act linked to it, citing the record it rests on | Act history for the session | The answer gives the value, not a place to find it, and the cited record exists |
| 2 | Request with a deadline → promise (R1, R6) | Joe: "by HH:MM, do X". A: "I'll do X by HH:MM". | Request act; promise record (beneficiary Joe, deadline, test) | Open commitments of A (R26) | The promise is listed with its deadline before it is due |
| 3 | Offer → acceptance = agreement (R12) | A lists two options; Joe picks one. | Agreement as a joint act | Agreements query | One agreement, both parties, with the chosen option |
| 4 | Permission (R11, R10) | Joe gives a go-ahead for one named action. | Grant record covering that action | Grants query | The grant names its scope and holder |
| 5 | Unauthorised act refused (R10) | A attempts an act that no grant covers. | Nothing written | The route's reply | 403, and no new act in the history |
| 6 | Fulfilment (R6, R3) | A does X before the deadline. | Promise released as fulfilled | Verdict after the deadline | `:fulfilled`, with the time it was done |
| 7 | Lapse (R6, R3) | A second promise is deliberately left undone. | Promise lapsed | Verdict after its deadline | `:unfulfilled`; not hidden, not counted as kept |
| 8 | Withdrawal (R14, R19) | A withdraws a third promise, giving a reason, before its deadline. | Withdrawal act carrying the reason | Commitments of A as of before and after | The promise exists as of before the withdrawal and not as of after |
| 9 | Blocker escalation (P12) | A hits a real blocker on a fourth request. | `escalation/blocker` act, one bell to the orchestrator | Act history; job list | One act, one bell, linked to the source job, without scripting |
| 10 | As-of view (R18, R19) | Joe asks: "what did A owe me at HH:MM?" | — | As-of query at a time between steps 2 and 6 | Matches what was open then, not what is open now |
| 11 | Accounts both ways (R26, R32) | Joe asks what A owes and what is owed to A. | — | Owed/owing query | Both directions, consistent with steps 2–9 |
| 12 | Session checks | — | — | `scripts/xiang_acceptance.py <session>` | Every check PASS, or each FAIL/PENDING explained by a gap already on record |

## Acceptance (for M-象-2000)

- The demo is run once, live, with Joe watching. Each row is marked pass, fail
  or gap, with the record ids and query outputs pasted in, as DEMO-2026-10-16
  did.
- Every gap points to a ticket, or to a deferred item with its re-arm
  condition.
- The results are written into M-象-2000 §DOCUMENT, which is what its
  acceptance checklist asks for.

## Open

- Steps 5 and 9 need a real case, not a staged one. Staging them shows only
  that the code path runs, which the 09-29 scripted check already showed.
- Whether 象's per-turn reading of the demo turns (request, offer, permission)
  agrees with the stamped acts is a separate check from the act semantics.
  Count disagreements, and do not treat them as failures of the demo.
