# M-u88-contextual-preferences — OPEN — fixture episode milestone pending

Status: OPEN — fixture episode milestone pending

Activation date: 2026-09-11

Activation authority: Joe explicitly directed mission activation and exact pin freeze on 2026-09-11.

Activation provenance: exact mission body from the independently reviewed draft at futon2 commit `db99f729be4b0fc29e2eb4e2e61b772c75ef75e7`; the retained draft and `U88-PREPARATION-BASIS.json` remain the review history.

Activation open hole: implement and independently review the fixture-scoped contextual-preferences interpreter described below.

Source: worklist `:U88` (open, owner `:joe`, deps `[]`); issue-board
`wm-ticket/U88` (:ready-for-lane, as-of 2026-09-10); Joe's 2026-09-10
direction to build non-blocked outer-loop/system items, relayed by Codex-17.

## Objective

Implement the contextual preferences / institutions **local interpreter**
proposed in
`futon2/holes/labs/wm-contract/SESSION-IAD-feedback-interpretation-2026-09-10.md`
as a pure, fixture-scoped Clojure namespace: institutional bindings as typed
data, applicability/obligation derivation as pure functions over that data,
and the frozen fixture episode below as the demonstration. Development
demonstrates only the proposed interpreter against **supplied fixture
authority**; it adopts nothing and contacts nothing.

## Explicit scope exclusions (violating any is out of scope)

- No institutional selector, no preference masses, no ranking.
- No HTTP endpoints, no message transport, no live agent interaction.
- No adoption of any binding outside the fixture; no registry/worklist writes.
- No obligation to prove the absence of every future rule-revision path
  (amendment authority is represented as data; probing its totality is not
  required).

## Prerequisites, represented as data (unknown ⇒ typed unknown/refusal, never a policy pick)

| Prerequisite | Fixture binding | If unspecified |
|---|---|---|
| Membership | Frozen roster revision listing author A and owner O; recorded establisher and per-recipient reason; online presence irrelevant | `{:membership :unknown}` blocks activation |
| Affected consumers | Evidence-backed list, part of the roster revision | same |
| Authority | The fixture's supplied adoption warrant (test-supplied; standing authority is production's concern) | `{:authority :missing}` blocks activation |
| Payload / view | Revision + finding + reason + response route; SHA-256 view digest | missing field ⇒ event refused |
| Receipt standard | `:authorized-inbox` (named in the instance) | `:transport-acceptance-only` is strictly weaker and cannot satisfy an inbox obligation; absence of a verifiable inbox adapter is an **adapter gap**, never a relabeling of a send |
| Deadline | Required iff the instance claims timeliness; fixture supplies one | unspecified ⇒ timeliness claims refused, no invented duration |
| Applicability | `:established` / `:not-applicable` / `:unknown`, with evidence and scope | `:unknown` blocks; binding a solo task must not manufacture a team |

## Frozen fixture episode (caption-review feedback, from the session note)

Revision 2 of feedback F for task T; recipients A (author) and O (owner);
required coverage 2; receipt standard `:authorized-inbox`; view V with digest
D. Event sequence and expected derived state:

1. `feedback-created` (reviewer) → obligation set {A, O}, coverage 0/2.
2. `delivery-attempted` → transport acceptance for A ⇒ coverage still 0/2
   (transport ≠ inbox receipt).
3. `inbox-receipt` A, digest D, revision 2 ⇒ coverage 1/2.
4. duplicate `inbox-receipt` A (same event id) ⇒ idempotent no-op returning the existing result, coverage 1/2.
   Reusing that ID with conflicting content is a typed refusal.
5. `inbox-receipt` A for digest D′ ≠ D ⇒ refused, coverage 1/2.
6. `inbox-receipt` A naming revision 1 ⇒ refused (revision mismatch), 1/2.
7. O leaves the registry / seat disconnects ⇒ denominator stays 2; O's
   obligation remains outstanding, coverage 1/2. Registry absence alone
   does not establish inbox unavailability; only separate route evidence can
   support `:unavailable`.
8. `deadline-reached` ⇒ O `:overdue`; never auto-satisfied.
9. A `disputed` ⇒ A still receipted (disagreement counts as receipt, not
   endorsement); separate dispute state opens.
10. O `inbox-receipt` (correct digest/revision) ⇒ coverage 2/2,
    `delivery-complete true`, `consideration :unobserved`,
    `subsequent-use :unobserved` (delivery, consideration, revision
    acceptance, and use are separate outcomes).
11. unauthorized `amendment` event ⇒ refused; outstanding obligations intact.

## Worker rubric (objective tests)

- Each rule above is a unit test with expected coverage/obligation state.
- Typed refusals (not nil, not exceptions-as-data-silence) for: wrong
  recipient, wrong digest, wrong revision, conflicting reuse of an event id, unauthorized actor,
  missing prerequisite field.
- One-recipient receipt cannot discharge another's obligation (test 7 vs 3).
- Missing applicability or authority blocks activation (test).
- Empty recipient set ⇒ vacuous, earns no delivery evidence (test).
- No `mark-done` operation exists in the interpreter API (test: namespace
  exposes derivation only).
- clj-kondo clean; `futon4/dev/check-parens.el` clean; focused ns tests only.

## Done means

Fixture episode derives exactly the states above under fixture-supplied
authority; rejecting cases refuse typed; scope exclusions restated in the
closing record. Closure/adoption of worklist `:U88` remains with Joe; this
mission's worker stops at the demonstrated interpreter.

## Coordinator review correction, 2026-09-10

Exact event replay is idempotent; conflicting reuse refuses. Registry absence
does not prove absence of an inbox route. These clarify the source contract,
not a new sanction or institutional adoption.
