# P9-0 — obligations query discovery

Date: 2026-09-28. This is a read-only source survey for the P9 query. P9 asks,
for an agent and time `T`, what it owes and is owed; a released promise must not
remain in the answer (`BUILD-PLAN-象2000.md:156-166`).

## Records available now

| Source | Stored shape and location | How it can be read as of `T` | What it proves for P9 |
|---|---|---|---|
| P1 promise | A park or followup creation history row, type `promise/park-made` or `promise/followup-enqueued`, tagged `promise-history`. Its lossless format-3 payload contains the original record (`agency/promise_history.clj:90-107,150-159`). Optional promise fields are `:beneficiary`, absolute ISO `:deadline`, and typed `:fulfilment-criterion`; criteria are `job-terminal-ok` or non-machine-evaluable `prose` (`agency/promise_record.clj:9,17-35,43-53`). The promisor is `:agent`; the promise id is `:id` or `:followup-id`. | `GET /api/alpha/evidence?tag=promise-history&system-as-of=T&valid-as-of=T&limit=1000`, then retain creation types and filter decoded records by `:agent` (owed) or `:beneficiary` (owed-to). Evidence LIST supports both time axes (`futon1b/API-CONTRACT.md:217-228`). A complete reader must reject a full/truncated page and pre-format-3 rows rather than guess. | Debtor, optional creditor, optional deadline and criterion. The park payload may describe the work but is not a normalized deliverable. Missing beneficiary/deadline stays unknown, not Joe/never-due by default. |
| P2a lifecycle | Evidence rows tagged `promise-history`: dependency terminated, woken, released, budget exhausted, deadline expired, followup queue transitions, and replay store changes. Each has event time, per-promise sequence and predecessor (`agency/promise_history.clj:77-123,161-198`; mission `M-象-2000.md:759-767,786-800`). | The same evidence query and format-3 decoder. Fold rows by promise id and `history/promise-sequence`, after `check-chains`; timestamps alone do not order tied transitions. | Whether the promise was still present and which lifecycle transitions occurred. Dependency termination and wake are explicitly not fulfilment. |
| P5 outcome | Evidence types `promise/fulfilled` and `promise/lapsed`, tagged `promise-outcome`. Body includes promise id, source creation evidence id, beneficiary/deadline, criterion, observed job fields, and outcome basis (`agency/promise_outcome.clj:17,53-100`). | Evidence LIST by exact type (or tag, then exact namespaced type) with both as-of axes. Join `:evidence/body :promise-id` to the creation. | `fulfilled` proves the criterion true. `lapsed` proves the deadline passed first. A late success can produce both observations (`promise_outcome.clj:32-50`), so the projection retains outcome history rather than treating these as mutually exclusive states. |
| Live park cache | `/tmp/futon3c-parked-on.edn`; a record contains agent/session, awaited ids, payload, liveness timer/deadline, budget, mode, and the optional P1 fields (`agency/parked_on.clj:368-397`). | It has no bitemporal read and cannot answer historical `T`. It is useful only as a current-cache comparison; history is retained after cache deletion (`promise_outcome.clj:102-116`). | Current operational waiting, not historical authority. `:deadline-ms` is a liveness backstop, distinct from the promise's ISO `:deadline` (`promise_record.clj:33-36`). |
| Agreement | A schema-1 `agreement/record` hyperedge contains offer id, acceptance evidence, option id, exact copied option scope, offeror, Joe as acceptor, time, stamp and harness (`agency/agreement_record.clj:19-22,97-147`). Endpoints are offer id, `agent:<offeror>`, `agent:joe`, and evidence id (`:149-163`). | `GET /api/alpha/hyperedges?type=agreement%2Frecord&end=agent:<agent>&valid-as-of=T&system-as-of=T&limit=1000`. The hyperedge route makes `type` and `end` conjunctive and supports both axes (`futon1b/API-CONTRACT.md:457-500`). Join the named offer, also available by endpoint. | One obligation from offeror (debtor) to Joe (creditor), derived only from the accepted option's copied structured scope. An offer without an agreement, or one withdrawn before acceptance, produces none. |

P6 supplies the two temporal query axes above. P6o stamps record origin; it makes
the producing path attributable but does not itself create an obligation. P3's
grant chain can be reported from an act stamp/grant where one exists; legacy
promise rows have no act stamp, so their authority is `:unrecorded`, never inferred
from origin. P2a supplies the ordered lifecycle from which revocation/release is
decided.

## P8 versus the P5 evaluator

No P8 fulfilment-check generator or P8 check record exists. Repository searches
find only the plan, survey artifacts, and P5's `promise-outcome` evaluator. P8 calls
for an automatic record at deadline with one of fulfilled, unfulfilled, or unable
to determine, each citing its source, and expressly rejects wake as proof
(`BUILD-PLAN-象2000.md:156-160`).

P5 implements the machine-evaluable part: it evaluates retained history, emits
deterministic fulfilled/lapsed observations, and runs a bounded asynchronous sweep
(`promise_outcome.clj:29-100`; `promise_history.clj:200-225`). It does not emit an
unknown result: prose criteria, missing jobs, and read failures produce no outcome
(`promise_outcome.clj:1-9,46-51,102-117`). Therefore a conservative P9 can be built
now from creations, lifecycle rows, and P5 outcomes, but silence must remain
`:outcome-unknown`; it cannot claim P8 coverage or distinguish “not yet checked”
from “not machine-checkable” without inspecting the criterion and read completeness.

## What “released” means

Release is the explicit `promise/released` history transition written in a
`finally` after wake delivery (`agency/parked_on.clj:326-331`). Completed parks are
removed from the `/tmp` cache before their wake/release records are submitted
(`:343-357`). It is separate from dependency termination, wake, fulfilment, and
lapse; Joe's P5 decision retained this distinction (`M-象-2000.md:856-861`).

For P9, a creation is visible only before its first release transition at or before
`T`. Thus the plan's bad case—one overdue promise and one released promise—returns
only the overdue one. `promise/fulfilled` also closes an ordinary debt as completed;
`promise/lapsed` does not close it, but marks it overdue. Budget exhaustion and
deadline expiry should be surfaced as lifecycle facts, not silently equated with
release or fulfilment. This is a query rule over immutable rows; no record is deleted.

## Agreement fields and gaps

For an accepted agreement, debtor is `:agreement/offeror`, creditor is
`:agreement/acceptor` (currently required to be `joe`), and the candidate
deliverable is exactly `:agreement/scope`, copied byte-for-byte from the selected
offer option (`agreement_record.clj:110-146`). Free-form offer text and labels must
not create extra duties (`P11-0-offer-agreement-discovery.md:155-163`).

The scope presently knows `:act-kinds`, `:rule-ids`, description, and optionally
`:grant-until` (`agency/offer_record.clj:54-76`). These describe authority produced
by an agreement. `:grant-until` is the grant's expiry, not a delivery deadline.
There is no structured obligation deliverable, due time, fulfilment criterion,
outcome, release, or withdrawal-of-agreement field. The first projection can list
the accepted scope as an undated open obligation, clearly marking those fields
unknown; it must not invent a deadline from `:grant-until`.

## Smallest implementation packet

Add a pure `futon3c.agency.obligations/obligations-as-of` over already-decoded
plain records:

```clojure
(obligations-as-of {:promise-history [...] :promise-outcomes [...]
                    :agreements [...] :offers [...]}
                   agent-id t)
;; => {:owes [...] :owed [...] :ignored [...] :incomplete [...]}
```

Each returned row should have `:obligation/id`, `:source/id`, `:source/kind`,
`:debtor`, `:creditor`, `:deliverable`, `:due-at`, `:status`, `:authority`, and
`:as-of`. Unknown values remain nil with a reason in `:incomplete`. Promise status
is derived from an intact sequence up to `T`; agreement status begins `:open` and
has no due time. Partition by debtor/creditor after projection so one source cannot
yield inconsistent “owes” and “owed” answers.

Acceptance tests:

1. Two promises due before `T`, one with `promise/lapsed`, one with a later
   `promise/released`: only the lapsed/unreleased row is returned as overdue.
2. A wake without fulfilled evidence remains open/overdue; it is not completed.
3. A fulfilled row closes the debt; a late fulfilled+lapsed pair preserves both
   facts while status is completed-late.
4. An accepted offer yields exactly one open agreement obligation from offeror to
   Joe with the copied scope; unaccepted and withdrawn offers yield none.
5. Description-only or missing-deadline inputs remain explicit unknowns; a broken
   promise chain or truncated input appears in `:incomplete`, never a partial answer.
6. Boundary checks at event time are half-open: a release at `T` is excluded at
   `T`; an event after `T` cannot affect the answer.

Deferred: I/O adapters and pagination, P8's explicit unknown checks, normalized
agreement deliverable/deadline/outcome records, generic P10 withdrawal handling,
and P3 attribution for legacy unstamped promises.

## Spot checks after drafting

1. I re-read `promise_outcome/decide` (`promise_outcome.clj:32-51`): late success
   really may emit both lapsed and fulfilled, so the proposed projection preserves
   both rather than imposing a single terminal enum.
2. I re-read agreement validation and endpoints (`agreement_record.clj:124-163`):
   scope equality is exact, Joe is the acceptor, and the endpoint query can find
   either party; no deadline is stored on the agreement.
