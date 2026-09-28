# P10-1 discovery — withdrawal interpretation, effect, and readers

Date: 2026-09-28. This is a source survey only. It adds no intent, record,
route, reader behavior, prompt segment, or runtime state. Joe's lighter P10
decision is recorded at `holes/missions/M-象-2000.md:926–939`; the binding v2
constraints are at `holes/labs/M-象-2000/BUILD-PLAN-象2000.md:170–178`.

## Existing termination mechanisms

| Withdrawable act | What exists today | What an as-of reader actually sees | Missing for P10 |
|---|---|---|---|
| Rule records (`:rule/record`) | A rule version carries a timeline. The special `:effect :followup-half-withdrawn` is accepted as a later version (`agency/rule_timeline.clj:12–36`). `intervals` closes an applied version at the next applied version's time (`:40–53`). P13b used that for the requisition followup half. | `rule-timeline/as-of` selects the applied interval and renders either “requisition rule applied” or “followup half withdrawn” (`:55–66`). A plain LIST of rule hyperedges does not decide force; callers must use this timeline reader. | This is a family-specific replacement version, not a generic withdrawal act. It has no separate interpretation, interpretation version, effect author/grant, target act ID, provisional status, reversal, or stale-interpretation review query. A last notice never withdraws a rule (`:1–4`). |
| Incident clearance (`:incident/clearance`) | The record says which rule measures **may end**, explains the incident, and retains compensation debts. Its payload references the incident and measure IDs (`agency/incident_clearance.clj:74–86`). P14 explicitly says permission only: no withdrawal or settlement (`P14-clearance.md:1–6`). | Clearance records remain alongside rule records. `rule-timeline/as-of` does not read them, so clearance alone does not change rule force. | A clearance is a condition/permission for a later effect, not that effect. Treating it as automatic withdrawal would conflate explanation with action and violate 象/释义非授. |
| Grants (`:grant/record`) | A grant has immutable domain interval `[from,until)`; `until` is optional (`agency/grant_record.clj:47–62`). `grant-covers?` enforces it at every chain level (`:122–147`). | Queries before `until` may return granted; at exactly `until` and later they return no grant. An open live grant stays open. | `until` is chosen when the grant is minted. Closing an existing open grant later would require overwriting/retracting it or a new reader-visible termination record. There is no grant-withdrawal reader today. Withdrawing somebody else's grant also needs the P3 authority check. |
| Promises / parks | The authoritative `/tmp` park/followup state is deleted/released by existing transitions. History appends `:promise/released`, `:promise/dependency-terminated`, `:promise/budget-exhausted`, `:promise/deadline-expired`, wake/lease/ack transitions (`agency/parked_on.clj:329–354,400–428,477–511`; `followup_queue.clj:56–65`). P5 separately appends `:promise/fulfilled` and `:promise/lapsed` observations (`agency/promise_outcome.clj:13–14,44–91`). | Promise replay folds the transition chain; outcome readers distinguish fulfillment/lapse from wake/release. The retained creation/history survives removal from `/tmp` (`promise_outcome.clj:93–110`; `P5-outcomes.md:1–31`). | These are promise-specific terminal acts. Release does not prove fulfillment, and none is a generic withdrawal of another act. P2c still keeps `/tmp` authoritative, so using promise history as the generic P10 authority would change a held decision. |
| Pattern cards | `POST /api/alpha/evidence/psr` appends a pattern-selection observation and then `backpack-put!` overwrites one agent-keyed entry (`transport/http.clj:3532–3579`). `POST …/pur` appends a use outcome and `backpack-clear!` destructively removes it (`:3581–3630`). The backpack is persisted in `~/code/storage/futon3c/backpacks.json` and mirrored into registry metadata (`:3473–3529`). | `GET /api/alpha/backpack/:agent-id` returns only current state (`:3682–3690`); it has no valid/system as-of contract. The prompt provider's `active-pattern-card` hook still returns nil (`agency/pattern_card_provider.clj:22–25`), so prompt-line currently shows retrieval, not backpack state. | No append-only card selection/effect history, no exact session key, no atomic save, no withdrawal authority, no provisional status, and no reader joining effects. A second PSR silently replaces the first agent-wide card; a PUR silently clears it even if another session set it. |

The storage API also supports direct hyperedge retraction with an explicit valid
time (`futon1b/API-CONTRACT.md:396–420,451–454`). A retraction makes current or
later valid-as-of reads omit the document while older valid-as-of reads can
still see its prior version; system-as-of can distinguish what the store knew
before and after the retract transaction. This is useful substrate behavior,
but it is not P10's effect model: the target itself disappears from ordinary
LIST results, the retraction is not a queryable act carrying actor, grant,
interpretation/version, or provisional/reversal state, and readers cannot list
effects whose interpretation changed. P10 therefore must not use raw retract
as its domain operation.

## The required as-of shape

P10 needs an append-only effect, for example `:act/withdrawal`, whose properties
include at least:

```clojure
{:withdrawal/target "act:..."
 :withdrawal/author "agent-id"
 :withdrawal/at "..."
 :withdrawal/basis {:kind :self|:grant|:provisional-interpretation
                    :grant-id "act:..."          ; when another party's act
                    :interpretation-id "..."     ; for inferred withdrawal
                    :interpretation-version 3}   ; exact relied-on version
 :withdrawal/status :effective|:provisional
 :withdrawal/reverses nil}
```

The exact schema is a DERIVE/implementation decision, not established here.
The important behavior is that the target hyperedge/evidence remains queryable.
An “in force as of T” reader fetches targets and withdrawal effects at the same
valid/system basis, validates self-authorship or a P3 chain, and subtracts a
target only when an effect is in force at T. An interpretation label alone is
never in that effect set. Reversal should itself append an effect referring to
the provisional withdrawal; it must not erase either record.

Every present rule consumer would need the join. `rule-timeline/as-of` currently
uses only rule timelines, so it would not see a new generic withdrawal until
changed. Generic hyperedge LIST deliberately returns stored documents rather
than domain “in force” answers. A separate `rules-in-force-as-of` projection is
therefore clearer than changing LIST to hide withdrawn documents. The same
principle applies later to grant coverage, promises, and agreements.

When an interpretation is revised, a review query compares each effect's
`interpretation-id/version` with the latest version of that interpretation and
lists mismatches. It does not silently reverse the old effect: P10 says list it
for review, and provisional reversal is a separate act.

## Pattern cards and mid-turn swap

There are two nearby facilities, but neither implements a card act:

1. PSR/PUR plus the backpack stores one current pattern per **agent**, not per
   exact `(agent, session)`. PSR overwrites it and PUR clears it
   (`transport/http.clj:3473–3529,3532–3630`). The persistence uses plain
   `spit`, and current-state mutation happens even if the evidence append's
   receipt reports failure; it is not an append-only authority.
2. `session-mode.el:116–222,319–348,536–568` fetches context-retrieval evidence
   and decorates cooked turns with a retrieved-pattern sigil. It displays what
   retrieval associated with a turn, not an agent's declared active card.
3. Prompt-line already reserves the precedence seam: an active card wins over
   retrieval (`SEAM-prompt-line.md:119–125`), but
   `pattern-card-provider/active-pattern-card` is the intentionally empty hook.

A correct mid-turn swap needs an exact-session append-only selection act, then a
self-authored withdrawal effect for the previous card selection, then a new
selection. The prompt provider resolves those records for exact `(agent,
session)` and changes on the next bounded render. It must not use the agent-wide
backpack overwrite as proof of the swap. A PUR can still describe outcome; it
must not silently withdraw a newer card from another session.

## Withdraw interpretation and Joe's provisional surface

The controlled literal vocabulary has **no `withdraw` intent**. Its tags are
approve, disagree, clarify, propose, extend, prioritize, delegate, verify,
constrain, defer, continue, redirect, explain, report-problem, collect, qualify,
and ask-action (`emacs/session-mode.el:605–623`). A search of current
`~/.emacs-graph/session-turn-analysis/*.analysis.json` found no exact stored
intent value `withdraw`; historical withdrawals therefore remain distributed
among other inferred labels, as the mission records at `M-象-2000.md:253–260`.
Adding the vocabulary label changes classification only. By the BUILD-PLAN it
cannot terminate anything.

The best existing visibility surface for a provisional effect is a dedicated
prompt-line segment:

- It renders for the exact agent/session in the JVM, is bounded, appears in
  Joe's REPL prompt, and the same facts appear in the next agent turn header
  (`SEAM-prompt-line.md:27–31` and API decisions).
- A concise marker/header can name “provisional withdrawal of X” and its effect
  ID without imperative text. It stays visible until reversed.
- Joe's one-word reversal can be recognized from his next operator turn only if
  it resolves an unambiguous visible provisional effect for that exact session.
  Ambiguity must ask which effect; caller name or temporal proximity must not
  choose one. The reversal appends a record and the segment then disappears.

The session-mode 象 lighter is useful secondary annotation after a turn has
been analyzed, but it currently decorates retrieved patterns and inferred turn
vocabulary; it has no durable provisional-effect identity or command path. The
agent turn header is valuable for the agent but is not Joe's primary control
surface. No existing component accepts the one-word reversal today. Nothing
should be bell-routed to Joe: his ordinary next input and prompt are the seam
named by the decision.

## First implementation packet

Start with **self-withdrawal of an exact-session pattern card**. This is the
smallest act whose author and current consumer can be made explicit: Joe's
decision names immediate card swaps, the selection producer already has author
and session, and prompt-line has a dormant active-card hook. It avoids changing
rule enforcement while the generic effect contract is proved.

Packet P10-2a:

1. Define and validate append-only `:pattern-card/selection` and
   `:act/withdrawal` hyperedges (or one selection act plus the agreed generic
   effect), both with exact agent/session, valid time, source, and harness.
   Self-withdrawal requires effect author = stored selection author. No caller
   parameter can override the stored author. Do not mutate/retract the target.
2. Add a pure card-as-of query over selections, effects, and interpretation
   records, with separate valid/system bases. Only an effective self effect (or
   later validated P3 effect) ends a selection. A provisional effect changes
   the provisional presentation but remains reversible.
3. Wire `active-pattern-card` to a pre-fed exact-session cache; keep futon1b off
   the 250 ms render path. Expose explicit set/swap operations. Do not reuse the
   agent-wide backpack as authority.
4. Produce no new general “withdraw intent means effect” automation in this
   packet. That belongs to the later provisional packet.

Acceptance includes both required bad cases:

- Append an unconfirmed/inferred withdraw **interpretation** for card A. At
  valid-as-of before and after that label, card A remains active. This directly
  pins 象/释义非授.
- Append A's self-withdrawal effect at T. The selection record A remains
  readable at every later system time; card-as-of is A before T and inactive
  at/after T. This directly pins 象/收回亦是行.
- Then select B in the same operation boundary used for swap. The exact-session
  prompt resolves B, while another session's card is unchanged. A failed B
  append cannot leave A withdrawn without a typed incomplete-swap result and
  recovery rule; the packet must choose an enforceable transaction protocol,
  not ordered best-effort writes.

## Later packets, in order

1. **P10-2b, rule self-withdrawal:** add rule-author identity if necessary,
   generic withdrawal effects, and a `rules-in-force-as-of` reader. Preserve
   the existing P13b timeline semantics and prove all current consumers use the
   new reader before calling the rule withdrawn.
2. **P10-2c, withdrawal by grant:** use P3 `grant-covers?` over an explicit
   withdrawal act kind or target rule ID; reject expired, wrong-grantee and
   broken-chain grants. This enables withdrawal of another party's acts.
3. **P10-2d, 象 interpretation:** add `withdraw` / `op-drop-stack` to the
   versioned turn-analysis vocabulary. Store an interpretation record with
   target and version. Prove labels alone leave every in-force view unchanged.
4. **P10-2e, provisional effect and reversal:** create the provisional effect,
   prompt-line segment, exact-session one-word reversal, ambiguity handling,
   and review list when an interpretation version changes. No approval bell.
5. **P10-2f, grants:** add a withdrawal-effect join to `grant-covers?`; do not
   rewrite a grant's original interval.
6. **P10-2g, remaining types:** reconcile promise terminal acts and incident
   clearance with the generic view, then extend agreements/offers when P11
   exists. Preserve their domain-specific outcome distinctions.

Each packet must enumerate and test its readers. Merely storing an effect while
the enforcement/query path ignores it does not withdraw the act.

## Read discipline

No live reads were needed. Futon1b was already reported overloaded, so this
survey used source and committed reports only. No code, runtime, store,
backpack, vocabulary, prompt registry, or Emacs state was changed.
