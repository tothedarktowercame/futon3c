# P12-5-0 — Joe's negation of a disclosed choice

Read-only discovery, 2026-09-28. The governing split is unchanged: a negation
is an interpretation plus a separately authorised withdrawal. The interpretation
does not confer authority (`holes/missions/M-象-2000.md:486-505`).

## What the withdraw analysis does now

The delegated brief asks 象 for fragments containing `intent`, exact source
offsets/text, `target`, rationale and relations. For `withdraw`, the target is an
explicit act id when Joe names one, `seat-active-card` only for “this/the
pattern/card” in the current seat, and null otherwise. Guessing is expressly
forbidden (`emacs/session-turn-analysis.el:163-195`). Only operator records are
processed (`:669-695`), consistent with Decision P19.

The reaper extracts every fragment whose intent is `withdraw`
(`session-turn-analysis.el:493-502`). Null becomes a local 422
`target-unresolved`; `seat-active-card` is sent without a target, while an act id
is sent as the target to `/api/alpha/withdrawal/provisional`. The payload also
carries caller `xiang`, exact agent/session, interpretation id/version and a
stable fragment idempotency key (`:504-527`).

That server route is card-specific. It refreshes the exact seat, accepts only
the active card, and constructs a provisional `act/withdrawal` with basis
`provisional-interpretation` (`transport/http.clj:10165-10215`). It first looks
up a grant covering the caller; without Joe's provisional grant it returns 403
`:no-grant` before resolving or writing a target (`:10174-10178`). Emacs records
that outcome in `withdrawal_effects` in the local JSON turn record and displays
the “off until Joe's grant exists” message once per Emacs session
(`session-turn-analysis.el:669-716`). The analysis and outcome are local files;
there is no evidence entry for the interpretation. Consequently the disclosure
audit deliberately uses an empty interpretation population and marks its
negation check not run (`transport/http.clj:9742-9782`).

## Binding a reply to its report

A completion bell's text names the original job and links its job detail URL
(`transport/http.clj:1190-1211`). More importantly, the delivery job stores the
original as `:bellback-of` (`:1229-1240`), and the surface header exposes the
thread relation (`:4639-4684`). These are structured joins; the prose is only a
display.

The operator chat evidence does not preserve that join. It stores session,
turn-id, text and origin, and its `in-reply-to` is merely the preceding chat
evidence id (`emacs/agent-chat.el:3771-3835`). The local analysis record likewise
has agent, session, turn id and `origin=operator`, but no source job
(`session-turn-analysis.el:145-155`). Thus current records cannot prove which
report Joe answered.

The smallest sound repair is to retain the trusted completion-delivery job id in
the buffer and resolve its stored `:bellback-of` on the server. The operator-turn
evidence and analysis record then carry `source_job=<original job>`. Never infer
this by searching the visible bell text.

Resolution within that job is deliberately classical:

1. An explicit disclosure act id in the fragment selects that disclosure only
   after checking its `source-job` equals the bound job.
2. With no explicit id, exactly one standing disclosure for the bound job may be
   selected as `:single-standing`.
3. Zero gives `:target-unresolved`; two or more gives `:target-ambiguous` plus
   the candidate ids, shown to Joe. Text similarity between the fragment and
   disclosure `chosen`/`quote` is never a selector.

This is deterministic and makes the ambiguity visible. It also handles the
common report with one disclosure without forcing Joe to transcribe an act id.

## Durable interpretation

Store one evidence entry, not an authority-bearing act:

```clojure
{:evidence/type :interpretation/negation
 :evidence/author "xiang"
 :evidence/subject {:ref/type :evidence :ref/id <operator-turn-evidence-id>}
 :evidence/in-reply-to <operator-turn-evidence-id>
 :evidence/session-id <session>
 :evidence/body
 {:source-job <invoke-id> :target <disclosure-act-id>
  :intent :withdraw :fragment-id <stable-id> :fragment-text <exact-text>
  :analysis-version 3 :resolution :explicit-id|:single-standing}}
```

Its deterministic id is derived from operator evidence id plus fragment id. The
server writes it only after verifying Joe/operator origin, the source-job join,
and the target disclosure. It carries the ordinary evidence origin and harness
stamp naming the 象 interpreter. It carries **no `:act/stamp`**: 象 is reporting
a reading, not signing an effect. This supplies exactly the records consumed by
`disclosure-audit/negates?`, which joins an interpretation's target and withdraw
intent to a missing effect (`agency/disclosure_audit.clj:17-50`). Ambiguous and
unresolved readings should also be durable typed interpretation results, but
without a target they cannot satisfy that join.

## Confirmation and authority

Use option (a): route the recorded negation to the source job's orchestrator,
who withdraws or declines. DERIVE-2 is explicit that the unique invoke edge's
`from` authorises withdrawal; Joe has operator authority only when that edge
names Joe as `from` (`holes/missions/M-象-2000.md:492-501`). Allowing Joe to
override when he is not the orchestrator would contradict that decision and
turn an interpretation into authority by a different path.

The existing effect route already re-reads the disclosure's source-job edge,
requires caller = edge `from`, writes the effective withdrawal, and routes a
deterministic bell to the author with `bellback-of` the source job
(`transport/http.clj:9602-9638,9644-9717`). A new interpretation therefore
creates a correlated request to that orchestrator. Confirmation is the
orchestrator's call to the existing withdrawal route; refusal is also recorded.
Joe receives a short ambiguity notice when selection failed, not an approval
queue for every ordinary case.

## Packet order

1. Preserve the completion delivery's stored source-job join on the next
   operator evidence/analysis record. Test that ordinary typed turns have none
   and that reply evidence names the original job without parsing display text.
2. Add a pure resolver and durable `interpretation/negation` writer. Test
   explicit id, one standing disclosure, zero, and multiple candidates. Switch
   P12-4 audit from `:negation-check {:run? false}` to the evidence population.
3. Route resolved interpretations to the unique dispatch-edge `from`; record
   confirm/decline. Confirmation calls the existing disclosure withdrawal path.
4. End-to-end BUILD-PLAN case: two disclosures, Joe negates one in a report
   reply, one interpretation is stored, the orchestrator confirms one effect,
   the effect bell reaches the author, and the other disclosure remains standing.
   The bad case is a stored negation with no effect, reported as
   `:negation-without-effect`.
