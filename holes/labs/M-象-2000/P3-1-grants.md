# P3-1 explicit grants — implementation, store verification blocker

Status: local implementation and requested test gates pass; live acceptance is
**incomplete**. No P3-2 dispatch graph or P3-3 harness tagging. P13a/P13b/P14
records and historical notes remain untouched.

## Reviewed source acts

- **16:03:24.319201750Z**, `emacs-d120721d051f3e879ca067d3a0b4c982`:
  “Your fix isn't really yet getting at my requirement; I'd say, to do work on a
  Kimi seat, we need to pass the mission, excursion, or ticket name. If it changes,
  we compact.” **Not an explicit grant recorded here**: a requirement correction,
  not a separate dispatch. No authority inferred from its P13b adoption label.
- **16:20:01.051116340Z**, `emacs-46e69c9bcb4fad45e86fd6a63e649a84`:
  “we should create an enforcement rule similar to the inbox-zero followup”.
  Explicit joe → claude-11 instruction. Scope: create the Kimi target-enforcement
  rule; narrow act kind `:kimi/create-target-enforcement` and the corresponding
  P13b rule record `act:4b526112-dd5d-4765-a8a6-ed8701d0089c`. Does not grant
  standing authority to speak as Joe. Full quote is in the request/fixture.
- **16:28:10.181607946Z**, `emacs-b8292f5d0dd1a1d808cc859285150956`:
  “Let's look into these issues a bit more carefully.” Explicit investigation
  dispatch, scope `:kimi/investigate-target-gate`. The following “could be as
  simple as” proposal is NOT broadened into permission to activate it. No rule-id
  coverage assigned to this investigation grant.

Both candidates begin at their sourced act timestamp and have an open interval
end. No expiration or parent grant is invented. Root grantor is joe. Selection
and scope wording are reviewed transcriptions of explicit source instructions,
not automatic inference from intent labels. Validation proves the quoted text
and author against independently queried evidence; it cannot classify arbitrary
natural language as granting permission.

## Local API

`futon3c.agency.grant-record/validate!` takes record and evidence/ancestor context.
It checks source identity, author, time, verbatim quote, explicit basis, root
operator, parent identity (parent grantee = child grantor), subset scope,
contained [from,until) intervals and cycles. Text-only child scope is refused
with `:scope-unchecked`; a text-only root cannot answer a coverage query.

`grant-covers?` accepts validated stored grant hyperedges, grantee, kind/rule-id
and valid-time instant. It returns `{:status :granted :chain [root ... leaf]}`
or `{:status :no-grant :reason ...}`. It checks every chain level; until is
exclusive. Store validity starts at from; finite domain until is enforced by
this query, not an inferred deletion of the historical record.

`grant-status-for` additionally matches the adoption event's source ref and
returns `{:status :recorded :act-id ...}` or `:unrecorded`. It does not mutate
old adoption records. The matching P13b source/id lookup is tested locally;
no live successful lookup is claimed while readback is blocked.

CLI from futon3c:

```
clojure -M -m futon3c.agency.grant-record FILE.edn
clojure -M -m futon3c.agency.grant-record --write FILE.edn
```

Default mode reads source/ancestors and validates; --write re-reads those inputs,
opts into `:hx/mint-id`, uses a stable idempotency key, and verifies returned act.
No existing-id/upsert/retraction option is accepted.

## Live attempt and exact outstanding state

Both candidates passed default CLI validation against live evidence.
The 16:20 write returned HTTP503 `:postcommit-missing-act`, identifying
**`act:6c2f1392-4489-4e45-9d46-79c59b604471`**. A subsequent read-only GET found
that act. It contains the expected source and scope, but the store omits nested
nil values: `:grant/parent nil` is absent and `:grant/interval :until nil` is absent.
An identical keyed retry returned the same act receipt (not a second act), then
our exact readback check refused `:readback-mismatch`.

This is NOT a verified-success receipt. The act and durable idempotency receipt
exist despite the initial failure. Do not mint a new key, retract/overwrite this
act, or edit the receipt to hide the failure. **16:28 has not been written.**

Store source `futon1b_server.clj:212–255`: the transformed request, including
nested nils, is retained in `:act/request`; transaction writes act+receipt;
verification compares doc with store hydration. The throw at ~247 precedes
`hx/on-put!` and cache invalidation. The keyed no-op branch returns directly,
so retry does not perform those maintenance steps. Their completion must not be
assumed. Readback artifact saved locally at `/tmp/p3-1-postcommit-probe.edn`.

Required next step is a store-owned normalization/verification decision and a
supported recovery for the already-minted receipt/act. Optional absent/open
fields need one canonical representation across request, persistence, receipt
comparison and query validation. This packet does not weaken comparison, mutate
indexes, patch the running server or create duplicate grants as a workaround.
No JVM reload/restart was performed.

## Gates

- clj-kondo: 0 errors, 0 warnings; check-parens: OK.
- grant-record-test: 6 tests / 37 assertions, pass.
- rule-record-test: 5 / 36, pass.
- rule-timeline-test: 5 / 15, pass.
- incident-clearance-test: 4 / 56, pass.
- Refusal tests mutate the real 16:20 request; delegation uses that record with
  explicitly synthetic parent/child identities. Coverage checks include both
  interval boundaries, out-of-scope, broken/cyclic chains and text-only scope.
- No full suite. Live write failed as disclosed above; local test success is not
  represented as store acceptance. Related existing records are append-only and
  unchanged. Owner review required before resolving the store blocker.
