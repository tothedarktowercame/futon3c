# P4-2 discovery — live act sources for the overreach scanner

Date: 2026-09-28. This is a read-only source and record survey for P4-2. It
does not define a new record type, run the scanner over live data, or change a
producer. P4-1 is `futon3c.agency.overreach/scan` at `0feb3aeb`, with the
missing-time correction at `9991c229`.

## Read basis and an unavailable fresh census

I chose one fresh system-time pin, **`2026-09-28T00:35:57.017Z`**, and issued
this first, sequential LIST request (shown unescaped for readability):

```
GET http://127.0.0.1:7073/api/alpha/hyperedges
    ?type=grant/record
    &limit=1000
    &system-as-of=2026-09-28T00:35:57.017Z
```

It returned HTTP **503**, with no result body to count. Immediately before it,
the cheap `/health` read reported `permits/available 0`, three evidence reads
holding permits for 179–906 seconds, and heap use 3808/4096 MB. Per the packet
instruction I did not retry or issue further futon1b reads. Consequently this
report does **not** claim a fresh count at that pin. The two supplied live grant
IDs and the prior verified captures below remain evidence, but do not substitute
for a successful new temporal LIST query.

Existing pinned evidence that can be reused without another store read:

- P3-2 captured
  `GET /api/alpha/evidence?tags=coordination,mesh-edge&since=2026-09-27T00:00:00Z&before=2026-09-27T22:18:11.900276Z&system-as-of=2026-09-27T22:18:11.900276Z&limit=1000`
  (with returned cursor pages): **586** mesh records, all `:invoke`.
- P3-3 captured all evidence in that same day through system pin
  `2026-09-27T22:29:21.685918Z`: **10,150** records. That census classified
  harness/origin fields; it did not classify all bodies as acts or search them
  for grant authority.
- P13a's verified valid-time read found three named `:rule/record` acts:
  `act:8f827fa4-b3de-46a3-a9cb-f3262f7a94ff`,
  `act:4b526112-dd5d-4765-a8a6-ed8701d0089c`, and
  `act:9bb67b13-caf7-4f96-9232-52bacf99fd8e`. Its recorded URL was
  `/api/alpha/hyperedges?type=rule/record` at valid-as-of
  `2026-09-27T20:11:00.759554Z`; it did not record a system-as-of pin, so it is
  evidence of those reads, not a compliant new P4 census.
- P14 verified one `:incident/clearance`,
  `act:0d850827-6af2-4ef9-92d3-8bbec4c7a618`.
- P3-1 verified the supplied two `:grant/record` IDs:
  `act:6c2f1392-4489-4e45-9d46-79c59b604471` and
  `act:85dcc857-49be-41d6-892a-55eeeaa4df7c`.

Thus the committed artifacts establish **six named minted acts**, not that six
is the current total. Hyperedge LIST supports both valid/system pins
(`futon1b/API-CONTRACT.md:457–500`). Evidence LIST supports system-as-of, while
the Agency job endpoint is a current bounded snapshot and has no historical
system-time contract (`dispatch_graph.clj:116–140`). Local WM click files have
no futon1b LIST route.

## What should count as an act

P4 should scan records of an action or an explicit institutional change. It
should not turn every evidence assertion into an act merely because it has an
author and timestamp. Otherwise observations, retrieval results, and
interpretations become actions and all fail `:no-grant` by construction.

| Live source | Producer and pinned evidence | Mapping to P4-1's six fields | Missing information |
|---|---|---|---|
| Minted rule records, `:hx/type :rule/record` | `rule_record.clj:70–85`; three named records in the P13a/P13b verified reads above. A compliant fresh URL would be `/api/alpha/hyperedges?type=rule%2Frecord&limit=1000&system-as-of=<pin>`, but no such read succeeded today. | id=`:hx/id`; kind=`:rule/record` (or a domain kind derived explicitly from `:rule/kind`, to be decided); rule-id can be the act's own `:hx/id` only when another act is governed by that rule, not for the rule-creation act itself; at=`:hx/valid-time`. | No stored executor. `:rule/provenance :author` describes provenance, not necessarily the CLI executor. No grant reference / authority field. |
| Minted incident clearances, `:hx/type :incident/clearance` | `incident_clearance.clj:74–86`; one named verified record above. Fresh LIST would use type `incident%2Fclearance`; not executed after the 503. | id=`:hx/id`; kind=`:incident/clearance`; at=`:hx/valid-time`. The referenced measure IDs are subjects of the clearance, not automatically `:act/rule-id`. | No stored executor and no grant reference. `:clearance/provenance :grant-status :unrecorded` explicitly says there is no recorded grant; it is not an authority ID. |
| Minted grants, `:hx/type :grant/record` | `grant_record.clj:169–183`; two supplied and previously verified records. The exact fresh LIST URL and failure are recorded above. | id=`:hx/id`; kind=`:grant/record`; at=`:hx/valid-time`; executor cannot be derived from grantor/grantee. For a delegated grant only, `:grant/parent` is the authority for the child grant act. | Both live roots omit parent, so both lack `:act/authority`. Grantor is the speaker and grantee receives authority; neither proves who ran the CLI. |
| Mesh dispatch evidence | `coordination_ledger.clj:84–117`; P3-2 pinned 586 invokes. | id should be `:evidence/id` (the immutable record); kind from body `:edge/kind`; executor=`:edge/from` for the dispatch action; at=`:edge/at`; rule-id absent. `:edge/id` is the job join key, equal to job-id for HTTP jobs but not universally. | No grant reference. Caller identity and `:evidence/harness` are provenance, not authority. P3-2's upstream result is a bounded reconstruction, never a stored causal grant. |
| Other evidence entries that encode actions | The day-wide P3-3 LIST has 10,150 evidence records, but there is no committed type/tag census that separates actions from observations. Producers include invoke lifecycle, promise transitions, origin corrections and WM ticks (P3-3 inventory). | Possible id=`:evidence/id`, at=`:evidence/at`, kind from a closed adapter table over `:evidence/type` plus tags/body event. Executor may come from a producer-specific body field; it must not default to `:evidence/author`. | A generic evidence record cannot reliably supply kind, executor, rule-id, or authority. P4-2 must whitelist reviewed action schemas rather than scan all evidence. A new census is still owed when futon1b admits reads. |
| Agency invoke jobs | Created in `transport/http.clj:1590–1710`; P3-2 used `GET :7070/api/alpha/invoke/jobs?limit=1000`, a current snapshot rather than a system-pinned read. | id=`:job-id`; kind=`:agency/invoke`; executor for the dispatch act=`:caller`; at=`:created-at`; rule-id absent. Target `:agent-id` is the intended worker, not the dispatcher. | `:request-commission` is request integrity, not a `grant/record`. No grant reference. Job retention is bounded, so this cannot support an exhaustive historical report by itself. The durable mesh edge is the better first source. |
| WM clicks | HTTP admission is `transport/http.clj:9013–9116`. Ordinary clicks write local budget/admission/run artifacts; commissioned R10 completion writes coordination evidence (P3-3b-3 report). There is no system-pinned click LIST or fresh count. | Local click id can be act/id; kind=`:wm/click`; at and executor require the particular click/run artifact. A commissioned completion can instead use its durable coordination evidence mapping. | Issuing caller is provenance, not a grant. Commission, RUN4 pin, harness execution-id and cast roles are not grant IDs. Ordinary local artifacts do not provide a single durable, system-time query for “every act.” |

Interpretation evidence and context retrievals are evidence **about** speech or
context, not acts to authorize. They become relevant when an actual act cites
one as authority, as described below.

## Recorded authority today

There is no common `:act/authority` or grant-reference field in any source
above.

- The **six named minted acts** contain **zero usable authority references**.
  The two grants are root grants, so `:grant/parent` is absent. The rule and
  clearance records have provenance and `:grant-status :unrecorded`, not a grant
  act ID.
- The **586 pinned mesh invokes** have **zero authority references by their
  producer shape**: the body contains edge id/kind/from/to/surface/at/ok/error,
  and the optional harness stamp is not a grant
  (`coordination_ledger.clj:84–111`).
- Agency jobs and WM clicks likewise have commissions, callers, harnesses and
  admission records, but no grant-record reference.

This is a schema count over the reviewed sources, not a claim that every one of
the 10,150 heterogeneous evidence bodies was re-examined. On these inputs an
adapter would map authority to nil and P4-1 would report every adapted act as
`:no-grant`. Whether existing acts should all appear in that report, or whether
P4 begins only after producers record authority, is an operator policy decision
for Joe. The adapter must not invent authority from signer/executor equality,
caller, harness, commission, provenance author, temporal overlap, or a grant
whose scope happens to match.

## 象 / 象-sonnet interpretation references

The local analysis sidecars identify themselves with `"method":
"agent-interpretation"`, commonly `"labeller": "象-sonnet"`, and link back
through `request_file`, for example
`~/.emacs-graph/session-turn-analysis/turn-55QW9M.json.analysis.json:4–6,75`.
Backfilled evidence is explicitly described as retrospective interpretation in
`P6o-3-backfill.md:117–141`.

No act source currently defines a typed authority-reference field for these.
P4-2b should accept an authority reference only from an explicit field and map
a cited interpretation to a typed value such as:

```clojure
{:kind :interpretation
 :ref "evidence:<evidence-id>"}
```

or, for a local source that has not been deposited:

```clojure
{:kind :interpretation
 :ref "session-turn-analysis:turn-55QW9M.json.analysis.json"}
```

Recognition must come from the referenced record's stored type/tag/body method,
not filename substring alone. P4-1 then returns
`:interpretation-as-grant`. The interpretation may explain why somebody acted;
it cannot become permission to act (象/释义非授).

## The scope union matters on one live grant

The 16:20 grant `act:6c2f1392-4489-4e45-9d46-79c59b604471` carries both
`:act-kinds [:kimi/create-target-enforcement]` and
`:rule-ids ["act:4b526112-dd5d-4765-a8a6-ed8701d0089c"]`. Consequently either
the domain act kind **or** that exact rule ID covers an act. Requiring both would
incorrectly reject the P13b rule act. The 16:28 grant has only
`:act-kinds [:kimi/investigate-target-gate]`; it does not cover activation by
description or by a missing rule-id. This matches `grant_record.clj:137–142`
and P4-1's OR rule.

## Smallest P4-2b packet

Start with **minted hyperedges only**, because they are append-only, have stable
act IDs and valid time, and have a bounded bitemporal LIST API. Do not combine
jobs, local click files and generic evidence in the first adapter.

1. Add a pure `hyperedge->act` adapter for the three reviewed types. Preserve
   the source ID/type/time. Return a typed mapping failure when executor or act
   kind cannot be supplied; do not substitute provenance author. The practical
   result may be that none can yet be scanned until executor is recorded. That
   is more accurate than naming the CLI's grantor or grantee as executor.
2. Add a read-only CLI that takes required `--system-as-of` and optional
   valid-time/window arguments, performs three sequential LIST reads with
   `limit <= 1000`, refuses incomplete/truncated pages, reads the grant records
   at the same system pin, and runs P4-1. It must report coverage and mapping
   omissions alongside findings.
3. Write the result to a caller-selected local report file. Do not POST a
   “越权发现” record. That new record type needs Joe's decision.
4. Acceptance should pin the current hard case: a stored rule record with no
   executor/authority is reported as unmappable or, only after an explicit
   policy decision to adapt it, as `:no-grant`; it must never be silently
   assigned the provenance author and matched to a convenient grant.

Before implementation, Joe therefore needs to choose between (a) reporting
legacy records with missing executor/authority as incomplete coverage, and
(b) treating every adapted legacy act with absent authority as `:no-grant`.
The present storage does not contain evidence that lets the adapter make that
choice itself.

## Discipline

Only source files and committed reports were read. The sole live operations
were cheap `/health` and one failed GET LIST. There were no POSTs, local store
writes, route calls that dispatch work, reloads or retries. The only file added
by this packet is this report.
