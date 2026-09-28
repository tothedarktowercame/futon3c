# P10-2b discovery: rule self-withdrawal

Date: 2026-09-28. This is a source survey only. It made no live read: the
committed rule records, reports, fixtures, and readers answer the questions, so
no futon1b capacity was consumed.

## What currently decides that a rule is in force

| Reader | Source and time basis | Consequence |
|---|---|---|
| `futon3c.agency.rule-timeline/intervals` and `as-of` | `src/futon3c/agency/rule_timeline.clj:43-75` extracts `:rule/timeline` from `rule/record` maps. It orders positive `:live {:status :applied :at ...}` witnesses and forms half-open intervals. `as-of` compares its argument to those embedded domain times; commit and storage time do not activate a rule. | This is the only shared Clojure projection of rule force. It knows only the family versions `:requisition-with-followups` and `:followup-half-withdrawn`; it does not read generic withdrawal effects. |
| `scripts/xiang2000_p13b.clj` | Lines 8-16 build records from the checked-in P13b fixture, call `intervals`, and ask `as-of` at fixed domain times. `--write` writes the records, but the printed force answers still come from the fixture maps. | A CLI/report consumer, not a live enforcement path. It is the smallest first consumer to move to a new projection. |
| `scripts/xiang2000_p0.py` capture | Lines 216-225 LIST all `rule/record` and `incident/clearance` hyperedges with the same pinned `system-as-of` and `valid-as-of`; lines 218-224 refuse partial scans. | The snapshot has storage bitemporal bounds, but captures no `act/withdrawal` records today. |
| `scripts/xiang2000_p0.py` reconstruction | Lines 301-332 independently reconstruct the active P13b version from embedded `timeline.live.at`. It deliberately ignores old descriptions without a timeline and treats clearance as descriptive context, not termination. | This is a second implementation of force semantics. It must be changed after the shared reader, or it will continue reporting a withdrawn rule as active. |
| Kimi requisition gate | `src/futon3c/agents/zai_api.clj:1181-1195` parses the requisition; `:1255-1282` decides admission; the invoke path calls it at `:2035-2042`. Kimi selects this code policy at `src/futon3c/agents/kimi_api.clj:40-50`. | Enforcement is deployed code. It does not query rule records, `rule-timeline`, a rule cache, or an as-of instant. Therefore a withdrawal record cannot currently turn enforcement off. |
| Incident clearance validator | `src/futon3c/agency/incident_clearance.clj:49-72` checks named rule IDs and incident relationships against raw hyperedges. | It is not a force reader. Its contract permits measures to end but does not end them; the mission records that clearance leaves timeline answers unchanged (`holes/missions/M-象-2000.md:1104-1109`). |

No production prompt, HUD, HTTP route, runtime gate, or cache calls
`rule-timeline/as-of`. Source search found only the P13b report and tests. Thus
there is presently no stored-rule-driven enforcement consumer to switch. The
mission's decision is consistent with the implementation: application is
proved by runtime evidence, while actual admission remains the loaded code
(`holes/missions/M-象-2000.md:850-866,1078-1089`).

## Self-withdrawal has no identified self

A rule record cannot support an honest `:basis {:kind :self}` decision today.

`rule_record.clj:60-66` requires `:rule/provenance :author`, but that field is
the author of the record's account. The P13a record names `codex-4` and labels
the basis `:historical-reconstruction`
(`holes/labs/M-象-2000/P13a-requisition-rule.edn:30-40`). The P13b versions
likewise name `codex-4`, while their adoption sources are Joe's operator acts
and their grant status is unrecorded. Treating the reconstructing scribe as the
party empowered to end the rule would invent authority.

The CLI does not repair that gap. `rule_record.clj:70-85` mints the act and
stores the supplied record and harness; it stores neither CLI caller nor
executor. `:act/harness` describes execution context and grants no authority.
The runtime witness says where and when code was applied, not who owns the
rule. The source acts that adopted the versions are evidence of adoption, but
the schema does not designate an author/owner who has Decision P10's immediate
self-withdrawal right.

Therefore minting an effective rule self-withdrawal is blocked on the same
authority/executor decision exposed by P4. Until Joe chooses and the record
stores the entitled party, the system can record a proposed effect but must
not classify it as effective self-withdrawal.

## P13b's partial change and whole-rule withdrawal

They should coexist.

`:followup-half-withdrawn` is an applied successor within the P13b family. It
removes caller followups while retaining requisition admission and context
clearing. The report records the new no-op and its live time
(`holes/labs/M-象-2000/P13b-rule-times.md:15-27`). Recasting it as a generic
withdrawal would falsely end the whole rule and erase the evidence that the
remaining behavior continued.

A generic `:act/withdrawal` is instead a separate effect that ends an entire
target rule from the effect's `:at`, without deleting either the target or its
family history. Existing `rule-timeline/as-of` should remain the projection of
family versions. A new outer projection should first obtain that answer, then
apply authorized generic effects. This preserves the historical meaning of
P13b and gives future rule families the same termination mechanism.

The target needs an explicit convention. A P13b family has a description act
and multiple version acts; an effect aimed merely at an obsolete version must
not accidentally terminate a later version. The smallest coherent contract is
to give each rule record a stable family/root identity and have whole-rule
withdrawal target that identity. Until that identity and entitled author are
stored, a reader may surface a candidate effect but cannot safely terminate
the family.

## Proposed reader and packet order

`rules-in-force-as-of` should be a pure projection over:

1. rule records visible at one externally pinned system-time snapshot;
2. generic withdrawal effects visible at that same snapshot;
3. a domain valid-time `t`; and
4. an authority result for each effect.

It should return the base family answer, the active rule/version IDs, and the
generic effects classified as `:effective`, `:provisional`, or ignored with a
typed reason. It should call the existing timeline projection for family
semantics. Only an effective effect with a resolved whole-rule target,
`effect.at <= t`, and verified authority may remove the rule from the in-force
set. Interpretation and provisional effects leave the base answer unchanged.
System time selects the records the reader may know; embedded live/effect time
decides their domain effect.

The first implementation packet can proceed without settling rule ownership:

1. Add the pure outer projection and target/effect validation.
2. Represent unresolved authorship as `:authority-unresolved` and fail closed:
   the candidate effect is reported but does not terminate the rule.
3. Pin tests that P13b's partial successor remains in force, an interpretation
   changes nothing, an unresolved self claim changes nothing, and a supplied
   verified-authority decision ends the whole family at the exact half-open
   boundary.
4. Switch `scripts/xiang2000_p13b.clj` first because it is the only Clojure
   report directly calling `timeline/as-of` and has a bounded fixture.

That packet proves projection mechanics but does **not** authorize a live rule
self-withdrawal. After Joe's P4 decision, add the chosen author/owner field to
new rule versions, validate it at write time, and connect the authority check.
Then update P0 capture to include generic effects at its identical
system/valid pins and replace its independent force reconstruction. Finally,
any runtime enforcement must explicitly consume the new projection; today the
Kimi gate cannot react to stored withdrawal records at all.

No new record type is needed for the effect: `:act/withdrawal` already exists.
What is missing is the rule-family target identity, the stored party entitled
to self-withdraw, and consumers that use the joined projection.
