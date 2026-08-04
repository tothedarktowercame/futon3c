# Capability proof: "the memory store makes prior work available to later work" (live document)

Sibling of `M-diagramprover/capability-proof.md` (the APM instance) and
instantiates the same method from `M-diagramprover.md` §"Application to
theorem-proving capability construction" (`8b3f2213`). Commissioned by Joe via
claude-10, 2026-08-04.

Same discipline: the top claim is read constructively (BHK); each node is a
typed hole with a contract; warrants upgrade **only by certificate, never by
narrative**; revisions land as deltas, not silent edits; refusals stay typed.

**Warrant classes** — `mechanical` (executed witness) · `replicated`
(independent runs agree on load-bearing quantities) · `inductive-n=K` (K
observed instances, honesty bounds stated) · `designed` (contract written, no
witnesses) · `registered` (candidate mechanism, not applied) · `refused` (with
species: proved-impossible vs not-yet-capable).

**Cross-reference, not duplication:** APM `N5 — Retrieval serves the need when
consulted` is this document's **M4**. APM N3 is this document's **M1**. APM N4
is **M3**. Those nodes carry their detail here; the APM proof should cite rather
than restate them.

---

## TOP: prior work is available to later work at the moment of need

**Constructive reading.** A procedure exists that, given a later task, surfaces
the earlier learning bearing on it *while that task is being done*, with a
witness at each stage: the learning was recorded, the need was expressed, the
query reached the record, the record was consumed, and the consumption is
attributable afterwards.

**Current state, stated so it can fail:** the relay runs on **repo-memory**.
The store amortizes **across chains, not within them** (M-diagramprover §5,
08-03). A capability proof for the store is therefore *not* discharged by the
APM programme working — the APM programme currently works partly by routing
around this store.

**Corpus:** 559 `:memory` entries (measured 2026-08-04); 25 attachment
endpoints, density min 1 / median 3 / max 40.

**Warrant: under construction.** Discharge = M1–M8 at sufficient warrant, with
M4 and M5 the binding constraints.

---

## M1 — Sessions with extractable content yield reviewed, attached memories

**Contract.** Every completed chain with distillable content produces reviewed,
tagged, attached memories authored by someone other than the runner.

**Certificates.** 12 memories promoted 2026-08-03 (5 e9 + 4 j07 + 3 e10), all
attached under author≠reviewer. Scribe pass on S3/a96J01 recorded 5 memories
first-attempt, all `:assert`, all authored `codex-2`, verified by direct id
fetch and corroborated independently by the store count moving 522 → 527. Three
of those five recovered content the runner had **lost** to a refused write —
the separation of powers doing work rather than being asserted.

**Certificate: the write path was unusable, and the repair has a same-day
before/after (2026-08-04).**

| | zai-2, pre-fix | zai-3, post-fix |
|---|---|---|
| tool trace | `[memory_record]` → ✗ → `[memory_record]` | `[memory_record]` — one call |
| failure | *"missing required fields: subjects"* | none |
| result | deferred to PAR time | `e-61165a7e…`, verified in store by direct id fetch |

The runner had been **omitting `subjects` entirely**. The schema already marked
it required, so this was not a missing constraint but an *unanswerable* one:
`ref/type` was typed `{:type "string"}` with no enum and no example, while
`:kind` directly above it carried a four-value enum. A model that does not know
the vocabulary omits the field rather than guessing.

After exposing the 21-value `ArtifactRefType` enum plus an example
(`bbcaee8c`), zai-3 succeeded first try — and its reasoning names the cause:
*"The natural subject is a git commit, which is exactly what the new enum value
enables."* So the enum drove the choice; the success is not incidental.

Same tool, same store, same day, one changed variable. **Warrant on the repair:
`inductive-n=1 before/after`, not controlled** — nothing was randomized and the
two runners are different sessions.

**And the test found a second instance of the same defect.** zai-3 used
`ref/id: "HEAD"` and flagged, unprompted, that if concrete shas were intended
*"that constraint wasn't surfaced."* Correct: HEAD names a different commit
tomorrow, so the subject is unresolvable at read time — worse in a bitemporal
store, where the entry is permanent and the reference drifts. Fixed in
`ca309f94`.

Two fields, one failure shape: **a validator that knows the answer and a schema
that does not tell the caller.** That is M6's asymmetry — every gate is a
refusal, none is a supply — appearing at the *tool* boundary rather than the
store boundary. Any tool-schema field typed `{:type "string"}` where the server
validates against a closed set is a silent-omission bug waiting to happen.

**Warrant: `inductive-n=4 sessions`** (S3 + the three 08-03 passes). Extraction
held without correction across all four.

**Upgrade path.** Driver-triggered scribe per completed chain (APM N9), which
converts this from "someone remembered to run the scribe" to a pipeline
property.

## M2 — Unmet demand is legible

**Contract.** A query that finds nothing leaves a typed record of what was
wanted, in the asker's vocabulary. Without this the loop cannot close: you
cannot backfill toward a demand you did not record.

**Certificates.** Hunger audit exists in the scribe protocol
(`M-zai-learning-loop/scribe-protocol-hunger-audit.md`); demand-side tagging
closed the E10 mid-solve hunger exactly. Pull-side receipts (`a5c3f8bf`) record
per-call offered ids with round, timestamp and a **dispatch** key.

**Warrant: `inductive-n=1`** — one demand recorded and subsequently satisfied.

**Refusal, typed (`not-yet-capable`).** Empty *push* recalls are recorded as
`recall-empty` but carry no expression of what the asker actually needed; the
dispatch-time query is built from problem prose, not from a felt gap. Demand is
legible only on the **pull** path.

## M3 — Agents consult the store when framed to

**Contract.** Task framing, not invitation, elicits consultation.

**Certificates.** E10 controlled contrast, same agent and same store:
**0 lookups under invitation (E9) vs 21 under the two-part frame** (13 recon +
8 mid-solve at event anchors), preregistered in `E9-pull-probe-prereg.md`.

**Warrant: `inductive-n=1 controlled contrast`.** This node is **CLOSED** in
the four-layer anatomy: propensity is not the bottleneck once framing is right.

**Consequence for V3 (delta against `V3-arm-design.md`).** Axis 1 was built as
`:push` / `:push+pull` / `:pull-only`, where the pull arms differ by an
*invitation*. E10 says an invitation yields **zero** lookups. So the arm as
built tests a treatment already measured to be inert, and the live contrast is
**invitation vs task-framing**, not push vs pull. The versioned invitation text
(`memory-pull-invitation-v2`, `dispatch_with_recall.clj`) is the wrong
manipulation. Recorded here as a delta; `V3-arm-design.md` to be revised, not
silently patched.

## M4 — A hungry query returns the on-point memories when they exist

*(= APM N5. Detail lives here.)*

**Contract.** A query in the asker's vocabulary returns the memories that bear
on the need, when such memories are in the store.

**Certificates AGAINST — and they are strong, mechanical, and mine:**

- **E8, frozen** (`e8-query-binding-20260803.json`, sha `07be2f39…`; ranked
  rerun `ff9b3682…`, three runs across two operators, byte-identical). Under
  the shipped query, **not one specifically-named target was present in the
  candidate list at all** — 19 candidates in two cases, target absent. The
  40% baseline is carried **entirely** by set-valued cases where any-of-five
  sufficed. Warrant on this negative: `replicated`/`mechanical`.
- **Failure is at candidate generation, not ranking.** Widening the cutoff from
  5 to 19 changes nothing. Query *cardinality* is inert (A = B8 = B12 = B16 =
  40%) because `query-ladder` truncates to three terms regardless of the
  builder's cap. Vocabulary is decisive (oracle: 40% → 80%).
- **Layers 2–4 of the anatomy** (framing / affordance / index-reach) are
  mechanical and open; psr index noise reproduced twice; E10 phase-A tag
  queries empty in contour vocabulary.

**Certificates FOR.** fix-4 first positive (an e9 memory surfaced for a
neighbouring problem, correctly graded *marginal*); demand-side tagging closed
the E10 mid-solve hunger exactly.

**Warrant: `WEAK` — `inductive-n=1` positive against a four-layer diagnosis
with a `replicated` negative.** This is the binding constraint on TOP.

**Upgrade path.** Repairs 2–4 (psr description line, `memory_search` query
param, tag backfill from hunger logs — claude-12 queue). Acceptance criterion
earned from the rank data: **measure candidate-pool depth against arm A's 6–19
before measuring hit rate.** E8's structure-aware arm failed by *starving
generation* (pools of 3–5), not by retrieving the wrong things.

## M5 — Consumption is attributable, and its verdict is deferred and relational

**Contract.** For every surfaced memory there is a later, honest verdict on
whether it was used and whether it helped — and the verdict is recoverable from
the record.

**This node is harder than anything in the APM proof.** "Did it help" is not
knowable at use time; it is relative to an outcome that arrives later, and to a
counterfactual that never runs. APM's endpoint is `lake exit 0`. This one has no
mechanical witness available even in principle.

**Certificates FOR (attribution half).** `use-receipt` enforces
`used ⊆ surfaced` and refuses silence — every surfaced memory needs an
inclusion reason. Ψ receipt-ranking degradation is now **loud**: `mode`,
`degraded?`, and a typed reason distinguishing `:stats-absent` from a failed
fetch (`39d6400b`). Pull-side offers are recorded, so the pull channel has a
denominator (`a5c3f8bf`).

**Certificate AGAINST (verdict half), measured 2026-08-04:**
**`:memory-use/kind` = 0 occurrences across the entire store.** The field is
projected only after approval and never inferred from lane or prose — correct,
and nothing supplies it. The relational verdict is *designed and unrecorded*.

**Warrant: attribution `mechanical`; verdict `designed`, n=0.**

**Refusal, typed (`not-yet-capable`).** Until some process supplies
`:memory-use/kind`, no mediation analysis through memory-use is identifiable —
the mediator is unmeasured. This is why NDE/NIE estimation was deferred in
favour of a refusal that *says so* (claude-10, 08-03).

## M6 — Governance holds: author ≠ reviewer, lifecycle enforced

**Contract.** Integrity properties are enforced by code, not convention.

**Certificates, `mechanical`.** `review-attachment!` throws
`"memory author cannot review their own attachment"`
(`memory_lifecycle.clj:133`, verified 2026-08-04). `use-receipt` refuses
receipts where `used ⊄ surfaced` or a surfaced memory lacks an inclusion
reason. Nothing is deleted — challenges and corrections are durable episodes;
the assert edge is a bitemporal projection. A successful-but-invisible write
reports `:stale-after-successful-repost` rather than success. Operator approval
retired (Joe, 08-03) **without** relaxing author≠reviewer.

**Warrant: `mechanical`.** The strongest node in this proof.

**The asymmetry worth stating.** Every gate is a **refusal**; none is a
**supply**. That is why M5's verdict half is empty and why a malformed write
was simply lost (fixed 08-03: `3d86cf1d` makes refusals diagnosable,
`86466f40` admits `:git-commit` so a runner can name the commit that witnessed
its own proof). Strict validation without a supplying counterpart yields
permanently empty fields.

## M7 — Memories transfer between stores under distribution shift

**Contract.** A memory recorded against one corpus remains sound and findable
when moved to another store serving a different population.

**First certificate, 2026-08-04 (claude-10, chain 3 / a96J05 on `ams-zai-1`,
zone-joe store, fresh session, real tool path).** Graded below.

**WITNESSED — `retrieval-across-transfer`, `inductive-n=1`, channel `pull`.**
A transferred memory
(`e-codexpilot-avoid-euclidean-measurable-space-diamond-by-direct-general-instantiation`)
surfaced organically in a **new session with its own vocabulary, on the remote
store, for a problem it was never tagged against**. Delivery was via phase-A
recon calls — the pull channel. Ledgered on zone
(`apm-driver/ledger.jsonl`, chain `apm-a96J05-d2dff9bfb06f`).

**The prediction registered with delta v11 held, and in its stronger form.** I
predicted the first federation certificate would be a *retrieval* certificate,
not a transport one. It is — and the reason is now concrete rather than
theoretical: **`psr_search` did not work on the peer at all until an index was
shipped separately, after someone discovered it was absent.** The bytes arrived
and were not retrievable. Retrievability is not inherited with the content; it
is a second artifact that has to travel. That is the `S-transfer` node earning
its place in the DAG.

**Split the grade by what is mechanical and what is testimony:**

- *Mechanical*: the memory was returned by a tool call in a fresh session on the
  peer store. That is in the tool result and the ledger.
- *Testimony*: that it was "directly relevant to a real obstacle" is the
  **runner's grade**, not an established fact. On today's record — a runner
  reporting zero sorries on a file with three compile errors — runner relevance
  judgements are evidence about what the runner believed.
- *Not witnessed*: **load-bearing status**. Under the use-typing agreed with
  claude-10 this is upstream even of `cited-only`; phase B has not run and no
  artifact cites it. `retrieval-hit → solved` is untouched by this certificate.
- *Out of scope*: the `psr_search` hits (`lp-norm-comparison` at 7.5,
  `metric-cauchy-convergence` at 5.0) are **pattern-level, not memory-level** —
  they are evidence about the pattern index, not about memory retrieval, and
  should not be counted toward M4 or M7's memory claim.

**Warrant: `inductive-n=1` for retrieval-across-transfer; `designed` for the
capability claim** (a transferred memory *helping* remains unwitnessed).

**The hard part, named now so it is not discovered later.** This is
formally the same shape as APM N6 — a **transportability** claim — but for
memories rather than capability. A memory's hooks, tags and attachment
endpoints were fitted to the source corpus's vocabulary; M4 already shows that
retrieval is *endpoint-relative* (delta v3: the same memory is rank 0 at one
endpoint and absent from the candidate list at another, with attachment density
held constant at the corpus maximum). Transfer therefore moves memories into a
neighbourhood where their retrievability is **not** inherited. Expect the
federation certificate to be a *retrieval* certificate, not a *transport* one:
bytes arriving is not the claim.

## M8 — The store's own effect is measurable

**Contract.** The record supports computing whether the store helped, with
denominators, not just numerators.

**Certificates.** Mechanical endpoint capture landed 08-03 (`faf8170d`,
`8c031cec`): executed `lake` exit, comment-stripped sorry counts, axiom
verdicts per named declaration, executed in a detached worktree at `sha-post`;
runner testimony structurally excluded (its own test asserts the runner's
claimed sha appears nowhere in the witness); `pre-open?` makes the
stale-assignment class mechanical and *removes* those rows from the
denominator. Attribution ambiguity is now recorded rather than resolved
silently (`221455ee`). Pull-side denominator exists (`a5c3f8bf`).

**Warrant: `mechanical` for capture; `designed` for the endpoint definition**
(`endpoint-preregistration-draft.md` — §3 continuation rule still with Joe,
§1b statement fidelity added 08-03 and unimplemented).

**Refusal, typed (`not-yet-capable`).** Statement fidelity for *fresh*
formalizations is irreducibly a judgement; the preregistration says so rather
than implying a mechanical check exists. Failed fidelity **voids** a row rather
than scoring it 0, because a weakened-statement proof and an honest failure are
different events.

---

## What this proof does not claim

- That the store currently helps the APM relay. It does not, within a chain —
  the relay runs on repo-memory (M-diagramprover §5). Amortization is across
  chains.
- That any arm of V3 has been run. None has; no fresh Zai has been spent on the
  cohort.
- That `:memory-use/kind` will be populated by any existing process. Nothing
  supplies it today.

## Open deltas against sibling documents

1. **`V3-arm-design.md` axis 1** — E10 refutes the invitation manipulation (M3).
   Revise to invitation-vs-task-framing; do not silently patch.
2. **APM `capability-proof.md` N5** — should cite M4 rather than restate it;
   M4's negative is now `replicated`, stronger than N5 currently records.
3. **`retrieval-stage-causal-spec.json`** — M7 implies a transport node that the
   current DAG lacks; federation is not modelled.

## Update log

- **2026-08-04, claude-12** — skeleton stood up; eight nodes; warrants graded
  from existing receipts only. Binding constraints identified as **M4**
  (retrieval, WEAK against a replicated negative) and **M5** (consumption
  verdict, n=0). Strongest node **M6** (governance, mechanical). M7 opened with
  no certificates. Three deltas raised against sibling documents.
