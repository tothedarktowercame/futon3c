# Memory retrieval white paper — Version 2 plan

**Status:** proposed, pending agreement with claude-9 and scoping by Joe
**Opened:** 2026-07-31 (claude-2)
**V1:** `docs/retrieval-whitepaper.md` — frozen, keep as Version 1
**Ledger:** `docs/retrieval-evidence-ledger.md` — chronology, unchanged
**Supersedes for planning purposes:** `E-memory-whitepaper-plan.md` §Experiment 0

---

## 1. What changed since V1 was written

V1 was written on 2026-07-30 against a 16-half receipt slice filtered to
`author=ground-control`. An overnight run supervised by claude-9 has since
completed. The live corpus is ~17× larger and was written under a different
author, so V1's headline channel figures describe a slice, not the system.

Measured by claude-2 against the live store on 2026-07-31, frozen as
`receipts-export-20260731-all-authors.edn` and audited by
`observation_channel_audit.py` (regression-checked: it still reproduces
16/14/2/2/1 on the 07-28 export):

| | V1 (07-28 slice) | V2 (07-31, all authors) |
|---|---|---|
| Offered halves | 14 | **129** |
| Outcome halves | 2 | **115** |
| Outcome-half completion | 14.3% | **89.2%** |
| Joined rows | 2 | **114** |
| Metric-bearing rows | 1 | **20** |
| Recall `:ok` | 4 (29%) | 47 (36%) |
| Recall empty | 10 (71%) | 82 (64%) |
| `:timeout` / `:store-unavailable` | 2 / 0 | **46 / 12** |
| Total surfaced ids | 8 | 215 |
| `surfacing-via` populated | 0 | **60** |
| `inclusion-reasons` | 4 rows, 1 string | absent (superseded) |

**Two V1 predictions resolved.**

- V1 §8.2.1 item 2 named outcome-half completion as "the single
  highest-leverage repair". It went 14% → 89%. That repair has landed.
- V1 §8.2.1 item 1 warned that "a deployed writer is not a populated field"
  for `surfacing-via`. Verified populated on 60 rows. Closed.

**One V1 assumption refuted.** V1 §8.2.2 sized draft 2 against "489
dispatches" of headroom. That headroom does not exist (§3.1).

---

## 2. Three findings that reshape the plan

### 2.1 The APM queue has ~48 problems of headroom, not ~360

Three different counts of "problems with Lean" were in circulation — 89 (my
first pass, from `processed_level`), 135 (the sanctioned counter), and 145
(`Main.lean` files on disk). claude-9 flagged the gap as load-bearing and
declined to lock any of them in. That was right, and the resolution is that
**none of the three was the number we wanted.**

Classifying all 145 `problems/*/lean/Main.lean` files by whether they contain
a declaration and ≥ 8 non-blank lines:

| | substantive | stub |
|---|---|---|
| In manifest's Lean-bearing set | 52 | **24** |
| Not in it | **69** | 0 |
| | **121** | 24 |

The manifest's `processed_level` fails in *both* directions: it admits 24
scaffold-only stubs and misses 69 substantive files. (claude-9's hypothesis
was that the gap was stubs the manifest correctly excluded; it is the
opposite.)

**The addressable set is 121 substantive Lean problems.** Normalising the
receipts' `:problem` slugs to APM ids — three slug shapes occur
(`a92J05-…`, `hard-problems-a94j07-…`, and 15 named construction targets such
as `rouche-root-count-transfer` that are not APM rows at all) — yields 74
receipted ids, 73 of them substantive. This reproduces claude-9's independent
count of 73 exactly, which is the cross-check that the normalisation is
sound.

    121 substantive − 73 receipted = ≈48 problems of headroom

Not ~16 (my first figure), not ~360 (V1's assumption). ≈48 is roughly a 37%
increase on the 129 dispatches in hand: **material for RQ-level statistics,
nowhere near enough for per-coefficient promotion.**

claude-9 reports the run finished with 0 dispatchable rows and nothing in
flight; the queue stands at 50 resolved / 27 blocked-and-diagnosed / 4 partial
/ 1 dependency-blocked. So the 48 are not queued work — reaching them is a
scoping decision for Joe, not a resumption.

**Two prerequisites gate the 48, and neither is imminent.**

- *The recall-budget fix* (§6.3). It has **no owner and is not in progress**;
  claude-9 identified it and deliberately did not apply it, since it changes
  dispatch behaviour for every future run. It is Joe's call, not imminent
  work. Crucially it must be **validated on 2–3 throwaway dispatches with a
  measured drop in `:timeout` rate before the 48 are committed** — "the fix
  landed" is not "recall completes", and spending the last clean problems to
  buy a partial fix would be the worst available outcome.
- *A census step.* The queue covers 74 APM problems while 145 `Main.lean`
  exist on disk, so **71 problems have no queue row at all.** The headroom is
  not waiting to be dispatched; it must first be censused into rows (statement
  hints, `:line`, kind), and claude-9's session surfaced that step's own
  failure modes: stale hints silently repointing at a neighbouring target,
  empty `:line` lists throwing in the selector, two rows sharing one file.
  This work is **independent of the recall fix and can proceed in parallel.**

**Consequence: V2 is built from the 129 dispatches in hand.** The 48 are a V3
input, and worth dispatching only *after* the recall-budget fix (§2.2),
because dispatching them now would reproduce the 45% infrastructure-loss rate
on the last untouched problems we have.

### 2.2 Recall loss is structural, diagnosed, and non-randomly biased

This is V2's strongest new systems result, and it is a mechanism rather than
a rate. Verified independently by claude-2:

- `default-recall-timeout-ms` is **30 000 ms, a total budget**
  (`dispatch_with_recall.clj:19`, documented as such at line 136).
- `evidence/text-search` costs **9–16 s per query** on a healthy store
  (claude-9, receipt `4f7eeadd`).
- Recall issues **one query per subject**, with 6–12 subjects per row.

So recall **structurally cannot complete** for any row carrying more than
about two common subject terms. 46 of 129 dispatches timed out; with 12
`:store-unavailable`, **58 of the 82 empty results (71%) are operational
failures rather than retrieval misses** — i.e. ~45% of all dispatches lose
recall to infrastructure.

The bias is **not random**. claude-9 measured clustering by term commonality
rather than subject count: `construction-deriv` timed out on 6 subjects
(`deriv`, `zero`, `isopen`, `differentiableon`) while `a97A08` succeeded on 6
(`count`, `outer`, `roots`, `filter`, `card`, `inner`). It falls hardest on
analysis-heavy rows whose subject terms are common mathematical vocabulary. A
04:40 store restart halved the tail (29.9 s → 14.4 s for `deriv`) without
fixing it.

This directly vindicates V1 §5.5(i) — *missing observations are not negative
observations* — and upgrades it from a caution to a measured, directional
bias that any analysis over this corpus must carry.

### 2.3 Bitemporal `as-of` fails on two different routes, for two different reasons — and neither is XTDB

**This section was rewritten 2026-07-31 after tracing the defect to source.
The earlier conclusion — "bitemporality is broken, Experiment 0 is dead" —
was drawn from testing one endpoint and was too broad in one direction and
too pessimistic in another.**

claude-9 and claude-2 independently observed that `system-as-of` had no effect
on `GET /api/alpha/evidence`:

```
GET /api/alpha/evidence?type=pattern-outcome&limit=3                  → 17441 bytes
GET /api/alpha/evidence?...&system-as-of=2026-07-31T04:00:00Z         → 17441 bytes
GET /api/alpha/evidence?...&system-as-of=2020-01-01T00:00:00Z         → 17441 bytes
```

Byte-identical, no error. Both of us then generalised from this to "the store
ignores as-of". **That generalisation is wrong.** Tracing to source:

**Defect 1 — the evidence route never reads the parameter.**
`evidence-route` (`futon1b_server.clj:382–426`) dispatches to
`ev/query-evidence-response`, and `futon1b_evidence.clj` contains **zero**
occurrences of `as-of`, `basis`, or `snapshot`. The parameter is dropped in
futon1b's own handler and never reaches XTDB. Silent, undocumented, and the
caller receives present-time data believing it is historical.

**Defect 2 — the projection route implements as-of correctly, then refuses to
return it.** `memory-projection-route` (`futon1b_server.clj:582–604`) *does*
parse both parameters, and its own comment states "Explicit bitemporal
projection still reaches XTDB". Tested:

| request | result |
|---|---|
| no as-of, limit 10 | 200, 22 973 bytes, **10 memories** |
| `system-as-of=2020-01-01` | 200, 571 bytes, **0 memories** ✓ correct |
| `system-as-of=2026-07-29` (any in-range value, any limit 3/5/10) | **400** `:memory-projection-result-bound-exceeded` |

The 2020 case is the important one: it returns **empty**, which is the
correct answer for a store that did not exist then. **Bitemporality on this
route genuinely reaches the engine.**

The 400 is ours. `futon1b_graph.clj:942–949` fetches `(inc raw-limit)` rows
and throws if the count exceeds `raw-limit`:

```clojure
(list 'limit (inc raw-limit))
…
(when (> (count selected+) raw-limit)
  (throw (gates/layered-error 4 :memory-projection-result-bound-exceeded …)))
```

Under an as-of query the underlying scan returns **every historical version**
of each edge, so the row count always exceeds the limit and the guard always
fires. The guard cannot distinguish "too many distinct results" from "the same
results, many versions" — it is a truncation check that predates bitemporal
use and is blind to version history. Lowering the limit does not help; it
lowers the threshold in step.

**Consequence for the plan, in both directions.**

- *Do not report this to JUXT as an XTDB bug.* Both defects are futon1b
  application-layer faults with named source lines. Taking them to James
  Henderson as XTDB issues would be wrong on the facts. (There may be a
  legitimate adjacent question about XTDB history-retention semantics, but
  nothing measured here establishes one.)
- *Experiment 0 is less dead than V1 and the first draft of this plan said.*
  The projection path — which is what `dispatch_with_recall.clj:376–388`
  actually uses — reaches XTDB bitemporally. If the result-bound guard is
  made version-aware (dedupe by entity id before the count, or count distinct
  entities), dispatch-time **graph** state may become reconstructible without
  per-problem snapshots. That is a plausible partial rescue, and it is a
  one-file change in our own code rather than a dependency on anyone else.

**Claim discipline.** What is verified: the parameter reaches the engine on the
projection route (2020 → empty), and the bound guard blocks in-range queries.
What is **not** verified: that XTDB retains enough history to reconstruct
07-25 state, or that valid-time versus system-time semantics give what
Experiment 0 needs. Those are the next tests, not established results, and
V2 must not claim the rescue until they run.

#### 2.3.1 Defect 2 REPAIRED and independently verified (2026-07-31)

`futon1b` commit **`11c84dc9`** "Fix bitemporal memory projection result
bounds" (codex-2). The temporal XTQL scan now groups by the composite key
`(xt/id, matched-endpoint)` *before* the sentinel limit — collapsing
system-time versions of one edge while preserving an edge's legitimate
membership in several requested endpoint groups — with a defensive `distinct`
at the application boundary. Two files, +160/−1.

Verified by claude-2 re-running every probe rather than accepting the report:

| probe | result |
|---|---|
| in-range as-of, limit 20 | 200, `:selected-count 14` — **the repair** |
| in-range as-of, limit 14 | 200, 14 — passes exactly at the boundary |
| in-range as-of, limit 10 | 400 bound-exceeded — **correctly**, see below |
| out-of-range 2020, limit 10 | 569 bytes, `:selected-count 0` — bitemporality intact |
| current cached, limit 10 | 200, unchanged |

`clojure -M:node -m test-temporal` re-run: 12 tests, 47 assertions, 0 failures
— reproduces the reported figure. Commit touches only `futon1b_graph.clj` and
`test_temporal.clj`; Defect 1 and all futon3c files untouched, as scoped.

**A correction to my own acceptance bar, from codex-2.** I specified "an
in-range as-of query returns rows (not 400)" at limit 10. That test was
under-specified: at `2026-07-29` this endpoint has **14 genuinely distinct
edges**, so a limit of 10 *should* still refuse. The limit sweep disambiguates
what a single probe could not — 10 → 400, 14 → 200, 20 → 200 — which is
exactly the signature of 14 distinct results rather than version inflation.
Had codex-2 simply satisfied my bar as written, the guard would have been
broken to pass a bad test.

**Consequence.** §6.2 step 4 — "the cheap shot at Experiment 0" — has landed
and works. Dispatch-time *graph* state is now queryable by system-time on the
projection route.

#### 2.3.2 Both Experiment-0 caveats discharged — and a third gap found (2026-07-31)

The two caveats I had been holding open are now tested. Read-only probes on
`math-formalization/tactic-algebra-interference`, limit 50, with retry backoff
(the route is under `with-expensive-read!` and returns
`:expensive-read-busy` under load — transient, not a failure).

**Probe 1 — retention depth: PASS.**

| `system-as-of` | `:selected-count` |
|---|---|
| 2027-01-01 (future) | 22 |
| 2026-07-29 | 14 |
| 2026-07-27 | 7 |
| **2026-07-25** | **7** |
| 2026-07-23 | 0 |
| 2026-07-20 / 07-01 / 2026-01-01 | 0 |

Monotonic growth 0 → 7 → 14 → 22, and **history reaches 2026-07-25** — the
date of the first witnessed loop closure. The zeros are genuine absence, not a
retention cliff: they stop at 07-23, coinciding with when this pattern's
attachments were first written and with the receipt corpus's own start
(07-22), rather than at any round retention window.

**Probe 2 — valid-time vs system-time: PASS, and they coincide.**
`:valid-as-of` and `:system-as-of` returned identical counts at 07-25, 07-27
and 07-29. Identical output is also what a *silently ignored* parameter looks
like — the Defect 1 signature — so the discriminating test was run:
`:valid-as-of 2020-01-01` → **0**, and `:valid-as-of 2026-07-23` → **0**.
Both are honoured; they agree because records are written with valid-time =
system-time (no backdating). **`system-as-of` alone therefore reconstructs
dispatch-time graph state**, with no valid/system divergence to reason about.

**The third gap, found by completing the check: the text index has no
temporal capability at all.** Recall's *first* stage is a text search, so
graph reconstruction alone is insufficient. Tested:

```
GET /evidence/text-search?q=roots outside&limit=5                        → 13525 bytes
GET /evidence/text-search?q=roots outside&limit=5&system-as-of=2020-01-01 → 13525 bytes
```

Byte-identical, and the response reports only its own current
`:index-as-of "2026-07-31T04:44:43Z"`. Expected on inspection: it is an
application-controlled **SQLite FTS5 sidecar** (`futon1bi.text-index`, V1
§5.5(iii)), and SQLite has no bitemporality. This is not a bug to file; it is
an architectural property.

**Net position on Experiment 0 — a real but partial rescue, precisely bounded:**

| pipeline stage | as-of | reconstructible? |
|---|---|---|
| query → **text index** (seed) | ignored, current-only | **No** |
| matched memories → patterns → **graph projection** | works, retained to 07-25, valid ≡ system | **Yes** |

So we **can** now reconstruct: *given a seed set, what the graph would have
returned at dispatch time* — the pattern-arm counterfactual, which extends
`damage_state_sweep.bb` from two frozen fixtures across the whole history.
That is a genuine unblock for RQ2/D_functional.

We **cannot** reconstruct: *what the text index would have found* at dispatch
time. Since that is the seed, recall@k against a dispatch-time corpus stays
unavailable — we have the numerator (recorded `surfaced-ids`, plus deterministic
`subjects_for` giving the query terms) but not the denominator (what else was
findable). **RQ1's hybrid ablation therefore remains a V3 item**, gated on either
a temporal text index or dispatch-time seed-set capture going forward — the
latter being cheap and belonging in §6.3.

**Partial rescue on the query side.** `subjects_for(row)` is deterministic, so
the terms issued per dispatch *can* be recovered and paired with
`surfaced-ids`, giving (query → returned) pairs — more than the receipts alone
carry, and enough to characterise which queries starved. Two caveats that must
travel with any such reconstruction:

- The rows have been **edited since dispatch** (hints refreshed, `:line`
  repointed), so recovery is approximate and biased toward the *original*
  dispatch rather than the current row.
- **57 rows carry duplicate keys**, and the parser keeps the **oldest** value.
  That happens to bias in the helpful direction here, but it is an accident of
  parser behaviour, not a designed guarantee, and it should be verified
  per-row rather than assumed.

This does **not** restore recall@k — nothing freezes what was *retrievable* —
but it makes "which queries returned nothing, and did they share a term
profile?" answerable, which is the evidence §2.2's bias claim currently rests
on indirectly.

---

## 3. What V2 can and cannot claim

### 3.1 Off the table for V2

- **Experiment 0 as specified** (frozen chronological benchmark with
  dispatch-time corpus state) — blocked by §2.3, unretrofittably for existing
  data.
- **recall@k / nDCG against a frozen corpus** — same cause.
- **Threshold B / per-coefficient promotion** — n ≥ 20 *per coefficient*
  needs volume that §2.1 shows is not available from APM.

### 3.2 Newly on the table for V2

The overnight corpus turns out to carry a label set richer than V1 knew
about. Across 255 receipts claude-9 reports `used-ids` 238, `unused-ids` 182,
`rejected-ids` 126, **`rejection-reasons` 129**, `retrieval-to-use-ms` 126.

That is a **three-way used / unused / rejected label per surfaced id, with
free-text grounds** — captured in production, at scale, without a labelling
campaign. It substitutes for a meaningful part of what Experiment 0's
relevance judgments were for. Sample quality is high ("the imported
contraction theorem handles kernel integrability internally"), and a94J04
shows a distinct *considered-but-declined* category.

**This is the single most valuable asset the overnight run produced**, and V1
did not know it existed.

#### 3.2.0 CORRECTION (2026-07-31): the n is 121, not 129, and the source is not the receipts

*Verified before building anything on it. The correction changes V2-3's method
and must be read before §4.*

claude-9's figures — `used-ids` 238, `rejected-ids` 126, **`rejection-reasons`
129** — count **key presence**, not populated content. Balanced-parsing the
frozen export for vectors with actual content:

| field | key present | empty | **non-empty** |
|---|---|---|---|
| `rejection-reasons` | 135 | 128 | **7** |
| `rejected-ids` | 137 | 131 | **6** |
| `used-ids` | 250 | 220 | **30** |
| `surfaced-ids` | 224 | 152 | **72** |

**So the receipt-side taxonomy corpus is n = 7, not n = 129** — a 20×
overstatement, and V2-3 as originally specified does not survive on receipts.

**The job results rescue it.** Parsing all 130 captured files as JSON: 126 have
a non-empty `result` field, and **121 carry a memory-usage report**. The
reasoning the receipts failed to record is present in the runner's own prose:

> "`math-informal/pass-to-a-subsequence` (pattern library): conceptually
> relevant but had no reviewed memory attachments. The approach it suggested
> was subsumed by Mathlib's existing `exists_seq_tendsto_ae`."

**This is the entire justification for the capture in §3.2.1.** Without those
130 files the corpus would be 7. With them it is 121. claude-9's instinct that
the raw results were both richer and perishable was exactly right.

**What this changes in the method.** The reasons are **free-text prose, not
structured per-memory fields**, so V2-3 becomes a coding exercise over
narrative with its own error modes: reasons must be attributed to specific
memory ids by reading, some reports discuss patterns rather than memories, and
inter-coder agreement now matters. The pre-registered five categories (§4)
still apply; the coding protocol must additionally record **which memory id
each coded reason attaches to**, and flag reports where that cannot be
determined rather than guessing.

*Candidate sixth category, noted honestly and held to protocol:* two of the
first reports read carry a runner **overriding a stale prior assessment**
("prior candidate `status.json` claimed this was blocked — this was
misleading; I ignored this assessment and found the API directly"). That is
neither mismatch nor subsumption. Per §4's protocol it goes in the residue and
becomes a category only if it survives coding the full 121.

#### 3.2.1b Stratification: the taxonomy's n is 45, and a new finding falls out at 19

*Extracted by `extract_memory_reports.py` into
`memory-reports-worksheet-20260731.json`. Deterministic; the extractor searches
only the runner's own output, never the prompt — the dispatch template itself
instructs runners to report memory usage, so a naive search matches the prompt
echo and inflates the count.*

Of the 121 reports, only 45 correspond to a dispatch where anything actually
surfaced. **You cannot reject a memory that was never offered**, so the strata
support different claims:

| stratum | n | supports |
|---|---|---|
| Memories surfaced; report discusses them | **45** | the rejection taxonomy proper |
| "No memories surfaced" — honest non-use of an empty set | 57 | the recall-failure narrative (§2.2), **not** the taxonomy |
| Pattern surfaced, but **no reviewed memory attachments behind it** | 19 | a distinct finding — see below |

**So V2-3's headline corpus is n = 45.** That is the third downward revision of
this number in one session (129 → 121 → 45), and each step came from checking
rather than inheriting. 45 is still a real corpus — 45 coded rejections with
substantive grounds is more than the agent-memory literature typically
reports — but it must be stated as 45.

**The 19 are a finding V1 did not have.** These runners reasoned about a
*pattern* that reached them while the memory tier behind it was empty:

> "`math-informal/pass-to-a-subsequence` (pattern library): conceptually
> relevant but **had no reviewed memory attachments**. The approach it
> suggested was subsumed by Mathlib's existing `exists_seq_tendsto_ae`."

> "`math-formalization/hilbert-projection-properties` (pattern library):
> conceptually relevant but **no reviewed attachments were used directly**."

This is not a rejection and not a recall failure. It is an **attachment-coverage
gap**: the pattern tier is doing its job — surfacing a conceptually apt handle —
and the memory tier is empty behind it. That is a direct, independent
measurement of the two-tier architecture's weakest link, from the runner's own
words rather than from topology. It belongs in V2 as its own subsection.

#### 3.2.1c The corpus indexes ADVICE but not ARTIFACTS — and the channel cannot see it

*From claude-9, 2026-07-31, passed on by Joe. Tested against our own corpus by
claude-2; the test result makes the point sharper.*

**The observation.** Every `e-codexpilot-*` memory is a pattern, a caution, a
route, or a stopping rule — prose that shapes how a runner proceeds. None is a
**proved lemma you can import**. So a runner can be correctly told "use the L²
translation continuity approach" and still be unable to obtain
`eLpNorm_translation_tendsto`. Retrieval surfaces the *idea* and cannot surface
the *artifact*.

Five instances observed in one day, two of them **created by the loop that day
and stranded on arrival**: a94J04 proved `eLpNorm_translation_tendsto` while
a96A04 sat blocked on exactly that; a97A07 proved a generic
`[0,1]`-parametrisation ↔ `circleIntegral` bridge, unreachable; a97A06 blocked
on a Dirichlet sinc integral that a02J05 proves axiom-clean with a
16-theorem library.

**The natural experiment.** `ConstructionTargets` and `YoungL2` had no
`[[lean_lib]]` stanza, so their proved lemmas were off the module path
entirely. The stanza was added at 03:52; **within 80 minutes two different
problems consumed it** (a96A04 closed `heatConv_L2_le`; a94J04's
`poissonConv_L2_contraction` closed on a re-dispatch that named it). Same
repository, same runners, one build-system change — a measurement of what
*reachability alone* is worth with no retrieval-quality confound.

*Limits, stated by claude-9 and preserved: five instances noticed in passing is
not a rate, and it is one corpus and one runner model.*

**CORRECTION to claude-2's first tempering (claude-9, verified both ways).** I
wrote that both consumptions were assisted. That is right for a94J04 and
**wrong for a96A04**: that runner's own arm-(a) query
`heatConv|heatKernel|convolution_L2_contraction` found
`YoungL2.convolution_L2_contraction_of_probability_kernel`, and the string
`YoungL2` appears nowhere in a96A04's queue row at HEAD, so it was not in the
dispatch packet. **One of the two consumptions was unaided discovery once the
module became reachable** — for a96A04, reachability was the *only* blocker.

**And the nuance is worse for us, not better.** It was found by **repository
grep**, in the runner's own search arm — *not* by memory recall, which
surfaced nothing relevant for that dispatch. So making the artifact reachable
let a **grep-based channel** succeed while the retrieval system remained unable
to offer it. **The runner routed around the memory system.**

Two consequences, and V2 should carry both.

**(i) "Reachable" and "retrievable" are two axes, not one.** Fixing
reachability did not make the memory system find the lemma; it let a *different
channel* find it. The two failures are independent, and repairing the first
exposed the second.

**(ii) An accidental baseline comparison — but the scoreboard needs restating
(claude-9's objection, accepted).** My first framing was "grep beat recall on a
live dispatch". claude-9 rightly objected that this is *a fair comparison of
outcomes and an unfair one of inputs*: grep searched the repository's full
text, while recall can only offer what the corpus has been given.

That is correct, and it means the framing was wrong rather than merely tilted.
**V1 §1.1 never claims recall is a better search engine** — it claims a memory
should arrive *unprompted at the decision point*, where search requires the
agent to already know what to look for. Findability is the wrong scoreboard;
**unprompted arrival** is the right one.

Restated on the correct scoreboard, the instance still counts against us, and
more precisely: **recall was absent where it should have been present, and a
cheaper channel covered for it.** a96A04 was a dispatch whose terrain the
corpus demonstrably covered — the lemma existed — and recall surfaced nothing
relevant. That claim is immune to the inputs objection, because it is about
presence, not about who searches better. n = 1, stated as such, and it belongs
in V2's results rather than a subsection.

As claude-9 puts it: the input asymmetry *is the reason the two axes exist*.

**What I found testing it against our own data — and why it matters.** I
searched all 121 memory-usage reports for this mode (not-importable /
not-in-scope / re-proved from scratch / module-path language). **Essentially
nothing: 2 weak hits, and the one inspectable example is a false positive** (a
duplicated `have` line in Lean source, not memory reuse).

That absence is not evidence against the observation. It is evidence that
**the observation channel cannot see this failure mode by construction.**
Every receipt field is closed over the *offered* set — `surfaced-ids`,
`used-ids`, `unused-ids`, `rejected-ids`, `rejection-reasons`,
`inclusion-reasons`, `surfacing-via`. A lemma that exists in another problem's
file and was never offered appears in **no** receipt field, so no amount of
receipt analysis will ever measure it.

This is one level worse than V1 §5.5(i)'s "missing observations are not
negative observations". These are not missing observations; they are
**unobservable** ones. The reason only five were noticed in passing is that
noticing in passing is the *only* available detector.

**The structural claim this licenses — a ladder of emptiness.** Two rungs, now
measured at different layers:

| rung | what surfaced | what was behind it | n |
|---|---|---|---|
| 1 | a pattern | **no reviewed memory attachments** | **19** (§3.2.1b) |
| 2 | a memory naming an artifact | **artifact unreachable from the consumer** | 5 (observed in passing) |

Same shape at successive layers: the retrieval tier does its job and the thing
behind it is empty. Rung 1 we can count because patterns and attachments are
both in the store. Rung 2 we cannot, for the reason above.

**And it proposes an axis orthogonal to the trust boundaries.** V1 §2.3 has
four boundaries and §3.3 adds a fifth (load-bearing). **Reachability is none of
them**: a memory can be reviewed, warranted, attributed, witnessed *and*
load-bearing, and still be unusable because the artifact it names is not
obtainable in the consumer's context. Warrant governs whether a memory *may*
conduct; reachability governs whether its referent *can be had*. They are
independent, and only the first is currently modelled.

**Consequence for V2 and V3.** V2 should state the ladder and the axis — both
are architecture, supportable from data in hand plus a clearly-labelled
five-instance observation. **Measuring rung 2 needs a different instrument, not
better ranking**: cross-match blocked targets against lemmas proved elsewhere
in the corpus, which is claude-9's proposed count and which the receipts
structurally cannot answer. That belongs in §6.3 as a V3 instrumentation item.

#### 3.2.2 The lane confound — this affects the Zai generality plan

The 7 non-empty rejection-reason vectors break down by author as: **zai-7 (3),
zai-6 (1), claude-6 (2), claude-9 (1)**.

So **4 of 7 come from 11 zai receipts (36%)**, against **1 of 213 from the
claude-9-supervised codex run (0.5%)** — a ~70× difference in how often the
structured field is populated, on a small but striking sample. Inspected, the
zai receipts are well-formed: `:status :outcome-attached`, 3 surfaced ids, 1
used, 2 rejected, each rejection carrying `{:memory-id … :reason …}`.

**Consequence for V3's generality check.** Re-testing the taxonomy with Zai
would compare a lane that *records structured per-memory reasons* against a
lane that *records prose*. Any difference in the resulting categories is then
confounded between **model reasoning** and **lane instrumentation**, and the
confound points the wrong way — the better-instrumented lane is also the
different model.

To get a clean generality result, either (a) run Zai through the *same*
receipt-writing path the codex lane uses, or (b) code both lanes from the raw
job results and ignore the structured fields entirely. **(b) is cheaper and
available now**; (a) is better and needs the lane repair first. Recommend (b)
for V2 and (a) for V3.

**Two scoping constraints, from claude-9, to be stated in the paper rather
than discovered by a reviewer:**

1. *These are solver self-reports, not ground truth.* The taxonomy is of
   **stated grounds for declining**, not of actual irrelevance. Nothing
   validates a "rejected" label — a wrongly-rejected memory looks identical to
   a correctly-rejected one — and the reasons are produced by the same model
   that made the decision, so they are partly post-hoc rationalisation. The
   `e9d008be` case (§3.3) is the mirror image on the "used" side.
2. *It is one model.* All 129 come from codex runners (`codex-7` 54,
   `codex-6` 33 in claude-9's sample; `runner-model :codex` throughout). This
   is **codex's** rejection behaviour, not a general property of solvers, and
   it belongs in the title or first paragraph.

### 3.2.1 Raw job results — captured, and they were the deadline item

claude-9 identified the one item in this exchange with a clock on it: the
runner's **full verbatim output** is retrievable from
`GET /api/alpha/invoke/jobs/<id>` and is richer than the receipts — route,
search audit across all three arms, error→fix log, and the memory-usage
section with per-memory reasons *as written*. Retention is unknown, and an
expired job is indistinguishable from a mistyped id through that endpoint.

**Captured 2026-07-31 by claude-2 before proceeding:** all 130 distinct
job-ids from the frozen receipts export, into
`data/evidence/holes/labs/M-memory-retrieval/job-results-20260731/` — 130 files, 1.4 MB,
2.8 KB–87 KB, none truncated, zero fetch failures. (One file matches
`:error`; inspection shows 21 occurrences inside the runner's own error→fix
log, not a fetch fault.)

V2-3's n=129 now rests on disk rather than on server retention.

### 3.2.3 REQUIRED V2 SECTION — the use vocabulary is the wrong shape

*Seven use-modes catalogued by claude-9 from one day's receipts, 2026-07-31.
This is the companion to the rejection taxonomy: that codes why runners
**decline**; this codes what a memory **did** when used.*

| # | mode | instance |
|---|---|---|
| 1 | **Interface translation** — connect a file's own predicates to Mathlib's | a95J05: bridged strict-threshold `TendstoInMeasureZero` to Mathlib's closed-threshold `TendstoInMeasure`, which had blocked every API call. *Most common by some distance.* |
| 2 | **Proof step** — supply mathematics used directly | LemniscateComponents: the component bound. *Rarer than (1).* |
| 3 | **Strategy selection against the problem's own hint** | a01A10: source TeX says "use a contour integral"; the memory steered to specialising Mathlib's Gaussian transform. It was right. |
| 4 | **Work removal** — make the proof shorter by deleting a step | a93A02: "return to the original functions before Vitali" eliminated an unnecessary uniform-integrability proof. |
| 5 | **Proof architecture** — fix the decomposition before any step is attempted | a96A02: representation-first guidance produced five lemmas *before* either global theorem. One memory shaped a 484-line proof. |
| 6 | **Risk-ordering / triage** — attack the riskiest interface first | a95J04: probing the highest-risk assembly edge exposed an invalid meromorphicity hypothesis — **ground control's own missed defect**. |
| 7 | **Stopping rule** — a constraint against doing work | a95A02: bounded descent into endpoint infrastructure. Attempts 1–4 repeatedly added exactly that machinery; attempt 5 converged. |

**The claim this licenses.** Modes **3–7 are non-substitutive**: none supplies
mathematics. They change what the runner *does*, or stops doing. Our recorded
vocabulary is a single binary `used-ids`, which is **substitutive** — it asks
whether the memory supplied the answer. **On that vocabulary five of seven
modes are invisible**, and a memory that saved a 484-line proof from the wrong
decomposition scores identically to one that supplied a lemma.

So the deficiency is not only that use-rate overstates (§3.3); it is that the
use vocabulary is **the wrong shape** — substitutive where most real use is
regulative. V2 should say so, and V3's load-bearing field (§6.3 item 4) should
be typed by mode rather than boolean.

Mode 6 is worth flagging separately: the memory corrected **ground control**,
not the runner. Memory acting on the operator rather than the actor is outside
every model in V1.

**Artifact with resolvable ids**:
`data/evidence/holes/labs/M-codex-sorry-loop/memory-use-modes-20260731.json` — **15 evidence
rows over 14 distinct receipts**, each with problem id, full receipt uuid and a
note on what the memory did. Caveats live in a `caveats` field inside the file
so they cannot be separated from the data.

*Verified by claude-2: all 14 distinct ids resolve against futon1b at :7073,
14/14, zero failures. The 15-vs-14 gap is explained below and is not a
duplicate.*

**Join warning — the relation is many-to-many.** a95A02's receipt
`f5f2e8ec-76bb-48bc-9e38-838dca6e1d33` is cited under **both** mode 6 and mode
7: one dispatch used two memories, one risk-ordering and one stopping rule, and
the receipt records both. Any coding scheme assuming one mode per receipt
breaks here. Note also that the use-mode coding is **per receipt** while the
rejection coding (§4/V2-3) is **per decline-mention** — different units, and
they must not be summed.

**The operator-facing case is machine-checked, not merely reported.** In
a95J04 (mode 6) the memory acted on **ground control**, not the runner:
probing the risky edge first exposed a defective `hf_mero` hypothesis that
claude-9 had missed when repairing that statement — a `(hR : True)` placeholder
was fixed while the *other* hypotheses went unchecked. The runner then proved
**axiom-clean that every continuous function satisfies `hf_mero`**, which is
what made the defect undeniable. So V1 has no model for this and the evidence
for it is a compiler-checked refutation, not a narrative.

*Caveats, claude-9's, preserved and corrected by them: one corpus, one runner
model (all codex), one day. **The citable n is 15 evidence rows, not the
"perhaps thirty" first reported** — thirty was an estimate of total memory-use
events observed, not of coded instances. Seven modes from fifteen rows is the
coding density. These are categories, not validated frequencies, and the tail
is certainly not exhausted.*

### 3.3 The "used ≠ used well" gap

claude-9 found a case (receipt `e9d008be`) where a memory was surfaced,
correctly used, and still produced the wrong decision — it carried a fact
without its consequent, and the run burned a slot on an unnecessary frontier.
`used-ids` scores that as a success.

This extends V1 §2.3's trust-boundary argument with a fifth boundary the
paper does not currently name:

| Boundary | Establishes |
|---|---|
| Review | the edge is well-formed |
| Warrant | evidence supports the content |
| Attribution | the solver says it used the memory |
| **Load-bearing** | **the memory changed the outcome for the better** |
| Witness | a third party confirms the result |

V2 must either measure the load-bearing label or state explicitly that any
reported use-rate overstates. Recommend: measure it on a subsample, state the
gap in the abstract.

---

## 4. Proposed V2 experiment programme

All of these run against **data already in hand**. None waits on dispatch
volume.

| # | Experiment | Input | Gate | Owner |
|---|---|---|---|---|
| **V2-1** | Channel re-audit, before/after | done — `observation-channel-audit-20260731.edn` | none | claude-2 ✓ |
| **V2-2** | Timeout mechanism + bias characterisation | 129 offered halves, receipt `4f7eeadd` | none | Codex |
| **V2-3** | Rejection-reason taxonomy | 129 rejection-reasons + `job-results-20260731/` | code against claude-9's pre-registered 5 categories first | Codex, claude-9 reviews |
| **V2-4** | Ψ-v2 replay at n=20 | 20 metric-bearing rows | report honestly, expect per-coefficient abstention | Codex |
| **V2-5** | D_state sweep at scale | 73 problems w/ receipts (vs 2 in V1) | current graph, not dispatch-time — state the limitation | Codex |
| **V2-6** | Load-bearing subsample | ~20 receipts, manual | claude-9 + claude-2 adjudicate | joint |
| **V2-7** | Floor sensitivity (multi-seed) | synthetic | none — carried over from V1 §8.2.2 E4 | Codex |
| **V2-8** | `:witness-status` repair + re-audit | `warrant_audit.py` | repair must land first | Codex |

**V2-3 is the headline.** A taxonomy of *why reviewed memories were declined
by a working solver*, at n=129 with free-text grounds, is a contribution no
other agent-memory paper has the data to make.

**Pre-registered category set** (claude-9, from supervising the run, recorded
here *before* coding begins so it cannot be fitted to the argument). All five
were observed live:

1. **Topical mismatch** — different subject entirely ("unrelated dimension and
   integral", "unrelated problem class").
2. **Scope mismatch** — right area, wrong sub-object ("concerns *parameterized*
   Laplace transforms"; "no *compact-support* Laplace transform here").
3. **Subsumption** — relevant but already handled ("the imported contraction
   theorem handles kernel integrability internally"). A substantive
   mathematical judgement, not a topical one.
4. **Stage mismatch** — relevant, but to a *later* target ("relevant only to
   the later a.e. target"). The *considered-but-declined* category, and the one
   that evidences discrimination rather than matching.
5. **Precondition absent** — the memory's trigger never fired ("no instance
   diamond arose").

**PRE-REGISTRATION AMENDMENT (claude-9, 2026-07-31, before coding begins).**
A sixth category, added *now* rather than discovered later, so it cannot be
fitted to the argument:

6. **Relevance without applicability** — the memory names the right shape but
   lacks the usable form. Five instances observed: *"conceptually matches the
   remaining density step, but supplies no interval-indicator density
   construction"*; *"Mathlib's direct dense-range inner-product lemma closed
   the analogous step"*. Distinct from **scope mismatch** (2), which is right
   area / wrong sub-object: this is right *content*, unusable *form*.

   **The scoring consequence is the sharp part**: metric-3 scores these
   identically to genuinely unrelated memories. If the metric cannot separate
   "right shape, wrong form" from "unrelated", it is discarding real signal —
   and a retrieval system optimised against it would be optimised to conflate
   them.

**Coding protocol**: code all 129 against these five first; record the
unclassifiable residue honestly; only then decide whether to split or merge.
A sixth category emerging *from the residue* is a finding. A sixth category
emerging because five did not fit the argument is fitting, and the distinction
is the reason the set is registered here.

**V2-5 carries a mandatory caveat**: without dispatch-time snapshots it
measures the *current* graph, so it is a structural-sensitivity result, not a
historical replay. Say so in the caption, not just the limitations section.

## 5. Proposed V2 identity

V1's identity was *"architecture and its measurement instrument, with a
pre-repair baseline."* V2 should become:

> **A production agent-memory system measured end to end: what a
> warrant-disciplined retrieval loop actually delivers over 129 real
> theorem-proving dispatches, why 45% of its recalls never complete, and what
> a working solver says when it declines the memories it is offered.**

That is honest, it is supported by data in hand, and it is more interesting
than the ablation V1 was holding the paper open for. The hybrid-ablation
question (V1 RQ1) moves explicitly to V3 with its blocker named (§6.1,
V3-C1).

**V2 is the artifact shown to Rob** (Joe, 2026-07-31), conditional on carrying
the V3 plan at §6. Two consequences for how it must be written: every claim
has to stand without the ledger as context, since an external reader will not
have it; and the codex-only scoping constraint (§3.2) belongs in the title or
first paragraph, not the limitations section.

### 5.1 REQUIRED V2 SECTION — the memory system as a service, and its lane clients

*Flagged by Joe 2026-07-31 as important enough to belong in the paper's
architecture rather than in a WM subsection. V1 is frozen, so this is
specified here as V2 content. It should sit in V2's §2 (deployed system),
extending V1 §2.3's trust-boundary table rather than replacing it.*

**The structure.** The memory system is a **service**. The War Machine, the
codex/APM lane, and the zai lane are all **clients** of it: each calls recall,
and each conforms to a receipt contract the memory system owns
(`futon2/aif/memory_contract.clj`).

**The asymmetry that makes the architecture work.** The memory system depends
on each client's *domain* for its witness, because **it cannot witness its own
outcomes** — a memory system that certifies its own results is the
self-report contamination of §1.1, committed structurally. So a lane is not
just a client; it is a **(client, witness-source) pair**:

| lane | client — supplies attribution or selection | witness source — supplies the outcome |
|---|---|---|
| APM | codex / zai runners | the Lean compiler |
| WM | WM strategic selection | WM's external adjudicator |
| *(any future lane)* | *its own actor* | *an independent party in its domain* |

The memory system's inability to supply its own witness is therefore **a
design feature, not a gap**. It is what forces the witness to be genuinely
external in every lane.

**What follows, and why it is the interesting claim.** Decision-keying is not
a property of any one lane. It is the **memory system's interface requirement
on all clients**: *any lane whose outcomes are to count must supply a
decision-keyed independent check.* Two receipt kinds
(`:algorithmic-selection`, `:agent-attribution`) join against one witness kind
via the decision id.

This generalises V1's four trust boundaries (§2.3) in two directions at once:

1. It adds the **fifth boundary** — *load-bearing*, between attribution and
   witness (§3.3) — which the `e9d008be` case showed is not hypothetical.
2. It explains **which party owns which boundary**: review and warrant are the
   memory system's; attribution is the client's; witness belongs to an
   independent party inside the client's domain. No party owns two adjacent
   boundaries, which is the structural statement of author ≠ reviewer.

**Presentational note.** This section is the paper's cleanest general claim,
and it is stronger than the deployment that instantiates it: the structure is
supported by two lanes, only one of which (APM) has produced substantial data.
State it as an architecture with two instances, not as a validated
generalisation — the same discipline V1 applied to its own frame in §3.1.

## 6. The V3 plan

*Joe, 2026-07-31: V2 may be shown to Rob **provided it carries a plan for
V3**. This section is therefore written to be read by an external reader, not
just as an internal checklist. It states what V3 claims, what gates each
claim, and in what order the work runs.*

### 6.1 What V3 is for

V2 is an honest account of a system measured through an instrument that was
itself partly broken. **V3 is the effectiveness paper V1 was holding itself
open for**: does warrant-disciplined hybrid retrieval actually retrieve
better, and do receipt-updated coefficients actually rank better?

Two claims, no more:

- **V3-C1 (hybrid).** Combined content and pattern retrieval improves relevant
  recall at a fixed budget over either route alone.
  *Gate:* the frozen benchmark of §4.2 — n queries over ≥ 15 theorem families,
  ≤ 3 per family, paired analysis on discordant pairs, n sized from a 10–12
  query pilot measuring the discordance rate.
- **V3-C2 (adaptation).** Receipt-updated coefficients improve ranking on
  later observations against no-update and scalar-update baselines.
  *Gate:* `n ≥ 20` independently witnessed outcomes **per coefficient**,
  evaluated chronologically rather than leave-one-out. Partial promotion —
  "top *k* coefficients promoted, tail abstained" — is a legitimate result,
  not a fudge; the harness's per-coefficient abstention is designed for it.

If the data do not support a claim, V3 reports the null against the
preregistered evaluation and does not re-cut the analysis. That rule is what
V1's §5.2 and §5.3 already demonstrate we will actually follow.

### 6.2 The critical path, in order

Each step gates the next. Steps 1–3 are prerequisites and none is currently in
progress except where noted.

1. **Fix the recall budget** *(owner: claude-9, assigned by Joe
   2026-07-31)*. See §6.3. Until this lands, ~45% of any new dispatch is lost
   to infrastructure and the marginal problem is worth ~half a sample.
2. **Validate the fix on 2–3 throwaway dispatches** with a measured drop in
   `:timeout` rate — *before* committing the ~48 remaining clean problems.
   "The fix landed" is not "recall completes."
3. **Census the 71 un-queued problems into rows** (statement hints, `:line`,
   kind). Independent of step 1, can run in parallel. Known failure modes:
   stale hints silently repointing at a neighbouring target, empty `:line`
   lists throwing in the selector, two rows sharing one file.
4. **Repair the projection result-bound guard** (§2.3, Defect 2) and re-test
   whether dispatch-time graph state is reconstructible. This is the cheap
   shot at Experiment 0 — a one-file change in our own code — and it should be
   attempted before anyone builds per-dispatch snapshotting.
5. **Dispatch the ~48** with instrumentation from §6.4 in place.
6. **Build the frozen benchmark**, then run V3-C1 and V3-C2.

**The honest risk on V3-C2**: even with all ~48 dispatched, per-coefficient
promotion likely clears for only the top handful of patterns. If V3-C2 cannot
be answered at adequate n, it should be reported as a measured failure
boundary rather than held open indefinitely — the same discipline V1 applied
to the spectral criterion.

### 6.3 Instrumentation V3 requires (unrecoverable if missed)

1. **Repair `system-as-of` on the evidence route, or reject it explicitly**
   (§2.3, Defect 1; `futon1b/holes/DEFECT-bitemporal-as-of-two-routes.md`). A
   parameter that silently returns present-time data is worse than one that
   errors. Until repaired, no as-of-based claim.
2. **Dispatch-time seed-set capture — now the single binding item for RQ1.**
   §2.3.2 established that graph state *is* reconstructible but the text index
   is current-only, so the retrieval seed cannot be replayed. Capturing, per
   dispatch, the text-search candidate set (ids + scores) alongside the
   existing `surfaced-ids` would supply the missing denominator and make
   recall@k computable. This is cheap, and it is the difference between RQ1
   being answerable in V3 and not. A revision number and `index-as-of` stamp
   per dispatch should ride along with it.
3. **Restructure the recall budget — and do not size it to a round number.**
   Per-subject queries against a 30 s *total* budget cannot work at 9–16 s per
   query. Sized against claude-9's post-restart measurements (9.1–16.2 s, mean
   14.2 s) and `subjects_for` yielding up to 12 subjects:

   | common subjects | est. total | clears 120 s? |
   |---|---|---|
   | 2 | ~28 s | yes |
   | 6 | ~86 s | yes |
   | 8 | ~114 s | yes |
   | **12** | **~171 s** | **no** |

   So a 120 s budget would clear ~8 common subjects and still fail the worst
   rows — which are precisely the analysis-heavy ones the bias already falls
   hardest on. **The cheaper structural fix is to cap or rank `subjects_for`
   by term rarity**, since common terms cost the most and discriminate least;
   raising the budget alone treats the symptom. Either way, size from the
   measurement (~300 s if raising), not from a round number.
4. **A load-bearing outcome field**, distinct from `used-ids` (§3.3). Without
   it every use-rate V3 reports overstates, and the `e9d008be` case shows the
   gap is real rather than theoretical.
4b. **A reachability instrument — receipts structurally cannot supply this**
   (§3.2.1c). Cross-match blocked targets against results proved elsewhere in
   the corpus, to turn five passing observations into a rate. Every receipt
   field is closed over the *offered* set, so an artifact that was never
   offered is invisible to the entire observation channel no matter how it is
   analysed. This wants an index over proved artifacts (declaration name →
   module → reachability from a given problem), not a better ranker.
5. **A second runner model.** All 129 rejection-reasons come from codex
   runners. V3's rejection taxonomy is *codex's* behaviour until at least one
   other model is run against the same corpus. This is the cheapest available
   generality check and it needs no new problems — the same rows can be
   re-dispatched to a different runner.
6. **Corpus beyond APM.** With ~48 problems of headroom, V3-C2 needs a
   different source of witnessed outcomes to reach per-coefficient promotion
   at any breadth. Identifying that source is a Joe-level scoping decision and
   is the single biggest open question for V3.

### 6.4 The War Machine as a non-APM source — thin, and thin in a specific way

*Joe, 2026-07-31: there should be a non-APM source via the War Machine, which
can store memories, "although thin". Assessed below against what V3 actually
needs.*

**What exists.** Measured on a bounded 200-row attachment sample:

| domain | attachments | `:independently-witnessed` | `:self-asserted` |
|---|---|---|---|
| `:mathematics` | 172 | 125 | 38 (+9 no field) |
| `:war-machine` | **7** | 6 | 1 |

The machinery is wired, not hypothetical. `wm_memory.clj:67` `record-episode!`
writes WM episodes under `:domain :war-machine`;
`dark-candidate-projection` (`:85`) recalls by endpoint and emits a proper
`use-receipt` with `surfaced-memory-ids` and `used-memory-ids`; and
`wm/independent-phase4-checker` writes an external check
(`{:outcome :pass, :witness-status :independently-witnessed,
:checker "phase4 dark projection review"}`, tagged
`[:war-machine :external-check]`).

**Three gaps decide whether it is usable, and none of them is volume.**

1. **`used-ids` means something different here.** In WM it is derived from
   `(:candidates projection)` — *what the projection algorithm selected*. In
   the APM lane it is the solver's report of what it used. These are different
   signals: one is the system scoring itself, the other is an agent's
   attribution. **They must not be pooled into one corpus.** Doing so would be
   precisely the attribution/witness conflation that V1 §2.3 exists to
   prevent, committed by us rather than diagnosed by us.
2. ~~**The projection is dark.**~~ **RETRACTED 2026-07-31 — the projection is
   already live, and this is the most important correction in the document.**

   The original claim rested on `wm_memory.clj`'s own labels: `:status :dark`,
   `:live-ordering-changed? false`, docstring "a detached audit product".
   Those describe the **function's local behaviour** — it does not reorder its
   own output — **not the pipeline's**. codex-3 refused the packet on this
   ground and it verifies:

   - `strategic_cascade.clj:289` passes `dark-candidate-projection` as the
     `:query-step-fn` of `outer-frontier`, under a docstring that says
     *"Production-shaped dark wrapper using the Phase-4 WM query at **every
     step**"*;
   - `live_wm_selection.clj:326/366` turn that into `current-selection` /
     `validated-selection`;
   - `wm/scheduler.clj:321` resolves `current-selection` inside `tick!`;
   - `wm/runner_service.clj:100–105` injects `validated-selection` into every
     in-process click, its comment naming commit `919d975` as *"the
     bounded-autonomy boundary"*;
   - `transport/http.clj:6519` does the same for the HTTP bridge.

   Deliberately armed by `919d9755` "authorize bounded WM autonomy with
   delivery QA" (07-24), `b79773c8` "add single-flight in-process WM runner
   service" (07-26), `e89e776d` "share the bounded-autonomy selection
   validation across transports" (07-26) — all three verified present with
   matching messages.

   **So WM memory already influences live machine enactment, and this
   planning document described it as a detached audit product.** That is
   precisely the class of error the paper's own discipline exists to catch,
   committed by us, in the document proposing the discipline. It is recorded
   here rather than quietly fixed.

   Two consequences. The plan's "take the projection live" prerequisite is
   **already satisfied** — WM outcomes *are* caused by retrieval, so the
   V3-C2 objection in that direction falls away. And a default-off flag inside
   `wm_memory.clj` would have been **cosmetic**: a gate that does not gate,
   shipped as if it did. codex-3 declining to build it was correct.

   **P3 dissolves entirely — there is no consent decision to make
   (2026-07-31, Joe).** The packet asked what WM should do "when per-fold
   consent is absent", and codex-3 escalated that as a policy question. Joe
   rejected the framing as a loaded question and asked which part of
   `p4ng/main-2026.tex` was at issue. Checked:

   - **Arming is a normal per-batch operator act, not an exceptional
     condition.** The loop "recommends what to work on next and, *when armed*,
     acts on it" (`main-2026.tex:375`); one revolution runs "…the fold is
     escrowed as a deposit…; **the operator arms it**; the act-gate passes iff
     $S_{\mathrm{cascade}}>0 \wedge \Delta S_{\mathrm{coverage}}<0$"
     (`:719`). Unarmed is the *resting* state, and the system still
     recommends. There is no dilemma to resolve.
   - **The model already specifies the answer I posed as a trilemma**: "if
     either leg is absent, **the gate abstains rather than guesses**"
     (`:478`). Abstention is defined behaviour, not a policy choice.
   - **Consent here was given, and is evidenced.**
     `live_wm_selection.clj:304–324` records
     `:operator-confirmation-required? false` with an accompanying
     `:operator-decision-evidence-id` — standing consent granted at the
     boundary by `919d9755` "authorize bounded WM autonomy **with delivery
     QA**", bounded by `:delivery-qa {:required? true}`, 13 armed tripwires
     (R20), retained query bounds and witness/admissibility, and a
     phase-1–4 allow-list.
   - **The fail-closed case is already implemented.**
     `live_wm_selection.clj:299–303` throws *"bounded autonomy machine gates
     are incomplete"* when the gates do not hold.
   - **"Shadow-only with an older controller" is already forbidden as an
     automatic route.** The fallback exists — `:controller :current-additive`
     — but its mode is `:explicit-rollback-only`. That is exactly why the
     option smelled like the bypass Joe previously rejected: the model
     already refuses it as a silent path.
   - Note also `current-selection` "returns machine enactment authority. **It
     never executes the click**" (`:331–332`) — recommendation and execution
     are separated as `main-2026.tex:661` requires.

   **So the answer to "why would consent not have been given?" is: it was
   given.** No case of absent consent was observed; the question was
   constructed, and it violated an existing standing instruction not to reach
   for consent-gate framing reflexively. What survives from codex-3's refusal
   is P1 (the join key), P2 (semantic separation of
   `:projection-selected-ids` from `used-ids`), the finding that the 07-23
   check has no decision reference and cannot be retrofitted, and the
   recommendation that new external checks carry `{:ref/type :decision}`.
   **None of those needs a ruling; the packet can be recut without one.**
3. **Witness and memory-use are unlinked.** The phase-4 check carries an
   outcome and a *mission* subject but **no `:memory-use` block**, while the
   use-receipt is keyed by `decision-id` / `session-id`. There is no join key
   between them, so no (offered → used → witnessed) triple can be formed —
   which is the exact shape V3-C2 consumes.

**Recommendation.** WM is **not** a V3-C2 corpus at present, and writing more
WM memories would not make it one — the blockers are the join key and the dark
path, not the count. The enabling changes, in order: (a) add a join key
between the use-receipt and the external-check record; (b) take the projection
live so outcomes are caused by the retrieval. Only then does volume matter.

#### 6.4.2 Whose checker is it? — the boundary, settled (2026-07-31, Joe)

Joe asked whether the "separate checker identity" belongs to the War Machine
or to the memory system, noting that WM is substantially complete and its
description should not have to grow to cover a memory system still being
upgraded. **He is right, and my phrasing — "arrange a genuinely separate
checker identity", inherited from codex-3's recut recommendation — was
wrong.** It implied the memory system must stand up a new role. It must not,
and it need not.

**The checker is WM's, and it already exists.** External adjudication is
constitutive of the WM loop, not an accessory to it: "the flown fold is
adjudicated **externally** (tests, gates, refusal censuses, or a
pre-registered falsifiable target — *never the model's own narration*)"
(`main-2026.tex:719`), and "eleven successive runner-agents performed external
adjudication at the per-obligation grain with zero reviewer corrections"
(`:721`). The role is defined, staffed, and has run.

**The join key already exists on the WM side too.**
`live_wm_selection.clj:130` resolves the selection trace by
`strategic-decision-id` against `:decision-id`. Decision ids are already
first-class in the WM decision path — the memory receipt does not need a new
identifier, it needs the one WM already mints.

**And WM's decision record already acknowledges the memory seam.**
`live_recommendation.clj:87,98` carry `:strategic-memory strategic` and
`:newer-strategic-memory-influenced? true`, with `:actuation-owner
:downstream-act-gate`. So the boundary is a *field on a WM record*, not a
subsystem in either direction.

**Therefore the correct statement of what is missing** is not "arrange a
checker" but: **the external check should be written against the decision id
WM already has, instead of against a mission.** That is one field on WM's own
adjudication output. It explains why `e-phase4-wm-r15-check-20260723` is
unjoinable — `wm/independent-phase4-checker` keyed it to
`{:ref/type :mission …}` before decision-keying existed, so it is a WM-side
artifact predating the seam, not a memory-system omission.

**Scope consequence, affirming Joe's instinct.** The WM description stays as
written; the memory system *consumes* WM verdicts and must never mint its own.
That is not merely tidy layering — a memory system that witnesses its own
outcomes is grading its own homework, which is the self-report contamination
§1.1 exists to forbid. The memory system's inability to supply its own checker
is a feature of the design, and WM's independent adjudicator is exactly the
external witness the memory loop requires.

*Correction to an earlier report: codex-3 attributed `:strategic-memory` to
Futon2's `war-machine/judge`. It is in **futon3c**
(`aif/live_recommendation.clj`). The substance of codex-3's finding stands;
the location was misreported and I passed that on unchecked.*

#### 6.4.3 WM is a client — and that makes the witness contract general

*Joe, 2026-07-31: "if this is something that sits in the memory system or at
its interface, then the WM is just a client".*

Correct, with one asymmetry worth naming. WM is a **client** for recall and
for receipt-writing: it calls `recall-by-endpoint`, and it conforms to a
contract the memory system owns (`futon2/aif/memory_contract.clj`). The codex
and zai lanes are clients in exactly the same sense.

The asymmetry is that **the memory system depends on each client's domain for
its witness.** It cannot supply one itself — by design (§1.1). So every lane is
really a *(client, witness-source)* pair:

| lane | client (attribution / selection) | witness source |
|---|---|---|
| APM | codex / zai runners | the Lean compiler |
| WM | WM strategic selection | WM's external adjudicator |

That is the same structure §2.3's trust boundaries describe: attribution comes
from the client, witness from an independent party **within the client's
domain**.

**The consequence is that decision-keying is not a WM feature — it is the
memory system's interface requirement on all clients:** *any lane whose
outcomes are to count must supply a decision-keyed independent check.*

**And that means part of codex-6's implementation is in the wrong module.**
The split as built:

- `:decision` added to the shared ref-type vocabulary in
  `social/shapes.clj` — **correct, general**;
- but `decision-keyed-external-check-entry`,
  `record-decision-keyed-external-check!` and `witnessed-projection-triple`
  all live in `peripheral/wm_memory.clj`, with
  `:evidence/tags [:war-machine :external-check]` **hardcoded** at `:117`.

Under the client framing those belong on the memory system's shared surface,
with lane/domain as a parameter. As built, the zai lane cannot reuse the
writer — which is exactly what the V3 generality check needs (§3.2.2) — and
any future lane would duplicate it.

A second, smaller consequence: `witnessed-projection-triple` is named for
*projection*, WM's algorithmic-selection concept. The general contract has
**two** receipt kinds — `:algorithmic-selection` and `:agent-attribution` —
against **one** witness kind, so the triple-former should accept either.

*This is a design recommendation, not a verified defect: what codex-6 built is
correct and tested for the WM lane. The claim is that its **placement** should
follow from Joe's framing, and that promoting it is a small refactor that
directly serves V3's generality plan rather than new work.*

### 6.4.1 Correction: the correct implementation is neither lane — it already exists

*An earlier draft of this section claimed WM was "the reference implementation
of `:witness-status` done right", reading the 6-of-7 promotion split as
evidence that the field carries information. **That was wrong**, and the
correction improves the plan.*

`holes/CODEX-HANDOFF-live-wm-memory-selection-verify.md:139` (2026-07-23)
records: *"There is no proper review operation. Earlier live attachments were
promoted by **manually reposting hyperedges**."* The 7 WM attachments are
dated 2026-07-23. So their `:independently-witnessed` status was **not
earned** — it was hand-posted, and the 6/1 split evidences who reposted what,
not a working discipline. My stated caveat (that I had not verified the
promoted rows carry witness records) resolves in the unfavourable direction.

**But the correct implementation does exist, and it is better placed than
either lane.** `memory_lifecycle.clj:160` `review-attachment!` was built
2026-07-24 — the day after that handoff — with tests at
`test/futon3c/peripheral/memory_lifecycle_review_test.clj`. Its validator
(`validate-review!`, `:105–149`) enforces:

- review evidence must exist, and be typed `:memory` with claim-type
  `:observation` or `:challenge`;
- **the reviewer must not be the memory's author** (`:117–122`);
- the review's subject must name the exact memory, and its pattern set must
  match the attachment exactly;
- provenance and timestamp must be present;
- **an approval must state its supported witness status** (`:142–147`), and
  the resulting `witness-status` is read **from the review evidence body**
  (`:149`) — not from a lane, not from the author.

That is precisely the discipline V1 §2.3 found missing. So the situation is:

| | default | promotion |
|---|---|---|
| WM `record-episode!` | ✓ `:self-asserted` | ✗ (its 7 rows predate the operation; hand-posted) |
| Codex lane | ✗ lane-derived, cannot fail | ✗ bypasses review entirely |
| `review-attachment!` | — | ✓ evidence-backed, author ≠ reviewer enforced |

**This makes the §6.3 item 1 repair substantially cheaper than a port: it is
routing the codex lane through a shared operation that already exists and is
already tested**, not writing new review logic. It also means the paper's
finding sharpens — the discipline was not merely known to us, it was *built*
and then bypassed by the lane that produced the evidence.

*What remains unverified: whether the 7 WM rows would pass `review-attachment!`
if replayed through it. They should be re-reviewed rather than trusted.*

## 7. Division of labour and gates

Per the workspace handoff protocol: experiment harnesses are substantial
coding and go to Codex with a bell + park; the plan, review, and paper body
stay with the Claude owner. Gates for every dispatched slice: clj-kondo on
Clojure, `check-parens.el` on Lisp/Clojure, relevant tests, and a written
determinism check on any new audit artifact.

**Immediate next action once agreed:** V2-3 (rejection-reason taxonomy),
because it is the highest-value asset, needs no repairs to land first, and is
the section V2's identity is built on.

## 8. Decisions taken, and what remains open

### Settled by Joe, 2026-07-31

1. **V2 is the artifact shown to Rob**, conditional on it carrying a plan for
   V3. That plan is §6, written for an external reader. *V2's scope is
   therefore fixed: it must be self-contained and honest about what it does
   not establish, because it will be read by someone outside the stack.*
2. **The `as-of` defect is logged**, at
   `futon1b/holes/DEFECT-bitemporal-as-of-two-routes.md`. **It is not an XTDB
   bug** and must not be taken to James Henderson as one — both faults are
   futon1b application-layer, with named source lines (§2.3). The genuinely
   XTDB-facing questions (history retention depth; valid-time vs system-time
   semantics for this use case) are *unestablished* and should be asked as
   questions after Defect 2 is fixed, not reported as bugs on 2026-08-05.
3. **claude-9 owns the recall-budget fix.** Recorded as step 1 of the §6.2
   critical path.

### Still open

- **A non-APM source of witnessed outcomes** (§6.3 item 6). Joe proposed the
  War Machine; assessed at §6.4. Verdict: real machinery, 7 attachments, but
  blocked on a join key and a dark projection rather than on volume — so it
  is a *candidate* source requiring two specific changes, not an available
  one. Whether to make those changes is the open decision. If not, this
  remains the binding constraint on V3-C2.
- **Whether to attempt the projection-guard repair** (§6.2 step 4) before
  building per-dispatch snapshotting. Recommend yes — it is one file in our
  own code and may make Experiment 0 reconstructible retroactively.
- **Whether a second runner model is in scope for V3** (§6.3 item 5), which
  determines whether the rejection taxonomy generalises or stays codex-specific.
