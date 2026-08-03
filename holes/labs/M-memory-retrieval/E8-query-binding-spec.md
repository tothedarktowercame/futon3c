# E8 — is the retrieval query a binding constraint?

**Frozen data. No runners, no dispatches, no tokens.** Opened 2026-08-03 by
claude-12, on Joe's go-ahead.

## The question

`dispatch_with_recall.clj` builds the retrieval key as a bag of **at most four
frequency-ranked words** (`text-keywords` → stopword removal → frequency sort;
then `(take 4)` at line ~366, introduced `6d9c3c5f`, 2026-07-30).

V2 falsified two *lexical mechanisms* — term rarity and pairwise co-occurrence —
and inferred from their failure that the bottleneck lies past the lexical stage,
at the attachment layer (§5.2, explicitly labelled "Conjecture, not result").
Both falsified mechanisms concern **properties of terms**. Neither concerns the
**cardinality and selection rule of the query**. So a third lexical-stage
explanation survives V2's falsifications untested.

This matters because the V3 cohort dispatches through this builder, and
E-memory-resourcing-and-strategy §4 makes C1 shape the cohort's arms: if the
attachment layer is the bottleneck, the treatment is populated-graph vs
star-forest. **If the query is the binding constraint instead, populating the
graph yields a null for reasons unrelated to attachment — an expensive null,
paid for in fresh backlog problems.**

## Design

Known-item retrieval, holding store / ranking / projection fixed and varying
**only** query construction.

### Labelled cases

These were labelled *contemporaneously by the loop* in
`holes/labs/M-zai-learning-loop/cohort-2-ops-log.md`, not selected post hoc by
the analyst. Each is a documented miss with a named expected target.

| # | problem | expected target | contemporaneous diagnosis |
|---|---|---|---|
| 1 | a93A03 | `e-30e87097-f843-4341-81c0-a49ee7ce0ef4` (direction-scoped liminf) | S4: "NOT surfaced … v1.2 normalization firing but drowned by TeX fragments + packet boilerplate — **pollution not absence**" |
| 2 | a93J02 | that problem's own memories | S5: "recall-empty — own-terrain miss" |
| 3 | a96A03 | `weak-convergence-hilbert` pattern | "did NOT surface; Liouville pair surfaced instead" |
| 4 | lib-young-completion | missing-dependency pair `e-dfea2de9…` / `e-9751e537…` | S1b: "no memories surfaced (no problems/ dir → packet-only terms)" |
| 5 | a96A04 | `e-9751e537-f5b7-4c40-a857-0c0b699b93a2` | S6 (2026-08-03): predicted to surface under Ψ-weighting, did not |

All seven referenced memory ids were confirmed present in the store on
2026-08-03 (`type=memory` pull, 522 entries).

### Arms — query construction only

- **A (baseline)** — the shipped builder, `(take 4)`.
- **B (cardinality sweep)** — identical, with `(take 8)`, `(take 12)`, `(take 16)`.
- **C (structure-aware)** — terms drawn from mathematical identifiers in the
  Lean/TeX statement (declaration names, `Mathlib` module paths, operator names)
  rather than prose word frequency.
- **D (oracle)** — query built from the *expected target memory's own* name and
  body terms.

### Why D is the arm that matters

**D is the refutation arm.** If the expected target does not surface even when
queried with its own vocabulary, the failure is downstream of the query — in
attachment or projection — and V2 §5.2's conjecture is supported against my
hypothesis. Report D honestly whichever way it falls; a design that cannot come
out against the analyst is not an experiment.

### Required control: reachability vs retrievability

Before scoring any case, establish for each expected target whether it *could*
surface at all: does it carry a current `memory/assert` edge, with reviewed
attachment to a pattern the projection can reach? A target that is unreachable
under **any** query is a V2 §5.1 reachability failure, not a query failure, and
must be reported in that column rather than counted as a query miss.

### Measurements

Per (case × arm): whether the expected target appears in the surfaced set, and
at what rank; surfaced-set size; empty-recall (yes/no); Jaccard overlap of the
surfaced set against arm A.

Aggregate: known-item hit rate by arm; the A→B curve (does hit rate rise
monotonically with term count, and where does it saturate?); C vs best-B.

## Acceptance bar

- Deterministic and re-runnable: same inputs → byte-identical output. Freeze the
  result to `holes/labs/M-memory-retrieval/e8-query-binding-<date>.json` with a
  sha256, and record the store's entry count at read time (the store is live;
  say what you read).
- **No writes to the evidence store. No dispatches. No agent invocations.**
  Read-only throughout.
- Reuse the existing recall machinery (`futon3c.peripheral.memory-recall`,
  `futon3c.dispatch-with-recall`) rather than reimplementing retrieval — an
  ablation that reimplements the thing it ablates measures the reimplementation.
  Vary the query by parameterising the existing path.
- Do **not** modify the shipped `(take 4)` default. Add a parameter with the
  current behaviour as its default, or drive the internals from the analysis
  script.
- Report the reachability control separately from the hit rates.
- State every case where the labelled target is ambiguous (case 2 names "that
  problem's own memories" as a set, not an id — resolve it explicitly and say
  how).

## Gates

`clj-kondo` 0 errors 0 warnings on any Clojure touched; `futon4/dev/check-parens.el`;
existing test suites still green; no serving-JVM reload.

## Interpretation, written before the result

- Hit rate rises materially from A to B → **the query is binding**; C1's arm
  design needs revisiting before the cohort registers.
- Hit rate flat across A→B→C but D surfaces the targets → the query is not the
  binding constraint at the *margin* tested, though the vocabulary still is.
- D fails too → the bottleneck is downstream, **V2 §5.2 is supported**, and the
  populated-graph arm is the right treatment after all.

## Results — frozen run 2026-08-03

The run read 522 `:memory` entries. The canonical memory snapshot hash was
`b5fed62aecf0a9fa6cf4a149ec8a7581b96b3053f9fb22ebb259d39bda5572d0` and the
ground-control ranking-receipt snapshot hash was
`25860bf6bcc4a110ee892782c29c3fafe983c380230391f2ea8d1f3b0f5d6bbf`.
Both hashes were unchanged at the end of the run. The frozen result is
`e8-query-binding-20260803.json`, SHA-256
`07be2f39ee48aa38100aaf5ace7b70bcf2660de4681b0d565daedf510ca7b3a2`.

### Reachability control and per-case result

All five cases were scoreable. Every singleton or pair member had a current
reviewed mathematics-domain `memory/assert` path. The set-valued cases were
resolved before scoring:

- Case 2 means **any of the five** current reviewed memories directly attached
  to endpoint `a93J02`; the exact ids are frozen in the JSON.
- Case 3 means **any of the five current** memories attached to
  `math/weak-convergence-hilbert`. Two older members visible in raw projection
  rows were excluded because their edge state is superseded/retracted.
- Case 4 requires **both** named dependency memories; surfacing only one is a
  miss for the pair.

| case | reachable? | A | B8 | B12 | B16 | C | D |
|---|---:|---:|---:|---:|---:|---:|---:|
| a93A03 direction-scoped liminf | yes | miss | miss | miss | miss | miss | hit |
| a93J02 own-memory set | yes | hit | hit | hit | hit | hit | hit |
| a96A03 weak-convergence pattern | yes | hit | hit | hit | hit | hit | hit |
| lib-young missing-dependency pair | yes | miss | miss | miss | miss | miss | miss (1/2 surfaced) |
| a96A04 inventory memory | yes | miss | miss | miss | miss | miss | hit |

Known-item hit rates were therefore:

| arm | hits / 5 | rate |
|---|---:|---:|
| A, shipped take-4 | 2 | 40% |
| B8 | 2 | 40% |
| B12 | 2 | 40% |
| B16 | 2 | 40% |
| C, structure-aware | 2 | 40% |
| D, oracle vocabulary | 4 | 80% |

### Interpretation

The A→B curve is flat and saturates at baseline: **query cardinality is not a
binding constraint under the shipped recall path**. More precisely, every A
case hit the first three-term ladder rung, so merely increasing the builder's
four-term cap cannot affect retrieval: the ladder consumes the same first three
terms and stops. The B results mark this exact equivalence rather than pretending
to have made different backend calls.

C does not beat best-B (40% versus 40%) and changes no case verdict. Vocabulary
still matters, however: D recovers the a93A03 and a96A04 targets, taking the
rate to 80%. Thus the broad claim “the lexical stage is not binding” is too
strong; **cardinality is inert, while oracle vocabulary can be decisive**.

D also provides the preregistered refutation. For lib-young it surfaced
`e-9751e537-f5b7-4c40-a857-0c0b699b93a2` but not
`e-dfea2de9-8979-4f8f-9343-caabb48487e6`, although both passed the reachability
control. That member's failure is downstream of simple query vocabulary and
supports V2 §5.2's attachment/projection rival locally. It does not establish
that rival as the universal bottleneck, because D fixed two other misses.

No case was padded or dropped as unscoreable. Per-arm terms, surfaced ids,
ranks, set sizes, empty flags, Jaccard overlap with A, reachability evidence,
and input hashes are in the frozen JSON.

### Post-hoc refinement (claude-12, 2026-08-03) — what the 40% baseline is made of

Noticed while applying claude-10's absent-vs-drowned discriminator; checkable
from the frozen JSON, no re-run required.

| case | target kind | #expected | A | D |
|---|---|---:|---|---|
| 1 a93A03 | singleton | 1 | miss | **hit** |
| 2 a93J02 | set-valued (any of 5) | 5 | **hit** | hit |
| 3 a96A03 | set-valued (any of 5) | 5 | **hit** | hit |
| 4 lib-young | pair (both required) | 2 | miss | miss |
| 5 a96A04 | singleton | 1 | miss | **hit** |

**Both of arm A's hits are the set-valued cases.** Every case with a
specifically-named target — two singletons and one pair — misses under the
shipped query. So the shipped retrieval path did not once retrieve a *named*
target across these five cases; its 40% is carried entirely by cases where
any-of-five sufficed.

That splits something the aggregate rate conflates: retrieving *a relevant
memory* is not retrieving *the needed one*. Under oracle vocabulary both
singletons convert, which sharpens the vocabulary finding rather than softening
it — the terms decide whether a specific memory is reachable at all, and the
shipped term-selection never got there.

The pair case (4) remains the sole D-failure and remains unresolved between the
attachment and pollution residuals, pending the pre-cutoff-rank instrument.

## Ranked-candidate rerun — 2026-08-03

This later run supersedes only the freeze-status conclusion in the failed
determinism check immediately above.  That failed check remains recorded: it
identified the intermittent receipt fetch that this rerun now controls by
freezing the ranking inputs as the design requires.

The bounded rerun instruments the existing recall path immediately after its
final ranking step and before hydration and the `take 5` surfacing cutoff.  It
records the complete candidate vector produced by that stage, one-based
position, final ranking score, score kind, and the cutoff in effect.  This is
the full output of the ranking stage, not a whole-store enumeration: all
upstream search and projection limits remain exactly as shipped.

The run read 527 `:memory` entries.  The canonical memory snapshot hash was
`e9a1f680c0ae9666556feae1c97942df3ce030ff867707d784a4c5add3abe677`; the
ground-control ranking-receipt snapshot hash remained
`25860bf6bcc4a110ee892782c29c3fafe983c380230391f2ea8d1f3b0f5d6bbf`.
Both snapshots were unchanged within each run.  The frozen artifact is
`e8-query-binding-ranked-20260803.json`, SHA-256
`ff9b36823bbe52be207cd7b6469205ea04fe6cc8e223500daa9934e6facf8df1`.
Two complete reruns against those same hashes produced byte-identical output.

The first reproducibility check exposed two latency-dependent diagnostics: raw
FTS scores jittered below the rank-relevant precision, and a late receipt
fetch could time out and select fallback ordering.  The accepted instrument
therefore records the final ranking score rather than the auxiliary raw FTS
score and supplies the receipt snapshot already frozen at the start of the run
to the existing receipt-ranking function.  This holds ranking fixed; it does
not reimplement it.  Both behaviours are opt-in analysis parameters, and the
shipped four-term query default and ordinary dispatch result remain unchanged.

### The case-4 discriminator

Under case 4's D arm, the ranking stage produced seven candidates.  The target
`e-9751e537-f5b7-4c40-a857-0c0b699b93a2` was rank 1 (receipt-ranked score
1.5) and surfaced.  `e-dfea2de9-8979-4f8f-9343-caabb48487e6` was absent from
all seven, so its failure is **not cutoff pollution**: it is endpoint-relative
candidate starvation upstream of the final rank/cutoff.

The qualifier matters.  The same `e-dfea2de9…` memory is present under case
5/A at rank 1 (receipt-ranked score 1.2) and surfaces, via the direct `a96A04`
endpoint.  Thus the result says that the lib-young D query/projection did not
deliver this memory to ranking; it does not say that the memory is globally
unattached or globally unreachable.  This resolves the registered case-4
residual in favour of the endpoint-relative attachment/projection side of V2
§5.2, not pollution at the position-5 cutoff.

### Named targets under arm A

None of the specifically labelled targets was a near miss.  The case-1 target,
both case-4 pair members, and the case-5 target were each absent from arm A's
complete pre-cutoff candidate vector (null rank and score), rather than sitting
at rank 6 or below.  The original post-hoc finding therefore sharpens: arm A's
two hits are still entirely the any-of-five cases, and all named-target misses
occur before final ranking/cutoff.  Cutoff pollution does occur elsewhere in
the run (for example among non-winning members of the set-valued cases), but it
does not explain any named-target A miss or the case-4 D failure.

The aggregate arm outcomes are unchanged from the first run: A, B8, B12, B16,
and C each score 2/5 (40%), while D scores 4/5 (80%).

---

# Rank-instrumented rerun (2026-08-03) — reviewed by claude-12

Producing job `invoke-1785752055856-894-d82e96b4` **overran the ~30-min cap**
before committing. The instrument change landed (`b42b2db3`); the artifact was
left untracked on disk and is committed here after review. **Caveat: byte-level
determinism was NOT re-verified** — the producing job died before it could
re-run and bless its own output. sha256 recorded so any later re-run can check:
`adb67b134646ff3e7ed01a0b4d73c791b3b48cd066dbdd6a55a3447211cc06cc`. The original
run's artifact is untouched (`07be2f39…`, re-verified).

## Review checks

- **Ranks are genuinely pre-cutoff.** Arm A candidate depths are 19 / 16 / 13 /
  6 / 19 against a cutoff of 5, so the list extends past truncation and the
  absent-vs-drowned split is operable. (Arm C's depths are 5/5/3/17/3 — its
  queries generated almost no candidates at all, which is itself why C did not
  beat baseline: it was a *narrower* query, not a better one.)
- Shipped `default-query-term-limit 4` unchanged; zero store writes in the diff.
- clj-kondo 0/0; check-parens OK; focused suite 24 tests / 115 assertions with
  the 2 known pre-existing `live-dispatch-path-surfaces-a92j05-content-match`
  failures (independently reproduced at `dfe78c60` and its parent earlier today,
  so not a regression from this commit).

## The result: failure is at candidate generation, not at ranking

**Under arm A, not one specifically-named target is present in the candidate
list at all:**

| case | arm-A candidates | named target | present? |
|---|---:|---|---|
| 1 a93A03 | 19 | `e-30e87097…` | **no** |
| 4 lib-young | 6 | `e-9751e537…` and `e-dfea2de9…` | **no, neither** |
| 5 a96A04 | 19 | `e-9751e537…` | **no** |

Nineteen candidates, and the wanted memory is not among them. So this is not
"ranking too coarse" and not "cutoff too tight": **widening the cutoff from 5 to
19 would have changed nothing.** The failure is upstream of ranking entirely, at
candidate generation. Re-ranking, score tuning and cutoff widening cannot reach
a memory that was never a candidate.

**Scope correction (claude-10, 2026-08-03 — I stated this too broadly.)** That
pruning is **mode-scoped, not global**: it holds for the *named-target* failure
mode. Set-valued hits still route through ranking, and the Ψ observation below
(factor fires at 1.5, target did not surface at S6 dispatch) lives on that
side. A design that dropped ranking work entirely would over-read this result.
The named-vs-set-valued split is what keeps the two intervention families
addressed to the right failures — generation width for named targets, ranking
for set-valued ones.

**This corrects a contemporaneous diagnosis.** Case 1 is cohort-2's S4, logged
at the time as *"v1.2 normalization firing … but drowned by TeX fragments +
packet boilerplate — pollution not absence"*. The rank data says the opposite:
the target is absent from 19 candidates. It was **absence, not pollution**. The
loop's own real-time reading of its failure was wrong, and only the pre-cutoff
instrument could show it.

## The clean split, which differs by arm

- **Arm A absence** (all three named targets) → the shipped query never
  generated them as candidates. Query-side.
- **Arm D recovery** (cases 1 and 5 become hits) → oracle vocabulary reaches
  them, confirming those two failures were query-side.
- **Arm D absence** (case 4's `e-dfea2de9`, 7 candidates for 5 slots) → even its
  own vocabulary does not generate it. Downstream of the query —
  `endpoint-relative-candidate-absence`, and endpoint-relative because that same
  memory is rank 0 for a96A04 under the shipped four-term query.

The `:v1.2-receipt-ranked` Ψ factor is confirmed live: `e-9751e537` carries
`score 1.5, score-kind receipt-ranked`. It exists and fires; it simply did not
surface that memory at dispatch during S6.

---

## Determinism check — FAILS, for a reason worth more than the freeze

Re-ran the harness under an operator shell (no Agency cap) with the store
snapshot **byte-identical** to run 1 — same `memory-entry-count` 527, same
`memory-snapshot-sha256`, same `ranking-receipt-snapshot-sha256`. So inputs were
held; the outputs still differ:

    run 1  adb67b134646ff3e7ed01a0b4d73c791b3b48cd066dbdd6a55a3447211cc06cc
    run 2  ff9b36823bbe52be207cd7b6469205ea04fe6cc8e223500daa9934e6facf8df1

**The artifact is therefore NOT frozen and must not be cited as byte-reproducible.**

### What is stable, checked cell by cell

2 of 30 (case × arm) cells differ, both in case 5. Everything load-bearing is
identical across the two runs:

- all hit/miss verdicts,
- all pre-cutoff candidate counts (19/16/13/6/19 under arm A),
- all aggregate rates (A 40%, B8/B12/B16 40%, C 40%, D 80%),
- **case 4's classification — the one claude-10's delta v3 rests on**:
  `e-dfea2de9` `present-in-candidates: false`, `rank: null` in *both* runs.

The two differing cells are same-set, different-order surfaced lists.

So every claim reported from this experiment is **replicated across two
independent runs**, which is a better warrant than a single run plus a byte
freeze. The findings stand; the freeze does not.

### The substantive instability, which is a finding about the system

Case 5, arm D, `e-9751e537` — same rank (1), same presence, **different scoring
path**:

    run 1   score 1.0   score-kind "deterministic-base-order"
    run 2   score 1.5   score-kind "receipt-ranked"

**Receipt-ranking is intermittent.** The Ψ use-history factor fired in one run
and silently fell back to base ordering in the other, against an identical store
snapshot. The harness logs `retry after store busy` and the evidence endpoint
returns `:expensive-read-busy` under load, so the likely mechanism is that the
ranking-stats fetch fails under backpressure and degrades to unranked without
saying so. That is consistent with S6's live receipt recording
`receipt-ranking {enabled true, alpha 0.5, stats-found? false}`.

This qualifies a claim I made earlier today. "The Ψ factor is confirmed live" is
too strong: it fires *sometimes*. Any Ψ-dependent measurement is unreliable
until the fallback is made loud rather than silent — a ranker that quietly
degrades under load will produce arm differences that are really load
differences.

### Freeze status — RESOLVED (independent verification, claude-12)

codex-3's `aa213be8` freezes the ranking-receipt inputs from the run's initial
snapshot and records the final ranking score rather than the jittering auxiliary
FTS score. Ran the harness once more under an operator shell, independently of
the producing agent:

    committed / codex-3's claim   ff9b36823bbe52be207cd7b6469205ea04fe6cc8e223500daa9934e6facf8df1
    my independent run            ff9b36823bbe52be207cd7b6469205ea04fe6cc8e223500daa9934e6facf8df1

**REPRODUCED.** Three runs across two operators now agree byte-for-byte, so the
artifact is frozen in the strict sense, not merely replicated. The earlier
failed check above stands as recorded — it is what found the intermittent
receipt fetch, and deleting it would erase the evidence that produced the fix.

Note the scope: the *analysis harness* is now deterministic. The **production**
recall path still degrades silently under store load — that is SEQ-0.5, in
flight as `invoke-1785758489919-917-452f371d`. A frozen harness measures the
system reliably; it does not make the system reliable.

---

## a92J05 flake — ranking exonerated; I had over-read two failures as a property

claude-10 specified a paired-load protocol for testing whether the intermittent
`live-dispatch-path-surfaces-a92j05-content-match` was an unread alarm for the
ranking-degradation mechanism (v8). I declined the **induced-load** arm: the
store is the one serving JVM (I-0) and codex-12 was mid-flight on the attachment
exporter, so deliberately degrading a shared service to make a test fail would
have put someone else's work at risk. The test runs `:dry-run? true` — no bell,
no write — so the observational version was available for free.

**12 consecutive dry-run dispatches, read-only:**

    passes            12 / 12
    mode              :deterministic-base-order   × 12
    degraded?         false                       × 12
    reason            :stats-absent                × 12
    surfaced-set size 5                            × 12

**Ranking mode is constant for this query, so it cannot be what varies.** v8 via
a92J05 comes back negative: the flake is not ranking degradation. Recorded as a
negative result rather than left as a live hypothesis.

**And a correction to my own reporting.** Earlier today I told Joe and claude-10
that this pair was a *known pre-existing failure*, "independently reproduced at
`dfe78c60` and its parent". That was two samples taken while the machine was
busy — several codex lanes plus repeated E8 harness runs against the store. It
is load-dependent, and I read a stable property off two draws.

**Where the load actually bites, on current evidence.** The failing assertions
are that the packet contains the target id *and* `completed-with-memories` —
both of which fail together if recall returns nothing. All 12 passing runs show
a surfaced set of 5. The evidence endpoint returns
`{:ok false :error :expensive-read-busy}` under load, which a naive reader
counts as zero entries (noted earlier in the cohort-2 ops log). So the likely
mechanism is **candidate generation starving under store load**, not ranking
mode — which keeps claude-10's store-load intuition but relocates it one stage
upstream, to the same stage the pre-cutoff instrument already implicated for the
named-target misses.

Not claimed: confirming that needs a failing run's receipt, and getting one
requires the induced-load arm I declined. Left open rather than asserted.
