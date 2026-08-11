# E-ground-control-pass-to-zone

**Handover of APM/memory ground control from `claude-7` (Dionysus) to
`ams-claude-1` (Zone), 2026-08-10.**

Companion to **`futon6/README-apm-lean-ground-control.md`** (1147 lines) — the
note claude-7 was handed at the start of the 2026-08-09 session. That README
remains the operational reference: campaign structure, the freeze contract, the
review protocol, §4m's loop results. **Read it first.** This note covers only
what changed since, and what is still open.

The short version: the review campaign finished cleanly. Then, trying to restart
the proving lane, we found that the memory system it was supposed to use had
never been connected — and that most of its failures were *silent*.

---

## 1. Review campaign — CLOSED

91 → 0 over 26 autonomous iterations. `bridge_review.py` reports 169/169
reviewed. **94 verdict records across 90 distinct problems**; ~49% accepted.
Full detail in README §4m. Highlights that matter for what comes next:

- **Repackaging** dominates rejections — the goal handed back as a conjunct,
  curried, or delta-expanded. In nearly every case the closer's *prose* names
  the right mathematics while its Lean block restates the goal. This is a
  deliverable-format problem, not a capability gap.
- **18 problems block on one missing package** (algebraic topology). Mathlib
  computes *no* fundamental groups — not the torus, not even the circle.
  **`t91A05` names the cheapest unblock**: it needs only
  `Nontrivial (FundamentalGroup S¹ base)`, not `≅ ℤ`. That single item feeds
  no-retraction, Brouwer, and `t01A01`. **If one piece of the topology backlog
  is done first, it is this.**
- **Five cross-problem consolidations**, one lemma serving two problems each,
  four of five within a single prelim class: cyclic-number (b93J01+b01A02),
  sup-norm contraction (m02A06+m94A05), global Picard (m93J06+m00A05), Gaussian
  heat kernel (a96A04+m98J05 — **the only cross-class pair**), regular value
  theorem (t96J05+t97J05).
- **Corpus defect, recorded not applied**: `native_decide` in 10 files —
  `b00J01, b01A02, b94A01, b94J03, b96J02, b96J03, b97A01, b98A01, b99A02,
  t03J03`. Nine of ten are class `b`. `b96J04` shows kernel `decide` suffices.
  Frozen files are an immutable archive; do not edit them (README §4e).

---

## 2. What we found when we tried to restart the lane

This is the substance of the handover. Each item was verified, and several
corrected an earlier wrong conclusion of mine.

### 2.1 The lane was never connected to the memory system

`bridge_lane.py` has **zero** recall integration — it calls
`agency.dispatch_fn` directly. Across the entire 169-job campaign no memory was
ever injected and no recall receipt written. Memories that would have prevented
known re-derivation (`lemniscate-sublevel-components-inject-into-roots`,
`zeroCountInClosedBall-homotopy-invariant`) were in the store the whole time and
never reached a runner.

**The disconnection has a date.** Dionysus's `apm-driver` holds the original
campaign plus `ledger-zone-20260805.jsonl`: the campaign moved to Zone on
5 August, and the memory system stayed behind on Dionysus. Nothing failed
loudly, because recall was never invoked to fail.

### 2.2 Zone's substrate was pointed at a store that does not exist

`scripts/dev-zone-env` embedded futon1b on 7074 with
`FUTON1B_STORE_DIR=~/code/futon1b/ams-store` — **a directory that does not
exist**. Every text-search 500'd; dispatch got `store-unavailable` and reported
`recall-empty`. This masqueraded as a retrieval/ranking bug for most of a day.
Diagnosed by `ams-claude-1`. Fixed in `01d42841` (embed off, port 7073, store
`migration-store-21`).

**Dionysus was already correct** — no embedded futon1b, agency pointing at the
external server on 7073. Zone was the odd one out. Dionysus is the reference
shape; it needs no action.

### 2.3 Master could not boot from a clean checkout

`src/futon3c/agency/job_tree.clj` (623 lines, dated 6 Aug) **was never
committed**, while HEAD's `agent_pouch.clj` requires it. Latent because
Dionysus has the file untracked in its working tree. Any other checkout gets a
JVM that exits `status=2`. Found when `futon3c-zone.service` failed to restart.
Committed in `26308814`.

### 2.4 Three real query-construction fixes (verified)

Landed and independently re-verified, in `3d2051b7` and `d616c2db`:

1. **IDF ranking** — ranking was TF, so the rare technical term that *identifies*
   a problem sorted last and died to the 4-term cap. `t91A05` queried
   `unit ball boundary case` while discarding `retraction` (4/475 docs) and
   `functoriality` (0/475).
2. **Relevance floor + honest `recall-empty`** — recall used to fill its quota
   regardless of relevance. `recall-empty` had **never once fired** in the
   store's history. It is now emitted, and it is a *correct* answer.
3. **Diacritic folding** — the corpus writes `Hölder`/`Poincaré`/`Rouché`; the
   memory slugs are ASCII; they never met. **56 distinct diacritic terms, 622
   occurrences over 1429 files** — `Hölder` 166 (44 docs), `Poincaré` 87 (29),
   `Rouché` 52 (12), `Fréchet` 50 (20), `Möbius`, `Lindelöf`, `Arzelà`,
   `Künneth`, `Bézout`, `Grönwall`. These are the *named theorems* — i.e. the
   highest-IDF, most diagnostic terms in the whole corpus — and all were
   unmatchable.

### 2.5 Retrieval demonstrably works — once pointed at a live store

With `FUTON_SUBSTRATE_URL=http://127.0.0.1:7073`:

```
a94A09 -> completed-with-memories, ladder query "rouche",
          surfaced package-every-rouche-homotopy-slice-for-the-argument-principle
```

First genuine end-to-end recall success on a clean index. **Not yet proven
without the override** — see §4.

### 2.6 The A/B experiment ran, and cannot be evaluated

`ams-codex-1` (control) 199 jobs, `ams-codex-2` (memory arm) 196 jobs,
`ams-scribe-1` 82, over 8–9 Aug. Near-identical problem coverage (267 vs 269).
**But every seat-attributed ledger row has `sorries`, `outcome`, `build-exit`
and `declared` = null** — 1337 rows, all empty. The rows log *that work was
dispatched*, never what came of it. Work mix also differed sharply (355 pass-1
dispatches vs 11). Joe's ruling: **throw it out and redo later**; all
experiments are CLOSED until the apparatus is proven to do something at all.

---

## 3. The systemic finding

Four independent failures in one morning, all of the same shape:

| failure | why it was invisible |
|---|---|
| lane not wired to recall | recall was never invoked, so it never failed |
| substrate pointed at a missing dir | 500s swallowed into a typed `recall-empty` |
| `job_tree.clj` uncommitted | the one machine anyone ran had it locally |
| A/B outcome columns empty | jobs all reported `done`; `done` ≠ succeeded |

**We keep building loops whose failure mode is silence.** That is not a
retrieval problem, and it belongs in the memory-V3 writeup as its own finding.
The store already contains memories describing this exact hazard
(`diagnose-recall-empty-before-declaring-terrain-gap`,
`record-unrelated-recall-as-honest-non-use-and-a-terrain-gap`) — and I
rediscovered it the hard way anyway, which is itself evidence for the
findability thesis.

---

## 4. What is needed to close the gaps

In dependency order.

1. **Make the substrate reachable from a plain CLI dispatch.** *(in flight,
   `ams-claude-1`)* `dev-zone-env` configures the JVM it launches;
   `dispatch_with_recall.clj` is a separate CLI resolving the substrate from its
   own environment. A login shell never sources the profile, so the CLI may
   still hit the dead 7074. **This is the env a dispatched runner inherits**, so
   the fix must reach it, not just the service.
   *Acceptance:* `/tmp/c7_recall_test.sh` on Zone, no overrides →
   `a94A09` gives `completed-with-memories` + a rouche memory, **and `t91A05`
   stays `recall-empty`**. The store holds no π₁ memories; empty is the honest
   answer. If `t91A05` starts returning memories, the relevance floor has been
   broken — that is a regression, not an improvement.

2. **Prove recall changes runner behaviour** — a *real* dispatch, not
   `--dry-run`, where a runner cites a surfaced memory and does something
   different, with both receipt halves written (offered at dispatch, outcome at
   verification). Pick a problem where the memory is load-bearing; cohort-2's
   only recorded use event was explicitly "saved a search on a problem already
   solved", which is weaker than what we want to claim. **This is the bar for
   reopening experiments.**

3. **Wire `bridge_lane` through `dispatch_with_recall`.** Only worth doing after
   (1) — otherwise it generates receipts saying `store-unavailable`. Preserve
   the `fcntl` lock in `main()`; it fixes a real double-dispatch race
   (`a95J03`, dispatched twice 0.8s apart).

4. **Regenerate the DF corpus resource with folding applied.** Folding reached
   the query path but not corpus generation (hash byte-identical, `ac238b0`), so
   `holder`/`poincare`/`rouche` get `df=0 → idf 6.16` when `Hölder` is really in
   44/475 docs. Right answers, wrong reasons — must be fixed before any ranking
   claim enters a writeup.

5. **The AND-cliff**, for any caller sending genuine multi-term queries. FTS5's
   implicit connective is AND, so `belonging rouche` returns zero — those 16
   documents are disjoint from rouche's 262. Not today's bug, but live for the
   `--query-terms` analysis seam. The code comment at
   `dispatch_with_recall.clj:433` names the direction: several short queries
   unioned beats one conjunction.

6. **`futon3c/CLAUDE.md` invariant I-0** still says "One JVM Is Plenty" with
   `pgrep java` returning one PID as an explicit test, and futon1b embedded on
   7074. **The standard was revised 2026-08-10 (Joe): two JVMs are expected** —
   the fdev-started serving JVM and a standalone `futon1b-server`. The stale
   text stopped me mid-task; `ams-claude-1` is adding a temporary override.

7. **`native_decide` sweep** (§1) — unrelated to the above, still open.

---

## 5. Operational cautions for the successor

**Read "empty" as a fact about your query first.** I read a zero result as a
fact about the world three times in one session: `type=dispatch-recall-offered`
(the type does not exist — receipts are *tagged* `memory`/`memory-use`/
`memory-offered`, not typed); `type=hyperedge` (nearly concluded memories were
unwired); and `grep -oc`, which counts matching *lines* — Zone returns
single-line EDN, Dionysus multi-line, so identical data read as "1" vs "230".
That third one cost a needless outage on Joe's laptop. The ops log warned about
exactly this: *"a naive reader parses that as zero entries and reports an empty
result as a finding."*

**Do not use `ams-codex-2` as a scribe.** It holds `:memory-record` as the
**treatment arm** of a head-to-head, with `ams-codex-1` deliberately
unprovisioned as control (Joe, 2026-08-06; now committed in `f56780aa` so the
rationale cannot be lost to a checkout). The scribe is **`ams-scribe-1`**,
registered and idle on both agencies. Author ≠ actor: ground control does not
hold `memory_record`.

**Verify subagent conclusions.** Two confident, well-argued, wrong diagnoses in
one day: a "wiring/indexing failure" that was a unicode tokenizer bug, and an
"XTDB startup-readiness race" that was my own broken grep. Both stopped
correctly at the falsification boundary I had built into the packet — that
boundary is what made them safe. Keep putting one in.

**Split discovery from implementation**, and keep packets small. The
`a96J08`-shaped failure — prose that diagnoses correctly, Lean block that
restates the goal — recurs whenever a packet is large enough to build end-to-end
with no gate in the middle.

**Machine-specific config**: Zone uses `scripts/dev-zone-env`, not
`dev-laptop-env`. Zone's drift on the latter was pure staleness (missing the
`FUTON3C_POUCH_DEMUX` block, the fix for destroyed claude-14 replies) and has
been restored. `scripts/dev-laptop-env` still shows a 10-line deletion on
Zone — **do not commit that to master**; it would strip those lines from the
laptop.

**Chat-turn contamination is structural, not a Dionysus quirk.** The store
indexes operator conversation as evidence. On Dionysus a recall query returned
my own session transcript as its top hit and looked like success. On Zone 22 of
the top 30 FTS hits for `rouche` are `:coordination` noise — the proposal layer
filters them, so it does not win there. Any measurement taken on a store that
ingests operator chat is inflated by it. **Zone, with a clean index, is the
better measurement machine.**

---

## 6. Target frame

Longer-term work should run against
**`futon3c/holes/missions/M-memory-retrieval.md`**, aiming at a V3 memory
whitepaper (its WS6). But everything in §4 sits *below* those workstreams: a
dead store, an unbootable master, and an A/B with empty outcome columns are not
paper material. They are what has to be true before the paper's claims can be.

---

## Cross-references

- `futon6/README-apm-lean-ground-control.md` — the primary operational note (§4m = loop results)
- `futon3c/holes/missions/M-memory-retrieval.md` — workstreams, whitepaper (WS6)
- `algorithms/zai-learning-loop.md` — inner/outer loop, four mining lanes, receipts
- `futon3c/holes/labs/M-zai-learning-loop/cohort-2-ops-log.md` — the footguns, several of which I re-hit
- `futon3c/README-park.md` — park protocol (a bell without a park is a violation)
- Commits: `3d2051b7` `d616c2db` (query fixes) · `979b0bef` (campaign state) ·
  `f56780aa` (seat provisioning) · `a2f4ce32` `01d42841` (zone substrate) ·
  `26308814` (job_tree.clj)

---

> **CORRECTION (ams-claude-1, 2026-08-11): §4.1's t91A05 acceptance
> criterion is superseded.** The criterion conflated anchor-stage noise
> with relevance-floor breakage: "compiled" (a packet-vocabulary anchor)
> legitimately surfaced a generic memory without the floor being broken.
> Diagnosis and repair trail: batch-1-report.md (the miss dossier),
> batch-2-prereg.md Amendments 1-2 (per-dispatch correctness statistic;
> v1.5 stopwords), futon3c 6521fd3a + the v1.5 commit. t91A05 returns
> honest-empty under both anchor sources as of v1.5.
