# Campaign: C-cascade-real — make the pipeline-pattern-cascade live

**Date:** 2026-06-27 · **Status:** IDENTIFY + MAP (scoping pass; deliverable set, campaign-vs-mission framing, and first phase await Joe's ratification). **Drafted by:** claude-4.
**Keystone artifact:** `futon3c/holes/excursions/pipeline-pattern-cascade.html` — currently a hand-built control sketch.
**Relates to:** M-autoclock-in (member), C-substrate-completion / M-populate-substrate-2 (data substrate), M-futon-forward-model (futon7, downward layer), M-live-geometric-stack / M-stack-geometry-anthology / VSATARCS (rendering prior art).

---

## HEAD — operator shape

`pipeline-pattern-cascade.html` is a Moran-style **control sketch** of the stack: mission clusters → cited patterns → a capability semilattice (built features above, scans/holes below), with a downward forward-model (backlog → basin → valuable path → ROI → pipeline). Its own footer states the intent — *"promote the built capability cards into **generated** EDN rows, then attach devmap, invariant, mission, and VSATARCS evidence"* — and its closing schema names the deeper need: parked obligations should *"stop living as folklore in mission prose or chat history"* and become a **durable, queryable ledger** with wake-triggers and re-entry paths.

Today it is **hand-built from edn seeds** (`pipeline-pattern-map.edn`, `pipeline-semilattice-clusters.edn`). "Make it real" = back it with **live, durable, queryable** data so it regenerates from the running stack instead of being re-drawn by hand. The same gap shows up at the operator desk: right now, to teardown and restart fresh "on the 3 most recent missions, with these agents," Joe needs *a sheet of paper* — because that state isn't recorded anywhere queryable.

### The question (open spec → why a campaign, not a task)

**What does it take to turn the cascade from a hand-built sketch into a live, query-backed view of the stack's operational self-model — and in what order — given the data already lives, scattered and partly non-durable, across substrate-2, the agency registry, the edn seeds, futon7's forward-model, and the pattern library?**

The *gaps* are findable facts (below). The *composition* — how the durable spine, the data integration, and the rendering compose, and what the live cascade's node/edge ontology is — is the unknown the campaign exists to settle.

---

## IDENTIFY — the tension

The stack's operational self-model exists only as **folklore + hand-built artifacts**; the durable, queryable substrate underneath does not exist. Evidence (2026-06-27, live):

- **Agent↔mission lineage is non-durable and wrong.** The registry has a per-agent `mission-id`, but it is in-memory — a JVM restart left **all 13 agents at `mission=None`**. The fact that "claude-1 is on M-operational-vocabulary" lives only in claude-1's transcript. M-autoclock-in's own doc records this exact failure: M-operational-vocabulary *"was spun up after a long move-mining session was mis-clocked here — a live instance of the gap this mission exists to close."*
- **The cascade is static.** Its 12 clusters / cited patterns / capability cards are hand-maintained edn, drifting from the live missions.
- **Held/deferred work is prose-only.** No ledger; no wake-triggers; "what should wake up now?" needs manual archaeology.
- **The data is mostly already there, just not composed.** substrate-2 holds **271** mission-docs, ~**15k** mission-scopes, the code graph (var **125k**, commit **5510**) — and as of M-populate-substrate-2 D3 it is **time-travelable** (`db-as-of`). The forward-model has a working runner in futon7. The rendering pattern (stereolithograph / piano-roll / VSATARCS / WebArxana) is established. None of it is wired into one queryable operational view.

So: the pieces exist as sediment; the **connective spine (durable lineage + a generator)** is the wound — the same shape as M-populate-substrate-2's "palimpsest," one level up (operational state rather than code).

---

## MAP — inventory (2026-06-27)

Legend: ✅ exists & usable · ⚠️ exists but stale/non-durable/partial · ⛔ missing.

| Piece | State | Where |
|---|---|---|
| Mission docs / scopes | ✅ 271 mission-docs, ~15k mission-scopes, queryable | substrate-2 (futon1a) + filesystem `M-*.md` |
| Code graph / commits, **time-travelable** | ✅ var 125k, commit 5510, `db-as-of` | substrate-2 (D3, M-populate-substrate-2) |
| Cited patterns / pattern library | ⚠️ in seeds + futon3/library + futon3a meme.arrow; not unified/live | `pipeline-pattern-map.edn`, library, meme.arrow |
| Mission clustering (12 basins) | ⚠️ hand/script-built, 493KB edn, drift-prone | `pipeline-semilattice-clusters.edn` |
| **Agent↔session↔mission lineage** | ⛔ in-memory only, non-durable, mis-clocks | registry `mission-id` → **M-autoclock-in** |
| Process-tree lineage (agent ⊃ bg-tasks) | ✅ first node-type just built | `futon3c.agency.bg-process` (`e36c400`) |
| Held / deferred-work ledger | ⛔ prose-only folklore | cascade prose names the schema |
| Capability semilattice / scan-classes / holes | ⚠️ hand-built cards | cascade SVG + devmap/invariants/evidence sources |
| Forward-model (downward layer) | ✅ separate runner | futon7 `M-futon-forward-model.*` |
| Rendering substrate | ✅ prior art, not joined here | VSATARCS, piano-roll, WebArxana, M-live-geometric-stack |

---

## Phase 0 — RALLY (stakeholder discovery) — Joe, 2026-06-27

This campaign has **many in-flight stakeholders already** — facets of the `.html`
sketch exist as WIP across agents and repos. So the first act is a **RALLY**: map
who owns which facet before fixing the decomposition (D1–D6 below are *provisional*
until the rally lands). Fittingly, the rally must be done **by hand** because the
agent↔work lineage isn't queryable yet — the campaign's own gap, demonstrated.

**Attribution note:** agents commit as "Joe", so facet→agent mapping comes from
transcripts (3-day window) + a confirming bell-rally — not git author.

**Provisional stakeholder map (artifact + transcript survey, 2026-06-27; confirming via bells):**

| Facet | Provisional owner(s) | Signal | Artifacts |
|---|---|---|---|
| Sessions / autoclock lineage (D1) | **claude-1** | 419 autoclock / 98 clock-in / 525 mission-scope hits | `futon3c` 219df66 "multi-session session-overview"; M-autoclock-in |
| Cascade core / fold / rendering (D5) | **claude-10** | clock-in + `:apply-cascade` commits | M-wm-policies Car-3 (`:apply-cascade`, futon2/3c); futon3a E-fold-engine, E-llm-fold, cascade-scorer |
| Mining → ingest (data) | **claude-2** (?) | "completed mining data to ingest" (Joe) | `futon2` M-post-mining-ingest c310ef6 (IDENTIFY+MAP); + my M-populate-substrate-2 D3 |
| Forward-model (D6) | **claude-8** | 112 forward-model hits | futon7 M-futon-forward-model runner |
| Semilattice / clustering (D3–D4) | claude-6, claude-8, + unmapped `agent-a*` sessions | semilattice hits | `pipeline-semilattice-clusters.edn` (493KB) |

**Rally bells out (2026-06-27):** claude-1 (sessions), claude-10 (cascade), claude-8
(forward-model) — confirm facet + state + next-step + dependencies + who else.

### RALLY findings (incoming)

**claude-10 — CONFIRMED + campaign-reshaping intel (verified):**
- *Facets:* **the FOLD** (cascade → wiring diagram) `E-close-the-loop`; **`E-have-want-pairs`** (cascade supply/provenance + magnet quality); **CT semantics** (`ct-wiring-explainer.html` / E-darktower-wiring). State: fold impls #1 (classical, `E-fold-engine`) + #2 (LLM-turn, `E-llm-fold`) sim-verified; ct-wiring-explainer v1 shipped; all sim-only, off the live pilot.
- **🔑 The cascade generator ALREADY EXISTS** (verified on disk): `futon3a/holes/labs/M-memes-arrows/cascade_construct.py` + `cascade_serve.py` (minilm magnet, phylogeny-greedy coverage), shelled+memoized by `futon2/scripts/futon2/report/cascade_lane.clj`. **So D5 "make it real" is largely a WIRING job — swap the hand-edn for the live constructor feed — NOT a build.** Caveat (claude-10 measured): richness is gated by library coverage, and the magnet is strongest fed the **(have→want) meme**, not the mission id-stem (cascade-lane currently feeds the stem).
- **🚧 Blocking dep:** the paired **(have→want) corpus = substrate-2's empty PROOF join** — exactly **M-populate-substrate-2 D4** (argument/proof relations, all-empty) + M-operational-vocabulary. The magnet's "want" side (C-vector) is populated; the paired arrows aren't. *This makes C-cascade-real depend on substrate-2 D4 — closing the loop with C-substrate-completion.*
- *Surfaced stakeholders:* **claude-11** (R14 γ — consumes the fold's realized-outcome ΔG; live seam w/ claude-10) · **claude-1** also on **M-goals-and-holes** (mined the C-vector want-side — so claude-1 spans sessions/autoclock AND want-side) · **claude-4 (me)** owns the `:constructed`/`:open` **arrow store** (a fold's solution-side data).

**claude-8 — CONFIRMED:** owns **`M-interim-director-forward-model`** (futon7) — the
**supply+demand / cost→ROI corner** of the cascade's downward layer, *complement* to
claude-1's `M-futon-forward-model` (backlog→basin + the runner); they couple at the
`:anthropic-synergic-possibility` (salaried-hedge) square. State: materialized as
analysis + 4 figures behind a just-shipped skills advert, but **static** (not yet a
queryable runner). Cascade-axis next step (not started, Joe's call): expose the
cost/demand model as a parameterized runner so ROI/pipeline rows regenerate from it
instead of static PNGs. Data substrate = M-populate-substrate-2 D3; rendering = VSATARCS.
So **D6 (forward-model) has two owners** — claude-1 (backlog/runner) + claude-8 (cost/ROI).

**claude-1 — CONFIRMED (the linchpin):** owns **`M-points-de-fuite`** (futon2) — the
**live sessions surface**: per-turn sigils (`session-mode.el`) + per-session live
overview (`session-overview.el` — comb/threads/moves/scopes, auto-updating) + the
**WebArxana orbit/scope-surface** (live-polling + hover-to-explain, live today). State:
INSTANTIATE, built + verified on a fresh different-author session (claude-11). Also just
spawned **`M-post-mining-ingest`** (the canonical mine — "the durable, queryable substrate
a cascade self-model needs"; DERIVE next). Depends on the canonical mine for any
historical/aggregate cascade view. **⚠️ Says "M-autoclock-in is owned by claude-10"
(E-arxana-clock).**

### RALLY synthesis — consolidated stakeholder map (2026-06-27, all 3 replied)

| Facet of the cascade | Owner | Mission/excursion | State |
|---|---|---|---|
| **Live sessions surface** (the "sessions" render) | **claude-1** | M-points-de-fuite | INSTANTIATE — session-overview + WebArxana orbit live today |
| **Canonical mine / ingest** (data substrate) | **claude-1** | M-post-mining-ingest | just spawned; DERIVE next |
| **The FOLD** (cascade → wiring diagram) | **claude-10** | E-close-the-loop, E-have-want-pairs, CT semantics | fold impls #1/#2 sim-verified |
| **The cascade generator** | *already exists* | `cascade_construct.py`/`cascade_serve.py` + `cascade_lane.clj` | works on-demand (minilm magnet) |
| **Forward-model: backlog→basin + runner** | **claude-1** (per claude-8) | M-futon-forward-model (futon7) | runner exists |
| **Forward-model: cost/demand/ROI** | **claude-8** | M-interim-director-forward-model (futon7) | materialized but static (4 figures) |
| **R14 γ** (consumes fold ΔG) | **claude-11** | E-precision-over-policies | live seam w/ claude-10 |
| **Arrow store** (fold solution-side) | **claude-4 (me)** | `:constructed`/`:open` arrows | — |
| **(have→want) corpus = PROOF join** | *unowned BLOCKER* | substrate-2 **D4** + M-operational-vocabulary | empty (the universal gap) |
| **Autoclock / mission-clock lineage** (D1 pillar) | **claude-4** (resolved, Joe 2026-06-27) | M-autoclock-in | passed to claude-4 — it owns the bg-process process-tree node-type; D1 = the durable agent↔session↔mission lineage |

**What the rally changed vs my provisional guess:**
- The "sessions" render and the generator **already substantially exist** (claude-1's
  session-overview/orbit; cascade_construct/serve). Much of "make it real" is **wiring +
  feeding the right meme**, not greenfield build.
- The **one universal blocker is the (have→want) corpus = substrate-2 D4** — the fold
  (claude-10), the magnet, and any historical view (claude-1) all wait on it.
- **Conflicts (resolved + open):**
  1. **Autoclock ownership** — RESOLVED (Joe 2026-06-27): passed to **claude-4** (D1).
     The ambiguity itself was the live proof that the lineage isn't queryable.
  2. claude-1 spans 3 facets (sessions + mine + forward-model runner) — still needs
     explicit ownership boundaries, not folklore.

## Campaign roster (Joe 2026-06-27)

Retired all agents not joined to the campaign — deregistered + pouch-evicted
**claude-2, claude-3, claude-5, claude-6, claude-7, claude-9, codex-1, lon-claude-1**
(Agency-side; their session `.jsonl` persist + are re-registerable; Emacs repl buffers
left for Joe). **Active roster = the 5 stakeholders: claude-1, claude-4, claude-8,
claude-10, claude-11.** (Flag: **claude-2** holds the substrate-2 knowledge for the D4
blocker — re-add it if/when D4 gets a member-mission owner.)

**Reshaped first-car candidates (for Joe to pick):**
- **(A) The shared substrate** — (have→want) corpus = substrate-2 **D4** + claude-1's
  **M-post-mining-ingest** canonical mine. Unblocks the generator + fold + historical view
  at once. *Highest-leverage; the universal dependency.*
- **(B) The durable lineage / "no sheet of paper"** (D1) — resolve the autoclock-owner
  conflict + persist agent↔session↔mission so a teardown is reconstitutable. *Orthogonal,
  smaller, the operator-desk win.*
- **(C) Wire the existing generator** to the live constructor feed (swap hand-edn) — fast
  visible "it's live," but richness-limited until (A).

**Decomposition impact (provisional):** D5 downgrades from "build a generator" to "wire
the existing constructor + feed it the (have→want) meme"; the **(have→want) corpus
(= substrate-2 D4)** is promoted to a critical blocking dependency; claude-11 + the
arrow store join the stakeholder set. Re-ratify D1–D6 once claude-1/claude-8 land.

## CHARTER — what "make pipeline-pattern-cascade real" means

*(Phase after RALLY. Awaiting Joe + stakeholder ratification.)*

### Joint goal / gap

`pipeline-pattern-cascade.html` is today a **hand-drawn picture** of the stack's
operational self-model (mission clusters → patterns → capabilities → forward-model),
populated from hand-maintained edn. **"Real" = the cascade becomes a live PROJECTION
of the running stack, not a drawing of it.** The gap is not a missing renderer (the
generator exists) — it is the durable, queryable, composed *substrate* the renderer
should read.

**This is a *data* campaign** (see Non-criteria): the work is making the operational
self-model's data live/grounded/composed — *not* building inference or action on top of
it. That data-centricity is exactly why the cascade is relevant to so many
missions-in-progress at once.

### The shared standard — the cascade is "real" when ALL five hold

1. **Generated, not authored.** Every node/edge is produced by a query/constructor over
   live data (the existing `cascade_construct`/`cascade_serve` + the durable substrate)
   — **zero hand-typed edn rows**; regenerating is one command.
2. **Grounded.** Each node traces to live evidence — a mission (substrate-2 mission-doc),
   a scope, a cited pattern (library / meme.arrow), a capability/scan (devmap / invariant
   / evidence), an **agent↔session↔mission clock** (the durable lineage), a **held item**
   (the deferral ledger), a forward-model projection (the runners). No node is folklore.
3. **Durable, queryable, reconstitutable.** The operational layer (who is on which
   mission, what is held, what is in flight) **survives a teardown** and answers operator
   queries — *"the N most recent missions, who/which sessions are on each, and what held
   work is pending"* (the **no-sheet-of-paper** test).
4. **Honest (Moran's lesson).** A hole in the cascade is a *genuinely missing pattern/scan
   computed from the data* (a coverage gap), not a hand-placed `?`. Holes are where the
   adviser asks for new work.
5. **Composed.** The fold (cascade→wiring, claude-10), the sessions render (claude-1), the
   forward-model (claude-1/claude-8), and the lineage (claude-4) read/write **one shared
   ontology** — upward + downward + operational are one object, not four pictures.

### Joint completion criterion — two clauses

**Clause 1 — the data ask (the shared standard is met):**
- The cascade **regenerates from live queries with zero hand-typed rows** (standard 1).
- Every rendered node **resolves to live evidence** on inspection (standard 2) — spot-check
  passes for missions, patterns, capabilities, agent-clocks, held items.
- The **reconstitution query** works across a teardown (standard 3).
- At least one **honest hole** is shown to be a real, queryable coverage gap (standard 4).
- The four facet-owners' outputs **interoperate through the shared ontology** (standard 5).

**Clause 2 — the strategic implications are understood (Joe):** it is not enough that the
data *exists and is queryable* — we must **understand what it implies**. The capstone outcome
is **War Bulletin 11**, *read off the now-live cascade*: what the refreshed strategic
self-portrait shows, and — fittingly for a #11 — **what it refactors about the strategy**.
The campaign is not done until that reading is developed and recorded. (Bulletin 11 is thus
the cascade's *first real use*, and the proof that Clause 1 bought something.)

### Non-criteria — what "real" does NOT require

`pipeline-pattern-cascade` is fundamentally about **DATA** — making the stack's
operational self-model live, grounded, queryable, and composed. That is *why* it touches
so many missions-in-progress: they all read/write that data. What consumers *do* with the
data is out of scope. We explicitly do **NOT** require:

- **A working AIF / inference system** (Joe). Beliefs need not propagate, free-energy need
  not be minimized, no reasoning need run over the cascade. Live data ≠ a mind acting on it.
- **Autonomous action / a live pilot.** The cascade is a *projection*, not an actuator. The
  fold's `:apply-cascade` executor staying sim-only (off the live pilot) is fine — "real"
  does not mean the cascade *drives* work.
- **Correct / optimized forward-model economics.** Wiring the cost/ROI/backlog *data* into
  the cascade (D6) is in scope; the projections being *accurate* or *acted on* is not.
- **Complete coverage.** Not every mission perfectly clustered, not every capability card
  filled. Honest-and-partial (with real holes) beats complete-and-hand-maintained — the
  holes are the point (standard 4).
- **The full substrate-2 proof/inference layer.** The (have→want) corpus need only be
  *populated* enough to feed the magnet (the D4 **data** join); the argument/proof
  *inference* horizon (propagation, posteriors) is M-populate-substrate-2's, not this campaign's.
- **A new viz framework / pixel-perfect re-render / real-time dashboard.** Reuse the existing
  generator + VSATARCS/WebArxana; regenerable-on-command is enough — "real" is about the data
  behind the view, not the view.

**Boundary in one line:** the data is *live, grounded, and composed* — **not** that the
system *reasons or acts* on it.

### Membership (from the RALLY)

claude-1 (sessions surface + canonical mine + fwd-runner) · claude-10 (the fold) · claude-8
(fwd cost/ROI) · claude-11 (R14 γ seam) · **claude-4 (keystone-adjacent: D1 lineage owner +
arrow store)**. Held dependency owner **claude-2** (substrate-2 D4) — re-add when D4 is a member.

### Escrow preview (the held dependency everything waits on)

The **(have→want) corpus = substrate-2's empty PROOF join = M-populate-substrate-2 D4**.
The generator, the fold, and any historical view all block on it. To be formalised in an
ESCROW phase (cf. C-substrate-completion).

### Why a campaign, not a mission

Many stakeholders already own facets across repos (futon1a/2/3a/3c/7); the generator and
the data exist but are **disconnected**; the open spec (the shared ontology in standard 5)
must be settled jointly, not by one mission.

### Provenance — and the live-War-Room connection (Joe, 2026-06-27)

C-substrate-completion was chartered from a fresh War Bulletin (`war-bulletin-10` WR-21,
2026-05-31). **C-cascade-real has no fresh bulletin to charter from — because the strategic
portrait itself has gone stale.** `futon3/holes/war-room.md` + `war-bulletin-10.md` are both
**2026-05-31**; a great deal has landed since (substrate-2 D0–D3 + `db-as-of` time-travel, the
fold + generator, M-points-de-fuite, the two forward-models, M-post-mining-ingest, agency
hardening, bg-process). So this campaign is chartered from **Joe's direct direction
(2026-06-27/28) + the RALLY, recorded in this very session — `*claude-repl:claude-4*`** —
not a bulletin. (A fitting recursion: the chartering record is a *session* — exactly the
operational data this campaign makes queryable. When the cascade is real, *"how was
C-cascade-real chartered?"* is a query, not a doc pointer.) And rather than chartering
*from* a bulletin, this campaign **emits the next one — War Bulletin 11 — as its capstone**
(see Clause 2).

**The deeper point:** the cascade *is* a data-backed War Room — mission-clusters → patterns →
capabilities → forward-model + the operational lineage is exactly the War Room's
Mission-Portfolio + Decisions, **live instead of hand-written**. So *"make the cascade real"*
is the mechanism that makes the strategic self-portrait **self-refreshing**: it doesn't charter
*from* a bulletin, it makes bulletins *live*. Closing that loop — a stale hand-written War Room
→ a live projection — is the campaign's reason to exist.

**Data-source = forward + backward historical mining.** Backward = the D3 historical re-ingest
(`db-as-of`, landed) + claude-1's M-post-mining-ingest canonical mine; forward = the live
watcher / turn ingest. The cascade renders over this mined record — which is *why* the
(have→want) corpus (D4) + the canonical mine are the escrowed dependency.

**CHARTER exit criterion:** ✅ **RATIFIED — Joe, 2026-06-28** (`*claude-repl:claude-4*`).
The five standards + non-criteria are the agreed definition of "real" (a straightforward
*data* ask); the completion criterion is two-clause (data done-ness + strategic implications →
**War Bulletin 11**); membership is solved; provenance = this session. → Proceed to
CONSTITUTION (survey dependents + governance) + ESCROW (the D4 (have→want) corpus) + first car.

## Candidate deliverables (the "several other things") — the CHARTER's decomposition (RALLY-refined)

Ordered so each rests on the previous; member-mission boundaries to be ratified.

- **D1 — Durable agent↔session↔mission lineage (solve M-autoclock-in properly).** The pillar Joe named. A witnessed, single-active, operator-overridable clock-in that **persists** (survives restart) and is **queryable**: `agent · session · mission (+since) · last-active · bg-tasks`. bg-process already supplies the `agent ⊃ bg-tasks` node-type. Output: the "reconstitution sheet" query — *"3 most recent missions + who's on them"* — without paper. *First car.*
- **D2 — Held / deferred-work ledger.** Parked obligations as durable typed objects: reason, owner, evidence-condition, wake-trigger, expiry/review, re-entry path (the cascade's closing schema + its three candidate patterns: `held-item-wake-trigger`, `deferral-ledger-as-operator-memory`, `deferred-work-reentry-protocol`). Answers "what should wake up now?".
- **D3 — Live mission→pattern citations + clustering.** Replace the hand seeds: extract citations and recompute the 12 basins from substrate-2 + the pattern library, idempotently. The cascade's *upward* layer becomes generated.
- **D4 — Capability semilattice + scan-classes + holes, from evidence.** Generate the capability cards / scans / hollow nodes from devmap + invariants + mission/VSATARCS evidence (the footer's "next useful step").
- **D5 — The generator (keystone).** Render the cascade (and the operator reconstitution view) from D1–D4 queries — zero hand-typed rows. Reuses VSATARCS / stereolithograph / WebArxana rendering; the cascade HTML becomes an output, not a source.
- **D6 — Forward-model integration.** Join futon7's M-futon-forward-model downward layer (backlog→basin→ROI→pipeline) to the live upward structure.
- **D7 — War Bulletin 11 (capstone, Clause 2).** Read the now-live cascade and write the next bulletin: what the refreshed strategic self-portrait shows and what it *refactors* about the strategy. The cascade's first real use — and the proof D1–D6 bought something. (D1–D6 = the data; D7 = understanding what it implies.)

## CONSTITUTION — dependent-mission survey (≈ MAP, 2026-06-28)

*Method: read (not guessed) across the 5 stakeholders' mission/excursion docs (Explore
fan-out, cited). Recursion worth noting: this survey is itself a query the live cascade
should answer (substrate-2 mission-cross-ref / scope edges) — for now read by hand.*

### The web — four clusters the stakeholder-missions touch

- **Cluster A — Weaving + Mining (the in-flow data):** M-points-de-fuite ↔
  M-operational-vocabulary (forward methods) + M-goals-and-holes (backward goals / C-vector)
  + M-post-mining-ingest (governs the mine). The "concentration field" these produce **IS the
  cascade's observable.** → the campaign's canonical mine (D3, forward+backward).
- **Cluster B — The Fold (cascade→wiring):** E-close-the-loop (interface) + E-have-want-pairs
  (magnet quality) + E-darktower-wiring (CT types) + E-library-coverage. → the generator (D5)
  data path.
- **Cluster C — The AIF loop** (M-aif-wiring R16/R14/R19; E-precision-over-policies;
  E-C-vector-live): the cascade *runs inside* this and feeds it ΔG. **Per the CHARTER
  non-criteria this is the downstream CONSUMER — OUT of the campaign's completion scope.**
  We supply the data; whether the AIF loop reasons/acts on it is not our done-ness. (The
  survey's strongest service: it draws this boundary precisely.)
- **Cluster D — Lineage + Substrate (ground truth):** M-autoclock-in (D1) +
  M-populate-substrate-2 (D4) + M-mission-scopes-into-substrate-2 (live scope graph) +
  M-substrate-metric. → the D1 + D4 data infrastructure.

### The shared prerequisite (critical path): substrate-2 **D4** — the (have→want) PROOF join

Confirmed independently by the survey **and** claude-10's rally: **M-populate-substrate-2 D4
(the proof/argument-relation join) is the node the most clusters depend on** — it gates the
fold's risk-closure (B), the goals' discharge (A), and the cascade's grounding (D). It is the
campaign's escrowed dependency and the likely first car. *Unowned among the 5; claude-2
(substrate-2) is the natural owner — re-add per the roster note.*

### Hidden prerequisites surfaced (not on the stakeholder list — confirm with owners)

Named in E-have-want-pairs §3 (claude-10's territory): **E-proof-join-population** (= the D4
work), **E-wiring-diagram-corpus** (mint (cascade→wiring) pairs; the fold's corpus; gated on
magnet-quality), and **M-goals-and-holes DERIVE** (the backward C-vector is unassembled —
claude-1's want-side).

## ESCROW — the held dependency + the contract members join (2026-06-28)

**The escrowed (held) dependency:** **substrate-2 D4 — the (have→want) PROOF join**
(`constructs`/`closes`/`depends-on-sorry`/`uses-definition`/… from named sources). The fold
(B), the goals' discharge (A), and the cascade's grounding (D) all build *to* it; it
**releases** (`:contract-released`) once populated enough to feed the magnet (the *data* join
— not the full inference layer, per non-criteria). Owner: **claude-2** (substrate-2).

**The contract each member joins (the ESCROW):**
1. Build to the **five standards** (the data ask) and **inside the non-criteria** (it's a data
   campaign — supply the data; do not require the AIF loop / Cluster C to reason or act).
2. Own your facet's deliverable; read/write the **one shared ontology** (standard 5) so the
   facets compose.
3. Treat **D4** as the shared blocker — contribute to / build to the proof-corpus spec; don't
   route around it with folklore.
4. The capstone is **War Bulletin 11** (Clause 2) — the data must be made to *mean* something.

**Members joined (via coordinating whistle, 2026-06-28):**

| Member | Facet / deliverable | Joined? + first step |
|---|---|---|
| claude-2 | **D4 substrate** (the escrowed proof-join) — owner | ✅ — promote the **177 `:mined-structural` arrows** in `futon6/data/diffsub-moves-mined.edn` (the unpromoted overlay = the gap D4 closes) |
| claude-1 | Cluster A: canonical mine + sessions/D1-render | ✅ — **D1 render already LIVE** (WebArxana `#/dev/focus/C-cascade-real`, 7 cross-session orbits / 13 sessions, data-only); first step **M-post-mining-ingest → DERIVE** (pin the canonical mine) |
| claude-10 | Cluster B: the fold | ✅ — first step **E-have-want-pairs Q-B magnet-quality probe** (UNBLOCKED — runs on existing data, doesn't wait on D4); owns E-wiring-diagram-corpus |
| claude-11 | R14 γ seam | ✅ as **interested party / downstream consumer** (Cluster C / AIF — out of *completion* scope per non-criteria; validates the fold's ΔG, supplies no campaign data); waits on D4 + R16 live-wire |
| claude-8 | forward-model (cost/ROI, downward) | ✅ — first step a **parameterized runner** emitting cascade-consumable EDN (cost/demand/ROI) from local data (v0 routes around D4); **parked behind Joe's greenlight** (mid-advert, back Monday) |
| claude-4 (me) | D1 lineage + arrow store; campaign coordination | ✅ (owner-adjacent) |

**Whistle findings (2026-06-28):** the ownership of the surfaced prereqs is now confirmed —
**E-proof-join-population *is* the D4 blocker (claude-2)**, not a separate excursion;
E-wiring-diagram-corpus = claude-10; M-goals-and-holes DERIVE = claude-1. And there is an
**UNBLOCKED first car**: claude-10's Q-B magnet-quality probe runs on data we already have —
real work that doesn't wait on D4. (D5 is confirmed a wiring job: feed `cascade-lane` the
(have→want) meme, F −0.19→+0.90, gated on D4 producing the memes.)

### First car IN MOTION — claude-2's D4 feeder-(b) slice (2026-06-28)

**claude-2's reframe (accepted):** D4's deliverable is the **promotion path (overlay→store)**,
NOT inventing relation content. So D4 is *not fully blocked* — feeder-(b) ships now.

**Verified (substrate-2 count-pushdown, 2026-06-28):** the proof layer is EMPTY — `have-want
0 · arrow 0 · constructs 0 · closes 0 · depends-on-sorry 0 · uses-definition 0 · supported-by
0` (only `sorry 23 / related-mission 28`). So neither the want-side (C-vector / M-goals-and-holes,
DERIVE-pending) nor the forward arrows are promoted ⇒ **feeder-(b) is the unblocked first car.**

**GREENLIT (per Joe's "let owners run their unblocked first steps"):** claude-2 promotes the
**177 `:mined-structural` arrows** (`futon6/data/diffsub-moves-mined.edn`) through the T-A4
gate (canonical-source + normalized-target + named-consumer), kept `:mined-structural` —
**data-join only, not laundered as proofs** (non-criteria-compliant: enough to feed the magnet,
not the inference horizon). The full forward↔backward join sequences after claude-1's
M-post-mining-ingest DERIVE + the want-side land. (claude-4 shares the sorry-arrow contract —
(have,want)-keyed, miner MINTS, unify-on-promotion — available to pair.)

**Refinement (claude-10, 2026-06-28):** the **want-side C-vector is already *populated*** (263
wants; 55 diffsub-moves on the forward side) — so the D4 gap is *specifically the **paired
(have→want) arrows** in the store* (the promotion + unify-by-endpoint), not the endpoint data.
That tightens claude-2's slice: promote-and-pair, the endpoints largely exist. claude-10 also
brings a **ready D-standard now**: the wiring schema = DarkTower combs / typed-holes
(ct-wiring-explainer), CLean `.clean.edn` as the wiring format, the 0-sorry render as the
quality gate, with 102 math.CT CLeans (futon6 mark5-ct100-run) as reference data. And the **D5
keystone is one wiring fix**: feed `cascade-lane` the (have→want) meme not the mission id-stem
(F −0.19→+0.90), gated on D4 producing the paired memes.

### Strategic implication + governance

The data deliverables map cleanly onto **Clusters A/B/D**; **Cluster C (AIF) is the consumer
the CHARTER excludes** — a clean confirmation the campaign stays a *data* campaign. The
critical path is **D4**, where the fold, the goals, and the grounding all wait — so first-car
**(A) the D4 corpus** is corroborated as highest-leverage by the dependency structure, not
just by one stakeholder's say-so. Roles: claude-1 = Cluster A mine + sessions/D1-render;
claude-10 = Cluster B fold; claude-8 (+claude-1) = forward-model (downward); claude-4 = D1
lineage + arrow store; **claude-2 (re-add) = D4 substrate**. Coordination = swarm around D4.

## Exit / keystone (draft — for Joe to ratify)

1. The cascade regenerates from live queries with **zero hand-typed rows** (D3–D5).
2. The operator can query **"the N most recent missions, who/which sessions are on each, and what held work is pending"** and reconstitute it after a teardown (D1–D2).
3. A clock-in is durable, witnessed, single-active, and never silently mis-clocks (D1) — the claude-1 failure cannot recur.

---

## First car + open ratifications

**Proposed first car: D1 — durable lineage (M-autoclock-in done properly).** It is the pillar Joe named, the smallest self-contained durable-substrate win ("no sheet of paper"), it builds directly on the bg-process process-tree node-type just shipped, and everything downstream (D2–D6) reads the lineage it establishes.

**For Joe to ratify before build:**
1. **Campaign vs single mission?** This reads as a campaign coordinating M-autoclock-in (D1) + new members (D2 ledger, D3–D5 generator, D6 forward-model). Confirm the framing + the name (`C-cascade-real`?).
2. **Deliverable set + ordering** — is D1-first right, or do you want the durable lineage + the generator (D1+D5) as a vertical slice to see the live cascade sooner?
3. **Substrate home** — does the durable lineage live in **substrate-2** (futon1a hypergraph — natural, already holds missions/code/scopes, time-travelable) or a dedicated agency ledger (like invoke-jobs)? Leaning substrate-2 so the cascade is one query surface.
4. **Scope of "real"** — full cascade (upward + capability + forward-model) or upward-only first?
