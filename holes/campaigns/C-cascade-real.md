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

### Escrow ledger (one entry per cross-mission dependency)

Each held slice is **named, not built** (HEAD-as-escrow, generalised cross-mission — the
requirement stands, but isn't built on sand). Two-step release per entry:
`held → contract-released (build-to-verified-spec) → satisfied (consume-live-delivery)`.

| # | from-mission (the **held** slice) | on (standard / keystone) | requirement | status | **contract-released** by (verified spec) | **satisfied** by (live delivery) |
|---|---|---|---|---|---|---|
| E1 | **D5 generator wiring** (claude-10 — feed `cascade-lane` the (have→want) meme, F −0.19→+0.90) | **D4** paired (have→want) memes | D4 emits paired memes the magnet can consume | **held** | claude-2's promotion path passes the **T-A4 gate** (canonical-source + normalized-target + named-consumer) | `cascade-lane` consumes the live paired memes → cascade regenerates |
| E2 | **The fold's full risk-closure** (E-close-the-loop / E-have-want-pairs) | **D4** full (have→want) join | both endpoints paired + promoted in the store | **held** *(Q-B magnet-quality probe is a separate, UNBLOCKED slice)* | D4 + want-side promotion spec verified | the fold consumes the live paired corpus |
| E3 | **D4 full forward↔backward join** (claude-2, M-populate-substrate-2 D4) | **M-post-mining-ingest** canonical mine + want-side endpoints | the mine pinned (provenance/checksum) + want-endpoints promoted | **held** *(feeder-(b): the 177 :mined-structural arrows is the UNBLOCKED slice)* | claude-1's **M-post-mining-ingest DERIVE** pins the canonical mine | the join consumes the pinned mine + promoted want-side |
| E4 | **Substrate-backed forward-model runner** (claude-8, M-interim-director-forward-model) | **D4** (substrate-2 time-travelable graph) + **Joe greenlight** | ROI/backlog rows queryable from the live graph | **held** *(v0 local-data runner routes around — Joe-greenlight-gated)* | D4 released + **Joe greenlight** (back Monday) | the runner reads live graph rows → downward layer regenerates |
| E5 | **γ live self-calibration** (claude-11, E-precision-over-policies) | **D4** real ΔG + **R16 live-wire** (`*live-wire?*`) | real `:realized-outcome` data + the live-wire flag set | **held** *(sim validation unblocks on D4 alone)* | D4 released + claude-10's `:realized-outcome` producer contract stable | γ reads live realized-outcomes (R16 live-wire = Joe operator decision) |
| E6 | **D7 — War Bulletin 11** (capstone, Clause 2) | **the live cascade** (D1–D6) | cascade regenerates from live data, zero hand rows | **held** | D1–D6 standards (Clause 1) verified | bulletin-11 read off the live cascade → strategy refactor recorded |

**Exit check:** ✅ every cross-mission dependency (E1–E6) is in the ledger with a `held`
status and a named two-step release path (verified-spec → live-delivery). The UNBLOCKED slices
(claude-10's Q-B probe, claude-2's feeder-(b), claude-1's M-post-mining-ingest DERIVE, claude-1's
already-live render) are *not* held — they are the work that earns the contract-releases above.

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

## STANDARD-ARGUE — the cascade-real data contract (2026-06-28)

*Criteria (Joe-approved): (1) state the standard as a contract of obligations — what the
data must deliver, per consumer, not the implementation; (2) argue each obligation
**empirically / falsifiably** (checkable as a design now); (3) **cross-dimension adequacy** —
certify each consumer's fit independently (one being satisfied does not imply another's).*

**Correction (Joe, 2026-06-28): D4 is the *keystone* dimension, but "cascade-real" spans
several DATA DIMENSIONS — the property is not reducible to D4.** The contract below states the
obligation for *each* dimension the cascade renders over. Each instantiates the five CHARTER
standards (generated · grounded · queryable · honest · composed) for its own data + consumers.

| # | Data dimension | Owner | Obligation (contract — checkable as a *design*) | Fit-for (consumer) |
|---|---|---|---|---|
| **O1 (keystone)** | (have→want) **PROOF corpus** = D4 | claude-2 | paired arrows in the store, **(have,want)-keyed**, T-A4-gated, kept `:mined-structural` (data-join, **not** laundered as proofs), unify-on-promotion | the magnet/D5 (E1), the fold (E2) |
| **O2** | **canonical mine** (memes/moves/C-vector) | claude-1 | **pinned**: provenance + checksum + version + a named consumer contract (ONE canonical source) | every consumer reads a stable mine |
| **O3** | **sessions / lineage** (D1) | claude-4 (+claude-1) | durable agent↔session↔mission lineage, queryable, **survives teardown** (the reconstitution query) | the operational layer / "who's on what" |
| **O4** | mission→pattern **citations + clustering** (upward) | *(cascade_construct/lane — owner TBD)* | generated from live data (substrate-2 + library), **zero hand rows**; clusters reproducible | the upward cascade |
| **O5** | **capability semilattice + scan-classes + holes** | *TBD (hidden gap)* | capabilities + **honest holes** computed from devmap/invariants/evidence; a hole = a real coverage gap, not a hand `?` | the built-features / scans view |
| **O6** | **forward-model** data (cost/ROI/backlog) | claude-8 | cascade-consumable **EDN rows** from a parameterized runner (data, not static figures) | the downward cascade |
| **O7** | **wiring / CT** data | claude-10 | wiring schema: DarkTower combs/typed-holes + CLean `.clean.edn` + the **0-sorry render gate** (102 math.CT CLeans as reference) | the fold's output typing |

**Cross-dimension adequacy (criterion 3):** the join is **standard 5 — one shared ontology**:
every dimension's data must use one node/edge identity so O1–O7 compose into *one* cascade,
not seven pictures. Each obligation is certified **independently** (O1 satisfied ≠ O4
satisfied). D4 is the *keystone* because the most escrow entries (E1/E2/E3/E5) hold on it — but
**cascade-real = all of O1–O7 met**, not D4 alone.

**Empirical-argument note (criterion 2):** each obligation is a *design* check available now —
e.g. O1 "the magnet's F improves −0.19→+0.90 fed paired memes" (claude-10 measured); O3 "the
reconstitution query returns after a teardown" (the no-sheet-of-paper test); O5 "a rendered
hole resolves to a real coverage gap." None require building the consumer first.

**STANDARD-ARGUE exit:** ✅ **RATIFIED — Joe, 2026-06-28.** The contract is stated per
dimension, checkable-as-design; `owners-TBD` (O4/O5) explicitly accepted as open — the work is
picked up by the team, and **finding the best owners is a standing coordinator (claude-4)
responsibility** (not a blocker). Passing this contract (STANDARD-VERIFY, next phase) is what
moves the escrow entries `held → contract-released`.

**Coordinator's TBD-owner watch (claude-4):** O4 (citations+clustering) — mechanism is the
existing `cascade_construct/lane` (claude-10-adjacent); likely folds to whoever drives the
upward render, watch as O1/O2 land. O5 (capability semilattice + scans + holes) — natural home
is **M-capability-star-map / the devmap**; not on the roster, so either rally a candidate or
carry it as an honest hole until the work pulls an owner. I'll assign as the swarm's work
surfaces the natural fit, and record each assignment here.

## STANDARD-VERIFY — logic-model-before-code at Campaign tier (2026-06-28)

STANDARD-VERIFY ratifies the O1–O7 contract as **cross-mission-adequate *as a design*
(PASS-on-design)** — applying **`logic-model-before-code` at the campaign tier**: it moves
from the *data shape* (STANDARD-ARGUE: what each dimension's data must be) to **how it gets
populated** — the population logic, shown as a design-checkable model *before* the code. The
elegant part: several population logic-models are **already being demonstrated by the in-flight
first cars** (the design-evidence is real, not hypothetical).

| O | population logic — **source → process → cascade-ready data** | design-check (verifiable as a design *now*, no consumer built) | demonstrated by |
|---|---|---|---|
| O1 | 177 `:mined-structural` overlay (futon6/diffsub-moves) **→ T-A4 gate → substrate-2 promote**, (have,want)-keyed, unify-on-promotion | the magnet's **F −0.19→+0.90** when fed paired memes (claude-10 measured) | **claude-2 feeder-(b) — IN FLIGHT** |
| O2 | mined memes/moves/C-vector **→ pin (provenance+checksum+version) + consumer contract** → canonical mine | one consumer reads a stable, versioned mine | **claude-1 M-post-mining-ingest DERIVE — IN FLIGHT** |
| O3 | agent turns (watcher/clock) **→ durable agent↔session↔mission record** → queryable lineage | the **reconstitution query returns after a teardown** (no-sheet-of-paper) | **claude-1 render LIVE** + claude-4 D1 / bg-process node-type |
| O4 | missions+patterns (substrate-2/library) **→ `cascade_construct/lane` extract+cluster** → upward structure | regenerates with **zero hand rows**; clusters reproducible | generator exists (wire to live) |
| O5 | devmap/invariants/evidence **→ compute capabilities+scans+holes** → built/scans/holes view | a **rendered hole resolves to a real coverage gap** | *TBD (the gap)* |
| O6 | local cost/ROI data **→ parameterized runner** → cascade-consumable EDN | EDN rows regenerate the downward layer | claude-8 runner (Joe-gated) |
| O7 | cascades+fold **→ emit wiring (DarkTower/CLean), 0-sorry gate** → wiring data | a wiring renders **0-sorry** vs the 102 math.CT reference | claude-10 fold sim-verified |

**Cross-dimension composition (the join is checkable as a design):** the shared ontology
(standard 5) means the population pipelines write to **one identity** — a node minted in O1 (an
arrow endpoint) is the *same* identity O4 cites and O3 attributes. Verified via the
(have,want)-keying + the mission-scope identity, *as a design*, without building the consumers.

**The bar is RAISED (Joe, 2026-06-28): in-flight ≠ passed — it must LAND, and be VERIFIED.**
Unlike C-substrate-completion's PASS-on-design, this campaign's STANDARD-VERIFY does **not**
pass on promissory in-flight work. The population logic-models must (a) **land** (the cars
deliver the data) and (b) be **actually verified** — two levels:

- **Level 1 — verify in `core.logic`** (already a stack tool: `futon3c.logic.*`). Encode the
  population logic-model — O1–O7's `source→process→cascade-ready` relations + the
  **cross-dimension composition** (one shared identity: an O1 arrow endpoint = the O4-cited =
  the O3-attributed node) — as `core.logic` relations, and run **consistency / satisfiability**.
  The logic model becomes *executable*, not asserted. *(Owner: claude-4.)* — ✅ **BUILT
  2026-06-28** (`futon3c.logic.cascade-real`, commit `9618f85`): contract O1–O7 + E1–E6
  encoded; the conforming model is composition-consistent + acyclic + standard-covered,
  **D4(=O1) is the keystone by held-on in-degree (earned, not asserted)**, O4/O5 reported as
  honest holes; **5 tests/13 assertions green** incl. adversarial-caught (conflicting
  node-type / uncovered standard / self-dep). `db-from-data` runs the SAME checks over the
  real rows when the cars land — a runnable gate, not a doc claim.
- **Level 2 — the push (Joe): verify via the DarkTower model against the Lean CT theory.** Type
  the cascade's wiring (the fold's combs / typed-holes, O7) as **DarkTower** objects and
  **0-sorry-check** them against the real Lean theory (`mathlib4/DarkTower/Fill.lean` /
  `Discharge.lean` / `CLeanProofs.lean`; 102 math.CT CLeans as reference). This certifies the
  cascade's *structure* against formal CT — not just operational consistency. *(Owner:
  claude-10 / O7; the CT/Lean territory.)* — **SCOPED 2026-06-28 (claude-10): FEASIBLE, a real
  near-term slice, and D4-INDEPENDENT** (prototypable *now*, ahead of the blocker — renderer
  `clean_to_lean.py`, the DarkTower theory, the 0-sorry gate, and fold impl #2 all exist; only a
  thin adapter is new). **What L2 certifies (we-do-discipline — record exactly):** ✅ the wiring
  is a **structurally well-formed CT construction** (boxes compose — every `:consumes` wires to
  an upstream `:produces` or a declared open hole — valid `BV.seq` spine, coherent `copar`,
  valid discharge polarities); 0-sorry = "well-typed in the DarkTower theory." ❌ **NOT** semantic
  adequacy (it does *not* certify the fold made the *right* construction, the math, ΔG, or
  cascade relevance). Honest claim: *"the generated wiring is a machine-checked, structurally
  sound CT construction"* — a **soundness floor** that rules out the LLM-fold's malformed /
  hallucinated wirings, **not** "the cascade is correct." Minimal first slice: one real cascade
  (`cascade_serve.py`, e.g. M-value-creation-loop F=+0.90) → fold via impl #2 → CLean →
  `clean_to_lean` render → 0-sorry check vs the reference. *No D4 needed.*
  - **L2 scope extension — the cascade-of-cascades (Joe 2026-06-28).** The minimal slice proves
    *a* cascade is a well-typed comb. But the pipeline cascade is **all** the cascades, and in a
    real sense a **cascade-of-cascades** — so the formally interesting facts are about
    *composition*, and the DarkTower theory already has the leverage. Beyond rung 1, **one or two
    more proofs show the pipeline itself is well-formed**:
    - **Rung 2 — closure under composition.** If cascades C1, C2 are well-typed combs and
      composable (C1's `:produces` type-match C2's open holes), their `BV.seq` composite is a
      well-typed comb — i.e. the comb algebra is *closed* (the spine stays valid, discharge
      polarities compose). A machine-checked DarkTower lemma, not a per-cascade render.
    - **Rung 3 — pipeline-as-cascade (self-similarity).** The whole pipeline — the composite of
      all cascades under the cascade-wiring — is itself an object of the **same** cascade type:
      *cascade-of-cascades is a cascade*. The fixed-point / fractal reading, via induction on the
      pipeline DAG (the operad-multiplication structure of combs-of-combs).
    Rungs 2–3 stay **structural** (well-formedness lifted from one cascade to the composition and
    the whole) — the same honest scope as rung 1, **not** semantic correctness. Rung 1 is built
    so it extends toward these; claude-10 scopes how far rungs 2–3 go now vs later.

**STANDARD-VERIFY exit (raised):** O1–O7 **landed** + **core.logic-consistent** (Level 1), with
the **DarkTower-against-Lean-CT** check (Level 2) as the formal push that turns "design-adequate"
into "formally verified." Passing this moves the escrow entries `held → contract-released`; the
landed cars are then the **RUN/DELIVER** evidence (`→ satisfied`). The in-flight work
(claude-2's promote, claude-1's DERIVE, claude-10's Q-B) is the *path to* the bar, not the bar.

**STANDARD-VERIFY exit — REVISED (Joe, 2026-06-28): PASS on existing work (pending Joe's
ratification).** The raised bar is **relaxed**: STANDARD-VERIFY closes on **Level 1 alone** —
landed + core.logic-consistent + adversarial-caught — *without* requiring the Level 2 formal
proofs. Be explicit about the trade (not a silent slip):
- **DROPPED from blocking → DEFERRED enrichment:** the L2 DarkTower-vs-Lean-CT push *and* its
  rungs 2–3 (closure-under-composition, pipeline-as-cascade). Formally interesting, **not a
  blocker** for the rest of the campaign. claude-10's rung-1 build continues as a *bonus* that
  lands when it lands; the rung-2/3 sizing it returns informs a future enrichment ticket, not
  this gate.
- **STILL STANDS (the pass basis):** L1 `cascade_real.clj` (`9618f85`) — the O1–O7 + ESCROW
  contract is machine-verified composition-consistent / acyclic / standard-covered, D4 the
  earned keystone, **and each adversarial model is caught** (5 tests/13 assertions green). A
  real logic-model-before-code result, not a doc claim.
- **RELOCATED, not waived:** L1 today verifies the *model* (`contract-facts`), not live rows.
  `db-from-data` runs the SAME checks over the real data — that verification is the
  **RUN/DELIVER** gate (it lands when the cars do), so data-verification moves to its proper
  phase rather than blocking this one.
- **Net:** the design is verified (L1); the *data* is verified at RUN/DELIVER (same model, live
  rows); the *formal-CT* layer is a deferred enrichment. Escrow `held → contract-released` moves
  on this L1 pass; `→ satisfied` still waits on the cars (RUN/DELIVER).

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
