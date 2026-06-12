# In-flight notebook — WM piloted flight, 2026-06-12

Pilot: fable-2. Ground control: Joe (voice). Charter: fable-2 enters
the War Machine as pilot, narrates what it sees, fixes live, hands the
rest to the team. Notebook is APPEND-ONLY during the flight.

## Sortie 1 — ignition + instrument sweep (~16:25)

- IGNITION FAILURE: engine would not start; compute-delta-t-mission's
  requiring-resolve fallback throws instead of nils. Fixed live by
  pilot, futon2 774eaa2. Full panel: /tmp/wm-flight-1.md.
- DEAD FEEDS (handed off): mission scan reads a dead store, "0
  missions" (codex-1); mission-aif-head classpath failure surfaces as
  Priority #1 + Self-Watch evidence API dead (codex-3); 4/6 loop-health
  arrows 0%-never + all session mana balances zero — diagnose
  dead-feed vs real-gap (codex-4).
- LIVE INSTRUMENTS: working-tree drain (futon7a 22d), temporal
  coupling (futon3c<->futon5a 0.79), workstream balance (stack 74%,
  avoided range), R12 Beta posteriors.
- KEY LIVE READING: R12 address-sorry intrinsic 0.049 (12 follow-
  throughs / 226 emissions) — the machine has measured its own
  recommendations' uselessness correctly for weeks; nothing consults
  the reading.
- PHASE A DISEASE CONFIRMED: priorities are 11 channel-gap scalar
  nags; no chains. Hermit mode, G-epistemic 0.94 — "I don't know
  enough" with no way to say what to learn.

## Sortie 2 — Joe's interface (~16:30)

- Joe's surface showed a 50-day-old cached prior as Recommended Next
  Move; evidence-landscape turns absent. Another Claude took the
  interface fix (not this flight's lane).
- After their fix: live recommendation "advance mission M-first-flights"
  — ACCURATE (matches actual frontier). First live-true recommendation
  in 50 days.

## Sortie 3 — the tile click, performed by hand (~16:36)

- The recommendation tile has no descend wiring (R12 disease: authored
  link rendered inert). Pilot performed the click manually:
  M-first-flights -> Phase A PASSED (ckpt 20), Phase B armed, ratified
  next car = flight-pretty-print (ckpt 21).
- Chain assembled across three surfaces = problem -> cascade -> hole ->
  solution; Joe certified by inspection (the ruling-surface act,
  futon2/holes/ruling-surface-certification.md).
- flight-pretty-print DISPATCHED to codex-3 (queued behind its WM fix).
  On bell-back: pilot reviews, writes ckpt 22, multi-watcher reingests,
  WM re-reads — the first full observed WM loop closure, if it lands.

## Sortie 4 — advance typing + mission-anatomy gaps (~16:40)

- Joe specified the automation: advance TYPES = mission satiety —
  :checkpoint-only / :ratified-car / :design / :operator-ruling. The
  WM loop runs without narration by routing on hunger type. (Recorded
  in ruling-surface-certification.md.)
- Mission-mode on M-first-flights (27 scopes) shows the paper-walk
  diseases in the mission corpus: owed work not harvested from
  checkpoint prose (R12); INSTANTIATE ghost section instead of typed
  promissory hole (R8); VERIFY without bound pass-certificate (R4);
  loose checkpoints with no discharges-edges to phases.
- NEXT DISPATCH (when a codex slot frees): mission-scope detector
  upgrade — checkpoint harvest (certificates + ratified cars),
  promissory scopes for ghost phases, certificate-binding edges. One
  build feeds both mission-mode legibility and the WM advance queue.

## Open in-flight items

- [ ] codex-1 bell-back: mission scan -> substrate-2
- [ ] codex-3 bell-back x2: heads/self-watch; then flight-pretty-print
- [ ] codex-4 bell-back: loop-arrows + mana verdict table
- [ ] codex-2 bell-back: anatomy-v0 sweep build -> launch overnight CT run
- [ ] Pilot: review all, checkpoint M-first-flights, re-run engine,
      compare panel before/after (the loop-closure measurement)

## Sortie 4a — look at *mission-overview* (16:44, refinement)

Joe's look shows the overview tree: certificate·certificate-pass DOES
exist ("Status: PHASE A COMPLETE...") — the certificate detector
fired. Refinement of the sortie-4 finding: the gap is not detection
but BINDING — the certificate floats as a top-level status scope
inside a loose-section wrapper, with no discharges-edge to the VERIFY
phase it certifies, and the eightfold-phase scopes sit beside it
unconnected. Same R4 shape: the use and the binder both exist;
the edge doesn't. INSTANTIATE still shows no owed-work scope.

## Sortie 4b — pattern cross-references are a LIST, not a cascade (16:45)

Joe, on the mission's "pattern cross references" section: "we've
retrieved patterns... but this isn't formatted as a pattern cascade.
It's formatted as a list. A clear missed opportunity — formatting
retrieved patterns as a cascade was meant to be the VERY FIRST
requirement of a cycle in the war machine. So we haven't actually even
initiated the first cycle."

Finding: retrieval exists, composition doesn't. The flat list is the
1-d projection (registry-as-projection) of what should be a typed
composition: which pattern feeds which, FOR THIS PROBLEM, with roles.
ROOT CAUSE OF PHASE A: the cascade assembler is the missing organ —
and it is NOT missing from the stack: E-cascade-sampler-sampler builds
exactly this object (samplers assemble pattern cascades over
circumstances, judged by the grounded harness). It was never wired to
mission-mode/WM. WM cycle 1, step 1 = run the cascade assembler over
the mission's retrieved patterns + the mission's open hole as the
circumstance. The contest lane and the WM lane are the same lane.

## Sortie 4c — the training set already exists: closed missions ARE cascades (16:46)

Joe: "We've done hundreds of missions, probably, that are completed —
most of which have pattern cross-references. If we wanted to build a
training dataset of cascades, we have no further to look than our
actual closed missions."

Each closed mission is a LABELED cascade: circumstance = its IDENTIFY
tension; selected patterns = the cross-references; outcome = closure
with certificates (success label). The R12 lesson at corpus scale,
again: don't synthesize training circumstances (contest v0 did, for
fairness reasons that made sense then) — HARVEST the authored history.
One bulk pass over closed missions yields:
- the cascade training set for the assembler (GFlowNet lane gets real
  reward-labeled trajectories instead of proxy targets);
- metric golden triplets for free (patterns that co-fired in closed
  missions are near each other / near that problem class);
- prior cascade SHAPES for the ruling surface (what certified
  composition looks like, historically).
Pipeline shape is the anatomy-v0 sweep again: deterministic harvest,
per-mission loss (missing cross-refs, unbound certificates), DP passes
to clean, then train. Same architecture for papers, missions, flights —
third time today the corpora have converged.

**4c ratified (Joe): "All we'd have to do is reassemble the pattern
cross-references we've made as cascades rather than as lists, and we'd
be good to go."** And the reassembly evidence is AUTHORED: patterns
are cited at specific checkpoints/phases, so cite-site order + phase
context gives the cascade wiring deterministically — no model needed
for v0. Output format: the golden-graph EDN schema (a cascade IS a
hyperedge graph; nodes = patterns/problem/holes, edges = feeds/
discharges with :via quotes). Status: :ratified-car, queued for the
first free codex slot, behind the five in-flight tasks.

## Sortie 4d — look at M-first-flights.md:301 (realtime/structured-events-only)

Joe points at the mission's own PSR: "the trace must be typed events,
not prose. The current record half-obeys it (typed slots) and
half-defies it (the grounds live in prose)."

This is the forward/backward split for everything ratified today:
- BACKWARD (harvest): checkpoint prose mining works precisely because
  authors half-structured it — but it is retrofit, not a way of life.
- FORWARD (typed at birth): new records obey structured-events-only —
  checkpoints carry typed slots (:certificate, :next-car, :discharges
  PHASE), cascades are born as EDN, flights emit typed organs. The
  M-typed-bells precedent exactly (type at birth -> ArSE at birth).
The harvest pass is therefore ALSO the migration: it defines the typed
shapes that future records emit natively, and prose-mining decays to
zero as the corpus turns over. The mission predicted its own fix.
