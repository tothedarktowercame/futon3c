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
