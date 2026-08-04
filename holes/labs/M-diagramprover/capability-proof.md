# Capability proof: "all APM problems are solved" (live document)

Instantiates M-diagramprover §"Application to theorem-proving
capability construction" (commit 8b3f2213). This is the BHK-style
proof-by-construction of the programme's capability claim: the
constructive content is the relay pipeline; each node below is a typed
hole with a contract; warrants upgrade ONLY by certificate, never by
narrative. The automated workflow appends to §Update-log and upgrades
node warrants as runs land.

**Warrant classes** (from 2026-08-03 practice): `mechanical`
(executed witness: lake exit 0, axiom-clean, hash-verified) ·
`replicated` (independent runs agree on load-bearing quantities) ·
`inductive-n=K` (K observed instances, honesty bounds stated) ·
`designed` (contract written, no witnesses) · `registered` (candidate
mechanism, not applied) · `refused` (with species: proved-impossible
vs not-yet-capable).

---

## TOP: all APM problems are solved

Constructive reading: a procedure exists that, given any APM problem,
produces a mechanically-verified solution; witnesses attach to each
component. Current corpus state: ~135/493 problems closed (~27%).
Warrant: **under construction** — discharge = nodes N1–N8 at
sufficient warrant + the pipeline running continuously (N9).

## N1 — Extra resources can fill Mathlib holes

Contract: given a partial with a conforming boundary comment naming a
missing dependency, a closer hop either closes it (local construction,
found lemma, or route-around) or produces a strictly better boundary.
Certificates: a96J04 (interval decomposition, proved locally ~15 min,
commit 33575db, 6-step gate PASS); a96J07 (local Liouville from the
two-pole result, 462b48a, non-circularity import-verified).
Open instance: a96J08 (rectangular-contour residue theorem, 37192e1,
two unblock routes documented). Warrant: **inductive-n=2**, 1 open.
Upgrade path: J08 closer (the relay's 4th leg); each future closer hop
adds an instance.

## N2 — Work transports between agents

Contract: a session ending in a conforming boundary artifact transfers
its state to any successor at ~zero re-derivation cost. Mechanism =
the boundary-comment protocol (specified in store:
e-e9-a96j04-localize-an-observed-blocker-at-one-sorry).
Certificates: E9→J04 closer (15-min path paved by the boundary
comment); a92J06→a96J07 (cross-problem reuse, cited in source);
E10/a96J08 (consultation trail written INTO the artifact).
Warrant: **inductive-n=3**. Upgrade path: every relay hop; protocol
conformance becomes a driver-checked gate (N9).

## N3 — The memory store records learning

Contract: every session with extractable content yields reviewed,
tagged, attached memories; failed queries yield demand signals
(hunger audit). Certificates: 12 memories promoted 2026-08-03 (5 e9 +
4 j07 + 3 e10), all attached under author≠reviewer; scribe protocol
with hunger audit (scribe-protocol-hunger-audit.md); operator approval
retired (Joe 08-03). Warrant: write-side **inductive-n=3 sessions**;
extraction quality held without correction across all three passes.
Upgrade path: driver-triggered scribe per completed chain (N9).

## N4 — Agents consult memory when instructed

Contract: task framing (not invitation) elicits consultation.
Certificates: E10 controlled contrast — same agent, same store: 0
lookups under invitation (E9) vs 21 under the two-part frame
(13 recon + 8 mid-solve at event anchors), preregistered
(E9-pull-probe-prereg.md). Warrant: **inductive-n=1 controlled
contrast**. Upgrade path: every driver dispatch uses the two-part
frame; contrast accumulates for free.

## N5 — Retrieval serves the need when consulted

Contract: a hungry query in the asker's vocabulary returns the
on-point memories when they exist. Certificates AGAINST (the honest
state): four-layer anatomy (propensity/framing/affordance/
index-reach); E10 phase-A tag queries empty in contour vocabulary;
psr index noise reproduced twice. Certificates FOR: fix-4 first
positive (e9 memory surfaced for a neighboring problem, correctly
graded marginal); demand-side tagging closed the E10 mid-solve
hunger exactly. Warrant: **WEAK — inductive-n=1 positive against a
4-layer diagnosis**. Upgrade path: repairs 2–4 (psr description line,
memory_search query param, tag backfill from hunger logs — claude-12
queue); hunger audit per scribe pass.

## N6 — Capability transports APM → held-out BPM

Contract: formally a transportability claim (selection diagrams;
E-book-of-why-complete B1, promoted 08-03). Warrant: **designed** —
identification requirement named, no witnesses. Upgrade path: BPM
held-out evaluation after continuous operation produces enough closed
problems; the engine supplies the transport receipt.

## N7 — Outcomes are mechanically scoreable

Contract: endpoint-preregistration-draft.md — delta-form endpoint
(stale-assignment guard), executed-witness-only, statement-fidelity
with voiding classes, §5 capture schema. Certificates: draft hardened
through two rounds (statement-fidelity §1b); capture 1a LANDED
(sweeper carries the endpoint/ fields, 2026-08-04 morning check);
1b (fidelity hashes) pending; §3 continuation decision with Joe.
Warrant: **designed→partially mechanical**. Upgrade path: 1b lands;
§3 decided; driver runs the sweep per chain (N9).

## N8 — The process learns at the ability level

Contract: practice memories promote to packet-template deltas
(deltas-not-silent-edits, generalized to behavior). Certificates:
mechanism designed 08-03; all 08-03 ability transfers (event-anchored
lookups, ignore-and-move-on, boundary protocol) traveled through
hand-authored packets — the channel exists, the automation does not.
Warrant: **designed**. Upgrade path: packet templates become versioned
repo artifacts (part of N9 build); scribe passes propose deltas.

## N9 — The pipeline runs continuously (the constructive content)

Contract: a driver — attributable (registered identity), ledgered,
parkable, inspectable (Joe's cron-disable requirements, 08-03) —
executes: select problem → two-part dispatch (zai starter) →
classify {closed | conforming-partial | defective} → closer chain
(codex, ≤3 hops, §3 continuation recording) → mechanical gates run by
the driver (build, sorry count, axiom check, statement hash) →
CLAUDE REVIEW ONLY where judgment is irreducible (fresh-statement
fidelity adjudication; anomalies) → scribe + hunger audit + promotion
→ capability-proof update (this document) → next problem.
Certificates: every component exercised by hand 2026-08-03 (the full
factorial IS the hand-run of this loop); driver v0 built 2026-08-04
(six modules, six adversarial reviews); supervised trial COMPLETE
same day — chain 1 witnessed the safety branch (defective→anomaly→
abandon, two instrument fixes harvested), chain 2 witnessed the full
relay (partial→closer→closed→fidelity→scribe→promotion→self-updating
capability log; Steinhaus banked, commit 318160d8). Warrant:
**inductive-n=2 chains, both branches composition-witnessed**.
Upgrade path: continuous mode under a durable parent; ams-* seat
retarget when Zone's Agency lands; the v1 salvage path for truncated
sessions. zone-joe migration = copy of this
workflow (Joe, separate session).

---

## Update log

- 2026-08-04 09:1x — document created; certificates through 08-03
  attached; N7 upgraded (1a capture verified landed); N9 build begins
  this session.
- 2026-08-04 14:5x — TRIAL CHAIN 1 (apm-a96A08-43faa622afcd, a96A08):
  outcome ABANDONED via the anomaly path — phase-B session died at
  43min/216 tool events; gate correctly classified the mid-work
  snapshot DEFECTIVE (build-failed, nonconforming boundaries);
  anomaly checkpoint belled claude-10; abandon verdict folded on
  restart (run.py anomaly-consumption fix 2a8532be). N9 witness: the
  SAFETY rails composed correctly end-to-end (select→dispatch→poll→
  gate→checkpoint→verdict→close, all ledgered). N4/N5 note: first
  ORGANIC memory citation in an artifact (Dirichlet-sinc memory,
  on-point for the problem, cited in the docstring by the runner).
  v1 need registered: salvage path for truncated sessions;
  capability-update append on verdict-folded closes.
- 2026-08-04 — chain apm-a96J02-b2567506a05d; problem a96J02; outcome closed; hops 1; commits ce07348, 318160d89257eab8482e8066e284afb91a7ec6ac, eb9fe1eeb5de945f63b94070d1ff5af4e0b1cc14.
