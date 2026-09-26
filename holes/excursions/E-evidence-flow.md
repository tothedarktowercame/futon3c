# E-evidence-flow — where evidence is written, where it is read, and whether streams ever merge

**Status: IDENTIFY (2026-07-05). Chartered by Joe (emacs-repl), probes + write-up by
claude-18. Owner: TBD (single-agent, per E-prefix convention).**

A bounded excursion: establish the *actual* topology of evidence flow across the futon
mesh (laptop/Dionysus, lucy/linode1, metameso/linode2), then decide what it *should* be.
IDENTIFY names the gap and records tonight's probe results; it does not commit a design.

## The gap

Joe's working assumption (2026-07-05): *laptop evidence duplicates into the Linode
evidence store; Linode collates multiple streams.* First probes say **this is not what
happens** — but the probes also surfaced enough anomalies that the true topology needs
mapping before anything relies on it (live pattern attestation already had to pin
`localhost` to avoid counting against the wrong store; see below).

## Probe results (2026-07-05, ~00:45, both stores live)

`GET /api/alpha/evidence?limit=3000` against each:

| | laptop (localhost:7070) | lucy (172.236.28.208:7070) |
|---|---|---|
| entries returned | 3000 (limit hit) | **311 (total < limit)** |
| span of returned | Jul 3 → Jul 5 (07:01, minutes old) | **Jul 4 only**, latest 22:21 (nothing today) |
| context-retrieval | 330 | 5 |
| top authors | portfolio-inference (1170), claude-16, claude-10, joe, mission-control/sync | portfolio-inference (104), claude-10, codex, joe, war-machine |

Reading: the stores are **different sizes, different recencies, different mixes** — not
duplicates. Laptop is high-volume and hot (~1,500 entries/day). Lucy's stream is thin
and a day stale. Overlapping author *names* on both sides ≠ overlapping *entries*
(unverified — see Q3).

## What the code says (same night)

- **No replication mechanism exists** in `futon3c/src/futon3c/evidence/` — no
  mirror/forward/sync path. BUT `xtdb_backend.clj:63` says "…for replicated entries
  that arrive after a delay" — replication was *anticipated* in the backend's design
  and apparently never built. (Archaeology target: the commit that wrote that line.)
- **Writers write locally.** In-JVM writers (memory-backend tools, turn evidence,
  discipline.clj PSR/PUR paths) go through `estore/append!` → the local store of
  whichever JVM they run in. Laptop agents → laptop store; lucy-resident processes →
  lucy store. Hence two independent accumulations.
- **Readers read wherever `FUTON3C_EVIDENCE_BASE` points — which on the laptop is
  lucy.** `dev-laptop-env` line ~76 defaults `FUTON3C_EVIDENCE_BASE` to
  `FUTON3C_LINODE_URL` (172.236.28.208:7070). Consumers found: `futon0` reports
  (pattern-density, joe_hud), `cx`, `eoi-new`, ngircd_bridge, codex-autowake/-picker.
  So the laptop's own dashboards/reports read the THIN REMOTE stream by default while
  the laptop JVM accumulates the hot one. This already bit once: the first
  pattern-attestation refresh counted 5 events instead of 8,114 until
  `refresh_pattern_attestation.sh` pinned localhost (futon6 commit 2026-07-05).

## Anomalies to explain (each is a check, not a conclusion)

1. **Q1 — Who writes `portfolio-inference` evidence, and why is it still the top
   author on BOTH stores** when portfolio-inference was retired in the 2026-07-04
   decommission session? (Either the retirement missed a writer, or the author tag
   outlived the feature on some other process. Relates to E-feature-constellation's
   liveness axis.)
2. **Q2 — What populates lucy's store at all?** Its Jul-4 entries include codex and
   war-machine authors: are these lucy-resident processes writing locally, or laptop
   processes that POST remotely through some path not found tonight?
3. **Q3 — Entry-level overlap:** sample N evidence/ids present on both stores. Zero
   overlap ⇒ cleanly disjoint streams; partial ⇒ some path does copy (find it).
4. **Q4 — The intended design:** find the commit/mission behind `xtdb_backend.clj:63`'s
   "replicated entries" comment; was collation-at-lucy (Joe's assumption) a designed
   target that stalled?
5. **Q5 — metameso (linode2):** does it have a store, and does anything read or write it?

## Why it matters (consumers already coupled to the answer)

- **Live pattern attestation** (futon6 daily job) counts context-retrieval on the
  laptop store only. If lucy-resident agents' retrievals matter, they're invisible.
- **derive-pattern-activations** (WM endpoint) walks the local store; the WM UI's
  activation numbers have the same blind spot.
- **The Joe HUD and futon0 reports** read lucy by default — currently the thin stream.
- Any future "evidence landscape" claims (R-contract, faithfulness work) inherit
  whichever topology is real.

## What would move this to MAP/DERIVE (not committed here)

A verified flow diagram (who appends where, who reads where, with counts), answers to
Q1–Q5, and THEN the design question: duplicate-and-collate (Joe's assumption, made
true), federate-on-read (readers query both), or partition-by-concern (each store owns
a timescale). Each has different failure modes for the attestation/WM consumers above.

## Scope boundary (bounded excursion)

- Read-only probes against both stores; no schema changes, no replication built under
  this charter.
- Never restart either serving JVM (I-0); lucy access via HTTP GET only.
- A clean kill is a success: "streams are intentionally disjoint and consumers just
  need their bases pinned" is an acceptable finding — then this closes into
  documentation plus a handful of base-URL fixes.
