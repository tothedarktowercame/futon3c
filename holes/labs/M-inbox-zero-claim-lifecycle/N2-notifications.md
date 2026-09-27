# N2 — backlog consumer resolved; routing evidence re-qualified (C8)

**Supersedes [N1-notifications.md](N1-notifications.md) (3db44558), which is
corrected, not implemented.** Mission C8 / checkpoint e00730bf. Design only;
no production, runtime, notification, or registration change in this packet.

## 1. Corrections to N1 (owner findings, verified)

- **C1 (E1/E2 demoted).** N1 called exact-session claims and confirmed
  attributions "authorship-grade" for *current* dirty bytes. Wrong — this
  mission exists because exactly such a record authorized another agent's
  dirt. N2: claim/confirm records are **historical citations only**. They may
  appear in a backlog row as "file carried seat X's past edit witness" and
  may never, under the current schema, select a personal cleanup recipient.
  There is no live E1 mechanism in the current schema and N2 does not
  pretend one exists; the claim-lifecycle DERIVE is what would create one.
- **C2 (E3 removed).** Verified: `futon3/inbox-zero-lib/.../escalation.clj:62`
  `responsible-seat` reads the item/plan, `:99-115` falls back to a literal
  `street-sweeper` at tier 2, and **no triage-role registration exists**.
  Live probe: `GET /api/alpha/agents/street-sweeper` →
  `{"ok":false,"error":"Agent not found: street-sweeper"}` (2026-09-27).
  Escalation ledger rows are past routing outputs, not standing grants. A
  real grant would need explicit provenance, scope, expiry, and session
  binding — **absent today**; N2 uses no E3 routing. N1's claim that
  `agency/inbox.clj` job payloads carry session ids is also corrected: live
  `GET /api/alpha/invoke/jobs?limit=2` shows `"session-id": null` on a real
  current job. Job-ledger windows therefore **cannot** be session-matched;
  N1's E4 category collapses into E5 (weak diagnostic). All temporal
  overlap is weak diagnostic.
- **C3 (consumer).** N1 declared the backlog file "a consumed surface" while
  noting nothing reads it. Unsupported; corrected in §2.

## 2. The real consumer chain (verified end to end)

An existing operator-facing surface already displays **repo-level dirty
pressure with no authorship semantics**:

1. `futon0/scripts/mana-snapshot.bb` writes the mana snapshot (per-repo
   `{repo, P, count, max-age-days, total-bytes, tier}`; refreshed by
   `futon3c/peripheral/street_sweeper.clj:242-246`).
2. `futon2/scripts/futon2/report/war_machine.clj:4019-4084`
   `scan-metabolic-balance` reads it; `:4086-4094` `build-commit-hygiene`
   projects per-repo queues whose own docstring states they expose
   repo-level queues *instead of pretending* safe attribution; `:4421-4450`
   renders "Commit Hygiene" (Repo/Tier/Pressure/Dirty/Max-age/Action).
3. `futon3c/wm/scheduler.clj:245-251` caches the snapshot;
   `transport/http.clj:8411` serves `GET /api/alpha/war-machine`.
4. Consumers: the War Machine UI, and programmatically
   `peripheral/street_sweeper_backend.clj:181-197`
   `list-repos-with-pressure` + `:199-217` `current-metabolic-pressure`.

Live probe today: the route answers `503 war-machine-snapshot-unavailable`,
scheduler `running? false` — **the surface exists with real readers, but its
producer pipeline is currently down.** This is a fact the contract must
state, not hide.

## 3. The routing/visibility contract

- **Personal cleanup notices: suspended.** No current-schema evidence
  authorizes "commit or delete" to anyone. The sweeper's personal lane stops
  emitting (config flag, default off), rather than re-wording the same
  assignment. Historical citations move into backlog rows.
- **Uncertain pressure displays repo-level on the commit-hygiene queue** —
  whose semantics are already authorship-free and repo-wide, so the mixed
  known/unknown case is native (no `empty? targets` gate anywhere in N2).
  Per repo, the sweeper contributes an `:uncertain` block:
  `{:count :untracked :newest [paths≤5] :diagnostic-overlaps {path [agents]}}`
  where overlaps are labeled diagnostic, never assignment.
- **Backlog file kept** as the per-file drill-down; each queue row's Action
  column already exists and gains the backlog path for that repo.
  `operator-backlog.edn` gets rows for **every** repo with uncertain
  entries (mixed included), citing historical claim/confirm records as
  citations with "bytes unverified".
- **Volume:** the queue is repo-granular, tiered, and capped by existing
  floor/threshold semantics — no per-file operator judgement demand is
  created (escalation policy: volume never selects recipients, and N2 adds
  no new message class at all).
- **Dedupe / rollover:** with personal notices suspended, the notices
  ledger is inert; backlog remains current-state-rewritten (no dedupe
  needed); session rollover is moot for routing because nothing routes to
  sessions. When the claim lifecycle later authorizes a personal lane, N1's
  session-match rule still applies as its gate.

## 4. Injection seam — owner decision needed (2 grounded options)

The chain crosses futon0 → futon2 → futon3c. The sweeper's `:uncertain`
block needs one merge point:

- **Option A — producer-side (futon0).** Sweeper writes
  `storage/inbox-zero/uncertain-pressure.edn`; `mana-snapshot.bb` merges it
  into per-repo detail. *Pros:* single producer, queue/render/UI need no
  change beyond column display; semantics clean. *Cons:* touches futon0
  tooling; snapshot cadence governs freshness; schema change in the .bb
  contract.
- **Option B — serve-side overlay (futon3c only).** Sweeper writes the same
  EDN; `transport/http.clj` `handle-war-machine` overlays
  `:commit-hygiene :queues` with `:uncertain` at serve time, marked
  `:overlay/source :inbox-zero-sweeper`. *Pros:* one repo, no futon0/futon2
  change, testable against the handler with an injected snapshot atom.
  *Cons:* served payload diverges from the futon2 producer (documented
  overlay); UI text render (futon2-side) won't show it unless the UI reads
  the served JSON (it does — but the markdown synthesis path won't).
Both preserve "no personal assignment"; A is semantically cleaner, B is the
smaller change. **Decision needed from owner: A or B**, plus whether
reviving the currently-down WM scheduler is in scope for N-implementation
or a separate operational act (it is a runtime start, not this packet).

## 5. Joe's example under N2

claude-1: **no notice** (personal lane suspended). futon3c-d: one
commit-hygiene queue row (repo-level, as today when pressure crosses the
floor) showing dirty=10 with `:uncertain {:count 10}`; the backlog file
lists all 10 paths newest-first with diagnostic overlaps
`[codex-4, kimi-9, 象]` labeled non-authorship, and — for
`s0-git-transaction.py` — a historical citation to packet `727518e6`
marked "history, not current-bytes evidence". Actual owners: unknown.

## 6. Smallest implementation packet (after A/B decision)

- `sweeper.clj`: remove overlap→recipient attribution from the notice path;
  compute `:uncertain` per repo; write uncertain-pressure EDN (atomic);
  backlog rows for all uncertain repos; personal lane behind
  `FUTON3C_INBOX_ZERO_PERSONAL_NOTICES` default-off.
- Option A: `futon0/scripts/mana-snapshot.bb` merge + futon2 render column.
  Option B: `http.clj` overlay in `handle-war-machine`.
- Tests: `sweeper_test.clj` — overlap produces backlog+uncertain EDN, zero
  deliveries; mixed-repo backlog; citation formatting. Option B adds a
  handler test: injected snapshot atom + overlay file ⇒ served JSON
  contains `:uncertain`, proving producer→consumer wiring. Option A adds
  the equivalent .bb merge test.
- `xiang2000_p6o3.py:28`: unchanged (historical INBOX notices stay
  classifiable); no new notice template is introduced in N2, so no regex
  break. If a future personal-lane template appears, the union plan from
  N1 §5 applies then.
- Checks: clj-kondo, check-parens, `-n futon3c.inbox-zero.sweeper-test`,
  py_compile/unittest for scripts touched.

## 7. Acceptance matrix (C8, revised)

| Case | Behavior |
|---|---|
| Unrelated simultaneous agents | zero personal notices; overlap is diagnostic text in backlog |
| Declared triage responsibility | none exists (verified absent); no routing invented |
| Uncertain files visible/actionable | commit-hygiene queue row + backlog drill-down, real consumers |
| Session rollover | moot — nothing routes to sessions |
| Dedupe | current-state backlog; no notice ledger traffic |
| Mixed known/unknown repo | repo-wide queue semantics; backlog rows for all uncertain repos |

**Return:** consumer contract resolved (§2 chain, §3 contract); one narrow
owner decision outstanding — injection seam A vs B, and WM-scheduler
revival scope. No phase verdict; no implementation authorized by this
packet.
