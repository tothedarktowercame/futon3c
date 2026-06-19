# T-mesh-qa-misrouting — unified mesh + misrouting QA

Parent mission: `holes/missions/M-agency-hardening.md` (invariant #2 explicit
identity, #3 typed evidence, W3 audit). Dispatched by claude-6 (in-flight
mechanic), 2026-06-10. Reviewer: claude-6.

## Goal

Save the **mesh itself** — dispatch/bell/whistle **edges**, NOT agent-to-agent
turn content — in one queryable place, and check **misrouting invariants** over
it. Consumer: **QA of Agency** — proving bells and turns don't get misrouted
(crossed/missed bells). Builds on Car 1 (`:mesh-edge` coordination evidence) +
the invoke-jobs ledger; the data is already captured, this unifies it and adds
the checks.

## Deliverables

1. **Unified mesh reader** (Clojure ns, e.g. `src/futon3c/agency/mesh_qa.clj`):
   merge the invoke-jobs ledger (HTTP bell/whistle/invoke jobs) with `:mesh-edge`
   coordination evidence (direct-invoke paths) into one edge set:
   `{:edge-id :from :to :surface :kind :accepted-at :terminal-at
     :terminal-state :delivered? :delivery-surface :session-id :ok?
     :bellback-of}`. Dedup the HTTP-job edge vs its evidence twin.
2. **Misrouting invariant checker**: the MQ-* set below. Each returns
   `[{:invariant <id> :ref <job/edge-id> :detail <str>}]`.
3. **Logic-model FIRST** (futonic discipline; do before the checker impl):
   `test/futon3c/agency/mesh_qa_model_test.clj` — a core.logic/pldb (or
   data-driven) model over an abstract mesh trace asserting: a conforming trace
   yields 0 violations, and a per-invariant adversarial trace trips exactly that
   invariant. **This ratifies the invariant SET — bell claude-6 at this stage if
   any invariant feels under-specified before you build the full checker.**
4. **Read surface**: `GET /api/alpha/coordination/qa?limit=N` → current
   violations + per-invariant counts. (Optionally also `mesh_trace.py --qa`.)
5. **Impl tests** for the reader + each invariant.

## Invariant set (MQ-*) — the spec

- **MQ-1 Delivery integrity** — every terminal job has a delivery record to its
  caller (no terminal-but-never-delivered past a grace window). [silent drop]
- **MQ-2 No orphaned bells** — every accepted job reaches a terminal state or is
  explicitly stale/timeout within its timeout window. [missed bell]
- **MQ-3 Caller attribution** — a bellback/completion edge's `:to` equals the
  original job's `:caller`. [completion crossing]
- **MQ-4 Recipient fidelity** — a job dispatched to X executed against X's
  session (the `:session-id` that served it is consistent with X's registered
  session, not another agent's). [session-file crossing]
- **MQ-5 Surface return** — a job's `:delivery-surface` matches its originating
  `:surface`. [REPL reply crossing — Joe's symptom #1]
- **MQ-6 Ordering** (OPTIONAL — may defer to the Car-3 queue work) — per
  recipient, terminal order matches accepted order.

## Honesty rule (important)

Use only fields already captured (`:caller :surface :state :events :delivery
:session-id`). If an invariant needs a field we DON'T capture yet, **report that
as a QA capture-gap finding — do NOT fabricate the field or weaken the invariant
silently.** The gap is itself a valuable result.

## Gates (before bell-back)

- clj-kondo clean (no new warnings vs base); `futon4/dev/check-parens.el`;
  `clojure -X:test` green (model + impl nses).

## In-flight constraints (HARD — live collab on this JVM)

- No JVM restart/kill/pkill, no Drawbridge reload — claude-6 does the live reload
  later. You only edit + `clojure -X:test`.
- **Branch from the Car-1 base** (it carries the `:mesh-edge` schema you need):
  `git worktree add ../futon3c-mesh-qa -b codex/mesh-qa codex/mesh-edge-coverage`.
  Commit there. No push, no merge to master. Keep any `http.clj` edit minimal
  (the new GET route only) — multiple reviewed branches touch http.clj.
- (FYI: claude-6 has a pending one-line defensive filter on the Car-1 base —
  `recent-mesh-edges` should drop entries lacking `:edge/from`. Doesn't affect
  your work.)

## Done = bell claude-6 back

`python3 futon3c/scripts/agency_send.py --from codex-1 --to claude-6 --kind bell`
with: branch + sha(s), gate results, changed files, the unified-edge schema you
chose, and — per invariant — whether it's fully checkable from current data or
revealed a capture-gap.
