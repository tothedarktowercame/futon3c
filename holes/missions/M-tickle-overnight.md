# M-tickle-overnight — Tickle Overnight Math Discovery Loop

## Status: In Progress

## Motivation

Joe pays £400/month for OpenAI Pro + Claude Max. Idle agents leave money on
the table. The futon6 CT validation corpus has 313 PlanetMath category theory
entries ready for wiring diagram extraction. Tickle should orchestrate Claude
and Codex overnight to process this corpus — real mathematical research that
produces documented value.

This is the "one week in october" revisited, with sharp edges rounded off:
- Incremental processing (one entry at a time, not 800 at once)
- Claude reviews every extraction (not just bulk upload)
- Evidence trail documents every step (not fire-and-forget)
- Resumable — crashes don't lose progress
- Ground truth validation (topology.json provides baseline counts)

## What Was Built

### 1. Escalation → Restart (dev.clj start-tickle!)

The notify-fn in Tickle's escalate-config now:
1. Writes escalation to blackboard (existing behavior)
2. Emits escalation evidence to the store
3. Posts IRC notification
4. Calls `restart-agents!` via future (auto-restart, configurable)

New option: `:auto-restart?` (default true).

### 2. CT Work Queue (tickle_work_queue.clj)

New module that:
- Loads 313 CT entities from `futon6/data/ct-validation/entities.json`
- Loads full .tex bodies from `/home/joe/code/planetmath/18_Category_theory_homological_algebra/`
- Constructs extraction prompts using the golden wiring taxonomy (30 component types, 11 port types, 5 wire types, 32 labels)
- Tracks progress via evidence store (completed entities are skipped)
- Provides `next-unprocessed`, `queue-status`, `entity->issue`, golden data access

### 3. REPL Helpers (dev.clj)

- `ct-progress!` — show N/313 processed
- `run-ct-entry!` — process single entry (extract + review)
- `run-ct-batch! :n 50` — overnight batch run
- `FUTON3C_TICKLE_AUTOSTART=true` env var for auto-starting Tickle on boot

### 4. Integration Tests (tickle_work_queue_test.clj)

13 tests, 52 assertions covering:
- Entity loading (313 entries, field validation)
- .tex body loading from PlanetMath corpus
- Prompt construction (taxonomy + body)
- Review prompt construction (with ground truth)
- Issue synthesis (entity → orchestrator-compatible issue)
- Progress tracking (completed-entity-ids, next-unprocessed, skip logic)
- Evidence emission
- Complexity sorting
- Golden reference data access

Full suite: 960 tests, 3418 assertions, 0 failures.

## Usage

```clojure
;; Single entry
(dev/run-ct-entry!)

;; Specific entry
(dev/run-ct-entry! :entity-id "pm-ct-FunctorCategory")

;; Overnight batch (50 entries, ~2.5 hours at 3 min/entry)
(dev/run-ct-batch! :n 50)

;; Full corpus (~10 hours overnight)
(dev/run-ct-batch! :n 313)

;; Check progress
(dev/ct-progress!)
```

## Files

| File | Change |
|------|--------|
| `src/futon3c/agents/tickle_work_queue.clj` | New: CT work queue module |
| `test/futon3c/agents/tickle_work_queue_test.clj` | New: 13 integration tests |
| `dev/futon3c/dev.clj` | Modified: escalation→restart, REPL helpers, autostart |
| `holes/missions/M-tickle-overnight.md` | New: this mission doc |

## Next Steps

- [ ] Run first overnight batch (10 entries) to validate end-to-end
- [ ] Compare extraction results against golden reference outputs
- [ ] Tune extraction timeout (currently 5 min — may need more for long entries)
- [ ] Add morning report (summary of overnight work posted to IRC)
- [ ] Wire incoming arXiv math.CT data as additional work items
