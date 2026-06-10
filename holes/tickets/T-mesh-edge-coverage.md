# T-mesh-edge-coverage (Car 1) — make every agent→agent invoke visible in the mesh

Parent mission: `holes/missions/M-agency-hardening.md` (W3 evidence/audit;
invariant #3 typed evidence). Dispatched by claude-6 (in-flight mechanic),
2026-06-10. Reviewer: claude-6.

## Problem

`scripts/mesh_trace.py` reads the invoke-jobs ledger, which is populated ONLY by
the HTTP endpoints (`/api/alpha/bell|whistle|invoke`). Several paths call
`registry/invoke-agent!` **directly** and bypass the ledger, so those edges are
invisible to the mesh:

- `dev/futon3c/dev.clj` IRC dispatch relay (~line 4635) — caller = IRC nick.
- `src/futon3c/social/dispatch.clj` `direct-invoke` (line 242) — caller = message sender.
- `src/futon3c/social/whistles.clj` `whistle!` (line 115) — caller = whistle sender.
- (lower priority, follow-up: `agents/tickle_orchestrate.clj`, `dev/.../apm_conductor*.clj`.)

## Design (decided — do NOT redesign; implement this)

**Do NOT modify `registry/invoke-agent!`** (it is the hottest function in a live
system). Instead add a thin recorder in the **social layer**, which already
legitimately depends on both the registry and the evidence store (see
`social/bells.clj`, which emits `:coordination` evidence):

1. New ns `src/futon3c/social/coordination_ledger.clj` (or extend an existing
   social ns if a better home is obvious — justify in the bell-back):
   - `(record-invoke-edge! {:keys [from to surface kind at]})` — append ONE typed
     `:evidence/type :coordination` entry to the evidence store, edge-shaped:
     `{:edge/from from :edge/to to :edge/surface surface :edge/kind :invoke
       :edge/at at}`. Mirror the evidence shape used in `bells.clj`
     `make-bell-evidence` (tags `[:coordination :mesh-edge]`).
   - `(invoke-with-edge! {:keys [from to surface prompt timeout-ms]})` — record the
     edge, then call `reg/invoke-agent!`, then record a terminal outcome edge
     (`:edge/kind :invoke-result`, `:edge/ok? bool`). Returns invoke-agent!'s map
     unchanged.
2. Migrate the three primary bypass entry surfaces to call `invoke-with-edge!`
   with the real `from`/`surface` instead of `reg/invoke-agent!` directly:
   - IRC relay (dev.clj): `from` = sender nick, `surface "irc"`.
   - `social/dispatch.clj` `direct-invoke`: `from` = `(:msg/from classified-message)`
     (or the best available sender field), `surface "dispatch"`.
   - `social/whistles.clj`: `from` = whistle caller if available else `"whistle"`,
     `surface "whistle"`.
3. Expose a read surface so `mesh_trace.py` can see these edges:
   `GET /api/alpha/coordination/edges?limit=N` → recent mesh edges as JSON
   (from/to/surface/kind/at/ok?). Wire it in `transport/http.clj` routing next to
   the existing invoke-jobs endpoints.
4. Extend `scripts/mesh_trace.py` to merge the coordination edges with the
   invoke-jobs view (dedupe by `(from,to,at±2s)`); label each row's `surface` so
   IRC/dispatch/whistle/bell edges are distinguishable. Keep the existing
   invoke-jobs source — this ADDS a source, does not replace it.

## Logic-model FIRST (futonic discipline — do before writing impl)

Before the impl, write a tiny `core.logic`/pldb (or a plain data-driven) model
asserting the invariant: **for every `invoke-with-edge!` call, exactly one
`:invoke` edge is recorded with from=caller and to=target, and exactly one
`:invoke-result` edge with the same (from,to)**; and an adversarial trace where a
caller is nil/blank still records an edge with `from "unknown"` (never drops the
edge). Put it in `test/futon3c/social/coordination_ledger_model_test.clj`.
This verifies the DESIGN; the impl tests verify the code.

## Tests (`clojure -X:test`, offline)

- `record-invoke-edge!` appends a well-formed `:coordination` `:mesh-edge` entry.
- `invoke-with-edge!` records invoke + invoke-result edges around a stubbed
  `reg/invoke-agent!` and returns its result unchanged.
- nil/blank `from` → edge recorded with `from "unknown"` (never dropped).
- The three migrated call sites pass the right `from`/`surface` (unit-test the
  call-site arg construction where practical, or a smoke test).

## Gates (all must pass before bell-back)

- clj-kondo clean on changed `.clj` (no NEW warnings vs HEAD).
- `futon4/dev/check-parens.el` on changed Clojure files.
- `clojure -X:test` green (new model + impl test nses at minimum).

## In-flight constraints (HARD — live collaboration on this JVM)

- **No JVM restart / kill / pkill. No Drawbridge reload.** claude-6 does the live
  reload at a window Joe picks. You only edit + `clojure -X:test` (separate JVM).
- **Isolated git worktree** off **master**:
  `git worktree add ../futon3c-mesh-edges -b codex/mesh-edge-coverage`.
  Commit there. **No push, no merge to master** — claude-6 reviews the sha.
  (Note: an unrelated reviewed branch `codex/auto-bellback` touches http.clj
  routing; you will also add a route to http.clj — keep your http.clj edits
  minimal and localized to the new GET endpoint to ease the eventual merge.)
- Conflict markers parse as valid Clojure — if you rebase, grep
  `^(<<<<<<<|=======|>>>>>>>)` on changed files.

## Done = bell claude-6 back

`python3 futon3c/scripts/agency_send.py --from codex-1 --to claude-6 --kind bell`
with: branch + commit sha(s), gate results (clj-kondo / check-parens / test
counts), changed-file list, the new-ns home decision + rationale, and the
`from`/`surface` value chosen at each of the three migrated call sites.
