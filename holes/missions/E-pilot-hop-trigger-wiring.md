# Excursion: Pilot Hop-Trigger Wiring (war-machine-pilot ⇄ {street-sweeper, night-shift})

**Type:** Excursion (E-prefix; bounded scope-out from M-war-machine-pilot; cross-peripheral architecture).
**Status:** SPEC awaiting operator review BEFORE implementation (Joe's call).
**Date:** 2026-05-25
**Author + end-to-end owner:** claude-1 (inhabiting `:war-machine-pilot`; emacs-repl surface).
**Co-designed with:** claude-2 (E-street-sweeper owner) via `~/code/storage/hop-wiring-scratch.md` Q1/A1.
**Parent mission:** `futon3c/holes/missions/M-war-machine-pilot.md`.
**Trigger:** whistle from claude-2 (emacs-repl 2026-05-25): "The WM self-watch surface STILL shows 'Working-tree pressure is stop-the-line / CRITICAL'... the responsible peripheral (street-sweeper) isn't being invoked via hop-trigger from your pilot side."

## Why this exists

Two sibling excursions (`E-street-sweeper`, `E-night-shift`) have landed substantial v0 capability — invariant-enforced envelopes, tests passing, work landed via top-level orchestration — but their **integration with the war-machine-pilot peripheral has not landed**.  The shared last-mile is the hop mechanism: how does a pilot inhabiting `war-machine-pilot` transition into `street-sweeper` (or `night-shift`) to execute a substantive cycle, then transition back?

Currently the sweepers run as top-level orchestration (`futon3c.peripheral.street-sweeper/run-full-sweep`).  That works but bypasses the pilot envelope — the cg-id discipline, the Pilot-I1 substantive-arg checks, the inhabitation log, the WM live-recommendation surface.  Joe's framing ("the WM is fit for inhabitation") is partially proven by the live cycle on `cg-5b03db29` / `cg-58271911` — but only INSIDE the war-machine-pilot peripheral; the hop boundary is the place pilot inhabitation currently can't reach.

The visible symptom: `:commit-hygiene :working-tree-channel :tier :stop-the-line` is honest data; the stop-the-line banner is the action prompt; but there's no button/tool the pilot can invoke that hops into `street-sweeper` to clear the pressure.  (See `core.cljs:1282` `stop-the-line-banner` — the CTA is missing.)

## Scope

### (1) `agency.registry` bidirectional hop primitives (per claude-2 A1)

In `futon3c/src/futon3c/agency/registry.clj`:

- **Agent record** gains two fields:
  - `:agent/current-peripheral <peripheral-id-or-nil>` — what peripheral the agent is currently inhabiting; nil if not in any
  - `:agent/hop-stack [<prev-peripheral-id> ...]` — vector; push on hop-in, pop on hop-back
- **Peripheral record** (existing `:type :peripheral` entries) gains one field:
  - `:agent/current-inhabitant <agent-id-or-nil>` — what agent is currently inside this peripheral; nil if vacant

The bidirectional pointer enables O(1) reverse lookup ("who's in `street-sweeper` right now?") which is needed for **foreign-hop-in rejection** once Tickle starts firing hop-prompts (see [[project_tickle_as_operator_model]]).

### (2) `hop!` + `hop-back!` operations (single atomic swap)

Two new public fns in `agency.registry`:

```clojure
(hop! agent-id new-peripheral-id) → {:ok true :from <prev-peri> :to <new-peri>}
                                  | {:ok false :error :peripheral-occupied :by <other-agent>}
                                  | {:ok false :error :agent-not-registered}
                                  | {:ok false :error :peripheral-not-registered}

(hop-back! agent-id)              → {:ok true :from <new-peri> :to <prev-peri>}
                                  | {:ok false :error :hop-stack-empty}
                                  | {:ok false :error :agent-not-registered}
```

Both are **single `swap! !registry`** with all four field updates atomic (no consistency window):

For `hop!`:
1. push prev peripheral onto agent's `:hop-stack`
2. set agent's `:current-peripheral` to new
3. set new peripheral's `:current-inhabitant` to agent (rejecting if non-nil + non-equal)
4. clear prev peripheral's `:current-inhabitant` (only if it was the agent's — defensive)

For `hop-back!`: reverse the same swap.

### (3) Inhabitation-log emission

Each successful `hop!` / `hop-back!` emits a `:event :hop-in` / `:event :hop-out` entry into `futon5a/data/pilot-inhabitations.edn` (per E-street-sweeper success-criterion §7).  Carries:

```clojure
{:id "hop/<agent>/<peripheral-from>-to-<peripheral-to>/<timestamp>"
 :at  "<ISO-8601>"
 :event :hop-in | :hop-out
 :pilot-agent <agent-id>
 :from-peripheral <from-id>
 :to-peripheral   <to-id>
 :cited-consent-gate-event-id <cg-id-required-for-substantive-hops>
 :driver :pilot-hop-trigger}
```

Pilot-I1 requires a cg-id citation for substantive cycles; the hop itself is the boundary of "substantive" — the cg covers the *intended* cycle in the destination peripheral, not the hop transition.

### (4) `core.cljs` `stop-the-line-banner` CTA wiring

In `futon2/web/war-machine/src/war_machine/client/core.cljs` `stop-the-line-banner` (around line 1282):

- Add a CTA button when the override-mode is `:stop-the-line` AND a registered peripheral can address the cause.  Map causes → destination peripherals:
  - `:working-tree` / `:commit-hygiene` → `street-sweeper`
  - `:night-shift-pending` / `:branch-isolation-pending` → `night-shift`
  - default (no matching peripheral) → no CTA (current behaviour)
- Button label: e.g. `"Hop into street-sweeper (clear working-tree pressure)"`
- Click handler: POST to a new endpoint `/api/alpha/agency/hop` with `{:agent-id <current-agent> :to-peripheral <destination>}`
- Surface the response inline: `Hopped into street-sweeper` or error reason

### (5) HTTP endpoint `/api/alpha/agency/hop` + `/api/alpha/agency/hop-back`

In `futon3c/src/futon3c/transport/http.clj`: thin wrappers around `agency.registry/hop!` and `hop-back!`.  Accept the same agent-id + peripheral-id + optional cg-id payload.  Return the registry result + the emitted inhabitation-log entry.

### (6) Pilot-side tool: `:hop-trigger`

In `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj`: add `:hop-trigger` as a substantive tool (Pilot-I1 checks apply — requires `:consent-gate-event-id`).  The tool itself calls `agency.registry/hop!` directly (in-process) and returns the result.  The HTTP endpoint from §(5) is for cljs-side CTA invocation; the in-process tool is for direct pilot-Drawbridge invocation.

### (7) Hop-back protocol

The destination peripheral (`street-sweeper` / `night-shift`) calls `agency.registry/hop-back!` when its cycle completes.  Wiring: each peripheral's backend gets a `hop-back!` invocation at the end of its main `run-*` function — guarded so direct top-level orchestration (`run-full-sweep` outside a hop) is a no-op.

## What this excursion does NOT own

- **Refactoring the existing top-level orchestration entry points** (`run-full-sweep` etc.) — those continue to work for direct invocation; this excursion ADDS the hop path, doesn't replace.
- **Tickle integration** (firing hop-prompts to the pilot when stop-the-line warrants a sweep) — that's the third intervention shape in [[project_tickle_as_operator_model]] and a separate excursion.
- **Cross-agent hops** — hops are within one agent across peripherals (per I-1: one agent = one session).  An "agent A hands off to agent B" pattern is a bell/handoff, not a hop.  Foreign-hop-in rejection protects this invariant.
- **The E-wm-staleness-meta-stop / mana-snapshot scheduler work** — that's E-street-sweeper's sibling-band-aid and addresses a different staleness path.

## Implementation plan (claude-1, after operator review)

| Step | Where | Status |
|---|---|---|
| (1) Add `:current-peripheral` + `:hop-stack` + `:current-inhabitant` fields in `register-agent!` (default nil/[]) | `agency/registry.clj` | pending |
| (2) Implement `hop!` + `hop-back!` with single atomic swap | `agency/registry.clj` | pending |
| (3) Tests covering: vacant→occupied; occupied-by-self (legal hop-back); occupied-by-other (rejected); hop-stack empty on hop-back; bidirectional consistency post-swap | `test/futon3c/agency/registry_test.clj` | pending |
| (4) Emit inhabitation-log `:event :hop-in` / `:hop-out` entries | new fn in `agency/registry.clj` or sibling | pending |
| (5) HTTP endpoints `/api/alpha/agency/hop` + `/api/alpha/agency/hop-back` | `transport/http.clj` | pending |
| (6) cljs CTA in `stop-the-line-banner` + cause→peripheral map | `futon2/web/war-machine/.../core.cljs` | pending |
| (7) `:hop-trigger` tool in pilot backend | `peripheral/war_machine_pilot_backend.clj` | pending |
| (8) `hop-back!` invocation in street-sweeper + night-shift run-* functions | `peripheral/street_sweeper.clj`, `peripheral/night_shift.clj` | pending |
| (9) End-to-end demo: pilot → hop into street-sweeper → run sweep → hop-back → inhabitation log shows both legs | running JVM + Playwright | pending |
| (10) Append `DONE: <sha>` to `~/code/storage/hop-wiring-scratch.md` | both repos | pending |

cg-id: to be minted at start of implementation cycle.  Parent: TBD (likely sibling to `cg-58271911`).

## Success criteria

1. **Registry primitives**: `hop!` and `hop-back!` exist, atomic, with foreign-hop-in rejection; tests green.
2. **Bidirectional consistency**: `(:current-peripheral agent)` = X iff `(:current-inhabitant (get-peripheral X))` = agent.  Property test asserts post-every-hop.
3. **Pilot tool wired**: `:hop-trigger` callable from war-machine-pilot peripheral with Pilot-I1 cg-id citation enforcement.
4. **HTTP endpoint live**: `curl -X POST /api/alpha/agency/hop -d '{...}'` returns success with inhabitation-log entry.
5. **CTA visible**: stop-the-line-banner renders a hop-trigger button when override-mode is :stop-the-line AND cause maps to a registered peripheral.  Verified via Playwright.
6. **End-to-end cycle**: one full demo of pilot → hop → cycle → hop-back recorded in `pilot-inhabitations.edn` with both `:hop-in` and `:hop-out` entries; the working-tree-channel `:tier` shifts away from `:stop-the-line` after the swept commits land.
7. **No regression**: existing top-level orchestration (`run-full-sweep` direct invocation) still works; existing peripheral tests green.

## Open design choices for operator review

1. **cg-id requirement on the hop itself**: My read — the cg covers the *intended cycle in destination*, not the hop.  Pilot-I1 applies to substantive actions inside the destination peripheral; the hop transition is plumbing.  If you disagree, the hop fn signature gains a required `:consent-gate-event-id` arg.

2. **Foreign-hop-in rejection severity**: claude-2's spec rejects when destination's `:current-inhabitant` is non-nil + non-self.  Should this be a hard reject, or soft (warn + proceed, evict the prior inhabitant)?  Hard is safer; soft would surface starvation issues sooner.  Recommendation: hard for v1.

3. **Hop-back on cycle abort**: if a peripheral cycle errors mid-flight, who calls `hop-back!`?  Options: (a) peripheral's error handler always tries hop-back; (b) timeout-based janitor in `agency.registry` that hop-backs agents stuck in a peripheral past TTL; (c) operator explicit hop-back via API.  Recommendation: (a) + (b) belt-and-braces.

4. **Inhabitation-log entry on hop**: do `:hop-in` / `:hop-out` events appear in the Inhabitation Log card's WIP/Done counts, or are they filtered out?  My read: they're transitions, not cycles — filter from WIP/Done counts but include in `:all-events`.

## Cross-references

- `~/code/storage/hop-wiring-scratch.md` — live collab scratch with claude-2; Q1/A1 resolved this excursion's central design question
- `futon3c/holes/missions/E-street-sweeper.md` §"Hop protocol (war-machine-pilot ⇄ street-sweeper)" — origin of p1 + the receiving-side state claude-2 has already prepared
- `futon3c/holes/missions/E-night-shift.md` — parallel destination peripheral; same hop mechanism applies
- `futon3c/holes/missions/M-war-machine-pilot.md` — parent; the hop is the integration this mission's pilot peripheral has been waiting for
- `futon3c/src/futon3c/agency/registry.clj` — implementation site for primitives
- `futon3c/resources/peripherals.edn` — peripheral catalogue (existing explore/edit/test/deploy/reflect entries; war-machine-pilot, street-sweeper, night-shift live here too)
- `futon2/web/war-machine/src/war_machine/client/core.cljs:1282` — `stop-the-line-banner` CTA site
- `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj` — pilot backend; new `:hop-trigger` tool will land here
- `futon3c/src/futon3c/peripheral/street_sweeper.clj` + `night_shift.clj` — destination peripherals; `hop-back!` invocation will land here
- [[project_consent_gate]] — consent-gate-event-id discipline; relevant to design choice #1
- [[project_tickle_as_operator_model]] — Tickle's deflection-challenge mode would fire hop-prompts; foreign-hop-in rejection protects this
- [[project_e_prefix_excursions]] — naming/ownership convention

## Provenance

- Symptom surfaced: claude-2 whistle (emacs-repl 2026-05-25), "the WM self-watch surface STILL shows 'Working-tree pressure is stop-the-line / CRITICAL'"
- Q1 (where does `:current-peripheral` live?) authored by claude-1 in scratch
- A1 (go bidirectional; agent-side hop-stack; single atomic swap; foreign-hop-in rejection) by claude-2 in scratch
- This excursion file authored: claude-1, 2026-05-25, awaiting operator review BEFORE implementation per [[project_consent_gate]] discipline (cross-peripheral architecture is broader-blast-radius than per-cycle pilot work)
