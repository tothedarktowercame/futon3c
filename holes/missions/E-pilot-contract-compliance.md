# Excursion: Pilot Contract Compliance (does the WM pilot actually run its cleaning phase?)

**Type:** Excursion (E-prefix; bounded scope-out). Captures a *finding* + a *use case*, not yet a build.
**Status:** IDENTIFY — finding recorded; "run-via-WM" use case captured; cron deferred.
**Date:** 2026-06-15
**Author:** claude (Opus 4.8, paired with Joe during an interactive repo-hygiene sweep).
**Parent mission:** `futon3c/holes/missions/M-pilot-appearance.md` — the pilot "shows up well" across its registers.
**Sibling excursions:** `E-street-sweeper.md` (the cleaning capability), `E-pilot-hop-trigger-wiring.md` (the hop mechanism).

## Why this exists

M-pilot-appearance is about the pilot *showing up well* — more than "can act." There is an implicit
**contract** in the design: when the War Machine judges `:stop-the-line` on the `:working-tree`
channel, the pilot is supposed to hop into `:street-sweeper` and run the cleaning phase, then hop back
(see `E-street-sweeper` trigger condition + `E-pilot-hop-trigger-wiring`). This excursion records the
finding that — in reality — **that contract is not honored autonomously: it is a manual loop.**

It surfaced on 2026-06-15: Joe and claude cleaned ~16 repos **by hand** (the exact job street-sweeper
automates — stage/commit/push with safety judgement), and Joe asked whether street-sweeper actually
runs in the WM loop. The answer is the substance below.

## Finding: the cleaning loop is manual (the War Machine side)

Confirmed against the running JVM and the code, 2026-06-15:

- **The capability is built and live.** `:street-sweeper` loads + health-checks in the running JVM
  (`spike-check` → `:valid-config? true`, all 22 invariants loaded: secret-patterns, exclusion
  patterns, file-count caps, the INV-15 auto-approve class, the defer-queue).
- **The hop mechanism partly landed** (the spec in `E-pilot-hop-trigger-wiring`): `registry/hop!` +
  `hop-back!`, the `POST /api/alpha/agency/hop` endpoint, the `:hop!` action in
  `war_machine_pilot_backend.clj`, and the `:current-peripheral`/`:hop-stack` registry fields exist.
  So an agent inhabiting `:war-machine-pilot` *can* hop in.
- **But there is no autonomous trigger.** The pilot does not auto-hop on `:stop-the-line`; there is
  **no cron, no systemd timer, no scheduler/tickle** invoking the sweep or the hop. The
  `stop-the-line-banner` CTA (`E-pilot-hop-trigger-wiring` §4) fires only on operator click. This is
  by design — **WM-I4: "the war machine doesn't act unilaterally."**
- **The end-to-end pilot path is still unverified.** The live `spike-check` still reports
  `:deferred-criteria [:C3-agency-registration-and-inhabitation :C7-hop-protocol-round-trip ...]` —
  the hop-from-pilot round-trip was never closed.
- **It has run for real exactly once**, in the 2026-05-25/27 build session: ~202 commits carry the
  `Co-Authored-By: ... via :street-sweeper` trailer (futon4 81, futon3c 63, futon2 55, futon0 3),
  with real defer-queue runs under `~/code/storage/sweeper-deferred/2026-05-25T18..19Z/`. Nothing
  since but test-suite invocations (`sweeper-deferred/test-*`).

**So:** the pilot *can* comply with its cleaning-contract (capability + hop both exist), but it does
**not** do so on its own — it needs a manual hop / REPL invocation, and in reality the WM cleaning
phase has not run in ~3 weeks. The strongest evidence is this very session: the cleaning was done by
hand. This is a pilot-appearance gap — a register at which the pilot is supposed to act on its own
contract but currently does not.

## Possible use case: run street-sweeper *via* the War Machine

Joe's proposal (2026-06-15): rather than a raw cron that bypasses the pilot envelope, route the
trigger **through the War Machine** — send the WM a **"run street sweeper"** command:

- **If a pilot is currently inhabiting `:war-machine-pilot`**, the command makes it run the cleaning
  phase: hop into `:street-sweeper`, run the (consented) cycle, hop back.
- **If no pilot is inhabiting**, the command is a no-op (or queues / surfaces a prompt) — TBD.

Why via-WM rather than a detached cron:

- **Respects WM-I4 + consent-gate discipline** — the action runs inside the pilot envelope, with
  cg-id provenance and the inhabitation log, not as a free-floating autonomous process.
- **Single dispatch point for "both places"** — the same command works whether fired by an operator,
  a schedule, or a tickle; the pilot-inhabitation check is the natural guard ("if there is a pilot in
  there, it will run the cleaning phase").

Open shape (to spec later, not now):

- Command surface — e.g. a WM command/endpoint (`POST /api/alpha/war-machine` `"run-street-sweeper"`)
  that, when a pilot inhabits `:war-machine-pilot`, triggers the hop + cleaning cycle.
- No-pilot behavior — no-op vs queue vs prompt.
- Whether it runs the full consented cycle or just the INV-15 auto-approve class + defer-the-rest.

## Deferred (handle separately)

- **Cron** questions — schedule, frequency, which repos, commit-only vs also-push (the `E-street-sweeper`
  v1 push question), and whether to formalize autonomous committing. Explicitly deferred per Joe. The
  via-WM command above is the *trigger surface* a cron would eventually call; the cron *policy* is its
  own decision.
- **Closing C3/C7** (the end-to-end hop round-trip verification) — owned by `E-pilot-hop-trigger-wiring`.

## Cross-references

- `M-pilot-appearance.md` — parent; "showing up well" across the four registers; this is the
  contract-compliance register.
- `E-street-sweeper.md` — the cleaning capability, the 22 invariants, the auto-approve/defer split.
- `E-pilot-hop-trigger-wiring.md` — the hop mechanism spec (partly landed; C3/C7 open).
- `futon5a/data/war-machine-strategic-vocabulary.edn` — **WM-I4** ("doesn't act unilaterally"): why the
  cleaning phase is operator-triggered by design.
- Code sites: `futon3c/src/futon3c/agency/registry.clj` (`hop!`), `transport/http.clj`
  (`/api/alpha/agency/hop`), `peripheral/war_machine_pilot_backend.clj` (`:hop!` action),
  `peripheral/street_sweeper.clj` (`run-full-sweep`).
- [[project_consent_gate]] — consent-gate discipline; the via-WM route preserves it.
- [[project_tickle_as_operator_model]] — a tickle could later fire the "run street sweeper" command.

## Provenance

- Surfaced 2026-06-15: Joe + claude did an interactive repo-hygiene sweep across ~16 repos (the job
  street-sweeper automates); Joe asked whether street-sweeper actually runs in the WM loop.
  Investigation confirmed: built + live + ran-once-3-weeks-ago, but no autonomous trigger — a manual
  loop. Joe directed this excursion (off M-pilot-appearance) + the via-WM use-case capture; cron
  deferred.
