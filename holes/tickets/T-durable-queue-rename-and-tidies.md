# T-durable-queue-rename-and-tidies

Parent: `M-agency-hardening.md`. Post-landing polish before activating the queue.
Dispatched by claude-6, 2026-06-10. Reviewer: claude-6. Branch off **master**.

## 1. Rename the queue flag → `FUTON3C_DURABLE_QUEUE`

The Car-3 codename is internal; the user-facing flag should be meaningful.
Rename **everywhere**: `FUTON3C_CAR3_QUEUE` → `FUTON3C_DURABLE_QUEUE` and
`FUTON3C_CAR3_QUEUE_PATH` → `FUTON3C_DURABLE_QUEUE_PATH`.
- `src/futon3c/agency/turn_queue.clj` `enabled?` + `queue-store-path` (both the
  `System/getProperty` and `config/env`/`env-bool` lookups).
- Any comments, tests, ticket/design-note references. Grep the repo for
  `CAR3_QUEUE` and `FUTON3C_CAR3` and update all hits.
- No back-compat alias needed (the flag was never activated in prod).

## 2. Tidy: auto-bellback jobs must record delivery (kills MQ-1 self-noise)

`enqueue-auto-bellback!` (http.clj) creates a bell job + runs it, but never
records delivery — so every auto-bellback job trips **MQ-1** (terminal but no
delivery record). Mirror `handle-bell`: after `run-invoke-job!`, call
`record-invoke-job-delivery-by-job-id!` with an appropriate surface/destination/
note so the auto-bellback job is marked delivered. Add/extend a test.

## 3. Tidy: MQ-7 should exclude human/`joe` callers

`unaddressable-caller?` (mesh_qa.clj) currently flags a codex completion whose
caller is `joe` (not a registered agent) — but a human-dispatched codex job
legitimately has no agent to bell back, so that's not a capture-gap. Exclude
`"joe"` (and align with auto-bellback's caller exclusions: `http-caller`,
`joe`, blank). MQ-7 should fire only for genuine *agent* capture-gaps. Update
the model + impl tests (codex + joe-caller → no MQ-7).

## Out of scope (explicitly): the auto-bellback-suppress-on-manual-bell
refinement — that needs same-job-id manual-bell detection; not a "small tidy",
stays deferred.

## Gates
clj-kondo clean (no new warnings); `futon4/dev/check-parens.el`; `clojure -X:test`
green on the touched nses (turn-queue, mesh-qa, transport auto-bellback/http).

## In-flight constraints
No JVM restart/reload — claude-6 reloads + activates. Worktree off master:
`git worktree add ../futon3c-tidies -b codex/durable-queue-tidies master`.
Commit there, NO push/merge.

## Done = bell claude-6
with branch + sha, gate results, the grep-confirmed list of renamed
`CAR3_QUEUE` hits, and the changed files.
