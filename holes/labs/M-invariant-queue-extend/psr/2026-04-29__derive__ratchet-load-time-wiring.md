# PSR: Coverage-ratchet load-time wiring

context:
  M-invariant-queue-unstuck shipped the ratchet's pre-commit script (mode (b))
  and the on-demand validation entry point (mode (c)), but the predecessor's
  DOCUMENT phase recorded "Coverage-ratchet load-time wiring (currently only
  the pre-commit hook surface is implemented; load-time check is dormant)."
  The predecessor explicitly bound `:enforced-at` for I-coverage-ratchet to
  `:operational-when-enabled`. Operator activation requires the load-time
  check to actually fire on JVM boot, not just at git commit time.

patterns considered:
  - **Boot-banner pattern (existing futon3c precedent).** `dev/bootstrap.clj`
    already runs `evidence-invariant/check-store-backing` after the
    evidence-store is constructed, printing a structured banner on failure
    and *continuing boot* either way. That's the closest existing instance
    of "a load-time invariant check that surfaces loudly without aborting
    boot." **Selected as the wiring shape.**
  - **`:family-fired` evidence emission (existing probe pattern).** The
    probe's hourly sweep emits `:family-fired :outcome :ok|:violation|:inactive`
    per family per run. The load-time check is structurally a one-shot
    fire of the `:coverage-ratchet` family — emitting the same shape makes
    the load-time event visible in the same operational-families view as
    the probe sweep. **Selected as the evidence shape.**
  - **Synchronous, single-resolution emit through the boundary (boundary
    pattern, M-invariant-queue-unstuck PSR).** The boundary's nil-store fix
    is load-bearing: emit-load-time-fired! takes the resolved evidence-store
    explicitly. **Selected as the call shape.**

decision:
  Add `ratchet/check-on-load!` that:
   1. Runs `check-pre-commit` (working-tree vs git HEAD) — the same logic
      the pre-commit script already validates.
   2. Maps the result to a single `:family-fired` outcome —
      `:ok` (clean), `:violation` (unreconciled demotions),
      `:inactive` (no baseline available, e.g. tests / no git).
   3. Emits one `:family-fired` evidence entry through `boundary/append!`
      tagged `[:invariant-queue :family-canary :family-fired :load-time
      :coverage-ratchet outcome]` so probe-view queries find it.
   4. Prints a structured banner on `:violation`, a one-line OK on `:ok`,
      a one-line INACTIVE note on `:inactive`. Boot continues either way.

  Wire into `dev/bootstrap.clj` after the existing `check-store-backing`
  call (both load-time invariants then fire together; ordering matters
  because the ratchet emits evidence and therefore depends on the
  evidence-store being durable, which `check-store-backing` verifies).

  Wrap the call in `try/catch` at the bootstrap site: a load-time exception
  in the ratchet must not block boot of unrelated subsystems. The
  exception-shape becomes a printed line, not a JVM kill.

alternatives:
  - **Hook into `inventory/load-inventory` directly.** Rejected: makes
    `inventory.clj` depend on `ratchet.clj`, introducing a circular
    require risk and forcing every test that loads the inventory to also
    load the ratchet (with all its evidence-emission requirements).
    Explicit boot-time wiring is cleaner.
  - **Throw on violation.** Rejected: the ratchet's job is to *make
    degradation visible*, not to block startup. Boot-blocking would
    make a stale demotion-event database (e.g. after a checkout from
    a fresh clone) prevent any work, including the work needed to
    reconcile the demotion. Loud surfacing + durable evidence is enough.
  - **Emit a separate `:event :load-time-checked` shape rather than
    `:family-fired`.** Rejected: the operational-families view already
    knows how to render `:family-fired`; introducing a parallel event
    type forks the rendering path. Better to reuse the canary shape.

outcome (target):
  - `ratchet/check-on-load!` exists and emits a `:family-fired` entry per
    invocation, with `:outcome` matching the validation result.
  - Boot path calls it after `check-store-backing`; the printed banner
    is visible in the dev console.
  - Operator-driven activation criterion of M-invariant-queue-extend
    Track 4.1 satisfied: the load-time check is no longer dormant.
  - I-coverage-ratchet's `:status` in the inventory remains
    `:operational-when-enabled` for now (the pre-commit hook is still
    operator-installed); a graduation to `:operational` follows once
    the hook is also wired (separate operator action).

confidence: high.
  Pattern lineage is direct (boot-banner + `:family-fired` shape, both
  already in production). The new function is a thin wrapper around
  existing validated code paths (`check-pre-commit` + `boundary/append!`).
  Test surface is small (two new deftests covering emit + emit-skip).
