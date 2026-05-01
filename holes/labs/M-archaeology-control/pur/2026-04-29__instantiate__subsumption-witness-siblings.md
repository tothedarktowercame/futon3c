# PUR: Subsumption-witness siblings — three sibling check-fns landed

pattern (re-confirmed):
  - **invariant-coherence/subsumption-witness** — primary. Each sibling
    implements enumerate-A / locate-P / subsumption-test.
  - **invariant-coherence/protocol-family-naming** — accompanying.
    Namespace IDs `obsolescence-recognition/<artifact-class>` carry the
    relationship without schema changes.
  - **invariant-coherence/shape-first-identify** — methodology.
    Exhibited the shape across all three artifact-classes simultaneously.
  - **agency/single-routing-authority** + **storage/durability-first** —
    inherited via the probe/boundary apparatus.

actions taken:
  - Created `src/futon3c/logic/archaeology.clj` (~280 lines):
    `I-obsolescence-recognition` canonical statement; three check-fn
    factories (`check-autostash-obsolescence`,
    `check-deferred-stub-obsolescence`,
    `check-pipeline-tracer-obsolescence`); helpers (`list-stashes`,
    `stash-subsumed-by-head?`, `check-fn-result-deferred?`,
    `inventory-status-operational?`, `query-by-tags`,
    `entry-track-id`, `past-target-date?`); convenience registrar
    `register-archaeology-control-taps!` with `default-repo-paths`.
  - Created `test/futon3c/logic/archaeology_test.clj` — 12 deftests /
    21 assertions / 0 failures: empty-input ok, no-stashes ok,
    nonexistent-repo skipped, registry-empty ok, no-inventory ok,
    non-deferred-fns-not-flagged, no-tracers ok, closed-track flagged,
    past-target flagged, fresh-future not-flagged, canonical statement
    grep-verifiable, register installs three taps.
  - Edited `docs/structural-law-inventory.sexp` lines 200-209 to:
    rename `obsolescence-recognition` → `obsolescence-recognition/
    autostash`, add `obsolescence-recognition/deferred-stub` and
    `obsolescence-recognition/pipeline-tracer` siblings, all with
    `:status :operational-when-enabled` and full `:implemented-in /
    :enforced-at / :evidenced-by` triples.
  - Created `scripts/check-autostash-obsolescence.sh` (~85 lines):
    pre-commit hook mirroring `check-coverage-ratchet.sh`. Quiet on
    success, structured banner on violation, documented activation via
    `ln -sf` symlink, `FUTON3C_SKIP_AUTOSTASH_CHECK=1` bypass for
    emergency-only use. `chmod +x` applied.
  - Created PSR at
    `holes/labs/M-archaeology-control/psr/2026-04-29__derive__subsumption-witness-siblings.md`.
  - Live verification via Drawbridge nREPL: registered all three taps
    in the running JVM; each fired against
    `@futon3c.dev/!evidence-store`, returning `:outcome :ok` with
    expected detail. Pipeline-tracer correctly counted the 6 open
    tracers from earlier in this session; autostash scanned futon3c
    (zero stashes confirmed); deferred-stub walked the live registry
    (only the three archaeology check-fns registered, none deferred).

outcome:
  Full pass — mission test sweep across boundary + invariant + store +
  ratchet + probe + probe-taps + tracer + archaeology:
  **87 tests / 238 assertions / 0 failures**.

  The shape is exhibited in three independent forms; each form has a
  real subsumption test, not a heuristic placeholder. The autostash
  slice has the strong-mode binding (pre-commit hook, blocking) and
  the other two slices have the probe-mode binding (read-only,
  surfacing). Cleanup automation stays manual via `futon-sync.clj` —
  the invariants tell you when, not how.

prediction errors:
  - PSR predicted high confidence; held. The only stumble was a
    forward-reference bug in `past-target-date?` (called
    `length-or-zero` before its definition), caught immediately on
    file-write and resolved by inlining `(count target-iso)`. No
    pattern-level surprise.
  - PSR worried about downstream tools breaking on namespace-
    discriminated IDs (`obsolescence-recognition/autostash`). Confirmed
    safe via live test: `inventory/extract-all-families` parses them as
    keyword-with-namespace; the ratchet's diff treats the rename as
    `:added` + `:removed`, not as a demotion. No structural problem.

invariants verified:
  - `I-obsolescence-recognition` (canonical statement) — string,
    grep-verifiable, present in archaeology.clj.
  - `obsolescence-recognition/autostash` — empty repo list ok,
    nonexistent paths skipped, real stash-subsumption test runnable.
  - `obsolescence-recognition/deferred-stub` — empty registry ok,
    no-inventory cross-check ok, non-deferred fns never flagged.
  - `obsolescence-recognition/pipeline-tracer` — empty store ok,
    closed-track flagged, past-target flagged, future-open not-flagged.
  - `register-archaeology-control-taps!` installs all three with
    proper namespace-discriminated keyword IDs.

connections:
  - **Sibling mission:** M-invariant-queue-extend — the apparatus all
    three sibling checks build on (probe / boundary / canary /
    boundary-pattern). M-archaeology-control is now the worked example
    of the protocol-family-naming + shape-first-identify methodologies.
  - **Library feedback:** invariant-coherence/{shape-first-identify,
    subsumption-witness, protocol-family-naming} got their first
    application confidence-bump. Each pattern's NEXT-STEPS are now
    actionable (count siblings registered; track namespace-discriminated
    IDs in inventory; render shape-prefix as a separate column in the
    arxana operational-families view).
  - **Dogfood loop available:** the deferred-stub check is now built;
    if M-invariant-queue-extend Track 1 (substrate-2 lift) lands real
    check-fns for the 6 substrate-2 deferred-stubs but the inventory
    isn't updated to reflect their operational status, the deferred-
    stub check will catch that mismatch on the next probe sweep.
  - **Available activation steps for the operator:**

    ```clojure
    ;; Register the three archaeology checks as probe taps (live system):
    (require '[futon3c.logic.archaeology :as arch])
    (arch/register-archaeology-control-taps!)
    ```

    ```bash
    # Install the autostash pre-commit hook (per repo, optional):
    cd ~/code/futon3c
    ln -sf ../../scripts/check-autostash-obsolescence.sh \
           .git/hooks/pre-commit
    ```
