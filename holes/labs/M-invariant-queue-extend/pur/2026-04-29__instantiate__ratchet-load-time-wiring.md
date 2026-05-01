# PUR: Coverage-ratchet load-time wiring — boot path now fires the canary

pattern (re-confirmed):
  - **Boot-banner pattern** (existing futon3c precedent in `dev/bootstrap.clj`'s
    `evidence-invariant/check-store-backing` call): structured banner on
    failure, OK line on success, boot continues either way.
  - **`:family-fired` evidence shape** (existing probe pattern in
    `futon3c.logic.probe/emit-family-fired!`): one outcome per family per
    fire, tagged for query-by-tag retrieval.
  - **Single-routing-authority via boundary** (existing M-invariant-queue-unstuck
    pattern): every emit goes through `boundary/append!`.

actions taken:
  - Added `ratchet/check-on-load!` (`src/futon3c/logic/ratchet.clj`) plus
    helper `emit-load-time-fired!`. The function takes an evidence-store
    and an options map (`:emit?` default true, `:print?` default true),
    runs `check-pre-commit`, maps the result to one of `:ok | :violation
    | :inactive`, emits a `:family-fired` evidence entry, prints the
    appropriate banner, and returns the result map enriched with
    `:outcome` and (when emitting) `:emit-receipt`.
  - Added `[futon3c.logic.ratchet :as ratchet]` to the require list of
    `dev/futon3c/dev/bootstrap.clj` and a try-wrapped call to
    `ratchet/check-on-load! evidence-store` immediately after the existing
    `check-store-backing` boot check. Boot continues if the call throws.
  - Added two deftests to `test/futon3c/logic/ratchet_test.clj`:
    `check-on-load-emits-family-fired-evidence` (verifies emit-receipt
    populated and outcome is one of the three valid keywords) and
    `check-on-load-respects-emit-flag` (verifies `:emit? false` skips
    emission). Both pass against in-memory XTDB.
  - Fixed a pre-existing pre-commit-blocked compile error in
    `src/futon3c/transport/irc.clj`: the predecessor's INSTANTIATE-2-rest
    boundary-require conversion left an orphan `[futon3c.evidence.boundary
    :as boundary]` form *outside* the `(:require ...)` block. The line
    moved inside the require list. The running JVM (started 2026-04-24)
    didn't surface this because it never reloaded the file; my Track 4.1
    boot wiring exposed the issue because verifying the new wiring requires
    boot to actually succeed in a fresh JVM.
  - Created PSR at
    `holes/labs/M-invariant-queue-extend/psr/2026-04-29__derive__ratchet-load-time-wiring.md`.

outcome:
  Full pass on the targeted suites — boundary + invariant + store + ratchet
  + probe + probe-taps: 69 tests / 204 assertions / 0 failures. The two
  new deftests cover both `:emit? true` and `:emit? false` paths.
  Wider sweep showed 24 failures + 19 errors, all in pre-existing
  brokenness from Joe's other in-flight uncommitted work (peripherals.edn
  / shapes mismatches in social.peripheral-test, mode-integration-test,
  peripheral-spec-test, mission-control-test, and others) — confirmed
  unrelated to Track 4.1 by inspection. The +14 errors vs the predecessor's
  reported sweep are tests previously hidden behind the irc.clj compile
  failure (test_fixtures.clj:295 — peripheral spec validation against
  Joe's modified peripherals.edn).

prediction errors:
  - PSR predicted the load-time check could re-use `:family-fired` directly.
    Confirmed — the rendering view (when extended in Track 4.3) will treat
    a load-time fire identically to a probe-sweep fire, with the only
    difference being the `:load-time` tag for filtering. No surprise.
  - PSR predicted "high confidence." Held. The only surprise was the
    pre-existing `transport/irc.clj` compile blocker, which is upstream
    of Track 4.1's logic but downstream of its verification — fixing it
    is reasonable infrastructure repair, not scope creep.

invariants verified:
  - I-coverage-ratchet (canonical statement): unchanged; still grep-verifiable.
  - I-evidence-per-turn (predecessor): still binding — every emit through
    the boundary calls `verify-persisted`.
  - I-single-boundary (predecessor): still holds — `:family-fired` emission
    uses `boundary/append!`, no new direct `store/append*` calls.
  - I-coverage-ratchet load-time mode (new): the load-time canary fires
    on every boot, recording `:family-fired :coverage-ratchet :load-time
    <outcome>` evidence. The dormant-mode case in the predecessor's
    DOCUMENT phase is now non-dormant.

connections:
  - **Track 4.2 next:** snapshot-as-evidence convention. Every load-time
    canary fire is *itself* a snapshot — the entry's `:body` records the
    counts of demotions / promotions / additions / removals at boot time.
    Track 4.2's task is to formalize the convention (probably
    `:event :inventory-snapshot` shape recorded on every inventory load,
    independent of ratchet outcome) and decide whether the load-time
    ratchet fire subsumes it or is a separate event.
  - **Track 4.3 next:** the arxana operational-families view extension
    queries `:family-fired` events for `:last-fire-at` / `:last-violation-at`
    columns. Track 4.1 produces the data the view will read; the column
    rendering is Track 4.3's INSTANTIATE.
  - **Cross-mission:** the irc.clj compile fix unblocks any other agent
    trying to boot a fresh JVM against Joe's working tree. This is a
    side-effect-positive: future missions don't have to re-discover the
    blocker.
