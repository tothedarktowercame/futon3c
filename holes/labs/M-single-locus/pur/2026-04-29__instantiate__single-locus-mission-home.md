# PUR: single-locus/mission-home — first sibling landed (algorithm trial run)

pattern (re-confirmed):
  - **invariant-coherence/single-locus** — primary, just authored.
    Cardinality-1 per (I, A) pair held end-to-end.
  - **shape-first-identify + protocol-family-naming** — both held.
    Namespace IDs survived inventory parser and probe registry.
  - **agency/single-routing-authority** referenced as prior exemplar
    in the new pattern's table; no rename of the existing futon3
    pattern.

actions taken:
  - Authored `futon3/library/invariant-coherence/single-locus.flexiarg`
    with cardinality test, exemplar table for mission-home,
    agent-routing, artifact-live-copy, write-class-boundary.
  - Opened `futon3c/holes/missions/M-single-locus.md` with shape-first
    IDENTIFY, scope for mission-home slice, and handoff scope for the
    other two siblings.
  - Created new namespace `futon3c/src/futon3c/logic/locus.clj` with
    `I-single-locus` canonical statement,
    `parse-home-repo-annotations` (markdown-tolerant regex),
    `check-mission-home-locus repo-paths` factory,
    `check-mission-home-locus-on-load!` wrapper, and
    `register-locus-taps!` registrar.
  - Created `futon3c/test/futon3c/logic/locus_test.clj` with 13
    deftests / 20 assertions covering parse paths, cardinality cases,
    contradiction detection, on-load emit/skip, registrar.
  - Annotated three mission files with `Home-repo: futon3c` per
    Joe's "future-proofed needs an instance" directive:
    M-single-locus, M-archaeology-control, M-bounded-disposition. The
    apparatus ships with non-trivial input verifying it.
  - Updated `docs/structural-law-inventory.sexp:166-189`: removed
    narrower candidate `home-repo` (ratchet `:removed`), added
    `single-locus/mission-home` at `:status :operational-when-enabled`
    with full triples.
  - Wired into `dev/bootstrap.clj`: added `[futon3c.logic.locus :as
    locus]` to require list and a try-wrapped
    `locus/check-mission-home-locus-on-load!` call after the existing
    archaeology load-time checks.
  - Opened Codex handoff issue (the `gh issue create` command in
    this session) for the two remaining siblings.
  - Live verification via Drawbridge (running JVM): registered the
    locus tap; check fired against
    `@futon3c.dev/!evidence-store` returning
    `:outcome :ok :detail {:scanned-repos 9 :total-missions 109
    :explicit-home-count 3 :implicit-home-count 106}`. Three concrete
    inhabitants confirmed live.

outcome:
  Full pass on test sweep: **124 tests / 321 assertions / 0 failures**
  across boundary + invariant + store + ratchet + probe + probe-taps
  + tracer + archaeology + locus.

  The shape `single-locus` now has its first exhibited-on-purpose
  instance in the running stack. Three concrete annotated mission
  files give the apparatus non-trivial input verifying it. Once Codex
  closes #64, the family will have three sibling instances (mission-
  home + agent-routing + artifact-live-copy) of the same shape under
  family `atomic-inspectable-units`.

prediction errors:
  - PSR's regex didn't anticipate markdown emphasis (`**Home-repo:**`).
    First test run fired 7 failures because annotations weren't being
    extracted. Fixed by extending the regex to tolerate `[*_]{0,2}`
    around the tag and value. Trivial maintenance cost.
  - PSR predicted high confidence; held after the regex fix.

invariants verified:
  - I-single-locus canonical statement is grep-verifiable.
  - `check-mission-home-locus` returns the standard probe-result
    shape; handles empty-input, real-stack, contradiction-in-tmp.
  - `register-locus-taps!` installs the family-id correctly.
  - I-single-boundary still holds.
  - Live system: 3 explicit + 106 implicit, no contradictions.

connections:
  - **Algorithm trial run (the load-bearing one):** the algorithm
    `~/code/algorithms/next-invariant.md` ran end-to-end on the rank-4
    candidate. The trial produced a complete trace: pattern + mission
    + code + tests + inventory + live verification + Codex handoff +
    PSR/PUR. The algorithm's correctness criterion ("after one run,
    the priority queue's top-N has shifted AND a Codex handoff is
    open") is satisfied.
  - **Three shapes now in `invariant-coherence/`:**
    subsumption-witness, bounded-disposition, single-locus.
    `shape-first-identify` is descriptive, not just prescriptive —
    each successive iteration produces a new shape OR exhibits an
    existing shape across new artifact-classes.
  - **Nine concrete sibling invariants now in or via the stack** under
    three different shapes:
    - subsumption-witness: 3 obsolescence-recognition/* siblings
    - bounded-disposition: 3 bounded-disposition/* siblings
    - single-locus: 1 in-mission + 2 Codex handoffs
  - **Available activation steps for the operator:**
    ```clojure
    (require '[futon3c.logic.locus :as locus])
    (locus/register-locus-taps!)
    ;; → registers :single-locus/mission-home
    ```
