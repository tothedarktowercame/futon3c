# PUR: bounded-disposition/stash — first sibling landed

pattern (re-confirmed):
  - **invariant-coherence/bounded-disposition** — primary, just authored.
    Two-obligation shape distinct from subsumption-witness.
  - **shape-first-identify + protocol-family-naming** — methodology +
    naming. Each held: the absorption of two narrow candidates into one
    shape-aligned sibling produced cleaner semantics than building two
    literal siblings.

actions taken:
  - Authored `futon3/library/invariant-coherence/bounded-disposition.flexiarg`
    with full vocabulary + bound predicate + exemplar table for stash,
    branch, mission-doc, pr.
  - Opened `futon3c/holes/missions/M-bounded-disposition.md` with shape-
    first IDENTIFY, scope for stash slice, and handoff scope for branch
    + mission-doc siblings.
  - Extended `src/futon3c/logic/archaeology.clj` with:
    `I-bounded-disposition` canonical statement;
    `stash-disposition-vocabulary`,
    `default-stash-disposition`,
    `default-undecided-bound`,
    `default-old-stash-days` data;
    `parse-stash-disposition message` (case-insensitive
    `[disposition: ...]` regex extraction);
    `list-stashes-with-time repo-path` (`git stash list --format=...`
    with author-date for age computation);
    `check-stash-disposition repo-paths {:undecided-bound :old-days}`
    factory returning the standard probe-result shape;
    `register-archaeology-control-taps!` extended to register the
    fourth tap.
  - Extended `test/futon3c/logic/archaeology_test.clj` with 6 new
    deftests (vocabulary, parse paths, empty-input, no-stashes,
    nonexistent-repo-skipped, canonical-statement) plus update the
    previous `register-installs-three-taps` to
    `register-installs-four-taps`.
  - Updated `docs/structural-law-inventory.sexp:204-218`: removed two
    narrow candidates (`stash-debt-bounded`, `recover-or-drop`); added
    `bounded-disposition/stash` at `:status :operational-when-enabled`
    with full implementation/enforcement/evidence triples.
  - Live verification via Drawbridge nREPL: registered all four taps
    against the running JVM; bounded-disposition/stash returns
    `:outcome :ok` (zero stashes in the futon-stack — nothing to
    triage). Test sweep across full mission suite: 99 tests / 268
    assertions / 0 failures.
  - Opened Codex handoff issue (the `gh issue create` command in this
    session) for the two remaining siblings.

outcome:
  Full pass on test sweep. Live system carries the new tap and fires
  cleanly. The shape (bounded-disposition) now has its first
  exhibited-on-purpose instance in the running stack; once Codex
  closes the branch + mission-doc siblings, three concrete instances
  of the shape will exist in production — mirroring the three
  obsolescence-recognition siblings under the same family.

prediction errors:
  - PSR predicted high confidence; held. The only stumble was the
    `register-installs-three-taps` test name, which had to update to
    `register-installs-four-taps` after the registrar took on the
    fourth check. Trivial maintenance cost; not a pattern surprise.

invariants verified:
  - I-bounded-disposition canonical statement is grep-verifiable.
  - `stash-disposition-vocabulary` is exactly the documented set.
  - `parse-stash-disposition` returns one of the vocabulary or the
    default; never throws.
  - `check-stash-disposition` returns the standard probe-result shape;
    handles empty inputs, real repos with no stashes, and nonexistent
    paths without exception.
  - `register-archaeology-control-taps!` installs four family-ids
    (three obsolescence-recognition + one bounded-disposition).
  - I-single-boundary still holds: no new direct `*/append*` calls.

connections:
  - **Sibling mission (different shape, same family):**
    M-archaeology-control. The two missions exhibit two protocols
    (subsumption-witness vs bounded-disposition) under one family
    (archaeology-control). The methodology Joe flagged ("the shape is
    itself a shape we can reuse") now has two worked examples, not
    one.
  - **Pattern library:** invariant-coherence/ now has four patterns.
    Each pattern has at least one fully-implemented exemplar in the
    stack.
  - **Codex handoff (issue):** the GH issue that follows this PUR
    extends the same shape across two more artifact-classes. The
    methodology recurs.
  - **Available activation steps for the operator:**
    ```clojure
    (require '[futon3c.logic.archaeology :as arch])
    (arch/register-archaeology-control-taps!)
    ;; → registers four checks including :bounded-disposition/stash
    ```
