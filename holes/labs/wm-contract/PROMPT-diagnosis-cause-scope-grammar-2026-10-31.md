# Diagnosis prompt-contract correction — cause-scope grammar stated inline

Branch `review/p4-diagnosis-prompt-grammar` (futon3c), zai-4 on dispatch
codex-11 (8640), 2026-10-31. Isolated worktree; **no merge, no reload, no
click, no contact, no ledger edits.** Follows zai-5's completed job 8638
(futon2 `3bbfeabe7`, branch `review/p4-diagnosis-evidence-correction`),
which recovered the full retained diagnosis response for click
`wm-click-0930ccf1` and corrected the mechanism.

## Cause (directly inspected)

`futon3c/src/futon3c/wm/meta_self_heal_adapters.clj` `producer-prompt`
:diagnosis asked for `:healthy-predicate <closed result predicate>` without
stating any grammar. The real verifier — `futon2.aif.cause-scope/predicate?`
— requires a MAP with `:op ∈ #{:result-path-equals :result-path-present
:result-path-absent}`, a vector `:path`, and `:value` required for equals
(nil a valid value). The diagnosis seat returned the fn-literal STRING
`(fn [record] (nil? (:failure record)))`; every other :cause-entry clause
passed; the boundary refused `:cause-scope/predicate-not-result-derived`.
**The old record "missing predicate" is NOT the cause; the missing prompt
grammar is.** No predicate for this injury is prescribed here, no old
response is auto-converted, and no example op is implied correct.

## Change (prompt only; no verifier/authority/acceptance machinery touched)

The :diagnosis prompt now states the complete existing grammar inline:
map-only (never code, a function, or a function string), the three ops with
exact semantics, `:value` REQUIRED for equals with nil valid, `:path`
evaluated (get-in) on the **RETURN VALUE of the named boundary applied to
the pinned retained carrier** — NOT on the original record — present/absent
vs nil/false distinction, the boundary must accept the actual retained
carrier, choose a justified result-derived condition or refuse
`:cause-scope-unretained`, and an explicit no-prescription sentence.

## Focused witness (real exit check, no testless help, no broad suite)

`clojure -M:test -n futon3c.wm.meta-self-heal-adapters-test`:
**52 tests / 419 assertions, 0 failures, 0 errors.** New tests:
- `diagnosis-prompt-states-the-complete-cause-scope-grammar` — prompt
  contains the full syntax and input/result semantics; negative check that
  no specific predicate (e.g. `:path [:failure]`) is implied.
- `prompt-grammar-agrees-with-the-real-cause-scope-predicate-verifier` —
  the actual 0930ccf1 function STRING is refused by the real
  `cause-scope/predicate?`; each documented map op admitted; malformed maps
  (empty, unknown op, equals missing `:value`, missing `:path`, non-vector
  `:path`) rejected; equals-missing-value vs equals-present-nil
  distinguished; extra keys documented as tolerated (existing open-map
  behavior, unchanged).
- Existing prompt contract tests unchanged and green.

No `run-opportunity!` was invoked on the retained fragment; no production
side effects; tests establish prompt/verifier agreement only — no fake
successful diagnosis is claimed.

## Truthful scope accounting

- Charge remains **243/230/13**; repair attempt **unspent**; ledger
  `ca73914f…1a44` untouched.
- **Diagnosis-seat contact DID occur** during P4 (one producer invoke on the
  wm-author seat) though no repair-author dispatch happened; the P4 receipt
  (8f887ba20, corrected by 3bbfeabe7 / d93e255dd) says so.
- Protected hashes pinned around P4 are operational snapshots, **not a C2
  repair baseline**.
- No P3 optional-limit witness is claimed from this packet.
- Criteria touched: A3/B2/C1 diagnosis-producer contract plus D4 truthful
  refusal. **No criterion closure.**
- Incident note (same class as 8579): one accidental
  `clojure -M:test -e '…'` invocation in this worktree started the alias's
  main-opts (the `-e` was ignored) and began scanning the whole test tree;
  it was killed during startup (weight-loading phase) and errored on a
  missing namespace (`futon2.aif.outer-cascade`) before any test executed or
  reported. No test results were produced and no file side effects observed;
  the correct invocation used afterward is `-n <namespace>` only.

## For review

Exact diff is prompt text + tests in
`src/futon3c/wm/meta_self_heal_adapters.clj` and
`test/futon3c/wm/meta_self_heal_adapters_test.clj`. Any material change
beyond prompt/tests would have stopped this packet (none was needed); no new
recovery architecture built. Reviewer: zai-5.
