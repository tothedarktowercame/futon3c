# Independent offline verifier review

Accepted the verifier artifact in b5fcac20, SHA256
`6ca6f397b1f7531c91a0f4b9ab6cbc3d2712eb4b347c3c897b91a62c72958829`.
The retained review script fetched the completed Agency review job, invoked the
actual authoritative verifier into a fresh temporary directory, and reproduced
the retained artifact byte-for-byte. This rechecks the finding, qualification,
plan, source bytes, Git ancestry/current HEAD, executed review and marker through
the production verifier. No repair-store execution port was invoked.

One configuration correction was necessary: historical-revalidation-entry reads
`:repair-reviewer`, not `:reviewer`. The disabled casting now explicitly has
`:author "codex-10"`, `:reviewer "codex-12"`, and
`:repair-reviewer "codex-12"`. Using the actual read-only candidate port against
the canonical finding, the actual selector accepts that casting. Removing the
repair-reviewer or replacing it with the author refuses. Merely constructing
runner ports did not establish this selection compatibility.

Validation:

- Retained review script: byte-identical replay and selector/negative assertions pass.
- Historical verifier: 1 test / 12 assertions, no failures or errors.
- Historical freshness/race test with `-M:test:test-all`: 2 / 13, no failures or errors.
- Default test alias excludes that slow race test; its initial zero-test result
  was not counted as validation.
- clj-kondo: zero errors/warnings; check-parens and diff check pass.

Reproduce the review from futon3c with:

```sh
clojure -M holes/labs/wm-contract/runs/RUN4-U88-successor-deployment-2026-09-11/review-offline-verification.clj
```

The script writes only a new temporary verifier artifact and reads production
finding/review evidence. It never calls the historical execution port.
The packet remains disabled. This establishes a qualified verification artifact
and a selectable candidate, not an executed repair admission, resolved repair,
or successor identity. Next preparation must bind this authority and casting
into a disabled service packet; a successor link must await the actual immutable
historical execution receipt. No live capacity, attempt, reset, restart, or
service configuration changed; unrelated dirty files remain untouched.
