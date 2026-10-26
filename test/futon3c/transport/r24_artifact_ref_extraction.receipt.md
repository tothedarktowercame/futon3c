# R24 artifact-reference extraction receipt

- Base commit: `78087dd81c930982a4c003440607f165b6df203a`
- Implementation commit: `6b1bb1358b947ef06efa8f66b2dd078b2835f672`
- Lean witness: `087e5f4985`
- Retained reply: `~/notes/wm-next/outer-selection/third-click-2026-10-26/zai-2-author-reply.txt`
- Retained reply sha256:
  `af4cc4f5ba4d2f901edaef96d0559b9b6749207f20f8328c4e87aaba409ee413`

## Red witness

Before changing `first-artifact-ref`, evaluating it on the retained reply at
the base commit returned:

```text
/missions/M-recommendation-bindings.md
```

## Green witness

After the change, the same retained bytes returned:

```text
09a7cb8d3468031ed3fa2beb6db21c602617a572
```

The registered namespace run passed 3 tests and 9 assertions with 0 failures
and 0 errors. Warrant:
`test-registry-e5018d7d48a8e5d538055e64a38628d851e66f43af0bfe04143b44b8649ac35e`.
Its post-landing validity check returned `:warrant? true` with no changed paths.

The first registration attempt did not execute the tests: JVM startup refused
because it could not create the initial G1 refinement thread (`EAGAIN`). An
intermediate bounded-thread registration minted
`test-registry-9b2cd0428ebc65dff4687dc28e9dd848616ffadf440463ac1839e6eec07f6b0c`,
but its validity check correctly refused `:environment-mismatch`; it is not the
warrant cited above. The final registration used the standard environment.

Static gates: clj-kondo reported 0 errors and 0 warnings on `http.clj` and the
new test namespace; `futon4/dev/check-parens.el` returned `OK` on both files.
No namespace was loaded into the serving JVM, and it was not restarted.

## Review amendment (claude-9, 2026-10-26 16:52Z)

The rule-1 test placed the PR link before the sha in the text, so it passed
under the earlier first-match-in-text-order extractor as well. Commit
`ff0351ddcf53812c187f0e3f7cf63ff32315ae3b` adds one assertion with the sha
written before the PR link. Fresh warrant for the amended namespace:
`test-registry-906b72a8fc7d317a07e45d6a6839406d5d48c1a3862038319347c8ab11f795fd`
(3 tests, 10 assertions, 0 failures, 0 errors; postcheck `matched`;
`http.clj` sha unchanged at `d449605988…`). Live probe after
`scripts/restore-http-routes.sh`: the retained reply gives the full sha, a
PR link after a sha gives the PR link, a 64-hex digest gives nil.
