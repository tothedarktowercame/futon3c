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
`test-registry-9b2cd0428ebc65dff4687dc28e9dd848616ffadf440463ac1839e6eec07f6b0c`.

The first registration attempt did not execute the tests: JVM startup refused
because it could not create the initial G1 refinement thread (`EAGAIN`). The
successful pinned registration used `JAVA_TOOL_OPTIONS='-XX:+UseSerialGC
-XX:ActiveProcessorCount=2'`; that environment is recorded by the warrant.

Static gates: clj-kondo reported 0 errors and 0 warnings on `http.clj` and the
new test namespace; `futon4/dev/check-parens.el` returned `OK` on both files.
No namespace was loaded into the serving JVM, and it was not restarted.
