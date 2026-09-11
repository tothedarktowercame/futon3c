# Repository resolution fixed

Futon2 commit `38de06f5` restricts commissioned build resolution to the commissioned repository. It does not select another worktree merely because that worktree shares Git objects.

Zai-1 independently accepted the source change in job `invoke-1789159960255-20287-22d0f376`, inspecting the helper and all three callsites without rerunning tests. The focused real-Git regression passed: 1 test, 7 assertions, zero failures/errors.

The fix was loaded from canonical Futon2 while the runner was idle. A subsequent read-only call to the already-loaded resolver resolved Codex-20 commit `ee5ab864f5df54cc5b2edb0581b57531f7d48028` to `/home/joe/code/futon2`, returning the mission, implementation, and test files. Loaded-source SHA-256: `f38c0f9dd81b056d881cb7ad2b77096f7fb5609dee29e77e165dc24cf8307509`.

The retained check contains no reload, reservation, dispatch, or queue mutation. This establishes repository resolution only. The failed U88 attempt remains failed; neither independent review of its implementation nor task success is inferred. Codex-20's implementation commit is preserved.
