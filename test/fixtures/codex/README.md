`exec-apply-patch.jsonl` is an excerpt of the real Codex rollout
`rollout-2026-09-21T13-39-31-01a0c431-796e-7c53-af97-b6c4c7ec0c13.jsonl`,
at 13:58:47.287Z and 13:58:47.308Z (ordinals 423–424). It includes the
`exec` call containing `tools.apply_patch` and its completed `FileChange`
receipt. The edited path was replaced by `src/work.clj`, and the receipt's
thread ID by `clock-session`, so tests can use an isolated mission tree.
All other event fields, including item ID, status and patch hunk, are retained.
The fixture is parsed as data; its tool-call text is never executed.
