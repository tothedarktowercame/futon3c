# G6 task bank: pass rules, fixed before any run (claude-3, 2026-10-17)

Each task is plan-only (the seat states a command or statement and runs
nothing that changes state). The deciding memory is named; it is in the
minus40 store and not in CLAUDE.md, AGENTS.md or a README the seat would
normally read (T4's rule is also in deps.edn comments; a seat that reads
deps.edn can pass without the store).

| task | deciding memory | pass | fail |
|---|---|---|---|
| T1 | agency-send-mode-brief-trap | the command has `--mode work` (and `--from`, `--to codex-5`, `--kind bell`) | no `--mode work` |
| T2 | pkill-f-self-match | the kill cannot match its own shell: bracket form (`[w]idget_probe.py`), an escaped pattern the shell text does not contain (`widget_probe\.py`), or pgrep output filtered of `$$`/its own pid, or matching argv[0] exactly with `-x` on the python interpreter excluded | a bare `pkill -f widget_probe.py` or `pgrep -f widget_probe.py` piped to kill |
| T3 | invoke-ledger-test-clobber | sets `FUTON3C_INVOKE_JOBS_FILE` to a temp path | does not |
| T4 | slow-tagged-tests-hide | includes the slow tag (`-M:test:test-all -i :slow`, or equivalent), or says the default alias would exclude it | plain `clojure -M:test -n futon3c.test-registry-test` with no word on the exclusion |
| T5 | lean-top-is-analytic | smoothness `∞` (`ContDiff ℝ ∞ f`, or `((⊤ : ℕ∞) : WithTop ℕ∞)`) | `ContDiff ℝ ⊤ f` |

Arms. Baseline: zai-1 as it stands (minus40 tools and the standing
instruction to call m40_find are present; using them is the seat's choice).
Informed: zai-2, the same packet text preceded by the deciding memory's file
text under a heading saying it was injected, as G4 would put it before round
1. The memory was picked by hand: the informed arm measures effect given
delivery, not retrieval. A task whose baseline passes is dropped and the
drop recorded; the informed arm runs only on failed baselines.
