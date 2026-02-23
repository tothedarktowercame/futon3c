CLOJURE ?= $(shell if [ -x .tools/clojure/bin/clojure ]; then echo .tools/clojure/bin/clojure; else echo clojure; fi)
EVIDENCE_BASE?=http://localhost:7070

.PHONY: tools dev test claude claude-repl codex codex-repl codex-autowake tickle status repl \
	alfworld-server alfworld-runner alfworld-test alfworld-demo

tools:
	./scripts/bootstrap-tools.sh

dev:
	$(CLOJURE) -M:dev

test:
	$(CLOJURE) -X:test

claude:
	./scripts/claude-picker $(ARGS)

claude-repl:
	./scripts/claude-picker --repl $(ARGS)

codex:
	./scripts/codex-picker $(ARGS)

codex-repl:
	./scripts/codex-picker --repl $(ARGS)

codex-autowake:
	./scripts/codex-autowake $(ARGS)

tickle:
	./scripts/tickle-start $(ARGS)

status:
	@echo "=== Agency Health ==="
	@curl -sS --max-time 3 $(EVIDENCE_BASE)/health 2>/dev/null | python3 -m json.tool || echo "(Agency not reachable)"
	@echo
	@echo "=== Registered Agents ==="
	@curl -sS --max-time 3 $(EVIDENCE_BASE)/api/alpha/agents 2>/dev/null | python3 -m json.tool || echo "(Agency not reachable)"
	@echo
	@echo "=== Recent Evidence (last 5) ==="
	@curl -sS --max-time 3 "$(EVIDENCE_BASE)/api/alpha/evidence?limit=5" 2>/dev/null | python3 -c "import sys,json; d=json.load(sys.stdin); [print(f'  {e.get(\"evidence/at\",\"?\")[:19]}  {e.get(\"evidence/author\",\"?\"):12s}  {e.get(\"evidence/tags\",[])}') for e in d.get('entries',[])]" 2>/dev/null || echo "(no evidence or Agency not reachable)"
	@echo
	@echo "=== Git Log (last 5) ==="
	@git log --oneline -5 2>/dev/null || echo "(not a git repo)"

repl:
	$(CLOJURE) -M:dev:repl

ALFWORLD_PORT ?= 3456
ALFWORLD_PY ?= python

# Deterministic runner defaults (override at call site if you want).
# Example: make alfworld-runner ALFWORLD_RUNNER_GAMES=50 ALFWORLD_RUNNER_ARGS=
ALFWORLD_RUNNER_GAMES ?= 10
ALFWORLD_RUNNER_ARGS ?= verbose

# -----------------------------------------------------------------------------
# ALFWorld convenience targets
# -----------------------------------------------------------------------------

alfworld-server:
	$(ALFWORLD_PY) scripts/alfworld-server.py --port $(ALFWORLD_PORT)

alfworld-runner:
	bb scripts/alfworld_runner.clj $(ALFWORLD_RUNNER_GAMES) $(ALFWORLD_RUNNER_ARGS)

alfworld-test:
	bb scripts/alfworld_test.clj

alfworld-demo:
	bb scripts/alfworld_demo.clj
