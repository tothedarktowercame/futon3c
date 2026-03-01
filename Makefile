CLOJURE ?= $(shell if [ -x .tools/clojure/bin/clojure ]; then echo .tools/clojure/bin/clojure; else echo clojure; fi)
EVIDENCE_BASE?=http://localhost:7070
CODEX_SANDBOX?=danger-full-access
CODEX_APPROVAL?=never
CODEX_APPROVAL_POLICY?=$(CODEX_APPROVAL)
NONSTARTER_DB?=$(HOME)/code/storage/nonstarter.db

export CODEX_SANDBOX
export CODEX_APPROVAL
export CODEX_APPROVAL_POLICY
export NONSTARTER_DB

.PHONY: tools dev dev-linode test claude claude-repl codex codex-repl codex-autowake tickle status repl \
	alfworld-server alfworld-runner alfworld-test alfworld-demo fresh

tools:
	./scripts/bootstrap-tools.sh

dev:
	@echo "[dev] Codex defaults: sandbox=$(CODEX_SANDBOX) approval=$(CODEX_APPROVAL_POLICY)"
	$(CLOJURE) -M:dev

dev-linode:
	FUTON3C_IRC_PORT=0 FUTON3C_ROLE=linode FUTON1A_STATIC_DIR=$(HOME)/code/futon4/dev/web $(MAKE) dev

dev-laptop:
	FUTON3C_ROLE=laptop FUTON3C_LINODE_URL=http://172.236.28.208:7070 $(MAKE) dev

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
  
fresh:
	@claude_session_file="$${CLAUDE_SESSION_FILE:-/tmp/futon-session-id}"; \
	codex_session_file="$${CODEX_SESSION_FILE:-/tmp/futon-codex-session-id}"; \
	echo "Clearing local agent continuity files:"; \
	echo "  - $$claude_session_file"; \
	echo "  - $$codex_session_file"; \
	rm -f "$$claude_session_file" "$$codex_session_file" /tmp/futon-irc-inbox.jsonl /tmp/futon-irc-outbox.jsonl; \
	echo "Done. Next invoke starts fresh sessions."
