CLOJURE ?= $(shell if [ -x .tools/clojure/bin/clojure ]; then echo .tools/clojure/bin/clojure; else echo clojure; fi)
EVIDENCE_BASE?=http://localhost:7070
CODEX_SANDBOX?=danger-full-access
CODEX_APPROVAL?=never
CODEX_APPROVAL_POLICY?=$(CODEX_APPROVAL)
NONSTARTER_DB?=$(HOME)/code/storage/nonstarter.db
# Host-specific JVM sizing, passed as -J flags rather than baked into the
# shared :dev alias so two hosts never edit the same committed line. Override
# from your scripts/dev-<host>-env (a separate file per host), not here.
# Defaults are the 3.8G Linode's values (the 2026-07-14 OOM-ledge fix): -Xmx2g
# + 768m direct + Arrow native + JVM overhead ran ~3.2G resident on a 3.8G box
# already ~850M into swap, so a FUTON1B_EMBED=1 run swap-thrashed until the
# in-process :7074 query blew the 120s idle timeout ("futon1b unreachable") and
# the OOM killer reaped `make dev`. 1536m/640m is ~2.4G resident, leaving swap
# as backstop rather than working set. Raise only if the host has the RAM.
FUTON3C_HEAP ?= 1536m
FUTON3C_DIRECT ?= 640m
FUTON3C_JVM_SIZING = -J-Xmx$(FUTON3C_HEAP) -J-XX:MaxDirectMemorySize=$(FUTON3C_DIRECT)
# Embedded shadow-cljs watcher (CLAUDE.md I-0: one JVM at rest). When true,
# `make dev` runs the CLJS watch inside the futon3c JVM instead of a second
# `npx shadow-cljs` process. Both builds verified embedded 2026-05-30.
FUTON3C_SHADOW_AUTOSTART ?= true
FUTON3C_SHADOW_BUILDS ?= war-machine,webarxana
# claude lives in ~/.local/bin, which is absent from PATH when make dev is
# launched outside a login shell (systemd, desktop launcher). Absolute path
# keeps agent-pouch spawns working regardless of the JVM's inherited PATH.
CLAUDE_BIN ?= $(HOME)/.local/bin/claude
FUTON3C_REPO_BASE_EFFECTIVE ?= $(if $(FUTON3C_REPO_BASE),$(FUTON3C_REPO_BASE),$(if $(FUTON_REPO_BASE),$(FUTON_REPO_BASE),$(HOME)/code))
FUTON3C_REPOS ?= futon3c=$(FUTON3C_REPO_BASE_EFFECTIVE)/futon3c,futon3b=$(FUTON3C_REPO_BASE_EFFECTIVE)/futon3b,futon3a=$(FUTON3C_REPO_BASE_EFFECTIVE)/futon3a,futon5=$(FUTON3C_REPO_BASE_EFFECTIVE)/futon5,futon3=$(FUTON3C_REPO_BASE_EFFECTIVE)/futon3,futon4=$(FUTON3C_REPO_BASE_EFFECTIVE)/futon4,futon6=$(FUTON3C_REPO_BASE_EFFECTIVE)/futon6

export CODEX_SANDBOX
export CODEX_APPROVAL
export CODEX_APPROVAL_POLICY
export NONSTARTER_DB
export CLAUDE_BIN
export FUTON3C_SHADOW_AUTOSTART
export FUTON3C_SHADOW_BUILDS
export FUTON3C_REPOS

.PHONY: tools dev dev-linode restart test claude claude-repl codex codex-repl zai-repl codex-autowake tickle status gh-hygiene gh-issue-holes repl \
	alfworld-server alfworld-runner alfworld-test alfworld-demo fresh

tools:
	./scripts/bootstrap-tools.sh

dev:
	@echo "[dev] Codex defaults: sandbox=$(CODEX_SANDBOX) approval=$(CODEX_APPROVAL_POLICY)"
	$(CLOJURE) $(FUTON3C_JVM_SIZING) -M:dev

dev-linode:
	FUTON3C_IRC_PORT=0 FUTON3C_ROLE=linode FUTON1A_STATIC_DIR=$(HOME)/code/futon4/dev/web $(MAKE) dev

dev-laptop:
	FUTON3C_ROLE=laptop FUTON3C_LINODE_URL=http://172.236.28.208:7070 $(MAKE) dev

# Operator-only restart of the one serving JVM (I-0). Agents must NOT use this —
# reload code via Drawbridge instead (README-drawbridge.md). Delegates to the
# canonical launcher `fdev --restart`, which force-respawns the exact tmux server
# pane (kill its JVM + relaunch — surgical, no pkill). The fresh boot auto-arms
# the L4 canonical-id mission gate (futon1a cdb3359).
restart:
	fdev --restart

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

zai-repl:
	./scripts/zai-picker $(ARGS)

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

gh-hygiene:
	./scripts/github-hygiene-scan $(ARGS)

gh-issue-holes:
	@if command -v bb >/dev/null 2>&1; then \
		./scripts/gh-issue-holes $(ARGS); \
	else \
		$(CLOJURE) -M -m futon3c.peripheral.issue-holes $(ARGS); \
	fi

repl:
	$(CLOJURE) $(FUTON3C_JVM_SIZING) -M:dev:repl

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
