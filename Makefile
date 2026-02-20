CLOJURE=clojure
EVIDENCE_BASE?=http://localhost:7070

.PHONY: dev test claude codex codex-autowake tickle status repl

dev:
	$(CLOJURE) -M:dev

test:
	$(CLOJURE) -X:test

claude:
	./scripts/claude-picker $(ARGS)

codex:
	./scripts/codex-picker $(ARGS)

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
