CLOJURE=clojure

.PHONY: dev test claude codex repl

dev:
	$(CLOJURE) -M:dev

test:
	$(CLOJURE) -X:test

claude:
	./scripts/claude-picker $(ARGS)

codex:
	./scripts/codex-picker $(ARGS)

repl:
	$(CLOJURE) -M:dev:repl
