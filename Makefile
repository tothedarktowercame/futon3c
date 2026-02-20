CLOJURE=clojure

.PHONY: dev test claude codex tickle repl

dev:
	$(CLOJURE) -M:dev

test:
	$(CLOJURE) -X:test

claude:
	./scripts/claude-picker $(ARGS)

codex:
	./scripts/codex-picker $(ARGS)

tickle:
	./scripts/tickle-start $(ARGS)

repl:
	$(CLOJURE) -M:dev:repl
