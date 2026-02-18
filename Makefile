CLOJURE=clojure

.PHONY: dev test claude repl

dev:
	$(CLOJURE) -M:dev

test:
	$(CLOJURE) -X:test

claude:
	./scripts/claude-picker

repl:
	$(CLOJURE) -M:dev:repl
