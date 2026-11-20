#!/usr/bin/env bash
# Run every test suite of the Together work (M-arxana-together), in both repos.
# Nothing here touches the live hub on port 7096, ~/storage/arxana-together, or a
# running Emacs: the end-to-end scripts start their own hub on a free port in a
# temporary directory. Usage: scripts/together-check.sh [--fast]
#   --fast  skips the browser run, the Emacs end-to-end scripts and the slow
#           catalogue probes.
set -u
here="$(cd "$(dirname "$0")/.." && pwd)"
futon4a="$(cd "$here/../futon4a" && pwd)"
fast=0; [ "${1:-}" = "--fast" ] && fast=1
failed=()
run() { # name, dir, command...
  local name="$1" dir="$2"; shift 2
  local log; log="$(mktemp)"
  if (cd "$dir" && "$@") >"$log" 2>&1; then
    printf 'ok    %s  %s\n' "$name" "$(grep -E '^Ran |^ℹ pass|SUMMARY|PASS' "$log" | tail -1)"
  else
    printf 'FAIL  %s  (log: %s)\n' "$name" "$log"; tail -15 "$log" | sed 's/^/      /'
    failed+=("$name")
  fi
}
[ -n "$(ls "$futon4a"/lisp/*.elc 2>/dev/null)" ] && { echo "FAIL  stale .elc files in $futon4a/lisp (they shadow the sources)"; failed+=(elc); }

run "clojure: engine, hub, structure, catalogues" "$here" \
  clojure -M:test -n futon3c.arxana.together-test -n futon3c.arxana.together-http-test \
    -n futon3c.arxana.together-structure-test -n futon3c.arxana.together-structure-http-test \
    -n futon3c.arxana.together-catalogue-test -n futon3c.arxana.catalogue-document-test \
    -n futon3c.arxana.incubator-catalogue-test
run "javascript: model and binding" "$here" node --test test/js/binding.test.mjs test/js/together.test.mjs
run "emacs: model, binding, transport, live, incubator sharing" "$futon4a" \
  emacs -Q --batch -L lisp -L "$here/emacs" -l test/futon4a-together-test.el \
    -l test/futon4a-together-buffer-test.el -l test/futon4a-together-net-test.el \
    -l test/futon4a-together-live-test.el -l test/futon4a-incubator-together-test.el \
    -f ert-run-tests-batch-and-exit
run "emacs: incubator form" "$futon4a" \
  emacs -Q --batch -L "$here/emacs" -L lisp -L test -l test/futon4a-incubator-test.el \
    -f ert-run-tests-batch-and-exit
if [ "$fast" = 0 ]; then
  run "browser: two-browser run against a throwaway hub" "$here" node test/js/page-e2e.mjs
  run "emacs end to end: transport" "$futon4a" bash test/together-net-e2e.sh
  run "emacs end to end: live buffers" "$futon4a" bash test/together-live-e2e.sh
  run "emacs end to end: incubator sharing" "$futon4a" bash test/incubator-together-e2e.sh
  run "clojure: catalogue probes that run the ERT suites (slow)" "$here" \
    clojure -M:test:test-all -n futon3c.arxana.incubator-catalogue-test
fi
if [ ${#failed[@]} -eq 0 ]; then echo "all suites passed"; else echo "FAILED: ${failed[*]}"; exit 1; fi
