#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

exec clojure -M:test -e :slow \
  -n futon3c.apm.jit-queue-coordinator-test \
  -n futon3c.apm.durable-coordinator-test \
  -n futon3c.apm.countdown-control-test
