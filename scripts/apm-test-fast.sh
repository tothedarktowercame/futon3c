#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

exec clojure -M:test -e :slow \
  -n futon3c.apm.solver-progress-rollover-test \
  -n futon3c.apm.problem-queue-supervisor-test \
  -n futon3c.apm.queued-frame-terminal-test \
  -n futon3c.apm.live-solver-rounds-test \
  -n futon3c.apm.live-proof-phases-test \
  -n futon3c.apm.generated-contract-test \
  -n futon3c.apm.qualification-test \
  -n futon3c.apm.job-port-contract-test \
  -n futon3c.apm.authority-port-test \
  -n futon3c.apm.toolchain-port-test \
  -n futon3c.apm.disruption-soak-test
