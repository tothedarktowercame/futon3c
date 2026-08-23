#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

exec clojure -M:test -i :slow \
  -n futon3c.apm.countdown-control-test
