#!/usr/bin/env bash
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# :test excludes ^:slow by default; the :test-all overlay lifts that.
exec clojure -M:test:test-all -i :slow \
  -n futon3c.apm.countdown-control-test
