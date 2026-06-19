#!/usr/bin/env bash
# scripts/check-coverage-ratchet.sh
#
# Pre-commit / pre-push hook for I-coverage-ratchet.
#
# Validates that any reduction in `:status` of an operational-family entry
# in docs/structural-law-inventory.sexp is matched by a :family-demoted
# evidence entry in the futon1a evidence store.
#
# Usage:
#   scripts/check-coverage-ratchet.sh          # run the check, exit 0/1
#
# Exit codes:
#   0 — ratchet holds (no demotions, or all demotions reconciled by evidence)
#   1 — ratchet violation (one or more demotions lack a matching evidence entry)
#   2 — script setup error (missing clojure, can't find repo root, etc.)
#
# Mission: M-invariant-queue-unstuck.

set -euo pipefail

# Move to repo root (works whether invoked from anywhere)
cd "$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "[ratchet] not inside a git repo — skipping" >&2
  exit 0
}

# If the inventory file isn't staged, skip the check (nothing for the ratchet
# to bind on; commit unrelated to the inventory).
if ! git diff --cached --name-only 2>/dev/null \
     | grep -q '^docs/structural-law-inventory\.sexp$'; then
  exit 0
fi

# Need clojure to invoke the ratchet
if ! command -v clojure >/dev/null 2>&1; then
  echo "[ratchet] clojure not on PATH — skipping (configure CI to fail loudly)" >&2
  exit 2
fi

# Run the ratchet entry point. -M:dev brings in the dev classpath so the
# default !store atom is reachable. The entry point exits 0/1 as appropriate.
exec clojure -M:dev -m futon3c.logic.ratchet
