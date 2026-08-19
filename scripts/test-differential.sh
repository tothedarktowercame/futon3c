#!/usr/bin/env bash
# Differential full-suite gate.
#
# `make test` cannot be a pass/fail gate in this repo today, for two reasons
# measured on 2026-08-19:
#
#  1. the suite is ALREADY red at base -- 133 failures / 39 errors at
#     42196c67, with no change applied at all;
#  2. the integration tests host futon1b/XTDB2 in-process (see deps.edn :test)
#     and so fight the live stack for ports. One run aborted outright with
#     "Address already in use", and counts wander between runs (87, 126, 133
#     were all observed for near-identical trees).
#
# So the answerable question is not "is it green" but "did this change make it
# worse". This runs the suite at HEAD and at a base ref in a sibling worktree
# and compares. A base worktree must be a SIBLING of the other futon repos --
# deps.edn uses :local/root "../futon1b", so /tmp does not work.
#
# Stop the local futon1b before trusting the numbers:
#     systemctl --user stop futon1b-server.service
set -uo pipefail

BASE_REF=${1:-origin/master}
REPO=$(cd "$(dirname "$0")/.." && pwd)
WT="$(dirname "$REPO")/f3c-diffbase-$$"

counts() {  # -> "<failures> <errors>"
  (cd "$1" && timeout 900 clojure -M:test 2>&1 | tail -3 \
    | grep -oE '[0-9]+ failures, [0-9]+ errors' \
    | grep -oE '[0-9]+' | tr '\n' ' ')
}

echo "== HEAD ($(git -C "$REPO" rev-parse --short HEAD))"
head_c=$(counts "$REPO"); echo "   failures/errors: ${head_c:-UNPARSEABLE}"

echo "== base ($BASE_REF)"
git -C "$REPO" worktree add -q --detach "$WT" "$BASE_REF" || { echo "   could not create worktree"; exit 2; }
base_c=$(counts "$WT")
git -C "$REPO" worktree remove --force "$WT" 2>/dev/null
echo "   failures/errors: ${base_c:-UNPARSEABLE}"

set -- $head_c; hf=${1:-}; he=${2:-}
set -- $base_c; bf=${1:-}; be=${2:-}
if [ -z "$hf" ] || [ -z "$bf" ]; then
  echo "== INCONCLUSIVE: could not parse both runs (ports held? see header)"; exit 2
fi
echo "== base $bf failures / $be errors  ->  HEAD $hf failures / $he errors"
if [ "$hf" -le "$bf" ] && [ "$he" -le "$be" ]; then
  echo "== PASS: no new failures or errors"; exit 0
fi
echo "== FAIL: this change adds failures or errors"; exit 1
