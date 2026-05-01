#!/usr/bin/env bash
# Pre-commit wrapper: runs every reachable-from-boot-style invariant in
# sequence. Each sub-check is independently activatable; the wrapper
# refuses the commit if any sub-check refuses.
#
# Currently runs:
#   1. autostash-obsolescence       — `obsolescence-recognition/autostash`
#                                     (M-archaeology-control)
#   2. reachable-from-boot          — `reachable-from-boot/evidence-store`
#   3. reachable-from-boot          — `reachable-from-boot/family-check-fns`
#   4. reachable-from-boot          — `reachable-from-boot/agent-registry`
#   5. reachable-from-boot          — `reachable-from-boot/dev-evidence-store`
#                                     (M-reachable-from-boot)
#
# Future invariants with strong-mode pre-commit binding extend this
# wrapper. Symlink this file as `.git/hooks/pre-commit` (instead of
# the autostash hook directly) when the repo participates in multiple
# pre-commit invariants.

set -e

REPO_DIR="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"
SCRIPTS="$REPO_DIR/scripts"

[[ -z "$REPO_DIR" ]] && exit 0

# Sub-checks are run in order; first refusal aborts the commit.
SUB_CHECKS=(
  "$SCRIPTS/check-autostash-obsolescence.sh"
  "$SCRIPTS/check-reachable-from-boot.sh"
  "$SCRIPTS/check-reachable-from-boot-family-check-fns.sh"
  "$SCRIPTS/check-reachable-from-boot-agent-registry.sh"
  "$SCRIPTS/check-reachable-from-boot-dev-evidence-store.sh"
)

# Some sub-checks live only in futon3c's scripts/. If a repo mirrors only
# the autostash hook (e.g. a non-futon3c futon repo), check this wrapper's
# existence and fall back to that single hook.
for chk in "${SUB_CHECKS[@]}"; do
  if [[ -x "$chk" ]]; then
    bash "$chk" || exit 1
  fi
done

exit 0
