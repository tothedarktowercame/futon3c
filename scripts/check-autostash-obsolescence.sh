#!/usr/bin/env bash
# Pre-commit hook: refuse a commit when any obsolete autostashes remain.
#
# An autostash is "obsolete" when its content is already present in the
# HEAD commit graph — i.e. `git stash show -p N | git apply --reverse
# --check` succeeds against HEAD. Such stashes are latent operational
# debt; the invariant `obsolescence-recognition/autostash` (M-archaeology-
# control) refuses to let new commits land while obsolete stashes exist,
# forcing the operator to drop them first.
#
# Activation (operator-driven):
#
#   cd ~/code/futon3c
#   ln -sf ../../scripts/check-autostash-obsolescence.sh \
#          .git/hooks/pre-commit
#
# To run manually (no installation):
#
#   bash ~/code/futon3c/scripts/check-autostash-obsolescence.sh
#
# Exit codes:
#   0  no obsolete autostashes (commit allowed)
#   1  obsolete autostashes detected (commit refused)
#
# To bypass (NOT recommended; the invariant exists precisely to catch
# this class of latent debt):
#
#   FUTON3C_SKIP_AUTOSTASH_CHECK=1 git commit ...
#
# Mission: M-archaeology-control (futon3c/holes/missions/).
# Pattern: futon3/library/invariant-coherence/subsumption-witness.flexiarg.

set -e

if [[ "${FUTON3C_SKIP_AUTOSTASH_CHECK:-0}" == "1" ]]; then
  echo "[autostash] skip requested via FUTON3C_SKIP_AUTOSTASH_CHECK; ratchet bypassed"
  exit 0
fi

# Operate against the current repo only. Pre-commit hooks live in one
# repo; cross-repo scanning is the probe's job, not the hook's.
REPO_DIR="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"

if [[ -z "$REPO_DIR" ]]; then
  echo "[autostash] not in a git repo; skipping"
  exit 0
fi

cd "$REPO_DIR"

STASH_LIST=$(git stash list --format='%gd' 2>/dev/null || echo "")

if [[ -z "$STASH_LIST" ]]; then
  # No stashes; trivially OK.
  exit 0
fi

OBSOLETE_COUNT=0
OBSOLETE_LINES=""

while IFS= read -r ref; do
  [[ -z "$ref" ]] && continue
  # `git stash show -p REF | git apply --reverse --check -` returns 0
  # iff the stash's content is already present in HEAD (the apply-in-
  # reverse against HEAD succeeds, i.e. HEAD already contains it).
  if git stash show -p "$ref" 2>/dev/null \
        | git apply --reverse --check - 2>/dev/null; then
    OBSOLETE_COUNT=$((OBSOLETE_COUNT + 1))
    SUBJ=$(git stash list --format='%s' "$ref" 2>/dev/null | head -n 1)
    OBSOLETE_LINES="${OBSOLETE_LINES}        ${ref}: ${SUBJ}\n"
  fi
done <<< "$STASH_LIST"

if [[ "$OBSOLETE_COUNT" -gt 0 ]]; then
  echo "================================================================"
  echo "[autostash] obsolescence-recognition/autostash VIOLATION"
  echo "    $OBSOLETE_COUNT obsolete autostash(es) in $REPO_DIR:"
  echo
  echo -e "$OBSOLETE_LINES"
  echo "    These stash entries are subsumed by HEAD (their content is"
  echo "    already present in committed history). Drop them before"
  echo "    committing:"
  echo
  echo "        git stash drop <ref>"
  echo
  echo "    Or to bypass once (not recommended):"
  echo
  echo "        FUTON3C_SKIP_AUTOSTASH_CHECK=1 git commit ..."
  echo "================================================================"
  exit 1
fi

# Quiet success — pre-commit hooks should be silent on the happy path.
exit 0
