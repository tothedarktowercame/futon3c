#!/usr/bin/env bash
# Pre-commit hook: refuse commits introducing reset!/swap! / reset-store!
# calls against the evidence-store atom from outside the bootstrap-or-test
# allowlist.
#
# This is the strong-mode binding for the structural-law-inventory invariant
# `reachable-from-boot/evidence-store`. Background: the Evidence Landscape
# was lost for ~2 weeks because `@futon3c.evidence.store/!store` was reset
# to an in-memory map and the loss was silent. The invariant makes that
# class of mutation impossible to land via commit. Watchdog → invariant.
#
# Allowlist (paths where the mutation is legitimate):
#   - dev/futon3c/dev/bootstrap.clj   — the boot construction path
#   - src/futon3c/evidence/store.clj  — defines reset-store! itself
#   - src/futon3c/evidence/backend.clj — the in-memory backend's swap! is
#                                       the AtomBackend's own internal
#                                       append (no reset of authoritative
#                                       value to a substrate-stripped state)
#   - test/**                         — test fixtures
#
# Anything else attempting reset! / swap! / reset-store! against !store
# or !evidence-store is refused.
#
# Activation:
#
#   cd ~/code/futon3c
#   ln -sf ../../scripts/check-reachable-from-boot.sh .git/hooks/pre-commit
#
# (NOTE: only one pre-commit hook can be installed per repo via direct
# symlink. If autostash hook is also installed, use a wrapper that runs
# both. For the hot-fix we install this hook in futon3c specifically since
# !store is a futon3c-only concern.)
#
# Bypass (NOT recommended; the invariant exists precisely to catch the
# silent data-loss class):
#
#   FUTON3C_SKIP_REACHABLE_FROM_BOOT=1 git commit ...
#
# Mission: M-reachable-from-boot (futon3c/holes/missions/).
# Pattern: futon3/library/invariant-coherence/reachable-from-boot.flexiarg.

set -e

if [[ "${FUTON3C_SKIP_REACHABLE_FROM_BOOT:-0}" == "1" ]]; then
  echo "[reachable-from-boot] skip requested via FUTON3C_SKIP_REACHABLE_FROM_BOOT; invariant bypassed"
  exit 0
fi

REPO_DIR="$(git rev-parse --show-toplevel 2>/dev/null || echo "")"

if [[ -z "$REPO_DIR" ]]; then
  echo "[reachable-from-boot] not in a git repo; skipping"
  exit 0
fi

cd "$REPO_DIR"

# Only enforce in the futon3c repo (the home of the !store defonce).
if [[ ! -f "src/futon3c/evidence/store.clj" ]]; then
  exit 0
fi

ALLOWED_PATHS=(
  "dev/futon3c/dev/bootstrap.clj"
  "src/futon3c/evidence/store.clj"
  "src/futon3c/evidence/backend.clj"
  "src/futon3c/evidence/xtdb_backend.clj"
)

# Pattern: reset! / swap! / reset-store! targeting !store or !evidence-store.
# Skip lines starting with `;` (comments), and the docstring lines that
# discuss the pattern.
PATTERN='(reset!|swap!|reset-store!)[[:space:]]*\(?[a-z./-]*!store\b|estore/reset-store!|store/reset-store!|reset-store!\)'

VIOLATIONS=""
VIOLATION_COUNT=0

while IFS= read -r file; do
  [[ -z "$file" ]] && continue
  # Only check .clj/.cljs/.cljc/.bb files
  case "$file" in
    *.clj|*.cljs|*.cljc|*.bb) ;;
    *) continue ;;
  esac
  # Skip allowlisted paths
  for allowed in "${ALLOWED_PATHS[@]}"; do
    [[ "$file" == "$allowed" ]] && continue 2
  done
  # Skip test/** entirely (test fixtures legitimately reset)
  case "$file" in
    test/*|*/test/*) continue ;;
  esac
  # Grep the file for violations.
  if matches=$(grep -nE "$PATTERN" "$file" 2>/dev/null); then
    while IFS= read -r match; do
      [[ -z "$match" ]] && continue
      # Skip comment lines (line content starts with optional whitespace + ;)
      content="${match#*:}"
      content="${content#*:}"
      trimmed="$(echo "$content" | sed -E 's/^[[:space:]]+//')"
      [[ "$trimmed" == ';'* ]] && continue
      VIOLATION_COUNT=$((VIOLATION_COUNT + 1))
      VIOLATIONS="${VIOLATIONS}        $file:$match\n"
    done <<< "$matches"
  fi
done < <(git diff --cached --name-only --diff-filter=ACM 2>/dev/null)

if [[ "$VIOLATION_COUNT" -gt 0 ]]; then
  echo "================================================================"
  echo "[reachable-from-boot] reachable-from-boot/evidence-store VIOLATION"
  echo "    $VIOLATION_COUNT call site(s) attempting to mutate the evidence"
  echo "    store atom from outside the construction-path allowlist:"
  echo
  echo -e "$VIOLATIONS"
  echo "    The Evidence Landscape was lost for ~2 weeks because such a"
  echo "    mutation went unprevented. The invariant makes this class"
  echo "    structurally impossible to land. Allowed locations:"
  echo
  for allowed in "${ALLOWED_PATHS[@]}"; do
    echo "        $allowed"
  done
  echo "        test/**"
  echo
  echo "    To bypass once (not recommended):"
  echo
  echo "        FUTON3C_SKIP_REACHABLE_FROM_BOOT=1 git commit ..."
  echo "================================================================"
  exit 1
fi

# Quiet success.
exit 0
