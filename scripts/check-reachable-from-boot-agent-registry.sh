#!/usr/bin/env bash
# Pre-commit hook: refuse commits introducing direct `reset!` / `swap!`
# mutations of `futon3c.agency.registry/!registry` outside the registry
# module's own helper surface.
#
# This is the strong-mode binding for
# `reachable-from-boot/agent-registry`.

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

if [[ ! -f "src/futon3c/agency/registry.clj" ]]; then
  exit 0
fi

ALLOWED_PATHS=(
  "dev/futon3c/dev/bootstrap.clj"
  "src/futon3c/agency/registry.clj"
)

PATTERN='\([[:space:]]*(reset!|swap!)[[:space:]]+\(?[[:alnum:]_./-]*!registry\b'

VIOLATIONS=""
VIOLATION_COUNT=0

while IFS= read -r file; do
  [[ -z "$file" ]] && continue
  case "$file" in
    *.clj|*.cljs|*.cljc|*.bb) ;;
    *) continue ;;
  esac
  for allowed in "${ALLOWED_PATHS[@]}"; do
    [[ "$file" == "$allowed" ]] && continue 2
  done
  case "$file" in
    test/*|*/test/*) continue ;;
  esac
  if matches=$(grep -nE "$PATTERN" "$file" 2>/dev/null); then
    while IFS= read -r match; do
      [[ -z "$match" ]] && continue
      content="${match#*:}"
      trimmed="$(echo "$content" | sed -E 's/^[[:space:]]+//')"
      [[ "$trimmed" == ';'* ]] && continue
      VIOLATION_COUNT=$((VIOLATION_COUNT + 1))
      VIOLATIONS="${VIOLATIONS}        $file:$match\n"
    done <<< "$matches"
  fi
done < <(git diff --cached --name-only --diff-filter=ACM 2>/dev/null)

if [[ "$VIOLATION_COUNT" -gt 0 ]]; then
  echo "================================================================"
  echo "[reachable-from-boot] reachable-from-boot/agent-registry VIOLATION"
  echo "    $VIOLATION_COUNT staged call site(s) mutate !registry outside"
  echo "    the agency module's construction-path allowlist:"
  echo
  echo -e "$VIOLATIONS"
  echo "    Allowed locations:"
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

exit 0
