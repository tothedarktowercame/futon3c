#!/usr/bin/env bash
# M-repl-circus: one id, one document — kimi-task.sh must derive the task-id
# prefix from the repo so the giraffe re-read ledger cannot mint futon2's
# E-kimi-task names. Hermetic: AGENCY_URL points at a dead port, so the bell
# fails, the script refuses (no-job) and removes the minted file — but the
# refusal line still names the id it minted, which is what we assert on.
set -euo pipefail
SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/scripts/kimi-task.sh"
WORK=$(mktemp -d); trap 'rm -rf "$WORK"' EXIT
export AGENCY_URL="http://127.0.0.1:1"

# mint_id REPO-DIRNAME SEED-FILE-NAME [KIMI_TASK_PREFIX]
# Creates a temp repo with one seeded excursion, runs the script against the
# dead Agency, prints the id the refusal line names, and asserts the minted
# file did not survive the failed dispatch.
mint_id() {
  local repo="$WORK/$1" seed="$2" prefix="${3:-}"
  mkdir -p "$repo/holes/excursions"
  printf '# %s\n\n**Requisition:** completed — x\n' "${seed%.md}" \
    > "$repo/holes/excursions/$seed.md"
  printf 'packet body\n' > "$WORK/packet.md"
  local out id
  out=$(env KIMI_TASK_REPO="$repo" ${prefix:+KIMI_TASK_PREFIX="$prefix"} \
    AGENCY_URL="$AGENCY_URL" bash "$SCRIPT" --from tester --to nobody \
    --purpose "prefix test" "$WORK/packet.md" 2>&1 >/dev/null || true)
  id=$(echo "$out" | grep -oE 'E-[A-Za-z0-9-]+-[0-9]+' | head -1 || true)
  [ "$(ls "$repo/holes/excursions")" = "$seed.md" ] \
    || { echo "FAIL($1): minted file survived a failed dispatch" >&2; exit 1; }
  echo "$id"
}

id=$(mint_id giraffe-reread-requisitions E-giraffe-read-3)
[ "$id" = "E-giraffe-read-4" ] \
  || { echo "FAIL: giraffe ledger minted '$id', want E-giraffe-read-4"; exit 1; }

# a stray legacy-prefixed file is not counted by another ledger's sequence
id=$(mint_id other-reread-repo E-kimi-task-3 E-gr2-read)
[ "$id" = "E-gr2-read-1" ] \
  || { echo "FAIL: override ledger minted '$id', want E-gr2-read-1"; exit 1; }

id=$(mint_id futon2 E-kimi-task-3)
[ "$id" = "E-kimi-task-4" ] \
  || { echo "FAIL: futon2 minted '$id', want E-kimi-task-4"; exit 1; }

id=$(mint_id somerepo E-other-ledger-9 E-other-ledger)
[ "$id" = "E-other-ledger-10" ] \
  || { echo "FAIL: KIMI_TASK_PREFIX override minted '$id', want E-other-ledger-10"; exit 1; }

echo "kimi-task prefix: ok"
