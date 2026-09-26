#!/usr/bin/env bash
# Cron-safe APM natural-language proof harvest via Claude Opus.
#
# This intentionally runs outside the live Agency conductor:
# - explicit Opus selection via `claude -p --model opus`
# - no JVM/server dependency
# - resumable by output files in data/apm-informal-proofs/
# - live usage gate: queries the Claude usage API (the same endpoint the
#   interactive /usage command uses, authed via ~/.claude/.credentials.json)
#   and skips the run when 5-hour or weekly utilization is above ceiling,
#   so APM only spends genuinely idle window headroom
# - local 5-hour slot cap as backstop when the usage API is unavailable,
#   and as a damper so a fully idle window is not all spent on APM.

set -euo pipefail

FUTON3C_DIR="${FUTON3C_DIR:-/home/joe/code/futon3c}"
APM_SOURCE_DIR="${APM_SOURCE_DIR:-/home/joe/code/apm-lean/apm}"
OUTPUT_DIR="${APM_HARVEST_OUTPUT_DIR:-$FUTON3C_DIR/data/apm-informal-proofs}"
LOG_PATH="${APM_HARVEST_LOG:-/home/joe/code/futon2/logs/apm-nl-harvest-opus.log}"
STATE_DIR="${APM_HARVEST_STATE_DIR:-$FUTON3C_DIR/.state}"
STATE_PATH="${APM_HARVEST_STATE:-$STATE_DIR/apm-nl-harvest-opus.state}"
LOCK_PATH="${APM_HARVEST_LOCK:-/tmp/apm-nl-harvest-opus.lock}"

CLAUDE_BIN="${CLAUDE_BIN:-claude}"
CLAUDE_MODEL="${CLAUDE_MODEL:-opus}"
TIMEOUT_SECONDS="${APM_HARVEST_TIMEOUT_SECONDS:-1500}"
MAX_PER_RUN="${APM_HARVEST_MAX_PER_RUN:-2}"
MAX_PER_SLOT="${APM_HARVEST_MAX_PER_SLOT:-4}"
SLOT_SECONDS="${APM_HARVEST_SLOT_SECONDS:-18000}"
MIN_OUTPUT_BYTES="${APM_HARVEST_MIN_OUTPUT_BYTES:-1200}"
# Live usage gate ceilings (percent utilization from the Claude usage API).
# Opus in print mode draws from the 5-hour session pool and the weekly-all
# pool; the model-scoped weekly limits (e.g. Fable) do not apply to it.
MAX_5H_UTIL="${APM_HARVEST_MAX_5H_UTIL:-70}"
MAX_WEEKLY_UTIL="${APM_HARVEST_MAX_WEEKLY_UTIL:-90}"
USAGE_TIMEOUT_SECONDS="${APM_HARVEST_USAGE_TIMEOUT_SECONDS:-15}"
DRY_RUN=0

if [[ "${1:-}" == "--dry-run" ]]; then
  DRY_RUN=1
fi

mkdir -p "$(dirname "$LOG_PATH")" "$OUTPUT_DIR" "$STATE_DIR"
exec 9>"$LOCK_PATH"
if ! flock -n 9; then
  printf '%s already-running lock=%s\n' "$(date -Is)" "$LOCK_PATH" >>"$LOG_PATH"
  exit 0
fi

log() {
  printf '%s %s\n' "$(date -Is)" "$*" | tee -a "$LOG_PATH"
}

current_slot() {
  python3 - <<PY
import time
print(int(time.time()) // int("$SLOT_SECONDS"))
PY
}

read_slot_count() {
  local slot="$1"
  if [[ -f "$STATE_PATH" ]]; then
    local saved_slot saved_count
    read -r saved_slot saved_count <"$STATE_PATH" || true
    if [[ "$saved_slot" == "$slot" ]]; then
      printf '%s\n' "${saved_count:-0}"
      return
    fi
  fi
  printf '0\n'
}

write_slot_count() {
  local slot="$1"
  local count="$2"
  printf '%s %s\n' "$slot" "$count" >"$STATE_PATH"
}

missing_ids() {
  APM_SOURCE_DIR="$APM_SOURCE_DIR" OUTPUT_DIR="$OUTPUT_DIR" python3 - <<'PY'
import os
from pathlib import Path

source_dir = Path(os.environ["APM_SOURCE_DIR"])
output_dir = Path(os.environ["OUTPUT_DIR"])

source_ids = {p.stem for p in source_dir.glob("*.tex")}
done_ids = set()
for p in output_dir.glob("apm-*.md"):
    try:
        if p.stat().st_size >= 100:
            done_ids.add(p.stem.removeprefix("apm-"))
    except OSError:
        pass

for pid in sorted(source_ids - done_ids):
    print(pid)
PY
}

# Live usage gate against the same endpoint the interactive /usage command
# uses. Prints exactly one line:
#   OK 5h=<pct> weekly=<pct> resets_at=<ts>   — under ceilings, proceed
#   SKIP <reason>                             — over a ceiling, do not spend
#   UNAVAILABLE <reason>                      — no live data; fall back to
#                                               the local slot cap only
# The endpoint is undocumented, so UNAVAILABLE must never abort the harvest.
usage_gate() {
  APM_MAX_5H_UTIL="$MAX_5H_UTIL" \
  APM_MAX_WEEKLY_UTIL="$MAX_WEEKLY_UTIL" \
  APM_USAGE_TIMEOUT="$USAGE_TIMEOUT_SECONDS" \
  python3 - <<'PY'
import json, os, time, urllib.request
from pathlib import Path

max_5h = float(os.environ["APM_MAX_5H_UTIL"])
max_weekly = float(os.environ["APM_MAX_WEEKLY_UTIL"])
timeout = float(os.environ["APM_USAGE_TIMEOUT"])

try:
    creds = json.loads(
        (Path.home() / ".claude" / ".credentials.json").read_text()
    )["claudeAiOauth"]
    if creds.get("expiresAt", 0) / 1000 < time.time():
        print("UNAVAILABLE access-token-expired")
        raise SystemExit(0)
    token = creds["accessToken"]
except SystemExit:
    raise
except Exception as exc:
    print(f"UNAVAILABLE credentials: {exc}")
    raise SystemExit(0)

req = urllib.request.Request(
    "https://api.anthropic.com/api/oauth/usage",
    headers={"Authorization": f"Bearer {token}",
             "anthropic-beta": "oauth-2025-04-20"})
try:
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        obj = json.load(resp)
except Exception as exc:
    print(f"UNAVAILABLE usage-api: {exc}")
    raise SystemExit(0)

five = obj.get("five_hour") or {}
util_5h = five.get("utilization")
resets = five.get("resets_at", "?")
weekly = None
for lim in obj.get("limits", []):
    if lim.get("kind") == "weekly_all":
        weekly = lim.get("percent")

if util_5h is None:
    print("UNAVAILABLE no-five-hour-data")
elif util_5h >= max_5h:
    print(f"SKIP five-hour-utilization={util_5h} ceiling={max_5h} resets_at={resets}")
elif weekly is not None and weekly >= max_weekly:
    print(f"SKIP weekly-utilization={weekly} ceiling={max_weekly}")
else:
    print(f"OK 5h={util_5h} weekly={weekly} resets_at={resets}")
PY
}

build_prompt() {
  local pid="$1"
  local tex_path="$APM_SOURCE_DIR/$pid.tex"
  python3 - "$pid" "$tex_path" <<'PY'
from pathlib import Path
import sys

pid = sys.argv[1]
tex = Path(sys.argv[2]).read_text(errors="replace")
print(f"""Problem: apm-{pid}

```latex
{tex}
```

You are doing the Claude side of the APM natural-language proof harvest.

Constraints:
- Use Claude Opus-level mathematical care.
- Do not use tools.
- Do not run Lean or lake.
- Do not create files.
- Reply inline only.
- This output will be saved as futon3c/data/apm-informal-proofs/apm-{pid}.md.

Deliver exactly these sections:

## 1. Informal proof
- Why it's hard (1-2 sentences)
- The key insight (1-2 sentences)
- Complete readable proof (not a sketch)
- What connects (1 paragraph)

## 2. Lean 4 theorem statement
State the main theorem as a Lean 4 declaration with full type signature, using
real Mathlib types where practical. Body is `sorry`. Include helper lemmas as
separate `sorry`-bodied statements only when they clarify the formalization.

```lean
-- example shape:
theorem apm_{pid}_main : True := by sorry
```

## 3. Mathlib cross-references
List concrete Mathlib names or search terms that would be used later by a Lean
formalizer. Group by purpose:
- **Types/structures**
- **Key lemmas**
- **Tactic hints**

Be specific. Do not invent a completed formal proof.
""")
PY
}

slot="$(current_slot)"
slot_count="$(read_slot_count "$slot")"
remaining_slot=$(( MAX_PER_SLOT - slot_count ))
if (( remaining_slot <= 0 )); then
  log "slot-cap-reached slot=$slot count=$slot_count max=$MAX_PER_SLOT"
  exit 0
fi

gate="$(usage_gate)"
case "$gate" in
  SKIP*)
    log "usage-gate $gate"
    exit 0
    ;;
  UNAVAILABLE*)
    log "usage-gate $gate (local slot cap is the only control this run)"
    ;;
  *)
    log "usage-gate $gate"
    ;;
esac

mapfile -t ids < <(missing_ids)
if (( ${#ids[@]} == 0 )); then
  log "complete no-missing-informal-proofs"
  exit 0
fi

to_run="$MAX_PER_RUN"
if (( to_run > remaining_slot )); then
  to_run="$remaining_slot"
fi
if (( to_run > ${#ids[@]} )); then
  to_run="${#ids[@]}"
fi

log "start model=$CLAUDE_MODEL missing=${#ids[@]} to_run=$to_run slot=$slot slot_count=$slot_count max_per_slot=$MAX_PER_SLOT"

if (( DRY_RUN )); then
  printf 'DRY RUN: would process %s\n' "${ids[@]:0:$to_run}" | tee -a "$LOG_PATH"
  exit 0
fi

processed=0
for pid in "${ids[@]:0:$to_run}"; do
  out="$OUTPUT_DIR/apm-$pid.md"
  if [[ -s "$out" ]]; then
    log "skip-existing pid=$pid path=$out"
    continue
  fi

  # One harvest call can move the 5-hour needle; re-check before each
  # subsequent spend. UNAVAILABLE keeps going — the slot cap still holds.
  if (( processed > 0 )); then
    gate="$(usage_gate)"
    log "usage-gate-recheck $gate"
    case "$gate" in
      SKIP*) break ;;
    esac
  fi

  prompt="$(mktemp "/tmp/apm-harvest-$pid.prompt.XXXXXX")"
  tmp_out="$(mktemp "/tmp/apm-harvest-$pid.out.XXXXXX")"
  build_prompt "$pid" >"$prompt"

  log "invoke pid=$pid model=$CLAUDE_MODEL"
  set +e
  timeout "$TIMEOUT_SECONDS" "$CLAUDE_BIN" \
    -p \
    --model "$CLAUDE_MODEL" \
    --no-session-persistence \
    --permission-mode dontAsk \
    --disallowedTools Bash,Read,Edit,Write,WebFetch,WebSearch \
    <"$prompt" >"$tmp_out" 2>>"$LOG_PATH"
  rc=$?
  set -e

  slot_count=$(( slot_count + 1 ))
  write_slot_count "$slot" "$slot_count"

  bytes=0
  [[ -f "$tmp_out" ]] && bytes="$(wc -c <"$tmp_out")"
  if (( rc == 0 && bytes >= MIN_OUTPUT_BYTES )); then
    mv "$tmp_out" "$out"
    processed=$(( processed + 1 ))
    log "ok pid=$pid bytes=$bytes path=$out slot_count=$slot_count"
  else
    failed="$OUTPUT_DIR/apm-$pid.failed.$(date +%Y%m%dT%H%M%S).md"
    mv "$tmp_out" "$failed" 2>/dev/null || true
    log "failed pid=$pid rc=$rc bytes=$bytes failed_path=$failed slot_count=$slot_count"
    rm -f "$prompt"
    break
  fi
  rm -f "$prompt"
done

log "done processed=$processed slot=$slot slot_count=$slot_count"
