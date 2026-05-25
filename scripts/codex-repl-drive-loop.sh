#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DRIVER="${ROOT}/scripts/codex-repl-drive-turn.sh"
BUFFER_NAME="${CODEX_REPL_BUFFER_NAME:-*codex-repl:codex-8*}"
COUNT="${CODEX_REPL_LOOP_COUNT:-3}"
SLEEP_SECONDS="${CODEX_REPL_LOOP_SLEEP_SECONDS:-2}"
IDLE_TIMEOUT_SECONDS="${CODEX_REPL_LOOP_IDLE_TIMEOUT_SECONDS:-900}"
OUT_DIR="${CODEX_REPL_LOOP_OUT_DIR:-/tmp/codex-repl-drive-loop}"

if [[ ! -x "${DRIVER}" ]]; then
  echo "Single-turn driver is missing or not executable: ${DRIVER}" >&2
  exit 2
fi

mkdir -p "${OUT_DIR}"
RUN_STAMP="$(date +%Y%m%dT%H%M%S)"
RUN_DIR="${OUT_DIR}/${RUN_STAMP}"
mkdir -p "${RUN_DIR}"

lisp_string() {
  python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1"
}

BUFFER_LISP="$(lisp_string "${BUFFER_NAME}")"

wait_until_idle() {
  local start_ts now_ts state
  start_ts="$(date +%s)"
  while true; do
    state="$(emacsclient --eval "(let ((buffer (get-buffer ${BUFFER_LISP}))) (if (buffer-live-p buffer) (with-current-buffer buffer (list :live t :pending (and (boundp 'agent-chat--pending-process) (process-live-p agent-chat--pending-process) t) :thinking (and (boundp 'codex-repl--thinking-start-time) codex-repl--thinking-start-time))) '(:live nil)))")"
    if ! grep -q ":pending t" <<<"${state}"; then
      printf '%s\n' "${state}"
      return 0
    fi

    now_ts="$(date +%s)"
    if (( now_ts - start_ts > IDLE_TIMEOUT_SECONDS )); then
      echo "Timed out waiting for idle Codex REPL buffer: ${BUFFER_NAME}" >&2
      echo "${state}" >&2
      return 1
    fi

    sleep 1
  done
}

split_driver_output() {
  local raw_file submit_file report_file
  raw_file="$1"
  submit_file="$2"
  report_file="$3"
  python3 - "$raw_file" "$submit_file" "$report_file" <<'PY'
import pathlib
import sys

raw_path = pathlib.Path(sys.argv[1])
submit_path = pathlib.Path(sys.argv[2])
report_path = pathlib.Path(sys.argv[3])
text = raw_path.read_text()
parts = text.split("\n---\n", 1)
if len(parts) != 2:
    raise SystemExit(f"Could not split driver output: {raw_path}")
submit_path.write_text(parts[0].strip() + "\n")
report_path.write_text(parts[1].strip() + "\n")
PY
}

report_summary() {
  local report_file
  report_file="$1"
  python3 - "$report_file" <<'PY'
import pathlib
import re
import sys

text = pathlib.Path(sys.argv[1]).read_text()

def capture(pattern, default="?"):
    m = re.search(pattern, text, re.S)
    return m.group(1) if m else default

fields = {
    "generated_at": capture(r':generated-at "([^"]+)"'),
    "turn_id": capture(r':turn-id ([0-9]+)'),
    "ret_to_dispatch_ms": capture(r':label "ret->dispatch".*?:elapsed-ms ([0-9]+)'),
    "first_text_ms": capture(r':label "first-event->first-text".*?:elapsed-ms ([0-9]+)'),
    "finish_visible_ms": capture(r':label "finish-enter->visible-finished".*?:elapsed-ms ([0-9]+)'),
    "done_cleanup_ms": capture(r':label "done->cleanup".*?:elapsed-ms ([0-9]+)'),
    "ret_cleanup_ms": capture(r':label "ret->cleanup".*?:elapsed-ms ([0-9]+)'),
}

print(
    "{generated_at}\tturn={turn_id}\tret->dispatch={ret_to_dispatch_ms}ms\t"
    "first-text={first_text_ms}ms\tfinish-visible={finish_visible_ms}ms\t"
    "done->cleanup={done_cleanup_ms}ms\tret->cleanup={ret_cleanup_ms}ms".format(**fields)
)
PY
}

write_summary() {
  local tsv_file summary_file
  tsv_file="$1"
  summary_file="$2"
  python3 - "$tsv_file" "$summary_file" <<'PY'
import csv
import pathlib
import statistics
import sys

tsv_path = pathlib.Path(sys.argv[1])
summary_path = pathlib.Path(sys.argv[2])
rows = list(csv.DictReader(tsv_path.open(), delimiter="\t"))
metrics = [
    "ret_to_dispatch_ms",
    "first_text_ms",
    "finish_visible_ms",
    "done_cleanup_ms",
    "ret_cleanup_ms",
]

lines = [f"runs={len(rows)}"]
for metric in metrics:
    values = [int(row[metric]) for row in rows if row.get(metric, "").isdigit()]
    if not values:
      continue
    avg = round(statistics.fmean(values), 1)
    lines.append(
        f"{metric}: min={min(values)} max={max(values)} avg={avg}"
    )

summary_path.write_text("\n".join(lines) + "\n")
print(summary_path.read_text(), end="")
PY
}

TSV_FILE="${RUN_DIR}/timings.tsv"
printf 'run\tgenerated_at\tturn_id\tret_to_dispatch_ms\tfirst_text_ms\tfinish_visible_ms\tdone_cleanup_ms\tret_cleanup_ms\n' >"${TSV_FILE}"

printf 'run-dir=%s\n' "${RUN_DIR}"

for ((i = 1; i <= COUNT; i++)); do
  wait_until_idle >/dev/null

  RAW_FILE="${RUN_DIR}/run-${i}.raw"
  SUBMIT_FILE="${RUN_DIR}/run-${i}.submit.sexp"
  REPORT_FILE="${RUN_DIR}/run-${i}.timing.sexp"

  bash "${DRIVER}" "$@" >"${RAW_FILE}"
  split_driver_output "${RAW_FILE}" "${SUBMIT_FILE}" "${REPORT_FILE}"

  SUMMARY_LINE="$(report_summary "${REPORT_FILE}")"
  printf 'run %d: %s\n' "${i}" "${SUMMARY_LINE}"

  python3 - "${REPORT_FILE}" "${TSV_FILE}" "${i}" <<'PY'
import pathlib
import re
import sys

report_text = pathlib.Path(sys.argv[1]).read_text()
tsv_path = pathlib.Path(sys.argv[2])
run_number = sys.argv[3]

def capture(pattern, default=""):
    m = re.search(pattern, report_text, re.S)
    return m.group(1) if m else default

row = [
    run_number,
    capture(r':generated-at "([^"]+)"'),
    capture(r':turn-id ([0-9]+)'),
    capture(r':label "ret->dispatch".*?:elapsed-ms ([0-9]+)'),
    capture(r':label "first-event->first-text".*?:elapsed-ms ([0-9]+)'),
    capture(r':label "finish-enter->visible-finished".*?:elapsed-ms ([0-9]+)'),
    capture(r':label "done->cleanup".*?:elapsed-ms ([0-9]+)'),
    capture(r':label "ret->cleanup".*?:elapsed-ms ([0-9]+)'),
]

with tsv_path.open("a") as fh:
    fh.write("\t".join(row) + "\n")
PY

  if (( i < COUNT )); then
    sleep "${SLEEP_SECONDS}"
  fi
done

SUMMARY_FILE="${RUN_DIR}/summary.txt"
printf '%s\n' "---"
write_summary "${TSV_FILE}" "${SUMMARY_FILE}"
