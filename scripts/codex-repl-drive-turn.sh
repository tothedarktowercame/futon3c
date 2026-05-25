#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUFFER_NAME="${CODEX_REPL_BUFFER_NAME:-*codex-repl:codex-8*}"
PROMPT_FILE="${CODEX_REPL_PROMPT_FILE:-}"
TIMEOUT_SECONDS="${CODEX_REPL_DRIVE_TIMEOUT_SECONDS:-900}"
REPORT_FILE="${CODEX_REPL_TIMING_REPORT_FILE:-/tmp/codex-repl-last-timing.sexp}"

if [[ $# -gt 0 && -n "${PROMPT_FILE}" ]]; then
  echo "Use either argv prompt text or CODEX_REPL_PROMPT_FILE, not both." >&2
  exit 2
fi

if [[ -n "${PROMPT_FILE}" ]]; then
  if [[ ! -r "${PROMPT_FILE}" ]]; then
    echo "Prompt file is not readable: ${PROMPT_FILE}" >&2
    exit 2
  fi
  PROMPT_CONTENT="$(cat "${PROMPT_FILE}")"
elif [[ $# -gt 0 ]]; then
  PROMPT_CONTENT="$*"
else
  PROMPT_CONTENT="Reply with exactly OK."
fi

lisp_string() {
  python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1"
}

BUFFER_LISP="$(lisp_string "${BUFFER_NAME}")"
PROMPT_LISP="$(lisp_string "${PROMPT_CONTENT}")"

STATE_PROBE_EL="$(mktemp /tmp/codex-repl-drive-state.XXXXXX.el)"
cleanup() {
  rm -f "${TMP_EL:-}" "${STATE_PROBE_EL:-}"
}
trap cleanup EXIT
STATE_PROBE_EL_LISP="$(lisp_string "${STATE_PROBE_EL}")"

cat >"${STATE_PROBE_EL}" <<EOF
(let ((buffer (get-buffer ${BUFFER_LISP})))
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (prin1-to-string
         (list :live t
               :major major-mode
               :pending (and (process-live-p agent-chat--pending-process) t)
               :input-start (and (markerp agent-chat--input-start)
                                 (marker-position agent-chat--input-start))
               :turn-id codex-repl--invoke-turn-id)))
    nil))
EOF

STATE_SEXP="$(emacsclient --eval "(load-file ${STATE_PROBE_EL_LISP})")"

if [[ "${STATE_SEXP}" == "nil" ]]; then
  echo "Target buffer is not live in Emacs: ${BUFFER_NAME}" >&2
  exit 3
fi

if grep -q ":pending t" <<<"${STATE_SEXP}"; then
  echo "Codex REPL buffer is busy; refusing to inject a new turn: ${BUFFER_NAME}" >&2
  echo "${STATE_SEXP}" >&2
  exit 4
fi

BEFORE_MTIME=0
if [[ -f "${REPORT_FILE}" ]]; then
  BEFORE_MTIME="$(stat -c %Y "${REPORT_FILE}")"
fi

TMP_EL="$(mktemp /tmp/codex-repl-drive-turn.XXXXXX.el)"
TMP_EL_LISP="$(lisp_string "${TMP_EL}")"

cat >"${TMP_EL}" <<EOF
(let ((buffer (get-buffer ${BUFFER_LISP})))
  (unless (buffer-live-p buffer)
    (error "Target buffer not live: %s" ${BUFFER_LISP}))
  (with-current-buffer buffer
    (unless (eq major-mode 'codex-repl-mode)
      (error "Target buffer is not codex-repl-mode: %S" major-mode))
    (when (process-live-p agent-chat--pending-process)
      (error "Target Codex REPL already has a pending process"))
    (goto-char (point-max))
    (delete-region (marker-position agent-chat--input-start) (point-max))
    (insert ${PROMPT_LISP})
    (codex-repl-send-input)
    (with-temp-file "/tmp/codex-repl-drive-turn-submit.sexp"
      (prin1
       (list :buffer (buffer-name)
             :prompt ${PROMPT_LISP}
             :turn-id codex-repl--invoke-turn-id
             :submitted-at (format-time-string "%FT%T%z")
             :pending (and (process-live-p agent-chat--pending-process) t))
       (current-buffer)))))
EOF

emacsclient --eval "(load-file ${TMP_EL_LISP})" >/dev/null

START_TS="$(date +%s)"
while true; do
  NOW_TS="$(date +%s)"
  if (( NOW_TS - START_TS > TIMEOUT_SECONDS )); then
    echo "Timed out waiting for timing report update: ${REPORT_FILE}" >&2
    exit 5
  fi

  if [[ -f "${REPORT_FILE}" ]]; then
    CURRENT_MTIME="$(stat -c %Y "${REPORT_FILE}")"
    if (( CURRENT_MTIME > BEFORE_MTIME )); then
      cat /tmp/codex-repl-drive-turn-submit.sexp
      printf '\n---\n'
      cat "${REPORT_FILE}"
      exit 0
    fi
  fi

  sleep 1
done
