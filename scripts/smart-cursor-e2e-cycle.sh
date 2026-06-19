#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUTFILE="${SMART_CURSOR_E2E_OUTFILE:-/tmp/smart_cursor_e2e_cycle_reply.sexp}"
TRACE_OUTFILE="${SMART_CURSOR_E2E_TRACE_OUTFILE:-${OUTFILE%.sexp}.trace.txt}"

if [[ $# -gt 0 ]]; then
  BUFFERS=("$@")
else
  BUFFERS=(
    "*codex-repl:codex-8*"
    "*claude-repl:claude-1*"
    "*claude-repl:claude-10*"
    "*claude-repl:claude-9*"
    "*claude-repl:claude-7*"
  )
fi

lisp_buffers=""
for buffer in "${BUFFERS[@]}"; do
  lisp_buffers="${lisp_buffers}\"${buffer}\" "
done

TMP_EL="$(mktemp /tmp/smart-cursor-e2e-cycle.XXXXXX.el)"
cleanup() {
  rm -f "$TMP_EL"
}
trap cleanup EXIT

cat >"$TMP_EL" <<EOF
(load-file "${ROOT}/emacs/smart-cursor.el")

(let* ((buffers '(${lisp_buffers}))
       (missing (seq-remove #'get-buffer buffers)))
  (when missing
    (error "Missing target buffers: %S" missing))
  (let ((reply nil)
        (steps nil))
    (dolist (buffer buffers)
      (let ((slug (replace-regexp-in-string "[^[:alnum:]]+" "-" buffer)))
        (setq steps
              (append steps
                      (list
                       \`((op . "switch-buffer") (buffer . ,buffer) (label . ,(concat "enter-" slug)))
                       \`((op . "read-word") (label . ,(concat slug "-here")))
                       \`((op . "backward-word") (count . 1) (label . ,(concat slug "-back")))
                       \`((op . "read-word") (label . ,(concat slug "-prev"))))))))
    (setq steps
          (append steps
                  (list '((op . "snapshot") (label . "final-snapshot")))))
    (cl-letf (((symbol-function 'smart-cursor--send-minibuffer-response)
               (lambda (payload)
                 (setq reply payload)
                 (setq smart-cursor--last-minibuffer payload))))
      (smart-cursor--execute-run-script
       \`((request-id . "external-e2e-cycle")
         (name . "external-e2e-cycle")
         (steps . ,steps))))
    (with-temp-file "${OUTFILE}"
      (prin1 reply (current-buffer)))
    (with-temp-file "${TRACE_OUTFILE}"
      (with-current-buffer "*smart-cursor-trace*"
        (insert
         (buffer-substring-no-properties
          (max (point-min) (- (point-max) 4000))
          (point-max)))))))
EOF

emacsclient --eval "(load-file \"$TMP_EL\")" >/dev/null
cat "$OUTFILE"
