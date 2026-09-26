#!/usr/bin/env bash
# Count EXECUTABLE sorries — the only sanctioned way to do it in this lane.
#
# WHY THIS EXISTS. Ground control miscounted sorries with grep three times on
# 2026-07-30 (a00J01 3-vs-2, a01A06 5-vs-4, a03J08 1-vs-0), each time reporting
# a runner as wrong when the runner was right. `grep -c '\bsorry\b'` counts
# prose mentions in comments and docstrings; `grep -c '^\s*sorry\b'` misses
# inline `:= by sorry`. Both were logged in the register as ⊸fix and both
# recurred, because a note is not a mechanism. This wrapper is the mechanism.
#
# It delegates to stack-hud--apm-count-sorries, which is comment-, string- and
# block-comment-aware, and is the same counter behind the operator's HUD — so
# these numbers agree with what Joe sees by construction.
#
# Usage:
#   count_sorries.sh <dir-or-file> [more...]     # per-target counts + total
#   count_sorries.sh --problem a01A02            # a problem's lean/ directory
#   count_sorries.sh --corpus                    # every problems/*/lean, HUD totals
set -euo pipefail

HUD=/home/joe/code/futon0/contrib
APM=/home/joe/code/apm-lean

run_elisp() { emacs -Q --batch --eval "$1" 2>/dev/null | grep -v '^$'; }

case "${1:-}" in
  --corpus)
    run_elisp "(progn (add-to-list 'load-path \"$HUD\") (load \"stack-hud.el\" nil t)
      (setq stack-hud--apm-cache nil)
      (let ((s (stack-hud--apm-scan)))
        (princ (format \"problems=%s informal=%s with-lean=%s with-sorry=%s CLEAN=%s EXECUTABLE-SORRIES=%s\\n\"
          (plist-get s :total) (plist-get s :informal) (plist-get s :lean-total)
          (plist-get s :lean-with-sorry) (plist-get s :lean-clean) (plist-get s :sorries)))))"
    ;;
  --problem)
    [ $# -ge 2 ] || { echo "usage: $0 --problem <id>" >&2; exit 2; }
    run_elisp "(progn (add-to-list 'load-path \"$HUD\") (load \"stack-hud.el\" nil t)
      (princ (format \"%s\\n\" (stack-hud--apm-count-sorries \"$APM/problems/$2/lean\"))))"
    ;;
  ""|-h|--help)
    sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//' ; exit 0 ;;
  *)
    total=0
    for t in "$@"; do
      if [ -d "$t" ]; then
        n=$(run_elisp "(progn (add-to-list 'load-path \"$HUD\") (load \"stack-hud.el\" nil t)
              (princ (format \"%s\\n\" (stack-hud--apm-count-sorries \"$t\"))))")
      else
        d=$(mktemp -d); cp "$t" "$d/"
        n=$(run_elisp "(progn (add-to-list 'load-path \"$HUD\") (load \"stack-hud.el\" nil t)
              (princ (format \"%s\\n\" (stack-hud--apm-count-sorries \"$d\"))))")
        rm -rf "$d"
      fi
      printf '%6s  %s\n' "$n" "$t"
      total=$((total + n))
    done
    [ $# -gt 1 ] && printf '%6s  TOTAL\n' "$total"
    ;;
esac
