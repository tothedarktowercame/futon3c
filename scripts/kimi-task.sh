#!/usr/bin/env bash
# kimi-task.sh — clock a Kimi seat in on a UNIQUE task, so its conversation
# starts fresh (Joe, 2026-09-25: "Kimi drains should be solved by clocking
# them in on unique tasks, like E-kimi-task-1"). A Kimi seat clears its
# conversation only when the requisition target changes (zai_api.clj
# context-carry-decision), and the target must resolve to
# holes/excursions/<E-*>.md, so each task gets one small excursion file.
#
# usage: kimi-task.sh --from <caller> --to <kimi-N> --purpose "<one line>" <packet.md> [more.md ...]
# Mints futon2/holes/excursions/E-kimi-task-N.md (next N), writes the purpose,
# caller, date, and the packet text into it, commits that one path, then sends
# the bell with `Requisition: E-kimi-task-N — <purpose>` prepended. Prints the
# task id and the job id.
set -euo pipefail
FROM=""; TO=""; PURPOSE=""
while [ $# -gt 0 ]; do
  case "$1" in
    --from) FROM="$2"; shift 2;;
    --to) TO="$2"; shift 2;;
    --purpose) PURPOSE="$2"; shift 2;;
    *) break;;
  esac
done
[ -n "$FROM" ] && [ -n "$TO" ] && [ -n "$PURPOSE" ] && [ $# -ge 1 ] || { echo "usage: $0 --from <id> --to <kimi-N> --purpose <line> <packet.md>..." >&2; exit 2; }
for f in "$@"; do [ -f "$f" ] || { echo "no such packet file: $f" >&2; exit 2; }; done
REPO=/home/joe/code/futon2; DIR=$REPO/holes/excursions
N=$(ls "$DIR" 2>/dev/null | sed -n 's/^E-kimi-task-\([0-9]*\)\.md$/\1/p' | sort -n | tail -1); N=$(( ${N:-0} + 1 ))
ID="E-kimi-task-$N"; FILE="$DIR/$ID.md"; TODAY=$(date -u +%Y-%m-%d)
{
  echo "# $ID — $PURPOSE"; echo
  echo "Clocked in by $FROM for $TO on $TODAY (one Kimi task, one excursion, so the seat's"
  echo "conversation starts fresh; see scripts/kimi-task.sh)."; echo
  echo "## Packet"; echo
  cat "$@"
} > "$FILE"
git -C "$REPO" add "holes/excursions/$ID.md"
git -C "$REPO" commit -q -m "$ID: $PURPOSE

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>" -- "holes/excursions/$ID.md"
JOB=$( { echo "Requisition: $ID — $PURPOSE"; echo; cat "$@"; } | python3 /home/joe/code/futon3c/scripts/agency_send.py --from "$FROM" --to "$TO" --kind bell --mode work --requisition "$ID — $PURPOSE" 2>&1 | grep -oE 'invoke-[0-9]+-[0-9]+-[0-9a-f]+' | head -1)
echo "task=$ID file=$FILE job=$JOB"
