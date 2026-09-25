#!/usr/bin/env bash
# kimi-task.sh — clock a Kimi seat in on a UNIQUE task, so its conversation
# starts fresh (Joe, 2026-09-25: "Kimi drains should be solved by clocking
# them in on unique tasks, like E-kimi-task-1"). A Kimi seat clears its
# conversation only when the requisition target changes (zai_api.clj
# context-carry-decision), and the target must resolve to
# holes/excursions/<E-*>.md, so each task gets one small excursion file.
#
# A requisition is not pending work (Joe, 2026-09-25, futon2 a461b124): it is
# in progress from dispatch and completed after, so never eligible to be
# dispatched or flown again. The file says which, on the first line after the
# H1 (the target field's reader matches it there; do not vary the format):
#   **Requisition:** in-progress — dispatched <ISO-8601 UTC> to <seat> as <job-id>
#   **Requisition:** completed — <ISO-8601 UTC>, job <job-id>, state <done|failed>
#
# usage: kimi-task.sh --from <caller> --to <kimi-N> --purpose "<one line>" [--cascade <c.clj>] <packet.md> [more.md ...]
# Mints futon2/holes/excursions/E-kimi-task-N.md (next N) with the purpose,
# caller, date and packet text, sends the bell with `Requisition:
# E-kimi-task-N — <purpose>` prepended, writes the in-progress line with the
# job id, and commits that one path. If the bell returns no job id, the file
# is removed and nothing is committed: a dispatch that did not happen leaves
# no record that could read as pending work. Prints the task id and job id.
# With --cascade, the bell's body is that pattern cascade (agency_send.py
# --cascade: it must lint, and the receiver is told how to read one) and the
# packet files ride as the caller's context; the excursion file holds both.
#
# usage: kimi-task.sh --complete <E-kimi-task-N> <job-id>
# Reads the job's final state and time from GET /api/alpha/invoke/jobs/<id>
# and rewrites the line to completed, one commit. Refuses, changing nothing,
# when the file has no Requisition line, names a different job, or the job is
# not finished.
set -euo pipefail
REPO="${KIMI_TASK_REPO:-/home/joe/code/futon2}"; DIR=$REPO/holes/excursions
AGENCY_URL="${AGENCY_URL:-http://localhost:7070}"

# set_requisition FILE LINE — put LINE on the first line after the H1 (after
# its blank line), replacing an existing **Requisition:** line there.
set_requisition() {
  F="$1" L="$2" python3 - <<'PY'
import os
f, line = os.environ["F"], os.environ["L"]
rows = open(f).read().split("\n")
assert rows[0].startswith("# "), f"{f}: first line is not an H1"
body = rows[1:]
while body and body[0].strip() == "":
    body = body[1:]
if body and body[0].startswith("**Requisition:**"):
    body = body[1:]
    while body and body[0].strip() == "":
        body = body[1:]
open(f, "w").write("\n".join([rows[0], "", line, ""] + body))
PY
}

if [ "${1:-}" = "--complete" ]; then
  [ $# -eq 3 ] || { echo "usage: $0 --complete <E-kimi-task-N> <job-id>" >&2; exit 2; }
  ID="$2"; JOBID="$3"; FILE="$DIR/$ID.md"
  [ -f "$FILE" ] || { echo "refusal:no-such-task — $FILE" >&2; exit 1; }
  CUR=$(sed -n '2,4p' "$FILE" | grep -m1 '^\*\*Requisition:\*\* ' || true)
  [ -n "$CUR" ] || { echo "refusal:no-requisition-line — $FILE has no **Requisition:** line under its H1; nothing changed" >&2; exit 1; }
  case "$CUR" in
    "**Requisition:** completed — "*"job $JOBID,"*) echo "task=$ID already completed: $CUR"; exit 0;;
    *"as $JOBID") ;;
    *) echo "refusal:job-mismatch — $FILE names another job: $CUR; nothing changed" >&2; exit 1;;
  esac
  READ=$(curl -sf -m 30 "$AGENCY_URL/api/alpha/invoke/jobs/$JOBID" | python3 -c '
import json, sys
d = json.load(sys.stdin); j = d.get("job", d)
print(j.get("state") or "", j.get("finished-at") or "")') \
    || { echo "refusal:job-unreadable — GET /api/alpha/invoke/jobs/$JOBID failed; nothing changed" >&2; exit 1; }
  STATE=${READ%% *}; AT=${READ#* }
  case "$STATE" in
    done|failed) ;;
    *) echo "refusal:job-not-finished — $JOBID is '$STATE'; nothing changed" >&2; exit 1;;
  esac
  AT=$(date -u -d "$AT" +%Y-%m-%dT%H:%M:%SZ)
  set_requisition "$FILE" "**Requisition:** completed — $AT, job $JOBID, state $STATE"
  git -C "$REPO" commit -q -m "$ID: requisition completed ($STATE)

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -- "holes/excursions/$ID.md"
  echo "task=$ID job=$JOBID state=$STATE at=$AT"
  exit 0
fi

FROM=""; TO=""; PURPOSE=""; CASCADE=""
while [ $# -gt 0 ]; do
  case "$1" in
    --from) FROM="$2"; shift 2;;
    --to) TO="$2"; shift 2;;
    --purpose) PURPOSE="$2"; shift 2;;
    --cascade) CASCADE="$2"; shift 2;;
    *) break;;
  esac
done
[ -n "$FROM" ] && [ -n "$TO" ] && [ -n "$PURPOSE" ] && [ $# -ge 1 ] || { echo "usage: $0 --from <id> --to <kimi-N> --purpose <line> <packet.md>..." >&2; exit 2; }
for f in "$@"; do [ -f "$f" ] || { echo "no such packet file: $f" >&2; exit 2; }; done
if [ -n "$CASCADE" ]; then
  [ -f "$CASCADE" ] || { echo "no such cascade file: $CASCADE" >&2; exit 2; }
  # Refuse before minting: a cascade that does not lint would be refused by
  # agency_send after the excursion file exists.
  python3 /home/joe/code/futon3c/scripts/xlate.py lint "$CASCADE" | tail -1 | grep -q ' 0 problem(s)$' \
    || { echo "refusal:cascade-does-not-lint — $CASCADE" >&2; python3 /home/joe/code/futon3c/scripts/xlate.py lint "$CASCADE" >&2 || true; exit 2; }
fi
N=$(ls "$DIR" 2>/dev/null | sed -n 's/^E-kimi-task-\([0-9]*\)\.md$/\1/p' | sort -n | tail -1); N=$(( ${N:-0} + 1 ))
ID="E-kimi-task-$N"; FILE="$DIR/$ID.md"; TODAY=$(date -u +%Y-%m-%d)
{
  echo "# $ID — $PURPOSE"; echo
  echo "Clocked in by $FROM for $TO on $TODAY (one Kimi task, one excursion, so the seat's"
  echo "conversation starts fresh; see scripts/kimi-task.sh)."; echo
  echo "## Packet"; echo
  cat "$@"
  if [ -n "$CASCADE" ]; then
    echo; echo "## Cascade"; echo; echo '```clojure'; cat "$CASCADE"; echo '```'
  fi
} > "$FILE"
AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)
JOB=$( { echo "Requisition: $ID — $PURPOSE"; echo; cat "$@"; } | python3 /home/joe/code/futon3c/scripts/agency_send.py --from "$FROM" --to "$TO" --kind bell --mode work --requisition "$ID — $PURPOSE" ${CASCADE:+--cascade "$CASCADE"} 2>&1 | grep -oE 'invoke-[0-9]+-[0-9]+-[0-9a-f]+' | head -1 || true)
if [ -z "$JOB" ]; then
  rm -f "$FILE"
  echo "refusal:no-job — the bell to $TO returned no job id; $ID removed, nothing committed" >&2
  exit 1
fi
set_requisition "$FILE" "**Requisition:** in-progress — dispatched $AT to $TO as $JOB"
git -C "$REPO" add "holes/excursions/$ID.md"
git -C "$REPO" commit -q -m "$ID: $PURPOSE

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>" -- "holes/excursions/$ID.md"
echo "task=$ID file=$FILE job=$JOB"
