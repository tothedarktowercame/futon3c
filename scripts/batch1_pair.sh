#!/usr/bin/env bash
# Open both frames for one batch-1 problem and dispatch both arms.
# Usage: batch1_pair.sh <problem-id>
# Per batch-1-prereg.md: mem arm -> ams-codex-2 --memory-channel :push,
# ctl arm -> ams-codex-1 --memory-channel :none; bellback-only, no parks.
set -euo pipefail
PID="$1"
BATCH="batch-1"
F3C=/home/joe/code/futon3c
APM=/home/joe/code/apm-lean
HEAD=$(git -C "$APM" rev-parse HEAD)
LOG="$F3C/data/experiment-frames/$BATCH/dispatch-log.tsv"
mkdir -p "$(dirname "$LOG")"

# Shared packet body from the committed template, filled mechanically.
TEMPLATE=$(git -C "$F3C" show HEAD:data/codex-sorry-packet-template.txt 2>/dev/null \
  || git -C "$F3C" log --all --format=%H -- data/codex-sorry-packet-template.txt | head -1 | xargs -I{} git -C "$F3C" show {}:data/codex-sorry-packet-template.txt)
STATEMENT=$(awk "/^theorem apm_${PID,,}|^theorem apm_${PID}/,/:= by/" \
  "$APM/problems/$PID/lean/Main.lean" | head -20)
[ -n "$STATEMENT" ] || STATEMENT="(main theorem statement: see the target file; the frozen statement must not change)"
NSORRY=$(grep -cE "^\s*sorry\b" "$APM/problems/$PID/lean/Main.lean")

for ARM in mem ctl; do
  if [ "$ARM" = mem ]; then SEAT=ams-codex-2; CHAN=push; CHANFLAG=":push"; else SEAT=ams-codex-1; CHAN=none; CHANFLAG=":none"; fi
  FRAME="$BATCH-$PID-$ARM"
  CHECKOUT="/home/joe/code/apm-frames/$FRAME"
  bb "$F3C/scripts/frames.bb" open --problem "$PID" --arm "$ARM" \
     --base-rev "$HEAD" --seat "$SEAT" --memory-channel "$CHAN" \
     --recall-system v1.3-kind-instrumented --batch "$BATCH"
  PACKET=$(python3 - "$PID" "$CHECKOUT" "$NSORRY" <<PYEOF
import sys
pid, checkout, nsorry = sys.argv[1], sys.argv[2], sys.argv[3]
t = """$TEMPLATE"""
t = (t.replace('@@ID@@', f'{pid} [FRAME $FRAME]')
      .replace('@@KIND@@', 'sorry closure (batch-1 paired dispatch)')
      .replace('@@FILE@@', f'../apm-frames/$FRAME/problems/{pid}/lean/Main.lean')
      .replace('@@LINE@@', f'{nsorry} remaining sorry step(s); grep the file')
      .replace('@@STATEMENT@@', """$STATEMENT""")
      .replace('@@UNBLOCKS@@', 'none recorded for this row')
      .replace('@@AVAILABLE@@', 'GREP LEMMA-INDEX.md (/home/joe/code/apm-lean; 196 importable) and ConstructionTargets/ before re-deriving ANYTHING.')
      .replace('@@ROUTE@@', 'none recorded; read the boundary comments in the target file'))
t += f'''

FRAME CONTRACT ($FRAME — overrides rule 3 paths): work ONLY in
{checkout} (branch exp/{pid}-$ARM). Build:
  cd {checkout} && lake env lean problems/{pid}/lean/Main.lean
Commit on the frame branch only. Do NOT touch /home/joe/code/apm-lean.
'''
print(t)
PYEOF
)
  JOB=$(echo "$PACKET" | "$F3C/scripts/dispatch_with_recall.clj" \
    --problem "$PID" --to "$SEAT" --from ams-claude-1 \
    --memory-channel "$CHANFLAG" --limit 5 --recall-timeout-ms 60000)
  printf '%s\t%s\t%s\t%s\n' "$FRAME" "$ARM" "$SEAT" "$JOB" >> "$LOG"
  echo "DISPATCHED $FRAME -> $JOB ($SEAT, channel $CHAN)"
done
