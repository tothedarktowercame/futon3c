#!/usr/bin/env bash
# operator-round.sh — one operator round, over two Kimi seats (Joe, 2026-09-25).
# Seat A assembles: it translates Joe's turn into a pattern cascade. Seat B
# applies: it carries the cascade out in the code, with Joe's words beside
# it, and cites a join and pattern as the warrant for every change. Both go
# through kimi-task.sh, so each is a fresh seat on its own requisition.
#
# usage: operator-round.sh translate --from <id> --to <kimi-N> <turn-id>
#        operator-round.sh apply     --from <id> --to <kimi-N> <turn-id> <translation.md>
# The round's files are under storage/operator-turns/rounds/<turn-id>/.
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
TURNS=/home/joe/.emacs-graph/session-turn-analysis
XL=/home/joe/code/storage/operator-turns/translations
ROUNDS=/home/joe/code/storage/operator-turns/rounds
STEP="${1:-}"; shift || true
FROM=""; TO=""
while [ $# -gt 0 ]; do
  case "$1" in
    --from) FROM="$2"; shift 2;;
    --to) TO="$2"; shift 2;;
    *) break;;
  esac
done
TURN="${1:-}"
[ -n "$FROM" ] && [ -n "$TO" ] && [ -n "$TURN" ] || { sed -n 9,10p "$0" >&2; exit 2; }
TURN_JSON="$TURNS/$TURN.json"
[ -f "$TURN_JSON" ] || { echo "refusal:no-such-turn — $TURN_JSON" >&2; exit 2; }
DIR="$ROUNDS/$TURN"; mkdir -p "$DIR"

case "$STEP" in
  translate)
    OUT="$XL/$TURN.$TO.md"
    sed -e "s|TURN_JSON|$TURN_JSON|g" -e "s|OUT_MD|$OUT|g" "$HERE/seat-a-translate.md" > "$DIR/seat-a-packet.md"
    R=$(bash "$HERE/../kimi-task.sh" --from "$FROM" --to "$TO" \
          --purpose "operator round $TURN: seat A, translate the turn into a cascade" "$DIR/seat-a-packet.md")
    ;;
  apply)
    SRC="${2:-}"; [ -f "$SRC" ] || { echo "refusal:no-translation — $SRC" >&2; exit 2; }
    # The cascade is the first clojure block of the translation.
    python3 - "$SRC" "$DIR/cascade.clj" <<'PY'
import re, sys
m = re.search(r"```(?:clojure|lisp)?\n(\(cascade.*?)```", open(sys.argv[1]).read(), re.S)
if not m:
    sys.exit(f"refusal:no-cascade-block — {sys.argv[1]}")
open(sys.argv[2], "w").write(m.group(1).rstrip() + "\n")
PY
    python3 "$HERE/../xlate.py" lint "$DIR/cascade.clj" --turn "$TURN_JSON" | tee "$DIR/cascade.lint" | tail -1
    grep -q ' 0 problem(s)$' "$DIR/cascade.lint" || { echo "refusal:cascade-does-not-lint — $DIR/cascade.lint" >&2; exit 1; }
    { sed -e "s|TURN_ID|$TURN|g" "$HERE/seat-b-apply.md"
      echo; echo "## Joe's turn $TURN, as spoken"; echo; echo '```text'
      python3 -c 'import json,sys;print(json.load(open(sys.argv[1]))["source_text"])' "$TURN_JSON"
      echo '```'; } > "$DIR/seat-b-packet.md"
    R=$(bash "$HERE/../kimi-task.sh" --from "$FROM" --to "$TO" --cascade "$DIR/cascade.clj" \
          --purpose "operator round $TURN: seat B, carry out the cascade with warrants" "$DIR/seat-b-packet.md")
    ;;
  *) sed -n 9,10p "$0" >&2; exit 2;;
esac
echo "$R"
echo "$(date -u +%FT%TZ) $STEP $TO $R" >> "$DIR/log.txt"
