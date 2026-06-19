#!/usr/bin/env bash
# demo-ready.sh — M-smart-emacs-cursor VERIFY row 7: a cold Emacs reaches
# demo-ready state from this single entry point.
#
# Idempotent. Loads the demo organs into the operator's Emacs daemon,
# registers the observer identity with the Agency, connects the shared WS,
# and reports every precondition of the unbroken scene (A7).
#
# Usage: bash scripts/demo-ready.sh [emacs-socket]   (default: server)
set -uo pipefail

SOCKET="${1:-server}"
BASE="http://localhost:7070"
F3C=/home/joe/code/futon3c

# 1. Observer identity (409 duplicate-registration is success).
reg=$(curl -s -X POST "$BASE/api/alpha/agents" -H "Content-Type: application/json" \
  -d '{"agent-id":"emacs-hud","type":"mock","capabilities":[]}')
case "$reg" in
  *'"ok":true'*|*duplicate*) echo "[demo-ready] observer: emacs-hud registered" ;;
  *) echo "[demo-ready] observer registration UNEXPECTED: $reg" ;;
esac

# 2. Load organs + connect, inside the operator's Emacs.
status=$(emacsclient -s "$SOCKET" --eval '(progn
  (load "'"$F3C"'/emacs/futon-agency-ws.el" nil t)
  (load "'"$F3C"'/emacs/futon-agent-cursor.el" nil t)
  (load "'"$F3C"'/emacs/mission-mode.el" nil t)
  (futon-agency-hud-enable)
  (sit-for 1)
  (format "ws-open=%s ready=%s posframe=%s mission-mode=%s agent-cursor=%s"
          (and futon-agency-ws--ws (websocket-openp futon-agency-ws--ws) t)
          futon-agency-ws--ready
          (and (require (quote posframe) nil t) t)
          (fboundp (quote mission-mode))
          (fboundp (quote futon-agent-cursor-type))))' 2>&1)
echo "[demo-ready] emacs: $status"

# 3. Voice route state (reported, not forced — the operator owns the toggle).
if [[ -e /tmp/futon-voice-route.on ]]; then
  echo "[demo-ready] voice route: ON (inbox /tmp/futon-voice-inbox.jsonl)"
else
  echo "[demo-ready] voice route: off (touch /tmp/futon-voice-route.on to route)"
fi
if pgrep -f "enhanced-voice-typing" >/dev/null; then
  echo "[demo-ready] voice process: running"
else
  echo "[demo-ready] voice process: NOT running (M-x my-chatgpt-shell-voice-toggle)"
fi

# 4. Agency roster sanity.
count=$(curl -s "$BASE/api/alpha/agents" | python3 -c "import json,sys; print(json.load(sys.stdin)['count'])" 2>/dev/null)
echo "[demo-ready] agency: $count agents on the roster"
echo "[demo-ready] scene preconditions reported above — speak when ready."
