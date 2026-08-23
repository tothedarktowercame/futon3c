#!/usr/bin/env bash
# library-lane-run.sh — one library-lane attempt, in its own JVM.
# Launch it durably: scripts/bg.py launch "…/library-lane-run.sh" --label lane-<problem>
set -euo pipefail
export PATH="$HOME/.elan/bin:$PATH"
cd /home/joe/code/futon3c
exec clojure -Sdeps "{:paths [\"src\" \"resources\" \"library\" \"scripts\"]}" -M -m library-lane-run
