#!/usr/bin/env bash
# Restart futon3c-zone.service from OUTSIDE its own cgroup.
#
# Why this exists: an agent turn runs as a grandchild of the fdev JVM and lives
# inside futon3c-zone.service's cgroup, so `systemctl --user restart` sends
# SIGTERM to the agent too — killing the only observer of whether the service
# came back. This script is launched via `systemd-run --user`, which places it
# in app.slice, so it survives and leaves a receipt.
#
#   systemd-run --user --unit=fdev-restart --collect \
#     /home/joe/code/futon3c/scripts/restart-fdev-detached.sh
#
# Then read /tmp/fdev-restart-receipt.txt once Agency answers again.
set -uo pipefail
R=/tmp/fdev-restart-receipt.txt
: > "$R"
say(){ echo "$(date -u +%H:%M:%SZ) $*" >> "$R"; }

say "cgroup=$(cat /proc/self/cgroup)"
say "PRE-CHECK: turn_queue.clj must define queue-view or the build will fail"
if grep -q 'defn queue-view' /home/joe/code/futon3c/src/futon3c/agency/turn_queue.clj 2>/dev/null; then
  say "  queue-view PRESENT in working tree - safe to proceed"
else
  say "  *** queue-view MISSING - ABORTING, transport/http.clj will not compile ***"
  say "RESULT=aborted"; exit 3
fi

say "restarting futon3c-zone.service"
systemctl --user restart futon3c-zone.service
say "  systemctl returned $?"

for i in $(seq 1 150); do
  if curl -sf -o /dev/null --max-time 3 http://127.0.0.1:7070/api/alpha/agents; then
    say "AGENCY UP after $((i*2))s"; say "RESULT=ok"; exit 0
  fi
  sleep 2
done
say "AGENCY DID NOT RETURN within 300s"
say "  journalctl --user -u futon3c-zone -n 40 --no-pager:"
journalctl --user -u futon3c-zone -n 40 --no-pager >> "$R" 2>&1
say "RESULT=failed"; exit 1
