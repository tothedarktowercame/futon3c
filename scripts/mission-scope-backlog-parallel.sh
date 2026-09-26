#!/usr/bin/env bash
# Shard the REMAINING backlog (list minus every mission already logged ok)
# across N workers, each its own transient user unit + log.
#   scripts/mission-scope-backlog-parallel.sh N LIST [TAG]
# Logs: /tmp/scope-backlog-<TAG>-w<i>.log ; units scope-backlog-<TAG>-w<i>
set -euo pipefail
N="$1"; LIST="$2"; TAG="${3:-$(date -u +%Y%m%dT%H%M%SZ)}"
done_re=$(cat /tmp/scope-backlog-*.log 2>/dev/null | awk "/\\] [0-9]+\\/[0-9]+ ok /{print \$4}" | sort -u | tr "\n" "|" | sed "s/|\$//")
if [ -n "$done_re" ]; then
  cut -f1 "$LIST" | grep -v -E "^(${done_re})\$" > "/tmp/scope-backlog-remaining-$TAG.txt"
else
  cut -f1 "$LIST" > "/tmp/scope-backlog-remaining-$TAG.txt"
fi
total=$(wc -l < "/tmp/scope-backlog-remaining-$TAG.txt")
echo "remaining=$total workers=$N tag=$TAG"
for i in $(seq 0 $((N-1))); do
  awk -v n="$N" -v i="$i" "NR % n == i" "/tmp/scope-backlog-remaining-$TAG.txt" > "/tmp/scope-backlog-shard-$TAG-w$i.txt"
  systemd-run --user --unit="scope-backlog-$TAG-w$i" --collect \
    "${WORKER:-/home/joe/code/futon3c/scripts/mission-scope-backlog.sh}" \
    "/tmp/scope-backlog-shard-$TAG-w$i.txt" "/tmp/scope-backlog-$TAG-w$i.log"
done
