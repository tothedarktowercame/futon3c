#!/bin/bash
# Sample the apm-driver unit's cgroup + box load while it is active.
# Output: CSV to $1 (default /tmp/apm-profile.csv). Runs until the unit stops.
OUT="${1:-/tmp/apm-profile.csv}"
echo "ts,load1,unit_cpu_ns,unit_mem_bytes,lake_procs,lake_cpu_pct,lake_rss_mb" > "$OUT"
while systemctl --user is-active apm-driver >/dev/null 2>&1; do
  ts=$(date -u +%H:%M:%S)
  load1=$(cut -d" " -f1 /proc/loadavg)
  cpu=$(systemctl --user show apm-driver -p CPUUsageNSec --value 2>/dev/null)
  mem=$(systemctl --user show apm-driver -p MemoryCurrent --value 2>/dev/null)
  lake=$(ps -C lake,lean -o pcpu=,rss= 2>/dev/null | awk '{n++; c+=$1; r+=$2} END {printf "%d,%.0f,%.0f", n, c, r/1024}')
  echo "$ts,$load1,$cpu,$mem,${lake:-0,0,0}" >> "$OUT"
  sleep 15
done
