#!/usr/bin/env bash
# Report futon1b heap growth, and only when it matters.
#
# The heap climbed 504 -> 1872 MB of 4096 across 2026-08-30 with just six
# old-generation collections, i.e. retention rather than load, while two
# substrate reads timed out. A slow leak is invisible until it is an outage,
# so watch the trend rather than waiting to meet it during a failure.
set -u
prev=0; peak=0; floor=0
while true; do
  h=$(timeout 20 curl -s localhost:7073/health 2>/dev/null)
  if [ -z "$h" ]; then
    echo "!! futon1b unreachable $(date -u +%H:%M:%SZ)"
    sleep 300; continue
  fi
  used=$(printf '%s' "$h" | grep -o ':used-mb [0-9]*' | head -1 | grep -o '[0-9]*')
  max=$(printf '%s' "$h" | grep -o ':max-mb [0-9]*' | head -1 | grep -o '[0-9]*')
  [ -z "${used:-}" ] && { sleep 300; continue; }
  pct=$(( used * 100 / ${max:-4096} ))
  # Alarm bands, each announced once as it is crossed.
  if [ "$pct" -ge 85 ] && [ "$peak" -lt 85 ]; then
    echo "!! futon1b heap ${used}/${max}MB (${pct}%) $(date -u +%H:%M:%SZ) -- OOM risk, my notes record a prior futon1b OOM at a full heap"
  elif [ "$pct" -ge 70 ] && [ "$peak" -lt 70 ]; then
    echo "!! futon1b heap ${used}/${max}MB (${pct}%) $(date -u +%H:%M:%SZ) -- crossed 70%"
  elif [ "$pct" -ge 55 ] && [ "$peak" -lt 55 ]; then
    echo "== futon1b heap ${used}/${max}MB (${pct}%) $(date -u +%H:%M:%SZ) -- crossed 55%"
  fi
  [ "$pct" -gt "$peak" ] && peak=$pct
  # A large drop is a collection. The FIRST one falsified the leak reading and
  # was worth saying; the next three said the same thing at 1099, 1157, 1098 MB.
  # What matters is not that it collects but where it collects TO: a stable
  # floor is a healthy sawtooth, a rising floor is the leak. So track the floor
  # and speak only when it climbs.
  if [ "$prev" -gt 0 ] && [ $(( prev - used )) -gt 400 ]; then
    if [ "$floor" -eq 0 ]; then
      echo "== futon1b heap collects: ${prev} -> ${used}MB $(date -u +%H:%M:%SZ); floor noted, silent unless it rises"
      floor=$used
    elif [ $(( used - floor )) -gt 500 ]; then
      echo "!! futon1b heap FLOOR ROSE ${floor} -> ${used}MB $(date -u +%H:%M:%SZ) -- collections no longer reclaiming; this is what a leak looks like"
      floor=$used
    fi
    peak=$pct
  fi
  prev=$used
  sleep 600
done
