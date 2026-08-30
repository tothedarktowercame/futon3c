#!/usr/bin/env bash
# Report futon1b heap growth, and only when it matters.
#
# The heap climbed 504 -> 1872 MB of 4096 across 2026-08-30 with just six
# old-generation collections, i.e. retention rather than load, while two
# substrate reads timed out. A slow leak is invisible until it is an outage,
# so watch the trend rather than waiting to meet it during a failure.
set -u
prev=0; peak=0
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
  # A large drop means a real old-gen collection or a restart: the leak
  # hypothesis is wrong or the node bounced. Either is worth knowing.
  if [ "$prev" -gt 0 ] && [ $(( prev - used )) -gt 400 ]; then
    echo "== futon1b heap DROPPED ${prev} -> ${used}MB $(date -u +%H:%M:%SZ) -- collection or restart; growth was not a pure leak"
    peak=$pct
  fi
  prev=$used
  sleep 600
done
