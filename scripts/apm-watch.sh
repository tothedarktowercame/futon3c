#!/usr/bin/env bash
# Stream APM campaign state changes as one line per change.
#
# Watches the most recently touched campaign: frame/phase transitions, verdict
# tallies, regulator status, and stalls. A stall is only worth reporting when
# the coordinator is actually meant to be ticking -- a coordinator an operator
# has switched off has flat ticks and a durable :running status, which reads
# exactly like a hang. apm-coordinator-enabled.py is what tells them apart.
set -u
cd /home/joe/code/futon3c

prev=""; lasttk=""; lastrs=""; lasten=""; stall=0

while true; do
  # Watch the most recently touched campaign that the coordinator registry
  # actually knows about. Plain "most recently touched" follows fixture and
  # smoke-test directories -- ftriangle-live-smoke-v1 hijacked this watch at
  # 19:35 -- and those have no regulator, so every signal below reads as zero.
  C=""
  for candidate in $(ls -dt data/apm-campaigns/*/); do
    state=$(python3 scripts/apm-coordinator-enabled.py "$(basename "$candidate")" 2>/dev/null)
    if [ "$state" != "unknown" ]; then C="$candidate"; break; fi
  done
  [ -z "$C" ] && C=$(ls -dt data/apm-campaigns/*/ | head -1)
  campaign=$(basename "$C")

  cur=$(timeout 240 python3 scripts/apm-frame-pulse.py 2>&1); rc=$?
  key=$(echo "$cur" \
        | grep -E "^campaign|^frame|^    [a-z-]+ +\{|supply:|^    a[0-9]:|GATE" \
        | sed -E 's/worktree Main\.lean [0-9]+ lines \([^)]*\)//; s/round [0-9]+, [0-9]+ left//' \
        | tr -s ' ')

  if [ "$key" != "$prev" ]; then
    if [ -n "$prev" ]; then
      echo "=== APM change $(date -u +%H:%M:%SZ) [$campaign] ==="
      diff <(echo "$prev") <(echo "$key") | grep '^>' | sed 's/^> /  /'
    fi
    prev="$key"
  fi

  [ $rc -ne 0 ] && echo "!! PULSE GATE FAILED rc=$rc $(date -u +%H:%M:%SZ)"
  echo "$cur" | grep -q spinning && echo "!! SOLVER MAY BE SPINNING $(date -u +%H:%M:%SZ)"

  qs=$(grep -o ':status :[a-z-]*' "$C/queue-state.edn" 2>/dev/null | head -1 | awk '{print $2}')
  tk=$(grep -o ':regulator/ticks [0-9]*' "$C/coordinator.edn" 2>/dev/null | grep -o '[0-9]*')
  rs=$(grep -o ':regulator/status :[a-z-]*' "$C/coordinator.edn" 2>/dev/null | head -1 | awk '{print $2}')
  en=$(python3 scripts/apm-coordinator-enabled.py "$campaign" 2>/dev/null)

  if [ "$rs" != "$lastrs" ]; then
    case "$rs" in
      :failed*)
        code=$(grep -o ':error/code :[a-z-]*' "$C/coordinator.edn" | head -1 | awk '{print $2}')
        echo "!! REGULATOR -> :failed $(date -u +%H:%M:%SZ): $code" ;;
      :running*)
        [ -n "$lastrs" ] && echo "== regulator -> running $(date -u +%H:%M:%SZ)" ;;
    esac
    lastrs="$rs"
  fi

  # An operator switching the coordinator on or off is the single most
  # important thing to say out loud: every other signal is read differently
  # depending on it.
  if [ "$en" != "$lasten" ]; then
    case "$en" in
      disabled) [ -n "$lasten" ] && echo "== COORDINATOR DISABLED $(date -u +%H:%M:%SZ) [$campaign] -- machine stopped, flat ticks are expected" ;;
      enabled)  [ -n "$lasten" ] && echo "== COORDINATOR RE-ENABLED $(date -u +%H:%M:%SZ) [$campaign]" ;;
    esac
    lasten="$en"
    stall=0
  fi

  # An ABSENT tick counter is not a frozen one: "" = "" compares equal and
  # would report a stall forever against a directory that never ticks.
  if [ -n "$tk" ] && [ "$qs" != ":paused" ] && [ "$qs" != ":pause-after-active" ] \
     && [ "$rs" != ":failed" ] && [ "$en" != "disabled" ]; then
    if [ "$tk" = "$lasttk" ]; then
      stall=$((stall+1))
      if [ $stall -eq 10 ]; then
        echo "!! TICKS FROZEN at $tk ~10min $(date -u +%H:%M:%SZ)"
        stall=0
      fi
    else
      stall=0
    fi
  else
    stall=0
  fi
  lasttk="$tk"

  sleep 60
done
