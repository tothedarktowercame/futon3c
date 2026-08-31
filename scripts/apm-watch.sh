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

prev=""; lasttk=""; lastrs=""; lasten=""; lastcampaign=""; stall=0

lastfrozen=""
while true; do
  # Watch the most recently touched campaign that the coordinator registry
  # actually knows about. Plain "most recently touched" follows fixture and
  # smoke-test directories -- ftriangle-live-smoke-v1 hijacked this watch at
  # 19:35 -- and those have no regulator, so every signal below reads as zero.
  # Registry membership alone is not enough: jit-all-open-nontopology-v1 is
  # registered, enabled, and FINISHED (:complete, last touched 26h ago). At
  # 21:55 a stray write made its directory newest and the watch moved to it,
  # reporting yesterday's f44 as current. Skip terminal coordinators too.
  C=""
  for candidate in $(ls -dt data/apm-campaigns/*/); do
    state=$(python3 scripts/apm-coordinator-enabled.py "$(basename "$candidate")" 2>/dev/null)
    [ "$state" = "unknown" ] && continue
    rstat=$(grep -o ':regulator/status :[a-z-]*' "$candidate/coordinator.edn" 2>/dev/null | head -1 | awk '{print $2}')
    case "$rstat" in :complete*) continue;; esac
    C="$candidate"; break
  done
  # If nothing passed the filter, KEEP WATCHING THE LAST CAMPAIGN. The old
  # fallback took the newest directory regardless, which defeated the filter
  # entirely: when jit-all-open-v2 read momentarily "unknown" (its registry
  # entry mid-rewrite) and nontopology-v1 was skipped as :complete, the
  # fallback selected the very campaign the filter had just rejected, and the
  # watch oscillated between them reporting a finished f44 as current.
  if [ -z "$C" ]; then
    if [ -n "$lastcampaign" ]; then
      C="data/apm-campaigns/$lastcampaign/"
    else
      C=$(ls -dt data/apm-campaigns/*/ | head -1)
    fi
  fi
  campaign=$(basename "$C")

  # Enable/disable and tick deltas are only meaningful WITHIN one campaign.
  # Comparing them across a switch reported "COORDINATOR RE-ENABLED" when
  # nothing had been re-enabled -- the watch had simply changed subject.
  if [ "$campaign" != "$lastcampaign" ]; then
    [ -n "$lastcampaign" ] && echo "== WATCHING $campaign $(date -u +%H:%M:%SZ) (was $lastcampaign)"
    prev=""; lasttk=""; lastrs=""; lasten=""; stall=0; lastfrozen=""
    lastcampaign="$campaign"
  fi

  # Pass the campaign explicitly. The pulse takes it POSITIONALLY and
  # otherwise picks the newest directory itself, so an unqualified call
  # reports one campaign under another's name -- at 20:29 the header said
  # jit-all-open-v2 while the body described ftriangle-live-smoke-v1.
  cur=$(timeout 240 python3 scripts/apm-frame-pulse.py "${C%/}" 2>&1); rc=$?
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
  # ONE number, not every match. :regulator/quiescence-history carries a
  # :regulator/ticks for each past witness, so an unfiltered grep returned 87
  # values and printed "TICKS FROZEN at 3980\n4551\n..." as one alarm. Ticks
  # are monotonic and the history holds only past values, so the current count
  # is the maximum.
  tk=$(grep -o ':regulator/ticks [0-9]*' "$C/coordinator.edn" 2>/dev/null \
       | grep -o '[0-9]*' | sort -n | tail -1)
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
  # Ticks advancing is not progress. On 2026-08-30/31 f65 sat at promote-solver
  # for NINE HOURS through two transport repairs while the regulator ticked
  # ~10000 times with no error, and this watch called that healthy because it
  # only ever asked whether the tick counter moved. Watch the FRAME instead:
  # the newest phase file under the active frame's live/ directory. Solver
  # rounds and student attempts legitimately take tens of minutes, so only
  # speak after 90 minutes of no phase write while the coordinator is enabled.
  # Take the ACTIVE frame, not the first :frame/id in the file. head -1 read a
  # completed frame and reported f46 as stalled for 5909 minutes, four days
  # after it closed and banked. queue-state.edn holds :active, :parked and
  # :completed, and their order is not guaranteed.
  # Anchoring to :active is not enough. BETWEEN frames :active is nil, so a
  # forward search runs past it into :completed and returns its first entry --
  # which is f46, reported as stalled for 6311 minutes at 17:37 while f67 was
  # closing and f68 minting. Only accept a :frame/id inside the active MAP.
  afr=$(python3 -c "
import re,sys
try: t=open('$C/queue-state.edn').read()
except OSError: sys.exit()
i=t.find(':active')
if i<0: sys.exit()
rest=t[i+len(':active'):].lstrip()
if not rest.startswith('{'): sys.exit()   # :active nil -- no active frame
depth=0
for j,ch in enumerate(rest):
    if ch=='{': depth+=1
    elif ch=='}':
        depth-=1
        if depth==0: break
m=re.search(r':frame/id \"([^\"]+)\"', rest[:j+1])
print(m.group(1) if m else '')" 2>/dev/null)
  if [ -n "$afr" ] && [ "$en" = "enabled" ]; then
    newest=$(ls -t "$C"/*"$afr"/live/*.edn 2>/dev/null | head -1)
    if [ -n "$newest" ]; then
      age=$(( $(date +%s) - $(stat -c %Y "$newest" 2>/dev/null || echo 0) ))
      if [ "$age" -ge 5400 ]; then
        if [ "$lastfrozen" != "$afr" ]; then
          echo "!! FRAME $afr HAS NOT ADVANCED in $(( age / 60 ))min $(date -u +%H:%M:%SZ) -- newest phase $(basename "$newest" .edn); ticks may still be moving"
          lastfrozen="$afr"
        fi
      else
        lastfrozen=""
      fi
    fi
  fi

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
