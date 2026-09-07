#!/usr/bin/env bash
# Shunt long-running loops onto a resumable siding, so the JVM can be restarted
# without losing work.  Joe, 2026-09-07: "maybe we would be able to take that
# work and kind of shunt it off into a resumable sideline. Like you would at a
# train yard ... what we don't have is a way to shunt them off and restart."
#
#   shunt.sh park [reason]   quiesce everything and write a resume manifest
#   shunt.sh resume          bring it all back from the manifest
#   shunt.sh status          what is parked, and what is running now
#
# What is actually at risk in a JVM restart, and what is not:
#
#   topology-build-loop.sh  an OS process, but it DISPATCHES to the Agency in
#                           the JVM. It traps TERM ("in-flight job retained for
#                           restart recovery") and its ledger is worklist.edn on
#                           disk, so a signalled stop loses nothing.
#   jit-all-open-v3         lives INSIDE the JVM. Its coordinator, queue and
#                           frame ledgers are all on disk; only the scheduler
#                           and its in-flight role jobs are volatile.
#   wm-build-loop.sh        drives `claude -p` directly and never touches the
#                           Agency -- but that does NOT make it safe. Anything
#                           launched from a shell descended from the service
#                           lands in the futon3c-zone.service cgroup, and
#                           systemctl restart kills the whole cgroup. On
#                           2026-09-07 it was left unparked on exactly that
#                           reasoning and was killed mid-iteration anyway,
#                           losing an F11 work turn. Independence from the
#                           Agency is not independence from the cgroup.
#
# The JVM restart itself is deliberately NOT automated here. Park, restart by
# hand, resume: the destructive step stays something a person types.
set -uo pipefail
FUTON3C=/home/joe/code/futon3c
TOPO_LAB=/home/joe/code/apm-lean/holes/labs/topology-contract
REGISTRY="$FUTON3C/data/apm-coordinators/registry.edn"
V3=jit-queue:jit-all-open-v3
YARD="$FUTON3C/data/shunt"
MANIFEST="$YARD/manifest.edn"
AGENCY=http://127.0.0.1:7070
mkdir -p "$YARD"

log() { printf '[shunt] %s\n' "$*"; }

topo_pid() { pgrep -f "topology-build-loop.sh" 2>/dev/null | head -1; }
wm_pid()   { pgrep -f "wm-build-loop.sh" 2>/dev/null | head -1; }
WM_LAB=/home/joe/code/futon2/holes/labs/wm-contract

wm_env() {
  local pid="$1" out=""
  for k in MAX_ITER WORK_SEAT REVIEW_SEAT SLEEP; do
    local v; v=$(tr '\0' '\n' < "/proc/$pid/environ" 2>/dev/null | grep "^$k=" | head -1)
    [ -n "$v" ] && out="$out $v"
  done
  echo "${out# }"
}

# Reconstruct the loop's launch environment from /proc so resume is faithful
# rather than a guess at the flags someone used hours ago.
topo_env() {
  local pid="$1" out=""
  for k in MAX_ITER PROBE_SEAT STRATEGY_SEAT AUTHOR_SEAT REVIEW_SEAT OWNER_SEAT; do
    local v
    v=$(tr '\0' '\n' < "/proc/$pid/environ" 2>/dev/null | grep "^$k=" | head -1)
    [ -n "$v" ] && out="$out $v"
  done
  echo "${out# }"
}

drain_jobs() {
  local budget="${1:-180}" waited=0 n
  while [ "$waited" -lt "$budget" ]; do
    n=$(curl -s --max-time 10 "$AGENCY/api/alpha/invoke/jobs?limit=40" 2>/dev/null \
        | python3 -c "
import json,sys
try: d=json.load(sys.stdin)
except Exception: print(0); raise SystemExit
js=d.get('jobs',d)
print(sum(1 for j in (js if isinstance(js,list) else []) if j.get('state') in ('running','queued')))" 2>/dev/null || echo 0)
    [ "${n:-0}" = 0 ] && { log "agency drained"; return 0; }
    log "waiting for $n in-flight job(s) ... ${waited}s/${budget}s"
    sleep 15; waited=$((waited+15))
  done
  log "WARNING: $n job(s) still in flight after ${budget}s; they will be lost"
  return 1
}

case "${1:-status}" in

park)
  reason="${2:-operator park}"
  log "parking: $reason"
  tp=$(topo_pid); tenv=""
  if [ -n "$tp" ]; then
    tenv=$(topo_env "$tp")
    log "topology loop pid=$tp env=[$tenv]"
  else
    log "topology loop not running"
  fi

  wp=$(wm_pid); wenv=""
  if [ -n "$wp" ]; then wenv=$(wm_env "$wp"); log "wm loop pid=$wp env=[$wenv]";
  else log "wm loop not running"; fi

  # Let in-flight work finish BEFORE signalling, so a row is not abandoned
  # mid-review.  This is the whole point of a siding.
  drain_jobs 180

  if [ -n "$tp" ]; then
    log "signalling topology loop (TERM: it retains its in-flight row)"
    kill -TERM "$tp" 2>/dev/null
    for _ in $(seq 1 20); do kill -0 "$tp" 2>/dev/null || break; sleep 1; done
    kill -0 "$tp" 2>/dev/null && log "WARNING: topology loop still alive" \
                              || log "topology loop stopped"
  fi

  if [ -n "$wp" ]; then
    log "signalling wm loop (it dies with the cgroup either way; stop it cleanly)"
    kill -TERM "$wp" 2>/dev/null
    for _ in $(seq 1 20); do kill -0 "$wp" 2>/dev/null || break; sleep 1; done
    kill -0 "$wp" 2>/dev/null && log "WARNING: wm loop still alive" || log "wm loop stopped"
  fi

  log "stopping the v3 coordinator through its lifecycle API"
  ( cd "$FUTON3C" && timeout 300 clojure -M:dev-admin eval \
    "(let [stop! (ns-resolve (find-ns (quote futon3c.apm.durable-coordinator)) (quote stop!))
           r (stop! \"$REGISTRY\" \"$V3\"
                    ;; schema per durable-coordinator/valid-stop-cause?
                    {:stop-cause/type :operator
                     :stop-cause/reason-code :shunt-park})]
       {:ok (:ok r) :status (:status r) :error (:error/code r)})" 2>&1 | tail -1 )

  cat > "$MANIFEST" <<EOF
{:state/type :shunt-manifest
 :parked-at "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
 :reason "$reason"
 :topology {:was-running $([ -n "$tp" ] && echo true || echo false)
            :lab "$TOPO_LAB"
            :env "$tenv"}
 :apm {:coordinator-id "$V3" :registry "$REGISTRY"}
 :wm {:was-running $([ -n "$wp" ] && echo true || echo false)
      :lab "$WM_LAB"
      :env "$wenv"}}
EOF
  log "manifest written: $MANIFEST"
  log "SAFE TO RESTART THE JVM NOW. Then: $0 resume"
  ;;

resume)
  [ -f "$MANIFEST" ] || { log "no manifest at $MANIFEST"; exit 1; }
  log "resuming from $(bb -e "(:parked-at (read-string (slurp \"$MANIFEST\")))" 2>/dev/null)"
  # bb prints a string pr-style, WITH its quotes. Interpolating that straight
  # into a Clojure string literal produced "shunt resume after: "reason"" and a
  # reader error at resume time, which is the worst moment to discover it.
  reason=$(bb -e "(print (:reason (read-string (slurp \"$MANIFEST\"))))" 2>/dev/null | tr -d '"' | tr ';' ',')
  [ -n "$reason" ] || reason="operator park"

  log "resuming the v3 coordinator"
  ( cd "$FUTON3C" && timeout 300 clojure -M:dev-admin eval \
    "(let [resume! (ns-resolve (find-ns (quote futon3c.apm.durable-coordinator)) (quote resume!))
           r (resume! \"$REGISTRY\" \"$V3\" \"shunt resume after: $reason\")]
       {:ok (:ok r) :status (:status r) :error (:error/code r)})" 2>&1 | tail -1 )

  if [ "$(bb -e "(get-in (read-string (slurp \"$MANIFEST\")) [:topology :was-running])" 2>/dev/null)" = "true" ]; then
    tenv=$(bb -e "(get-in (read-string (slurp \"$MANIFEST\")) [:topology :env])" 2>/dev/null | tr -d '"')
    # A job cut off by the restart cannot ever emit its nonce-bound receipt, so
    # the loop's reclaim path STOPs on it immediately ("emitted no unique
    # nonce-bound receipt") and the resume looks like a failed launch. Set the
    # orphan aside -- keeping it as evidence -- so the row is re-dispatched
    # instead. Safe because a row only leaves :open when a receipt is applied:
    # no receipt means the ledger never advanced.
    if [ -f "$TOPO_LAB/runs/inflight.edn" ]; then
      mkdir -p "$TOPO_LAB/runs/orphaned"
      mv "$TOPO_LAB/runs/inflight.edn" \
         "$TOPO_LAB/runs/orphaned/inflight-shunt-$(date -u +%Y%m%dT%H%M%SZ).edn"
      log "orphaned in-flight row set aside; it will be re-dispatched"
    fi
    log "relaunching topology loop with [$tenv]"
    ( cd "$TOPO_LAB" && setsid env $tenv nohup ./topology-build-loop.sh \
        > /tmp/topo-loop-resumed.log 2>&1 < /dev/null & disown )
    sleep 20
    pgrep -f "topology-build-loop.sh" >/dev/null && log "topology loop running" \
                                                 || log "WARNING: topology loop did not start"
  fi
  if [ "$(bb -e "(get-in (read-string (slurp \"$MANIFEST\")) [:wm :was-running])" 2>/dev/null)" = "true" ]; then
    wenv=$(bb -e "(print (get-in (read-string (slurp \"$MANIFEST\")) [:wm :env]))" 2>/dev/null | tr -d '"')
    log "relaunching wm loop with [$wenv]"
    ( cd "$WM_LAB" && setsid env $wenv nohup ./wm-build-loop.sh \
        > /tmp/wm-loop-resumed.log 2>&1 < /dev/null & disown )
    sleep 20
    pgrep -f "wm-build-loop.sh" >/dev/null && log "wm loop running" \
                                           || log "WARNING: wm loop did not start"
  fi
  mv "$MANIFEST" "$YARD/manifest-$(date -u +%Y%m%dT%H%M%SZ).edn"
  log "resumed; manifest archived"
  ;;

status)
  if [ -f "$MANIFEST" ]; then log "PARKED:"; cat "$MANIFEST" | sed 's/^/    /';
  else log "nothing parked"; fi
  log "topology loop: $(topo_pid >/dev/null && echo running || echo stopped)"
  log "wm loop:       $(wm_pid >/dev/null && echo running || echo stopped)"
  ;;

*) echo "usage: $0 park [reason] | resume | status"; exit 2 ;;
esac
