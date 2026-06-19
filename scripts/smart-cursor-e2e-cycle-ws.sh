#!/usr/bin/env bash
# smart-cursor-e2e-cycle-ws.sh — orthogonal in-system version of
# smart-cursor-e2e-cycle.sh.
#
# Registers a synthetic WS sender for a fresh agent-id directly in the
# Agency's WS invoke registry (`futon3c.transport.ws.invoke`), then
# dispatches a run-script peripheral_event addressed to that agent and
# captures the frame on receipt. No live smart-cursor / Emacs connection
# is needed — this isolates the Agency-side dispatch path.
#
# Captures timings:
#   :server-sent-at-ms   (Clojure, right before send-peripheral-event!)
#   :received-at-ms      (Clojure, inside the synthetic sender fn)
#   :dispatch-elapsed-ms (Clojure, t1 - t0 around send-peripheral-event!)
#   :frame-bytes         (size of serialized JSON frame)
#
# Requires futon3c.transport.http to be loaded with the :server-sent-at-ms
# instrumentation (only relevant for the agent-text dispatch path; this
# harness sets the stamp itself).

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [[ $# -gt 0 ]]; then
  BUFFERS=("$@")
else
  BUFFERS=(
    "*codex-repl:codex-8*"
    "*claude-repl:claude-1*"
    "*claude-repl:claude-10*"
    "*claude-repl:claude-9*"
    "*claude-repl:claude-7*"
  )
fi

AGENT_ID="${SMART_CURSOR_E2E_AGENT_ID:-claude-cycle-driver-$$-$(date +%s)}"

build_steps_clj() {
  local first=1
  printf '['
  for buffer in "${BUFFERS[@]}"; do
    local slug
    slug="$(printf '%s' "$buffer" | sed 's/[^A-Za-z0-9]\+/-/g')"
    if (( first )); then first=0; else printf ' '; fi
    printf '{:op "switch-buffer" :buffer "%s" :label "enter-%s"} ' "$buffer" "$slug"
    printf '{:op "read-word"     :label "%s-here"} '   "$slug"
    printf '{:op "backward-word" :count 1 :label "%s-back"} '  "$slug"
    printf '{:op "read-word"     :label "%s-prev"}'  "$slug"
  done
  printf ' {:op "snapshot" :label "final-snapshot"}]'
}

STEPS_CLJ="$(build_steps_clj)"

EVAL_FORM=$(cat <<EOF
(do
  (require '[futon3c.transport.ws.invoke :as wsi])
  (require '[futon3c.transport.peripheral-events :as pe])
  (let [agent-id "${AGENT_ID}"
        captured (atom [])
        send-fn  (fn [json-str]
                   (swap! captured conj
                          {:received-at-ms (System/currentTimeMillis)
                           :frame-bytes    (count json-str)}))]
    (wsi/register! agent-id send-fn)
    (try
      (let [steps   ${STEPS_CLJ}
            payload {:command           "run-script"
                     :request-id        "orthogonal-ws-e2e"
                     :name              "orthogonal-ws-e2e"
                     :steps             steps
                     :server-sent-at-ms (System/currentTimeMillis)}
            t0      (System/currentTimeMillis)
            sent?   (pe/send-peripheral-event! agent-id :emacs-cursor :minibuffer payload)
            t1      (System/currentTimeMillis)]
        {:agent-id            agent-id
         :sent?               sent?
         :dispatch-elapsed-ms (- t1 t0)
         :server-sent-at-ms   (:server-sent-at-ms payload)
         :step-count          (count steps)
         :captured            (mapv (fn [c]
                                      (assoc c :delay-from-dispatch-ms
                                             (- (:received-at-ms c) t0)))
                                    @captured)})
      (finally
        (wsi/unregister! agent-id)))))
EOF
)

bash "${ROOT}/scripts/proof-eval.sh" "$EVAL_FORM"
echo
