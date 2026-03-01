#!/usr/bin/env bash
# Agency diagnostic — run on the server, paste output to Claude.
# Usage: bash scripts/agency-diagnostic.sh [drawbridge-url] [token]

URL="${1:-http://localhost:6768/eval}"

# Resolve token: arg > env > .admintoken file
if [ -n "${2:-}" ]; then
  TOKEN="$2"
elif [ -n "${FUTON3C_ADMIN_TOKEN:-}" ]; then
  TOKEN="$FUTON3C_ADMIN_TOKEN"
elif [ -n "${ADMIN_TOKEN:-}" ]; then
  TOKEN="$ADMIN_TOKEN"
elif [ -f .admintoken ]; then
  TOKEN="$(cat .admintoken | tr -d '[:space:]')"
else
  TOKEN="change-me"
fi

eval_clj() {
  curl -s -X POST "$URL" -H "x-admin-token: $TOKEN" -d @- <<CLOJURE
$1
CLOJURE
}

echo "=== Agency Diagnostic ==="
echo "  at: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "  drawbridge: $URL"
echo

echo "--- Registry ---"
eval_clj '(futon3c.agency.registry/registry-status)'
echo
echo

echo "--- WS Invoke Slots ---"
eval_clj '(do (require (quote futon3c.transport.ws.invoke))
              (let [reg @futon3c.agency.registry/!registry
                    ids (keys reg)]
                (into {} (map (fn [id] [id {:ws-available? (futon3c.transport.ws.invoke/available? id)}]) ids))))'
echo
echo

echo "--- WS Connections (transport) ---"
eval_clj '(do (require (quote futon3c.dev))
              (when-let [sys @futon3c.dev/!f3c-sys]
                (when-let [conns (:ws-connections sys)]
                  (into {} (map (fn [[ch c]] [(:agent-id c) (select-keys c [:connected? :agent-id])]) @conns)))))'
echo
echo

echo "--- IRC Server ---"
eval_clj '(when-let [sys @futon3c.dev/!irc-sys]
            {:port (:port sys)
             :has-relay-bridge (some? (:relay-bridge sys))
             :has-server (some? (:server sys))})'
echo
echo

echo "--- Evidence (last 5 invoke entries) ---"
eval_clj '(do (require (quote futon3c.evidence.store))
              (let [store @futon3c.dev/!evidence-store
                    entries (futon3c.evidence.store/query* store {:tag "invoke"})
                    recent (take-last 5 entries)]
                (mapv (fn [e] {:id (:evidence/id e)
                               :at (:evidence/at e)
                               :tags (:evidence/tags e)
                               :body-event (get-in e [:evidence/body "event"])
                               :body-agent (get-in e [:evidence/body "agent-id"])})
                      recent)))'
echo
echo

echo "--- Federation ---"
eval_clj '(do (require (quote futon3c.agency.federation))
              @futon3c.agency.federation/!config)'
echo
echo

echo "--- Process Check ---"
echo "claude -p processes:"
ps aux | grep '[c]laude.*-p' | head -5 || echo "  (none)"
echo "codex exec processes:"
ps aux | grep '[c]odex.*exec' | head -5 || echo "  (none)"
echo

echo "=== Done ==="
