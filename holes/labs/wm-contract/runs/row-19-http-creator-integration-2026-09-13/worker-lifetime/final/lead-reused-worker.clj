(require '[futon3c.transport.invoke-ingress-integration-test :as f] '[futon3c.transport.http :as http] '[futon3c.agency.invoke-ingress-controller :as i] '[futon3c.agency.registry :as reg])
(let [c (#'f/durable-controller) removals (atom [])]
 (try (#'f/with-http-fixture c (fn [{:keys [ledger]}]
  (#'http/create-invoke-job! (#'f/invoke-request "owned-running"))
  (#'http/mark-invoke-job-running! "owned-running")
  (with-redefs-fn {#'http/parked-on-notify! (constantly {}) #'http/auto-bellback-enabled? (constantly false) #'http/inbox-agent? (constantly false) #'reg/get-agent (constantly {}) #'reg/mark-agent-idle! (constantly nil) #'http/unregister-job-worker! (fn [id] (swap! removals conj id))}
   (fn [] (let [r (#'http/run-invoke-job! {:job-id "owned-running" :agent-id "worker-1"}) state (get-in @ledger [:jobs "owned-running" :state])]
    (prn {:duplicate-result r :original-state state :unregister-calls @removals})
    (assert (= "failed" state)) (assert (= ["owned-running"] @removals)))))))
  (finally (i/release-controller! c))))
(shutdown-agents)
