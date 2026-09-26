(require '[futon3c.transport.invoke-ingress-integration-test :as f]
 '[futon3c.transport.http :as http]
 '[futon3c.agency.invoke-ingress-controller :as i]
 '[futon3c.agency.registry :as reg])
(let [c (#'f/durable-controller)
      release (promise) started (promise)
      worker (Thread. (fn [] (deliver started true) @release))]
 (.start worker) @started
 (try
  (#'f/with-http-fixture c
   (fn [_]
    (#'http/create-invoke-job! (#'f/invoke-request "still-running"))
    (#'http/mark-invoke-job-running! "still-running")
    (with-redefs-fn {#'http/parked-on-notify! (constantly {})
                    #'http/auto-bellback-enabled? (constantly false)
                    #'http/inbox-agent? (constantly false)
                    #'reg/get-agent (constantly {})}
     (fn [] (#'http/finalize-invoke-job! "still-running" "timeout" "job-ceiling-exceeded" nil {:ok false} nil)))
    (#'http/record-invoke-job-delivery-by-job-id! "still-running"
     {:surface "test" :destination "fixture" :delivered? true})
    (let [s (#'f/snapshot c)]
     (prn {:case :terminal-before-worker-exit :worker-alive? (.isAlive worker) :snapshot s})
     (assert (.isAlive worker))
     (assert (:drained? s)))))
  (finally (deliver release true) (.join worker) (i/release-controller! c))))
(shutdown-agents)
