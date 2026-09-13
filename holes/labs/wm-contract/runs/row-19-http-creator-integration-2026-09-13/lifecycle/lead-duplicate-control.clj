(require '[futon3c.transport.invoke-ingress-integration-test :as f]
 '[futon3c.transport.http :as http]
 '[futon3c.agency.invoke-ingress-controller :as i])
(let [c (#'f/durable-controller)]
 (try
  (#'f/with-http-fixture c
   (fn [_]
    (#'http/create-invoke-job! (#'f/invoke-request "duplicate-running"))
    (#'http/mark-invoke-job-running! "duplicate-running")
    (#'http/create-invoke-job! (#'f/invoke-request "duplicate-running"))
    (let [s (#'f/snapshot c)]
     (prn {:case :duplicate-create-running :snapshot s})
     (assert (= 1 (:executing s)))
     (assert (= 1 (:accepted-queued s))))))
  (finally (i/release-controller! c))))
(shutdown-agents)
