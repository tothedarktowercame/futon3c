(ns futon3c.apm.role-job-reconciliation
  "Standard live-job ports for an authenticated APM role job."
  (:require [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-job-driver :as driver]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.typed-role-submission :as submission]))

(defn drive!
  "Drive one role job through the shared exactly-once reconciliation path.

  Phase code supplies policy functions (prompt, preparation, validation,
  repair planning, and receipt construction); this function owns the common
  Agency, typed-submission, persistence, and cancellation ports."
  [{:keys [request state state-path persist-fn agency-base prompt-fn prepare-fn archive-packet-fn
           terminal-validator posthoc-fault-origin-fn terminal-repair-request-fn
           terminal-budget-config missing-observation-provider receipt-provider]
    :as inputs
    :or {agency-base "http://localhost:7070"
         prepare-fn (constantly {:ok true})
         archive-packet-fn (fn [_ _] {:ok true})}}]
  (let [state (if (contains? inputs :state)
                state
                (runtime/read-state state-path))
        persist-fn (or persist-fn #(runtime/atomic-persist! state-path %))]
    (driver/drive!
     {:request request
    :state state
    :announce-fn
    (fn [req]
      (let [req (submission/with-job-authority req)]
        (job-port/announce!
         agency-base
         {:agent-id (:agent-id req)
          :prompt (prompt-fn (assoc req :agency-base agency-base))
          :job-id (:submission/job-id req)})))
    :activate-fn
    (fn [req ticket]
      (let [prepared (prepare-fn req)
            reset-response (when (and (:ok prepared) (:fresh-session? req))
                             (runtime/http-json
                              "POST" (str agency-base "/api/alpha/agents/"
                                          (:agent-id req) "/reset-session") {}))
            reset-ok? (or (nil? reset-response)
                          (and (= 200 (:http/status reset-response))
                               (:ok reset-response)))]
        (cond
          (not (:ok prepared)) prepared
          (not reset-ok?)
          {:ok false :error/code :student-session-reset-failed}
          :else
          (let [packet (prompt-fn
                        (assoc (submission/with-job-authority req)
                               :agency-base agency-base))
                archived (archive-packet-fn req packet)
                _ (when-not (:ok archived)
                    (binding [*out* *err*]
                      (println "[apm.packet-archive]" (pr-str archived))))
                activated (job-port/activate!
                           agency-base
                           {:agent-id (:agent-id req)
                            :prompt packet
                            :job-id (:job-id ticket)})]
            (cond-> activated
              (not (:ok archived))
              (assoc :packet/archive-finding archived))))))
    :job-fn #(job-port/observe agency-base %)
    :cancel-fn #(job-port/cancel!
                 agency-base % "typed-submission wrapper reconciliation")
    :persist-fn persist-fn
    :ticket-register-fn submission/register!
    :terminal-submission-provider
    (fn [req ticket _]
      (submission/authenticated-completion req ticket))
    :terminal-validator terminal-validator
    :posthoc-fault-origin-fn posthoc-fault-origin-fn
    :terminal-repair-request-fn terminal-repair-request-fn
    :terminal-budget-config terminal-budget-config
    :missing-observation-provider missing-observation-provider
    :receipt-provider receipt-provider})))
