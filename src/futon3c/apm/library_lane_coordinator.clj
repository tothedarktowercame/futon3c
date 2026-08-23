(ns futon3c.apm.library-lane-coordinator
  "Durable coordinator adapter for nonblocking library-lane steps."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.library-lane-adapters :as adapters]
            [futon3c.apm.library-lane-effects :as effects]
            [futon3c.apm.library-lane-launch :as launch]
            [futon3c.apm.library-lane-runner :as runner]))

(def adapter-key :apm/library-lane)
(def default-registry-path "data/apm-coordinators/registry.edn")

(defn run-step!
  [{:keys [agency-base corpus-root frames-root state-root problem-id
           trunk-branch keying-target contract-path]}]
  (let [eff (effects/live-effects {:agency-base agency-base
                                   :corpus-root corpus-root
                                   :frames-root frames-root})
        launched (launch/launch!
                  (merge eff {:corpus-root corpus-root :problem-id problem-id
                              :trunk-branch trunk-branch
                              :keying-target keying-target
                              :state-root state-root :agency-base agency-base}))]
    (if-not (:ok launched)
      launched
      (let [config (:config launched)]
        (runner/step-one!
         {:corpus-root corpus-root :problem-id problem-id
          :contract (edn/read-string (slurp contract-path))
          :seat (:seats config)
          :phase-inputs-fn (adapters/make-phase-inputs-fn config)
          :bank-request-fn (adapters/make-bank-request-fn config)})))))

(defn- next-intent [config state]
  (let [body {:coordinator/id (:coordinator-id config)
              :problem-id (:problem-id config)
              :prior-intent/digest
              (get-in state [:coordinator/last-settled-intent :intent/digest])
              :regulator/ticks (:regulator/ticks state)}]
    {:job-id (str "library-step-" (machine/ledger-digest [body]))
     :dispatch/id (machine/ledger-digest
                   [(assoc body :dispatch/type :library-lane-step)])
     :dispatch/action :library-lane/step
     :expected/postcondition
     {:ruling/one-of [:awaiting :partial-banked :closed]}}))

(defn adapter-constructor [config]
  {:decide-fn
   (fn [state]
     {:ok true :coordinator/action :activate
      :coordinator/intent (next-intent config state)})
   :reconcile-fn
   (fn [_ _]
     (let [result (run-step! config)]
       (case (:ruling result)
         :awaiting {:ok true :status :awaiting-job :lane/result result}
         :partial-banked {:ok true :status :library-increment-banked
                          :coordinator/clear-intent? true :lane/result result}
         :closed {:ok true :status :frame-complete
                  :coordinator/clear-intent? true :lane/result result}
         (if (:ok result)
           {:ok false :error/code :library-lane-ruling-invalid
            :finding result}
           result))))})

(coordinator/register-adapter! adapter-key adapter-constructor)

(defn start!
  [{:keys [registry-path state-path coordinator-id period-ms] :as options
    :or {registry-path default-registry-path period-ms 500}}]
  (let [config (dissoc options :registry-path :state-path :period-ms)
        registered (coordinator/register!
                    {:registry-path registry-path :coordinator-id coordinator-id
                     :adapter adapter-key :config config :state-path state-path
                     :period-ms period-ms})]
    (if (:ok registered)
      (coordinator/start-registered! registry-path coordinator-id)
      registered)))

(defn status [registry-path coordinator-id]
  (coordinator/status registry-path coordinator-id))

(defn stop! [registry-path coordinator-id]
  (coordinator/stop! registry-path coordinator-id))
