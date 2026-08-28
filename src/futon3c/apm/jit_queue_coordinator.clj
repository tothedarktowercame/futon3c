(ns futon3c.apm.jit-queue-coordinator
  "Durable coordinator adapter for the JIT problem queue."
  (:require [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.phase-status :as phase-status]))

(def adapter-key :apm/jit-problem-queue)
(def default-registry-path "data/apm-coordinators/registry.edn")
(def default-tick-work-timeout-minutes 30)
(def ^:dynamic *intent-now-fn* #(System/currentTimeMillis))

(defn- minutes->ms [minutes]
  (* minutes 60 1000))

(defn- next-intent [config state]
  (let [tick-work-timeout-minutes
        (or (:tick-work-timeout-minutes config)
            default-tick-work-timeout-minutes)
        tick-work-timeout-ms (minutes->ms tick-work-timeout-minutes)
        body {:coordinator/id (:coordinator-id config)
              :queue/name (:queue-name config)
              :queue/id (:queue-id config)
              :prior-intent/digest
              (get-in state [:coordinator/last-settled-intent :intent/digest])
              :regulator/ticks (:regulator/ticks state)}]
    {:job-id (str "jit-tick-" (machine/ledger-digest [body]))
     :dispatch/id (machine/ledger-digest
                   [(assoc body :dispatch/type :jit-problem-queue-tick)])
     :dispatch/action :jit-problem-queue/tick
     :dispatch/parameters
     {:deadline-ms (+ (*intent-now-fn*) tick-work-timeout-ms)
      :permitted-duration-ms tick-work-timeout-ms
      :permitted-duration-source :coordinator/tick-work-timeout-minutes}
     :expected/postcondition
     {:status/one-of (vec (sort (phase-status/known-statuses
                                 :jit-queue-postcondition)))}}))

(defn adapter-constructor [config]
  {:decide-fn
   (fn [state]
     {:ok true :coordinator/action :activate
      :coordinator/intent (next-intent config state)})
   :reconcile-fn
   (fn [_intent _state]
     (let [step (requiring-resolve
                 'futon3c.apm.countdown-control/autonomous-problem-list-step!)
           result (step (:launch config))]
       (cond
         (not (:ok result)) result
         (contains? #{:batch-complete :batch-paused} (:status result))
         {:ok true :status :frame-complete
          :coordinator/clear-intent? true :queue/result result}
         :else
         {:ok true :status :queue-tick-complete
          :coordinator/clear-intent? true :queue/result result})))})

(coordinator/register-adapter! adapter-key adapter-constructor)

(defn start!
  "Register and start one JIT queue from EDN-serializable launch authority."
  [{:keys [registry-path state-path coordinator-id launch period-ms
           tick-work-timeout-minutes]
    :or {registry-path default-registry-path period-ms 500
         tick-work-timeout-minutes default-tick-work-timeout-minutes}}]
  (if-not (pos-int? tick-work-timeout-minutes)
    {:ok false :error/code :jit-tick-work-timeout-invalid
     :tick-work-timeout-minutes tick-work-timeout-minutes}
    (let [config {:coordinator-id coordinator-id
                  :queue-name (:queue-name launch) :queue-id (:queue-id launch)
                  :launch launch
                  :tick-work-timeout-minutes tick-work-timeout-minutes}
        registered (coordinator/register!
                    {:registry-path registry-path :coordinator-id coordinator-id
                     :adapter adapter-key :config config :state-path state-path
                     :period-ms period-ms})]
      (if (:ok registered)
        (coordinator/start-registered! registry-path coordinator-id)
        registered))))

(defn recover!
  ([] (recover! default-registry-path))
  ([registry-path] (coordinator/recover-all! registry-path)))

(defn stop! [registry-path coordinator-id]
  (coordinator/stop! registry-path coordinator-id))

(defn status [registry-path coordinator-id]
  (coordinator/status registry-path coordinator-id))
