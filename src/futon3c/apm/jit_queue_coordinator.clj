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

(def substrate-wait-max-ms
  "Longest the queue may sit in :awaiting-substrate for one scheduled retry.

   The wait already carried :not-before-ms, but nothing bounded how far ahead
   that instant could be. `nat-int?` accepts any absolute time, and the
   watchdog reads :not-before-ms AS the wait's deadline -- so a wake far in
   the future is a wait the watchdog can never find late, because its deadline
   is never exceeded. That is an unbounded wait wearing a deadline's clothes.

   Bounded against the same injected clock `decide-fn` reads rather than
   against a fixed epoch, so it means the same thing under a test clock. A
   wake in the PAST needs no bound: it fires on the next tick, which is the
   correct behaviour after the coordinator has been stopped for a while."
  (* 30 60 1000))

(defn- wake-beyond-horizon?
  "True when WAKE-MS is unusable as a bounded wait: absent, not a time, or
   further ahead than one retry is allowed to push the queue."
  [wake-ms now-ms]
  (or (not (nat-int? wake-ms))
      (> wake-ms (+ now-ms substrate-wait-max-ms))))

(defn- unbounded-wait-decision
  "The typed refusal for a retry whose wake cannot bound the wait.

   Reuses the existing :jit-transport-retry-deadline-invalid code rather than
   minting one: fault-taxonomy routes an unrecognised tick failure to a frame
   park either way, and a new code for the same fault is the design defect
   this repair is trying to stop producing."
  [retry wake-ms now-ms]
  {:ok false
   :error/code :jit-transport-retry-deadline-invalid
   :finding {:retry/id (:retry/id retry)
             :not-before-ms wake-ms
             :now-ms now-ms
             :horizon-ms substrate-wait-max-ms}})

(defn adapter-constructor [config]
  {:decide-fn
   (fn [state]
     (let [retry (:coordinator/delayed-retry state)
           now-ms (long (*intent-now-fn*))
           wake-ms (:not-before-ms retry)]
       (cond
         (and retry (wake-beyond-horizon? wake-ms now-ms))
         (unbounded-wait-decision retry wake-ms now-ms)

         (and retry (< now-ms wake-ms))
         {:ok true :status :awaiting-substrate
          :retry/not-before-ms wake-ms
          ;; Absolute, so no reader has to reconstruct the expiry from a
          ;; duration and no reader can disagree about when this wait ends.
          :retry/expires-at-ms wake-ms}
         :else
         {:ok true :coordinator/action :activate
          :coordinator/intent (next-intent config state)
          :regulator/state-updates
          (when retry
            {:coordinator/delayed-retry nil
             :coordinator/last-woken-retry
             (assoc retry :woken-at-ms now-ms)})})))
   :reconcile-fn
   (fn [_intent _state]
     (let [step (requiring-resolve
                 'futon3c.apm.countdown-control/autonomous-problem-list-step!)
           result (step (assoc (:launch config)
                               :coordinator-registry-path
                               (or (:registry-path config)
                                   default-registry-path)
                               :coordinator-id (:coordinator-id config)))]
       (cond
         (not (:ok result)) result
         (= :transport-retry-scheduled (:status result))
         (let [not-before-ms (:retry/not-before-ms result)
               retry {:retry/id
                      (str "substrate-retry-"
                           (machine/ledger-digest
                            [(:queue-id config) not-before-ms
                             (:transport-retry/history result)]))
                      :kind :transport
                      :not-before-ms not-before-ms
                      :scheduled-at-ms (*intent-now-fn*)
                      :attempt (get-in result [:transport-retry :attempt])
                      :max-attempts
                      (get-in result [:transport-retry :max-attempts])
                      :history (:transport-retry/history result)}]
           (if (wake-beyond-horizon? not-before-ms (long (*intent-now-fn*)))
             (unbounded-wait-decision retry not-before-ms
                                      (long (*intent-now-fn*)))
             {:ok true :status :queue-tick-complete
              :coordinator/clear-intent? true :queue/result result
              :regulator/state-updates
              {:coordinator/delayed-retry retry}}))
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
                  :registry-path registry-path
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

(defn stop! [registry-path coordinator-id stop-cause]
  (coordinator/stop! registry-path coordinator-id stop-cause))

(defn status [registry-path coordinator-id]
  (coordinator/status registry-path coordinator-id))
