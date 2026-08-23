(ns futon3c.apm.library-lane-coordinator
  "Durable coordinator adapter for nonblocking library-lane steps."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.durable-coordinator :as coordinator]
            [futon3c.apm.library-lane-adapters :as adapters]
            [futon3c.apm.library-lane-effects :as effects]
            [futon3c.apm.library-lane-launch :as launch]
            [futon3c.apm.library-lane-runner :as runner]
            [futon3c.apm.live-preflight-runtime :as persistence])
  (:import [java.nio.file Path]))

(def adapter-key :apm/library-lane)
(def default-registry-path "data/apm-coordinators/registry.edn")
(def phase-rulings [:awaiting :phase-certified :partial-banked :closed])

(defn run-step!
  [{:keys [agency-base corpus-root frames-root state-root problem-id
           trunk-branch keying-target contract-path phase]}]
  (let [eff (effects/live-effects {:agency-base agency-base
                                   :corpus-root corpus-root
                                   :frames-root frames-root})
        observed (when-not (false? (:ok eff))
                   ((:observe-problem-fn eff)
                    {:corpus-root corpus-root :problem-id problem-id}))
        resumed (when (:ok observed)
                  (launch/resume-config
                   {:state-root state-root :problem-id problem-id
                    :revision (get-in observed [:problem :revision])
                    :outcome-fn (:outcome-fn eff)}))
        launched (cond
                   (false? (:ok eff)) eff
                   (not (:ok observed)) observed
                   resumed {:ok true :status :resumed :config resumed}
                   :else
                   (launch/launch!
                    (merge eff
                           {:corpus-root corpus-root :problem-id problem-id
                            :trunk-branch trunk-branch
                            :keying-target keying-target
                            :state-root state-root
                            :agency-base agency-base})))]
    (if-not (:ok launched)
      launched
      (let [config (:config launched)]
        (runner/step-one!
         {:corpus-root corpus-root :problem-id problem-id
          :contract (edn/read-string (slurp contract-path))
          :seat (:seats config)
          :phase-limit phase
          :phase-inputs-fn (adapters/make-phase-inputs-fn config)
          :bank-request-fn (adapters/make-bank-request-fn config)})))))

(defn- next-intent [config state]
  (let [body {:coordinator/id (:coordinator-id config)
              :problem-id (:problem-id config)
              :phase (or (:library/phase state) :preflight)
              :prior-intent/digest
              (get-in state [:coordinator/last-settled-intent :intent/digest])
              :regulator/ticks (:regulator/ticks state)}]
    {:job-id (str "library-step-" (machine/ledger-digest [body]))
     :dispatch/id (machine/ledger-digest
                   [(assoc body :dispatch/type :library-lane-step)])
     :dispatch/action :library-lane/step
     :dispatch/parameters {:phase (or (:library/phase state) :preflight)}
     :expected/postcondition {:ruling/one-of phase-rulings}}))

(defn adapter-constructor [config]
  {:decide-fn
   (fn [state]
     {:ok true :coordinator/action :activate
      :coordinator/intent (next-intent config state)})
   :reconcile-fn
   (fn [intent state]
     (let [phase (get-in intent [:dispatch/parameters :phase])
           current-phase (or (:library/phase state) :preflight)
           successor {:preflight :solve :solve :verify :verify :bank}]
       (if (not= phase current-phase)
         {:ok false :error/code :library-lane-phase-intent-drift
          :finding {:intent-phase phase :state-phase current-phase}}
         (let [result (run-step! (assoc config :phase phase))]
           (case (:ruling result)
         :awaiting {:ok true :status :awaiting-job :lane/result result}
         :phase-certified
         {:ok true :status :library-phase-certified
          :coordinator/clear-intent? true :lane/result result
          :regulator/state-updates {:library/phase (successor phase)}}
         :partial-banked {:ok true :status :library-increment-banked
                          :coordinator/clear-intent? true :lane/result result
                          :regulator/state-updates {:library/phase :preflight}}
         :closed {:ok true :status :frame-complete
                  :coordinator/clear-intent? true :lane/result result}
         (if (:ok result)
           {:ok false :error/code :library-lane-ruling-invalid
            :finding result}
             result))))))})

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

(defn migrate-pending-phase-intent!
  "Bind one legacy library intent to its already-persisted phase.

   This migration is deliberately narrow: it accepts only an otherwise valid
   library step intent with no dispatch parameters and preserves its job,
   dispatch, and pre-state identities."
  [registry-path coordinator-id]
  (let [{:keys [registration durable-state]}
        (coordinator/status registry-path coordinator-id)
        intent (:coordinator/pending-intent durable-state)
        phase (or (:library/phase durable-state) :preflight)]
    (cond
      (nil? registration)
      {:ok false :error/code :durable-coordinator-not-registered}
      (nil? intent)
      {:ok true :status :no-pending-intent}
      (not (coordinator/valid-intent? coordinator-id durable-state intent))
      {:ok false :error/code :durable-coordinator-intent-integrity-invalid
       :findings (coordinator/intent-findings coordinator-id durable-state intent)}
      (not= :library-lane/step (:dispatch/action intent))
      {:ok false :error/code :library-lane-intent-migration-action-invalid}
      (not (keyword? phase))
      {:ok false :error/code :library-lane-intent-migration-phase-invalid}
      (and (some? (:dispatch/parameters intent))
           (not= {:phase phase} (:dispatch/parameters intent)))
      {:ok false :error/code :library-lane-intent-migration-parameters-invalid}
      (and (= {:phase phase} (:dispatch/parameters intent))
           (= {:ruling/one-of phase-rulings}
              (:expected/postcondition intent)))
      {:ok true :status :already-migrated}
      :else
      (let [amended (assoc intent
                           :dispatch/parameters {:phase phase}
                           :expected/postcondition
                           {:ruling/one-of phase-rulings})
            amended (assoc amended :intent/digest
                           (coordinator/intent-digest amended))
            updated (-> durable-state
                        (assoc :coordinator/pending-intent amended)
                        (update :coordinator/intent-migrations (fnil conj [])
                                {:migration/type :library-phase-parameter
                                 :prior-intent/digest (:intent/digest intent)
                                 :amended-intent/digest (:intent/digest amended)
                                 :phase phase}))]
        (persistence/atomic-persist!
         (Path/of (:coordinator/state-path registration)
                  (make-array String 0))
         updated)))))

(defn resume! [registry-path coordinator-id]
  (coordinator/resume! registry-path coordinator-id))
