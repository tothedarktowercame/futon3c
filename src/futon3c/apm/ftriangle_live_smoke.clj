(ns futon3c.apm.ftriangle-live-smoke
  "Isolated pre-go-live smoke frame. Preflight is safe to run against the
  shared JVM; live execution is effect-injected and never targets production
  campaign state implicitly."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.countdown-control :as countdown-control]
            [futon3c.apm.generated-contract :as generated-contract]
            [futon3c.apm.memory-access-gate :as memory-gate]
            [futon3c.apm.semantic-progress-watchdog :as watchdog]))

(def campaign-id "ftriangle-live-smoke-v1")
(def frame-id "ft1")
(def coordinator-id "ftriangle-live-smoke-v1")
(def ledger-root "data/apm-campaigns/ftriangle-live-smoke-v1")
(def fixture-path "test/resources/apm-regressions/ftriangle-smoke-v1.edn")

(def condition-order
  [:historical-ledger-valid :priors-non-empty :loaded-runtime-current
   :watchdog-armed :trace-assembler-accepted :same-problem-shelf-fixture])

(def traversal-order
  [:preflight-admission :dispatch-terminal :holdout-exclusion
   :watchdog-progress :forced-repair-durability :combined-trace-closure])

(defn read-fixture [] (edn/read-string (slurp fixture-path)))

(defn- loaded-runtime-current []
  (let [contract (generated-contract/read-contract
                  campaign-trace/default-contract-path)
        validated (when (:ok contract)
                    (generated-contract/validate (:contract contract)))
        required
        [['futon3c.apm.generated-contract 'validate]
         ['futon3c.apm.campaign-trace 'issue-combined-trace-receipt!]
         ['futon3c.apm.campaign-machine 'projection]
         ['futon3c.apm.countdown-control 'campaign-prior-memories]]
        missing (keep (fn [[namespace-symbol var-symbol]]
                        (when-not (ns-resolve namespace-symbol var-symbol)
                          (str namespace-symbol "/" var-symbol)))
                      required)]
    (if (and (:ok validated) (empty? missing))
      {:ok true :contract/id (get-in validated [:contract :contract/id])}
      {:ok false :error/code :ftriangle-loaded-runtime-stale
       :missing-vars (vec missing) :contract-validation validated})))

(defn default-checks
  ([] (default-checks {}))
  ([{:keys [historical-ledger-path priors-queue-path smoke-root]
     :or {historical-ledger-path
          "data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f28/ledger.edn"
          priors-queue-path
          "data/apm-campaigns/jit-all-open-nontopology-v1/queue-state.edn"
          smoke-root ledger-root}}]
   (let [fixture (read-fixture)]
     {:historical-ledger-valid
      (fn []
        (let [result (ledger/read-ledger historical-ledger-path)
              status (get-in result [:projection :projection/status])]
          {:ok (and (:ok result) (= :valid status))
           :projection/status status :path historical-ledger-path}))
      :priors-non-empty
      (fn []
        (let [result (countdown-control/campaign-prior-memories
                      priors-queue-path)]
          {:ok (boolean (and (:ok result) (seq (:candidates result))))
           :candidate-count (count (:candidates result))
           :dropped-count (count (:dropped result))}))
      :loaded-runtime-current loaded-runtime-current
      :watchdog-armed
      (fn [] {:ok (boolean (watchdog/running? coordinator-id))
              :watchdog/id coordinator-id})
      :trace-assembler-accepted
      (fn []
        (campaign-trace/issue-combined-trace-receipt!
         {:certificate {:receipt/id "ftriangle-synthetic-preflight"}
          :durable-documents (:synthetic-durable-documents fixture)
          :trace-path (io/file smoke-root "preflight/synthetic-trace.json")}))
      :same-problem-shelf-fixture
      (fn []
        (let [authority {:problem-id (:problem/id fixture)
                         :shelf/holdout :same-problem}
              decision (memory-gate/enforce-carrier
                        :shelf-materialization authority (:shelf fixture))]
          {:ok (boolean (and (seq (:shelf fixture))
                             (seq (:excluded decision))))
           :excluded (:excluded decision)
           :decision-evidence (:evidence decision)}))})))

(defn preflight
  "Run every condition without short-circuiting, then refuse with the exact
  unmet condition names. CHECKS is injectable for deterministic tests."
  ([] (preflight (default-checks)))
  ([checks]
   (let [results (into {}
                       (map (fn [condition]
                              [condition
                               (if-let [check (get checks condition)]
                                 (try (check)
                                      (catch Throwable error
                                        {:ok false
                                         :error/code :ftriangle-check-threw
                                         :message (.getMessage error)}))
                                 {:ok false
                                  :error/code :ftriangle-check-missing})]))
                       condition-order)
         unmet (into [] (keep (fn [condition]
                                (when-not (true? (get-in results [condition :ok]))
                                  condition)))
                     condition-order)]
     (if (seq unmet)
       {:ok false :error/code :ftriangle-preconditions-unmet
        :unmet unmet :conditions results}
       {:ok true :status :ready :conditions results}))))

(defn classify-failure [stage result]
  (let [substrate? (or (= :transport (:error/component result))
                       (contains? #{:hyperedge-unreachable :job-port-budget-exhausted
                                    :dispatch-timeout :terminal-timeout}
                                  (:error/code result)))]
    {:ok false :status :failed :stage stage
     :failure/class (if substrate? :substrate :apparatus)
     :failure/action (if substrate? :retry :block-go-live)
     :finding result}))

(defn execute!
  "Execute an isolated live smoke frame through explicitly supplied effects.
  Each effect receives accumulated evidence and must return {:ok true
  :evidence ...}. No default can dispatch or mutate a production campaign."
  [{:keys [checks effects persist-ledger-fn]}]
  (let [ready (preflight (or checks (default-checks)))]
    (if-not (:ok ready)
      ready
      (loop [remaining traversal-order evidence {:preflight-admission ready}]
        (if-let [stage (first remaining)]
          (if (= :preflight-admission stage)
            (recur (rest remaining) evidence)
            (if-let [effect (get effects stage)]
              (let [result (effect evidence)]
                (if (:ok result)
                  (recur (rest remaining)
                         (assoc evidence stage (:evidence result)))
                  (classify-failure stage result)))
              (classify-failure stage
                                {:error/code :ftriangle-live-effect-missing})))
          (let [missing (into [] (remove #(contains? evidence %)) traversal-order)
                closure (:combined-trace-closure evidence)
                valid-close? (campaign-trace/valid-combined-trace-receipt?
                              (:certificate closure))]
            (cond
              (seq missing)
              (classify-failure :ledger-evidence
                                {:error/code :ftriangle-ledger-evidence-missing
                                 :missing missing})
              (not valid-close?)
              (classify-failure :combined-trace-closure
                                {:error/code :ftriangle-closure-receipt-invalid})
              (not (fn? persist-ledger-fn))
              (classify-failure :ledger-evidence
                                {:error/code :ftriangle-ledger-persist-port-missing})
              :else
              (let [persisted (persist-ledger-fn
                               {:campaign/id campaign-id :frame/id frame-id
                                :traversal/evidence evidence})]
                (if (:ok persisted)
                  {:ok true :status :closed :campaign/id campaign-id
                   :frame/id frame-id :evidence evidence
                   :persisted persisted}
                  (classify-failure :ledger-evidence persisted))))))))))
