(ns futon3c.apm.ftriangle-live-smoke
  "Isolated pre-go-live smoke frame. Preflight is safe to run against the
  shared JVM; live execution is effect-injected and never targets production
  campaign state implicitly."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.countdown-control :as countdown-control]
            [futon3c.apm.countdown-manifest :as countdown-manifest]
            [futon3c.apm.countdown-pre-admission :as admission]
            [futon3c.apm.durable-coordinator :as durable-coordinator]
            [futon3c.apm.generated-contract :as generated-contract]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-job-driver :as job-driver]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.live-regulator :as regulator]
            [futon3c.apm.memory-access-gate :as memory-gate]
            [futon3c.apm.semantic-progress-watchdog :as watchdog])
  (:import [java.nio.file Path]))

(def campaign-id "ftriangle-live-smoke-v1")
(def frame-id "ft1")
(def coordinator-id "ftriangle-live-smoke-v1")
(def watchdog-id (str "semantic-progress:" coordinator-id))
(def ledger-root "data/apm-campaigns/ftriangle-live-smoke-v1")
(def fixture-path "test/resources/apm-regressions/ftriangle-smoke-v1.edn")
(def registry-path "data/apm-coordinators/registry.edn")
(def coordinator-state-path
  "data/apm-campaigns/ftriangle-live-smoke-v1/coordinator.edn")

(def condition-order
  [:historical-ledger-valid :priors-non-empty :loaded-runtime-current
   :watchdog-armed :trace-assembler-accepted :same-problem-shelf-fixture])

(def traversal-order
  [:preflight-admission :dispatch-terminal :holdout-exclusion
   :watchdog-progress :forced-repair-durability :combined-trace-closure])

(defn read-fixture [] (edn/read-string (slurp fixture-path)))

(defn read-live-config
  "Read the isolated authorities named by CONFIG. Paths are data, not an
  executable reader form, so the committed config remains plain EDN."
  [config]
  (cond-> config
    (:manifest-path config)
    (assoc :manifest (edn/read-string (slurp (:manifest-path config))))
    (:contract-path config)
    (assoc :contract (edn/read-string (slurp (:contract-path config))))))

(defn arm-isolated-coordinator!
  "Register and start only F△'s coordinator. This deliberately never calls
  recover-all!, whose scope includes production coordinators."
  ([] (arm-isolated-coordinator! {}))
  ([{:keys [registry state-path period-ms]
     :or {registry registry-path state-path coordinator-state-path
          period-ms 250}}]
   (durable-coordinator/register-adapter!
    :ftriangle/live-smoke
    (fn [_]
      {:decide-fn
       (fn [state]
         {:ok true :status :observed
          :regulator/state-updates
          {:phase :ftriangle-smoke
           :frame-id frame-id
           :last-committed-event-id
           (str "ftriangle-tick-" (inc (or (:regulator/ticks state) 0)))}})
       :reconcile-fn (fn [_ _] {:ok true :status :observed})}))
   (let [state-file (Path/of state-path (make-array String 0))]
     (when-not (runtime/read-state state-file)
       (runtime/atomic-persist! state-file
                                (regulator/initial-state coordinator-id))))
   (let [registered
         (durable-coordinator/register!
          {:registry-path registry :coordinator-id coordinator-id
           :adapter :ftriangle/live-smoke :config {}
           :state-path state-path :period-ms period-ms})]
     (if (:ok registered)
       (assoc (durable-coordinator/start-registered! registry coordinator-id)
              :registration (:status registered))
       registered))))

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
      (fn [] {:ok (boolean (watchdog/running? watchdog-id))
              :watchdog/id watchdog-id})
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
      (loop [remaining traversal-order evidence {}]
        (if-let [stage (first remaining)]
          (if-let [effect (get effects stage)]
            (let [result
                  (loop [attempt 1]
                    (let [observed (effect evidence)
                          classified (when-not (:ok observed)
                                       (classify-failure stage observed))]
                      (if (and (= :substrate (:failure/class classified))
                               (< attempt 2))
                        (recur (inc attempt))
                        (assoc observed :ftriangle/attempts attempt))))]
              (if (:ok result)
                (recur (rest remaining)
                       (assoc evidence stage
                              (assoc (:evidence result)
                                     :ftriangle/attempts
                                     (:ftriangle/attempts result))))
                (assoc (classify-failure stage result)
                       :ftriangle/attempts (:ftriangle/attempts result))))
            (classify-failure stage
                              {:error/code :ftriangle-live-effect-missing}))
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

(defn- smoke-path [root relative]
  (.resolve (Path/of (str root) (make-array String 0)) relative))

(defn wired-effects
  "Construct the six live effects from the same APIs used by production.
  Construction has no effects. CONFIG must name F△'s isolated manifest,
  contract, dispatch authority, watchdog state, and root."
  [{:keys [manifest contract agency-base dispatch-request watchdog-state-path
           smoke-root request-fn await-options]
    :or {agency-base "http://127.0.0.1:7070"
         smoke-root ledger-root
         request-fn runtime/http-json
         await-options {:max-polls 60 :poll-ms 500}}}]
  (let [fixture (read-fixture)
        dispatch-evidence (atom nil)
        repair-state (atom nil)
        watchdog-evidence (atom nil)
        root (str smoke-root)]
    (when-not (= campaign-id (:campaign/id fixture))
      (throw (ex-info "Ftriangle fixture campaign mismatch"
                      {:error/code :ftriangle-isolation-invalid})))
    {:preflight-admission
     (fn [_]
       (let [manifest-check (countdown-manifest/validate manifest)
             result (admission/validate
                     {:countdown-manifest manifest :cycle-contract contract
                      :frame-id frame-id :manifest-check manifest-check})]
         (if (:ok result)
           {:ok true :evidence result}
           (assoc result :error/code :ftriangle-admission-refused))))

     :dispatch-terminal
     (fn [_]
       (let [request (merge {:surface "emacs-repl" :caller campaign-id
                             :job-id (str campaign-id "-" frame-id "-dispatch")
                             :timeout-ms 30000}
                            dispatch-request)
             announced (job-port/announce! request-fn agency-base request)]
         (if-not (:ok announced)
           (assoc announced :error/component :transport)
           (let [request (assoc request :job-id (:job-id announced))
                 activated (job-port/activate! request-fn agency-base request)]
             (if-not (:ok activated)
               (assoc activated :error/component :transport)
               (let [terminal (job-port/await-terminal!
                               request-fn agency-base
                               {:job-id (:job-id announced)
                                :activation-accepted? (:accepted? activated)}
                               await-options)
                     evidence {:request request :announced announced
                               :activated activated :terminal terminal}]
                 (reset! dispatch-evidence evidence)
                 (if (:ok terminal)
                   {:ok true :evidence evidence}
                   terminal)))))))

     :holdout-exclusion
     (fn [_]
       (let [authority {:problem-id (:problem/id fixture)
                        :shelf/holdout :same-problem}
             decision (memory-gate/enforce-carrier
                       :shelf-materialization authority (:shelf fixture))
             receipt {:receipt/type :ftriangle-holdout
                      :receipt/excluded (:excluded decision)
                      :receipt/decision-evidence (:evidence decision)}]
         (if (seq (:excluded decision))
           {:ok true :evidence {:receipt receipt :decision decision}}
           {:ok false :error/code :ftriangle-holdout-evidence-missing})))

     :watchdog-progress
     (fn [_]
       (let [state (runtime/read-state watchdog-state-path)
             observation (:watchdog/trace-observation state)]
         (reset! watchdog-evidence state)
         (if (and (= :watching (:watchdog/status state))
                  (true? (:semantic-cursor-advanced? observation)))
           {:ok true :evidence {:state state :observation observation}}
           {:ok false :error/code :ftriangle-watchdog-progress-absent
            :watchdog/state state})))

     :forced-repair-durability
     (fn [_]
       (let [{:keys [request announced terminal]} @dispatch-evidence
             job (get-in terminal [:dispatch-observation :terminal])
             collection-id (str (:job-id announced) "-forced-invalid")
             initial-state
             {:state/type :live-job-dispatched :request request
              :active-request request
              :ticket {:ticket/id (:job-id announced)
                       :job-id (:job-id announced)}
              :activation/accepted? true
              :terminal-collection
              {:evidence {:collection/id collection-id}
               :submission {:payload {:evidence (:report job)}}}}
             persist-path (smoke-path root "live/forced-repair.edn")
             persisted (atom initial-state)
             persist-fn (fn [state]
                          (let [result (runtime/atomic-persist!
                                        persist-path state)]
                            (when (:ok result) (reset! persisted state))
                            result))
             result
             (job-driver/drive!
              {:request request :state initial-state
               :announce-fn #(job-port/announce! request-fn agency-base %)
               :activate-fn
               (fn [repair ticket]
                 (job-port/activate! request-fn agency-base
                                     (assoc repair :job-id (:job-id ticket))))
               :job-fn (constantly job) :persist-fn persist-fn
               :terminal-validator
               (constantly {:ok false
                            :error/code :ftriangle-deliberate-invalid-terminal
                            :findings [:ftriangle-forced-repair]})
               :receipt-provider
               (constantly {:ok false :error/code :must-not-certify-invalid})
               :terminal-repair-request-fn
               (fn [original _ticket _job failure]
                 {:ok true
                  :request (assoc original
                                  :dispatch/id
                                  (str (:dispatch/id original) "-repair-1")
                                  :repair/attempt 1
                                  :repair/findings (:findings failure))})
               :terminal-budget-config {:collection-attempts 1
                                        :repair-attempts 1}})
             archived (first (:superseded-terminals @persisted))]
         (reset! repair-state @persisted)
         (if (and (:ok result) archived
                  (= (:job-id job) (get-in archived [:job :job-id]))
                  (get-in archived [:terminal-collection :evidence
                                    :collection/id]))
           {:ok true :evidence {:driver result :state @persisted
                                :predecessor archived}}
           {:ok false :error/code :ftriangle-predecessor-not-durable
            :driver result :state @persisted})))

     :combined-trace-closure
     (fn [_]
       (let [terminal (get-in @dispatch-evidence
                              [:terminal :dispatch-observation :terminal])
             documents [@watchdog-evidence @repair-state terminal]
             issued (campaign-trace/issue-combined-trace-receipt!
                     {:certificate {:receipt/id "ftriangle-close"
                                    :receipt/frame-id frame-id}
                      :durable-documents documents
                      :trace-path (smoke-path
                                   root "terminal/combined-trace.json")})]
         (if (:ok issued)
           {:ok true :evidence {:certificate (:certificate issued)
                                :trace (:trace issued)}}
           issued)))}))

(defn run-live!
  "The sole live F△ call. It refuses a non-isolated root and otherwise uses
  the wired effects. Calling this function may dispatch agents; preflight does
  not."
  [raw-config]
  (let [{:keys [smoke-root] :as config} (read-live-config raw-config)
        root (str (or smoke-root ledger-root))]
    (if (or (= root "data/apm-campaigns/jit-all-open-v2")
            (not (.contains root "ftriangle-live-smoke")))
      {:ok false :error/code :ftriangle-isolation-invalid :root root}
      (execute!
       {:checks (default-checks {:smoke-root root})
        :effects (wired-effects (assoc config :smoke-root root))
        :persist-ledger-fn
        (fn [evidence]
          (runtime/atomic-persist!
           (smoke-path root "ledger/evidence.edn") evidence))}))))
