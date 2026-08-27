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

(def harness-input-error-codes
  #{:ftriangle-live-effect-missing
    :ftriangle-ledger-persist-port-missing
    :ftriangle-port-contract-invalid
    :live-job-driver-input-invalid})

(defn validate-live-port-config
  "Normalize the EDN config into the host types required by F△'s effect ports.
  Invalid call shapes are rejected before preflight or dispatch."
  [config]
  (let [dispatch (:dispatch-request config)
        findings
        (cond-> []
          (not (map? (:manifest config))) (conj :manifest-not-map)
          (not (map? (:contract config))) (conj :contract-not-map)
          (not (and (string? (:smoke-root config))
                    (not-empty (:smoke-root config))))
          (conj :smoke-root-not-string)
          (not (and (string? (:watchdog-state-path config))
                    (not-empty (:watchdog-state-path config))))
          (conj :watchdog-state-path-not-string)
          (not (and (map? dispatch)
                    (every? #(and (string? (get dispatch %))
                                  (not-empty (get dispatch %)))
                            [:agent-id :prompt :job-id])))
          (conj :dispatch-request-invalid))]
    (if (seq findings)
      {:ok false :error/code :ftriangle-port-contract-invalid
       :failure/class :harness :failure/action :fix-ftriangle
       :verdict :verdict/none :findings findings}
      {:ok true
       :config (assoc config
                      :watchdog-state-path
                      (Path/of (:watchdog-state-path config)
                               (make-array String 0)))})))

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
   (if (get-in (durable-coordinator/read-registry registry)
               [:entries coordinator-id])
     (durable-coordinator/resume! registry coordinator-id)
     (let [registered
           (durable-coordinator/register!
            {:registry-path registry :coordinator-id coordinator-id
             :adapter :ftriangle/live-smoke :config {}
             :state-path state-path :period-ms period-ms})]
       (if (:ok registered)
         (assoc (durable-coordinator/start-registered! registry coordinator-id)
                :registration (:status registered))
         registered)))))

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
  (let [error-codes #{(:error/code result)
                      (get-in result [:driver :error/code])}
        harness? (boolean (some harness-input-error-codes error-codes))
        substrate? (or (= :transport (:error/component result))
                       (contains? #{:hyperedge-unreachable :job-port-budget-exhausted
                                    :dispatch-timeout :terminal-timeout}
                                  (:error/code result)))]
    {:ok false :status :failed :stage stage
     :failure/class (cond substrate? :substrate harness? :harness
                          :else :apparatus)
     :failure/action (cond substrate? :retry harness? :fix-ftriangle
                           :else :block-go-live)
     :verdict (if harness? :verdict/none :verdict/fail)
     :finding result}))

(defn repair-request
  "Mint F△'s one permitted repair as a distinct Agency job."
  [original failure]
  {:ok true
   :request (assoc original
                   :job-id (str (:job-id original) "-repair-1")
                   :dispatch/id (str (:dispatch/id original) "-repair-1")
                   :repair/attempt 1
                   :repair/findings (:findings failure))})

(defn execute!
  "Execute an isolated live smoke frame through explicitly supplied effects.
  Each effect receives accumulated evidence and must return {:ok true
  :evidence ...}. No default can dispatch or mutate a production campaign."
  [{:keys [checks effects persist-ledger-fn]}]
  (let [invalid-effects (into [] (remove #(fn? (get effects %)))
                              traversal-order)
        port-check (cond
                     (seq invalid-effects)
                     {:ok false :error/code :ftriangle-port-contract-invalid
                      :failure/class :harness :failure/action :fix-ftriangle
                      :verdict :verdict/none
                      :findings (mapv #(vector % :effect-not-function)
                                      invalid-effects)}
                     (not (fn? persist-ledger-fn))
                     {:ok false :error/code :ftriangle-port-contract-invalid
                      :failure/class :harness :failure/action :fix-ftriangle
                      :verdict :verdict/none
                      :findings [[:persist-ledger :not-function]]}
                     :else {:ok true})
        ready (when (:ok port-check)
                (preflight (or checks (default-checks))))]
    (if-not (:ok port-check)
      port-check
      (if-not (:ok ready)
        ready
        (loop [remaining traversal-order evidence {}]
          (if-let [stage (first remaining)]
            (let [effect (get effects stage)
                  result
                  (loop [attempt 1]
                    (let [raw-observed
                          (try (effect evidence)
                               (catch Throwable error
                                 {:ok false
                                  :error/code :ftriangle-live-effect-threw
                                  :exception/class (.getName (class error))
                                  :exception/message (.getMessage error)}))
                          observed
                          (if (and (map? raw-observed)
                                   (boolean? (:ok raw-observed)))
                            raw-observed
                            {:ok false
                             :error/code :ftriangle-port-contract-invalid
                             :findings [[stage :result-not-typed-map]]})
                          classified (when-not (:ok observed)
                                       (classify-failure stage observed))]
                      (if (and (= :substrate (:failure/class classified))
                               (< attempt 2))
                        (recur (inc attempt))
                        (assoc observed :ftriangle/attempts attempt))))]
              (if (:ok result)
                (let [next-evidence
                      (assoc evidence stage
                             (assoc (:evidence result)
                                    :ftriangle/attempts
                                    (:ftriangle/attempts result)))
                      checkpoint
                      (persist-ledger-fn
                       {:campaign/id campaign-id :frame/id frame-id
                        :traversal/status :in-progress
                        :traversal/evidence next-evidence})]
                  (if (:ok checkpoint)
                    (recur (rest remaining) next-evidence)
                    (classify-failure
                     :ledger-evidence
                     {:error/code :ftriangle-stage-evidence-not-durable
                      :stage stage :finding checkpoint})))
                (assoc (classify-failure stage result)
                       :ftriangle/attempts (:ftriangle/attempts result))))
            (let [closure (:combined-trace-closure evidence)
                  valid-close? (campaign-trace/valid-combined-trace-receipt?
                                (:certificate closure))]
              (if-not valid-close?
                (classify-failure
                 :combined-trace-closure
                 {:error/code :ftriangle-closure-receipt-invalid})
                (let [persisted
                      (persist-ledger-fn
                       {:campaign/id campaign-id :frame/id frame-id
                        :traversal/status :closed
                        :traversal/evidence evidence})]
                  (if (:ok persisted)
                    {:ok true :status :closed :campaign/id campaign-id
                     :frame/id frame-id :evidence evidence
                     :persisted persisted}
                    (classify-failure :ledger-evidence persisted)))))))))))

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
                             :dispatch/id
                             (str campaign-id "-" frame-id "-dispatch")
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
                 (repair-request original failure))
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
                      :trace-path (io/file
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
  (let [validated (validate-live-port-config (read-live-config raw-config))
        {:keys [smoke-root] :as config} (:config validated)
        root (str (or smoke-root ledger-root))]
    (if-not (:ok validated)
      validated
      (if (or (= root "data/apm-campaigns/jit-all-open-v2")
            (not (.contains root "ftriangle-live-smoke")))
      {:ok false :error/code :ftriangle-isolation-invalid :root root}
      (execute!
       {:checks (default-checks {:smoke-root root})
        :effects (wired-effects (assoc config :smoke-root root))
        :persist-ledger-fn
        (fn [evidence]
          (runtime/atomic-persist!
           (smoke-path root "ledger/evidence.edn") evidence))})))))
