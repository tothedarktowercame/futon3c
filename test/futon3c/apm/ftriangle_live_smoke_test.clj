(ns futon3c.apm.ftriangle-live-smoke-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.countdown-manifest :as countdown-manifest]
            [futon3c.apm.countdown-pre-admission :as admission]
            [futon3c.apm.ftriangle-live-smoke :as sut]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-job-driver :as job-driver]
            [futon3c.apm.live-preflight-runtime :as runtime]))

(defn passing-checks []
  (into {} (map #(vector % (constantly {:ok true}))) sut/condition-order))

(deftest committed-live-authorities-resolve-and-validate
  (let [config (sut/read-live-config
                (edn/read-string
                 (slurp "data/apm-campaigns/ftriangle-live-smoke-v1/config.edn")))
        validated (countdown-manifest/validate (:manifest config))]
    (is (:valid? validated) (pr-str validated))
    (is (= :apm-complete-frame-cycle-v2
           (get-in config [:contract :contract/id])))
    (is (= "ft1" (get-in config [:manifest :units 0 :frame/id])))))

(deftest preflight-names-each-unmet-condition-without-dispatch
  (doseq [condition sut/condition-order]
    (let [dispatches (atom 0)
          result (sut/execute!
                  {:checks (assoc (passing-checks) condition
                                  (constantly {:ok false :finding condition}))
                   :effects (into {}
                                  (map #(vector % (fn [_]
                                                    (swap! dispatches inc)
                                                    {:ok true :evidence {}})))
                                  sut/traversal-order)})]
      (is (= :ftriangle-preconditions-unmet (:error/code result)) condition)
      (is (= [condition] (:unmet result)) condition)
      (is (zero? @dispatches) condition))))

(deftest failure-classification-separates-substrate-from-apparatus
  (is (= [:substrate :retry]
         ((juxt :failure/class :failure/action)
          (sut/classify-failure :dispatch-terminal
                                {:error/component :transport
                                 :error/code :hyperedge-unreachable}))))
  (is (= [:apparatus :block-go-live]
         ((juxt :failure/class :failure/action)
          (sut/classify-failure :holdout-exclusion
                                {:error/code :holdout-evidence-missing})))))

(deftest harness-classification-is-explicit-and-produces-no-verdict
  (let [direct (sut/classify-failure
                :preflight-admission
                {:error/code :ftriangle-live-effect-missing})
        nested (sut/classify-failure
                :forced-repair-durability
                {:error/code :ftriangle-predecessor-not-durable
                 :driver {:error/code :live-job-driver-input-invalid}})
        ambiguous (sut/classify-failure
                   :forced-repair-durability
                   {:error/code :live-job-terminal-repair-request-invalid})]
    (doseq [result [direct nested]]
      (is (= :harness (:failure/class result)))
      (is (= :fix-ftriangle (:failure/action result)))
      (is (= :verdict/none (:verdict result))))
    (is (= :apparatus (:failure/class ambiguous)))
    (is (= :block-go-live (:failure/action ambiguous)))))

(deftest substrate-stage-retries-once-without-retrying-apparatus
  (let [calls (atom 0)
        effects (into {}
                      (map (fn [stage]
                             [stage (if (= stage :preflight-admission)
                                      (fn [_]
                                        (if (= 1 (swap! calls inc))
                                          {:ok false
                                           :error/component :transport
                                           :error/code :hyperedge-unreachable}
                                          {:ok false
                                           :error/code :admission-refused}))
                                      (constantly {:ok true :evidence {}}))]))
                      sut/traversal-order)
        result (sut/execute! {:checks (passing-checks) :effects effects})]
    (is (= 2 @calls))
    (is (= :apparatus (:failure/class result)))
    (is (= 2 (:ftriangle/attempts result)))))

(deftest executor-requires-all-six-ledger-evidence-items-and-valid-closure
  (let [trace-body {"schemaVersion" 1 "traceKind" "ftriangle-test"}
        digest (campaign-trace/combined-trace-digest trace-body)
        certificate {:trace/combined trace-body :trace/digest digest
                     :trace/projected-from-durable-state? true
                     :trace/observation-kinds
                     (mapv :kind (campaign-trace/observation-schemas))
                     :trace/checker-receipt
                     {:checker/status :accepted :trace/digest digest}}
        called (atom [])
        effects
        (into {}
              (map (fn [stage]
                     [stage (fn [_]
                              (swap! called conj stage)
                              {:ok true
                               :evidence (if (= :combined-trace-closure stage)
                                           {:certificate certificate}
                                           {:receipt/id (name stage)})})]))
              sut/traversal-order)
        result (sut/execute!
                {:checks (passing-checks) :effects effects
                 :persist-ledger-fn (fn [ledger]
                                      {:ok true :ledger ledger})})]
    (is (:ok result) (pr-str result))
    (is (= (vec sut/traversal-order) @called))
    (is (= (set sut/traversal-order)
           (set (keys (:evidence result)))))))

(deftest missing-live-effect-is-a-harness-failure
  (let [result (sut/execute! {:checks (passing-checks) :effects {}})]
    (is (= :harness (:failure/class result)))
    (is (= :preflight-admission (:stage result)))
    (is (= :fix-ftriangle (:failure/action result)))
    (is (= :verdict/none (:verdict result)))))

(deftest thrown-live-effect-is-a-named-apparatus-failure
  (let [result (sut/execute!
                {:checks (passing-checks)
                 :effects {:preflight-admission
                           (fn [_] (throw (ClassCastException. "wrong port type")))}})]
    (is (= :apparatus (:failure/class result)))
    (is (= :ftriangle-live-effect-threw
           (get-in result [:finding :error/code])))
    (is (= "java.lang.ClassCastException"
           (get-in result [:finding :exception/class])))))

(deftest successful-stage-evidence-is-durable-before-the-next-stage
  (let [persisted (atom [])
        effects (into {}
                      (map (fn [stage]
                             [stage
                              (if (= stage :watchdog-progress)
                                (fn [_] (throw (Exception. "later failure")))
                                (fn [_] {:ok true
                                         :evidence {:stage stage}}))]))
                      sut/traversal-order)
        result (sut/execute!
                {:checks (passing-checks) :effects effects
                 :persist-ledger-fn
                 (fn [state] (swap! persisted conj state) {:ok true})})
        holdout-checkpoint
        (some #(when (contains? (:traversal/evidence %)
                                :holdout-exclusion) %)
              @persisted)]
    (is (= :watchdog-progress (:stage result)))
    (is (map? holdout-checkpoint))
    (is (= :holdout-exclusion
           (get-in holdout-checkpoint
                   [:traversal/evidence :holdout-exclusion :stage])))))

(deftest wired-effects-call-production-ports-and-preserve-repair-evidence
  (let [calls (atom [])
        trace-body {"schemaVersion" 1 "traceKind" "wired-test"}
        digest (campaign-trace/combined-trace-digest trace-body)
        certificate {:trace/combined trace-body :trace/digest digest
                     :trace/projected-from-durable-state? true
                     :trace/observation-kinds
                     (mapv :kind (campaign-trace/observation-schemas))
                     :trace/checker-receipt
                     {:checker/status :accepted :trace/digest digest}}]
    (with-redefs
     [countdown-manifest/validate
      (fn [_] (swap! calls conj :manifest) {:valid? true})
      admission/validate
      (fn [_] (swap! calls conj :admission) {:ok true})
      job-port/announce!
      (fn [& _] (swap! calls conj :announce) {:ok true :job-id "job-1"})
      job-port/activate!
      (fn [& _] (swap! calls conj :activate) {:ok true :accepted? true})
      job-port/await-terminal!
      (fn [& _]
        (swap! calls conj :terminal)
        {:ok true :dispatch-observation
         {:terminal {:job-id "job-1" :state :done :report {}}
          :terminal-job-id "job-1"}})
      runtime/read-state
      (fn [_] {:watchdog/status :watching
               :watchdog/trace-observation
               {:semantic-cursor-advanced? true}})
      runtime/atomic-persist!
      (fn [_ _] (swap! calls conj :persist) {:ok true})
      job-driver/drive!
      (fn [{:keys [persist-fn]}]
        (let [state {:superseded-terminals
                     [{:job {:job-id "job-1"}
                       :terminal-collection
                       {:evidence {:collection/id "collection-1"}}}]}]
          (persist-fn state)
          {:ok true :status :awaiting-terminal :state state}))
      campaign-trace/issue-combined-trace-receipt!
      (fn [_]
        (swap! calls conj :checker)
        {:ok true :certificate certificate :trace trace-body})]
      (let [effects (sut/wired-effects
                     {:manifest {} :contract {} :watchdog-state-path "watch"
                      :dispatch-request {:dispatch/id "ftriangle-dispatch"
                                         :agent-id "fixture-agent"
                                         :prompt "trivial"}})
            result (sut/execute!
                    {:checks (passing-checks) :effects effects
                     :persist-ledger-fn (fn [_] {:ok true})})]
        (is (:ok result) (pr-str result))
        (is (= (set sut/traversal-order) (set (keys (:evidence result)))))
        (is (every? (set @calls)
                    [:manifest :admission :announce :activate :terminal
                     :persist :checker]))))))
