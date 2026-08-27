(ns futon3c.apm.ftriangle-live-smoke-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-trace :as campaign-trace]
            [futon3c.apm.ftriangle-live-smoke :as sut]))

(defn passing-checks []
  (into {} (map #(vector % (constantly {:ok true}))) sut/condition-order))

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
                                  (rest sut/traversal-order))})]
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
              (rest sut/traversal-order))
        result (sut/execute!
                {:checks (passing-checks) :effects effects
                 :persist-ledger-fn (fn [ledger]
                                      {:ok true :ledger ledger})})]
    (is (:ok result) (pr-str result))
    (is (= (vec (rest sut/traversal-order)) @called))
    (is (= (set sut/traversal-order)
           (set (keys (:evidence result)))))))

(deftest missing-live-effect-is-an-apparatus-failure
  (let [result (sut/execute! {:checks (passing-checks) :effects {}})]
    (is (= :apparatus (:failure/class result)))
    (is (= :dispatch-terminal (:stage result)))
    (is (= :block-go-live (:failure/action result)))))
