(ns futon3c.peripheral.strategic-canary-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.strategic-canary :as canary]
            [futon3c.peripheral.strategic-policies :as policies]
            [futon3c.peripheral.strategic-policies-test
             :refer [phase7-input]]))

(defn- read-edn [path] (-> path io/file slurp edn/read-string))

(defn- canary-input []
  (let [{:keys [outer-result fixture]} (phase7-input)]
    {:shadow-result (policies/run-shadow-window outer-result fixture)
     :fixture
     (read-edn
      "holes/labs/M-typed-memories/phase8-advice-only-canary.edn")}))

(deftest bounded-autonomy-authorizes-the-reviewed-policy-not-a-click
  (let [{:keys [shadow-result fixture]} (canary-input)
        result (canary/bounded-autonomy shadow-result fixture)]
    (is (= :bounded-autonomy-authorized (:status result)))
    (is (= :bounded-autonomy (:effective-rung result)))
    (is (= ["M-shared-memory-control-build-test"
            "M-aif-policy-conditioned-eig"]
           (get-in result [:recommendation :mission-ids])))
    (is (= ["M-shared-memory-control-build-test"]
           (get-in result [:counterfactual-baseline :mission-ids])))
    (is (= canary/operator-decision-evidence-id
           (:operator-decision-evidence-id result)))
    (is (= 13 (count (get-in result [:machine-gates
                                     :armed-tripwire-ids]))))
    (is (true? (get-in result [:enactment :authorized?])))
    (is (false? (get-in result [:enactment :executed?])))
    (is (false? (get-in result [:enactment
                                :operator-confirmation-required?])))
    (is (= "M-shared-memory-control-build-test"
           (:selected-mission result)))
    (is (true? (:live-ordering-changed? result)))
    (is (= 13 (get-in result [:calibration :sample-count])))
    (is (false? (get-in result [:calibration :advance?])))
    (is (= "http://127.0.0.1:7070/api/alpha/morning-brief/addendum"
           (get-in result [:delivery-qa :endpoint])))))

(deftest operator-confirmation-is-no-longer-an-enactment-input
  (let [{:keys [shadow-result fixture]} (canary-input)]
    (doseq [archival-value [true false nil]]
      (let [result
            (canary/bounded-autonomy
             shadow-result
             (assoc fixture :operator-confirmed? archival-value))]
        (is (= :bounded-autonomy-authorized (:status result)))
        (is (true? (get-in result [:enactment :authorized?])))))))

(deftest machine-gates-still-fail-closed
  (let [{:keys [shadow-result fixture]} (canary-input)
        reason
        (fn [changed]
          (:rollback-reason
           (canary/bounded-autonomy shadow-result changed)))]
    (testing "all 13 tripwires are armed and clear"
      (is (= :tripwire-fired
             (reason (assoc fixture :tripwire-clear? false))))
      (is (= :invalid-bounded-autonomy-fixture
             (reason (update fixture :armed-tripwire-ids pop)))))
    (testing "query and resource bounds"
      (is (= :query-or-resource-bound-failed
             (reason (assoc fixture :query-limit 11)))))
    (testing "the immediate cache-aware warm-up bound"
      (is (= :block-unwarmed-click
             (reason
              (assoc-in fixture
                        [:serving-cache-gate
                         :accepted-endpoint-latencies 0 :elapsed-ms]
                        1001.0)))))
    (testing "independent witness and memory provenance"
      (is (= :independent-outcome-incomplete
             (reason
              (assoc-in fixture [:observed-outcome :witness-status]
                        :self-asserted))))
      (is (= :independent-outcome-incomplete
             (reason
              (assoc-in fixture [:observed-outcome :memory-ids-used]
                        ["e-not-surfaced"])))))
    (testing "delivery QA is a gate and port 7070 is mandatory"
      (is (= :delivery-qa-gate-invalid
             (reason (assoc-in fixture [:delivery-qa :required?] false))))
      (is (= :delivery-qa-gate-invalid
             (reason
              (assoc-in fixture [:delivery-qa :endpoint]
                        "http://127.0.0.1:7073/api/alpha/morning-brief/addendum")))))
    (testing "a missing, rejected, or incomplete QA note is a delivery-gate failure"
      (is (= :delivery-qa-note-missing
             (reason (assoc-in fixture [:delivery-qa :note-status] :absent))))
      (is (= :delivery-qa-note-missing
             (reason (update fixture :delivery-qa dissoc :note-status))))
      (is (= :delivery-qa-note-rejected
             (reason (assoc-in fixture [:delivery-qa :note-status] :rejected))))
      (is (= :delivery-qa-note-incomplete
             (reason (update-in fixture [:delivery-qa :note]
                                dissoc :changed-or-progressed))))
      (is (= :delivery-qa-note-incomplete
             (reason (assoc-in fixture [:delivery-qa :note :evidence-ids] []))))
      (is (= :delivery-qa-note-incomplete
             (reason (assoc-in fixture [:delivery-qa :note :commit-shas] []))))
      (is (= :delivery-qa-note-incomplete
             (reason (assoc-in fixture [:delivery-qa :note :commit-shas]
                               ["not-a-sha"]))))
      (let [result (canary/bounded-autonomy
                    shadow-result
                    (update fixture :delivery-qa dissoc :note))]
        (is (false? (get-in result [:enactment :authorized?])))
        (is (= :current-additive (:fallback-controller result)))))
    (testing "additive behavior exists only behind the named rollback"
      (let [result
            (canary/bounded-autonomy
             shadow-result (assoc fixture :tripwire-clear? false))]
        (is (= :current-additive (:fallback-controller result)))
        (is (= :explicit-rollback-only (:fallback-mode result)))
        (is (= "e74c7e7" (:rollback-boundary result)))
        (is (false? (get-in result [:enactment :authorized?])))))))
