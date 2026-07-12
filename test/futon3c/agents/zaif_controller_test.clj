(ns futon3c.agents.zaif-controller-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.zai-api :as zai]
            [futon3c.agents.zaif-controller :as zaif]))

(deftest fixture-beliefs-produce-deterministic-arm-choices
  (testing "posting statistics can select retrieve"
    (let [d (zaif/decide {:mission "M-z"
                          :observations {:posting-stats {:total-docs 1000 :dfs [1]}
                                         :estimated-tokens 400}
                          :task-belief {:act-value 0.1}
                          :c-belief {:operator-c-uncertainty 0.2}})]
      (is (= :retrieve (:arm d)))
      (is (= 1.0 (:gamma-used d)))))
  (testing "operator uncertainty can select ask after attention cost"
    (is (= :ask (:arm (zaif/decide {:mission "M-z"
                                    :c-belief {:operator-c-uncertainty 1.0}
                                    :observations {:retrieve-eig 0.1}
                                    :task-belief {:act-value 0.1}}))))))

(deftest gamma-lowered-mission-shifts-act-to-hedge
  (let [inputs {:mission "M-low"
                :gamma {"M-low" {:policy-precision 0.5}}
                :task-belief {:act-value 0.6}
                :observations {:retrieve-eig 0.45 :estimated-tokens 100}
                :c-belief {:operator-c-uncertainty 0.1}}
        neutral (zaif/decide (assoc inputs :gamma {"M-low" {:policy-precision 1.0}}))
        lowered (zaif/decide inputs)]
    (is (= :act (:arm neutral)))
    (is (= :retrieve (:arm lowered)))
    (is (= 0.5 (:gamma-used lowered)))))

(deftest missing-gamma-uses-uniform-prior
  (let [d (zaif/decide {:mission "M-unburned"
                        :gamma {}
                        :task-belief {:act-value 0.2}})]
    (is (= 1.0 (:gamma-used d)))))

(deftest evidence-record-shape
  (let [inputs {:mission "M-z" :task-belief {:act-value 0.2}}
        decision (zaif/decide inputs)
        ev (zaif/decision-evidence-entry {:agent-id "zai-test"
                                          :sid "sid-1"
                                          :decision decision
                                          :inputs inputs})]
    (is (= :coordination (:evidence/type ev)))
    (is (= :step (:evidence/claim-type ev)))
    (is (= [:zaif :arm-choice] (:evidence/tags ev)))
    (is (= (:arm decision) (get-in ev [:evidence/body :arm])))
    (is (= (:g-terms decision) (get-in ev [:evidence/body :g-terms])))
    (is (= "M-z" (get-in ev [:evidence/body :mission])))
    (is (string? (get-in ev [:evidence/body :inputs-digest :sha256-16])))))

(deftest zai-profile-does-not-consult-controller
  (let [called? (atom false)]
    (with-redefs [zaif/decide (fn [_] (reset! called? true) {:arm :yield})
                  zaif/persist-decision! (fn [_])]
      (is (nil? (#'zai/maybe-zaif-decision! {:profile :zai
                                             :agent-id "zai-test"
                                             :sid "sid"})))
      (is (false? @called?)))))

(deftest zaif-profile-consults-controller-and-persists
  (let [persisted (atom nil)]
    (with-redefs [zaif/persist-decision! (fn [ctx] (reset! persisted ctx))]
      (let [decision (#'zai/maybe-zaif-decision!
                      {:profile :zaif
                       :agent-id "zai-test"
                       :sid "sid"
                       :zaif-inputs-fn (fn [_] {:mission "M-z"
                                                :task-belief {:act-value 1.0}})})]
        (is (= :act (:arm decision)))
        (is (= :act (get-in @persisted [:decision :arm])))
        (is (= "M-z" (get-in @persisted [:inputs :mission])))))))
