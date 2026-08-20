(ns futon3c.aif.instrumented-campaign-live-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.aif.instrumented-campaign :as campaign]
            [futon3c.aif.instrumented-campaign-live :as live]))

(defn- ranked [rank repo target holes score]
  {:rank rank
   :controller-score score
   :action {:type :advance-mission
            :target target
            :mission-path (str "/home/joe/code/" repo "/holes/" target ".md")
            :open-hole-count holes}})

(def judgement
  {:ranked-actions
   [(ranked 1 "futon4" "outside-deep" 5 10.00)
    (ranked 2 "futon4" "outside-compact" 4 10.05)
    (ranked 3 "futon3c" "inside-compact" 1 10.10)
    (ranked 4 "futon3c" "inside-cheap" 0 10.20)]
   :decision {}})

(deftest live-plan-is-budgeted-and-changes-policy-with-slow-mode
  (let [context {:campaign/id "dry"
                 :slow-state {:slow/mode :exploration}}
        request (#'live/r11-request judgement context)
        tick {:r11/request request :r15/input @#'live/r15-input}
        exploration (campaign/instrument-judgement judgement tick context)
        consolidation
        (campaign/instrument-judgement
         judgement tick (assoc-in context [:slow-state :slow/mode]
                                  :consolidation))]
    (testing "local winners exceed the parent while a cross-field pair fits"
      (is (= 7.0 (get-in exploration [:r11 :total-cost])))
      (is (= #{"inside-futon3c/inside-cheap"
               "rest-of-stack/outside-deep"}
             (get-in exploration [:r11 :selected-ids]))))
    (testing "the slow prior changes which selected field supplies the action"
      (is (= :advance-capability
             (get-in exploration [:r15 :primary-move :move/class])))
      (is (= :close-hole
             (get-in consolidation [:r15 :primary-move :move/class]))))))

(deftest r17-corpus-uses-only-live-mission-capability-observations
  (let [corpus (#'live/r17-corpus
                {:capabilities {"a" {} "b" {} "not-observed" {}}
                 :missions {"M1" {:scope ["a" "unknown"] :produces ["b"]}
                            "M2" {:scope [] :produces ["a"]}}})]
    (is (= {:capabilities ["a" "b"]
            :edges [["a" "M1"] ["b" "M1"] ["a" "M2"]]
            :discharges []}
           corpus))))
