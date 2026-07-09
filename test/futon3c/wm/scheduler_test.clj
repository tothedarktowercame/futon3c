(ns futon3c.wm.scheduler-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.capability-star-map-extractor :as star-extractor]
            [futon3c.wm.scheduler :as scheduler]))

(deftest trim-action-predictions-adds-structural-hole-count-beside-lexical-count
  (testing "served ranked-actions carry structural holes without changing lexical holes"
    (with-redefs [star-extractor/structural-hole-report
                  (fn [mission-id]
                    (when (= "M-learning-loop" mission-id)
                      {:structural-hole-count 7}))]
      (let [judgement {:ranked-actions
                       [{:rank 1
                         :action {:type :advance-mission
                                  :target "M-learning-loop"
                                  :open-hole-count 6}
                         :prediction {:next-belief {:large true}
                                      :next-observation {:ok true}}}
                        {:rank 2
                         :target "M-learning-loop"
                         :open-hole-count 6}
                        {:rank 3
                         :action {:type :address-sorry
                                  :target "sorry/not-a-mission"
                                  :open-hole-count 1}}]}
            out (#'scheduler/trim-action-predictions judgement)
            [nested top-level sorry] (:ranked-actions out)]
        (is (= 6 (get-in nested [:action :open-hole-count])))
        (is (= 7 (get-in nested [:action :structural-hole-count])))
        (is (nil? (get-in nested [:prediction :next-belief])))
        (is (= {:ok true} (get-in nested [:prediction :next-observation])))
        (is (= 6 (:open-hole-count top-level)))
        (is (= 7 (:structural-hole-count top-level)))
        (is (nil? (get-in sorry [:action :structural-hole-count])))))))

(deftest trim-action-predictions-degrades-when-structural-read-fails
  (testing "scope read failure cannot break snapshot rendering"
    (with-redefs [star-extractor/structural-hole-report
                  (fn [_mission-id] (throw (ex-info "ego proxy down" {})))]
      (let [judgement {:ranked-actions
                       [{:action {:target "M-learning-loop"
                                  :open-hole-count 6}}]}
            out (#'scheduler/trim-action-predictions judgement)]
        (is (= 6 (get-in out [:ranked-actions 0 :action :open-hole-count])))
        (is (nil? (get-in out [:ranked-actions 0 :action :structural-hole-count])))))))
