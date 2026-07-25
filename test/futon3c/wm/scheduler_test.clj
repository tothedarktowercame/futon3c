(ns futon3c.wm.scheduler-test
  (:require [babashka.http-client :as http-client]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
            [futon3c.logic.capability-star-map-extractor :as star-extractor]
            [futon3c.wm.scheduler :as scheduler]))

(deftest selection-failure-is-deposited-as-a-typed-loss
  (let [seen (atom nil)
        failure
        (ex-info "live WM verification produced an empty frontier"
                 {:first-failed-seam :phase5-admissible-projection})
        result
        (with-redefs
          [http-client/post
           (fn [url opts]
             (reset! seen {:url url
                           :body (json/parse-string (:body opts) true)})
             {:status 200
              :body
              (json/generate-string
               {:ok true
                :item-ref "/field-desk/items/failure.edn"})})]
          (scheduler/deposit-selection-loss! failure))]
    (is (= :recorded (:status result)))
    (is (= :strategic-selection-empty-frontier
           (:failure-kind result)))
    (is (= "http://127.0.0.1:7070/api/alpha/morning-brief/item"
           (:url @seen)))
    (is (= "strategic-selection-empty-frontier"
           (get-in @seen [:body :failure :kind])))
    (is (= "selection"
           (get-in @seen [:body :failure :stage])))
    (is (= "phase5-admissible-projection"
           (get-in @seen [:body :failure :first-failed-seam])))))

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

(deftest refresh-forwards-the-reason-bearing-selector
  (testing "the scheduler cannot silently call the WM generator without selection"
    (let [seen (atom nil)
          statuses (atom [])
          selector (fn [_] {:status :verified-live-selection})
          generate
          (fn [days opts]
            (reset! seen {:days days :opts opts})
            {:data {:window {:days days}}
             :judgement {:decision {:selected-policy-id "pi-s-test"}}})]
      (with-redefs-fn
        {#'scheduler/render-payload-json
         (fn [bundle]
           {:payload bundle
            :body "{}"})
         #'scheduler/report-snapshot-status!
         (fn [status activity]
           (swap! statuses conj [status activity]))}
        (fn []
          (#'scheduler/refresh-one-window! generate selector 14)))
      (is (= 14 (:days @seen)))
      (is (identical? selector
                      (get-in @seen [:opts :strategic-selection-fn])))
      (is (= [[:invoking "snapshot scan 14d window"]
              [:idle nil]]
             @statuses)))))

(deftest war-machine-roster-entry-is-stable-and-non-invokable
  (let [registered (atom nil)]
    (with-redefs [reg/get-agent (constantly nil)
                  reg/register-agent!
                  (fn [record]
                    (reset! registered record)
                    record)]
      (scheduler/ensure-war-machine-agent!))
    (is (= "war-machine" (get-in @registered [:agent-id :id/value])))
    (is (= :apparatus (get-in @registered [:agent-id :id/type])))
    (is (= :wm (:type @registered)))
    (is (nil? (:invoke-fn @registered)))
    (is (= {:apparatus? true
            :cwd "/home/joe/code/futon2"
            :agency/contracts {:bell-on-complete? false}}
           (:metadata @registered)))))
