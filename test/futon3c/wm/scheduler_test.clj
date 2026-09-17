(ns futon3c.wm.scheduler-test
  (:require [babashka.http-client :as http-client]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
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

(deftest trim-cascade-predictions-strips-heavy-next-belief-from-the-cascade-lane
  (testing "the cascade decision and cascade problems lose :next-belief but keep
            light prediction fields; the flat :ranked-actions shrink is gone"
    (let [judgement {:decision
                     {:action {:kind :cascade-candidate
                               :precedence [{:type :advance-mission :target "M-x"}]}
                      :selection-law {:applied :cascade-selection-posterior
                                      :posterior {}}
                      :beta {:value 0.25 :status :declared}}
                     :cascade-problems
                     {:problems [{:target "M-x"
                                  :prediction {:next-belief {:large true}
                                               :next-observation {:ok true}}}]}
                     :priorities []}
          out (scheduler/trim-cascade-predictions judgement)]
      (is (nil? (get-in out [:decision :action :precedence 0 :prediction])))
      (is (nil? (get-in out [:cascade-problems :problems 0 :prediction :next-belief])))
      (is (= {:ok true}
             (get-in out [:cascade-problems :problems 0 :prediction :next-observation])))
      (is (= :cascade-selection-posterior
             (get-in out [:decision :selection-law :applied]))))))

(deftest trim-cascade-predictions-deep-next-belief-inside-the-decision
  (testing "a :next-belief nested anywhere inside the decision subtree is dropped"
    (let [candidate {:kind :cascade-candidate :precedence [:p1]}
          out (scheduler/trim-cascade-predictions
               {:decision {:selection-law
                           {:posterior {candidate
                                        {:prediction {:next-belief {:huge true}}}}}}})]
      (is (nil? (get-in out [:decision :selection-law :posterior candidate
                             :prediction :next-belief]))))))

(deftest trim-cascade-predictions-leaves-non-map-judgements-alone
  (testing "absent decision/cascade-problems keys change nothing"
    (is (= {:priorities []}
           (scheduler/trim-cascade-predictions {:priorities []})))))

(deftest refresh-uses-controller-generator-without-fixture-selector
  (testing "the generator owns controller selection; scheduler supplies no fixture override"
    (let [seen (atom nil)
          statuses (atom [])
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
          (#'scheduler/refresh-one-window! generate 14)))
      (is (= 14 (:days @seen)))
      (is (= {} (:opts @seen)))
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
