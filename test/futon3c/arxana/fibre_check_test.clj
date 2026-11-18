(ns futon3c.arxana.fibre-check-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.fibre-check :as f]))

(def src {:repo "futon4" :revision "abc" :path "p.md" :section "§1"})
(def cat-view {:status :ok :choices [{:id "c1" :sources [src] :checks [{:id "k1" :latest-run {:id "r1"}}]}
                                     {:id "c2" :sources [src] :checks []}]})
(defn reading [id] {:record {:id id} :evidence [] :evidence-completeness :complete})
(def ben-view {:status :ok :benefit (reading "b")
               :items [{:item (reading "i1") :serves [(reading "s1")] :names [{:link (reading "n1") :entity (reading "e1")}]}
                       {:item (reading "i2") :serves [(reading "s2")] :names []}]
               :substrate [{:serves (reading "s3")}]})

(deftest fibres-open
  (is (= :pass (:result (f/check [[:catalogue/view cat-view] [:cascade/benefit-filings ben-view]])))))

(deftest planted-gaps-are-violations
  (is (= [[:choices 1]] (map :path (:violations (f/check [[:catalogue/view (assoc-in cat-view [:choices 1 :sources] [])]])))))
  (is (= [[:items 0 :names 0 :entity]]
         (map :path (:violations (f/check [[:cascade/benefit-filings
                                            (update-in ben-view [:items 0 :names 0 :entity] assoc :evidence-completeness :partial)]])))))
  (is (= [[:items 1 :item]] (map :path (:violations (f/check [[:cascade/benefit-filings (update-in ben-view [:items 1 :item] dissoc :evidence)]]))))))

(deftest incomplete-view-is-error-not-pass
  (is (= :error (:result (f/check [[:catalogue/view {:status :inconclusive :reason :stale-index}]]))))
  (is (= :error (:result (f/check [])))))
