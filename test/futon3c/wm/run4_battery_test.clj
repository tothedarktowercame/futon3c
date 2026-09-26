(ns futon3c.wm.run4-battery-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-battery :as sut]))

(def sha (apply str (repeat 64 "a")))
(def control {:edges [{:from :R20 :to :R12}]
              :route-measured-drawn [] :decisions {}})
(def bundle {:identity {:series-id "RUN4" :trial-id :t1}
             :run-record {:run/id "run-1"
                          :route [{:fromNode "R20" :toNode "R12" :via "scan"}]}})

(deftest exact-nonempty-population-validates
  (let [battery (sut/produce "RUN4" sha sha [bundle] control)]
    (is (= 4 (count (:rows battery))))
    (is (sut/validate battery "RUN4" sha sha [bundle]))
    (doseq [bad [(update battery :rows pop)
                 (update battery :rows conj (first (:rows battery)))
                 (assoc-in battery [:rows 0 :row/run-id] "foreign")
                 (assoc-in battery [:rows 0 :row/verdict] :red)
                 (assoc battery :series-sha256 (apply str (repeat 64 "b")))]]
      (is (false? (sut/validate bad "RUN4" sha sha [bundle]))))))

(deftest unmapped-route-produces-red-not-acceptance
  (let [bad (assoc-in bundle [:run-record :route 0 :toNode] "R99")
        battery (sut/produce "RUN4" sha sha [bad] control)]
    (is (some #(= :red (:row/verdict %)) (:rows battery)))
    (is (false? (sut/validate battery "RUN4" sha sha [bad])))))
