(ns futon3c.apm.generated-contract-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.generated-contract :as sut]))

(def generated-path
  "holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v3.json")

(def clojure-contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")))

(deftest lean-emitted-contract-round-trips-through-clojure
  (let [result (sut/validate-round-trip generated-path clojure-contract)]
    (is (:ok result) (pr-str result))
    (is (= "promote-solver"
           (get-in result [:contract :transitions 2 :to])))))

(deftest complete-cycle-is-non-vacuous
  (let [contract (:contract (sut/read-contract generated-path))]
    (is (= 11 (count (:phase-order contract))))
    (is (= "preflight" (first (:phase-order contract))))
    (is (= "close-frame" (last (:phase-order contract))))
    (is (nil? (get-in contract [:transitions 10 :to])))))

(deftest missing-promotion-mutation-is-killed
  (let [contract (:contract (sut/read-contract generated-path))
        mutated (-> contract
                    (update :phase-order #(vec (remove #{"promote-solver"} %)))
                    (update :transitions #(vec (remove
                                                (fn [edge]
                                                  (or (= "promote-solver" (:from edge))
                                                      (= "promote-solver" (:to edge))))
                                                %))))
        result (sut/validate mutated)]
    (is (false? (:ok result)))
    (is (some #{:generated-contract-verify-bypasses-promotion}
              (:findings result)))))
