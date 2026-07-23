(ns futon3c.peripheral.shared-memory-contract-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.memory-contract :as memory-contract]))

(defn- fixtures []
  (-> "fixtures/shared_memory_contract_fixtures.edn"
      io/resource
      slurp
      edn/read-string))

(deftest futon3c-consumes-the-shared-dual-domain-contract
  (let [{:keys [mathematics war-machine math-receipt wm-receipt]} (fixtures)
        math-memory (memory-contract/compact-memory mathematics)
        wm-memory (memory-contract/compact-memory war-machine)
        math-use (memory-contract/use-receipt math-receipt)
        wm-use (memory-contract/use-receipt wm-receipt)]
    (is (= ["e-math-1" "e-wm-1"]
           (mapv :memory/id [math-memory wm-memory])))
    (is (= [:mathematics :war-machine]
           (mapv :memory/domain [math-memory wm-memory])))
    (is (= [:mathematics :war-machine]
           (mapv :memory-use/domain [math-use wm-use])))
    (is (= [:outcome-attached :pending-outcome]
           (mapv :memory-use/status [math-use wm-use])))))
