(ns futon3c.peripheral.shared-memory-contract-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.memory-contract :as memory-contract]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(defn- fixtures []
  (-> "fixtures/shared_memory_contract_fixtures.edn"
      io/resource
      slurp
      edn/read-string))

(deftest futon3c-consumes-the-shared-dual-domain-contract
  (let [{:keys [mathematics war-machine math-receipt
                wm-projection-receipt]} (fixtures)
        math-memory (memory-contract/compact-memory mathematics)
        wm-memory (memory-contract/compact-memory war-machine)
        math-use (memory-contract/use-receipt math-receipt)
        wm-projection
        (memory-contract/wm-projection-receipt wm-projection-receipt)]
    (is (= ["e-math-1" "e-wm-1"]
           (mapv :memory/id [math-memory wm-memory])))
    (is (= [:mathematics :war-machine]
           (mapv :memory/domain [math-memory wm-memory])))
    (is (= :mathematics (:memory-use/domain math-use)))
    (is (= :war-machine (:wm-projection/domain wm-projection)))
    (is (= :agent-attribution (:memory-use/signal math-use)))
    (is (= :algorithmic-selection
           (:wm-projection/signal wm-projection)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (memory-contract/agent-attribution-corpus
                  [math-use wm-projection])))
    (is (not-any? #(= "used-ids" (name %))
                  (keys wm-projection)))
    (is (= :offered-projection-selected-witnessed
           (:wm-outcome-triple/type
            (wm-memory/witnessed-projection-triple
             (assoc wm-projection
                    :wm-projection/projection-selected-ids ["e-wm-1"])
             (wm-memory/decision-keyed-external-check-entry
              {:evidence-id "e-wm-contract-check"
               :decision-id "wm-shadow-decision-1"
               :author "wm/test-independent-checker"
               :session-id "wm-test-check-session"
               :at "2026-07-31T09:10:00Z"
               :outcome :pass
               :witness-status :independently-witnessed
               :checker "test-only shared-contract checker"})))))))
