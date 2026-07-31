(ns futon3c.peripheral.shared-memory-contract-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.memory-contract :as memory-contract]
            [futon3c.peripheral.memory-lifecycle :as memory-lifecycle]))

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
    (is (= :offered-selected-witnessed
           (:memory-outcome-triple/type
            (memory-contract/witnessed-memory-outcome-triple
             (assoc wm-projection
                    :wm-projection/projection-selected-ids ["e-wm-1"])
             (memory-contract/decision-keyed-external-check-entry
              {:evidence-id "e-wm-contract-check"
               :decision-id "wm-shadow-decision-1"
               :domain :war-machine
               :author "wm/test-independent-checker"
               :session-id "wm-test-check-session"
               :at "2026-07-31T09:10:00Z"
               :outcome :pass
               :witness-status :independently-witnessed
               :checker "test-only shared-contract checker"})))))))

(deftest non-wm-agent-attribution-writes-and-joins-a-witness
  (let [{:keys [math-receipt]} (fixtures)
        attribution
        (memory-contract/use-receipt (dissoc math-receipt :outcome-id))
        store (atom {:entries {} :order []})
        check-request
        {:evidence-id "e-zai-compiler-check"
         :decision-id "math-decision-1"
         :author "lean/test-compiler"
         :session-id "zai-check-session"
         :at "2026-07-31T10:05:00Z"
         :outcome :pass
         :witness-status :independently-witnessed
         :checker "test-only Lean compiler witness"}
        _ (memory-lifecycle/record-decision-keyed-external-check!
           {:evidence-store store :domain :mathematics}
           check-request)
        check (get-in @store [:entries "e-zai-compiler-check"])
        triple
        (memory-contract/witnessed-memory-outcome-triple attribution check)]
    (is (= [:mathematics :external-check] (:evidence/tags check)))
    (is (= {:memory-outcome-triple/type :offered-selected-witnessed
            :memory-outcome-triple/selection-signal :agent-attribution
            :memory-outcome-triple/decision-id "math-decision-1"
            :memory-outcome-triple/domain :mathematics
            :memory-outcome-triple/offered-ids ["e-math-1"]
            :memory-outcome-triple/selected-ids ["e-math-1"]
            :memory-outcome-triple/witness-evidence-id
            "e-zai-compiler-check"
            :memory-outcome-triple/witness-status
            :independently-witnessed
            :memory-outcome-triple/outcome :pass
            :memory-outcome-triple/checker "lean/test-compiler"}
           triple))))
