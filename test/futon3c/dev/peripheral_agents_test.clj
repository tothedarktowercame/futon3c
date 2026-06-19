(ns futon3c.dev.peripheral-agents-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
            [futon3c.dev.peripheral-agents :as peripheral-agents]))

(deftest register-scribe-agent-registers-invoke-ready-scribe
  (testing "scribe-1 is a persistent write-capable Claude-backed agent"
    (reg/reset-registry!)
    (let [calls (atom [])
          invoke-fn (fn [_prompt _session-id] {:result "ok"})
          result (peripheral-agents/register-scribe-agent!
                  {:read-session-id (fn [_] "sid-scribe")
                   :make-claude-invoke-fn
                   (fn [opts]
                     (swap! calls conj opts)
                     invoke-fn)})
          agent (get-in (reg/registry-status) [:agents "scribe-1"])]
      (is (= invoke-fn (:invoke-fn result)))
      (is (= "sid-scribe" (:session-id result)))
      (is (= 1 (count @calls)))
      (is (= "scribe-1" (:agent-id (first @calls))))
      (is (= "/home/joe/code" (:cwd (first @calls))))
      (is (= :scribe (:type agent)))
      (is (= :idle (:status agent)))
      (is (true? (:invoke-ready? agent)))
      (is (= [:write :observe] (:capabilities agent)))
      (is (= "interest-network-scribe" (get-in agent [:metadata :role]))))))
