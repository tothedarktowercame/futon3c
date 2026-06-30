(ns futon3c.logic.cascade-real-live-test
  "Live-gate tests for C-cascade-real RUN/DELIVER. The HTTP fetch is exercised live
   over Drawbridge; here we pin the pure extractor and prove the cross-dimension
   composition gate bites on REAL-shaped node-ids (so a bad future car is caught)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.cascade-real :as cr]
            [futon3c.logic.cascade-real-live :as live]))

(def ^:private sample-clock-edges
  ;; as the substrate-2 query returns them — keyword AND string prop keys
  [{:hx/type :clock/clocked-on
    :hx/endpoints ["agent:claude-4" "mission:M-autoclock-in"]
    :hx/props {:agent-id "claude-4" :mission-id "M-autoclock-in"}}
   {:hx/type :clock/clocked-on
    :hx/endpoints ["agent:claude-4" "campaign:C-cascade-real"]
    :hx/props {"agent-id" "claude-4" "campaign-id" "C-cascade-real"}}])

(deftest o3-extractor-maps-lineage-to-claims
  (testing "clock edges → claims-typeo for mission/campaign/agent nodes (kw + str keys)"
    (let [claims (live/o3-claims-from sample-clock-edges)]
      (is (some #{[cr/claims-typeo :O3 "mission:M-autoclock-in" :mission]} claims))
      (is (some #{[cr/claims-typeo :O3 "campaign:C-cascade-real" :campaign]} claims))
      (is (some #{[cr/claims-typeo :O3 "agent:claude-4" :agent]} claims))
      (is (every? #(= cr/claims-typeo (first %)) claims) "only claims-typeo facts"))))

(deftest gate-bites-cross-dimension-conflict
  (testing "a 2nd dimension's LIVE claim that types a shared real node differently is CAUGHT"
    ;; O3 says mission:M-x is :mission; a hypothetical O4 car lands claiming it :pattern
    (let [o3  [cr/claims-typeo :O3 "mission:M-x" :mission]
          bad [cr/claims-typeo :O4 "mission:M-x" :pattern]
          v   (cr/verify (cr/db-from-data [o3 bad]))]
      (is (some #{"mission:M-x"} (:composition-violations v))
          "the shared-node type conflict is detected over real-shaped node-ids")
      (is (false? (:consistent? v))))))

(deftest gate-clean-when-consistent
  (testing "two dimensions agreeing on a shared node's type compose cleanly"
    (let [o3 [cr/claims-typeo :O3 "mission:M-x" :mission]
          o4 [cr/claims-typeo :O4 "mission:M-x" :mission]
          v  (cr/verify (cr/db-from-data [o3 o4]))]
      (is (= [] (:composition-violations v))))))

(deftest empty-edges-no-claims
  (testing "a dimension with no live rows contributes nothing (honest non-landing)"
    (is (= [] (vec (live/o3-claims-from []))))))
