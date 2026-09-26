(ns futon3c.agency.clock-lineage-test
  "D1/O3 durable-lineage unit tests (C-cascade-real RUN/DELIVER). The substrate
   round-trip (persist! → reconstitute) is exercised live over Drawbridge; here
   we pin the two pure cores: the single-active target precedence and the
   reconstitution aggregation."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.clock-lineage :as cl]))

(deftest target-kind+id-precedence
  (testing "most-specific target wins (excursion > mission > campaign), matching the clock label"
    (is (= [:excursion "E-x"] (cl/target-kind+id {:campaign-id "C-c" :mission-id "M-m" :excursion-id "E-x"})))
    (is (= [:mission "M-m"]   (cl/target-kind+id {:campaign-id "C-c" :mission-id "M-m"})))
    (is (= [:campaign "C-c"]  (cl/target-kind+id {:campaign-id "C-c"})))
    (is (nil? (cl/target-kind+id {:campaign-id nil :mission-id nil :excursion-id nil}))
        "a clock with no target has no endpoint (no-op for persist)")))

(deftest canonical-endpoint-never-mints-an-island
  (testing "a mission with no doc / no canonical node resolves to nil — Clause 3 (don't write non-canonical)"
    (is (nil? (cl/canonical-endpoint :mission "M-totally-bogus-nonexistent-zzz")))
    (is (nil? (cl/canonical-endpoint :mission "no-prefix-so-skip"))
        "an id without the K- prefix is skipped (not a real clock target)")))

(defn- edge
  "A minimal clock/clocked-on hyperedge as the futon1a query returns it."
  [agent mission session at]
  {:hx/type :clock/clocked-on
   :hx/endpoints [(str "agent:" agent) (str "mission:" mission)]
   :hx/props {"agent-id" agent "mission-id" mission "session-id" session "clocked-at-ms" at}})

(deftest summarize-groups-by-target
  (testing "who/which sessions are on each mission, deduped, most-recent-first"
    (let [edges [(edge "claude-1" "M-a" "s1" 100)
                 (edge "claude-2" "M-a" "s2" 300)   ; two agents on M-a
                 (edge "claude-4" "M-b" "s4" 200)
                 (edge "claude-1" "M-a" "s1" 100)]  ; duplicate — must dedupe
          out (cl/summarize-edges edges)]
      (is (= ["M-a" "M-b"] (mapv :target out))
          "ordered by last-clock-ms desc (M-a's 300 beats M-b's 200)")
      (let [m-a (first (filter #(= "M-a" (:target %)) out))]
        (is (= #{"claude-1" "claude-2"} (set (:agents m-a))) "both agents on M-a")
        (is (= #{"s1" "s2"} (set (:sessions m-a))) "sessions deduped")
        (is (= 300 (:last-clock-ms m-a)) "last-clock-ms is the max")))))

(deftest summarize-handles-string-and-keyword-props
  (testing "reads string-keyed props (as futon1a returns) and skips targetless edges"
    (let [edges [(edge "claude-1" "M-a" "s1" 100)
                 {:hx/type :clock/clocked-on :hx/props {"agent-id" "claude-9"}}]] ; no target
      (is (= 1 (count (cl/summarize-edges edges))) "the targetless edge is dropped"))))

(deftest summarize-empty
  (testing "no edges → empty missions"
    (is (= [] (cl/summarize-edges [])))))

(deftest summarize-surfaces-canonical-endpoint
  (testing ":canonical is the non-agent endpoint — the held-work join key"
    (let [out (cl/summarize-edges [(edge "claude-1" "M-a" "s1" 100)])]
      (is (= "mission:M-a" (:canonical (first out)))
          "the canonical node is the endpoint that is not agent:*"))))

(defn- held-edge
  "A minimal held/on-mission hyperedge as futon1a returns it."
  [item-id mission disposition reason]
  {:hx/type :held/on-mission
   :hx/endpoints [(str "held/item/" item-id) mission]
   :hx/props {:held/disposition disposition :held/reason reason}})

(deftest held-by-mission-groups-on-canonical
  (testing "held items group by their canonical mission endpoint (Exit-criterion-2 join)"
    (let [held (cl/held-by-mission
                [(held-edge "prose/h1" "futon4-d/mission/writing-ethics" "held" "scope r1")
                 (held-edge "prose/h2" "futon4-d/mission/writing-ethics" "held" "scope r2")
                 (held-edge "prose/h3" "futon7-d/mission/self-documenting-stack" "held" "r3")])]
      (is (= #{"futon4-d/mission/writing-ethics"
               "futon7-d/mission/self-documenting-stack"} (set (keys held))))
      (is (= 2 (count (get held "futon4-d/mission/writing-ethics"))) "two items on writing-ethics")
      (is (= "held/item/prose/h1" (:item (first (get held "futon4-d/mission/writing-ethics"))))
          "item endpoint preserved")
      (is (= "scope r1" (:reason (first (get held "futon4-d/mission/writing-ethics"))))))
    (is (= {} (cl/held-by-mission [])) "no held edges → empty map")))
