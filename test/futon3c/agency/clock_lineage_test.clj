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
