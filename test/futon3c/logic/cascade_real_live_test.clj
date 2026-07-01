(ns futon3c.logic.cascade-real-live-test
  "Live-gate tests for C-cascade-real RUN/DELIVER. The HTTP fetch is exercised live
   over Drawbridge; here we pin the pure extractor and prove the cross-dimension
   composition gate bites on REAL-shaped node-ids (so a bad future car is caught)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.cascade-real :as cr]
            [futon3c.logic.cascade-real-live :as live]))

(def ^:private sample-clock-edges
  ;; as the substrate-2 query returns them — endpoints are now CANONICAL node-ids
  [{:hx/type :clock/clocked-on
    :hx/endpoints ["agent:claude-4" "futon3c-d/mission/autoclock-in"]
    :hx/props {:agent-id "claude-4" :mission-id "M-autoclock-in"}}
   {:hx/type :clock/clocked-on
    :hx/endpoints ["agent:claude-4" "campaign:C-cascade-real"]
    :hx/props {"agent-id" "claude-4" "campaign-id" "C-cascade-real"}}])

(deftest o3-extractor-keys-on-canonical-endpoints
  (testing "clock edges → claims-typeo on the CANONICAL node-ids the lineage writes"
    (let [claims (live/o3-claims-from sample-clock-edges)]
      (is (some #{[cr/claims-typeo :O3 "futon3c-d/mission/autoclock-in" :mission]} claims)
          "the canonical mission node is claimed (shares its id with O1/D4)")
      (is (some #{[cr/claims-typeo :O3 "campaign:C-cascade-real" :campaign]} claims))
      (is (some #{[cr/claims-typeo :O3 "agent:claude-4" :agent]} claims))
      (is (every? #(= cr/claims-typeo (first %)) claims) "only claims-typeo facts"))))

(deftest o1-extractor-maps-mined-moves
  (testing "mined-move edges → claims-typeo :O1 on the canonical HAVE mission node (:mission)"
    (let [edges  [{:hx/type :code/v05/mined-move
                   :hx/endpoints ["futon3c-d/mission/autoclock-in" "futon3c-d/mission/autoclock-in-head"]}]
          claims (live/o1-mined-move-claims-from edges)]
      (is (= [[cr/claims-typeo :O1 "futon3c-d/mission/autoclock-in" :mission]] claims)
          "claims the have (mission) node :mission; the -head want node is skipped"))))

(deftest o4-extractor-maps-clusters
  (testing "cascade/cluster-member edges → O4 claims the mission node :mission + cluster :cluster"
    (let [edges  [{:hx/type :cascade/cluster-member
                   :hx/endpoints ["cascade/cluster/operator-loops" "futon3c-d/mission/autoclock-in"]}]
          claims (live/o4-cluster-claims-from edges)]
      (is (some #{[cr/claims-typeo :O4 "cascade/cluster/operator-loops" :cluster]} claims))
      (is (some #{[cr/claims-typeo :O4 "futon3c-d/mission/autoclock-in" :mission]} claims)
          "claims the canonical mission node :mission (the shared spine with O1/O3)"))))

(deftest o4-o3-o1-compose-on-the-shared-mission-node
  (testing "O4 cluster, O1 arrow, O3 lineage all claim the SAME mission node :mission → clean compose"
    (let [o3 (live/o3-claims-from sample-clock-edges)
          o1 (live/o1-mined-move-claims-from
              [{:hx/endpoints ["futon3c-d/mission/autoclock-in" "futon3c-d/mission/autoclock-in-head"]}])
          o4 (live/o4-cluster-claims-from
              [{:hx/endpoints ["cascade/cluster/x" "futon3c-d/mission/autoclock-in"]}])
          v  (cr/verify (cr/db-from-data (concat o3 o1 o4)))]
      (is (= [] (:composition-violations v))
          "three dimensions agree the mission node is :mission — full-spine compose, no conflict"))))

(deftest o1-o3-compose-on-the-shared-mission-node
  (testing "D4 arrows × O3 lineage both claim the SAME canonical mission node :mission → non-vacuous, clean"
    (let [o3 (live/o3-claims-from sample-clock-edges)
          o1 (live/o1-mined-move-claims-from
              [{:hx/endpoints ["futon3c-d/mission/autoclock-in" "futon3c-d/mission/autoclock-in-head"]}])
          v  (cr/verify (cr/db-from-data (concat o3 o1)))]
      (is (= [] (:composition-violations v))
          "O1 and O3 agree the mission node is :mission — the gate's first real, non-vacuous compose"))))

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
    (is (= [] (vec (live/o3-claims-from []))))
    (is (= [] (vec (live/o2-meme-claims-from []))))))

(deftest o2-extractor-maps-memes
  (testing "mine/meme edges → claims-typeo :O2 meme:ask-* :meme (only meme: endpoints)"
    (let [edges  [{:hx/type :mine/meme :hx/endpoints ["meme:ask-abc123"] :hx/props {}}
                  {:hx/type :mine/meme :hx/endpoints ["meme:ask-def456" "concept:x"] :hx/props {}}]
          claims (live/o2-meme-claims-from edges)]
      (is (some #{[cr/claims-typeo :O2 "meme:ask-abc123" :meme]} claims))
      (is (some #{[cr/claims-typeo :O2 "meme:ask-def456" :meme]} claims))
      (is (= 2 (count claims)) "concept: endpoint not claimed in the first car"))))

(deftest o2-o3-compose-disjoint
  (testing "O2 memes + O3 missions are disjoint node-ids → compose cleanly"
    (let [v (cr/verify (cr/db-from-data [[cr/claims-typeo :O3 "mission:M-x" :mission]
                                         [cr/claims-typeo :O2 "meme:ask-1" :meme]]))]
      (is (= [] (:composition-violations v))))))

(deftest concept-index-collision-would-bite
  (testing "WHY claude-1 defers concept-index: a mission node claimed as :meme by O2 is caught"
    (let [v (cr/verify (cr/db-from-data [[cr/claims-typeo :O3 "mission:M-x" :mission]
                                         [cr/claims-typeo :O2 "mission:M-x" :meme]]))]
      (is (some #{"mission:M-x"} (:composition-violations v)))
      (is (false? (:consistent? v))))))

;; --- §7 DISSOLUTION Checklist B: the per-section BODY structure -------------

(deftest lineage-section-agent-to-target
  (testing "clock edges → agent→target rows, most-recent-first, mission OR campaign target"
    (let [rows (live/lineage-section
                [{:hx/endpoints ["agent:claude-4" "futon3c-d/mission/autoclock-in"]
                  :hx/props {:agent-id "claude-4" :session-id "s1" :clocked-at-ms 100}}
                 {:hx/endpoints ["agent:claude-1" "campaign:C-cascade-real"]
                  :hx/props {:agent-id "claude-1" :session-id "s2" :clocked-at-ms 200}}])]
      (is (= {:agent "agent:claude-1" :target "campaign:C-cascade-real" :session "s2" :at 200}
             (first rows)) "most-recent (at=200) first; target is the non-agent endpoint")
      (is (= "futon3c-d/mission/autoclock-in" (:target (second rows)))))))

(deftest cluster-section-cluster-to-mission
  (testing "cluster-member edges → cluster→mission rows"
    (is (= [{:cluster "cascade/cluster/00-war-machine"
             :mission "futon3c-d/mission/war-machine-first-outing"}]
           (live/cluster-section
            [{:hx/endpoints ["cascade/cluster/00-war-machine"
                             "futon3c-d/mission/war-machine-first-outing"]}])))))

(deftest hole-section-carries-kind
  (testing "hole-target edges → hole→target rows with the hole kind (the honest hole)"
    (is (= [{:hole "cascade/hole/capability-layer-not-canonical"
             :target "futon0-d/mission/capability-star-map"
             :kind "capability-not-canonical"}]
           (live/hole-section
            [{:hx/endpoints ["cascade/hole/capability-layer-not-canonical"
                             "futon0-d/mission/capability-star-map"]
              :hx/props {:hole-kind "capability-not-canonical" :composes true}}])))))

(deftest arrow-section-keeps-move-honesty
  (testing "mined-move edges → have→want rows carrying move-class + ΔG (self-loop visible)"
    (let [rows (live/arrow-section
                [{:hx/endpoints ["futon0-d/mission/capability-star-map"
                                 "futon0-d/mission/capability-star-map-document"]
                  :hx/props {:move-class ":close-hole" :delta-g -7.91E-4}}
                 {:hx/endpoints ["futon3c-d/mission/x-head" "y"] :hx/props {}}])]
      (is (= 1 (count rows)) "the -head want-side stem is skipped, like the O1 extractor")
      (is (= ":close-hole" (:move-class (first rows))))
      (is (= "futon0-d/mission/capability-star-map" (:have (first rows)))))))

(deftest held-section-reads-namespaced-props
  (testing "held/on-mission edges → held→mission rows with namespaced :held/reason + registry"
    (is (= [{:held "held/item/prose/h1003283572"
             :mission "futon7-d/mission/self-documenting-stack"
             :registry "prose" :reason "scope unification"}]
           (live/held-section
            [{:hx/endpoints ["held/item/prose/h1003283572"
                             "futon7-d/mission/self-documenting-stack"]
              :hx/props {:held/disposition "held" :held/source-registry "prose"
                         :held/reason "scope unification"}}])))))

(deftest mission-pattern-section-crosslinks
  (testing "cascade/mission-pattern edges → mission→pattern rows (the reconstructed cited-pattern layer)"
    (is (= [{:mission "futon3-d/mission/agency-rebuild"
             :pattern "agency/single-routing-authority"
             :relation "applied" :cos nil}]
           (live/mission-pattern-section
            [{:hx/endpoints ["futon3-d/mission/agency-rebuild" "agency/single-routing-authority"]
              :hx/props {:relation "applied" :source "mission-pattern-scopes"}}])))
    (testing "candidate edges keep the cosine score"
      (is (= 0.31 (:cos (first (live/mission-pattern-section
                                [{:hx/endpoints ["futon2-d/mission/x" "realtime/y"]
                                  :hx/props {:relation "candidate" :cos 0.31}}]))))))))

(deftest sections-empty-on-no-rows
  (testing "every section degrades to [] with no live rows (honest non-landing)"
    (is (= [] (live/lineage-section []) (live/cluster-section [])
           (live/hole-section []) (live/arrow-section []) (live/held-section [])
           (live/mission-pattern-section [])))))
