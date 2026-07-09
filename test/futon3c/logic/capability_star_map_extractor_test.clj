(ns futon3c.logic.capability-star-map-extractor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.capability-star-map-extractor :as extractor]))

(def fixture-missions
  [{:id "M-agency-unified-routing" :path "/home/joe/code/futon3c/holes/missions/M-agency-unified-routing.md" :status-class :complete :open-hole-count 1}
   {:id "M-drawbridge-multi-agent" :path "/home/joe/code/futon3c/holes/missions/M-drawbridge-multi-agent.md" :status-class :complete :open-hole-count 2}
   {:id "M-agency-rebuild" :path "/home/joe/code/futon3c/holes/missions/M-agency-rebuild.md" :status-class :complete :open-hole-count 3}
   {:id "M-labs-integration" :path "/home/joe/code/futon3c/holes/missions/M-labs-integration.md" :status-class :complete :open-hole-count 4}
   {:id "M-self-representing-stack" :path "/home/joe/code/futon3c/holes/missions/M-self-representing-stack.md" :status-class :complete :open-hole-count 5}
   {:id "M-three-column-stack" :path "/home/joe/code/futon3c/holes/missions/M-three-column-stack.md" :status-class :complete :open-hole-count 6}
   {:id "M-live-geometric-stack" :path "/home/joe/code/futon3c/holes/missions/M-live-geometric-stack.md" :status-class :complete :open-hole-count 7}
   {:id "M-war-machine-pilot" :path "/home/joe/code/futon3c/holes/missions/M-war-machine-pilot.md" :status-class :open :open-hole-count 8}
   {:id "M-war-machine-first-outing" :path "/home/joe/code/futon3c/holes/missions/M-war-machine-first-outing.md" :status-class :complete :open-hole-count 9}
   {:id "M-capability-star-map" :path "/home/joe/code/futon0/holes/missions/M-capability-star-map.md" :status-class :active :open-hole-count 10}])

(deftest wm-region-graph-verifies-and-reproduces-keystone
  (let [graph (extractor/build-graph {:missions fixture-missions
                                      :structural-holes? false})
        verify (extractor/run-verify-equivalent graph)
        keystone (extractor/keystone-path-report graph)]
    (is (:verified? verify) (pr-str (:violations verify)))
    (is (= [:efe-trustworthy-over-starmap] (:held keystone)))
    (is (:single-held-substantive-node? keystone))
    (is (= #{:agency
             :evidence-persistence
             :self-representing-stack
             :live-geometric-stack
             :war-machine
             :wm-steps-forward-guardrailed
             :efe-trustworthy-over-starmap
             :wm-overnight-unsupervised}
           (set (keys (:capabilities graph)))))))

(deftest held-ascent-capabilities-carry-pre-witness-candidates
  (let [graph (extractor/build-graph {:missions fixture-missions
                                      :structural-holes? false})
        efe-witness (get-in graph [:capabilities
                                   :efe-trustworthy-over-starmap
                                   :pre-witness
                                   0])
        overnight-witness (get-in graph [:capabilities
                                         :wm-overnight-unsupervised
                                         :pre-witness
                                         0])]
    (is (= :wm-ranks-applicable-single-cycle-leaf (:id efe-witness)))
    (is (= :efe-trustworthy-over-starmap (:attests efe-witness)))
    (is (= :candidate (:status efe-witness)))
    (is (seq (:success-criteria efe-witness)))
    (is (= :guarded-overnight-wm-run (:id overnight-witness)))
    (is (= :wm-overnight-unsupervised (:attests overnight-witness)))
    (is (= :candidate (:status overnight-witness)))
    (is (= [:wm-steps-forward-guardrailed :efe-trustworthy-over-starmap]
           (:requires overnight-witness)))
    (is (seq (:hard-gates overnight-witness)))))

(deftest mission-mapping-distinguishes-real-missions-from-non-mission-builders
  (let [graph (extractor/build-graph {:missions fixture-missions
                                      :structural-holes? false})
        missions (:missions graph)
        real (filter (comp true? :real-mission? val) missions)
        builders (filter (comp false? :real-mission? val) missions)]
    (is (= 10 (count real)))
    (is (= 4 (count builders)))
    (is (= 8 (get-in missions ["M-war-machine-pilot" :open-hole-count])))
    (is (= :open (get-in missions ["M-war-machine-pilot" :status])))
    (is (= {:builder "guardrails core"
            :built-under "WM-GUARDRAILS-SPEC"
            :real-mission? false}
           (select-keys (get missions "builder/wm-guardrails-core")
                        [:builder :built-under :real-mission?])))
    (is (empty? (filter #(re-matches #"M-wm.*|M-war-machine-(input-sources|hole-counter|gate-runner)" %)
                        (keys missions))))))

(deftest structural-hole-count-is-separate-from-lexical-open-hole-count
  (let [graph (extractor/build-graph {:missions fixture-missions
                                      :structural-hole-fn (fn [mission-id]
                                                           (when (= "M-war-machine-pilot" mission-id)
                                                             {:structural-hole-count 7}))})
        mission (get-in graph [:missions "M-war-machine-pilot"])]
    (is (= 8 (:open-hole-count mission))
        "lexical open-hole-count stays unchanged")
    (is (= 7 (:structural-hole-count mission)))))

(deftest structural-hole-report-counts-ghost-vacuous-and-open-question-holes
  (let [frame (fn [id binder sub-count]
                {:id id
                 :type "scope/frame"
                 :props {:scope/id id
                         :scope/binder binder
                         :fold/sub-count sub-count}})
        report (extractor/structural-hole-report-from-frames
                [(frame "demo/head" "eightfold-phase" 3)
                 (frame "demo/identify" "eightfold-phase" 2)
                 (frame "demo/map" "eightfold-phase" 0)
                 (frame "demo/open-questions" "loose-section" 4)])]
    (is (= ["head" "identify"] (:phase/written report)))
    (is (= ["map"] (:phase/vacuous report)))
    (is (= ["derive" "argue" "verify" "instantiate" "document"]
           (:phase/ghost report)))
    (is (= 1 (:loose/open-question-count report)))
    (is (= 7 (:structural-hole-count report)))))

(deftest toposort-rejects-requires-cycle
  (let [graph {:capabilities {:a {} :b {}}
               :edges [{:from :a :to :b :type :requires}
                       {:from :b :to :a :type :requires}]}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"cycle in :requires graph"
                          (extractor/requires-toposort graph)))))

(deftest contaminated-mission-paths-are-excluded
  (testing "worktrees, futon3 origin material, and .state are not graph inputs"
    (let [missions [{:id "M-good" :path "/home/joe/code/futon4/holes/missions/M-good.md"}
                    {:id "M-state" :path "/home/joe/code/futon4/.state/x/holes/missions/M-state.md"}
                    {:id "M-worktree" :path "/home/joe/code/futon4/worktrees/x/holes/missions/M-worktree.md"}
                    {:id "M-origin" :path "/home/joe/code/futon3/origin/holes/missions/M-origin.md"}]]
      (is (= ["M-good"] (mapv :id (extractor/clean-missions missions)))))))
