(ns futon3c.logic.capability-star-map-extractor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.capability-star-map-extractor :as extractor]))

(deftest wm-region-graph-verifies-and-reproduces-keystone
  (let [graph (extractor/build-graph {:missions []})
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
                    {:id "M-origin" :path "/home/joe/code/futon3/holes/missions/M-origin.md"}]]
      (is (= ["M-good"] (mapv :id (extractor/clean-missions missions)))))))
