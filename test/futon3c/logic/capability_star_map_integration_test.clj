(ns futon3c.logic.capability-star-map-integration-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.efe :as efe]
            [futon3c.logic.capability-star-map-extractor :as extractor]))

(def ^:private graph-path
  (io/file ".." "futon0" "holes" "missions"
           "M-capability-star-map.graph.edn"))

(def ^:private goal :wm-overnight-unsupervised)

(def ^:private base-state
  {:observation {:loop-health 0.9
                 :support-coverage 0.9
                 :attack-coverage 0.9
                 :mission-health 0.7
                 :stack-pct 0.20
                 :consulting-pct 0.25
                 :portfolio-pct 0.25
                 :mathematics-pct 0.20
                 :active-repo-ratio 0.8
                 :sorry-count-norm 0.1
                 :coupling-density 0.2
                 :ticks-firing-ratio 0.0
                 :depositing-signal 0.1}
   :belief {}})

(defn- real-graph []
  (edn/read-string (slurp graph-path)))

(defn- candidate-actions
  "Actions EFE can rank without operator consent: real mission nodes that are
   not already complete. Builder placeholders and complete producers are data
   evidence, not live actions."
  [graph]
  (->> (:missions graph)
       (remove (fn [[_ mission]]
                 (or (= :complete (:status mission))
                     (false? (:real-mission? mission)))))
       (mapv (fn [[mission-id _]]
               {:type :open-mission :target mission-id}))))

(deftest c3-real-graph-currently-fails-on-mega-mission-test
  (testing "C3 over the real WM-region graph: report the actual top action"
    (let [graph (real-graph)
          opts {:capability-graph graph
                :pre-registered-goal goal}
          ranked (efe/rank-star-map-actions base-state
                                             (candidate-actions graph)
                                             opts)
          top (first ranked)
          top-action (:action top)
          top-mission (get-in graph [:missions (:target top-action)])]
      (is (= {:type :open-mission :target "M-war-machine-pilot"} top-action))
      (is (true? (:graph/applicable? top)))
      (is (false? (:graph/single-cycle-leaf? top)))
      (is (= 9 (:open-hole-count top-mission)))
      (is (false? (efe/mission-single-cycle-leaf? graph (:target top-action)))
          "C3 is currently FAIL: the top action is applicable, but it is not a one-cycle leaf."))))

(deftest inv-g-refuses-unregistered-pursuit-and-goal-extension-test
  (testing "INV-G rejects pursuit outside the brief and goal-extending decompose"
    (let [graph (real-graph)
          opts {:capability-graph graph
                :pre-registered-goal goal}
          pentagon {:type :pursue :target :cap/pentagon}
          extending-decompose {:type :decompose
                               :target "M-capability-star-map"
                               :extends-goal? true}
          ranked (efe/rank-star-map-actions base-state
                                             [pentagon extending-decompose]
                                             opts)]
      (is (false? (efe/safe-action? graph goal pentagon)))
      (is (false? (efe/safe-action? graph goal extending-decompose)))
      (is (empty? ranked)))))

(deftest keystone-path-has-single-held-substantive-node-test
  (testing "The frontier's transitive scope has only the keystone held"
    (let [graph (real-graph)
          verify (extractor/run-verify-equivalent graph)
          report (extractor/keystone-path-report graph)]
      (is (true? (:verified? verify)))
      (is (= [:efe-trustworthy-over-starmap] (:held report)))
      (is (true? (:single-held-substantive-node? report))))))
