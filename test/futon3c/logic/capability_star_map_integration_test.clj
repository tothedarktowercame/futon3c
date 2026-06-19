(ns futon3c.logic.capability-star-map-integration-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.efe :as efe]
            [futon2.aif.mission-registry :as mission-registry]
            [futon3c.logic.capability-star-map-extractor :as extractor]
            [futon3c.wm.guardrails :as guardrails]))

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

(deftest real-expanded-graph-carries-held-ascent-pre-witnesses-test
  (let [graph (real-graph)]
    (is (= :wm-ranks-applicable-single-cycle-leaf
           (get-in graph [:capabilities
                          :efe-trustworthy-over-starmap
                          :pre-witness
                          0
                          :id])))
    (is (= :guarded-overnight-wm-run
           (get-in graph [:capabilities
                          :wm-overnight-unsupervised
                          :pre-witness
                          0
                          :id])))
    (is (= :candidate
           (get-in graph [:capabilities
                          :efe-trustworthy-over-starmap
                          :pre-witness
                          0
                          :status])))
    (is (= :candidate
           (get-in graph [:capabilities
                          :wm-overnight-unsupervised
                          :pre-witness
                          0
                          :status])))))

(def ^:private terminal-c3-candidate-docs
  [{:target "M-war-machine-pilot"
    :path ["." "holes" "missions" "M-war-machine-pilot.md"]
    :region :wm}
   {:target "M-capability-star-map"
    :path [".." "futon0" "holes" "missions" "M-capability-star-map.md"]
    :region :wm}
   {:target "M-webarxana"
    :path [".." "futon4" "holes" "missions" "M-webarxana.md"]
    :region :t5}
   {:target "M-essay-corpus-substrate"
    :path [".." "futon4" "holes" "missions" "M-essay-corpus-substrate.md"]
    :region :t5}
   {:target "M-arxana-roundtrip"
    :path [".." "futon4" "holes" "missions" "M-arxana-roundtrip.md"]
    :region :t5}
   {:target "M-stack-stereolithography"
    :path [".." "futon5a" "holes" "missions" "M-stack-stereolithography.md"]
    :region :t5}
   {:target "M-stack-geometry"
    :path [".." "futon5a" "holes" "missions" "M-stack-geometry.md"]
    :region :t5}
   {:target "M-hypergraph-operator"
    :path [".." "futon5a" "holes" "missions" "M-hypergraph-operator.md"]
    :region :t5}
   {:target "M-superpod-mark2"
    :path [".." "futon6" "holes" "missions" "M-superpod-mark2.md"]
    :region :t3}
   {:target "M-prior-mathematics"
    :path [".." "futon6" "holes" "missions" "M-prior-mathematics.md"]
    :region :t3}
   {:target "M-apm-solutions"
    :path ["." "holes" "missions" "M-apm-solutions.md"]
    :region :t3}
   {:target "M-futonzero-prelim-practice"
    :path [".." "futon0" "holes" "missions" "M-futonzero-prelim-practice.md"]
    :region :t3}
   {:target "M-differentiable-math"
    :path [".." "futon6" "holes" "missions" "M-differentiable-math.md"]
    :region :t3
    :survey-extra? true}
   {:target "M-diagramprover"
    :path ["." "holes" "missions" "M-diagramprover.md"]
    :region :t3
    :survey-extra? true}
   {:target "M-expressions-of-interest"
    :path [".." "futon5a" "holes" "missions" "M-expressions-of-interest.md"]
    :region :t2}
   {:target "M-buyer-discovery"
    :path [".." "futon5a" "holes" "missions" "M-buyer-discovery.md"]
    :region :t2}])

(defn- doc-file [parts]
  (apply io/file parts))

(defn- mission-entry-from-doc
  [{:keys [target path] :as spec}]
  (let [f (doc-file path)]
    (if (.isFile f)
      (assoc (#'mission-registry/mission-doc->entry (.getAbsolutePath f))
             :target target
             :region (:region spec)
             :survey-extra? (boolean (:survey-extra? spec)))
      (assoc spec
             :missing? true
             :absolute-path (.getAbsolutePath f)))))

(defn- terminal-c3-entries []
  (mapv mission-entry-from-doc terminal-c3-candidate-docs))

(defn- merge-terminal-candidate
  [graph {:keys [target status-class open-hole-count path status-line]}]
  (let [existing (get-in graph [:missions target])
        mission-node (merge {:scope []
                             :produces []
                             :real-mission? true
                             :next-exit-operator-verify? false}
                            existing
                            {:open-hole-count (long (or open-hole-count 0))
                             :phase status-class
                             :status status-class
                             :status-line status-line
                             :path path})]
    (assoc-in graph [:missions target] mission-node)))

(defn- terminal-c3-graph
  [base-graph entries]
  (reduce merge-terminal-candidate
          base-graph
          (remove :missing? entries)))

(defn- terminal-c3-report
  []
  (let [entries (terminal-c3-entries)
        missing (filterv :missing? entries)
        graph (terminal-c3-graph (real-graph) entries)
        actions (mapv (fn [{:keys [target]}]
                        {:type :open-mission :target target})
                      (remove :missing? entries))
        opts {:capability-graph graph
              :pre-registered-goal goal}
        ranked (efe/rank-star-map-actions base-state actions opts)
        top (first ranked)
        leaf-count (count (filter :graph/single-cycle-leaf? ranked))
        verdict (if (:graph/single-cycle-leaf? top) :pass :fail)]
    {:entries entries
     :missing missing
     :graph graph
     :ranked ranked
     :top top
     :leaf-count leaf-count
     :verdict verdict}))

(defn- live-mission-status?
  [{:keys [status-class missing?]}]
  (and (not missing?)
       (not (contains? #{:complete :inactive :draft} status-class))))

(defn- terminal-c3-guardrails-report
  []
  (let [entries (terminal-c3-entries)
        status-by-target (into {}
                               (for [{:keys [target open-hole-count] :as entry} entries]
                                 [target {:open? (live-mission-status? entry)
                                          :open-hole-count open-hole-count}]))
        ctx {:mission-status-fn (fn [target]
                                  (get status-by-target target
                                       {:open? false :open-hole-count 0}))}
        graph (terminal-c3-graph (real-graph) entries)
        raw-actions (mapv (fn [{:keys [target]}]
                            {:type :open-mission :target target})
                          (remove :missing? entries))
        filtered-actions (filterv #(guardrails/autonomous-admissible? % ctx)
                                  raw-actions)
        opts {:capability-graph graph
              :pre-registered-goal goal}
        ranked (efe/rank-star-map-actions base-state filtered-actions opts)
        top (first ranked)
        leaf-count (count (filter :graph/single-cycle-leaf? ranked))
        verdict (if (:graph/single-cycle-leaf? top) :pass :fail)]
    {:entries entries
     :status-by-target status-by-target
     :filtered-actions filtered-actions
     :graph graph
     :ranked ranked
     :top top
     :leaf-count leaf-count
     :verdict verdict}))

(defn- ranked-summary
  [ranked n]
  (mapv (fn [{:keys [action rank G-total G-risk G-ambiguity G-info
                     G-survival G-structural G-graph-pragmatic
                     G-applicability G-body-size G-ascent-progress
                     graph/applicable? graph/single-cycle-leaf?]}]
          {:rank rank
           :action action
           :G-total G-total
           :G-risk G-risk
           :G-ambiguity G-ambiguity
           :G-info G-info
           :G-survival G-survival
           :G-structural G-structural
           :G-graph-pragmatic G-graph-pragmatic
           :G-applicability G-applicability
           :G-body-size G-body-size
           :G-ascent-progress G-ascent-progress
           :applicable? applicable?
           :single-cycle-leaf? single-cycle-leaf?})
        (take n ranked)))

(deftest c3-real-graph-currently-fails-on-mega-mission-test
  (testing "C3 over the real WM-region graph: report the actual top action"
    (let [graph (real-graph)
          opts {:capability-graph graph
                :pre-registered-goal goal}
          wm-actions [{:type :open-mission :target "M-war-machine-pilot"}
                      {:type :open-mission :target "M-capability-star-map"}]
          ranked (efe/rank-star-map-actions base-state
                                             wm-actions
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

(deftest terminal-c3-expanded-curated-landscape-test
  (testing "Terminal C3 over the expanded curated survey set"
    ;; This is the expanded curated set surfaced by the WM/T5/T3/T2 grounding
    ;; surveys, not a full production mission-registry scan. Counts come from
    ;; futon2.aif.mission-registry's real per-document parser.
    (let [{:keys [entries missing ranked top leaf-count verdict graph]} (terminal-c3-report)
          top-action (:action top)
          candidate-count (count (remove :missing? entries))
          counts (mapv (fn [{:keys [target open-hole-count status-class missing?]}]
                         {:target target
                          :open-hole-count open-hole-count
                          :status status-class
                          :missing? (boolean missing?)})
                       entries)]
      (println "TERMINAL C3 expanded-curated counts:" counts)
      (println "TERMINAL C3 missing docs:" (mapv :target missing))
      (println "TERMINAL C3 leaf-count:" leaf-count "of" candidate-count)
      (println "TERMINAL C3 top-action:" top-action
               "applicable?" (:graph/applicable? top)
               "single-cycle-leaf?" (:graph/single-cycle-leaf? top)
               "G-total" (:G-total top))
      (println "TERMINAL C3 verdict:" verdict)
      (is (pos? candidate-count))
      (is (= "M-buyer-discovery" (:target (first missing))))
      (is (= leaf-count (count (filter #(efe/mission-single-cycle-leaf?
                                         graph
                                         (get-in % [:action :target]))
                                       ranked))))
      (is (= {:type :open-mission :target "M-arxana-roundtrip"} top-action))
      (is (= :fail verdict)
          "Terminal C3 FAIL is the honest current result: the top ranked expanded-landscape action is not an applicable single-cycle leaf."))))

(deftest terminal-c3-production-guardrails-filtered-landscape-test
  (testing "Terminal C3 with production guardrails filtering before EFE ranking"
    (let [{:keys [filtered-actions ranked top leaf-count verdict status-by-target]} (terminal-c3-guardrails-report)
          top-action (:action top)
          top-target (:target top-action)
          filtered-targets (mapv :target filtered-actions)
          top-count (get-in status-by-target [top-target :open-hole-count])
          top-breakdown (ranked-summary ranked 3)]
      (println "TERMINAL C3 corrected filtered candidates:" filtered-targets)
      (println "TERMINAL C3 corrected leaf-count:" leaf-count
               "of" (count filtered-actions))
      (println "TERMINAL C3 corrected top-count:" top-count)
      (println "TERMINAL C3 corrected top-3:" top-breakdown)
      (println "TERMINAL C3 corrected verdict:" verdict)
      (is (= ["M-war-machine-pilot"
              "M-capability-star-map"
              "M-webarxana"
              "M-essay-corpus-substrate"
              "M-stack-stereolithography"
              "M-stack-geometry"
              "M-hypergraph-operator"
              "M-expressions-of-interest"]
             filtered-targets))
      (is (= leaf-count
             (count (filter :graph/single-cycle-leaf? ranked))))
      (is (pos? leaf-count))
      (is (= {:type :open-mission :target "M-essay-corpus-substrate"} top-action))
      (is (= 1 top-count))
      (is (true? (:graph/single-cycle-leaf? top)))
      (is (= :pass verdict)
          "Corrected production-faithful Terminal C3 PASS: guardrails remove 0-hole non-actions before EFE ranking."))))

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
