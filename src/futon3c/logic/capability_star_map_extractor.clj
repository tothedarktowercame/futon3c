(ns futon3c.logic.capability-star-map-extractor
  "Unit A extractor for M-capability-star-map's WM-region slice.

  The first slice is intentionally bounded: it starts from the hand-authored
  ensemble-1 capability set, enriches it from the live mission registry and the
  pudding-prover registry, emits the bipartite mission/capability graph EDN, and
  adapts that real graph back into the existing invariant queries."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon2.aif.mission-registry :as mission-registry]
            [futon3c.logic.capability-star-map-invariants :as inv]))

(def default-ensemble-path
  "/home/joe/code/futon0/holes/missions/M-capability-star-map.ensemble.edn")

(def default-output-path
  "/home/joe/code/futon0/holes/missions/M-capability-star-map.graph.edn")

(def default-pudding-registry-path
  "/home/joe/code/futon7/holes/pudding-prover-registry.edn")

(def ^:private wm-region-capability-order
  [:agency
   :evidence-persistence
   :self-representing-stack
   :live-geometric-stack
   :war-machine
   :wm-steps-forward-guardrailed
   :efe-trustworthy-over-starmap
   :wm-overnight-unsupervised])

(def ^:private canonical-mission-aliases
  {"guardrails core (Unit 1)" "M-wm-guardrails-core"
   "input-sources hygiene (Unit 2)" "M-war-machine-input-sources"
   "hole-counter (Unit 3)" "M-war-machine-hole-counter"
   "gate-runner (Unit 4)" "M-war-machine-gate-runner"
   "M-capability-star-map (THIS mission — in progress)" "M-capability-star-map"})

(defn contaminated-path?
  "True for sources that must not enter the canonical landscape prior."
  [path]
  (let [s (str path)]
    (or (str/includes? s "/.state/")
        (str/includes? s "/worktrees/")
        (str/includes? s "/futon3/")
        (str/includes? s "/futon3-origin/"))))

(defn clean-missions
  [missions]
  (->> missions
       (remove #(contaminated-path? (:path %)))
       vec))

(defn- strip-md [s]
  (str/replace (str s) #"\.md$" ""))

(defn- mission-id-from-minted-by [s]
  (or (get canonical-mission-aliases s)
      (some-> (re-find #"M-[A-Za-z0-9-]+" (str s)) strip-md)))

(defn- status->mission-status [cap-status]
  (if (= :satisfied cap-status) :complete :held))

(defn read-edn-file [path]
  (with-open [r (io/reader path)]
    (edn/read (java.io.PushbackReader. r))))

(defn read-pudding-registry
  ([] (read-pudding-registry default-pudding-registry-path))
  ([path]
   (let [doc (read-edn-file path)]
     (into {} (map (juxt :id identity) (:sorries doc))))))

(defn- capability-from-ensemble [pudding-by-id [cap-id cap]]
  (let [pudding (case cap-id
                  :wm-overnight-unsupervised (get pudding-by-id :T4.2)
                  nil)]
    [cap-id
     (cond-> {:title (:title cap)
              :status (:status cap)
              :scope (vec (:scope cap))
              :minted-by (->> (:minted-by cap)
                              (keep mission-id-from-minted-by)
                              distinct
                              vec)
              :pre-registered? true}
       (:attested cap) (assoc :attested true)
       (:frontier cap) (assoc :frontier true)
       (:keystone cap) (assoc :keystone true)
       (:grounding cap) (assoc :grounding (:grounding cap))
       pudding (assoc :pudding-thesis (:id pudding)
                      :cap/altitude (:altitude pudding)))]))

(defn capability-nodes
  [ensemble pudding-by-id]
  (let [caps (:capabilities ensemble)]
    (into (sorted-map)
          (map #(capability-from-ensemble pudding-by-id [% (get caps %)]))
          wm-region-capability-order)))

(defn- mission-node [mission-index cap-id cap mission-id]
  (let [registry (get mission-index mission-id)]
    [mission-id
     {:scope (vec (:scope cap))
      :produces [cap-id]
      :open-hole-count (long (or (:open-hole-count registry) 0))
      :phase (or (:status-class registry)
                 (status->mission-status (:status cap)))
      :status (or (:status-class registry)
                  (status->mission-status (:status cap)))
      :next-exit-operator-verify? (= cap-id :efe-trustworthy-over-starmap)}]))

(defn mission-nodes
  [missions capabilities]
  (let [mission-index (into {} (map (juxt :id identity) (clean-missions missions)))]
    (into (sorted-map)
          (for [[cap-id cap] capabilities
                mission-id (:minted-by cap)]
            (mission-node mission-index cap-id cap mission-id)))))

(defn requires-edges [capabilities]
  (vec (for [[cap-id cap] capabilities
             prereq (:scope cap)]
         {:from cap-id :to prereq :type :requires})))

(defn produces-edges [capabilities]
  (vec (for [[cap-id cap] capabilities
             mission-id (:minted-by cap)]
         {:from mission-id :to cap-id :type :produces})))

(defn pudding-edges [pudding-by-id]
  (let [interesting #{:T4 :T4.2}]
    (vec
     (concat
      (for [[id rec] pudding-by-id
            :let [parent (:parent rec)]
            :when (and (interesting id) parent)]
        {:from id :to parent :type :specialises})
      (for [[id rec] pudding-by-id
            :let [coupled (:couples rec)]
            :when (and (interesting id) coupled)]
        {:from id :to coupled :type :couples})))))

(defn build-graph
  "Build the WM-region graph. Optional opts: :ensemble-path, :pudding-path,
  :missions. Supplying :missions keeps tests and retries deterministic."
  ([] (build-graph {}))
  ([{:keys [ensemble-path pudding-path missions]
     :or {ensemble-path default-ensemble-path
          pudding-path default-pudding-registry-path}}]
   (let [ensemble (read-edn-file ensemble-path)
         pudding-by-id (read-pudding-registry pudding-path)
         capabilities (capability-nodes ensemble pudding-by-id)
         missions (or missions (:missions (mission-registry/load-missions)))
         mission-map (mission-nodes missions capabilities)]
     {:star-map/region :wm
      :star-map/source {:ensemble ensemble-path
                        :pudding-registry pudding-path
                        :mission-source :futon2.aif.mission-registry/load-missions
                        :exclusion [:worktrees :futon3-origin :.state]}
      :capabilities capabilities
      :missions mission-map
      :edges (vec (concat (requires-edges capabilities)
                          (produces-edges capabilities)
                          (pudding-edges pudding-by-id)))})))

(defn write-graph!
  ([] (write-graph! default-output-path))
  ([path]
   (let [graph (build-graph)]
     (spit path (with-out-str (pprint/pprint graph)))
     graph)))

(defn requires-toposort
  "Return capability ids in prerequisite-before-dependent order. Throws on cycle."
  [graph]
  (let [reqs (filter #(= :requires (:type %)) (:edges graph))
        nodes (set (keys (:capabilities graph)))
        deps (reduce (fn [m {:keys [from to]}]
                       (update m from (fnil conj #{}) to))
                     (zipmap nodes (repeat #{}))
                     reqs)]
    (loop [remaining deps
           ordered []]
      (if (empty? remaining)
        ordered
        (let [ready (->> remaining
                         (filter (fn [[_ ds]] (empty? (set/intersection ds (set (keys remaining))))))
                         (map key)
                         sort
                         vec)]
          (when (empty? ready)
            (throw (ex-info "cycle in :requires graph" {:remaining remaining})))
          (recur (apply dissoc remaining ready) (into ordered ready)))))))

(defn graph->trace
  "Adapt a real graph into the abstract invariant trace. The trace contains the
  actual :requires edges, provenance facts for satisfied capabilities, and only
  applicable advance steps for missions whose scope is already satisfied."
  [graph]
  (let [caps (:capabilities graph)
        satisfied? #(= :satisfied (get-in caps [% :status]))
        mission-complete? (fn [m] (= :complete (get-in graph [:missions m :status])))
        edge-steps (map-indexed (fn [i {:keys [from to]}]
                                  {:step (keyword (str "edge-" i))
                                   :edge-from from
                                   :edge-to to})
                                (filter #(= :requires (:type %)) (:edges graph)))
        provenance-steps (for [[cap-id cap] caps
                               :when (= :satisfied (:status cap))]
                           {:step (keyword (str "prov-" (name cap-id)))
                            :satisfied-cap cap-id
                            :minted-by-complete? (boolean (some mission-complete? (:minted-by cap)))})
        advance-steps (for [[mission-id mission] (:missions graph)
                            :let [scope (:scope mission)
                                  applicable? (every? satisfied? scope)]
                            :when applicable?]
                        {:step (keyword (str "advance-" mission-id))
                         :action :advance
                         :mission mission-id
                         :requires-sat? true
                         :crosses-exit? (:next-exit-operator-verify? mission)
                         :gap-agreed? true})]
    (vec (concat edge-steps provenance-steps advance-steps))))

(defn run-verify-equivalent
  ([] (run-verify-equivalent (build-graph)))
  ([graph]
   (let [order (requires-toposort graph)
         trace (graph->trace graph)
         violations (inv/query-violations trace)]
     {:verified? (not (inv/violations? violations))
      :toposort order
      :violations violations
      :trace-count (count trace)})))

(defn transitive-scope
  [graph cap-id]
  (let [caps (:capabilities graph)]
    (loop [frontier (seq (get-in caps [cap-id :scope]))
           seen #{}]
      (if-let [c (first frontier)]
        (if (seen c)
          (recur (next frontier) seen)
          (recur (concat (next frontier) (get-in caps [c :scope])) (conj seen c)))
        seen))))

(defn keystone-path-report
  ([] (keystone-path-report (build-graph)))
  ([graph]
   (let [scope (transitive-scope graph :wm-overnight-unsupervised)
         held (->> scope
                   (filter #(not= :satisfied (get-in graph [:capabilities % :status])))
                   sort
                   vec)]
     {:target :wm-overnight-unsupervised
      :transitive-scope (vec (sort scope))
      :held held
      :single-held-substantive-node? (= held [:efe-trustworthy-over-starmap])})))

(defn extract-write-and-verify!
  ([] (extract-write-and-verify! default-output-path))
  ([path]
   (let [graph (write-graph! path)]
     {:output path
      :graph-counts {:capabilities (count (:capabilities graph))
                     :missions (count (:missions graph))
                     :edges (count (:edges graph))}
      :verify (run-verify-equivalent graph)
      :keystone (keystone-path-report graph)})))
