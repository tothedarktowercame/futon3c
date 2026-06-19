(ns futon3c.metric.e1
  "Small runtime core for the M-substrate-metric E1 curvature cut.

   This namespace implements the contract-fixed substrate pieces around the OR
   solver: feeds-mu edge selection, lazy random-walk measures, hop distance,
   node-level strain rollup, and O3 proposal composition. It deliberately does
   not compute Ollivier-Ricci curvature itself."
  (:require [clojure.string :as str]))

(def default-alpha 0.5)

(defn- relation-string
  [relation]
  (cond
    (keyword? relation) (if-let [ns-part (namespace relation)]
                          (str ns-part "/" (name relation))
                          (name relation))
    (string? relation) relation
    (nil? relation) ""
    :else (str relation)))

(defn normalize-feeds-mu-relation
  "Return the E1 feeds-mu relation keyword, or nil when relation is not in the
   curvature substrate."
  [relation]
  (let [s (relation-string relation)]
    (cond
      (str/ends-with? s "related-mission") :related-mission
      (str/ends-with? s "mission-cross-ref") :mission-cross-ref
      (and (str/includes? s "file")
           (str/includes? s "mission")) :file->mission
      (and (str/includes? s "sorry")
           (str/includes? s "mission")) :sorry->related-missions
      :else nil)))

(defn feeds-mu-edge?
  [edge]
  (boolean (normalize-feeds-mu-relation
            (or (:relation/type edge)
                (:edge/type edge)
                (:hx/type edge)
                (:relation edge)
                (:type edge)))))

(defn edge-endpoints
  "Extract the two real endpoints from a normalized edge or substrate hyperedge."
  [edge]
  (let [eps (or (:endpoints edge)
                (:hx/endpoints edge)
                (when (and (contains? edge :source)
                           (contains? edge :target))
                  [(:source edge) (:target edge)]))]
    (->> eps
         (remove #(and (string? %) (str/starts-with? % "dir:")))
         vec)))

(defn feeds-mu-edges
  [edges]
  (->> edges
       (filter feeds-mu-edge?)
       (filter #(= 2 (count (edge-endpoints %))))
       vec))

(defn build-undirected-graph
  "Build an undirected multigraph adjacency map over feeds-mu edges.

   Repeated edges are preserved as repeated neighbor entries because mu_x is
   uniform over incident structural edges before masses are collected by node."
  [edges]
  (reduce
   (fn [graph edge]
     (let [[a b] (edge-endpoints edge)]
       (-> graph
           (update a (fnil conj []) b)
           (update b (fnil conj []) a))))
   {}
   (feeds-mu-edges edges)))

(defn lazy-random-walk-measure
  "mu_x = alpha * delta_x + (1-alpha) uniformly over incident feeds-mu edges.

   Isolated nodes return delta_x. Repeated neighbor entries collect mass at that
   neighbor, matching the contract's multiset rule."
  ([graph x] (lazy-random-walk-measure graph x default-alpha))
  ([graph x alpha]
   (let [neighbors (vec (get graph x))]
     (if (seq neighbors)
       (let [edge-mass (/ (- 1.0 (double alpha)) (count neighbors))]
         (reduce
          (fn [mu neighbor]
            (update mu neighbor (fnil + 0.0) edge-mass))
          {x (double alpha)}
          neighbors))
       {x 1.0}))))

(defn hop-distance
  "Shortest-path length in an unweighted undirected feeds-mu graph.

   Returns nil for disconnected pairs."
  [graph source target]
  (cond
    (= source target) 0
    :else
    (loop [frontier (conj clojure.lang.PersistentQueue/EMPTY [source 0])
           seen #{source}]
      (if (empty? frontier)
        nil
        (let [[[node dist] frontier*] [(peek frontier) (pop frontier)]
              next-dist (inc dist)
              unseen (remove seen (get graph node))]
          (if (some #{target} unseen)
            next-dist
            (recur (into frontier* (map #(vector % next-dist) unseen))
                   (into seen unseen))))))))

(defn strain-rollup
  "Roll edge-level kappa values up to the node fields consumed by E1."
  [incident-kappas]
  (let [ks (seq incident-kappas)]
    (if ks
      (let [min-entry (apply min-key :kappa ks)
            total (reduce + 0.0 (map :kappa ks))]
        {:curvature/min-incident-kappa (:kappa min-entry)
         :curvature/mean-incident-kappa (/ total (count ks))
         :curvature/strain-edge (:edge min-entry)
         :curvature/strain? (< (double (:kappa min-entry)) 0.0)})
      {:curvature/min-incident-kappa nil
       :curvature/mean-incident-kappa nil
       :curvature/strain-edge nil
       :curvature/strain? false})))

(defn propose-here?
  "O3 composition: curvature strain AND numeric unresolvedness AND actionability."
  [node]
  (let [resolvedness (or (:resolution-state/resolvedness node)
                         (:resolvedness node))
        actionable? (if (contains? node :resolution-state/actionable?)
                      (:resolution-state/actionable? node)
                      (:actionable? node))]
    (and (true? (:curvature/strain? node))
         (number? resolvedness)
         (< (double resolvedness) 1.0)
         (true? actionable?))))

(defn action-intensity
  "Ranking scalar after propose-here? has passed."
  [node]
  (let [k (:curvature/min-incident-kappa node)
        resolvedness (or (:resolution-state/resolvedness node)
                         (:resolvedness node))]
    (if (and (number? k) (number? resolvedness))
      (* (max 0.0 (- (double k)))
         (- 1.0 (double resolvedness)))
      0.0)))
