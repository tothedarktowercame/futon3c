(ns futon3c.diagramprover.causal.admg
  "Deterministic latent projection of a marked-variable DAG into an ADMG."
  (:refer-clojure :exclude [parents ancestors])
  (:require [clojure.set :as set]
            [futon3c.diagramprover.causal.dag :as dag]))

(defn latent? [causal-dag node]
  (= :latent-unobserved (keyword (get-in causal-dag [:variables node :kind]))))

(defn validate [{:keys [nodes directed bidirected] :as graph}]
  (let [unknown (set/difference
                 (into #{} (mapcat identity) directed)
                 nodes)
        unknown (set/union unknown
                           (set/difference (into #{} (mapcat identity) bidirected)
                                           nodes))]
    (when (seq unknown)
      (throw (ex-info "ADMG edge refers to unknown node" {:unknown unknown})))
    (when (some #(not= 2 (count %)) bidirected)
      (throw (ex-info "ADMG bidirected edge must join two distinct nodes" {})))
    (dag/topological-sort
     {:variables (zipmap nodes (map #(hash-map :id %) nodes))
      :arrows (mapv (fn [[from to]] {:from from :to to}) directed)})
    graph))

(defn- observed-frontier
  "Observed descendants reached through latent-only interiors."
  [causal-dag start]
  (loop [queue (seq (dag/children causal-dag start)) seen #{} result #{}]
    (if-let [node (first queue)]
      (cond
        (seen node) (recur (rest queue) seen result)
        (latent? causal-dag node)
        (recur (concat (rest queue) (dag/children causal-dag node))
               (conj seen node) result)
        :else (recur (rest queue) (conj seen node) (conj result node)))
      result)))

(defn latent-project [causal-dag]
  (let [observed (into (sorted-set)
                       (remove #(latent? causal-dag %))
                       (keys (:variables causal-dag)))
        directed (into (sorted-set)
                       (for [from observed
                             to (observed-frontier causal-dag from)]
                         [from to]))
        bidirected
        (into #{}
              (mapcat
               (fn [latent]
                 (let [targets (vec (sort (observed-frontier causal-dag latent)))]
                   (for [i (range (count targets))
                         j (range (inc i) (count targets))]
                     #{(targets i) (targets j)})))
               (filter #(latent? causal-dag %) (keys (:variables causal-dag)))))]
    (validate {:nodes observed :directed directed :bidirected bidirected})))

(defn induced [graph nodes]
  (let [nodes (set nodes)]
    (validate {:nodes nodes
               :directed (into #{} (filter #(every? nodes %)) (:directed graph))
               :bidirected (into #{} (filter #(every? nodes %)) (:bidirected graph))})))

(defn parents [graph node]
  (into #{} (keep (fn [[from to]] (when (= to node) from))) (:directed graph)))

(defn children [graph node]
  (into #{} (keep (fn [[from to]] (when (= from node) to))) (:directed graph)))

(defn ancestors [graph starts]
  (loop [frontier (seq starts) seen (set starts)]
    (if-let [node (first frontier)]
      (let [fresh (set/difference (parents graph node) seen)]
        (recur (concat (rest frontier) fresh) (into seen fresh)))
      seen)))

(defn districts [graph]
  (loop [remaining (set (:nodes graph)) result []]
    (if-let [start (first (sort remaining))]
      (let [component
            (loop [queue [start] seen #{start}]
              (if-let [node (first queue)]
                (let [neighbours (into #{}
                                       (comp (filter #(contains? % node))
                                             (mapcat identity)
                                             (remove #{node}))
                                       (:bidirected graph))
                      fresh (set/difference neighbours seen)]
                  (recur (into (vec (rest queue)) (sort fresh))
                         (into seen fresh)))
                seen))]
        (recur (set/difference remaining component) (conj result component)))
      (vec (sort-by #(vec (sort %)) result)))))

(defn topological-sort [graph]
  (dag/topological-sort
   {:variables (zipmap (:nodes graph) (map #(hash-map :id %) (:nodes graph)))
    :arrows (mapv (fn [[from to]] {:from from :to to}) (:directed graph))}))
