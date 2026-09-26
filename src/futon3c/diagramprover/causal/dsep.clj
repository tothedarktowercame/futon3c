(ns futon3c.diagramprover.causal.dsep
  "D-separation, active-path witnesses, and bounded implication enumeration."
  (:require [clojure.set :as set]
            [futon3c.diagramprover.causal.dag :as dag]
            [futon3c.diagramprover.causal.surgery :as surgery]))

(defn- node-set [nodes]
  (cond
    (nil? nodes) #{}
    (set? nodes) nodes
    (sequential? nodes) (set nodes)
    :else #{nodes}))

(defn- require-known [causal-dag nodes]
  (let [unknown (set/difference nodes (set (keys (:variables causal-dag))))]
    (when (seq unknown)
      (throw (ex-info "Unknown d-separation variable"
                      {:unknown-variables unknown}))))
  nodes)

(defn- connect [adjacency a b]
  (-> adjacency (update a (fnil conj #{}) b) (update b (fnil conj #{}) a)))

(defn- moral-adjacency [causal-dag retained]
  (let [adjacency (zipmap retained (repeat #{}))
        adjacency (reduce (fn [result {:keys [from to]}]
                            (if (and (retained from) (retained to))
                              (connect result from to)
                              result))
                          adjacency (:arrows causal-dag))]
    (reduce
     (fn [result node]
       (let [ps (vec (sort (set/intersection retained
                                              (dag/parents causal-dag node))))]
         (reduce (fn [r [a b]] (connect r a b)) result
                 (for [i (range (count ps))
                       j (range (inc i) (count ps))]
                   [(ps i) (ps j)]))))
     adjacency retained)))

(defn- reachable? [adjacency starts targets]
  (loop [queue (into clojure.lang.PersistentQueue/EMPTY starts)
         seen (set starts)]
    (if-let [node (peek queue)]
      (if (targets node)
        true
        (let [fresh (set/difference (get adjacency node #{}) seen)]
          (recur (into (pop queue) fresh) (into seen fresh))))
      false)))

(defn d-connected?
  "True when X and Y are d-connected given Z in a validated DAG."
  [causal-dag xs ys zs]
  (let [xs (require-known causal-dag (node-set xs))
        ys (require-known causal-dag (node-set ys))
        zs (require-known causal-dag (node-set zs))
        seeds (into xs (concat ys zs))
        ancestral (into seeds (dag/ancestors causal-dag seeds))
        retained (set/difference ancestral zs)
        adjacency (moral-adjacency causal-dag ancestral)]
    (and (seq xs) (seq ys)
         (reachable? (select-keys adjacency retained)
                     (set/difference xs zs)
                     (set/difference ys zs)))))

(defn d-separated? [causal-dag xs ys zs]
  (not (d-connected? causal-dag xs ys zs)))

(defn- directed-edge? [causal-dag from to]
  (boolean (some #(and (= from (:from %)) (= to (:to %)))
                 (:arrows causal-dag))))

(defn- active-path? [causal-dag path conditioned ancestors-of-conditioned]
  (and (not-any? conditioned [(first path) (last path)])
       (every?
        true?
        (for [[previous node following] (partition 3 1 path)]
          (let [collider? (and (directed-edge? causal-dag previous node)
                               (directed-edge? causal-dag following node))]
            (if collider?
              (contains? ancestors-of-conditioned node)
              (not (contains? conditioned node))))))))

(defn connecting-paths
  "Return active trail witnesses as `{:paths [...], :count n, :truncated? b}`.

  Enumeration stops at `:limit` (default 1); count is therefore a witnessed
  lower bound when `:truncated?` is true, not an exhaustive path count."
  ([causal-dag xs ys zs] (connecting-paths causal-dag xs ys zs {}))
  ([causal-dag xs ys zs {:keys [limit] :or {limit 1}}]
   (let [xs (require-known causal-dag (node-set xs))
         ys (require-known causal-dag (node-set ys))
         zs (require-known causal-dag (node-set zs))
         ancestor-z (into zs (dag/ancestors causal-dag zs))
         neighbours (fn [node] (into (dag/parents causal-dag node)
                                     (dag/children causal-dag node)))
         found (volatile! [])]
     (letfn [(walk [path]
               (when (< (count @found) limit)
                 (let [node (peek path)]
                   (if (and (> (count path) 1) (ys node))
                     (when (active-path? causal-dag path zs ancestor-z)
                       (vswap! found conj path))
                     (doseq [next-node (sort (remove (set path) (neighbours node)))]
                       (walk (conj path next-node)))))))]
       (doseq [start (sort xs)] (walk [start]))
       {:paths @found
        :count (count @found)
        :truncated? (= limit (count @found))}))))

(defn- combinations [items size]
  (cond
    (zero? size) '(())
    (empty? items) ()
    :else (concat (map #(cons (first items) %)
                       (combinations (rest items) (dec size)))
                  (combinations (rest items) size))))

(defn implied-independencies
  "Enumerate minimal pairwise separating sets up to size k (default 2).

  This is deliberately truncated: absence from the result does not show that
  no larger separating set exists."
  ([causal-dag] (implied-independencies causal-dag {}))
  ([causal-dag {:keys [max-conditioning] :or {max-conditioning 2}}]
   (let [nodes (vec (sort (keys (:variables causal-dag))))
         adjacent? (fn [a b] (or ((dag/children causal-dag a) b)
                                  ((dag/children causal-dag b) a)))]
     (vec
      (mapcat
       (fn [[x y]]
         (loop [size 0 found []]
           (if (> size max-conditioning)
             found
             (let [candidates (remove #{x y} nodes)
                   separating (->> (combinations candidates size)
                                   (map set)
                                   (filter #(d-separated? causal-dag x y %))
                                   (remove (fn [zs]
                                             (some #(set/subset? (:given %) zs)
                                                   found)))
                                   (mapv #(hash-map :x x :y y :given %)))]
               (recur (inc size) (into found separating))))))
       (for [i (range (count nodes))
             j (range (inc i) (count nodes))
             :let [x (nodes i) y (nodes j)]
             :when (not (adjacent? x y))]
         [x y]))))))

(defn backdoor-adjustment?
  "Whether Z is a valid backdoor adjustment set for treatment X and outcome Y."
  [causal-dag x y zs]
  (let [zs (require-known causal-dag (node-set zs))]
    (and (empty? (set/intersection zs (dag/descendants causal-dag x)))
         (d-separated? (surgery/cut-outgoing causal-dag x) x y zs))))
