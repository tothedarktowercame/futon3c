(ns futon3c.diagramprover.opaque-rules
  "Read-only exploration of captured deterministic whole-sequence transitions.
  Digests, not observations, decide joining. Missing rule entries are incomplete
  evidence, never implicit 'same'. Capture pins are retained without authentication."
  (:require [cheshire.core :as json]
            [clojure.set :as set]
            [futon3c.diagramprover.findings :as findings]))

(defn load-table [path] (json/parse-string (slurp path) true))
(defn- entry [m id] (get m (keyword id)))
(defn- list-items [t]
  (case (first t) "nil" [] "cons" (cons (second t) (list-items (nth t 2)))
        (throw (ex-info "Malformed captured list" {:reason :invalid-tree}))))
(defn observation
  "Count arms from the stored tree. Flat rows supply outer arity but, without
  reliable branch ownership, cannot establish arm-count consistency."
  [state]
  (let [splits (when (:tree state)
                 (filter #(and (vector? %) (= "split" (first %)))
                         (tree-seq vector? rest (:tree state))))
        flat-split (first (filter #(= "split" (:kind %)) (:rows state)))]
    {:well-nested? (:well_nested state)
     :arity-consistent? (if (:tree state)
                          (every? #(= (get-in % [1 0 1]) (count (list-items (nth % 5)))) splits)
                          (if flat-split :unknown (if (empty? (:rows state)) true :unknown)))
     :outer-arity (if (:tree state) (get-in (first splits) [1 0 1]) (:case_arity flat-split))}))
(defn- explore [table rules start]
  (loop [todo [start] seen #{} normal #{} incomplete (boolean (:bound_hit table))]
    (if-let [s (peek todo)]
      (if (contains? seen s) (recur (pop todo) seen normal incomplete)
          (let [row (entry (:table table) s)
                complete (and (entry (:states table) s) (every? #(contains? row %) rules))
                nexts (keep #(let [v (get row %)] (when (and v (not= v "same") (not= v s)) v)) rules)]
            (recur (into (pop todo) nexts) (conj seen s)
                   (cond-> normal (and complete (empty? nexts)) (conj s))
                   (boolean (or incomplete (not complete))))))
      {:reachable seen :normal-forms normal :incomplete? incomplete
       :observations (mapv #(observation (entry (:states table) %)) (sort normal))})))
(defn table-pairs [table]
  (let [rules (vec (sort (keys (:rules table))))
        reachable (:reachable (explore table rules (:start table)))]
    {:status (if (:incomplete? (explore table rules (:start table))) :inconclusive :complete)
     :rules rules
     :pairs
     (vec (for [s (sort reachable)
                :let [row (entry (:table table) s)
                      moves (filter #(let [v (get row %)] (and v (not= v "same") (not= v s))) rules)]
                i (range (count moves)) j (range (inc i) (count moves))
                :let [a (nth (vec moves) i) b (nth (vec moves) j)
                      sides (mapv #(explore table rules (get row %)) [a b])
                      incomplete (some :incomplete? sides)
                      status (cond incomplete :inconclusive
                                   (seq (apply set/intersection (map :reachable sides))) :joining
                                   :else :non-joining)]]
            {:schemas [a b] :instances (mapv #(hash-map :schema {:name %}) [a b])
             :state s :results [(get row a) (get row b)]
             :overlap [:opaque-state [[:digest s]] [[:module (:module table)]]
                       [[:declaration (:declaration table)]]]
             :position [] :path [] :kind :whole-sequence :status status
             :normalisations sides
             :observation-comparison (if incomplete :inconclusive
                                         (if (apply = (map #(set (:observations %)) sides)) :equal :different))}))}))
(defn table-findings [table pins]
  (findings/findings (table-pairs table)
                     (merge {:max-states (:max_states table)} pins
                            {:mode :unordered :rule-form :opaque :capture-pins (:pins table)})))
(defn rule-set-diff
  "Explore each selected rule set from its table's start. Preserve all normal
  forms; absent/multiple normal forms or an incomplete capture are inconclusive."
  [table-a rules-a table-b rules-b supersession-id]
  (let [version (fn [table rules]
                  (merge (explore table (mapv keyword rules) (:start table))
                         {:rules (mapv keyword rules) :start (:start table) :pins (:pins table)
                          :locator (select-keys table [:module :declaration])}))
        a (version table-a rules-a) b (version table-b rules-b)
        complete (every? #(and (not (:incomplete? %)) (= 1 (count (:normal-forms %)))) [a b])]
    {:a a :b b :supersession-id supersession-id
     :comparison (if complete (if (= (:normal-forms a) (:normal-forms b)) :equal :different) :inconclusive)
     :observation-comparison (if complete (if (= (:observations a) (:observations b)) :equal :different) :inconclusive)}))
