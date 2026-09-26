(ns futon3c.diagramprover.causal.dag
  "Pure validated DAG model and causal-spec JSON ingest."
  (:refer-clojure :exclude [parents ancestors descendants])
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn- id-keyword [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else x))

(defn- normalize-variable [variable]
  (update variable :id id-keyword))

(defn- normalize-arrow [arrow]
  (-> arrow
      (update :from id-keyword)
      (update :to id-keyword)))

(defn- normalize-leak [leak]
  (-> leak
      (update :id id-keyword)
      (update :to id-keyword)))

(defn- normalize-intervention [intervention]
  (update intervention :target id-keyword))

(defn- normalize-sensor [sensor]
  (update sensor :id id-keyword))

(defn parents [dag node]
  (into #{} (keep #(when (= node (:to %)) (:from %))) (:arrows dag)))

(defn children [dag node]
  (into #{} (keep #(when (= node (:from %)) (:to %))) (:arrows dag)))

(defn- closure [step dag nodes]
  (loop [frontier (seq nodes) seen (set nodes)]
    (if-let [node (first frontier)]
      (let [fresh (set/difference (step dag node) seen)]
        (recur (concat (rest frontier) fresh) (into seen fresh)))
      (set/difference seen (set nodes)))))

(defn ancestors [dag nodes]
  (closure parents dag (if (coll? nodes) nodes [nodes])))

(defn descendants [dag nodes]
  (closure children dag (if (coll? nodes) nodes [nodes])))

(defn exogenous? [dag node]
  (empty? (parents dag node)))

(defn topological-sort
  "Return a deterministic topological order, or throw with cyclic nodes named."
  [dag]
  (let [nodes (set (keys (:variables dag)))
        indegree (into {} (map (fn [node] [node (count (parents dag node))]) nodes))]
    (loop [order []
           degrees indegree
           ready (into (sorted-set) (keep #(when (zero? (get indegree %)) %)) nodes)]
      (if-let [node (first ready)]
        (let [[degrees' ready']
              (reduce (fn [[ds rs] child]
                        (let [degree (dec (get ds child))]
                          [(assoc ds child degree)
                           (cond-> rs (zero? degree) (conj child))]))
                      [degrees (disj ready node)]
                      (children dag node))]
          (recur (conj order node) degrees' ready'))
        (if (= (count order) (count nodes))
          order
          (let [cycle (->> degrees (keep (fn [[node degree]]
                                           (when (pos? degree) node))) sort vec)]
            (throw (ex-info "Causal graph contains a cycle"
                            {:cycle cycle}))))))))

(defn validate
  "Validate references and acyclicity; return DAG unchanged."
  [dag]
  (let [known (set (keys (:variables dag)))
        unknown (->> (:arrows dag)
                     (mapcat (juxt :from :to))
                     (remove known)
                     set)]
    (when (seq unknown)
      (throw (ex-info "Causal arrow refers to unknown variable"
                      {:unknown-variables unknown})))
    (topological-sort dag)
    dag))

(defn load-spec
  "Load and normalize one causal interchange JSON document."
  [path]
  (let [raw (json/parse-string (slurp (io/file path)) true)
        variables (mapv normalize-variable (:variables raw))
        leaks (mapv normalize-leak (get-in raw [:leak_edges :edges]))
        dag {:variables (into (sorted-map) (map (juxt :id identity)) variables)
             :arrows (mapv normalize-arrow (:arrows raw))
             :leak-edges leaks
             :interventions (mapv normalize-intervention (:interventions raw))
             :sensors (mapv normalize-sensor (get-in raw [:measurement_layer :sensors]))
             :metadata (dissoc raw :variables :arrows :leak_edges
                               :interventions :measurement_layer)}]
    (validate dag)))

(defn with-leaks
  "Materialize removable leaks as exogenous latent variables and arrows."
  [dag]
  (validate
   (reduce (fn [result {:keys [id to from] :as leak}]
             (if (contains? (:variables result) id)
               result
               (-> result
                   (assoc-in [:variables id]
                             {:id id :name from :kind :exogenous-latent
                              :leak/id id})
                   (update :arrows conj
                           {:from id :to to :status :leak
                            :leak/id id
                            :mechanism (:mechanism leak)}))))
           dag (:leak-edges dag))))

(defn without-leaks
  "Remove all materialized leak latents and their incident arrows."
  [dag]
  (let [latent-ids (into #{} (keep (fn [[id variable]]
                                     (when (:leak/id variable) id)))
                         (:variables dag))]
    (validate
     (-> dag
         (update :variables #(apply dissoc % latent-ids))
         (update :arrows (fn [arrows]
                           (into [] (remove #(or (latent-ids (:from %))
                                                (latent-ids (:to %)))) arrows)))))))
