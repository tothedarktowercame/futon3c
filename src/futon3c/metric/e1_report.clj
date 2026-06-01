(ns futon3c.metric.e1-report
  "Read-only R1 report for M-substrate-metric.

   The live path fetches only the E1 feeds-mu relation families from substrate-2
   and summarizes the resulting graph. No entity corpus, code embeddings, JAX,
   or full JSON entity arrays are read."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.set :as set]
            [futon3c.metric.e1 :as e1]))

(def default-futon1a-url
  (or (System/getenv "FUTON1A_URL") "http://localhost:7071"))

(def default-penholder
  (or (System/getenv "FUTON1A_PENHOLDER") "api"))

(def feeds-mu-query-types
  ["code/v05/related-mission"
   "code/v05/mission-cross-ref"
   "code/v05/file->mission"
   "code/v05/file→mission"
   "code/v05/sorry->related-missions"])

(defn mission-artifact-endpoint?
  "True for mission-adjacent report/sample artifacts that can be ingested as
   mission endpoints because they live under holes/missions and start with M-.
   These are documentation outputs, not O1 mission nodes."
  [endpoint]
  (and (string? endpoint)
       (re-find #"/mission/" endpoint)
       (boolean (re-find #"(?:\.R[0-9][A-Za-z0-9.-]*|\.OR-sample)$" endpoint))))

(defn- url-encode
  [s]
  (java.net.URLEncoder/encode (str s) "UTF-8"))

(defn- parse-json-body
  [body]
  (cond
    (string? body) (json/parse-string body true)
    (map? body) body
    :else nil))

(defn fetch-hyperedges-by-type
  ([hx-type] (fetch-hyperedges-by-type hx-type {}))
  ([hx-type {:keys [futon1a-url limit]
             :or {futon1a-url default-futon1a-url
                  limit 2000}}]
   (let [url (str futon1a-url
                  "/api/alpha/hyperedges?type="
                  (url-encode hx-type)
                  "&limit="
                  limit)
         resp (http/get url {:headers {"Accept" "application/json"
                                       "X-Penholder" default-penholder}
                             :throw false
                             :timeout 10000})]
     (if (= 200 (:status resp))
       (vec (or (:hyperedges (parse-json-body (:body resp))) []))
       (throw (ex-info "hyperedges-by-type failed"
                       {:type hx-type
                        :url url
                        :status (:status resp)
                        :body (:body resp)}))))))

(defn fetch-e1-edges
  [{:keys [fetcher relation-types limit]
    :or {fetcher fetch-hyperedges-by-type
         relation-types feeds-mu-query-types
         limit 2000}}]
  (let [by-type
        (into {}
              (for [hx-type relation-types]
                [hx-type (fetcher hx-type {:limit limit})]))]
    {:by-type by-type
     :edges (->> by-type
                 vals
                 (apply concat)
                 e1/feeds-mu-edges
                 (remove (fn [edge]
                           (some mission-artifact-endpoint?
                                 (e1/edge-endpoints edge))))
                 vec)}))

(defn node-type
  [endpoint]
  (cond
    (and (string? endpoint) (re-find #"/mission/" endpoint)) :mission
    (and (string? endpoint) (re-find #"/sorry/" endpoint)) :sorry
    (and (string? endpoint) (re-find #"/pattern/" endpoint)) :pattern
    (and (string? endpoint) (re-find #"/file/" endpoint)) :file
    :else :unknown))

(defn- graph-nodes
  [graph]
  (set (keys graph)))

(defn connected-components
  [graph]
  (loop [remaining (graph-nodes graph)
         components []]
    (if (empty? remaining)
      components
      (let [start (first remaining)
            component
            (loop [frontier [start]
                   seen #{start}]
              (if-let [node (peek frontier)]
                (let [neighbors (remove seen (get graph node))]
                  (recur (into (pop frontier) neighbors)
                         (into seen neighbors)))
                seen))]
        (recur (set/difference remaining component)
               (conj components component))))))

(defn- edge-pairs
  [edges]
  (map (fn [edge]
         (let [[a b] (e1/edge-endpoints edge)]
           (if (neg? (compare (str a) (str b))) [a b] [b a])))
       edges))

(defn- pair-multiplicities
  [edges]
  (frequencies (edge-pairs edges)))

(defn- reachable-without-pair
  [graph source blocked-pair]
  (let [[blocked-a blocked-b] blocked-pair
        blocked? #(or (= [blocked-a blocked-b] %)
                      (= [blocked-b blocked-a] %))]
    (loop [frontier [source]
           seen #{source}]
      (if-let [node (peek frontier)]
        (let [neighbors (->> (get graph node)
                             (remove seen)
                             (remove #(blocked? [node %])))]
          (recur (into (pop frontier) neighbors)
                 (into seen neighbors)))
        seen))))

(defn bridge-candidates
  "Return simple graph bridge candidates with split-size scores.

   Multiedge pairs are not bridges in the v0 structural graph because removing
   one edge leaves another direct connection."
  [graph edges]
  (let [multiplicity (pair-multiplicities edges)
        comps (connected-components graph)
        comp-by-node (into {}
                           (mapcat (fn [idx component]
                                     (map #(vector % idx) component))
                                   (range)
                                   comps))]
    (->> multiplicity
         (keep (fn [[[a b :as pair] n]]
                 (when (= 1 n)
                   (let [component (nth comps (comp-by-node a))
                         side (reachable-without-pair graph a pair)]
                     (when (not (contains? side b))
                       (let [side-size (count side)
                             other-size (- (count component) side-size)]
                         {:edge pair
                          :relation (some (fn [edge]
                                            (when (= pair (first (edge-pairs [edge])))
                                              (e1/normalize-feeds-mu-relation
                                               (or (:hx/type edge)
                                                   (:relation/type edge)
                                                   (:type edge)))))
                                          edges)
                          :component-size (count component)
                          :split-sizes [side-size other-size]
                          :bridge-score (* side-size other-size)}))))))
         (sort-by :bridge-score >)
         vec)))

(defn report
  [{:keys [fetcher limit top-bridges]
    :or {limit 2000
         top-bridges 20}
    :as opts}]
  (let [{:keys [by-type edges]} (fetch-e1-edges (assoc opts :limit limit))
        graph (e1/build-undirected-graph edges)
        nodes (graph-nodes graph)
        components (connected-components graph)
        bridges (bridge-candidates graph edges)]
    {:report :substrate-metric/e1-r1
     :limit-per-type limit
     :relation-types feeds-mu-query-types
     :fetched-by-type (into {}
                            (for [[t xs] by-type]
                              [t (count xs)]))
     :feeds-mu-edge-count (count edges)
     :node-count (count nodes)
     :node-count-by-type (frequencies (map node-type nodes))
     :component-count (count components)
     :largest-components (->> components
                              (sort-by count >)
                              (take 10)
                              (mapv (fn [component]
                                      {:size (count component)
                                       :node-types (frequencies (map node-type component))
                                       :sample (vec (take 8 (sort component)))})))
     :bridge-candidate-count (count bridges)
     :candidate-bridges (vec (take top-bridges bridges))}))
