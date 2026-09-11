(ns futon3c.apm.teaching-plan
  "Structural admission for V4 teaching records, never mathematical certification."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [futon3c.apm.campaign-machine :as machine]))

(defn text? [x] (and (string? x) (not (str/blank? x))))
(defn sha? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))
(defn tag [x] (if (keyword? x) (name x) x))
(defn digest [x] (machine/ledger-digest [x]))
(def phases #{:pattern-plan :pattern-plan-review :pattern-plan-revision})
(defn- texts? [xs] (and (vector? xs) (every? text? xs)))
(defn- distinct-ids? [xs] (= (count xs) (count (set xs))))
(defn- graph-valid? [nodes]
  (let [by-id (into {} (map (juxt :id identity) nodes))
        ids (set (keys by-id))
        roots (filter #(nil? (:parent %)) nodes)
        edges (into {} (for [n nodes]
                         [(:id n) (concat (:depends-on n)
                                          (map :id (filter #(= (:id n) (:parent %)) nodes)))]))]
    (and (distinct-ids? (map :id nodes)) (= 1 (count roots))
         (every? #(or (nil? (:parent %)) (contains? ids (:parent %))) nodes)
         (every? #(set/subset? (set (:depends-on %)) ids) nodes)
         ;; Parent chains must reach the sole root; all dependency edges must be acyclic.
         (every? (fn [id]
                   (loop [pending [id] visited #{}]
                     (if-let [x (peek pending)]
                       (if (contains? visited x) false
                           (recur (if-let [p (:parent (by-id x))] [p] []) (conj visited x)))
                       true))) ids)
         (let [colors (atom {})]
           (letfn [(visit [id]
                     (case (get @colors id)
                       :active false
                       :done true
                       (do (swap! colors assoc id :active)
                           (if (every? visit (edges id))
                             (do (swap! colors assoc id :done) true)
                             false))))]
             (every? visit ids))))))

(defn- warrant-valid? [w]
  (and (map? w) (text? (:explanation w))
       (case (tag (:kind w))
         ("ordinary" "gap") true
         ("pattern" "memory") (and (text? (:id w)) (sha? (:revision w)))
         false)))

(defn plan-valid?
  "Bind revision ancestry and stable old node IDs. New obligations may be added;
  removed or silently renamed obligations are refused. Pins are assertions for
  the TA to inspect, not evidence of retrieval or mathematical applicability."
  [auth plan]
  (try
    (let [context (:v4/teaching auth) prior (:prior-plan context)
          nodes (:nodes plan) responses (:responses plan)
          old-ids (set (map :id (:nodes prior)))]
      (and (map? plan) (= 1 (:version plan))
           (= (:problem-id auth) (:problem-id plan))
           (= (:revision context) (:revision plan))
           (= (when prior (digest prior)) (:parent-digest plan))
           (vector? nodes) (<= 1 (count nodes) 64)
           (every? #(and (map? %) (text? (:id %)) (contains? % :parent)
                         (text? (:goal %)) (texts? (:definitions %))
                         (vector? (:conditions %))
                         (every? (fn [c] (and (map? c) (text? (:statement c))
                                               (contains? #{"open" "established" "refuted"} (tag (:status c)))
                                               (text? (:argument c)))) (:conditions %))
                         (texts? (:depends-on %)) (distinct-ids? (:depends-on %))
                         (warrant-valid? (:warrant %))) nodes)
           (graph-valid? nodes)
           (set/subset? old-ids (set (map :id nodes)))
           (vector? responses)
           (if prior
             (and (= old-ids (set (map :node-id responses)))
                  (distinct-ids? (map :node-id responses))
                  (every? #(and (text? (:response %))
                                (contains? #{"changed" "retained-with-reason"} (tag (:action %)))) responses))
             (empty? responses))))
    (catch Exception _ false)))

(defn review-valid? [auth review]
  (let [plan (get-in auth [:v4/teaching :plan]) nodes (:nodes review)]
    (and (map? review) (= (digest plan) (:plan-digest review))
         (contains? #{"accept" "revise" "cannot-judge"} (tag (:verdict review)))
         (text? (:reason review)) (vector? nodes)
         (= (set (map :id (:nodes plan))) (set (map :node-id nodes)))
         (distinct-ids? (map :node-id nodes))
         (every? #(and (contains? #{"suitable" "revise" "cannot-judge"} (tag (:verdict %)))
                       (contains? #{"none" "retrieval" "applicability" "execution" "unknown"} (tag (:diagnosis %)))
                       (text? (:reason %)) (text? (:instruction %))) nodes)
         (or (not= "accept" (tag (:verdict review)))
             (every? #(= "suitable" (tag (:verdict %))) nodes)))))

(defn use-valid? [auth use]
  (let [plan (get-in auth [:v4/teaching-receipt :plan]) nodes (:nodes use)]
    (and (map? use) (= (digest plan) (:plan-digest use))
         (vector? nodes) (distinct-ids? (map :node-id nodes))
         (= (set (map :id (:nodes plan))) (set (map :node-id nodes)))
         (every? #(and (contains? #{"used" "changed" "unused" "unknown"} (tag (:status %)))
                       (text? (:reason %))) nodes))))

(defn payload-valid? [auth payload]
  (and (= 1 (get-in auth [:v4/teaching :version]))
       (sha? (get-in auth [:v4/teaching :exchange-id]))
       (= (if (= :pattern-plan-review (:phase auth)) :pattern-ta :student) (:role auth))
       (= 0 (:command-own-exit payload))
       (contains? #{"complete" "success"} (tag (:outcome payload)))
       (if (= :pattern-plan-review (:phase auth))
         (review-valid? auth (get-in payload [:evidence :plan-review]))
         (plan-valid? auth (get-in payload [:evidence :plan])))))
