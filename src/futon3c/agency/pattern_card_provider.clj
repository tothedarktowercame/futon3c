(ns futon3c.agency.pattern-card-provider
  "Cached prompt-line provider for the most recent exact-session retrieval."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.evidence.store :as estore]
            [futon3c.mission-control.service :as mcs])
  (:import [java.time Duration Instant]))

(def stale-after (Duration/ofMinutes 30))
(defonce ^:private !retrievals (atom {}))
(defonce ^:private !refreshing (atom #{}))

(defn active-pattern-card
  "P10 hook. A card, when implemented, takes precedence over retrieval."
  [_ctx]
  nil)

(defn- field [m k]
  (or (get m k) (get m (name k))))

(defn- body-map [entry]
  (let [body (:evidence/body entry)]
    (cond
      (map? body) body
      (string? body) (try
                       (let [parsed (edn/read-string body)]
                         (when (map? parsed) parsed))
                       (catch Throwable _ nil))
      :else nil)))

(defn- context-retrieval?
  [entry]
  (let [body (body-map entry)]
    (= "context-retrieval" (field body :event))))

(defn observe-entry!
  "Feed one persisted evidence entry into the exact-session cache."
  [entry]
  (when (context-retrieval? entry)
    (let [agent (str (:evidence/author entry))
          session (some-> (:evidence/session-id entry) str)
          results (vec (or (field (body-map entry) :results) []))]
      (when (and (not (str/blank? agent)) (not (str/blank? session)) (seq results))
        (swap! !retrievals
               (fn [cache]
                 (let [prior (get cache [agent session])
                       at (str (:evidence/at entry))]
                   (if (and prior (pos? (compare (:observed-at prior) at)))
                     cache
                     (assoc cache [agent session]
                            {:evidence-id (:evidence/id entry)
                             :observed-at at
                             :results results})))))))))

(defn reset-cache! [] (reset! !retrievals {}))

(defn refresh!
  "Refresh an exact seat from the evidence LIST seam. Intended for startup/tests,
   never for the render path. query-fn receives an EvidenceQuery."
  ([agent session] (refresh! agent session nil))
  ([agent session query-fn]
   (let [query-fn (or query-fn
                      (fn [q]
                        (estore/query* (or (:evidence-store @mcs/!config) estore/!store) q)))
         entries (query-fn {:query/tags [:context-retrieval]
                            :query/author (str agent)
                            :query/session-id (str session)
                            :query/limit 100})
         exact (filter #(and (= (str agent) (str (:evidence/author %)))
                             (= (str session) (str (:evidence/session-id %)))
                             (context-retrieval? %)) entries)
         latest (last (sort-by #(str (:evidence/at %)) exact))]
     (when latest (observe-entry! latest))
     latest)))

(defn refresh-async! [agent session]
  (let [key [(str agent) (str session)]]
    (when-not (contains? @!refreshing key)
      (swap! !refreshing conj key)
      (future
        (try (refresh! agent session)
             (catch Throwable _)
             (finally (swap! !refreshing disj key)))))))

(defn- fresh? [render-at observed-at]
  (try
    (let [age (Duration/between (Instant/parse observed-at) (Instant/parse render-at))]
      (and (not (.isNegative age)) (neg? (.compareTo age stale-after))))
    (catch Throwable _ false)))

(defn- result-field [result k] (or (get result k) (get result (name k))))

(defn provider
  [{:keys [agent-id session-id render-at] :as ctx}]
  (or (active-pattern-card ctx)
      (let [key [(str agent-id) (str session-id)]
            cached (get @!retrievals key)]
        (if-not cached
          (do (refresh-async! agent-id session-id) nil)
          (if-not (fresh? (str render-at) (:observed-at cached))
            (do (refresh-async! agent-id session-id) nil)
            (let [ranked (sort-by #(long (or (result-field % :rank) Long/MAX_VALUE))
                                  (:results cached))
                  top (first ranked)
                  id (some-> (result-field top :id) str)
                  score (result-field top :score)
                  also (keep #(some-> (result-field % :id) str) (take 2 (rest ranked)))]
              (when-not (str/blank? id)
                {:segment/id :pattern
                 :segment/value (str "~" id)
                 :segment/provider "futon3c.agency.pattern-card-provider/provider"
                 :segment/observed-at (:observed-at cached)
                 :segment/basis {:evidence-ref (:evidence-id cached)
                                 :scope {:agent-id (str agent-id)
                                         :session-id (str session-id)}}
                 :segment/header
                 (str "retrieved " id " " score
                      (when (seq also) (str "; also " (str/join ", " also))))})))))))

(defn register! []
  (prompt-line/register-provider!
   {:segment/id :pattern
    :provider "futon3c.agency.pattern-card-provider/provider"
    :fn provider
    :budget-ms 100}))

(register!)
