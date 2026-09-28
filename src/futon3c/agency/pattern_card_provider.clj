(ns futon3c.agency.pattern-card-provider
  "Cached prompt-line provider for the most recent exact-session retrieval."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.pattern-card-acts :as card-acts]
            [futon3c.agency.pattern-card-record :as card-record]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.agency.rule-record :as hx-store]
            [futon3c.evidence.store :as estore]
            [futon3c.mission-control.service :as mcs])
  (:import [java.net URLEncoder]
           [java.time Duration Instant]))

(def stale-after (Duration/ofMinutes 30))
(def card-stale-after
  "A card is shown for this long after its last refresh. It is not the
   withdrawal latency: every render rechecks the seat at most once per
   `recheck-interval-ms`, so a withdrawal is seen within about a minute of the
   next render.  At 2 minutes, the render at the end of any turn longer than
   that (refresh at turn start, render at turn end) hid an active card."
  (Duration/ofMinutes 30))
(defonce ^:private !retrievals (atom {}))
(defonce ^:private !refreshing (atom #{}))
(defonce ^:private !checked-at (atom {}))
(defonce ^:private !cards (atom {}))
(defonce ^:private !card-refreshing (atom #{}))

(def recheck-interval-ms
  "At most one background LIST read per seat per interval. Until the dev
   namespace's observe-entry! hook is loaded, this recheck is what brings a newer
   retrieval into the cache; it also stops a seat with no retrievals, or a stale
   one, from querying futon1b on every render."
  60000)

(defn- fresh-within? [render-at observed-at duration]
  (try
    (let [age (Duration/between (Instant/parse observed-at) (Instant/parse render-at))]
      (and (not (.isNegative age)) (neg? (.compareTo age duration))))
    (catch Throwable _ false)))

(defn- cache-card-entry! [agent session entry]
  (let [key [(str agent) (str session)]]
    (swap! !cards
           (fn [cache]
             (let [prior (get cache key)]
               (if (and prior
                        (pos? (compare (:observed-at prior) (:observed-at entry))))
                 cache
                 (assoc cache key entry)))))))

(defn publish-card-result!
  "Publish a card-as-of result already verified outside the render path.
   The CLI runs in another JVM, so calling this there cannot update the live
   cache; this seam is for a later in-JVM write route and tests."
  [agent session result observed-at]
  (when (and (not (str/blank? (str agent)))
             (not (str/blank? (str session)))
             (map? result))
    (cache-card-entry! agent session
                       {:result result :observed-at (str observed-at)})))

(defn cached-card [agent session]
  (get @!cards [(str agent) (str session)]))

(defn active-pattern-card
  "Return the cached active-card segment. Performs no I/O."
  [{:keys [agent-id session-id render-at]}]
  (when-let [{:keys [result observed-at]}
             (get @!cards [(str agent-id) (str session-id)])]
    (when-let [card (when (fresh-within? (str render-at) observed-at card-stale-after)
                     (:active result))]
      (let [pattern-id (str (:pattern-id card))
            act-id (str (:id card))]
        (when (and (not (str/blank? pattern-id)) (not (str/blank? act-id)))
          {:segment/id :pattern
           :segment/value (str "~" pattern-id)
           :segment/provider "futon3c.agency.pattern-card-provider/provider"
           :segment/observed-at observed-at
           :segment/basis {:evidence-ref act-id
                           :scope {:agent-id (str agent-id)
                                   :session-id (str session-id)
                                   :basis-status :pattern-card}}
           :segment/header (str "card " pattern-id " (" act-id ")")})))))

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
                             :basis-status :persisted
                             :observed-at at
                             :results results})))))))))

(defn reset-cache! []
  (reset! !retrievals {})
  (reset! !cards {})
  (reset! !refreshing #{})
  (reset! !card-refreshing #{})
  (reset! !checked-at {}))

(defn observe-results!
  "Publish exact-seat results before the durable evidence append completes."
  [agent session results observed-at evidence-id basis-status]
  (when (and (not (str/blank? (str agent)))
             (not (str/blank? (str session)))
             (seq results))
    (swap! !retrievals assoc [(str agent) (str session)]
           {:evidence-id evidence-id
            :basis-status basis-status
            :observed-at (str observed-at)
            :results (vec results)})))

(defn cached-retrieval
  "Return the inspectable exact-seat cache entry used for latency probes."
  [agent session]
  (get @!retrievals [(str agent) (str session)]))

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

(defn- encode [value] (URLEncoder/encode (str value) "UTF-8"))

(defn- default-card-query [type endpoint system-as-of]
  (let [path (str "/api/alpha/hyperedges?type=" (encode (subs (str type) 1))
                  "&end=" (encode endpoint)
                  "&limit=1000&include-total=false&valid-as-of=" (encode system-as-of)
                  "&system-as-of=" (encode system-as-of))]
    (:hyperedges (hx-store/request! "http://127.0.0.1:7073" "GET" path nil))))

(defn refresh-cards!
  "Refresh an exact-seat card projection. QUERY-FN receives
   [type endpoint system-as-of] and returns hyperedges. Selections are read by
   exact session endpoint, then withdrawals by each matching selection act id.
   Malformed historical documents are reported in the cache entry and excluded
   from the projection."
  ([agent session] (refresh-cards! agent session default-card-query))
  ([agent session query-fn]
   (let [system-as-of (str (Instant/now))
         read-edge (fn [edge]
                     (try
                       {:record (card-record/hyperedge->record edge)}
                       (catch clojure.lang.ExceptionInfo e
                         {:unreadable {:hx/id (:hx/id edge)
                                       :reason (:reason (ex-data e))}})))
         selection-edges (or (query-fn :pattern-card/selection
                                       (str "session:" session) system-as-of) [])
         parsed-selections (mapv read-edge selection-edges)
         seat-selections (->> parsed-selections
                              (keep :record)
                              (filter #(and (= (str agent) (str (:agent %)))
                                            (= (str session) (str (:session %)))))
                              vec)
         withdrawal-edges (mapcat #(or (query-fn :act/withdrawal
                                                  (:id %) system-as-of) [])
                                  seat-selections)
         parsed-withdrawals (mapv read-edge withdrawal-edges)
         records (concat seat-selections (keep :record parsed-withdrawals))
         unreadable (into (vec (keep :unreadable parsed-selections))
                          (keep :unreadable parsed-withdrawals))
         result (card-acts/card-as-of records (str agent) (str session) system-as-of)
         entry {:result result :observed-at system-as-of :unreadable unreadable}]
     (cache-card-entry! agent session entry)
     entry)))

(defn refresh-cards-async! [agent session]
  (let [key [(str agent) (str session)]]
    (when-not (contains? @!card-refreshing key)
      (swap! !card-refreshing conj key)
      (future
        (try (refresh-cards! agent session)
             (catch Throwable _)
             (finally (swap! !card-refreshing disj key)))))))

(defn- recheck-async!
  [agent session]
  (let [key [(str agent) (str session)]
        now (System/currentTimeMillis)
        before @!checked-at]
    (when (and (>= (- now (get before key 0)) recheck-interval-ms)
               (compare-and-set! !checked-at before (assoc before key now)))
      (refresh-async! agent session)
      (refresh-cards-async! agent session))))

(defn- fresh? [render-at observed-at]
  (fresh-within? render-at observed-at stale-after))

(defn- result-field [result k] (or (get result k) (get result (name k))))

(defn provider
  [{:keys [agent-id session-id render-at] :as ctx}]
  (recheck-async! agent-id session-id)
  (or (active-pattern-card ctx)
      (let [key [(str agent-id) (str session-id)]
            cached (get @!retrievals key)]
        (if-not cached
          nil
          (if-not (fresh? (str render-at) (:observed-at cached))
            nil
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
                                         :session-id (str session-id)
                                         :basis-status (:basis-status cached)}}
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
