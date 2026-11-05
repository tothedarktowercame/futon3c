(ns futon3c.agency.promise-log
  "Operational hyperedge copy of prepared promise-history outbox entries."
  (:require [clojure.string :as str]
            [futon3c.agency.rule-record :as store]
            [futon3c.substrate.client :as substrate])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(defn hyperedge-id
  "Derive the stable operational id without changing promise-history identity.
   Rows from 2026-09-27/28 carry the first id format, e-<uuid> (2,207 rows);
   some of their chains are still open, so they map to promise-log:e-<uuid>."
  [evidence-id]
  (cond
    (and (string? evidence-id) (str/starts-with? evidence-id "promise-history:"))
    (str "promise-log:" (subs evidence-id (count "promise-history:")))

    (and (string? evidence-id) (re-matches #"e-[0-9a-f-]{36}" evidence-id))
    (str "promise-log:" evidence-id)

    :else
    (throw (ex-info "Promise history id has the wrong form"
                    {:reason :invalid-promise-history-id :evidence/id evidence-id}))))

(defn entry->hyperedge
  "Turn one fixed promise-history entry into its lossless operational edge."
  [entry]
  (let [body (:evidence/body entry)
        promise-id (:history/promise-id body)
        agent (:evidence/author entry)]
    (when-not (and (string? promise-id) (not (str/blank? promise-id))
                   (string? agent) (not (str/blank? agent)))
      (throw (ex-info "Promise history entry lacks operational endpoints"
                      {:reason :invalid-promise-history-entry
                       :evidence/id (:evidence/id entry)})))
    {:hx/id (hyperedge-id (:evidence/id entry))
     :hx/type :promise/history-event
     :hx/endpoints [promise-id agent]
     :hx/valid-time (:evidence/at entry)
     :hx/props (cond-> {:history/payload-edn (:history/payload-edn body)
                        :history/promise-id promise-id
                        :history/promise-sequence (:history/promise-sequence body)
                        :history/transition-type (:evidence/type entry)
                        :history/event-time (:evidence/at entry)
                        :history/evidence-id (:evidence/id entry)}
                 (some? (:history/predecessor body))
                 (assoc :history/predecessor (:history/predecessor body)))}))

(def ^:dynamic *post-fn*
  (fn [edge]
    (store/request! (substrate/configured-url) "POST" "/api/alpha/hyperedge" edge)))

(def ^:dynamic *get-fn*
  (fn [id]
    (store/request! (substrate/configured-url) "GET"
                    (str "/api/alpha/hyperedge/"
                         (URLEncoder/encode id "UTF-8"))
                    nil)))

(defn same-edge?
  "Compare the immutable fields written by this adapter."
  [expected actual]
  (= (select-keys expected [:hx/id :hx/type :hx/endpoints :hx/props])
     (select-keys actual [:hx/id :hx/type :hx/endpoints :hx/props])))

(defn write!
  "Write ENTRY's operational edge and require exact readback. Identical replay
   succeeds even when the store reports the stable id as already present."
  [entry]
  (let [edge (entry->hyperedge entry)
        post-error (try
                     (*post-fn* edge)
                     nil
                     (catch Throwable error error))
        stored (try
                 (*get-fn* (:hx/id edge))
                 (catch Throwable read-error
                   (throw (ex-info "Promise log write/readback failed"
                                   {:reason :promise-log-unavailable
                                    :hx/id (:hx/id edge)
                                    :post-error (some-> post-error ex-message)}
                                   read-error))))]
    (when-not (same-edge? edge stored)
      (throw (ex-info "Promise log exact readback mismatch"
                      {:reason :promise-log-readback-mismatch
                       :hx/id (:hx/id edge)})))
    {:ok true :hyperedge stored :replayed? (boolean post-error)}))

(def max-history-rows
  "Hard bound matching the evidence reader's 20 pages of 1,000 rows."
  20000)

(def ^:dynamic *read-hyperedges-fn* substrate/hyperedges-by-type)

(defn hyperedge->entry
  "Rebuild the evidence-shaped chain projection from one operational edge."
  [edge]
  (when-not (= :promise/history-event (:hx/type edge))
    (throw (ex-info "Not a promise history hyperedge"
                    {:reason :invalid-promise-log-type :hx/id (:hx/id edge)})))
  (let [props (:hx/props edge)
        endpoints (:hx/endpoints edge)]
    {:evidence/id (:history/evidence-id props)
     :evidence/type (:history/transition-type props)
     :evidence/author (second endpoints)
     :evidence/at (:history/event-time props)
     :evidence/body (cond-> {:history/format 3
                             :history/payload-edn (:history/payload-edn props)
                             :history/promise-id (:history/promise-id props)
                             :history/promise-sequence (:history/promise-sequence props)}
                      (some? (:history/predecessor props))
                      (assoc :history/predecessor (:history/predecessor props)))}))

(defn read-history
  "Read the complete bounded operational promise-history population. OPTIONS
   accepts {:mode :current}, or {:mode :as-of :at <ISO instant>}. The latter
   pins both temporal axes supported by futon1b's hyperedge route."
  ([] (read-history {:mode :current}))
  ([{:keys [mode at] :or {mode :current}}]
   (when-not (contains? #{:current :as-of} mode)
     (throw (ex-info "Invalid promise log read mode" {:reason :invalid-mode})))
   (when (and (= :as-of mode)
              (not (and (string? at)
                        (try (Instant/parse at) true (catch Throwable _ false)))))
     (throw (ex-info "As-of promise log reads require an instant"
                     {:reason :invalid-as-of})))
   (let [options (cond-> {:limit max-history-rows
                          :page-size 1000
                          :request-budget 20}
                   (= :as-of mode) (assoc :valid-as-of at :system-as-of at))
         edges (*read-hyperedges-fn* :promise/history-event options)]
     (when (substrate/partial-result? edges)
       (throw (ex-info "Promise log input exceeded its bounded read"
                       (merge {:reason :truncated-input :source :promise-log
                               :rows (count edges)}
                              (meta edges)))))
     (mapv hyperedge->entry edges))))
