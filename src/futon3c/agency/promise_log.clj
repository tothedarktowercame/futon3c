(ns futon3c.agency.promise-log
  "Operational hyperedge copy of prepared promise-history outbox entries."
  (:require [clojure.string :as str]
            [futon3c.agency.rule-record :as store]
            [futon3c.substrate.client :as substrate])
  (:import [java.net URLEncoder]))

(defn hyperedge-id
  "Derive the stable operational id without changing promise-history identity."
  [evidence-id]
  (when-not (and (string? evidence-id)
                 (str/starts-with? evidence-id "promise-history:"))
    (throw (ex-info "Promise history id has the wrong form"
                    {:reason :invalid-promise-history-id :evidence/id evidence-id})))
  (str "promise-log:" (subs evidence-id (count "promise-history:"))))

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
