(ns futon3c.apm.promotion-review-store
  "Persist an independent promotion judgement and project it onto the exact
  candidate attachment before snapshot publication."
  (:require [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.substrate.client :as substrate])
  (:import [java.time Instant]))

(def ^:private attachment-verdicts #{:approve :reassign :reject})

(defn- review-entry [reviewer review-job review]
  {:evidence/id (:review-evidence-id review)
   :evidence/subject {:ref/type :memory :ref/id (:memory-id review)}
   :evidence/type :memory
   :evidence/claim-type :observation
   :evidence/author reviewer
   :evidence/session-id review-job
   :evidence/at (str (Instant/now))
   :evidence/tags [:memory :memory/attachment-review :apm/promotion-review]
   :evidence/body
   {:review/event :memory-attachment-review
    :review/memory-id (:memory-id review)
    :review/verdict (:verdict review)
    :review/pattern-ids (:pattern-ids review)
    :review/reason (:reason review)
    :review/residual (:residual review)
    :review/provenance {:kind :promotion-review :job-id review-job}}})

(defn- exact-entry? [expected observed]
  (= (dissoc expected :evidence/at) (dissoc observed :evidence/at)))

(defn persist!
  "Persist every attachment-changing review exactly as returned, then apply
  that persisted evidence through memory-lifecycle."
  ([review-result]
   (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
     (persist! review-result
               {:evidence-store backend
                :fetch-entry #(estore/get-entry* backend %)
                :append-entry #(boundary/append! backend %)
                :fetch-hyperedges substrate/hyperedges-by-end
                :post-hyperedge #(memory-write/post-hyperedge!
                                  {:evidence-store backend} %)})))
  ([{:keys [deposit reviewer reviews review-job]}
    {:keys [evidence-store fetch-entry append-entry
            fetch-hyperedges post-hyperedge]}]
   (let [depositor (:depositor deposit)]
     (if-not (and (string? reviewer) (string? depositor)
                  (not= reviewer depositor) (string? review-job))
       {:ok false :error/code :promotion-review-authority-invalid
        :depositor depositor :reviewer reviewer :review-job review-job}
       (loop [remaining (filterv #(contains? attachment-verdicts (:verdict %))
                                  reviews)
              persisted []]
         (if-let [review (first remaining)]
           (let [entry (review-entry reviewer review-job review)
                 review-id (:evidence/id entry)
                 existing (fetch-entry review-id)
                 appended (when-not existing (append-entry entry))
                 observed (fetch-entry review-id)]
             (cond
               (and existing (not (exact-entry? entry existing)))
               {:ok false :error/code :promotion-review-evidence-id-conflict
                :review-evidence-id review-id}
               (and (nil? existing) (not (:ok appended)))
               {:ok false :error/code :promotion-review-evidence-write-failed
                :review-evidence-id review-id :finding appended}
               (not (exact-entry? entry observed))
               {:ok false :error/code :promotion-review-evidence-not-visible
                :review-evidence-id review-id}
               :else
               (let [result
                     (try
                       (lifecycle/review-attachment!
                        {:evidence-store evidence-store :agent-id reviewer
                         :session-id review-job :domain :mathematics}
                        (select-keys review [:memory-id :review-evidence-id
                                             :verdict :pattern-ids])
                        {:fetch-entry fetch-entry
                         :fetch-hyperedges fetch-hyperedges
                         :post-hyperedge post-hyperedge})
                       (catch clojure.lang.ExceptionInfo e
                         {:ok false
                          :error/code :promotion-review-projection-invalid
                          :finding (ex-data e)}))]
                 (if (:ok result)
                   (recur (next remaining) (conj persisted result))
                   (if (:error/code result)
                     result
                     {:ok false
                      :error/code :promotion-review-projection-not-visible
                      :finding result})))))
           {:ok true :reviews reviews :persisted persisted}))))))
