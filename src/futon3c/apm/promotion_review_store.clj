(ns futon3c.apm.promotion-review-store
  "Persist an independent promotion judgement and project it onto the exact
  candidate attachment before snapshot publication."
  (:require [futon3c.evidence.boundary :as boundary]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.substrate.client :as substrate])
  (:import [java.time Instant]))

(def ^:private attachment-verdicts #{:approve :reassign :reject})

(defn- nonblank? [value]
  (and (string? value) (not-empty value)))

(defn canonical-review
  "Replace an agent-reported evidence name with a controller-owned identity.
  The reported name remains evidence about the submission, never authority."
  ([review-job review] (canonical-review review-job [] review))
  ([review-job candidates review]
   (let [reported-patterns (:pattern-ids review)
         candidate-patterns
         (:pattern-ids
          (some #(when (= (:memory-id review) (:memory-id %)) %)
                candidates))
         effective-patterns
         (if (and (= :reject (:verdict review)) (empty? reported-patterns))
           candidate-patterns
           reported-patterns)]
     (when (and (nonblank? review-job)
                (nonblank? (:memory-id review))
                (contains? attachment-verdicts (:verdict review))
                (vector? effective-patterns)
                (seq effective-patterns)
                (every? nonblank? effective-patterns))
       (let [identity-digest
             (machine/ledger-digest
              [{:review-job review-job
                :memory-id (:memory-id review)
                :verdict (:verdict review)
                :reason (:reason review)
                :residual (:residual review)
                :pattern-ids effective-patterns}])]
         (assoc review
                :reported-review-evidence-id (:review-evidence-id review)
                :reported-pattern-ids reported-patterns
                :pattern-ids effective-patterns
                :review-evidence-id
                (str "e-apm-promotion-review-"
                     (subs identity-digest 0 32))))))))

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
    :review/reported-evidence-id (:reported-review-evidence-id review)
    :review/reported-pattern-ids (:reported-pattern-ids review)
    :review/provenance {:kind :promotion-review :job-id review-job}}})

(defn- normalize-review-body [body]
  (let [body (if (and (not (contains? body :review/reported-pattern-ids))
                      (not= :reject (:review/verdict body)))
               (assoc body :review/reported-pattern-ids
                      (:review/pattern-ids body))
               body)]
    (cond-> body
      (nil? (:review/reported-evidence-id body))
      (dissoc :review/reported-evidence-id)
      (empty? (:review/reported-pattern-ids body))
      (dissoc :review/reported-pattern-ids))))

(defn- comparable-entry [entry]
  (-> entry
      (dissoc :evidence/at)
      (update :evidence/body normalize-review-body)))

(defn- exact-entry? [expected observed]
  (= (comparable-entry expected) (comparable-entry observed)))

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
                :post-hyperedge (fn [_ edge]
                                  (memory-write/post-hyperedge!
                                   {:evidence-store backend} edge))})))
  ([{:keys [deposit reviewer reviews review-job]}
    {:keys [evidence-store fetch-entry append-entry
            fetch-hyperedges post-hyperedge]}]
   (let [depositor (:depositor deposit)]
     (if-not (and (string? reviewer) (string? depositor)
                  (not= reviewer depositor) (string? review-job))
       {:ok false :error/code :promotion-review-authority-invalid
        :depositor depositor :reviewer reviewer :review-job review-job}
       (let [attachment-reviews
             (filterv #(contains? attachment-verdicts (:verdict %)) reviews)
             canonical (mapv #(canonical-review review-job
                                                (:candidates deposit) %)
                             attachment-reviews)]
         (if (some nil? canonical)
           {:ok false :error/code :promotion-review-identity-invalid
            :findings
            (mapv (fn [review]
                    {:memory-id (:memory-id review)
                     :verdict (:verdict review)
                     :reported-review-evidence-id
                     (:review-evidence-id review)})
                  (keep #(when-not (canonical-review review-job
                                                     (:candidates deposit) %)
                           %)
                        attachment-reviews))}
           (loop [remaining canonical
                  persisted []]
         (if-let [review (first remaining)]
           (let [entry (review-entry reviewer review-job review)
                 review-id (:evidence/id entry)
                 existing (fetch-entry review-id)
                 appended (when-not existing (append-entry entry))
                 ;; boundary/append! returns :entry only after its own durable
                 ;; backend readback.  The substrate query projection can lag
                 ;; that receipt briefly, so retain the verified delivery in
                 ;; this transaction instead of declaring a false absence.
                 delivered (when (:ok appended) (:entry appended))
                 observed (or (fetch-entry review-id) delivered)
                 fetch-verified
                 (fn [evidence-id]
                   (if (= review-id evidence-id)
                     (or (fetch-entry evidence-id) observed)
                     (fetch-entry evidence-id)))]
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
                        {:fetch-entry fetch-verified
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
             {:ok true
              :reviews (into (filterv #(not (contains? attachment-verdicts
                                                       (:verdict %)))
                                      reviews)
                             canonical)
              :persisted persisted}))))))))
