(ns futon3c.peripheral.memory-lifecycle
  "Append-first lifecycle operations for shared memories.

   Challenges and correcting episodes are durable evidence. The memory/assert
   edge is the bitemporal projection: it may become challenged, superseded, or
   end-valid-time retracted without deleting either episode."
  (:require [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.substrate.client :as substrate])
  (:import [java.time Duration Instant]
           [java.util UUID]))

(defn- now []
  (Instant/now))

(defn- instant
  [x]
  (cond
    (instance? Instant x) x
    (number? x) (Instant/ofEpochMilli (long x))
    (and (string? x) (not (str/blank? x))) (Instant/parse x)
    :else nil))

(defn correction-lag-ms
  "Elapsed time between when the correction became valid and when it landed."
  [valid-from recorded-at]
  (let [valid-from (instant valid-from)
        recorded-at (instant recorded-at)]
    (when (and valid-from recorded-at)
      (max 0 (.toMillis (Duration/between valid-from recorded-at))))))

(defn retrieval-to-use-ms
  "Elapsed time from recall projection to controller use receipt."
  [surfaced-at used-at]
  (let [surfaced-at (instant surfaced-at)
        used-at (instant used-at)]
    (when (and surfaced-at used-at)
      (max 0 (.toMillis (Duration/between surfaced-at used-at))))))

(defn- append-challenge!
  [{:keys [evidence-store agent-id session-id domain]}
   {:keys [memory-id reason outcome-id valid-from action correcting-memory-id]}]
  (let [id (str "e-" (UUID/randomUUID))
        recorded-at (now)
        entry
        {:evidence/id id
         :evidence/subject {:ref/type :memory :ref/id memory-id}
         :evidence/type :memory
         :evidence/claim-type :challenge
         :evidence/author (str agent-id)
         :evidence/session-id (str session-id)
         :evidence/at (str recorded-at)
         :evidence/body
         (cond-> {:event :memory-challenge
                  :memory/id memory-id
                  :memory/domain domain
                  :memory/reason reason
                  :memory/action action
                  :memory/valid-from (str valid-from)
                  :memory/correction-lag-ms
                  (correction-lag-ms valid-from recorded-at)}
           outcome-id (assoc :memory/outcome-id outcome-id)
           correcting-memory-id
           (assoc :memory/correcting-memory-id correcting-memory-id))
         :evidence/tags [:memory :memory/challenge]}
        receipt (boundary/append! evidence-store entry)]
    (if (:ok receipt)
      {:ok true :id id :recorded-at recorded-at :entry entry}
      {:ok false :id id :error receipt})))

(defn- current-edge
  [memory-id fetch-hyperedges]
  (->> (fetch-hyperedges memory-id {:type :memory/assert :limit 10})
       (filter #(= memory-id (get-in % [:hx/props :roles :entry])))
       first))

(defn- validate-edge!
  [ctx memory-id edge]
  (when-not edge
    (throw (ex-info "memory/assert edge not found" {:memory-id memory-id})))
  (when-not (= (:domain ctx) (get-in edge [:hx/props :domain]))
    (throw (ex-info "memory lifecycle domain mismatch"
                    {:memory-id memory-id
                     :requested-domain (:domain ctx)
                     :edge-domain (get-in edge [:hx/props :domain])})))
  edge)

(def ^:private review-verdicts
  #{:approve :challenge :reject})

(def ^:private witness-statuses
  #{:self-asserted :independently-witnessed})

(defn- nonblank-string?
  [x]
  (and (string? x) (not (str/blank? x))))

(defn- exact-patterns?
  [expected actual]
  (and (= (count expected) (count actual))
       (= (set expected) (set actual))))

(defn- validate-review!
  [memory-id pattern-ids verdict memory-entry review-entry]
  (when-not review-entry
    (throw (ex-info "memory attachment review evidence not found"
                    {:memory-id memory-id})))
  (let [body (:evidence/body review-entry)
        reviewer (:evidence/author review-entry)
        author (:evidence/author memory-entry)
        reviewed-patterns (:review/pattern-ids body)]
    (when-not memory-entry
      (throw (ex-info "memory evidence entry not found"
                      {:memory-id memory-id})))
    (when-not (and (nonblank-string? reviewer)
                   (not= reviewer author))
      (throw (ex-info "memory author cannot review their own attachment"
                      {:memory-id memory-id
                       :memory-author author
                       :reviewer reviewer})))
    (when-not (= {:ref/type :memory :ref/id memory-id}
                 (:evidence/subject review-entry))
      (throw (ex-info "review evidence subject does not name the memory"
                      {:memory-id memory-id
                       :subject (:evidence/subject review-entry)})))
    (when-not (and (= :memory-attachment-review (:review/event body))
                   (= memory-id (:review/memory-id body))
                   (= verdict (:review/verdict body))
                   (exact-patterns? pattern-ids reviewed-patterns)
                   (some? (:review/provenance body))
                   (nonblank-string? (:evidence/at review-entry)))
      (throw (ex-info "review evidence does not certify the exact attachment"
                      {:memory-id memory-id
                       :pattern-ids pattern-ids
                       :verdict verdict
                       :review-body body})))
    (when (and (= :approve verdict)
               (not (contains? witness-statuses
                               (:review/witness-status body))))
      (throw (ex-info "approval must state its supported witness status"
                      {:memory-id memory-id
                       :witness-status (:review/witness-status body)})))
    {:reviewer reviewer
     :witness-status (:review/witness-status body)}))

(defn- review-visible?
  [memory-id review-evidence-id attachment-status fetch-hyperedges]
  (let [edge (current-edge memory-id fetch-hyperedges)]
    (and edge
         (= attachment-status
            (get-in edge [:hx/props :attachment-status]))
         (= review-evidence-id
            (get-in edge [:hx/props :review :evidence-id])))))

(defn review-attachment!
  "Review one proposed pattern attachment using separately authored evidence.

   The Futon1b hyperedge POST synchronously invalidates its query cache.  This
   operation additionally performs a fresh endpoint read and refuses success
   until that read observes the new review version, making the cache contract
   an explicit postcondition."
  ([ctx request] (review-attachment! ctx request {}))
  ([ctx {:keys [memory-id review-evidence-id verdict pattern-ids] :as request}
    {:keys [fetch-hyperedges fetch-entry post-hyperedge]
     :or {fetch-hyperedges substrate/hyperedges-by-end
          fetch-entry #(estore/get-entry* (:evidence-store ctx) %)
          post-hyperedge memory-write/post-hyperedge!}}]
   (when-not (and (nonblank-string? memory-id)
                  (nonblank-string? review-evidence-id)
                  (contains? review-verdicts verdict)
                  (vector? pattern-ids)
                  (seq pattern-ids)
                  (every? nonblank-string? pattern-ids)
                  (keyword? (:domain ctx)))
     (throw (ex-info "attachment review requires ids, patterns, verdict, and domain"
                     {:request request :domain (:domain ctx)})))
   (let [edge (validate-edge! ctx memory-id
                              (current-edge memory-id fetch-hyperedges))
         edge-patterns (get-in edge [:hx/props :roles :patterns])
         memory-entry (fetch-entry memory-id)
         review-entry (fetch-entry review-evidence-id)
         {:keys [reviewer witness-status]}
         (validate-review! memory-id pattern-ids verdict memory-entry
                           review-entry)
         existing-review (get-in edge [:hx/props :review])
         attachment-status (case verdict
                             :approve :reviewed
                             :challenge :challenged
                             :reject :rejected)]
     (when-not (exact-patterns? edge-patterns pattern-ids)
       (throw (ex-info "review pattern set does not match attachment"
                       {:memory-id memory-id
                        :edge-patterns edge-patterns
                        :review-patterns pattern-ids})))
     (when-not (= :current (get-in edge [:hx/props :state]))
       (throw (ex-info "only a current memory attachment can be reviewed"
                       {:memory-id memory-id
                        :state (get-in edge [:hx/props :state])})))
     (if (= review-evidence-id (:evidence-id existing-review))
       (if (and (= verdict (:verdict existing-review))
                (exact-patterns? pattern-ids (:pattern-ids existing-review)))
         {:ok (review-visible? memory-id review-evidence-id
                               attachment-status fetch-hyperedges)
          :memory-id memory-id
          :review-evidence-id review-evidence-id
          :attachment-status attachment-status
          :idempotent-replay? true
          :cache-postcondition :fresh-endpoint-read}
         (throw (ex-info "review evidence id was reused with a changed verdict"
                         {:memory-id memory-id
                          :review-evidence-id review-evidence-id
                          :existing-review existing-review
                          :request request})))
       (let [reviewed-at (:evidence/at review-entry)
             review {:evidence-id review-evidence-id
                     :reviewer reviewer
                     :verdict verdict
                     :pattern-ids pattern-ids
                     :reviewed-at reviewed-at}
             updated
             (cond-> (-> edge
                         (assoc-in [:hx/props :attachment-status]
                                   attachment-status)
                         (assoc-in [:hx/props :review] review)
                         (update-in [:hx/props :review-history]
                                    (fnil conj []) review)
                         (assoc-in [:hx/props :system-time] reviewed-at))
               (= :approve verdict)
               (assoc-in [:hx/props :witness-status] witness-status))
             projection (post-hyperedge ctx updated)]
         (if-not (:ok projection)
           {:ok false
            :stage :review-projection
            :memory-id memory-id
            :review-evidence-id review-evidence-id
            :projection projection}
           (let [visible? (review-visible? memory-id review-evidence-id
                                           attachment-status
                                           fetch-hyperedges)]
             {:ok visible?
              :stage (if visible? :complete :cache-postcondition)
              :memory-id memory-id
              :review-evidence-id review-evidence-id
              :attachment-status attachment-status
              :witness-status (when (= :approve verdict) witness-status)
              :reviewer reviewer
              :cache-postcondition
              (if visible?
                :fresh-endpoint-read
                :stale-after-successful-repost)
              :projection projection})))))))

(defn challenge-memory!
  "Record a failed or disputed use, then project the memory as challenged.

   OUTCOME-ID remains a separately witnessed record. This operation neither
   deletes the episode nor converts the failed use into success training."
  ([ctx request] (challenge-memory! ctx request {}))
  ([ctx {:keys [memory-id reason outcome-id valid-from] :as request}
    {:keys [fetch-hyperedges post-hyperedge]
     :or {fetch-hyperedges substrate/hyperedges-by-end
          post-hyperedge memory-write/post-hyperedge!}}]
   (when-not (and (string? memory-id) (not (str/blank? memory-id))
                  (string? reason) (not (str/blank? reason))
                  (keyword? (:domain ctx)))
     (throw (ex-info "challenge requires memory-id, reason, and domain"
                     {:request request :domain (:domain ctx)})))
   (let [valid-from (or (instant valid-from) (now))
         edge (validate-edge! ctx memory-id
                              (current-edge memory-id fetch-hyperedges))
         challenge (append-challenge!
                    ctx {:memory-id memory-id
                         :reason reason
                         :outcome-id outcome-id
                         :valid-from valid-from
                         :action :challenged})]
     (if-not (:ok challenge)
       challenge
       (let [updated
             (-> edge
                 (assoc :hx/valid-time (str valid-from))
                 (assoc-in [:hx/props :state] :challenged)
                 (assoc-in [:hx/props :witness-status] :challenged)
                 (assoc-in [:hx/props :challenge-id] (:id challenge))
                 (assoc-in [:hx/props :valid-time] (str valid-from))
                 (assoc-in [:hx/props :system-time]
                           (str (:recorded-at challenge))))
             projection (post-hyperedge ctx updated)]
         {:ok (:ok projection)
          :memory-id memory-id
          :state :challenged
          :challenge-id (:id challenge)
          :outcome-id outcome-id
          :valid-from (str valid-from)
          :correction-lag-ms
          (correction-lag-ms valid-from (:recorded-at challenge))
          :projection projection})))))

(defn supersede-memory!
  "Record a correcting assert episode and supersede the original projection.

   The correcting edge inherits reviewed attachment status only when it points
   at the same reviewed pattern endpoints as the original."
  ([ctx request] (supersede-memory! ctx request {}))
  ([ctx {:keys [memory-id reason outcome-id valid-from correction] :as request}
    {:keys [fetch-hyperedges post-hyperedge record-memory]
     :or {fetch-hyperedges substrate/hyperedges-by-end
          post-hyperedge memory-write/post-hyperedge!
          record-memory memory-write/record-memory!}}]
   (when-not (map? correction)
     (throw (ex-info "supersession requires a correcting memory payload"
                     {:request request})))
   (let [valid-from (or (instant valid-from) (now))
         original (validate-edge! ctx memory-id
                                  (current-edge memory-id fetch-hyperedges))
         correction-receipt (record-memory ctx correction)]
     (if-not (:ok correction-receipt)
       {:ok false :stage :correction-entry :receipt correction-receipt}
       (let [correcting-id (:id correction-receipt)
             correcting-edge
             (validate-edge! ctx correcting-id
                             (current-edge correcting-id fetch-hyperedges))
             original-patterns
             (set (get-in original [:hx/props :roles :patterns]))
             correcting-patterns
             (set (get-in correcting-edge [:hx/props :roles :patterns]))
             reviewed-inheritance?
             (and (= :reviewed
                     (get-in original [:hx/props :attachment-status]))
                  (seq original-patterns)
                  (= original-patterns correcting-patterns))
             corrected-projection
             (cond-> (-> correcting-edge
                         (assoc :hx/valid-time (str valid-from))
                         (assoc-in [:hx/props :state] :current)
                         (assoc-in [:hx/props :valid-time] (str valid-from)))
               reviewed-inheritance?
               (assoc-in [:hx/props :attachment-status] :reviewed))
             correction-projection
             (post-hyperedge ctx corrected-projection)]
         (if-not (:ok correction-projection)
           {:ok false :stage :correction-projection
            :correcting-memory-id correcting-id
            :projection correction-projection}
           (let [challenge
                 (append-challenge!
                  ctx {:memory-id memory-id
                       :reason reason
                       :outcome-id outcome-id
                       :valid-from valid-from
                       :action :superseded
                       :correcting-memory-id correcting-id})]
             (if-not (:ok challenge)
               {:ok false :stage :challenge
                :correcting-memory-id correcting-id
                :challenge challenge}
               (let [superseded
                     (-> original
                         (assoc :hx/valid-time (str valid-from))
                         (assoc-in [:hx/props :state] :superseded)
                         (assoc-in [:hx/props :superseded-by] correcting-id)
                         (assoc-in [:hx/props :challenge-id] (:id challenge))
                         (assoc-in [:hx/props :valid-time] (str valid-from))
                         (assoc-in [:hx/props :system-time]
                                   (str (:recorded-at challenge))))
                     original-projection (post-hyperedge ctx superseded)]
                 {:ok (:ok original-projection)
                  :memory-id memory-id
                  :state :superseded
                  :correcting-memory-id correcting-id
                  :challenge-id (:id challenge)
                  :valid-from (str valid-from)
                  :correction-lag-ms
                  (correction-lag-ms valid-from (:recorded-at challenge))
                  :reviewed-inheritance? reviewed-inheritance?
                  :projection original-projection})))))))))

(defn retract-memory!
  "Record the reason, then end the memory/assert edge at VALID-FROM."
  ([ctx request] (retract-memory! ctx request {}))
  ([ctx {:keys [memory-id reason valid-from]}
    {:keys [fetch-hyperedges post-hyperedge]
     :or {fetch-hyperedges substrate/hyperedges-by-end
          post-hyperedge memory-write/post-hyperedge!}}]
   (let [valid-from (or (instant valid-from) (now))
         edge (validate-edge! ctx memory-id
                              (current-edge memory-id fetch-hyperedges))
         challenge (append-challenge!
                    ctx {:memory-id memory-id :reason reason
                         :valid-from valid-from :action :retracted})]
     (if-not (:ok challenge)
       challenge
       (let [projection
             (post-hyperedge
              ctx (assoc edge :hx/op "retract"
                         :hx/valid-time (str valid-from)))]
         {:ok (:ok projection)
          :memory-id memory-id
          :state :retracted
          :challenge-id (:id challenge)
          :valid-from (str valid-from)
          :projection projection})))))
