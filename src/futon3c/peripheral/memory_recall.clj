(ns futon3c.peripheral.memory-recall
  "Bounded, pattern-conditioned recall over the shared memory contract.

   Compact projections support candidate search.  Phase 2 may additionally
   request full bodies at the selection seam.  Proposed pattern attachments
   fail closed: an agent-authored endpoint is a curation proposal, not yet a
   warrant."
  (:require [clojure.string :as str]
            [futon2.aif.memory-contract :as memory-contract]
            [futon3c.evidence.store :as evidence-store]
            [futon3c.substrate.client :as substrate]))

(def default-limit 10)
(def max-limit 100)
(def max-batch-endpoints 20)
(def ^:private proposal-stopwords
  #{"a" "an" "and" "before" "for" "in" "of" "on" "the" "to" "using"
    "with"})

(defn- elapsed-ms
  [started-ns]
  (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn- timed
  [f]
  (let [started (System/nanoTime)
        value (f)]
    [value (elapsed-ms started)]))

(defn- project-components
  [domain bounded-limit include-bodies? components]
  (let [result
        (reduce
         (fn [acc {:keys [edge entry]}]
           (let [edge-domain (get-in edge [:hx/props :domain])
                 memory-id (get-in edge [:hx/props :roles :entry])
                 state (or (get-in edge [:hx/props :state]) :current)
                 attachment-status
                 (or (get-in edge [:hx/props :attachment-status])
                     :unreviewed)]
             (cond
               (not= domain edge-domain)
               (update acc :domain-excluded inc)

               (not= :reviewed attachment-status)
               (-> acc
                   (update :attachment-excluded inc)
                   (cond-> (= :proposed attachment-status)
                     (update :proposed-excluded inc)))

               (contains? #{:retracted :superseded} state)
               (update acc :state-excluded inc)

               (not (string? memory-id))
               (update acc :malformed inc)

               (nil? entry)
               (update acc :missing-entry inc)

               (and include-bodies?
                    (not (map? (:evidence/body entry))))
               (update acc :missing-body inc)

               :else
               (try
                 (let [compact
                       (memory-contract/compact-memory
                        {:entry entry
                         :edge edge
                         :domain domain
                         :witness-status
                         (or (get-in edge [:hx/props :witness-status])
                             :unknown)
                         :state state
                         :valid-time (get-in edge [:hx/props :valid-time])
                         :system-time (get-in edge [:hx/props :system-time])})
                       projected
                       (cond-> (assoc compact
                                      :memory/attachment-status
                                      attachment-status)
                         include-bodies?
                         (assoc :memory/body (:evidence/body entry)))]
                   (update acc :memories conj projected))
                 (catch clojure.lang.ExceptionInfo _
                   (update acc :malformed inc))))))
         {:memories [] :domain-excluded 0 :attachment-excluded 0
          :proposed-excluded 0 :state-excluded 0 :malformed 0
          :missing-entry 0 :missing-body 0}
         components)
        memories (->> (:memories result)
                      (sort-by :memory/id)
                      (take bounded-limit)
                      vec)]
    {:memories memories
     :audit {:edge-count (count components)
             :returned-count (count memories)
             :domain-excluded (:domain-excluded result)
             :attachment-excluded (:attachment-excluded result)
             :proposed-excluded (:proposed-excluded result)
             :state-excluded (:state-excluded result)
             :malformed (:malformed result)
             :missing-entry (:missing-entry result)
             :missing-body (:missing-body result)}}))

(defn recall-by-endpoint
  "Recall compact current memories touching ENDPOINT in the explicit DOMAIN.

   CTX requires :evidence-store and :domain. Tests may inject
   :fetch-hyperedges and :fetch-entry through OPTS; production defaults use the
   canonical substrate client and evidence backend. Cross-domain and malformed
   records are counted in audit and never silently returned."
  ([ctx endpoint] (recall-by-endpoint ctx endpoint {}))
  ([{:keys [evidence-store domain]}
    endpoint
    {:keys [limit fetch-hyperedges fetch-entry include-bodies?
            fetch-components valid-as-of system-as-of trace-id]
     :or {limit default-limit}}]
   (when-not (and (keyword? domain)
                  (string? endpoint)
                  (not-empty endpoint)
                  (integer? limit)
                  (pos? limit))
     (throw (ex-info "recall requires explicit domain, endpoint, and positive integer limit"
                     {:domain domain :endpoint endpoint :limit limit})))
   (let [started (System/nanoTime)
         bounded-limit (long (min max-limit limit))
         ;; Filtering happens after the substrate's deterministic id ordering.
         ;; Bounded overfetch prevents an old superseded or unreviewed edge
         ;; from consuming a result slot that should hold a current memory.
         fetch-limit (long (min max-limit (* 3 bounded-limit)))
         injected-fetch-hyperedges? (some? fetch-hyperedges)
         fetch-hyperedges
         (or fetch-hyperedges
             (fn [end opts] (substrate/hyperedges-by-end end opts)))
         current-projection?
         (and (nil? valid-as-of)
              (nil? system-as-of)
              (not injected-fetch-hyperedges?))
         fetch-components (or fetch-components substrate/memory-projection)
         fetch-entry
         (or fetch-entry
             (fn [memory-id]
               (evidence-store/get-entry* evidence-store memory-id)))]
     (try
       (let [[components substrate]
             (if current-projection?
               (let [[response caller-wall-ms]
                     (timed #(fetch-components
                              [endpoint]
                              {:limit fetch-limit :trace-id trace-id}))
                     group (first (:groups response))
                     components
                     (if include-bodies?
                       (mapv
                        (fn [{:keys [edge entry]}]
                          {:edge edge
                           :entry
                           (some->> (:evidence/id entry) fetch-entry)})
                        (:components group))
                       (vec (:components group)))]
                 [components
                  {:audit (:audit response)
                   :timing (:timing response)
                   :caller-wall-ms caller-wall-ms}])
               (let [edges
                     (vec
                      (fetch-hyperedges
                       endpoint
                       (cond-> {:type :memory/assert :limit fetch-limit}
                         valid-as-of (assoc :valid-as-of valid-as-of)
                         system-as-of (assoc :system-as-of system-as-of))))]
                 [(mapv
                   (fn [edge]
                     {:edge edge
                      :entry
                      (some->> (get-in edge [:hx/props :roles :entry])
                               fetch-entry)})
                   edges)
                  nil]))
             {:keys [memories audit]}
             (project-components
              domain bounded-limit include-bodies?
              components)]
         (cond->
          {:ok true
           :endpoint endpoint
           :domain domain
           :temporal-basis
           (cond-> {:mode (if (or valid-as-of system-as-of) :as-of :current)}
             valid-as-of (assoc :valid-as-of (str valid-as-of))
             system-as-of (assoc :system-as-of (str system-as-of)))
           :limit bounded-limit
           :fetch-limit fetch-limit
           :requested-limit limit
           :elapsed-ms (elapsed-ms started)
           :memories memories
           :audit audit}
           substrate (assoc :substrate substrate)))
       (catch Throwable t
         {:ok false
          :endpoint endpoint
          :domain domain
          :limit bounded-limit
          :fetch-limit fetch-limit
          :requested-limit limit
          :elapsed-ms (elapsed-ms started)
          :memories []
          :error {:error/component :shared-memory-recall
                  :error/code :substrate-read-failed
                  :error/message (or (.getMessage t)
                                     "Shared memory substrate read failed")}})))))

(defn recall-by-endpoints
  "Recall compact current memories for several endpoints in one substrate read.

  Search callers use this seam. Full-body selection remains an explicit
  single-endpoint operation so compact search responses never become an
  accidental body cache."
  ([ctx endpoints] (recall-by-endpoints ctx endpoints {}))
  ([{:keys [domain]}
    endpoints
    {:keys [limit fetch-components valid-as-of system-as-of trace-id]
     :or {limit default-limit}}]
   (let [started (System/nanoTime)
         endpoints (vec (distinct endpoints))]
     (when-not (and (keyword? domain)
                    (pos? (count endpoints))
                    (<= (count endpoints) max-batch-endpoints)
                    (every? #(and (string? %) (not (str/blank? %))) endpoints)
                    (integer? limit)
                    (pos? limit))
       (throw (ex-info
               "batch recall requires explicit domain, bounded endpoints, and positive limit"
               {:domain domain :endpoints endpoints :limit limit
                :maximum-endpoints max-batch-endpoints})))
     (let [bounded-limit (long (min max-limit limit))
           fetch-limit (long (min max-limit (* 3 bounded-limit)))
           fetch-components (or fetch-components substrate/memory-projection)]
       (try
         (let [[response caller-substrate-ms]
               (timed
                #(fetch-components
                  endpoints
                  (cond-> {:limit fetch-limit
                           :trace-id trace-id}
                    valid-as-of (assoc :valid-as-of valid-as-of)
                    system-as-of (assoc :system-as-of system-as-of))))
               group-by-endpoint
               (into {} (map (juxt :endpoint identity)) (:groups response))
               recalls
               (mapv
                (fn [endpoint]
                  (let [group (get group-by-endpoint endpoint
                                   {:endpoint endpoint :components []})
                        projection-start (System/nanoTime)
                        {:keys [memories audit]}
                        (project-components domain bounded-limit false
                                            (:components group))
                        projection-ms (elapsed-ms projection-start)]
                    {:ok true
                     :endpoint endpoint
                     :domain domain
                     :temporal-basis (:temporal-basis response)
                     :limit bounded-limit
                     :fetch-limit fetch-limit
                     :requested-limit limit
                     :elapsed-ms projection-ms
                     :timing {:projection-ms projection-ms
                              :shared-substrate-wall-ms caller-substrate-ms}
                     :memories memories
                     :audit (merge audit
                                   {:server-group (:audit group)})}))
                endpoints)]
           {:ok true
            :trace-id (or (:trace-id response) trace-id)
            :domain domain
            :endpoints endpoints
            :limit bounded-limit
            :fetch-limit fetch-limit
            :recalls recalls
            :substrate {:audit (:audit response)
                        :timing (:timing response)
                        :caller-wall-ms caller-substrate-ms}
            :elapsed-ms (elapsed-ms started)})
         (catch Throwable t
           {:ok false
            :trace-id trace-id
            :domain domain
            :endpoints endpoints
            :limit bounded-limit
            :fetch-limit fetch-limit
            :recalls
            (mapv (fn [endpoint]
                    {:ok false
                     :endpoint endpoint
                     :domain domain
                     :limit bounded-limit
                     :fetch-limit fetch-limit
                     :memories []
                     :error {:error/component :shared-memory-recall
                             :error/code :substrate-read-failed
                             :error/message
                             (or (.getMessage t)
                                 "Shared memory substrate batch read failed")}})
                  endpoints)
            :error {:error/component :shared-memory-recall
                    :error/code :substrate-read-failed
                    :error/message
                    (or (.getMessage t)
                        "Shared memory substrate batch read failed")}
            :elapsed-ms (elapsed-ms started)}))))))

(defn- memory-search-rows
  [search-result limit]
  (->> (:results search-result)
       (filter #(= :memory
                   (get-in % [:entry :evidence/type])))
       (take limit)
       vec))

(defn- proposal-tokens
  [query]
  (->> (re-seq #"[A-Za-z0-9_/-]+" (str/lower-case query))
       (remove proposal-stopwords)
       (filter #(>= (count %) 4))
       distinct
       (take 4)
       vec))

(defn- proposals-from-rows
  [domain bounded-limit recall-batch-fn rows trace-id]
  (let [memory-ids (->> rows
                        (keep #(get-in % [:entry :evidence/id]))
                        (filter string?)
                        distinct
                        vec)
        batch
        (when (seq memory-ids)
          (recall-batch-fn {:domain domain}
                           memory-ids
                           {:limit bounded-limit
                            :trace-id trace-id}))
        recall-by-memory
        (into {} (map (juxt :endpoint identity)) (:recalls batch))
        proposals
        (reduce
         (fn [acc {:keys [score entry]}]
           (let [memory-id (:evidence/id entry)
                 memories (:memories (get recall-by-memory memory-id))]
             (reduce
              (fn [inner memory]
                (reduce
                 (fn [m pattern-id]
                   (if (and (string? pattern-id) (not-empty pattern-id))
                     (update m pattern-id
                             (fn [prior]
                               (let [support
                                     {:memory-id memory-id
                                      :fts-score score
                                      :hook (or (get-in entry
                                                        [:evidence/body :hook])
                                                (:memory/hook memory))}]
                                 (-> (or prior
                                         {:pattern-id pattern-id
                                          :source
                                          :reviewed-memory-lexical-proposal
                                          :memory-support []})
                                     (update :memory-support conj support)))))
                     m))
                 inner
                 (:memory/pattern-ids memory)))
              acc
              memories)))
         {}
         rows)]
    {:proposals proposals
     :validation batch}))

(defn- batch-audit
  [batch]
  (when batch
    (select-keys batch
                 [:ok :trace-id :endpoints :limit :fetch-limit
                  :substrate :elapsed-ms :error])))

(defn propose-patterns-by-query
  "Bounded lexical proposal lane for pattern construction.

   The FTS result is not itself a warrant. Each candidate must lead through a
   currently reviewed memory/assert edge to a pattern endpoint in DOMAIN.
   This turns a task phrase into candidate pattern ids without an embedding
   fallback or an unbounded graph scan."
  ([ctx query] (propose-patterns-by-query ctx query {}))
  ([{:keys [domain]}
    query
    {:keys [limit search-evidence recall-batch-fn trace-id]
     :or {limit 10}}]
   (when-not (and (keyword? domain)
                  (string? query)
                  (not-empty query)
                  (integer? limit)
                  (pos? limit))
     (throw (ex-info "query proposal requires domain, query, and positive limit"
                     {:domain domain :query query :limit limit})))
   (let [started (System/nanoTime)
         bounded-limit (long (min max-limit limit))
         search-evidence (or search-evidence substrate/evidence-text-search)
         recall-batch-fn (or recall-batch-fn recall-by-endpoints)
         ;; FTS indexes all evidence, so boundedly overfetch before retaining
         ;; memory entries. This avoids spending graph reads on transcripts
         ;; that happen to quote the query while preserving a hard cap.
         [search-result primary-fts-ms]
         (timed #(search-evidence
                  query {:limit (min max-limit (* 3 bounded-limit))
                         :trace-id trace-id}))
         primary-rows (memory-search-rows search-result bounded-limit)
         primary-result
         (proposals-from-rows
          domain bounded-limit recall-batch-fn primary-rows trace-id)
         primary-proposals (:proposals primary-result)
         fallback-tokens
         (when (empty? primary-proposals) (proposal-tokens query))
         [fallback-result fallback-fts-ms]
         (when (seq fallback-tokens)
           (timed #(search-evidence
                    (str/join " OR " fallback-tokens)
                    {:limit (min max-limit (* 3 bounded-limit))
                     :trace-id trace-id})))
         fallback-rows
         (when fallback-result
           (memory-search-rows fallback-result bounded-limit))
         fallback-proposal-result
         (when fallback-result
           (proposals-from-rows
            domain bounded-limit recall-batch-fn fallback-rows trace-id))
         proposals
         (if (seq primary-proposals)
           primary-proposals
           (:proposals fallback-proposal-result))
         rows (if (seq primary-proposals) primary-rows fallback-rows)]
     {:ok true
      :trace-id trace-id
      :query query
      :limit bounded-limit
      :checked-memory-count (count rows)
      :query-strategy (if (seq primary-proposals)
                        :full-query
                        :bounded-token-disjunction)
      :fallback-tokens (vec fallback-tokens)
      :index-as-of (:index-as-of search-result)
      :validation
      {:primary (batch-audit (:validation primary-result))
       :fallback (batch-audit (:validation fallback-proposal-result))}
      :timing
      {:primary-fts-ms primary-fts-ms
       :primary-validation-ms
       (some-> primary-result :validation :elapsed-ms)
       :fallback-fts-ms fallback-fts-ms
       :fallback-validation-ms
       (some-> fallback-proposal-result :validation :elapsed-ms)
       :proposal-total-ms (elapsed-ms started)}
      :candidates (->> (vals proposals)
                       (sort-by (comp str :pattern-id))
                       vec)})))
