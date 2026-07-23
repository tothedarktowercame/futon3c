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
(def ^:private proposal-stopwords
  #{"a" "an" "and" "before" "for" "in" "of" "on" "the" "to" "using"
    "with"})

(defn- elapsed-ms
  [started-ns]
  (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

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
            valid-as-of system-as-of]
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
         fetch-hyperedges
         (or fetch-hyperedges
             (fn [end opts] (substrate/hyperedges-by-end end opts)))
         fetch-entry
         (or fetch-entry
             (fn [memory-id]
               (evidence-store/get-entry* evidence-store memory-id)))]
     (try
       (let [edges (vec (fetch-hyperedges
                         endpoint (cond-> {:type :memory/assert
                                          :limit fetch-limit}
                                    valid-as-of
                                    (assoc :valid-as-of valid-as-of)
                                    system-as-of
                                    (assoc :system-as-of system-as-of))))
             result
             (reduce
              (fn [acc edge]
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

                    :else
                    (if-let [entry (fetch-entry memory-id)]
                      (if (and include-bodies?
                               (not (map? (:evidence/body entry))))
                        (update acc :missing-body inc)
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
                            (update acc :malformed inc))))
                      (update acc :missing-entry inc)))))
              {:memories [] :domain-excluded 0 :attachment-excluded 0
               :proposed-excluded 0 :state-excluded 0 :malformed 0
               :missing-entry 0 :missing-body 0}
              edges)
             memories (->> (:memories result)
                           (sort-by :memory/id)
                           (take bounded-limit)
                           vec)]
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
          :audit {:edge-count (count edges)
                  :returned-count (count memories)
                  :domain-excluded (:domain-excluded result)
                  :attachment-excluded (:attachment-excluded result)
                  :proposed-excluded (:proposed-excluded result)
                  :state-excluded (:state-excluded result)
                  :malformed (:malformed result)
                  :missing-entry (:missing-entry result)
                  :missing-body (:missing-body result)}})
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
  [domain bounded-limit fetch-hyperedges rows]
  (reduce
   (fn [acc {:keys [score entry]}]
     (let [memory-id (:evidence/id entry)
           edges (if (string? memory-id)
                   (fetch-hyperedges
                    memory-id {:type :memory/assert
                               :limit bounded-limit})
                   [])]
       (reduce
        (fn [inner edge]
          (let [props (:hx/props edge)
                state (or (:state props) :current)
                status (or (:attachment-status props) :unreviewed)
                patterns (get-in props [:roles :patterns])]
            (if (and (= domain (:domain props))
                     (= :reviewed status)
                     (not (contains? #{:retracted :superseded} state)))
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
                                              (:hook props))}]
                               (-> (or prior
                                       {:pattern-id pattern-id
                                        :source
                                        :reviewed-memory-lexical-proposal
                                        :memory-support []})
                                   (update :memory-support conj support)))))
                   m))
               inner
               patterns)
              inner)))
        acc
        edges)))
   {}
   rows))

(defn propose-patterns-by-query
  "Bounded lexical proposal lane for pattern construction.

   The FTS result is not itself a warrant. Each candidate must lead through a
   currently reviewed memory/assert edge to a pattern endpoint in DOMAIN.
   This turns a task phrase into candidate pattern ids without an embedding
   fallback or an unbounded graph scan."
  ([ctx query] (propose-patterns-by-query ctx query {}))
  ([{:keys [domain]}
    query
    {:keys [limit search-evidence fetch-hyperedges]
     :or {limit 10}}]
   (when-not (and (keyword? domain)
                  (string? query)
                  (not-empty query)
                  (integer? limit)
                  (pos? limit))
     (throw (ex-info "query proposal requires domain, query, and positive limit"
                     {:domain domain :query query :limit limit})))
   (let [bounded-limit (long (min max-limit limit))
         search-evidence (or search-evidence substrate/evidence-text-search)
         fetch-hyperedges
         (or fetch-hyperedges
             (fn [end opts] (substrate/hyperedges-by-end end opts)))
         ;; FTS indexes all evidence, so boundedly overfetch before retaining
         ;; memory entries. This avoids spending graph reads on transcripts
         ;; that happen to quote the query while preserving a hard cap.
         search-result
         (search-evidence query {:limit (min max-limit
                                            (* 3 bounded-limit))})
         primary-rows (memory-search-rows search-result bounded-limit)
         primary-proposals
         (proposals-from-rows
          domain bounded-limit fetch-hyperedges primary-rows)
         fallback-tokens
         (when (empty? primary-proposals) (proposal-tokens query))
         fallback-result
         (when (seq fallback-tokens)
           (search-evidence
            (str/join " OR " fallback-tokens)
            {:limit (min max-limit (* 3 bounded-limit))}))
         fallback-rows
         (when fallback-result
           (memory-search-rows fallback-result bounded-limit))
         proposals
         (if (seq primary-proposals)
           primary-proposals
           (proposals-from-rows
            domain bounded-limit fetch-hyperedges fallback-rows))
         rows (if (seq primary-proposals) primary-rows fallback-rows)]
     {:ok true
      :query query
      :limit bounded-limit
      :checked-memory-count (count rows)
      :query-strategy (if (seq primary-proposals)
                        :full-query
                        :bounded-token-disjunction)
      :fallback-tokens (vec fallback-tokens)
      :index-as-of (:index-as-of search-result)
      :candidates (->> (vals proposals)
                       (sort-by (comp str :pattern-id))
                       vec)})))
