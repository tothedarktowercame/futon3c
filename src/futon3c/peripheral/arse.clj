(ns futon3c.peripheral.arse
  "ArSE (Artificial Stack Exchange) peripheral — corpus-backed QA for agents.

   A 'library card' peripheral: agents hop in to query the mathematical corpus,
   retrieve structured answers, and hop back to their proof work. Unlike the
   proof peripheral (which manages ledger state), the ArSE peripheral is
   stateless across sessions — it's a lookup service, not a state machine.

   Tools:
   - :arse-query   — search corpus for relevant threads/papers
   - :arse-browse  — list available corpus sources and their status
   - :arse-history — review queries made in this session

   State tracks: query history, retrieved threads (cached for re-reading).

   The search is delegated to proof-backend's corpus-check infrastructure
   (futon3a MiniLM embeddings + StackExchange keyword search + arXiv)."
  (:require [futon3c.peripheral.common :as common]
            [futon3c.peripheral.evidence :as evidence]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard :as bb]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.time Instant]))

;; =============================================================================
;; State shape
;; =============================================================================

(defn- initial-state
  [context]
  {:session-id (:session-id context)
   :author (common/resolve-author context)
   :problem-id (:problem-id context)
   ;; Query tracking
   :queries []           ;; [{:at Instant :query str :top-k int :result-count int}]
   :cached-results {}    ;; {query-text -> full result map}
   ;; Evidence
   :evidence-store (:evidence-store context)
   :last-evidence-id nil})

;; =============================================================================
;; Tool implementations
;; =============================================================================

(defn- invoke-corpus-agent
  "Try to invoke corpus-1 via Agency WS for deep retrieval (BGE + FAISS).
   Returns result map or nil if corpus-1 is not available."
  [query-text opts]
  (try
    (when-let [agent (reg/get-agent "corpus-1")]
      (when-let [invoke-fn (or (:agent/invoke-fn agent) (:invoke-fn agent))]
        (let [request (json/generate-string
                        {:query query-text
                         :top_k (or (:top-k opts) 5)
                         :sources (:sources opts)})
              {:keys [result]} (invoke-fn request nil)]
          (when result
            (try (json/parse-string result true)
                 (catch Exception _ nil))))))
    (catch Exception _ nil)))

(defn- merge-local-and-remote
  "Merge local corpus-check results with remote corpus-1 results.
   Deduplicates by :id, keeps highest score."
  [local-neighbors remote-neighbors top-k]
  (let [by-id (reduce (fn [m n]
                         (let [id (or (:id n) (:title n) (str n))]
                           (if (> (get n :score 0) (get-in m [id :score] 0))
                             (assoc m id n)
                             m)))
                       {}
                       (concat local-neighbors remote-neighbors))]
    (->> (vals by-id)
         (sort-by #(- (get % :score 0)))
         (take top-k)
         vec)))

(defn- tool-query
  "Search the corpus for relevant threads/papers.
   args: [query-text] or [query-text {:top-k N :sources [...]}]
   Searches locally (MiniLM + keyword) and remotely (corpus-1 BGE + FAISS)
   when available, merging results."
  [state args config]
  (let [query-text (first args)
        opts (or (second args) {})
        top-k (or (:top-k opts) 5)]
    (cond
      (or (nil? query-text) (str/blank? (str query-text)))
      {:result {:error "Query text is required. Ask a specific math question."}}

      :else
      (let [;; Local search (MiniLM + keyword — always available on Linode)
            local-result (#'pb/tool-corpus-check nil nil [query-text opts] config)
            local-neighbors (get-in local-result [:result :neighbors] [])
            local-sources (get-in local-result [:result :sources] [])
            ;; Remote search (corpus-1 BGE + FAISS — when laptop is connected)
            remote-result (invoke-corpus-agent query-text opts)
            remote-neighbors (get remote-result :neighbors [])
            remote-ok? (and remote-result (get remote-result :ok))
            ;; Merge results
            all-neighbors (if remote-ok?
                            (merge-local-and-remote local-neighbors remote-neighbors top-k)
                            local-neighbors)
            sources (cond-> (vec local-sources)
                      remote-ok? (conj :corpus-1))
            now (str (Instant/now))
            entry {:at now
                   :query query-text
                   :top-k top-k
                   :result-count (count all-neighbors)
                   :sources sources
                   :remote? remote-ok?}]
        {:result {:neighbors all-neighbors
                  :query query-text
                  :top-k top-k
                  :sources sources
                  :remote-available? remote-ok?}
         :state-updates {:queries (conj (:queries state) entry)
                         :cached-results (assoc (:cached-results state)
                                                query-text {:neighbors all-neighbors
                                                            :sources sources})}}))))

(defn- tool-browse
  "List available corpus sources and their current status.
   No args required."
  [_state _args config]
  (let [;; Probe local sources
        probe-result (#'pb/tool-corpus-check nil nil ["test" {:top-k 1}] config)
        local-status (get-in probe-result [:result :source-status] [])
        ;; Check if corpus-1 is available (laptop WS agent)
        corpus-agent (reg/get-agent "corpus-1")
        corpus-available? (boolean corpus-agent)]
    {:result {:sources (conj (vec local-status)
                             {:source :corpus-1
                              :available? corpus-available?
                              :note "Laptop BGE + FAISS retrieval (via WS)"})
              :note "Use :arse-query to search. When corpus-1 is connected, deep retrieval (BGE rerank + FAISS structural neighbors) is automatic."}}))

(defn- tool-history
  "Review queries made in this session."
  [state _args]
  {:result {:query-count (count (:queries state))
            :queries (mapv (fn [{:keys [at query result-count sources]}]
                             {:at at :query query :results result-count :sources sources})
                           (:queries state))}})

(defn- dispatch-tool
  "Dispatch an ArSE tool action."
  [state {:keys [tool args]} config]
  (case tool
    :arse-query   (tool-query state args config)
    :arse-browse  (tool-browse state args config)
    :arse-history (tool-history state args)
    {:result {:error (str "Unknown ArSE tool: " tool)}}))

;; =============================================================================
;; PeripheralRunner
;; =============================================================================

(def arse-tools
  "Tools available in the ArSE peripheral."
  #{:arse-query :arse-browse :arse-history})

(defrecord ArsePeripheral [spec backend config]
  runner/PeripheralRunner

  (start [_ context]
    (if-let [err (runner/validate-context :arse context #{:session-id})]
      err
      (let [sid (:session-id context)
            state (initial-state context)
            author (common/resolve-author context)
            ev (evidence/make-start-evidence :arse sid author)
            append-err (common/maybe-append-evidence! state ev)]
        (if append-err
          append-err
          {:ok true
           :state (assoc state :last-evidence-id (:evidence/id ev))
           :evidence ev}))))

  (step [_ state action]
    (if-let [err (common/validate-action :arse action)]
      err
      (let [tool (:tool action)
            args (or (:args action) [])]
        (if-not (contains? arse-tools tool)
          (runner/runner-error :arse :tool-not-allowed
                               (str "Tool not available in ArSE: " tool)
                               :tool tool
                               :available arse-tools)
          (let [{:keys [result state-updates]} (dispatch-tool state {:tool tool :args args} config)
                new-state (merge state state-updates)
                ev (evidence/make-step-evidence
                    :arse
                    (:session-id state)
                    (:author state)
                    tool
                    args
                    result
                    (:last-evidence-id state))
                new-state (assoc new-state :last-evidence-id (:evidence/id ev))
                new-state (update new-state :steps
                                  (fnil conj [])
                                  {:tool tool :result result :at (str (Instant/now))})]
            (bb/project! :arse new-state)
            (common/maybe-append-evidence! new-state ev)
            {:ok true :state new-state :result result :evidence ev})))))

  (stop [_ state reason]
    (let [fruit {:query-count (count (:queries state))
                 :queries (mapv #(select-keys % [:query :result-count]) (:queries state))}
          ev (evidence/make-stop-evidence
              :arse
              (:session-id state)
              (:author state)
              fruit
              reason
              (:last-evidence-id state))
          append-err (common/maybe-append-evidence! state ev)]
      (if append-err
        append-err
        {:ok true
         :context {:session-id (:session-id state)
                   :problem-id (:problem-id state)}
         :fruit fruit
         :evidence ev}))))

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-arse
  "Create an ArSE peripheral.

   Config options (optional):
   - :cwd — working directory (for locating corpus files)
   - :stackexchange-jsonl-paths — override paths to local SE data
   - :notions-search-script — path to futon3a search script"
  ([backend]
   (make-arse backend {}))
  ([backend config]
   (->ArsePeripheral
    {:peripheral/id :arse
     :peripheral/tools arse-tools
     :peripheral/scope :corpus-search
     :peripheral/entry #{:from-proof :from-any :user-request}
     :peripheral/exit #{:user-request :found-answer :hop-proof}
     :peripheral/context {:session-id :inherit :problem-id :optional}}
    backend
    (or config {}))))
