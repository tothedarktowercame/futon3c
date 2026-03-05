(ns futon3c.agents.ase-work-queue
  "ASE (Artificial Stack Exchange) work queue for Tickle — feeds synthetic QA
   generation prompts through the overnight batch pipeline.

   The queue reads ASE work items from futon6/data/ase-queue/entities.json
   (prepared by prepare-ase-tickle-queue.py), pairs each with a review prompt,
   and feeds them through the Tickle orchestrator (assign → review → evidence).

   Two modes:
     1. :generate — ask an agent to produce a synthetic QA thread in
        hypergraph-native format, then review it for correctness.
     2. :answer — given a question from a previous generation pass, ask an
        agent to produce a rigorous answer, then review it.

   Progress is tracked via evidence store: work items with existing ASE evidence
   are skipped on subsequent runs. This makes overnight batch runs resumable.

   Design: follows the exact pattern of tickle_work_queue.clj (CT extraction),
   but for synthetic corpus generation rather than wiring diagram extraction."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]))

;; =============================================================================
;; Paths
;; =============================================================================

(def ^:private ase-queue-dir
  "/home/joe/code/futon6/data/ase-queue")

(def ^:private ase-entities-path
  (str ase-queue-dir "/entities.json"))

(def ^:private ase-review-prompts-path
  (str ase-queue-dir "/review-prompts.json"))

(def ^:private ase-manifest-path
  (str ase-queue-dir "/queue-manifest.json"))

;; =============================================================================
;; Entity loading
;; =============================================================================

(defn- keyword->str
  "Coerce keyword or string to string."
  [x]
  (if (keyword? x) (name x) (str x)))

(defn load-ase-entities
  "Load ASE work items from entities.json (prepared by prepare-ase-tickle-queue.py).
   Returns a vector of maps with :entity-id, :title, :type, :problem, :node-id,
   :gap-severity, :prompt, etc."
  ([] (load-ase-entities ase-entities-path))
  ([path]
   (when (.exists (io/file path))
     (let [raw (json/parse-string (slurp path) true)]
       (->> raw
            (mapv (fn [e]
                    {:entity-id (keyword->str (:entity-id e))
                     :title (:title e)
                     :type (or (:type e) "synthetic-qa")
                     :corpus (or (:corpus e) "ase")
                     :problem (:problem e)
                     :node-id (keyword->str (:node-id e))
                     :instance (:instance e)
                     :gap-severity (:gap-severity e)
                     :prompt (:prompt e)
                     :response-schema (:response-schema e)})))))))

(defn load-review-prompts
  "Load the pre-built review prompts keyed by entity ID."
  ([] (load-review-prompts ase-review-prompts-path))
  ([path]
   (when (.exists (io/file path))
     (json/parse-string (slurp path) true))))

(defn load-manifest
  "Load the ASE queue manifest for metadata/progress info."
  ([] (load-manifest ase-manifest-path))
  ([path]
   (when (.exists (io/file path))
     (json/parse-string (slurp path) true))))

;; =============================================================================
;; Prompt construction
;; =============================================================================

(defn make-generation-prompt
  "Build the generation prompt for an ASE work item.
   The prompt is pre-built by prepare-ase-tickle-queue.py and stored in the entity."
  [entity]
  (str "Runtime surface contract:\n"
       "- Agent: codex-1 (Tickle ASE work queue — generation mode)\n"
       "- Task: Generate synthetic QA thread for proof node: "
       (:node-id entity) "\n"
       "- Problem: P" (:problem entity) "\n"
       "- Output format: hypergraph-native JSON\n"
       "- Your output will be ingested into the ASE store.\n\n"
       (:prompt entity)))

(defn make-review-prompt
  "Build a review prompt for Claude to evaluate a generated synthetic QA thread.
   Uses pre-built review prompts if available, otherwise constructs one."
  [entity generation-result]
  (let [prebuilt (load-review-prompts)
        entity-id (:entity-id entity)]
    (str (or (get prebuilt entity-id)
             (str "Runtime surface contract:\n"
                  "- Agent: claude-1 (Tickle ASE work queue — review mode)\n"
                  "- Task: Review synthetic QA pair for proof node: "
                  (:node-id entity) "\n"
                  "- Your verdict will be recorded as evidence.\n\n"
                  "## Review Criteria\n\n"
                  "1. **Mathematical correctness**: Is the question well-posed? "
                  "Is the answer mathematically rigorous?\n"
                  "2. **Gap targeting**: Does this QA address the identified gap?\n"
                  "3. **Hypergraph quality**: Are nodes/edges well-typed?\n"
                  "4. **Composability**: Would this be useful as retrieval context?\n"
                  "5. **LaTeX quality**: Are formulas correct?\n\n"))
         "\n\n--- Generation Result ---\n\n"
         generation-result "\n\n"
         "--- End ---\n\n"
         "Reply with a JSON object:\n"
         "```json\n"
         "{\"verdict\": \"accept\"|\"revise\"|\"reject\",\n"
         " \"correctness\": 1-5,\n"
         " \"gap_relevance\": 1-5,\n"
         " \"hypergraph_quality\": 1-5,\n"
         " \"notes\": \"free text\"}\n"
         "```\n")))

;; =============================================================================
;; Work item synthesis — convert ASE entities to orchestrator-compatible "issues"
;; =============================================================================

(defn entity->issue
  "Convert an ASE work item into an orchestrator-compatible issue map.
   The 'body' is the generation prompt. The 'number' is derived from the
   entity index (offset to avoid collision with CT and GH issues)."
  [entity idx]
  (let [prompt (make-generation-prompt entity)]
    {:number (+ 20000 idx)  ;; offset above CT range (10000+)
     :title (str "ASE-generate: " (:title entity))
     :body prompt
     :labels ["ase-generation" "tickle-work-queue"]
     ;; Carry entity metadata for evidence emission
     :entity-id (:entity-id entity)
     :node-id (:node-id entity)
     :problem (:problem entity)
     :instance (:instance entity)
     :gap-severity (:gap-severity entity)}))

;; =============================================================================
;; Progress tracking via evidence store
;; =============================================================================

(defn completed-entity-ids
  "Query evidence store for ASE entity IDs that have already been processed.
   Returns a set of entity-id strings."
  [evidence-store]
  (if-not evidence-store
    #{}
    (let [entries (estore/query* evidence-store {})
          ase-entries (->> entries
                          (filter #(some #{:ase-generation} (:evidence/tags %)))
                          (filter #(some #{:workflow-complete :kick-complete}
                                         (:evidence/tags %))))]
      (->> ase-entries
           (keep #(get-in % [:evidence/body :entity-id]))
           set))))

(defn queue-status
  "Return a summary of ASE queue progress.
   {:total N :completed N :remaining N :by-problem {7 {:total N :done N} ...}}"
  [evidence-store]
  (let [entities (or (load-ase-entities) [])
        done (completed-entity-ids evidence-store)
        total (count entities)
        by-problem (->> entities
                        (group-by :problem)
                        (map (fn [[p ents]]
                               [p {:total (count ents)
                                   :done (count (filter #(done (:entity-id %)) ents))
                                   :remaining (count (remove #(done (:entity-id %)) ents))}]))
                        (into (sorted-map)))]
    {:total total
     :completed (count (filter #(done (:entity-id %)) entities))
     :remaining (count (remove #(done (:entity-id %)) entities))
     :completed-ids done
     :by-problem by-problem}))

(defn next-unprocessed
  "Return the next N unprocessed ASE entities as issue maps.
   Skips entities that already have generation evidence.
   Sorts by gap-severity (highest first) so most important gaps go first."
  ([evidence-store] (next-unprocessed evidence-store 10))
  ([evidence-store n]
   (let [entities (or (load-ase-entities) [])
         done (completed-entity-ids evidence-store)]
     (->> entities
          (remove #(done (:entity-id %)))
          (sort-by :gap-severity >)  ;; highest severity first
          (map-indexed (fn [idx e] (entity->issue e idx)))
          (take n)
          vec))))

;; =============================================================================
;; Evidence emission (ASE-specific)
;; =============================================================================

(defn emit-ase-evidence!
  "Emit ASE generation evidence. Wraps the standard evidence format with
   ASE-specific metadata."
  [evidence-store {:keys [entity-id node-id problem instance session-id event-tag
                          generation-result verdict]}]
  (when evidence-store
    (estore/append* evidence-store
                    {:subject {:ref/type :task
                               :ref/id entity-id}
                     :type :coordination
                     :claim-type (case event-tag
                                   :workflow-start :goal
                                   :generation-complete :observation
                                   :review-complete :observation
                                   :workflow-complete :observation
                                   :observation)
                     :author "tickle-1"
                     :tags [:tickle :ase-generation event-tag]
                     :session-id session-id
                     :body (cond-> {:entity-id entity-id
                                    :node-id node-id
                                    :problem problem
                                    :instance instance
                                    :at (str (Instant/now))}
                             generation-result (assoc :result-preview
                                                      (subs generation-result
                                                            0 (min 500 (count generation-result))))
                             verdict (assoc :verdict verdict))})))

;; =============================================================================
;; Batch helpers
;; =============================================================================

(defn entities-by-severity
  "Sort ASE entities by gap severity.
   :desc for most severe first (default), :asc for least severe first."
  ([] (entities-by-severity :desc))
  ([order]
   (let [entities (or (load-ase-entities) [])
         sorted (sort-by :gap-severity entities)]
     (if (= order :desc)
       (reverse sorted)
       sorted))))

(defn entities-by-problem
  "Group ASE entities by problem number. Returns sorted map."
  []
  (->> (or (load-ase-entities) [])
       (group-by :problem)
       (into (sorted-map))))
