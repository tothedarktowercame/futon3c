(ns futon3c.agents.arse-work-queue
  "ArSE (Artificial Stack Exchange) work queue for Tickle — feeds synthetic
   QA generation prompts through the orchestrator for ArSE Tickling.

   The queue reads work items from futon6/data/arse-queue/entities.json,
   where each item already contains its generation prompt (built by
   generate-synthetic-qa.py). Review prompts come from review-prompts.json.

   Progress is tracked via evidence store: items with existing ArSE evidence
   are skipped on subsequent runs. This makes overnight batch runs resumable.

   Design: this module provides work items and prompts. The actual agent
   invocation flows through tickle_orchestrate.clj's existing machinery."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]))

;; =============================================================================
;; Paths
;; =============================================================================

(def ^:private arse-queue-dir
  "/home/joe/code/futon6/data/arse-queue")

(def ^:private arse-entities-path
  (str arse-queue-dir "/entities.json"))

(def ^:private arse-review-prompts-path
  (str arse-queue-dir "/review-prompts.json"))

;; =============================================================================
;; Entity loading
;; =============================================================================

(defn load-arse-entities
  "Load ArSE work items from entities.json.
   Returns a vector of maps with :entity-id, :title, :prompt, :problem, etc."
  ([] (load-arse-entities arse-entities-path))
  ([path]
   (let [raw (json/parse-string (slurp path) true)]
     (->> raw
          (mapv (fn [e]
                  {:entity-id (keyword (:entity-id e))
                   :entity-id-str (:entity-id e)
                   :title (:title e)
                   :type (:type e)
                   :corpus (keyword (or (:corpus e) :arse))
                   :problem (:problem e)
                   :node-id (:node-id e)
                   :instance (:instance e)
                   :gap-severity (:gap-severity e)
                   :prompt (:prompt e)
                   :response-schema (:response-schema e)}))))))

(defn load-review-prompts
  "Load the review prompts keyed by entity ID."
  ([] (load-review-prompts arse-review-prompts-path))
  ([path]
   (when (.exists (io/file path))
     (json/parse-string (slurp path) true))))

;; =============================================================================
;; Work item synthesis
;; =============================================================================

(defn entity->issue
  "Convert an ArSE entity into an orchestrator-compatible issue map.
   The prompt is already embedded in the entity — no template assembly needed."
  [entity idx]
  {:number (+ 20000 idx)  ;; offset: 10000 = CT, 20000 = ArSE
   :title (:title entity)
   :body (:prompt entity)
   :labels ["arse-generation" "tickle-work-queue"]
   :entity-id (:entity-id-str entity)
   :problem (:problem entity)
   :node-id (:node-id entity)
   :gap-severity (:gap-severity entity)})

(defn make-review-prompt
  "Build a review prompt for Claude to evaluate a generated QA pair."
  [entity generation-result]
  (let [review-prompts (load-review-prompts)
        base-review (get review-prompts (:entity-id-str entity))]
    (str (or base-review
             (str "Runtime surface contract:\n"
                  "- Agent: claude-1 (ArSE Tickling — review mode)\n"
                  "- Task: Review synthetic QA for: " (:node-id entity) "\n"
                  "- Your verdict will be recorded as evidence.\n\n"))
         "\n--- Generated QA Result ---\n\n"
         generation-result "\n\n"
         "--- End ---\n\n"
         "Review this synthetic QA thread. Reply with:\n"
         "- APPROVE if mathematically correct, well-targeted, and well-structured\n"
         "- REQUEST_CHANGES if there are errors (explain what)\n"
         "- UNCLEAR if you cannot determine (explain why)\n")))

;; =============================================================================
;; Progress tracking via evidence store
;; =============================================================================

(defn completed-entity-ids
  "Query evidence store for ArSE entity IDs already processed.
   Returns a set of entity-id strings."
  [evidence-store]
  (if-not evidence-store
    #{}
    (let [entries (estore/query* evidence-store {})
          arse-entries (->> entries
                           (filter #(some #{:arse-generation} (:evidence/tags %)))
                           (filter #(some #{:workflow-complete :kick-complete}
                                          (:evidence/tags %))))]
      (->> arse-entries
           (keep #(get-in % [:evidence/body :entity-id]))
           set))))

(defn queue-status
  "Return a summary of ArSE queue progress.
   Optional :problem filters by problem number."
  [evidence-store & {:keys [problem]}]
  (let [entities (cond->> (load-arse-entities)
                   problem (filter #(= problem (:problem %))))
        done (completed-entity-ids evidence-store)
        total (count entities)]
    {:total total
     :completed (count (filter #(done (:entity-id-str %)) entities))
     :remaining (count (remove #(done (:entity-id-str %)) entities))
     :completed-ids done
     :problem (or problem :all)}))

(defn next-unprocessed
  "Return the next N unprocessed ArSE entities as issue maps.
   Optional :problem filters by problem number."
  ([evidence-store] (next-unprocessed evidence-store 10))
  ([evidence-store n & {:keys [problem]}]
   (let [entities (cond->> (load-arse-entities)
                    problem (filter #(= problem (:problem %))))
         done (completed-entity-ids evidence-store)]
     (->> entities
          (map-indexed (fn [idx e]
                         (when-not (done (:entity-id-str e))
                           (entity->issue e idx))))
          (remove nil?)
          (take n)
          vec))))

;; =============================================================================
;; Evidence emission (ArSE-specific)
;; =============================================================================

(defn emit-arse-evidence!
  "Emit ArSE generation evidence."
  [evidence-store {:keys [entity-id problem node-id session-id event-tag
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
                     :tags [:tickle :arse-generation event-tag]
                     :session-id session-id
                     :body (cond-> {:entity-id entity-id
                                    :problem problem
                                    :node-id node-id
                                    :at (str (Instant/now))}
                             generation-result (assoc :result-preview
                                                      (subs generation-result
                                                            0 (min 500 (count generation-result))))
                             verdict (assoc :verdict verdict))})))

;; =============================================================================
;; Batch helpers
;; =============================================================================

(defn entities-by-problem
  "Group entities by problem number."
  []
  (group-by :problem (load-arse-entities)))

(defn queue-manifest
  "Load the queue manifest for metadata."
  []
  (let [path (str arse-queue-dir "/queue-manifest.json")]
    (when (.exists (io/file path))
      (json/parse-string (slurp path) true))))
