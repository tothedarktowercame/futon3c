(ns futon3c.agents.tickle-work-queue
  "CT work queue for Tickle — feeds PlanetMath category theory entries
   through the wiring diagram extraction pipeline.

   The queue reads 313 CT entries from futon6/data/ct-validation/entities.json,
   pairs each with the golden extraction taxonomy prompt, and feeds them through
   the existing Tickle orchestrator (assign → review → evidence).

   Progress is tracked via evidence store: entries with existing extraction
   evidence are skipped on subsequent runs. This makes overnight batch runs
   resumable — restart after a crash and it picks up where it left off.

   Design: this module provides the work items and prompts. The actual agent
   invocation flows through tickle_orchestrate.clj's existing machinery."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]))

;; =============================================================================
;; Paths
;; =============================================================================

(def ^:private ct-entities-path
  "/home/joe/code/futon6/data/ct-validation/entities.json")

(def ^:private ct-golden-dir
  "/home/joe/code/futon6/data/ct-validation/golden")

(def ^:private planetmath-ct-dir
  "/home/joe/code/planetmath/18_Category_theory_homological_algebra")

;; =============================================================================
;; Entity loading
;; =============================================================================

(defn load-ct-entities
  "Load the 313 CT entities from entities.json.
   Returns a vector of maps with :entity-id, :title, :type, :source-file, etc."
  ([] (load-ct-entities ct-entities-path))
  ([path]
   (let [raw (json/parse-string (slurp path) true)]
     (->> raw
          (mapv (fn [e]
                  {:entity-id (:entity_id e)
                   :title (:title e)
                   :type (:type e)
                   :source-file (:source_file e)
                   :canonicalname (:canonicalname e)
                   :body-length (:body_length e)
                   :ner-count (:ner_count e)
                   :wire-count (:wire_count e)
                   :port-count (:port_count e)
                   :scope-count (:scope_count e)
                   :msc-codes (:msc_codes e)
                   :defines (:defines e)}))))))

(defn load-tex-body
  "Load the full .tex body for an entity from the PlanetMath corpus."
  ([entity] (load-tex-body entity planetmath-ct-dir))
  ([entity pm-dir]
   (let [f (io/file pm-dir (:source-file entity))]
     (when (.exists f)
       (slurp f)))))

;; =============================================================================
;; Prompt construction
;; =============================================================================

(def ^:private taxonomy-prompt
  "The wiring diagram extraction taxonomy — shared across all entries.
   Loaded once from the first golden prompt file (everything before ## YOUR TASK)."
  (delay
    (let [golden-files (->> (.listFiles (io/file ct-golden-dir))
                            (filter #(str/ends-with? (.getName %) ".prompt.txt"))
                            sort)
          first-golden (first golden-files)]
      (when first-golden
        (let [full-text (slurp first-golden)
              ;; Extract everything up to and including the YOUR TASK header
              ;; but before the actual TEXT: section
              idx (str/index-of full-text "\n\nTEXT:")]
          (if idx
            (subs full-text 0 idx)
            ;; Fallback: take up to "## YOUR TASK" + the task instruction
            (let [task-idx (str/index-of full-text "## YOUR TASK")]
              (if task-idx
                (str (subs full-text 0 task-idx)
                     "## YOUR TASK\n\n"
                     "Analyze the following mathematical text. Extract ALL components "
                     "(30 scope types), ports (11 anaphora types), and wires "
                     "(5 connective types + optional labels). Classify each using the "
                     "types above. Return the JSON object with three arrays.")
                full-text))))))))

(defn make-extraction-prompt
  "Build a wiring diagram extraction prompt for a CT entity.
   Combines the golden taxonomy with the entity's .tex body."
  [entity & {:keys [pm-dir] :or {pm-dir planetmath-ct-dir}}]
  (let [taxonomy @taxonomy-prompt
        body (load-tex-body entity pm-dir)]
    (when (and taxonomy body)
      (str taxonomy "\n\nTEXT:\n" body))))

(defn make-review-prompt
  "Build a review prompt for Claude to check an extraction result.
   Includes the original text, the extraction output, and ground truth counts."
  [entity extraction-result & {:keys [pm-dir] :or {pm-dir planetmath-ct-dir}}]
  (let [body (load-tex-body entity pm-dir)]
    (str "Runtime surface contract:\n"
         "- Agent: claude-1 (Tickle CT work queue — review mode)\n"
         "- Task: Review wiring diagram extraction for PlanetMath entry: "
         (:title entity) "\n"
         "- Your verdict will be recorded as evidence.\n\n"
         "--- Ground Truth Counts (classical baseline) ---\n"
         "Components (scopes): " (:scope-count entity) "\n"
         "Wires: " (:wire-count entity) "\n"
         "Ports: " (:port-count entity) "\n"
         "NER terms: " (:ner-count entity) "\n\n"
         "--- Original Text (first 2000 chars) ---\n\n"
         (when body (subs body 0 (min 2000 (count body)))) "\n\n"
         "--- Extraction Result ---\n\n"
         extraction-result "\n\n"
         "--- End ---\n\n"
         "Review this wiring diagram extraction. Check:\n"
         "1. Is the JSON valid with components/ports/wires arrays?\n"
         "2. Are the component counts in the right ballpark vs ground truth?\n"
         "3. Are component types from the valid taxonomy (30 types)?\n"
         "4. Are wire types from the valid set (5 types + 32 labels)?\n"
         "5. Do parent pointers form a valid tree?\n\n"
         "Respond with:\n"
         "- APPROVE if the extraction is reasonable\n"
         "- REQUEST_CHANGES if there are structural errors (explain what)\n"
         "- UNCLEAR if you cannot determine (explain why)\n")))

;; =============================================================================
;; Work item synthesis — convert CT entities to orchestrator-compatible "issues"
;; =============================================================================

(defn entity->issue
  "Convert a CT entity into an orchestrator-compatible issue map.
   The 'body' is the full extraction prompt. The 'number' is derived from
   the entity index for evidence tracking."
  [entity idx & {:keys [pm-dir] :or {pm-dir planetmath-ct-dir}}]
  (let [prompt (make-extraction-prompt entity :pm-dir pm-dir)]
    (when prompt
      {:number (+ 10000 idx)  ;; offset to avoid collision with real GH issues
       :title (str "CT-extract: " (:title entity))
       :body prompt
       :labels ["ct-extraction" "tickle-work-queue"]
       ;; Carry entity metadata for evidence emission
       :entity-id (:entity-id entity)
       :entity-type (:type entity)
       :ground-truth {:scopes (:scope-count entity)
                      :wires (:wire-count entity)
                      :ports (:port-count entity)
                      :ner-terms (:ner-count entity)}})))

;; =============================================================================
;; Progress tracking via evidence store
;; =============================================================================

(defn completed-entity-ids
  "Query evidence store for entity IDs that have already been processed.
   Returns a set of entity-id strings."
  [evidence-store]
  (if-not evidence-store
    #{}
    (let [entries (estore/query* evidence-store {})
          ct-entries (->> entries
                          (filter #(some #{:ct-extraction} (:evidence/tags %)))
                          (filter #(some #{:workflow-complete :kick-complete}
                                         (:evidence/tags %))))]
      (->> ct-entries
           (keep #(get-in % [:evidence/body :entity-id]))
           set))))

(defn queue-status
  "Return a summary of queue progress.
   {:total N :completed N :remaining N :completed-ids #{...}}"
  [evidence-store]
  (let [entities (load-ct-entities)
        done (completed-entity-ids evidence-store)
        total (count entities)]
    {:total total
     :completed (count done)
     :remaining (- total (count done))
     :completed-ids done}))

(defn next-unprocessed
  "Return the next N unprocessed CT entities as issue maps.
   Skips entities that already have extraction evidence."
  ([evidence-store] (next-unprocessed evidence-store 10))
  ([evidence-store n]
   (let [entities (load-ct-entities)
         done (completed-entity-ids evidence-store)]
     (->> entities
          (map-indexed (fn [idx e]
                         (when-not (done (:entity-id e))
                           (entity->issue e idx))))
          (remove nil?)
          (take n)
          vec))))

;; =============================================================================
;; Evidence emission (CT-specific)
;; =============================================================================

(defn emit-ct-evidence!
  "Emit CT extraction evidence. Wraps the standard evidence format with
   CT-specific metadata."
  [evidence-store {:keys [entity-id entity-type session-id event-tag
                          ground-truth extraction-result verdict]}]
  (when evidence-store
    (estore/append* evidence-store
                    {:subject {:ref/type :task
                               :ref/id entity-id}
                     :type :coordination
                     :claim-type (case event-tag
                                   :workflow-start :goal
                                   :extraction-complete :observation
                                   :review-complete :observation
                                   :workflow-complete :observation
                                   :observation)
                     :author "tickle-1"
                     :tags [:tickle :ct-extraction event-tag]
                     :session-id session-id
                     :body (cond-> {:entity-id entity-id
                                    :entity-type entity-type
                                    :at (str (Instant/now))}
                             ground-truth (assoc :ground-truth ground-truth)
                             extraction-result (assoc :result-preview
                                                      (subs extraction-result
                                                            0 (min 500 (count extraction-result))))
                             verdict (assoc :verdict verdict))})))

;; =============================================================================
;; Batch helpers
;; =============================================================================

(defn entities-by-complexity
  "Sort entities by expected processing time (body-length).
   :asc for quickest first (good for testing), :desc for longest first."
  ([] (entities-by-complexity :asc))
  ([order]
   (let [entities (load-ct-entities)
         sorted (sort-by :body-length entities)]
     (if (= order :desc)
       (reverse sorted)
       sorted))))

(defn golden-entity-ids
  "Return the set of entity IDs that have golden reference extractions.
   These are the 20 exemplars — useful for validation runs."
  []
  (let [golden-files (->> (.listFiles (io/file ct-golden-dir))
                           (filter #(str/ends-with? (.getName %) ".json"))
                           (map #(.getName %)))]
    (->> golden-files
         (map (fn [fname]
                ;; golden-NN-pm-ct-FooBar.json → pm-ct-FooBar
                (let [parts (str/split fname #"-" 4)]
                  (when (>= (count parts) 4)
                    (str/replace (last parts) ".json" "")))))
         (remove nil?)
         set)))

(defn load-golden-expected
  "Load the expected golden output for an entity (if it exists).
   Returns parsed JSON or nil."
  [entity-id]
  (let [golden-files (->> (.listFiles (io/file ct-golden-dir))
                           (filter #(str/ends-with? (.getName %) ".json")))]
    (some (fn [f]
            (when (str/includes? (.getName f) entity-id)
              (json/parse-string (slurp f) true)))
          golden-files)))
