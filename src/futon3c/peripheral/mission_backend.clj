(ns futon3c.peripheral.mission-backend
  "MissionBackend — wraps RealBackend with mission-domain tools.

   The mission backend manages code development missions with DAG-tracked
   obligations, versioned mission specs, cycle records, and failed approaches.

   Reuses proof_dag.clj algorithms unchanged — the DAG is the same
   data structure (item/depends-on, item/unlocks) with different semantics.

   Structural enforcement:
   - Obligation status must be in the allowed set
   - Evidence class constrains status transitions (:assertion cannot yield :done)
   - Failed approaches are append-only (honesty)
   - DAG acyclicity is enforced on save

   Standard tools (:read, :glob, :grep, :bash, :bash-readonly, :write)
   are delegated to RealBackend."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set]
            [clojure.string :as str]
            [futon3c.peripheral.mission-shapes :as ms]
            [futon3c.peripheral.proof-dag :as dag]
            [futon3c.peripheral.tools :as tools])
  (:import [java.io File]
           [java.time Instant]
           [java.util UUID]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- now-str [] (str (Instant/now)))

(defn- gen-id [prefix]
  (str prefix "-" (UUID/randomUUID)))

(defn- mission-error
  "Create an error result for mission tool failures."
  [code message & {:as context}]
  {:ok false :error (cond-> {:code code :message message}
                      context (assoc :context context))})

(defn- state-path
  "Resolve the EDN file path for a mission's state."
  [cwd mission-id]
  (let [dir (io/file cwd "data" "mission-state")]
    (.mkdirs dir)
    (io/file dir (str mission-id ".edn"))))

;; =============================================================================
;; State persistence
;; =============================================================================

(defn- load-state
  "Load mission state from disk. Returns nil if not found."
  [cwd mission-id]
  (let [f (state-path cwd mission-id)]
    (when (.exists ^File f)
      (edn/read-string (slurp f)))))

(defn- save-state!
  "Atomically save mission state to disk with version bump."
  [cwd state]
  (let [mission-id (:mission/id state)
        f (state-path cwd mission-id)
        updated (-> state
                    (update :mission/version inc)
                    (assoc :mission/updated-at (now-str)))]
    (when-let [err (ms/validate ms/MissionState updated)]
      (throw (ex-info "Invalid mission state" {:validation-error err})))
    (spit f (pr-str updated))
    updated))

;; =============================================================================
;; In-memory state cache
;; =============================================================================

(defn- get-state
  "Get mission state from cache or disk."
  [cache cwd mission-id]
  (or (get @cache mission-id)
      (when-let [state (load-state cwd mission-id)]
        (swap! cache assoc mission-id state)
        state)))

(defn- put-state!
  "Update mission state in cache."
  [cache state]
  (swap! cache assoc (:mission/id state) state)
  state)

;; =============================================================================
;; Mission tool implementations
;; =============================================================================

(defn- tool-mission-load
  "Load mission state from disk."
  [cache cwd args]
  (let [mission-id (first args)]
    (if-let [state (load-state cwd mission-id)]
      (do (put-state! cache state)
          {:ok true :result state})
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-mission-save
  "Save mission state to disk with version bump."
  [cache cwd args]
  (let [mission-id (first args)]
    (if-let [state (get-state cache cwd mission-id)]
      (let [saved (save-state! cwd state)]
        (put-state! cache saved)
        {:ok true :result saved})
      (mission-error :not-found (str "No mission state loaded for " mission-id)))))

(defn- tool-obligation-query
  "Query the mission obligations. Supports filtering by status."
  [cache cwd args]
  (let [mission-id (first args)
        filter-opts (second args)]
    (if-let [state (get-state cache cwd mission-id)]
      (let [obligations (:mission/obligations state)
            items (if (and (map? filter-opts) (:status filter-opts))
                    (into {} (filter (fn [[_k v]]
                                      (= (:item/status v) (:status filter-opts)))
                                    obligations))
                    obligations)]
        {:ok true :result items})
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-obligation-upsert
  "Upsert an obligation with enforcement.
   - Status must be in the allowed set
   - Evidence class constrains transitions (:assertion cannot yield :done)
   - Cannot erase failure reasons"
  [cache cwd args]
  (let [mission-id (first args)
        item-id (second args)
        changes (nth args 2)]
    (if-let [state (get-state cache cwd mission-id)]
      (let [existing (get-in state [:mission/obligations item-id])
            new-status (:item/status changes)]
        (cond
          ;; Validate new status is in allowed set
          (and new-status (not (ms/valid-status? new-status)))
          (mission-error :invalid-status
                         (str "Status " new-status " is not valid. "
                              "Valid: :done :partial :open :blocked :abandoned"))

          ;; Validate status transition with evidence class
          (and existing new-status
               (not= new-status (:item/status existing))
               (not (ms/valid-status-transition?
                     (:item/status existing) new-status
                     (or (:item/evidence-type changes)
                         (:item/evidence-type existing)))))
          (mission-error :invalid-transition
                         (str "Cannot transition from " (:item/status existing)
                              " to " new-status
                              " with evidence type " (or (:item/evidence-type changes)
                                                         (:item/evidence-type existing))))

          ;; Cannot erase failure reasons
          (and existing
               (:item/failure-reason existing)
               (contains? changes :item/failure-reason)
               (nil? (:item/failure-reason changes)))
          (mission-error :honesty-violation
                         (str "Cannot erase failure reason for " item-id))

          :else
          (let [base (or existing {:item/id item-id
                                   :item/label (or (:item/label changes) item-id)
                                   :item/status :open
                                   :item/depends-on #{}
                                   :item/unlocks #{}
                                   :item/artifact-paths []})
                updated-item (merge base changes {:item/id item-id})
                new-state (assoc-in state [:mission/obligations item-id] updated-item)]
            (put-state! cache new-state)
            {:ok true :result updated-item})))
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-dag-check
  "Check DAG acyclicity."
  [cache cwd args]
  (let [mission-id (first args)]
    (if-let [state (get-state cache cwd mission-id)]
      {:ok true :result (dag/acyclic? (:mission/obligations state))}
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-dag-impact
  "Rank blockers by transitive unlock count."
  [cache cwd args]
  (let [mission-id (first args)]
    (if-let [state (get-state cache cwd mission-id)]
      {:ok true :result (dag/impact-scores (:mission/obligations state))}
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-mission-spec-get
  "Get the mission specification."
  [cache cwd args]
  (let [mission-id (first args)]
    (if-let [state (get-state cache cwd mission-id)]
      {:ok true :result (:mission/spec state)}
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-mission-spec-update
  "Update the mission spec with version tracking."
  [cache cwd args]
  (let [mission-id (first args)
        new-spec-data (second args)
        reason (nth args 2)]
    (if-let [state (get-state cache cwd mission-id)]
      (let [current-spec (:mission/spec state)
            old-history (:version-history current-spec)
            new-version (inc (:version current-spec))
            new-title (or (:title new-spec-data) (:title current-spec))
            history-entry {:version new-version
                           :title new-title
                           :changed-at (now-str)
                           :reason reason}
            new-spec (merge current-spec
                            new-spec-data
                            {:version new-version
                             :version-history (conj old-history history-entry)})
            new-state (assoc state :mission/spec new-spec)]
        (put-state! cache new-state)
        {:ok true :result new-spec})
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- find-cycle
  "Find a cycle by ID in the mission state."
  [state cycle-id]
  (first (filter #(= cycle-id (:cycle/id %)) (:mission/cycles state))))

(defn- update-cycle
  "Update a cycle in the mission state's cycle vector."
  [state cycle-id f]
  (update state :mission/cycles
          (fn [cycles]
            (mapv (fn [c]
                    (if (= cycle-id (:cycle/id c))
                      (f c)
                      c))
                  cycles))))

(defn- tool-cycle-begin
  "Begin a new mission cycle targeting a specific obligation."
  [cache cwd args]
  (let [mission-id (first args)
        blocker-id (second args)]
    (if-let [state (get-state cache cwd mission-id)]
      (if-not (get-in state [:mission/obligations blocker-id])
        (mission-error :invalid-blocker
                       (str "Obligation " blocker-id " not found"))
        (let [cycle-id (str mission-id "-C" (format "%03d" (inc (count (:mission/cycles state)))))
              cycle {:cycle/id cycle-id
                     :cycle/blocker-id blocker-id
                     :cycle/phase :observe
                     :cycle/phases-completed []
                     :cycle/phase-data {}
                     :cycle/started-at (now-str)
                     :cycle/updated-at (now-str)}
              new-state (update state :mission/cycles conj cycle)]
          (put-state! cache new-state)
          {:ok true :result cycle}))
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- validate-phase-advance
  "Check that all required outputs for the current phase are present."
  [cycle phase-data]
  (let [current-phase (:cycle/phase cycle)
        required (get ms/phase-required-outputs current-phase #{})
        provided (set (keys phase-data))
        missing (clojure.set/difference required provided)]
    (when (seq missing)
      (mission-error :missing-phase-outputs
                     (str "Phase " current-phase " requires: "
                          (pr-str missing) " before advancing")
                     :phase current-phase
                     :missing (vec missing)))))

(defn- tool-cycle-advance
  "Advance a cycle to the next phase.
   Enforces phase order and required outputs."
  [cache cwd args]
  (let [mission-id (first args)
        cycle-id (second args)
        phase-data (nth args 2)
        phase-transitions (into {} (map vector ms/phase-order (rest ms/phase-order)))]
    (if-let [state (get-state cache cwd mission-id)]
      (if-let [cycle (find-cycle state cycle-id)]
        (let [current-phase (:cycle/phase cycle)
              next-phase (get phase-transitions current-phase)]
          (cond
            (nil? next-phase)
            (mission-error :cycle-completed
                           (str "Cycle " cycle-id " is already completed"))

            :else
            (if-let [err (validate-phase-advance cycle phase-data)]
              err
              (let [updated-cycle (-> cycle
                                      (assoc :cycle/phase next-phase
                                             :cycle/updated-at (now-str))
                                      (update :cycle/phases-completed conj current-phase)
                                      (assoc-in [:cycle/phase-data current-phase] phase-data))
                    updated-cycle (if (= next-phase :completed)
                                    (assoc updated-cycle :cycle/result-status
                                           (or (:result-status phase-data) :inconclusive))
                                    updated-cycle)
                    new-state (update-cycle state cycle-id (constantly updated-cycle))]
                (put-state! cache new-state)
                {:ok true :result updated-cycle}))))
        (mission-error :cycle-not-found (str "Cycle " cycle-id " not found")))
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-cycle-get
  "Read a cycle by ID."
  [cache cwd args]
  (let [mission-id (first args)
        cycle-id (second args)]
    (if-let [state (get-state cache cwd mission-id)]
      (if-let [cycle (find-cycle state cycle-id)]
        {:ok true :result cycle}
        (mission-error :cycle-not-found (str "Cycle " cycle-id " not found")))
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-cycle-list
  "List all cycles for a mission."
  [cache cwd args]
  (let [mission-id (first args)]
    (if-let [state (get-state cache cwd mission-id)]
      {:ok true :result (:mission/cycles state)}
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-failed-approach-add
  "Record a failed approach (append-only)."
  [cache cwd args]
  (let [mission-id (first args)
        approach-data (second args)]
    (if-let [state (get-state cache cwd mission-id)]
      (let [failure-reason (:approach/failure-reason approach-data)]
        (cond
          (or (nil? failure-reason) (str/blank? failure-reason))
          (mission-error :missing-failure-reason
                         "Failed approach requires :approach/failure-reason")

          :else
          (let [approach (merge {:approach/id (gen-id "FA")
                                 :approach/recorded-at (now-str)}
                                approach-data)
                err (ms/validate ms/FailedApproach approach)]
            (if err
              (mission-error :invalid-approach (str "Invalid approach data: " (:error err)))
              (let [new-state (update state :mission/failed-approaches conj approach)]
                (put-state! cache new-state)
                {:ok true :result approach})))))
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-status-validate
  "Validate a status transition."
  [_cache _cwd args]
  (let [from (first args)
        to (second args)
        evidence-type (nth args 2)
        valid? (ms/valid-status-transition? from to evidence-type)]
    {:ok true :result {:valid? valid?
                       :from from
                       :to to
                       :evidence-type evidence-type
                       :reason (when-not valid?
                                 (cond
                                   (and (= to :done) (= evidence-type :assertion))
                                   "Cannot claim :done with only :assertion evidence"

                                   (= from :abandoned)
                                   "Cannot transition away from :abandoned status"

                                   (not (ms/valid-status? from))
                                   (str "Invalid source status: " from)

                                   (not (ms/valid-status? to))
                                   (str "Invalid target status: " to)

                                   :else "Unknown validation failure"))}}))

(defn- tool-gate-check
  "Run the gate checklist for a mission cycle."
  [cache cwd args]
  (let [mission-id (first args)
        cycle-id (second args)]
    (if-let [state (get-state cache cwd mission-id)]
      (if-let [cycle (find-cycle state cycle-id)]
        (let [blocker-id (:cycle/blocker-id cycle)
              blocker (get-in state [:mission/obligations blocker-id])
              phase-data (:cycle/phase-data cycle)
              dag-result (dag/acyclic? (:mission/obligations state))

              ;; G5: scope — blocker exists in obligations
              g5 {:gate :G5-scope
                  :passed? (boolean blocker)
                  :detail (if blocker "Blocker exists in obligations" "Blocker not found")}

              ;; G4: evidence — artifacts exist
              g4-artifacts (get-in phase-data [:execute :artifacts] [])
              g4 {:gate :G4-evidence
                  :passed? (seq g4-artifacts)
                  :detail (str (count g4-artifacts) " artifact(s) referenced")}

              ;; G3: status — valid classification
              g3-classification (get-in phase-data [:classify :classification])
              g3 {:gate :G3-status
                  :passed? (some? g3-classification)
                  :detail (if g3-classification
                            (str "Classified as: " g3-classification)
                            "No classification recorded")}

              ;; G2: DAG acyclicity
              g2 {:gate :G2-dag
                  :passed? (:acyclic? dag-result)
                  :detail (if (:acyclic? dag-result)
                            "DAG is acyclic"
                            (str "Cycle detected: " (:cycle-nodes dag-result)))}

              ;; G1: obligation consistency
              g1-dangling (dag/dangling-refs (:mission/obligations state))
              g1 {:gate :G1-obligations
                  :passed? (empty? g1-dangling)
                  :detail (if (empty? g1-dangling)
                            "No dangling references"
                            (str "Dangling refs: " g1-dangling))}

              ;; G0: commit — state persisted
              g0-saved (get-in phase-data [:commit :saved?])
              g0 {:gate :G0-commit
                  :passed? (boolean g0-saved)
                  :detail (if g0-saved "State saved" "State not yet saved")}

              gates [g5 g4 g3 g2 g1 g0]
              all-passed? (every? :passed? gates)]
          {:ok true :result {:gates gates
                             :all-passed? all-passed?
                             :cycle-id cycle-id}})
        (mission-error :cycle-not-found (str "Cycle " cycle-id " not found")))
      (mission-error :not-found (str "No mission state for " mission-id)))))

(defn- tool-evidence-query
  "Query evidence entries for this mission.
   Delegates to evidence-store if available in context."
  [_cache _cwd args]
  ;; Stub: returns empty results. In practice this would query the
  ;; evidence store, but the store is held by the peripheral state,
  ;; not the backend. This tool exists so the phase gate allows it.
  {:ok true :result {:entries [] :query (first args)}})

(defn- tool-corpus-check
  "Query futon3a's ANN index for structurally similar patterns.
   Same implementation as proof backend — delegates to notions_search.py."
  [_cache _cwd args config]
  (let [query-text (first args)
        opts (or (second args) {})
        top-k (or (:top-k opts) 5)
        method (or (:method opts) :auto)
        futon3a-root (or (:futon3a-root config)
                         (let [home (System/getProperty "user.home")]
                           (str home "/code/futon3a")))
        search-script (str futon3a-root "/scripts/notions_search.py")
        python (or (:futon3a-python config) "python3")]
    (cond
      (or (nil? query-text) (str/blank? query-text))
      (mission-error :invalid-query "corpus-check requires a non-empty query string")

      (not (.exists (io/file search-script)))
      (mission-error :corpus-unavailable
                     (str "futon3a search script not found: " search-script))

      :else
      (try
        (let [pb (ProcessBuilder.
                   [python search-script
                    "--query" (str query-text)
                    "--top" (str top-k)])
              _ (.directory pb (io/file futon3a-root))
              _ (.redirectErrorStream pb true)
              proc (.start pb)
              output (slurp (.getInputStream proc))
              exit-code (.waitFor proc)]
          (if (zero? exit-code)
            {:ok true :result {:neighbors [] :query query-text
                               :method method :top-k top-k :source :futon3a
                               :raw-output output}}
            (mission-error :corpus-search-failed
                           (str "Search returned exit code " exit-code ": " output))))
        (catch Exception e
          (mission-error :corpus-search-failed
                         (str "corpus-check failed: " (.getMessage e))))))))

;; =============================================================================
;; MissionBackend record
;; =============================================================================

(def mission-tools
  "The set of all mission-domain tool keywords."
  #{:mission-load :mission-save
    :obligation-query :obligation-upsert
    :dag-check :dag-impact
    :mission-spec-get :mission-spec-update
    :cycle-begin :cycle-advance :cycle-get :cycle-list
    :failed-approach-add :status-validate :gate-check
    :corpus-check :evidence-query})

(def delegated-tools
  "Tools delegated to the wrapped RealBackend."
  #{:read :glob :grep :bash :bash-readonly :write})

(defrecord MissionBackend [real-backend cache config]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (let [cwd (or (:cwd config) (System/getProperty "user.dir"))]
      (cond
        ;; Mission-domain tools
        (= tool-id :mission-load)       (tool-mission-load cache cwd args)
        (= tool-id :mission-save)       (tool-mission-save cache cwd args)
        (= tool-id :obligation-query)   (tool-obligation-query cache cwd args)
        (= tool-id :obligation-upsert)  (tool-obligation-upsert cache cwd args)
        (= tool-id :dag-check)          (tool-dag-check cache cwd args)
        (= tool-id :dag-impact)         (tool-dag-impact cache cwd args)
        (= tool-id :mission-spec-get)   (tool-mission-spec-get cache cwd args)
        (= tool-id :mission-spec-update) (tool-mission-spec-update cache cwd args)
        (= tool-id :cycle-begin)        (tool-cycle-begin cache cwd args)
        (= tool-id :cycle-advance)      (tool-cycle-advance cache cwd args)
        (= tool-id :cycle-get)          (tool-cycle-get cache cwd args)
        (= tool-id :cycle-list)         (tool-cycle-list cache cwd args)
        (= tool-id :failed-approach-add) (tool-failed-approach-add cache cwd args)
        (= tool-id :status-validate)    (tool-status-validate cache cwd args)
        (= tool-id :gate-check)         (tool-gate-check cache cwd args)
        (= tool-id :corpus-check)       (tool-corpus-check cache cwd args config)
        (= tool-id :evidence-query)     (tool-evidence-query cache cwd args)

        ;; Delegated tools
        (contains? delegated-tools tool-id)
        (tools/execute-tool real-backend tool-id args)

        :else
        {:ok false :error (str "Unknown mission tool: " tool-id)}))))

(defn make-mission-backend
  "Create a MissionBackend wrapping a RealBackend.

   config:
     :cwd — working directory for mission state persistence
     :futon3a-root — path to futon3a (for corpus-check)"
  ([]
   (make-mission-backend {}))
  ([config]
   (make-mission-backend config nil))
  ([config real-backend]
   (let [rb (or real-backend (tools/make-mock-backend))]
     (->MissionBackend rb (atom {}) config))))

;; =============================================================================
;; Initial state creation
;; =============================================================================

(defn make-initial-state
  "Create a blank mission state.
   This is the starting point before any cycles."
  [mission-id title success-criteria scope-in scope-out]
  {:mission/id mission-id
   :mission/version 0
   :mission/spec {:title title
                  :success-criteria (vec success-criteria)
                  :scope-in (vec scope-in)
                  :scope-out (vec scope-out)
                  :version 1
                  :version-history [{:version 1
                                     :title title
                                     :changed-at (now-str)
                                     :reason "Initial specification"}]}
   :mission/obligations {}
   :mission/cycles []
   :mission/failed-approaches []
   :mission/updated-at (now-str)})

(defn init-mission!
  "Initialize a new mission, saving initial state to disk.
   Returns the initial MissionState."
  [cwd mission-id title success-criteria scope-in scope-out]
  (let [state (make-initial-state mission-id title success-criteria scope-in scope-out)]
    (save-state! cwd state)))
