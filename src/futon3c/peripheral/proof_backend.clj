(ns futon3c.peripheral.proof-backend
  "ProofBackend — wraps RealBackend with 15 proof-domain tools.

   The proof backend enforces strategy requirements (SR-1..8) structurally:
   - SR-1: canonical statement versioning + hash
   - SR-2: ledger with enforced status policy
   - SR-3: DAG acyclicity check
   - SR-5: evidence class constrains status transitions
   - SR-6: impact scoring ranks blockers by transitive unlock count
   - SR-7: artifact minimum before integration
   - SR-8: failed routes are append-only, failure points can't be erased

   Standard tools (:read, :glob, :grep, :bash, :bash-readonly, :write)
   are delegated to RealBackend."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set]
            [futon3c.peripheral.proof-dag :as dag]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.tools :as tools])
  (:import [java.io File]
           [java.security MessageDigest]
           [java.time Instant]
           [java.util UUID]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- now-str [] (str (Instant/now)))

(defn- gen-id [prefix]
  (str prefix "-" (UUID/randomUUID)))

(defn- sha256 [s]
  (let [md (MessageDigest/getInstance "SHA-256")
        bytes (.digest md (.getBytes (str s) "UTF-8"))]
    (str "sha256:" (apply str (map #(format "%02x" %) bytes)))))

(defn- proof-error
  "Create an error result for proof tool failures."
  [code message & {:as context}]
  {:ok false :error (cond-> {:code code :message message}
                      context (assoc :context context))})

(defn- state-path
  "Resolve the EDN file path for a problem's proof state."
  [cwd problem-id]
  (let [dir (io/file cwd "data" "proof-state")]
    (.mkdirs dir)
    (io/file dir (str problem-id ".edn"))))

;; =============================================================================
;; State persistence
;; =============================================================================

(defn- load-state
  "Load proof state from disk. Returns nil if not found."
  [cwd problem-id]
  (let [f (state-path cwd problem-id)]
    (when (.exists ^File f)
      (edn/read-string (slurp f)))))

(defn- save-state!
  "Atomically save proof state to disk with version bump."
  [cwd state]
  (let [problem-id (:proof/problem-id state)
        f (state-path cwd problem-id)
        updated (-> state
                    (update :proof/version inc)
                    (assoc :proof/updated-at (now-str)))]
    (when-let [err (ps/validate ps/ProofState updated)]
      (throw (ex-info "Invalid proof state" {:validation-error err})))
    (spit f (pr-str updated))
    updated))

;; =============================================================================
;; In-memory state cache
;; =============================================================================

(defn- get-state
  "Get proof state from cache or disk."
  [cache cwd problem-id]
  (or (get @cache problem-id)
      (when-let [state (load-state cwd problem-id)]
        (swap! cache assoc problem-id state)
        state)))

(defn- put-state!
  "Update proof state in cache."
  [cache state]
  (swap! cache assoc (:proof/problem-id state) state)
  state)

;; =============================================================================
;; Proof tool implementations
;; =============================================================================

(defn- tool-proof-load
  "Load proof state from disk."
  [cache cwd args]
  (let [problem-id (first args)]
    (if-let [state (load-state cwd problem-id)]
      (do (put-state! cache state)
          {:ok true :result state})
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-proof-save
  "Save proof state to disk with version bump."
  [cache cwd args]
  (let [problem-id (first args)]
    (if-let [state (get-state cache cwd problem-id)]
      (let [saved (save-state! cwd state)]
        (put-state! cache saved)
        {:ok true :result saved})
      (proof-error :not-found (str "No proof state loaded for " problem-id)))))

(defn- tool-ledger-query
  "Query the proof ledger. Supports filtering by status."
  [cache cwd args]
  (let [problem-id (first args)
        filter-opts (second args)]
    (if-let [state (get-state cache cwd problem-id)]
      (let [ledger (:proof/ledger state)
            items (if (and (map? filter-opts) (:status filter-opts))
                    (into {} (filter (fn [[_k v]]
                                      (= (:item/status v) (:status filter-opts)))
                                    ledger))
                    ledger)]
        {:ok true :result items})
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-ledger-upsert
  "Upsert a ledger item with enforcement.
   SR-2: status must be in the allowed set.
   SR-5: evidence class constrains transitions.
   SR-8: cannot erase failure points or delete items with :false status."
  [cache cwd args]
  (let [problem-id (first args)
        item-id (second args)
        changes (nth args 2)]
    (if-let [state (get-state cache cwd problem-id)]
      (let [existing (get-in state [:proof/ledger item-id])
            ;; Validate status if changing
            new-status (:item/status changes)]
        (cond
          ;; Validate new status is in allowed set
          (and new-status (not (ps/valid-status? new-status)))
          (proof-error :invalid-status
                       (str "Status " new-status " is not in the allowed set. "
                            "Valid: :proved :partial :open :false :numerically-verified"))

          ;; SR-5: validate status transition with evidence class
          (and existing new-status
               (not= new-status (:item/status existing))
               (not (ps/valid-status-transition?
                     (:item/status existing) new-status
                     (or (:item/evidence-type changes)
                         (:item/evidence-type existing)))))
          (proof-error :invalid-transition
                       (str "Cannot transition from " (:item/status existing)
                            " to " new-status
                            " with evidence type " (or (:item/evidence-type changes)
                                                       (:item/evidence-type existing))))

          ;; SR-8: cannot erase failure points
          (and existing
               (:item/failure-point existing)
               (contains? changes :item/failure-point)
               (nil? (:item/failure-point changes)))
          (proof-error :honesty-violation
                       (str "Cannot erase failure point for " item-id
                            " (SR-8: honesty requirement)"))

          :else
          (let [base (or existing {:item/id item-id
                                   :item/label (or (:item/label changes) item-id)
                                   :item/status :open
                                   :item/depends-on #{}
                                   :item/unlocks #{}
                                   :item/artifact-paths []})
                updated-item (merge base changes {:item/id item-id})
                new-state (assoc-in state [:proof/ledger item-id] updated-item)]
            (put-state! cache new-state)
            {:ok true :result updated-item})))
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-dag-check
  "Check DAG acyclicity (SR-3)."
  [cache cwd args]
  (let [problem-id (first args)]
    (if-let [state (get-state cache cwd problem-id)]
      {:ok true :result (dag/acyclic? (:proof/ledger state))}
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-dag-impact
  "Rank blockers by transitive unlock count (SR-6)."
  [cache cwd args]
  (let [problem-id (first args)]
    (if-let [state (get-state cache cwd problem-id)]
      {:ok true :result (dag/impact-scores (:proof/ledger state))}
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-canonical-get
  "Get the canonical statement (SR-1)."
  [cache cwd args]
  (let [problem-id (first args)]
    (if-let [state (get-state cache cwd problem-id)]
      {:ok true :result (:proof/canonical state)}
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-canonical-update
  "Update the canonical statement with version tracking (SR-1)."
  [cache cwd args]
  (let [problem-id (first args)
        new-statement (second args)
        diff-reason (nth args 2)]
    (if-let [state (get-state cache cwd problem-id)]
      (let [canonical (:proof/canonical state)
            old-history (:version-history canonical)
            new-version (inc (count old-history))
            new-hash (sha256 new-statement)
            history-entry {:version new-version
                           :statement new-statement
                           :hash new-hash
                           :changed-at (now-str)
                           :reason diff-reason}
            new-canonical (assoc canonical
                                 :statement new-statement
                                 :statement-hash new-hash
                                 :version-history (conj old-history history-entry))
            new-state (assoc state :proof/canonical new-canonical)]
        (put-state! cache new-state)
        {:ok true :result new-canonical})
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-cycle-begin
  "Begin a new proof cycle (CR-1)."
  [cache cwd args]
  (let [problem-id (first args)
        blocker-id (second args)]
    (if-let [state (get-state cache cwd problem-id)]
      (if-not (get-in state [:proof/ledger blocker-id])
        (proof-error :invalid-blocker
                     (str "Blocker " blocker-id " not found in ledger"))
        (let [cycle-id (str problem-id "-C" (format "%03d" (inc (count (:proof/cycles state)))))
              cycle {:cycle/id cycle-id
                     :cycle/blocker-id blocker-id
                     :cycle/phase :observe
                     :cycle/phases-completed []
                     :cycle/phase-data {}
                     :cycle/started-at (now-str)
                     :cycle/updated-at (now-str)}
              new-state (update state :proof/cycles conj cycle)]
          (put-state! cache new-state)
          {:ok true :result cycle}))
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- find-cycle
  "Find a cycle by ID in the proof state."
  [state cycle-id]
  (first (filter #(= cycle-id (:cycle/id %)) (:proof/cycles state))))

(defn- update-cycle
  "Update a cycle in the proof state's cycle vector."
  [state cycle-id f]
  (update state :proof/cycles
          (fn [cycles]
            (mapv (fn [c]
                    (if (= cycle-id (:cycle/id c))
                      (f c)
                      c))
                  cycles))))

(defn- validate-phase-advance
  "Check that all required outputs for the current phase are present."
  [cycle phase-data]
  (let [current-phase (:cycle/phase cycle)
        required (get ps/phase-required-outputs current-phase #{})
        provided (set (keys phase-data))
        missing (clojure.set/difference required provided)]
    (when (seq missing)
      (proof-error :missing-phase-outputs
                   (str "Phase " current-phase " requires: "
                        (pr-str missing) " before advancing")
                   :phase current-phase
                   :missing (vec missing)))))

(defn- tool-cycle-advance
  "Advance a cycle to the next phase (CR-1..8).
   Enforces phase order and required outputs."
  [cache cwd args]
  (let [problem-id (first args)
        cycle-id (second args)
        phase-data (nth args 2)]
    (if-let [state (get-state cache cwd problem-id)]
      (if-let [cycle (find-cycle state cycle-id)]
        (let [current-phase (:cycle/phase cycle)
              next-phase (get ps/phase-transitions current-phase)]
          (cond
            (nil? next-phase)
            (proof-error :cycle-completed
                         (str "Cycle " cycle-id " is already completed"))

            ;; Check required outputs
            :else
            (if-let [err (validate-phase-advance cycle phase-data)]
              err
              (let [updated-cycle (-> cycle
                                      (assoc :cycle/phase next-phase
                                             :cycle/updated-at (now-str))
                                      (update :cycle/phases-completed conj current-phase)
                                      (assoc-in [:cycle/phase-data current-phase] phase-data))
                    ;; If completing, add result status
                    updated-cycle (if (= next-phase :completed)
                                    (assoc updated-cycle :cycle/result-status
                                           (or (:result-status phase-data) :inconclusive))
                                    updated-cycle)
                    new-state (update-cycle state cycle-id (constantly updated-cycle))]
                (put-state! cache new-state)
                {:ok true :result updated-cycle}))))
        (proof-error :cycle-not-found (str "Cycle " cycle-id " not found")))
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-cycle-get
  "Read a cycle by ID."
  [cache cwd args]
  (let [problem-id (first args)
        cycle-id (second args)]
    (if-let [state (get-state cache cwd problem-id)]
      (if-let [cycle (find-cycle state cycle-id)]
        {:ok true :result cycle}
        (proof-error :cycle-not-found (str "Cycle " cycle-id " not found")))
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-cycle-list
  "List all cycles for a problem."
  [cache cwd args]
  (let [problem-id (first args)]
    (if-let [state (get-state cache cwd problem-id)]
      {:ok true :result (:proof/cycles state)}
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-failed-route-add
  "Record a failed route (SR-8: append-only)."
  [cache cwd args]
  (let [problem-id (first args)
        route-data (second args)]
    (if-let [state (get-state cache cwd problem-id)]
      (let [route (merge {:route/id (gen-id "FR")
                          :route/recorded-at (now-str)}
                         route-data)
            ;; Validate the route
            err (ps/validate ps/FailedRoute route)]
        (if err
          (proof-error :invalid-route (str "Invalid route data: " (:error err)))
          (let [new-state (update state :proof/failed-routes conj route)]
            (put-state! cache new-state)
            {:ok true :result route})))
      (proof-error :not-found (str "No proof state for " problem-id)))))

(defn- tool-status-validate
  "Validate a status transition (SR-5)."
  [_cache _cwd args]
  (let [from (first args)
        to (second args)
        evidence-type (nth args 2)]
    (let [valid? (ps/valid-status-transition? from to evidence-type)]
      {:ok true :result {:valid? valid?
                         :from from
                         :to to
                         :evidence-type evidence-type
                         :reason (when-not valid?
                                   (cond
                                     (and (= to :proved) (= evidence-type :numerical))
                                     "Cannot claim :proved with only :numerical evidence"

                                     (= from :false)
                                     "Cannot transition away from :false status"

                                     (not (ps/valid-status? from))
                                     (str "Invalid source status: " from)

                                     (not (ps/valid-status? to))
                                     (str "Invalid target status: " to)

                                     :else "Unknown validation failure"))}})))

(defn- tool-gate-check
  "Run the G5-G0 gate checklist (CR-8)."
  [cache cwd args]
  (let [problem-id (first args)
        cycle-id (second args)]
    (if-let [state (get-state cache cwd problem-id)]
      (if-let [cycle (find-cycle state cycle-id)]
        (let [blocker-id (:cycle/blocker-id cycle)
              blocker (get-in state [:proof/ledger blocker-id])
              phase-data (:cycle/phase-data cycle)
              dag-result (dag/acyclic? (:proof/ledger state))

              ;; G5: scope — blocker matches canonical
              g5 {:gate :G5-scope
                  :passed? (boolean blocker)
                  :detail (if blocker "Blocker exists in ledger" "Blocker not found")}

              ;; G4: evidence — artifacts exist
              g4-artifacts (get-in phase-data [:execute :artifacts] [])
              g4 {:gate :G4-evidence
                  :passed? (seq g4-artifacts)
                  :detail (str (count g4-artifacts) " artifact(s) referenced")}

              ;; G3: status — valid transition
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

              ;; G1: ledger consistency
              g1-dangling (dag/dangling-refs (:proof/ledger state))
              g1 {:gate :G1-ledger
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
        (proof-error :cycle-not-found (str "Cycle " cycle-id " not found")))
      (proof-error :not-found (str "No proof state for " problem-id)))))

;; =============================================================================
;; ProofBackend record
;; =============================================================================

(def proof-tools
  "The set of all proof-domain tool keywords."
  #{:proof-load :proof-save
    :ledger-query :ledger-upsert
    :dag-check :dag-impact
    :canonical-get :canonical-update
    :cycle-begin :cycle-advance :cycle-get :cycle-list
    :failed-route-add :status-validate :gate-check})

(def delegated-tools
  "Tools delegated to the wrapped RealBackend."
  #{:read :glob :grep :bash :bash-readonly :write})

(defrecord ProofBackend [real-backend cache config]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (let [cwd (or (:cwd config) (System/getProperty "user.dir"))]
      (cond
        ;; Proof-domain tools
        (= tool-id :proof-load)       (tool-proof-load cache cwd args)
        (= tool-id :proof-save)       (tool-proof-save cache cwd args)
        (= tool-id :ledger-query)     (tool-ledger-query cache cwd args)
        (= tool-id :ledger-upsert)    (tool-ledger-upsert cache cwd args)
        (= tool-id :dag-check)        (tool-dag-check cache cwd args)
        (= tool-id :dag-impact)       (tool-dag-impact cache cwd args)
        (= tool-id :canonical-get)    (tool-canonical-get cache cwd args)
        (= tool-id :canonical-update) (tool-canonical-update cache cwd args)
        (= tool-id :cycle-begin)      (tool-cycle-begin cache cwd args)
        (= tool-id :cycle-advance)    (tool-cycle-advance cache cwd args)
        (= tool-id :cycle-get)        (tool-cycle-get cache cwd args)
        (= tool-id :cycle-list)       (tool-cycle-list cache cwd args)
        (= tool-id :failed-route-add) (tool-failed-route-add cache cwd args)
        (= tool-id :status-validate)  (tool-status-validate cache cwd args)
        (= tool-id :gate-check)       (tool-gate-check cache cwd args)

        ;; Delegated tools
        (contains? delegated-tools tool-id)
        (tools/execute-tool real-backend tool-id args)

        :else
        {:ok false :error (str "Unknown proof tool: " tool-id)}))))

(defn make-proof-backend
  "Create a ProofBackend wrapping a RealBackend.

   config:
     :cwd — working directory for proof state persistence
     :timeout-ms — command timeout for delegated bash tools"
  ([]
   (make-proof-backend {}))
  ([config]
   (make-proof-backend config nil))
  ([config real-backend]
   (let [rb (or real-backend
                (tools/make-mock-backend))]
     (->ProofBackend rb (atom {}) config))))

;; =============================================================================
;; Initial state creation
;; =============================================================================

(defn make-initial-state
  "Create a blank proof state for a new problem.
   This is the starting point before any cycles."
  [problem-id statement closure-criterion]
  (let [hash (sha256 statement)]
    {:proof/problem-id problem-id
     :proof/version 0
     :proof/canonical {:statement statement
                       :closure-criterion closure-criterion
                       :statement-hash hash
                       :version-history [{:version 1
                                          :statement statement
                                          :hash hash
                                          :changed-at (now-str)
                                          :reason "Initial statement"}]}
     :proof/ledger {}
     :proof/cycles []
     :proof/failed-routes []
     :proof/updated-at (now-str)}))

(defn init-problem!
  "Initialize a new proof problem, saving initial state to disk.
   Returns the initial ProofState."
  [cwd problem-id statement closure-criterion]
  (let [state (make-initial-state problem-id statement closure-criterion)]
    (save-state! cwd state)))
