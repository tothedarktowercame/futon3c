(ns futon3c.proof.bridge
  "Proof tool bridge — agent-friendly interface for CLI agents via /eval.

   This namespace provides a persistent bridge between CLI agents (running
   via `claude -p`) and the proof peripheral tools (Clojure functions in
   the running JVM). The bridge maintains a shared ProofBackend per problem
   so that mutations (canonical-update!, mode!, ledger-upsert!) accumulate
   in the same cache and save! persists them to disk.

   Usage from a CLI agent (via bash + curl):

     # Load the bridge (once per session)
     curl -s -H 'x-admin-token: TOKEN' \\
       -d '(require (quote [futon3c.proof.bridge :as pb]))' \\
       http://localhost:6768/eval

     # Get current proof mode
     curl ... -d '(pb/mode \"FM-001\")' http://localhost:6768/eval

     # Update the canonical statement (mutates cache)
     curl ... -d '(pb/canonical-update! \"FM-001\" \"new stmt\" \"reason\")' http://localhost:6768/eval

     # Save to disk (persists cached mutations)
     curl ... -d '(pb/save! \"FM-001\")' http://localhost:6768/eval

     # Or use auto-saving variants — most mutators auto-save by default
     curl ... -d '(pb/mode! \"FM-001\" :FALSIFY)' http://localhost:6768/eval

   Shared backend per problem: mutations accumulate in-memory and are
   auto-persisted after each mutating call. Call (pb/reset! \"FM-001\")
   to force a reload from disk if external changes were made."
  (:require [futon3c.peripheral.proof-backend :as pb]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Shared backend registry — one backend per problem, survives across calls
;; =============================================================================

(defonce ^{:doc "Map of problem-id → ProofBackend. Backends share a cache atom
  so mutations from one call are visible to the next."}
  !backends (atom {}))

(defn- cwd []
  (System/getProperty "user.dir"))

(defn- get-backend
  "Get or create a shared backend for a problem."
  [problem-id]
  (or (get @!backends problem-id)
      (let [backend (pb/make-proof-backend {:cwd (cwd)})]
        (swap! !backends assoc problem-id backend)
        backend)))

(defn- exec
  "Execute a proof tool using the shared backend for the problem.
   args: vector of positional args as expected by proof-backend tools."
  [problem-id tool-id args]
  (let [backend (get-backend problem-id)]
    (tools/execute-tool backend tool-id args)))

(defn- exec-and-save!
  "Execute a mutating proof tool, then auto-save to disk.
   Returns the tool result (save errors are logged but don't mask the result)."
  [problem-id tool-id args]
  (let [result (exec problem-id tool-id args)]
    (when (:ok result)
      (try
        (exec problem-id :proof-save [problem-id])
        (catch Throwable t
          (println (str "[bridge] auto-save warning for " problem-id ": " (.getMessage t))))))
    result))

(defn reload!
  "Discard the cached backend for a problem, forcing a fresh reload from disk
   on the next call. Use after external edits to the EDN file."
  [problem-id]
  (swap! !backends dissoc problem-id)
  {:ok true :message (str "Backend for " problem-id " reset. Next call will reload from disk.")})

;; =============================================================================
;; Problem state
;; =============================================================================

(defn load-problem
  "Load the full proof state for a problem.
   Returns {:ok true :result <proof-state>} or {:ok false :error ...}."
  [problem-id]
  (exec problem-id :proof-load [problem-id]))

(defn save!
  "Save the current proof state. Bumps version, validates, writes to disk."
  [problem-id]
  (exec problem-id :proof-save [problem-id]))

;; =============================================================================
;; Proof mode (SPEC → FALSIFY → CONSTRUCT → VERIFY → MAP)
;; =============================================================================

(defn mode
  "Get the current proof mode for a problem.
   Returns {:ok true :result {:mode :SPEC ...}} or error."
  [problem-id]
  (exec problem-id :proof-mode-get [problem-id]))

(defn mode!
  "Transition to a new proof mode. Auto-saves to disk.
   Enforces: FALSIFY must complete before CONSTRUCT.
   to: one of :SPEC :FALSIFY :CONSTRUCT :VERIFY :MAP"
  [problem-id to]
  (exec-and-save! problem-id :proof-mode-set [problem-id to]))

;; =============================================================================
;; Canonical statement
;; =============================================================================

(defn canonical
  "Get the canonical problem statement and spec-lock hash."
  [problem-id]
  (exec problem-id :canonical-get [problem-id]))

(defn canonical-update!
  "Update the canonical statement. Recomputes hash. Auto-saves to disk.
   new-statement: the new problem statement string
   reason: why the statement changed"
  [problem-id new-statement reason]
  (exec-and-save! problem-id :canonical-update [problem-id new-statement reason]))

;; =============================================================================
;; Ledger (claims, lemmas, status tracking)
;; =============================================================================

(defn ledger
  "Query the proof ledger. Returns all items by default.
   filter-opts: optional map e.g. {:status :open} for filtering."
  ([problem-id] (ledger problem-id nil))
  ([problem-id filter-opts]
   (exec problem-id :ledger-query [problem-id filter-opts])))

(defn ledger-upsert!
  "Create or update a ledger item. Auto-saves to disk.
   item-id: string identifier for the item
   item-data: map {:label str, :status keyword, :evidence-class keyword, ...}"
  [problem-id item-id item-data]
  (exec-and-save! problem-id :ledger-upsert [problem-id item-id item-data]))

;; =============================================================================
;; Proof cycles
;; =============================================================================

(defn cycle-begin!
  "Begin a new proof cycle targeting a blocker. Auto-saves to disk.
   blocker-id: string ID of the ledger item to unblock."
  [problem-id blocker-id]
  (exec-and-save! problem-id :cycle-begin [problem-id blocker-id]))

(defn cycle-advance!
  "Advance a cycle to the next phase. Auto-saves to disk.
   cycle-id: string cycle ID
   phase-data: map of outputs for the current phase"
  [problem-id cycle-id phase-data]
  (exec-and-save! problem-id :cycle-advance [problem-id cycle-id phase-data]))

(defn cycle-get
  "Get details of a specific cycle."
  [problem-id cycle-id]
  (exec problem-id :cycle-get [problem-id cycle-id]))

(defn cycles
  "List all cycles for a problem."
  [problem-id]
  (exec problem-id :cycle-list [problem-id]))

;; =============================================================================
;; DAG (dependency graph)
;; =============================================================================

(defn dag-check
  "Check DAG for acyclicity and structural issues."
  [problem-id]
  (exec problem-id :dag-check [problem-id]))

(defn dag-impact
  "Score blockers by transitive unlock count."
  [problem-id]
  (exec problem-id :dag-impact [problem-id]))

;; =============================================================================
;; Gates (G5 → G0 quality checklist)
;; =============================================================================

(defn gate-check
  "Run gate checklist for a cycle.
   cycle-id: string cycle ID to check"
  [problem-id cycle-id]
  (exec problem-id :gate-check [problem-id cycle-id]))

;; =============================================================================
;; TryHarder licensing
;; =============================================================================

(defn tryharder-create!
  "Create a TryHarder license for a persistence loop. Auto-saves to disk.
   opts: {:target-claim str, :bottleneck-type keyword, :new-lever str,
          :witness str, :kill-condition str, :timebox-minutes int}"
  [problem-id opts]
  (exec-and-save! problem-id :tryharder-license [problem-id :create opts]))

(defn tryharder-close!
  "Close the active TryHarder license with outcome. Auto-saves to disk.
   opts: {:witness-met boolean, :mode-after keyword, :notes str}"
  [problem-id opts]
  (exec-and-save! problem-id :tryharder-license [problem-id :close opts]))

(defn tryharder-status
  "Get the current TryHarder license status."
  [problem-id]
  (exec problem-id :tryharder-license [problem-id :active]))

(defn tryharder-list
  "List all TryHarder licenses (active and closed)."
  [problem-id]
  (exec problem-id :tryharder-license [problem-id :list]))

;; =============================================================================
;; Failed routes (honesty log)
;; =============================================================================

(defn failed-route!
  "Record a failed proof route (append-only, cannot be erased). Auto-saves.
   route: {:description str, :failure-point str, :reason str}"
  [problem-id route]
  (exec-and-save! problem-id :failed-route-add [problem-id route]))

;; =============================================================================
;; Status validation
;; =============================================================================

(defn status-validate
  "Validate a proposed status transition.
   item-id: string ledger item ID
   to-status: target status keyword
   evidence-class: keyword evidence class"
  [problem-id item-id to-status evidence-class]
  (exec problem-id :status-validate [problem-id item-id to-status evidence-class]))

;; =============================================================================
;; Corpus check
;; =============================================================================

(defn corpus-check
  "Search the mathematical corpus for related results.
   query: string search query"
  [problem-id query]
  (exec problem-id :corpus-check [problem-id query]))

;; =============================================================================
;; Convenience: problem summary
;; =============================================================================

(defn summary
  "Get a concise summary of the current proof state.
   Returns: {:problem-id, :mode, :cycles-count, :ledger-count, :falsify-done?}"
  [problem-id]
  (let [state-result (load-problem problem-id)]
    (if (:ok state-result)
      (let [state (:result state-result)]
        {:ok true
         :result {:problem-id problem-id
                  :mode (or (:proof/current-mode state) :SPEC)
                  :version (:proof/version state)
                  :cycles-count (count (:proof/cycles state))
                  :ledger-count (count (:proof/ledger state))
                  :falsify-done? (boolean (:proof/falsify-completed? state))
                  :updated-at (:proof/updated-at state)
                  :statement (get-in state [:proof/canonical :statement])}})
      state-result)))
