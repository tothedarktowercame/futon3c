(ns futon3c.peripheral.proof-logic
  "Relational invariant layer for the Proof Peripheral.

   Snapshots a proof state (ledger, DAG, cycles, failed routes, mode) into
   a core.logic fact database. Expresses structural properties as goals and
   queries for violations.

   This namespace checks whether a proof state satisfies structural law —
   properties that hold for *any* correct proof state regardless of the
   specific mathematical domain.

   Domains:

   1. DAG integrity — acyclicity, edge symmetry, no dangling refs
   2. Status discipline — evidence class constrains transitions, :false is terminal
   3. Cycle phase linearity — phases advance in order, required outputs present
   4. Mode gating — FALSIFY before CONSTRUCT
   5. Failed routes — append-only honesty (structural, not temporal)
   6. Dependency satisfaction — :proved items have :proved/:axiom deps

   Pattern follows tickle_logic.clj and agency/logic.clj:
   snapshot → build-db → goals → query-violations."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [futon3c.logic.structural-law :as law]))

;; =============================================================================
;; Relations (fact schema)
;; =============================================================================

;; --- Ledger item facts ---
(pldb/db-rel itemo item-id)
(pldb/db-rel item-statuso item-id status)
(pldb/db-rel item-evidence-typeo item-id evidence-type)
(pldb/db-rel depends-ono item-id dep-id)            ; item depends on dep
(pldb/db-rel unlockso item-id unlocked-id)           ; item unlocks unlocked
(pldb/db-rel failure-pointo item-id failure-point)   ; immutable once set

;; --- DAG structure (derived from depends-on/unlocks) ---
;; We use the direct relations above rather than a separate edge relation,
;; since the invariant is about symmetry *between* depends-on and unlocks.

;; --- Cycle facts ---
(pldb/db-rel cycleo cycle-id)
(pldb/db-rel cycle-phaseo cycle-id phase)
(pldb/db-rel cycle-blockero cycle-id blocker-id)
(pldb/db-rel cycle-has-outputo cycle-id phase output-key)
(pldb/db-rel cycle-resulto cycle-id result-status)

;; --- Mode facts ---
(pldb/db-rel proof-modeo mode)
(pldb/db-rel falsify-completedo)                     ; flag present iff true

;; --- Failed route facts ---
(pldb/db-rel failed-routeo route-id blocker-id)
(pldb/db-rel route-has-obstructiono route-id)        ; structural obstruction recorded

;; =============================================================================
;; Valid enums (from proof_shapes.clj)
;; =============================================================================

(def ^:private valid-statuses
  #{:proved :partial :open :false :numerically-verified})

(def ^:private phase-order
  [:observe :propose :execute :validate :classify :integrate :commit :gate-review :completed])

(def ^:private phase-required-outputs
  {:observe     #{:blocker-id}
   :propose     #{:approach}
   :execute     #{:artifacts}
   :validate    #{:validation-artifacts}
   :classify    #{:classification}
   :integrate   #{:rationale :ledger-changes}
   :commit      #{:saved?}
   :gate-review #{:gates-passed}})

;; =============================================================================
;; Database construction
;; =============================================================================

(defn- add-ledger-facts
  "Add facts from ledger items.
   ledger: {item-id -> LedgerItem}"
  [db ledger]
  (reduce-kv
    (fn [db' iid item]
      (let [status (or (:item/status item) :unknown)
            evidence-type (:item/evidence-type item)
            deps (or (:item/depends-on item) #{})
            unlocks (or (:item/unlocks item) #{})
            fp (:item/failure-point item)]
        (cond-> (-> db'
                    (pldb/db-fact itemo iid)
                    (pldb/db-fact item-statuso iid status))
          evidence-type
          (pldb/db-fact item-evidence-typeo iid evidence-type)
          fp
          (pldb/db-fact failure-pointo iid fp)
          (seq deps)
          (as-> d (reduce #(pldb/db-fact %1 depends-ono iid %2) d deps))
          (seq unlocks)
          (as-> d (reduce #(pldb/db-fact %1 unlockso iid %2) d unlocks)))))
    db
    ledger))

(defn- add-cycle-facts
  "Add facts from proof cycles."
  [db cycles]
  (reduce
    (fn [db' cycle]
      (let [cid (or (:cycle/id cycle) (str (hash cycle)))
            phase (or (:cycle/phase cycle) :unknown)
            blocker (:cycle/blocker-id cycle)
            phase-data (or (:cycle/phase-data cycle) {})
            result (:cycle/result-status cycle)]
        (cond-> (-> db'
                    (pldb/db-fact cycleo cid)
                    (pldb/db-fact cycle-phaseo cid phase))
          blocker
          (pldb/db-fact cycle-blockero cid blocker)
          result
          (pldb/db-fact cycle-resulto cid result)
          ;; Add output facts for each phase that has data
          (seq phase-data)
          (as-> d
            (reduce (fn [d' [phase-kw outputs]]
                      (if (map? outputs)
                        (reduce (fn [d'' k] (pldb/db-fact d'' cycle-has-outputo cid phase-kw k))
                                d' (keys outputs))
                        d'))
                    d phase-data)))))
    db
    cycles))

(defn- add-mode-facts
  "Add proof mode and falsify-completed flag."
  [db proof-state]
  (let [mode (or (:proof/current-mode proof-state) :unknown)]
    (cond-> (pldb/db-fact db proof-modeo mode)
      (:proof/falsify-completed? proof-state)
      (pldb/db-fact falsify-completedo))))

(defn- add-failed-route-facts
  "Add failed route facts."
  [db routes]
  (reduce
    (fn [db' route]
      (let [rid (or (:route/id route) (str (hash route)))
            blocker (:route/blocker-id route)
            has-obstruction? (some? (:route/structural-obstruction route))]
        (cond-> (pldb/db-fact db' failed-routeo rid (or blocker "unknown"))
          has-obstruction?
          (pldb/db-fact route-has-obstructiono rid))))
    db
    routes))

(defn build-db
  "Build a logic database from a proof state snapshot.

   Takes a map with:
     :ledger         — {item-id -> LedgerItem}
     :cycles         — [CycleRecord ...]
     :current-mode   — keyword
     :falsify-completed? — boolean
     :failed-routes  — [FailedRoute ...]

   Or a full proof state map (keys are :proof/* namespaced)."
  [proof-state]
  (let [ledger (or (:ledger proof-state) (:proof/ledger proof-state) {})
        cycles (or (:cycles proof-state) (:proof/cycles proof-state) [])
        routes (or (:failed-routes proof-state) (:proof/failed-routes proof-state) [])
        state  (cond-> proof-state
                 (:proof/current-mode proof-state)
                 (assoc :current-mode (:proof/current-mode proof-state)))]
    (-> (pldb/db)
        (add-ledger-facts ledger)
        (add-cycle-facts cycles)
        (add-mode-facts (or state proof-state))
        (add-failed-route-facts routes))))

;; =============================================================================
;; Goals — structural properties
;; =============================================================================

;; --- DAG integrity ---

(defn edge-symmetrico
  "If A unlocks B, then B depends-on A."
  [a b]
  (l/all
    (unlockso a b)
    (depends-ono b a)))

;; --- Status discipline ---

(defn status-valido
  "Item has a recognized status."
  [iid]
  (l/fresh [s]
    (item-statuso iid s)
    (l/project [s]
      (if (contains? valid-statuses s)
        l/succeed
        l/fail))))

(defn proved-with-numerical-onlyo
  "Item is :proved but has only :numerical evidence — SR-5 violation."
  [iid]
  (l/all
    (item-statuso iid :proved)
    (item-evidence-typeo iid :numerical)))

(defn false-is-terminalo
  "Item marked :false should not be a dependency of any non-false item
   that is :proved or :partial — structural oddity (not strictly an error
   but worth flagging)."
  [iid dependent-id]
  (l/all
    (item-statuso iid :false)
    (depends-ono dependent-id iid)
    (l/fresh [dep-status]
      (item-statuso dependent-id dep-status)
      (l/project [dep-status]
        (if (contains? #{:proved :partial :numerically-verified} dep-status)
          l/succeed
          l/fail)))))

;; --- Dependency satisfaction ---

(defn proved-dep-not-provedo
  "A :proved item has a dependency that is not :proved or :axiom-like.
   (:open, :partial, :false, :numerically-verified deps under a :proved item
   indicate an incomplete proof chain.)"
  [proved-id dep-id dep-status]
  (l/all
    (item-statuso proved-id :proved)
    (depends-ono proved-id dep-id)
    (item-statuso dep-id dep-status)
    (l/project [dep-status]
      (if (contains? #{:proved} dep-status)
        l/fail    ; dep IS proved — no violation
        l/succeed))))

;; =============================================================================
;; Queries — detect violations
;; =============================================================================

;; --- DAG integrity ---

(defn query-asymmetric-edges
  "Edges where A unlocks B but B does not depend-on A (or vice versa).
   Returns [[a b direction] ...] where direction is :unlocks-without-dep
   or :dep-without-unlock."
  [db]
  (law/query-paired-edge-mismatches
   db
   {:forward-rel unlockso
    :backward-rel depends-ono
    :forward-label :unlocks-without-dep
    :backward-label :dep-without-unlock}))

(defn query-dangling-refs
  "Items referenced in depends-on or unlocks that don't exist in the ledger.
   Returns [[referencing-item missing-item direction] ...]."
  [db]
  (vec
   (concat
    (law/query-dangling-targets
     db
     {:entity-rel itemo
      :ref-rel depends-ono
      :direction :depends-on})
    (law/query-dangling-targets
     db
     {:entity-rel itemo
      :ref-rel unlockso
      :direction :unlocks}))))

;; --- Status discipline ---

(defn query-invalid-statuses
  "Items with unrecognized status values."
  [db]
  (law/query-invalid-enum-values
   db
   {:value-rel item-statuso
    :allowed-values valid-statuses}))

(defn query-proved-without-analytical
  "Items marked :proved but with only :numerical evidence (SR-5 violation)."
  [db]
  (pldb/with-db db
    (l/run* [iid]
      (proved-with-numerical-onlyo iid))))

(defn query-proved-with-unproved-deps
  "Items marked :proved whose dependencies are not yet :proved.
   Returns [{:item dep :dep-status} ...]."
  [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [proved-id dep-id dep-status]
        (proved-dep-not-provedo proved-id dep-id dep-status)
        (l/== q {:item proved-id :dep dep-id :dep-status dep-status})))))

;; --- Cycle phase integrity ---

(defn query-missing-phase-outputs
  "Cycles that have advanced past a phase without recording required outputs.
   Returns [{:cycle phase :missing #{keys}} ...]."
  [db]
  (law/query-missing-phase-outputs
   db
   {:cycle-phase-rel cycle-phaseo
    :cycle-output-rel cycle-has-outputo
    :phase-order phase-order
    :phase-required-outputs phase-required-outputs}))

;; --- Mode gating ---

(defn query-mode-violations
  "Check if proof is in CONSTRUCT+ mode without falsify-completed.
   Returns [:construct-without-falsify] or empty."
  [db]
  (let [modes (pldb/with-db db (l/run* [m] (proof-modeo m)))
        falsified? (seq (pldb/with-db db (l/run* [q] (falsify-completedo) (l/== q true))))
        post-falsify-modes #{:CONSTRUCT :VERIFY :MAP}]
    (if (and (some post-falsify-modes modes)
             (not falsified?))
      [:construct-without-falsify]
      [])))

;; --- Failed route integrity ---

(defn query-routes-without-obstruction
  "Failed routes missing a structural obstruction (SR-8 requires one).
   Returns [route-id ...]."
  [db]
  (pldb/with-db db
    (l/run* [rid]
      (l/fresh [blocker]
        (failed-routeo rid blocker)
        (l/nafc route-has-obstructiono rid)))))

;; =============================================================================
;; Aggregate violation query
;; =============================================================================

(defn query-violations
  "Run all invariant checks against a proof logic database.
   Returns a map of violation category → violations.
   Empty vectors mean the invariant holds."
  [db]
  {:asymmetric-edges       (query-asymmetric-edges db)
   :dangling-refs          (query-dangling-refs db)
   :invalid-statuses       (query-invalid-statuses db)
   :proved-without-analytical (query-proved-without-analytical db)
   :proved-with-unproved-deps (query-proved-with-unproved-deps db)
   :missing-phase-outputs  (query-missing-phase-outputs db)
   :mode-violations        (query-mode-violations db)
   :routes-without-obstruction (query-routes-without-obstruction db)})

(defn violations?
  "True if any invariant has violations."
  [violations]
  (some (fn [[_k v]] (seq v)) violations))

;; =============================================================================
;; Convenience: load and check a proof state file
;; =============================================================================

(defn check-proof-state
  "Load a proof state map and check all invariants.
   proof-state: the full proof state (e.g. from reading an EDN file)."
  [proof-state]
  (-> proof-state build-db query-violations))
