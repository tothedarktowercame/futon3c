(ns futon3c.peripheral.mission-logic
  "Relational invariant layer for the Mission Peripheral.

   This namespace is a thin projection of laws the mission backend already
   enforces operationally:

   1. blocker existence for cycle start
   2. obligation status discipline
   3. linear phase progression
   4. required outputs for completed phases

   It intentionally does not promote DAG acyclicity or GF/GD gate checks into
   the foundational subset; those still live in separate runtime surfaces."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [futon3c.logic.structural-law :as law]
            [futon3c.peripheral.mission-shapes :as ms]))

(pldb/db-rel obligationo obligation-id)
(pldb/db-rel obligation-statuso obligation-id status)
(pldb/db-rel obligation-evidence-typeo obligation-id evidence-type)
(pldb/db-rel cycleo cycle-id)
(pldb/db-rel cycle-phaseo cycle-id phase)
(pldb/db-rel cycle-blockero cycle-id blocker-id)
(pldb/db-rel cycle-completed-phaseo cycle-id phase)
(pldb/db-rel cycle-has-outputo cycle-id phase output-key)

(def ^:private valid-statuses
  #{:done :partial :open :blocked :abandoned})

(defn- add-obligation-facts
  [db obligations]
  (reduce-kv
   (fn [db* obligation-id obligation]
     (cond-> (-> db*
                 (pldb/db-fact obligationo obligation-id)
                 (pldb/db-fact obligation-statuso obligation-id
                               (or (:item/status obligation) :unknown)))
       (:item/evidence-type obligation)
       (pldb/db-fact obligation-evidence-typeo obligation-id
                     (:item/evidence-type obligation))))
   db
   obligations))

(defn- add-cycle-facts
  [db cycles]
  (reduce
   (fn [db* cycle]
     (let [cycle-id (or (:cycle/id cycle) (str (hash cycle)))
           phase (or (:cycle/phase cycle) :unknown)
           blocker-id (:cycle/blocker-id cycle)
           completed (or (:cycle/phases-completed cycle) [])
           phase-data (or (:cycle/phase-data cycle) {})]
       (cond-> (-> db*
                   (pldb/db-fact cycleo cycle-id)
                   (pldb/db-fact cycle-phaseo cycle-id phase))
         blocker-id
         (pldb/db-fact cycle-blockero cycle-id blocker-id)
         (seq completed)
         (as-> d
           (reduce (fn [db** completed-phase]
                     (pldb/db-fact db** cycle-completed-phaseo cycle-id completed-phase))
                   d
                   completed))
         (seq phase-data)
         (as-> d
           (reduce (fn [db** [phase-key outputs]]
                     (if (map? outputs)
                       (reduce (fn [db*** output-key]
                                 (pldb/db-fact db*** cycle-has-outputo cycle-id phase-key output-key))
                               db**
                               (keys outputs))
                       db**))
                   d
                   phase-data)))))
   db
   cycles))

(defn build-db
  "Build a logic database from a mission state snapshot."
  [mission-state]
  (let [obligations (or (:obligations mission-state) (:mission/obligations mission-state) {})
        cycles (or (:cycles mission-state) (:mission/cycles mission-state) [])]
    (-> (pldb/db)
        (add-obligation-facts obligations)
        (add-cycle-facts cycles))))

(defn query-missing-blockers
  "Cycles whose blocker does not exist in the obligation ledger."
  [db]
  (law/query-dangling-targets
   db
   {:entity-rel obligationo
    :ref-rel cycle-blockero
    :direction :blocker}))

(defn query-invalid-statuses
  "Obligations with unrecognized statuses."
  [db]
  (law/query-invalid-enum-values
   db
   {:value-rel obligation-statuso
    :allowed-values valid-statuses}))

(defn query-done-with-assertion-only
  "Obligations marked :done despite only :assertion evidence."
  [db]
  (pldb/with-db db
    (l/run* [obligation-id]
      (obligation-statuso obligation-id :done)
      (obligation-evidence-typeo obligation-id :assertion))))

(defn query-phase-order-violations
  "Cycles whose completed-phase set does not match the prefix before the
   current phase."
  [db]
  (law/query-phase-prefix-mismatches
   db
   {:cycle-phase-rel cycle-phaseo
    :completed-phase-rel cycle-completed-phaseo
    :phase-order ms/phase-order}))

(defn query-missing-phase-outputs
  "Cycles that have passed a phase without recording the required outputs."
  [db]
  (law/query-missing-phase-outputs
   db
   {:cycle-phase-rel cycle-phaseo
    :cycle-output-rel cycle-has-outputo
    :phase-order ms/phase-order
    :phase-required-outputs ms/phase-required-outputs}))

(defn query-violations
  [db]
  {:missing-blockers (query-missing-blockers db)
   :invalid-statuses (query-invalid-statuses db)
   :done-with-assertion-only (query-done-with-assertion-only db)
   :phase-order-violations (query-phase-order-violations db)
   :missing-phase-outputs (query-missing-phase-outputs db)})

(defn violations?
  [violations]
  (some (fn [[_k v]] (seq v)) violations))

(defn check-mission-state
  [mission-state]
  (-> mission-state build-db query-violations))
