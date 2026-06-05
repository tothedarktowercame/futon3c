(ns ^{:clj-kondo/config
      '{:linters
        {:unresolved-symbol
         {:exclude [(clojure.core.logic.pldb/db-rel)
                    (clojure.core.logic/fresh)
                    (clojure.core.logic/run*)]}}}}
  futon3c.logic.wm-operator-lane-invariants
  "VERIFY logic model for E-wm-operator-lane. This is a pure abstract
   pldb/core.logic model: surfaced items are facts, lane assignment is a
   relation, and the queries check the six design invariants before any War
   Machine production code exists."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations over an abstract surfaced-item trace
;; =============================================================================

(pldb/db-rel itemo               id)
(pldb/db-rel in-joes-modelo      id)
(pldb/db-rel futon-importanto    id)
(pldb/db-rel risk-modeo          id)
(pldb/db-rel acknowledgedo       id)
(pldb/db-rel operator-dependento id)
(pldb/db-rel framing-blockedo    id)
(pldb/db-rel modeo               id mode)
(pldb/db-rel laneo               id lane)    ; :silent | :brief | :nag
(pldb/db-rel admissibleo         id)
(pldb/db-rel predicted-importanto id)        ; adversarial-only; never part of the gate

(declare itemo in-joes-modelo futon-importanto risk-modeo acknowledgedo
         operator-dependento framing-blockedo modeo laneo admissibleo
         predicted-importanto)

(def lanes #{:silent :brief :nag})

(defn- seqable-lanes [{:keys [lane lanes]}]
  (cond
    (seq lanes) lanes
    lane [lane]
    :else []))

(defn build-db
  "Assert an abstract item trace into a pldb fact database.

   Item keys:
   :id, :in-joes-model?, :futon-important?, :risk-mode?, :acknowledged?,
   :operator-dependent?, :framing-blocked?, :mode, :lane or :lanes,
   :admissible?, and adversarial-only :predicted-important?."
  [trace]
  (reduce
   (fn [db {:keys [id in-joes-model? futon-important? risk-mode? acknowledged?
                   operator-dependent? framing-blocked? mode admissible?
                   predicted-important?]
            :as item}]
     (let [db* (cond-> (pldb/db-fact db itemo id)
                 in-joes-model?      (pldb/db-fact in-joes-modelo id)
                 futon-important?    (pldb/db-fact futon-importanto id)
                 risk-mode?          (pldb/db-fact risk-modeo id)
                 acknowledged?       (pldb/db-fact acknowledgedo id)
                 operator-dependent? (pldb/db-fact operator-dependento id)
                 framing-blocked?    (pldb/db-fact framing-blockedo id)
                 mode                (pldb/db-fact modeo id mode)
                 admissible?         (pldb/db-fact admissibleo id)
                 predicted-important? (pldb/db-fact predicted-importanto id))]
       (reduce (fn [db lane]
                 (pldb/db-fact db laneo id lane))
               db*
               (seqable-lanes item))))
   pldb/empty-db
   trace))

(defn- has-laneo [id]
  (l/fresh [lane] (laneo id lane)))

;; =============================================================================
;; Invariant queries — empty vector means invariant holds
;; =============================================================================

(defn q-inv-1-no-autonomous-fire-on-framing-blocked [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [id]
        (itemo id)
        (modeo id :autonomous)
        (framing-blockedo id)
        (admissibleo id)
        (l/== q {:item id :violation :autonomous-framing-blocked-admissible})))))

(defn q-inv-2-lane-totality-exclusivity [db]
  (let [missing
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [id]
              (itemo id)
              (l/nafc has-laneo id)
              (l/== q {:item id :violation :missing-lane}))))
        invalid
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [id lane]
              (laneo id lane)
              (l/pred lane #(not (contains? lanes %)))
              (l/== q {:item id :lane lane :violation :invalid-lane}))))
        multiple
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [id lane-a lane-b]
              (laneo id lane-a)
              (laneo id lane-b)
              (l/!= lane-a lane-b)
              (l/== q {:item id :lanes [lane-a lane-b]
                       :violation :multiple-lanes}))))]
    (vec (concat missing invalid multiple))))

(defn q-inv-3-nag-gate-conjunction [db]
  (let [missing-joe
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [id]
              (laneo id :nag)
              (l/nafc in-joes-modelo id)
              (l/== q {:item id :missing :in-joes-model?
                       :violation :nag-without-full-conjunction}))))
        missing-important
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [id]
              (laneo id :nag)
              (l/nafc futon-importanto id)
              (l/== q {:item id :missing :futon-important?
                       :violation :nag-without-full-conjunction}))))
        missing-risk
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [id]
              (laneo id :nag)
              (l/nafc risk-modeo id)
              (l/== q {:item id :missing :risk-mode?
                       :violation :nag-without-full-conjunction}))))]
    (vec (concat missing-joe missing-important missing-risk))))

(defn q-inv-4-novelty-flows-down [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [id]
        (laneo id :nag)
        (l/nafc acknowledgedo id)
        (l/== q {:item id :violation :novel-item-nagged})))))

(defn q-inv-5-descriptive-only-importance [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [id]
        (laneo id :nag)
        (predicted-importanto id)
        (l/nafc futon-importanto id)
        (l/== q {:item id :violation :predicted-importance-used-as-gate
                 :ignored-field :predicted-important?})))))

(defn q-inv-6-silent-subset-dischargeable [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [id]
        (laneo id :silent)
        (operator-dependento id)
        (l/== q {:item id :violation :silent-operator-dependent})))))

(defn query-violations [db]
  {:inv-1-no-autonomous-fire-on-framing-blocked (q-inv-1-no-autonomous-fire-on-framing-blocked db)
   :inv-2-lane-totality-exclusivity             (q-inv-2-lane-totality-exclusivity db)
   :inv-3-nag-gate-conjunction                  (q-inv-3-nag-gate-conjunction db)
   :inv-4-novelty-flows-down                    (q-inv-4-novelty-flows-down db)
   :inv-5-descriptive-only-importance           (q-inv-5-descriptive-only-importance db)
   :inv-6-silent-subset-dischargeable           (q-inv-6-silent-subset-dischargeable db)})

(defn violations? [violations]
  (some (fn [[_ hits]] (seq hits)) violations))

;; =============================================================================
;; Witness + adversarial fixtures
;; =============================================================================

(def witness-trace
  [{:id :silent-autonomous-fix
    :mode :autonomous
    :lane :silent
    :admissible? true}
   {:id :brief-novel-important
    :mode :autonomous
    :lane :brief
    :futon-important? true}
   {:id :brief-framing-blocked
    :mode :autonomous
    :lane :brief
    :framing-blocked? true
    :operator-dependent? true
    :futon-important? true
    :in-joes-model? true}
   {:id :nag-earned-critical
    :mode :supervised
    :lane :nag
    :admissible? true
    :in-joes-model? true
    :futon-important? true
    :risk-mode? true
    :acknowledged? true
    :operator-dependent? true}])

(def adversarial-traces
  {:inv-1-no-autonomous-fire-on-framing-blocked
   [(assoc (nth witness-trace 2) :id :bad-autonomous-fire :admissible? true)]

   :inv-2-lane-totality-exclusivity
   [{:id :bad-two-lanes
     :mode :autonomous
     :lanes [:brief :nag]
     :in-joes-model? true
     :futon-important? true
     :risk-mode? true
     :acknowledged? true}]

   :inv-3-nag-gate-conjunction
   [{:id :bad-two-of-three-sneaky-nag
     :mode :autonomous
     :lane :nag
     :in-joes-model? true
     :futon-important? true
     :risk-mode? false
     :acknowledged? true}]

   :inv-4-novelty-flows-down
   [{:id :bad-novel-nag
     :mode :autonomous
     :lane :nag
     :in-joes-model? true
     :futon-important? true
     :risk-mode? true
     :acknowledged? false}]

   :inv-5-descriptive-only-importance
   [{:id :bad-predicted-only-nag
     :mode :autonomous
     :lane :nag
     :in-joes-model? true
     :futon-important? false
     :predicted-important? true
     :risk-mode? true
     :acknowledged? true}]

   :inv-6-silent-subset-dischargeable
   [{:id :bad-silent-needs-joe
     :mode :autonomous
     :lane :silent
     :operator-dependent? true}]})

(defn run-verify []
  (let [witness-v (query-violations (build-db witness-trace))
        witness-clean? (not (violations? witness-v))
        adversarial
        (into {}
              (map (fn [[cat trace]]
                     (let [v (query-violations (build-db trace))
                           hits (get v cat)]
                       [cat {:caught? (boolean (seq hits))
                             :hits hits
                             :all-violations v}]))
                   adversarial-traces))
        all-caught? (every? :caught? (vals adversarial))]
    {:verified? (and witness-clean? all-caught?)
     :witness {:clean? witness-clean? :violations witness-v}
     :adversarial adversarial
     :summary {:witness-clean? witness-clean?
               :all-adversarial-caught? all-caught?
               :n-invariants (count adversarial-traces)}}))
