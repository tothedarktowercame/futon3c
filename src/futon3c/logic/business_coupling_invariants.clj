(ns ^{:clj-kondo/config
      '{:linters
        {:unresolved-symbol
         {:exclude [(clojure.core.logic.pldb/db-rel)
                    (clojure.core.logic/fresh)
                    (clojure.core.logic/run*)]}}}}
  futon3c.logic.business-coupling-invariants
  "VERIFY slice-1 logic model for M-futon-forward-model §8.4 / §9.

   House idiom copied from futon3c.logic.aif2-invariants:
   snapshot -> build-db -> goals -> query-violations over clojure.core.logic
   + pldb. The snapshot is an ABSTRACT click/tick trace, so adversarial traces
   can be enumerated and the business-coupling guarantees checked without any
   live War Machine, XTDB, outbound sender, or real-world side effect.

   A design is VERIFIED iff:
     (a) the witnessed conforming trace yields ZERO violations, and
     (b) each adversarial trace (one per invariant) is CAUGHT by its category.

   Invariants encoded:
     consent              every outbound click has operator/autopen consent
     no-bypass            posterior deltas occur only on inbound clicks
     provenance           inbound posterior deltas carry interest-event
                          evidence refs
     realized-precedence  tick-derived/expected deltas cannot override realized
                          values for the same target/field."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations (fact schema over an abstract business click/tick trace)
;; =============================================================================

(pldb/db-rel stepo             sid)
(pldb/db-rel triggero          sid trigger)
(pldb/db-rel kindo             sid kind)
(pldb/db-rel consento          sid by at)
(pldb/db-rel evidence-anchoro  sid anchor)
(pldb/db-rel posterior-deltao  sid target field from to)
(pldb/db-rel realizedo         sid realized?)
(pldb/db-rel valid-posterior-stateo state)

(declare stepo triggero kindo consento evidence-anchoro posterior-deltao
         realizedo valid-posterior-stateo)

(def interest-event-posterior-states
  "From futon5a/data/interest-event-vocabulary.edn :standings. Kept inline in
   the abstract model so VERIFY does not read or write the live interest store."
  #{:live :spawned :refined :strengthened :addressed :falsified :foreclosed :reopened})

;; =============================================================================
;; Database construction -- a trace is a vector of click/tick records
;; =============================================================================
;;
;; record keys (all optional except :t):
;;   :t int
;;   :trigger #{:tick :inbound-click :outbound-click}
;;   :kind keyword
;;   :consent nil | {:by str :at str}
;;   :evidence-anchor nil | str | [str ...] ; event :evidence/refs
;;   :posterior-delta nil | {:target keyword|string ; event :target/entity-id
;;                           :field :posterior-state
;;                           :from keyword :to keyword}
;;   :realized? bool
;; =============================================================================

(defn build-db
  "Assert an abstract business click/tick trace into a pldb fact database."
  [trace]
  (reduce
   (fn [db {:keys [t trigger kind consent evidence-anchor posterior-delta realized?]}]
     (let [{:keys [by at]} consent
           {:keys [target field from to]} posterior-delta]
       (cond-> (pldb/db-fact db stepo t)
         trigger          (pldb/db-fact triggero t trigger)
         kind             (pldb/db-fact kindo t kind)
         consent          (pldb/db-fact consento t by at)
         evidence-anchor  (pldb/db-fact evidence-anchoro t evidence-anchor)
         posterior-delta  (pldb/db-fact posterior-deltao t target field from to)
         (some? realized?) (pldb/db-fact realizedo t realized?))))
   (reduce (fn [db state]
             (pldb/db-fact db valid-posterior-stateo state))
           pldb/empty-db
           interest-event-posterior-states)
   trace))

(defn- has-consento [s]
  (l/fresh [by at] (consento s by at)))

(defn- has-evidence-anchoro [s]
  (l/fresh [anchor] (evidence-anchoro s anchor)))

;; =============================================================================
;; Queries -- detect violations (empty vector = invariant holds)
;; =============================================================================

(defn q-consent [db]
  ;; every real-world outbound click must have an operator/autopen consent anchor
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s]
        (triggero s :outbound-click)
        (l/nafc has-consento s)
        (l/== q {:t s :violation :outbound-click-without-consent})))))

(defn q-no-bypass [db]
  ;; posterior deltas are interest-event posterior-state moves. They may only be
  ;; emitted by inbound clicks, never ticks/outbound.
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s trigger target field from to]
        (posterior-deltao s target field from to)
        (triggero s trigger)
        (l/!= trigger :inbound-click)
        (l/== q {:t s
                 :trigger trigger
                 :target target
                 :field field
                 :violation :posterior-delta-without-inbound-click})))))

(defn q-provenance [db]
  ;; inbound interest-events that move posterior state need event :evidence/refs
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s target field from to]
        (triggero s :inbound-click)
        (posterior-deltao s target field from to)
        (l/nafc has-evidence-anchoro s)
        (l/== q {:t s
                 :target target
                 :field field
                 :violation :posterior-delta-without-evidence-anchor})))))

(defn q-realized-precedence [db]
  ;; an expected/unrealized posterior delta cannot override a realized value for
  ;; the same target/field. The realized trace is authoritative over the model.
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s realized-s target field from to r-from r-to]
        (posterior-deltao s target field from to)
        (realizedo s false)
        (posterior-deltao realized-s target field r-from r-to)
        (realizedo realized-s true)
        (l/!= s realized-s)
        (l/== q {:t s
                 :realized-t realized-s
                 :target target
                 :field field
                 :violation :expected-delta-overrides-realized})))))

(defn query-violations
  "All structural invariant checks. category -> violating steps (empty = holds)."
  [db]
  {:consent             (q-consent db)
   :no-bypass           (q-no-bypass db)
   :provenance          (q-provenance db)
   :realized-precedence (q-realized-precedence db)})

(defn violations? [violations]
  (some (fn [[_ v]] (seq v)) violations))

;; =============================================================================
;; Fixtures -- witnessed conforming trace + one adversarial trace per invariant
;; =============================================================================

(def conforming-tick
  {:t 1 :trigger :tick :kind :calendar-advance :realized? false})

(def conforming-outbound
  {:t 2 :trigger :outbound-click :kind :send-eoi
   :consent {:by "joe" :at "2026-06-03T12:00:00Z"}
   :realized? true})

(def conforming-inbound
  {:t 3 :trigger :inbound-click :kind :eoi-reply
   :evidence-anchor ["sales.edn#journey/eric-reply-2026-06-03"]
   :posterior-delta {:target "vsat-poc-2026-q2-q3-scenario-c-vsatlatarium"
                     :field :posterior-state
                     :from :live
                     :to :strengthened}
   :realized? true})

(def conforming-trace
  [conforming-tick conforming-outbound conforming-inbound])

(def adversarial-traces
  "One trace per invariant, each planted to break exactly that invariant."
  {:consent
   [(dissoc conforming-outbound :consent)]

   :no-bypass
   [(assoc conforming-tick
           :posterior-delta {:target "vsat-poc-2026-q2-q3-scenario-c-vsatlatarium"
                             :field :posterior-state
                             :from :live
                             :to :refined})]

   :provenance
   [(dissoc conforming-inbound :evidence-anchor)]

   :realized-precedence
   [conforming-inbound
    {:t 4 :trigger :inbound-click :kind :model-replay
     :evidence-anchor ["roi-results.edn#default-effort"]
     :posterior-delta {:target "vsat-poc-2026-q2-q3-scenario-c-vsatlatarium"
                       :field :posterior-state
                       :from :strengthened
                       :to :refined}
     :realized? false}]})

(defn- verify-trace [trace]
  (let [v (query-violations (build-db trace))
        clean? (not (violations? v))]
    {:verified? clean?
     :witness {:clean? clean? :violations v}
     :summary {:witness-clean? clean?}}))

(defn run-verify
  "VERIFY entrypoint. With no args, witness must be clean and each adversarial
   trace must be caught BY ITS OWN category. With a trace arg, verify just that
   trace; this supports the M-futon-forward-model §9.6 test shape."
  ([]
   (let [witness-v (query-violations (build-db conforming-trace))
         witness-clean? (not (violations? witness-v))
         adversarial
         (into {}
               (map (fn [[cat trace]]
                      (let [v (query-violations (build-db trace))
                            caught? (seq (get v cat))]
                        [cat {:caught? (boolean caught?) :hits (get v cat)}]))
                    adversarial-traces))
         all-caught? (every? :caught? (vals adversarial))]
     {:verified? (and witness-clean? all-caught?)
      :witness {:clean? witness-clean? :violations witness-v}
      :adversarial adversarial
      :summary {:witness-clean? witness-clean?
                :all-adversarial-caught? all-caught?
                :n-invariants (count adversarial-traces)}}))
  ([trace]
   (verify-trace trace)))
