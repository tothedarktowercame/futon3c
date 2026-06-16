(ns ^{:clj-kondo/config
      '{:linters
        {:unresolved-symbol
         {:exclude [(clojure.core.logic.pldb/db-rel)
                    (clojure.core.logic/fresh)
                    (clojure.core.logic/run*)]}}}}
  futon3c.logic.outreach-intake-guard
  "VERIFY logic model for C-pudding-prover §8.1 / eoi-lifecycle Phase-4.

   Offline-only guard for abstract :outreach-sent evidence events. This is
   Car 1 of kit-intake: prove the two anti-laundering paths are unrepresentable
   before any runtime intake verb, registry write, XTDB touch, or real-world
   sender is wired.

   House idiom copied from futon3c.logic.business-coupling-invariants:
   trace -> build-db -> pldb goals -> query-violations -> run-verify.

   Invariants encoded, exactly:
     lead-class-matches-thesis
       every :plants-thesis :T2.3-cold record must have
       :lead-class :cold-scan-lead. Cold is relational: the path to the person,
       not topic match, determines lead-class.
     send-witness-present
       every :event :outreach-sent record must carry non-nil :send-witness."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations (fact schema over an abstract outreach evidence-event trace)
;; =============================================================================

(pldb/db-rel stepo          sid)
(pldb/db-rel evento         sid event)
(pldb/db-rel plants-thesiso sid thesis)
(pldb/db-rel lead-classo    sid lead-class)
(pldb/db-rel send-witnesso  sid witness)

(declare stepo evento plants-thesiso lead-classo send-witnesso)

;; =============================================================================
;; Database construction -- a trace is a vector of abstract event records
;; =============================================================================
;;
;; record keys:
;;   :t int
;;   :event keyword
;;   :plants-thesis keyword
;;   :lead-class keyword
;;   :send-witness nil | string
;; =============================================================================

(defn build-db
  "Assert an abstract outreach evidence-event trace into a pldb fact database."
  [trace]
  (reduce
   (fn [db {:keys [t event plants-thesis lead-class send-witness]}]
     (cond-> (pldb/db-fact db stepo t)
       event                (pldb/db-fact evento t event)
       plants-thesis        (pldb/db-fact plants-thesiso t plants-thesis)
       lead-class           (pldb/db-fact lead-classo t lead-class)
       (some? send-witness) (pldb/db-fact send-witnesso t send-witness)))
   pldb/empty-db
   trace))

(defn- has-send-witnesso [s]
  (l/fresh [witness] (send-witnesso s witness)))

;; =============================================================================
;; Queries -- detect violations (empty vector = invariant holds)
;; =============================================================================

(defn q-lead-class-matches-thesis [db]
  ;; T2.3-cold is the cold-conversion thesis. Only relationally cold leads may
  ;; camp it; warm/known lead paths must route elsewhere even if topically apt.
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s lead-class]
        (plants-thesiso s :T2.3-cold)
        (lead-classo s lead-class)
        (l/!= lead-class :cold-scan-lead)
        (l/== q {:t s
                 :plants-thesis :T2.3-cold
                 :lead-class lead-class
                 :violation :lead-class-mismatch})))))

(defn q-send-witness-present [db]
  ;; A claimed :outreach-sent event without the SEND's own proof is not evidence.
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s]
        (evento s :outreach-sent)
        (l/nafc has-send-witnesso s)
        (l/== q {:t s
                 :event :outreach-sent
                 :violation :outreach-sent-without-send-witness})))))

(defn query-violations
  "All outreach-intake guard checks. category -> violating events."
  [db]
  {:lead-class-matches-thesis (q-lead-class-matches-thesis db)
   :send-witness-present      (q-send-witness-present db)})

(defn violations? [violations]
  (some (fn [[_ v]] (seq v)) violations))

;; =============================================================================
;; Fixtures -- witnessed conforming trace + one adversarial trace per invariant
;; =============================================================================

(def conforming-cold-event
  {:t 1
   :event :outreach-sent
   :plants-thesis :T2.3-cold
   :lead-class :cold-scan-lead
   :send-witness "msg-id-123"})

(def conforming-hedge-known-event
  {:t 2
   :event :outreach-sent
   :plants-thesis :hedge-anthropic
   :lead-class :known
   :send-witness "form-confirm"})

(def conforming-trace
  [conforming-cold-event conforming-hedge-known-event])

(def adversarial-traces
  "One trace per invariant, each planted to break exactly that invariant."
  {:lead-class-matches-thesis
   [(assoc conforming-cold-event :lead-class :known :send-witness "x")]

   :send-witness-present
   [(assoc conforming-cold-event :send-witness nil)]})

(defn- verify-trace [trace]
  (let [v (query-violations (build-db trace))
        clean? (not (violations? v))]
    {:verified? clean?
     :witness {:clean? clean? :violations v}
     :summary {:witness-clean? clean?}}))

(defn run-verify
  "VERIFY entrypoint. With no args, the conforming witness must be clean and
   each adversarial trace must be caught by its own category. With a trace arg,
   verify just that trace."
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
