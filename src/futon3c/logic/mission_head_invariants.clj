(ns futon3c.logic.mission-head-invariants
  "VERIFY V0 logic model for E-mission-head (futon6/holes/missions/
   E-mission-head.md §5) — the design's structural invariants checked before
   further build, per `library/mission-coherence/logic-model-before-code`
   (which is itself the fourth pattern in the mission's own ARGUE cascade:
   the argument prescribes this VERIFY).

   House idiom (aif2_invariants, outing-invariants): an ABSTRACT trace of
   lifeform events as step-records; a design is VERIFIED iff the conforming
   witness yields zero violations AND each adversarial trace (one per
   invariant) is caught by its own category.

   Invariants (the mission's amendments + the store discipline it rests on):
     INV-measure-never-target  (A1) no gating/optimizing decision consumes
                               the health readout — observe/audit only
     INV-wire-or-die           (A2) at the deadline horizon, no arrow may
                               remain :open while the lifeform claims :alive —
                               promote or self-declare :moribund
     INV-mode-crossing         a promotion :open→:constructed carries a
                               construction artifact; attestation alone never
                               crosses the mode (anti-laundering, BHK)
     INV-sigil-provenance      a sigil derives from the HEAD's sections;
                               hand-set sigil bits are forbidden
     INV-falsifier-first       every construction event has a falsifier
                               authored for its claim at an EARLIER step

   OFFLINE-ONLY for VERIFY; candidates for live probe families at INSTANTIATE."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations (fact schema over a lifeform-event trace)
;; =============================================================================

(pldb/db-rel stepo             sid)
(pldb/db-rel decisiono         sid kind)      ; :gate | :optimize | :observe | :audit
(pldb/db-rel consumes-healtho  sid)           ; decision input includes the readout
(pldb/db-rel promoteo          sid aid)       ; arrow aid promoted :open→:constructed
(pldb/db-rel promote-evidenceo sid ev)        ; :construction | :attestation
(pldb/db-rel deadline-stepo    sid)           ; this step is the A2 horizon
(pldb/db-rel open-at-stepo     sid aid)       ; arrow aid still :open at step
(pldb/db-rel lifeform-stateo   sid state)     ; :alive | :moribund (claimed at step)
(pldb/db-rel sigilo            sid src)       ; :head-sections | :hand-set
(pldb/db-rel constructiono     sid cid)       ; construction event for claim cid
(pldb/db-rel falsifiero        sid cid)       ; falsifier authored for claim cid

;; =============================================================================
;; Database construction — a trace is a vector of step-records
;; =============================================================================

(defn build-db
  "Assert a lifeform-event trace into a pldb fact database."
  [trace]
  (reduce
   (fn [db {:keys [step decision-kind consumes-health? promote-arrow
                   promote-evidence deadline? open-arrow lifeform-state
                   sigil-source construction-claim falsifier-claim]}]
     (cond-> (pldb/db-fact db stepo step)
       decision-kind      (pldb/db-fact decisiono step decision-kind)
       consumes-health?   (pldb/db-fact consumes-healtho step)
       promote-arrow      (pldb/db-fact promoteo step promote-arrow)
       promote-evidence   (pldb/db-fact promote-evidenceo step promote-evidence)
       deadline?          (pldb/db-fact deadline-stepo step)
       open-arrow         (pldb/db-fact open-at-stepo step open-arrow)
       lifeform-state     (pldb/db-fact lifeform-stateo step lifeform-state)
       sigil-source       (pldb/db-fact sigilo step sigil-source)
       construction-claim (pldb/db-fact constructiono step construction-claim)
       falsifier-claim    (pldb/db-fact falsifiero step falsifier-claim)))
   pldb/empty-db
   trace))

(defn- has-falsifiero [c] (l/fresh [s] (falsifiero s c)))

;; =============================================================================
;; Queries — detect violations (empty vector = invariant holds)
;; =============================================================================

(defn q-measure-never-target [db]
  ;; a :gate or :optimize decision that consumes the health readout
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s k]
        (decisiono s k)
        (l/membero k [:gate :optimize])
        (consumes-healtho s)
        (l/== q {:step s :decision-kind k
                 :violation :health-readout-as-target})))))

(defn q-wire-or-die [db]
  ;; at a deadline step: an arrow still :open while the lifeform claims :alive
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s a]
        (deadline-stepo s)
        (open-at-stepo s a)
        (lifeform-stateo s :alive)
        (l/== q {:step s :arrow a
                 :violation :open-past-deadline-still-alive})))))

(defn q-mode-crossing [db]
  ;; a promotion whose evidence is attestation (or anything non-construction)
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s a ev]
        (promoteo s a)
        (promote-evidenceo s ev)
        (l/!= ev :construction)
        (l/== q {:step s :arrow a :evidence ev
                 :violation :promotion-without-construction})))))

(defn q-sigil-provenance [db]
  ;; a sigil from anywhere but the HEAD's sections
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s src]
        (sigilo s src)
        (l/!= src :head-sections)
        (l/== q {:step s :source src
                 :violation :hand-set-sigil})))))

(defn q-falsifier-first [db]
  ;; (a) a construction whose claim has NO falsifier at all;
  ;; (b) a construction all of whose falsifiers come LATER (ground arithmetic
  ;;     via project — facts are ground).
  (let [no-falsifier
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [s c]
              (constructiono s c)
              (l/nafc has-falsifiero c)
              (l/== q {:step s :claim c
                       :violation :construction-without-falsifier}))))
        earliest-falsifier
        (fn [c]
          (let [steps (pldb/with-db db
                        (l/run* [s] (falsifiero s c)))]
            (when (seq steps) (apply min steps))))
        late-falsifier
        (->> (pldb/with-db db
               (l/run* [q] (l/fresh [s c]
                             (constructiono s c)
                             (l/== q [s c]))))
             (keep (fn [[s c]]
                     (when-let [ef (earliest-falsifier c)]
                       (when (>= ef s)
                         {:step s :claim c :earliest-falsifier ef
                          :violation :falsifier-after-construction}))))
             vec)]
    (vec (concat no-falsifier late-falsifier))))

(defn query-violations
  "All structural invariant checks. category -> violating steps (empty = holds)."
  [db]
  {:measure-never-target (q-measure-never-target db)
   :wire-or-die          (q-wire-or-die db)
   :mode-crossing        (q-mode-crossing db)
   :sigil-provenance     (q-sigil-provenance db)
   :falsifier-first      (q-falsifier-first db)})

(defn violations? [violations]
  (some (fn [[_ v]] (seq v)) violations))

;; =============================================================================
;; Fixtures — witnessed conforming trace + one adversarial trace per invariant
;; =============================================================================

(def witness-trace
  "The intended E-mission-head story: falsifier authored first (F1), the
   sigil compiled from HEAD sections, the construction lands, health is
   consumed by observation and audit only, all arrows promoted (with
   constructions) before the deadline, lifeform alive at the horizon."
  [{:step 1 :falsifier-claim :c/sectioning-works}          ; F1 authored
   {:step 2 :sigil-source :head-sections}                  ; Golemization
   {:step 3 :construction-claim :c/sectioning-works}       ; v0.1 lands
   {:step 4 :decision-kind :observe :consumes-health? true} ; readout observed
   {:step 5 :decision-kind :audit :consumes-health? true}   ; sparse audit
   {:step 6 :promote-arrow :a/health-readout :promote-evidence :construction}
   {:step 7 :promote-arrow :a/seeded-beliefs :promote-evidence :construction}
   {:step 8 :promote-arrow :a/lifeform-lane :promote-evidence :construction}
   {:step 9 :deadline? true :lifeform-state :alive}])      ; horizon: nothing open

(def adversarial-traces
  "One trace per invariant, each planted to break exactly that invariant."
  {:measure-never-target
   [{:step 1 :decision-kind :optimize :consumes-health? true}]
   :wire-or-die
   [{:step 1 :deadline? true :open-arrow :a/health-readout :lifeform-state :alive}]
   :mode-crossing
   [{:step 1 :promote-arrow :a/health-readout :promote-evidence :attestation}]
   :sigil-provenance
   [{:step 1 :sigil-source :hand-set}]
   :falsifier-first
   [{:step 1 :construction-claim :c/unfalsified}
    {:step 2 :falsifier-claim :c/unfalsified}]})           ; falsifier too late

(defn run-verify
  "VERIFY entrypoint: witness must be clean; each adversarial trace must be
   caught BY ITS OWN category. Returns a report; :verified? is the gate."
  []
  (let [witness-v (query-violations (build-db witness-trace))
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
