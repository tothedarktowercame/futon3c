(ns futon3c.logic.capability-star-map-invariants
  "VERIFY logic model for M-capability-star-map (futon0/holes/missions/, §DERIVE
   INV-1..INV-G + the decompose line + the operator-verify gate) — the design's
   structural invariants checked BEFORE any code, per the captured pattern
   `library/mission-coherence/logic-model-before-code` (template:
   futon3c.logic.aif2-invariants / outing-invariants).

   House idiom: an ABSTRACT trace (a vector of WM-action step-records) is
   asserted into a pldb fact-db; invariant violations are core.logic / plain
   queries over it. A design is VERIFIED iff (a) the conforming witness trace
   yields ZERO violations, and (b) each adversarial trace (one per invariant) is
   CAUGHT by its own category.

   Invariants (M-capability-star-map DERIVE):
     :acyclic       the :requires graph is a DAG (a cycle => no toposort) [INV-1]
     :provenance    every :satisfied capability has a :complete producing
                    mission (no perfect-crime at the capability level)    [INV-2]
     :applicability a mission is only ADVANCED when its :requires are
                    satisfied                                             [INV-3]
     :buck          THE BUCK STOPS HERE — a capability NOT in the
                    pre-registered brief is never PURSUED without consent;
                    discovery is free, pursuit is gated                   [INV-G]
     :decompose     a goal-EXTENDING decompose (a big ∇-deform) needs
                    consent; a small path-refining decompose is free
     :gate          an ADVANCE never crosses an operator-verify exit (an
                    un-agreed gap) without agreement/consent — the
                    lifecycle's own exits ARE the gates

   Note: INV-4 (single-cycle-leaf) is a granularity predicate over the live
   hole-counter, not a pure trace relation — it is a VERIFY-spike/INSTANTIATE
   check, not modelled here. OFFLINE-ONLY for VERIFY."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Fact schema over an abstract WM-action trace
;; =============================================================================

(pldb/db-rel stepo               s)
(pldb/db-rel actiono             s a)   ; :advance | :pursue | :decompose | :discover
(pldb/db-rel capabilityo         s c)   ; the capability an action concerns
(pldb/db-rel missiono            s m)   ; the mission an action concerns
(pldb/db-rel pre-registeredo     s)     ; the capability is in the pre-registered brief
(pldb/db-rel consento            s)     ; operator consent granted for this step
(pldb/db-rel requires-sato       s)     ; the mission's :requires are all satisfied
(pldb/db-rel crosses-exito       s)     ; the advance crosses an operator-verify exit
(pldb/db-rel gap-agreedo         s)     ; the gap/scope is operator-agreed (exit passed)
(pldb/db-rel extends-goalo       s)     ; a decompose that extends the goal-set (big ∇-deform)
(pldb/db-rel satisfied-capo      s c)   ; a capability became :satisfied at this step
(pldb/db-rel minted-by-completeo s)     ; the satisfying mission is :complete

;; =============================================================================
;; Database construction
;; =============================================================================
;; step-record keys (all optional except :step):
;;   :step :action :capability :mission :pre-registered? :consent-granted?
;;   :requires-sat? :crosses-exit? :gap-agreed? :extends-goal?
;;   :satisfied-cap :minted-by-complete?  + :edge-from/:edge-to (for :acyclic)
;; =============================================================================

(defn build-db
  [trace]
  (reduce
   (fn [db {:keys [step action capability mission pre-registered? consent-granted?
                   requires-sat? crosses-exit? gap-agreed? extends-goal?
                   satisfied-cap minted-by-complete?]}]
     (cond-> (pldb/db-fact db stepo step)
       action              (pldb/db-fact actiono step action)
       capability          (pldb/db-fact capabilityo step capability)
       mission             (pldb/db-fact missiono step mission)
       pre-registered?     (pldb/db-fact pre-registeredo step)
       consent-granted?    (pldb/db-fact consento step)
       requires-sat?       (pldb/db-fact requires-sato step)
       crosses-exit?       (pldb/db-fact crosses-exito step)
       gap-agreed?         (pldb/db-fact gap-agreedo step)
       extends-goal?       (pldb/db-fact extends-goalo step)
       satisfied-cap       (pldb/db-fact satisfied-capo step satisfied-cap)
       minted-by-complete? (pldb/db-fact minted-by-completeo step)))
   pldb/empty-db
   trace))

;; =============================================================================
;; Queries — each returns violating steps (empty = invariant holds)
;; =============================================================================

(defn- edges-of [trace]
  (vec (for [{:keys [edge-from edge-to]} trace :when (and edge-from edge-to)]
         [edge-from edge-to])))

(defn q-acyclic
  "INV-1: a cycle in the :requires edges ⇒ no topological build-order exists.
   Transitive closure by fixpoint (monotone, terminates), then self-reach."
  [trace]
  (let [es    (edges-of trace)
        adj   (reduce (fn [m [a b]] (update m a (fnil conj #{}) b)) {} es)
        nodes (set (mapcat seq es))
        reach (loop [r adj]
                (let [r' (into {} (for [[a bs] r]
                                    [a (into bs (mapcat #(get r % #{}) bs))]))]
                  (if (= r' r) r (recur r'))))]
    (vec (for [n nodes :when (contains? (get reach n #{}) n)]
           {:node n :violation :requires-cycle}))))

(defn q-provenance [db]
  ;; INV-2: a :satisfied capability whose producing mission is not :complete
  (pldb/with-db db
    (l/run* [q] (l/fresh [s c]
                  (satisfied-capo s c) (l/nafc minted-by-completeo s)
                  (l/== q {:step s :cap c :violation :satisfied-without-complete-producer})))))

(defn q-applicability [db]
  ;; INV-3: an :advance of a mission whose :requires are not satisfied
  (pldb/with-db db
    (l/run* [q] (l/fresh [s m]
                  (actiono s :advance) (missiono s m) (l/nafc requires-sato s)
                  (l/== q {:step s :mission m :violation :advance-inapplicable-mission})))))

(defn q-buck [db]
  ;; INV-G: a :pursue of a NOT-pre-registered capability WITHOUT consent.
  ;; (discovery is free; the gate is at the pursuit boundary.)
  (pldb/with-db db
    (l/run* [q] (l/fresh [s c]
                  (actiono s :pursue) (capabilityo s c)
                  (l/nafc pre-registeredo s) (l/nafc consento s)
                  (l/== q {:step s :cap c :violation :pursue-unregistered-without-consent})))))

(defn q-decompose [db]
  ;; decompose line: a GOAL-EXTENDING decompose (big ∇-deform) without consent.
  ;; (a small path-refining decompose — :extends-goal? false — is free.)
  (pldb/with-db db
    (l/run* [q] (l/fresh [s m]
                  (actiono s :decompose) (missiono s m)
                  (extends-goalo s) (l/nafc consento s)
                  (l/== q {:step s :mission m :violation :goal-extending-decompose-without-consent})))))

(defn q-gate [db]
  ;; operator-verify gate: an :advance that crosses an operator-verify exit
  ;; without the gap agreed AND without consent (the un-agreed-gap push).
  (pldb/with-db db
    (l/run* [q] (l/fresh [s m]
                  (actiono s :advance) (missiono s m)
                  (crosses-exito s) (l/nafc gap-agreedo s) (l/nafc consento s)
                  (l/== q {:step s :mission m :violation :advance-past-unagreed-operator-verify-exit})))))

(defn query-violations
  "All structural invariant checks over a trace. category -> violating steps."
  [trace]
  (let [db (build-db trace)]
    {:acyclic       (q-acyclic trace)
     :provenance    (q-provenance db)
     :applicability (q-applicability db)
     :buck          (q-buck db)
     :decompose     (q-decompose db)
     :gate          (q-gate db)}))

(defn violations? [v] (boolean (some (fn [[_ x]] (seq x)) v)))

;; =============================================================================
;; Fixtures — conforming witness + one adversarial trace per invariant
;; =============================================================================

(def witness-trace
  "Every move in its LEGAL form: acyclic edges; an applicable advance; an advance
   that crosses an exit WITH the gap agreed; a pre-registered pursuit; an
   un-registered pursuit WITH consent; a small (path-refining) decompose; a big
   decompose WITH consent; a satisfied cap with a complete producer."
  [{:step 1 :edge-from :cap/agency   :edge-to :cap/self-rep}
   {:step 2 :edge-from :cap/self-rep :edge-to :cap/wm}
   {:step 3 :action :advance   :mission :M/wm-guardrails :requires-sat? true}
   {:step 4 :action :advance   :mission :M/foo :requires-sat? true :crosses-exit? true :gap-agreed? true}
   {:step 5 :action :pursue    :capability :cap/wm-overnight :pre-registered? true}
   {:step 6 :action :pursue    :capability :cap/new-thing :consent-granted? true}
   {:step 7 :action :decompose :mission :M/mega  :extends-goal? false}
   {:step 8 :action :decompose :mission :M/mega2 :extends-goal? true :consent-granted? true}
   {:step 9 :satisfied-cap :cap/wm :minted-by-complete? true}])

(def adversarial-traces
  "One trace per invariant, each planted to break exactly that invariant."
  {:acyclic       [{:step 1 :edge-from :cap/a :edge-to :cap/b}
                   {:step 2 :edge-from :cap/b :edge-to :cap/a}]            ; 2-cycle ⇒ no toposort
   :provenance    [{:step 1 :satisfied-cap :cap/x :minted-by-complete? false}] ; satisfied, no complete producer
   :applicability [{:step 1 :action :advance :mission :M/y :requires-sat? false}] ; advance inapplicable
   :buck          [{:step 1 :action :pursue :capability :cap/pentagon}]    ; pursue un-registered, no consent
   :decompose     [{:step 1 :action :decompose :mission :M/z :extends-goal? true}] ; big decompose, no consent
   :gate          [{:step 1 :action :advance :mission :M/w :requires-sat? true :crosses-exit? true}]}) ; past un-agreed exit

(defn run-verify
  "VERIFY entrypoint: witness must be clean; each adversarial trace must be
   caught BY ITS OWN category. :verified? is the gate."
  []
  (let [witness-v      (query-violations witness-trace)
        witness-clean? (not (violations? witness-v))
        adversarial
        (into {}
              (map (fn [[cat trace]]
                     (let [v (query-violations trace)]
                       [cat {:caught? (boolean (seq (get v cat))) :hits (get v cat)}]))
                   adversarial-traces))
        all-caught? (every? :caught? (vals adversarial))]
    {:verified? (and witness-clean? all-caught?)
     :witness {:clean? witness-clean? :violations witness-v}
     :adversarial adversarial
     :summary {:witness-clean? witness-clean?
               :all-adversarial-caught? all-caught?
               :n-invariants (count adversarial-traces)}}))
