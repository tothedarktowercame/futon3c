(ns futon3c.logic.aif2-invariants
  "VERIFY Stage-A logic model for M-aif2 (futon2/holes/M-aif2.md §5) — the
   design's structural invariants checked BEFORE any harness code, per the
   captured pattern `library/mission-coherence/logic-model-before-code`
   (origin: futon3c.logic.outing-invariants / M-war-machine-first-outing §10.6).

   House idiom (agency/logic.clj, portfolio/logic.clj, outing-invariants):
   snapshot -> build-db -> goals -> query-violations over clojure.core.logic
   + pldb. The 'snapshot' is an ABSTRACT registry-mutation trace: a run is a
   vector of step-records, so adversarial traces can be enumerated and the
   design's guarantees checked without the live aif2 code.

   A design is VERIFIED iff:
     (a) the witnessed conforming trace yields ZERO violations, and
     (b) each adversarial trace (one per invariant) is CAUGHT by its category.

   Invariants encoded (M-aif2 §3a INV-* / §5 Stage A) — the structural ones
   that are pure relations over a trace (NOT the empirical ∇T-reader residue,
   which is the Stage-B code spike, nor INV-reduction, which is a Stage-C
   golden-fixture code test):
     INV-provenance  every surfaced candidate carries :proposer-id AND :type
     INV-no-bypass   a decided candidate was both :ranked? and :admissible?
                     (tension-generated candidates never short-circuit the gate)
     INV-consent     an entry reaching :active had consent/admissibility granted
                     (a :candidate cannot install unilaterally)
     INV-uniformity  every install goes through the one general gate, never a
                     stratum-specific bypass (the meta-meta guarantee: S1 and
                     S2 entries take the SAME install path)

   OFFLINE-ONLY for VERIFY; may later register as live probe families during
   INSTANTIATE (like :sorry-closures-stick)."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations (fact schema over a registry-mutation trace)
;; =============================================================================

(pldb/db-rel stepo            sid)
(pldb/db-rel candidateo       sid)            ; step surfaced a candidate
(pldb/db-rel proposer-ido     sid pid)        ; candidate's source proposer
(pldb/db-rel typeo            sid t)          ; candidate's action-class (S2 id)
(pldb/db-rel rankedo          sid)            ; candidate entered EFE ranking
(pldb/db-rel admissibleo      sid)            ; candidate/entry passed admissibility
(pldb/db-rel decidedo         sid)            ; candidate became the decision
(pldb/db-rel installedo       sid stratum)    ; an entry reached :active at stratum
(pldb/db-rel consent-grantedo sid)            ; consent/admissibility granted for the install
(pldb/db-rel install-viao     sid path)       ; :general-gate | :stratum-bypass

;; =============================================================================
;; Database construction — a trace is a vector of step-records
;; =============================================================================
;;
;; step-record keys (all optional except :step):
;;   :step int
;;   ;; fast-loop / decision facts:
;;   :candidate? :proposer-id :type :ranked? :admissible? :decided?
;;   ;; install / boundary-redraw facts:
;;   :installed-stratum (:s1|:s2|nil) :consent-granted? :install-via (kw)
;; =============================================================================

(defn build-db
  "Assert a registry-mutation trace into a pldb fact database."
  [trace]
  (reduce
   (fn [db {:keys [step candidate? proposer-id type ranked? admissible? decided?
                   installed-stratum consent-granted? install-via]}]
     (cond-> (pldb/db-fact db stepo step)
       candidate?         (pldb/db-fact candidateo step)
       proposer-id        (pldb/db-fact proposer-ido step proposer-id)
       type               (pldb/db-fact typeo step type)
       ranked?            (pldb/db-fact rankedo step)
       admissible?        (pldb/db-fact admissibleo step)
       decided?           (pldb/db-fact decidedo step)
       installed-stratum  (pldb/db-fact installedo step installed-stratum)
       consent-granted?   (pldb/db-fact consent-grantedo step)
       install-via        (pldb/db-fact install-viao step install-via)))
   pldb/empty-db
   trace))

;; helpers: existential "has a proposer / has a type / was installed"
(defn- has-proposero [s] (l/fresh [p] (proposer-ido s p)))
(defn- has-typeo     [s] (l/fresh [t] (typeo s t)))
(defn- installed-anyo [s] (l/fresh [st] (installedo s st)))

;; =============================================================================
;; Queries — detect violations (empty vector = invariant holds)
;; =============================================================================

(defn q-provenance [db]
  ;; a surfaced candidate missing :proposer-id or missing :type
  (let [no-proposer
        (pldb/with-db db
          (l/run* [q] (l/fresh [s]
                        (candidateo s) (l/nafc has-proposero s)
                        (l/== q {:step s :missing :proposer-id}))))
        no-type
        (pldb/with-db db
          (l/run* [q] (l/fresh [s]
                        (candidateo s) (l/nafc has-typeo s)
                        (l/== q {:step s :missing :type}))))]
    (vec (concat no-proposer no-type))))

(defn q-no-bypass [db]
  ;; a decided candidate that was not ranked, or not admissible
  (let [decided-unranked
        (pldb/with-db db
          (l/run* [q] (l/fresh [s]
                        (decidedo s) (l/nafc rankedo s)
                        (l/== q {:step s :violation :decided-without-ranking}))))
        decided-inadmissible
        (pldb/with-db db
          (l/run* [q] (l/fresh [s]
                        (decidedo s) (l/nafc admissibleo s)
                        (l/== q {:step s :violation :decided-without-admissibility}))))]
    (vec (concat decided-unranked decided-inadmissible))))

(defn q-consent [db]
  ;; an entry reached :active without consent/admissibility granted
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s st]
        (installedo s st) (l/nafc consent-grantedo s)
        (l/== q {:step s :stratum st :violation :install-without-consent})))))

(defn q-uniformity [db]
  ;; an install via anything other than the one general gate (a stratum-specific
  ;; bypass) — the meta-meta guarantee that S1 and S2 share the install path
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [s path]
        (install-viao s path)
        (l/!= path :general-gate)
        (l/== q {:step s :install-via path :violation :stratum-specific-bypass})))))

(defn query-violations
  "All structural invariant checks. category -> violating steps (empty = holds)."
  [db]
  {:provenance  (q-provenance db)
   :no-bypass   (q-no-bypass db)
   :consent     (q-consent db)
   :uniformity  (q-uniformity db)})

(defn violations? [violations]
  (some (fn [[_ v]] (seq v)) violations))

;; =============================================================================
;; Fixtures — witnessed conforming trace + one adversarial trace per invariant
;; =============================================================================

(def conforming-decision
  "A fast-loop step: a candidate surfaced (with provenance), ranked, admissible,
   chosen. The tension-proposer slice instantiates this shape."
  {:step 1 :candidate? true :proposer-id :s1/tension :type :open-mission
   :ranked? true :admissible? true :decided? true})

(def conforming-install-s1
  {:step 2 :installed-stratum :s1 :consent-granted? true :install-via :general-gate})

(def conforming-install-s2
  ;; the SAME general gate at S2 — witnesses uniformity across strata
  {:step 3 :installed-stratum :s2 :consent-granted? true :install-via :general-gate})

(def witness-trace
  [conforming-decision conforming-install-s1 conforming-install-s2])

(def adversarial-traces
  "One trace per invariant, each planted to break exactly that invariant."
  {:provenance [(dissoc conforming-decision :proposer-id)]      ; candidate w/o source
   :no-bypass  [(assoc conforming-decision :ranked? false)]     ; decided, never ranked
   :consent    [(assoc conforming-install-s2 :consent-granted? false)] ; install w/o consent
   :uniformity [(assoc conforming-install-s2 :install-via :stratum-bypass)]}) ; bypassed gate

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
