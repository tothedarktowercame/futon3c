(ns futon3c.logic.outing-invariants
  "VERIFY model for M-war-machine-first-outing — the logic model checked
   BEFORE the harness is written (operator directive 2026-05-30: 'check the
   logic model before we write the code').

   Follows the house idiom (agency/logic.clj, portfolio/logic.clj):
   snapshot -> build-db -> goals -> query-violations, over clojure.core.logic
   + pldb. The 'snapshot' here is an ABSTRACT cycle-trace: a run is a vector of
   cycle-records, so adversarial traces can be enumerated and the design's
   guarantees checked without the live harness.

   A run is VERIFIED iff:
     (a) the witnessed conforming trace yields ZERO violations, and
     (b) each adversarial trace (one per invariant) is CAUGHT.
   Same self-validating shape repl_spec_verify uses (CONFORMS witness +
   synthetic-failures fixture).

   Invariants encoded (mission §10.2; wiring :invariants + R-I allowlist):
     I-1               no second agent appears in a cycle
     earned-discharge  no commit without G1 (conformance) and G2 (regression-ok),
                       and G3 (top-shift) when a discharge is claimed
     gate-at-merge     no write to master without an operator merge
     ledger-complete   no claimed discharge without a commit
     no-flood          registry grows only by earned discharge (promotion OFF)
     no-teleport (G3)  no claimed discharge that commits with top-shift false
     allowlist (R-I)   no autonomous step in a non-allowlisted action class
     agent-present(T3) no cycle runs without an agent present (refuses to certify
                       the agent-absent / overnight mode — unverified until R-N)

   This model is OFFLINE-ONLY for VERIFY; per mission §10.5 it may later register
   as live probe families (like :sorry-closures-stick) during INSTANTIATE."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; =============================================================================
;; Relations (fact schema over a cycle-trace)
;; =============================================================================

(pldb/db-rel cycleo            cid)
(pldb/db-rel claimed-dischargeo cid)            ; cycle flipped a sorry -> :addressed
(pldb/db-rel g1o               cid)             ; G1 conformance (verifier CONFORMS)
(pldb/db-rel g2o               cid)             ; G2 regression-ok (no resolved-but-open)
(pldb/db-rel top-shifto        cid)             ; field moved (G3 signal)
(pldb/db-rel committedo        cid target)      ; target: :branch | :master
(pldb/db-rel operator-mergeo   cid)             ; an operator merge happened
(pldb/db-rel action-classo     cid cls)         ; :address-sorry | :open-mission | :niche-deform
(pldb/db-rel autonomouso       cid)             ; executed autonomously (not escalated)
(pldb/db-rel registry-grew-byo cid by)          ; :earned-discharge | :auto-promotion
(pldb/db-rel agent-presento    cid)             ; an agent was present this cycle
(pldb/db-rel extra-agento      cid aid)         ; a second agent appeared (I-1 breach)

;; Allowlisted action classes for autonomous execution (R-I).
(def ^:private allowlisted-classes #{:address-sorry})

;; =============================================================================
;; Database construction — a trace is a vector of cycle-records
;; =============================================================================
;;
;; cycle-record keys (all optional except :cycle):
;;   :cycle int  :claimed-discharge? :g1? :g2? :top-shift? :autonomous?
;;   :agent-present?  :committed-to (:branch|:master|nil)  :operator-merge?
;;   :action-class kw  :registry-grew-by kw  :extra-agent (aid or nil)
;; =============================================================================

(defn build-db
  "Assert a cycle-trace into a pldb fact database."
  [trace]
  (reduce
   (fn [db {:keys [cycle claimed-discharge? g1? g2? top-shift? autonomous?
                   agent-present? committed-to operator-merge? action-class
                   registry-grew-by extra-agent]}]
     (cond-> (pldb/db-fact db cycleo cycle)
       claimed-discharge?  (pldb/db-fact claimed-dischargeo cycle)
       g1?                 (pldb/db-fact g1o cycle)
       g2?                 (pldb/db-fact g2o cycle)
       top-shift?          (pldb/db-fact top-shifto cycle)
       autonomous?         (pldb/db-fact autonomouso cycle)
       agent-present?      (pldb/db-fact agent-presento cycle)
       committed-to        (pldb/db-fact committedo cycle committed-to)
       operator-merge?     (pldb/db-fact operator-mergeo cycle)
       action-class        (pldb/db-fact action-classo cycle action-class)
       registry-grew-by    (pldb/db-fact registry-grew-byo cycle registry-grew-by)
       extra-agent         (pldb/db-fact extra-agento cycle extra-agent)))
   pldb/empty-db
   trace))

;; helper: "cycle c committed to some target"
(defn- committed-anyo [c]
  (l/fresh [t] (committedo c t)))

;; =============================================================================
;; Queries — detect violations (empty vector = invariant holds)
;; =============================================================================

(defn q-i1-extra-agents [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [c aid]
        (extra-agento c aid)
        (l/== q {:cycle c :extra-agent aid})))))

(defn q-earned-discharge [db]
  ;; A commit missing a required gate. Three sub-queries unioned (mirrors
  ;; agency/logic.clj route-inconsistencies). G3 only required when a discharge
  ;; is claimed.
  (let [commit-no-g1
        (pldb/with-db db
          (l/run* [q] (l/fresh [c t]
                        (committedo c t) (l/nafc g1o c)
                        (l/== q {:cycle c :missing :g1-conformance}))))
        commit-no-g2
        (pldb/with-db db
          (l/run* [q] (l/fresh [c t]
                        (committedo c t) (l/nafc g2o c)
                        (l/== q {:cycle c :missing :g2-regression}))))
        commit-claimed-no-g3
        (pldb/with-db db
          (l/run* [q] (l/fresh [c t]
                        (committedo c t) (claimed-dischargeo c) (l/nafc top-shifto c)
                        (l/== q {:cycle c :missing :g3-top-shift}))))]
    (vec (concat commit-no-g1 commit-no-g2 commit-claimed-no-g3))))

(defn q-gate-at-merge [db]
  ;; wrote to master without an operator merge
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [c]
        (committedo c :master) (l/nafc operator-mergeo c)
        (l/== q {:cycle c :violation :master-without-operator-merge})))))

(defn q-ledger-complete [db]
  ;; claimed a discharge but never committed it
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [c]
        (claimed-dischargeo c) (l/nafc committed-anyo c)
        (l/== q {:cycle c :violation :discharge-without-commit})))))

(defn q-no-flood [db]
  ;; registry grew by anything other than an earned discharge
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [c by]
        (registry-grew-byo c by)
        (l/!= by :earned-discharge)
        (l/== q {:cycle c :grew-by by})))))

(defn q-no-teleport [db]
  ;; claimed a discharge, committed, but the field did not move (fake-finish)
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [c t]
        (claimed-dischargeo c) (committedo c t) (l/nafc top-shifto c)
        (l/== q {:cycle c :violation :fake-finish})))))

(defn q-allowlist [db]
  ;; autonomous execution of a non-allowlisted action class (R-I)
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [c cls]
        (autonomouso c) (action-classo c cls)
        (l/project [cls]
          (if (allowlisted-classes cls) l/fail l/succeed))
        (l/== q {:cycle c :autonomous-class cls})))))

(defn q-agent-present [db]
  ;; a cycle ran without an agent present (the unverified agent-absent mode, T3)
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [c]
        (cycleo c) (l/nafc agent-presento c)
        (l/== q {:cycle c :violation :agent-absent-unverified})))))

(defn query-violations
  "All structural invariant checks. category -> violating cycles (empty = holds)."
  [db]
  {:i1-extra-agents     (q-i1-extra-agents db)
   :earned-discharge    (q-earned-discharge db)
   :gate-at-merge       (q-gate-at-merge db)
   :ledger-complete     (q-ledger-complete db)
   :no-flood            (q-no-flood db)
   :no-teleport         (q-no-teleport db)
   :allowlist           (q-allowlist db)
   :agent-present       (q-agent-present db)})

(defn violations? [violations]
  (some (fn [[_ v]] (seq v)) violations))

;; =============================================================================
;; Halt-rate verdict (central-tension hook, §10.3) — plain reduction, not logic
;; =============================================================================

(defn halt-rate-verdict
  "The central ARGUE tension made a number. not-ready := any hard-halt OR
   >50% soft-halts. `halts` is the run-level halt log: [{:cycle :kind}]."
  [trace halts]
  (let [n      (count trace)
        hard   (count (filter #(= :hard (:kind %)) halts))
        soft   (count (filter #(= :soft (:kind %)) halts))
        soft-rate (if (pos? n) (/ (double soft) n) 0.0)
        not-ready? (or (pos? hard) (> soft-rate 0.5))]
    {:cycles n :hard-halts hard :soft-halts soft :soft-rate soft-rate
     :ready? (not not-ready?)
     :rule "not-ready iff any hard-halt OR soft-rate > 0.5"}))

(defn verdict
  "Full VERIFY verdict over a trace (+ optional halt log)."
  ([trace] (verdict trace []))
  ([trace halts]
   (let [v (query-violations (build-db trace))]
     {:violations v
      :structurally-sound? (not (violations? v))
      :halt-verdict (halt-rate-verdict trace halts)})))

;; =============================================================================
;; Fixtures — witnessed conforming trace + one adversarial trace per invariant
;; =============================================================================

(def conforming-cycle
  "The abstract shape this session's two real cycles instantiate. Their
   conformance (V1-V4) and top-shift are WITNESSED (live-2d50834b,
   live-00c07332); the commit-to-branch is the harness completion the design
   adds. Earned, honest, allowlisted, agent present."
  {:cycle 1 :claimed-discharge? true :g1? true :g2? true :top-shift? true
   :autonomous? true :agent-present? true :committed-to :branch
   :action-class :address-sorry :registry-grew-by :earned-discharge})

(def witness-trace
  [conforming-cycle
   (assoc conforming-cycle :cycle 2)])

(def adversarial-traces
  "One trace per invariant, each planted to break exactly that invariant."
  {:i1-extra-agents  [(assoc conforming-cycle :extra-agent "claude-99")]
   :earned-discharge [(assoc conforming-cycle :g1? false)]           ; commit w/o conformance
   :gate-at-merge    [(assoc conforming-cycle :committed-to :master
                                              :operator-merge? false)]
   :ledger-complete  [(assoc conforming-cycle :committed-to nil)]    ; discharge, no commit
   :no-flood         [(assoc conforming-cycle :registry-grew-by :auto-promotion)]
   :no-teleport      [(assoc conforming-cycle :top-shift? false)]    ; claimed + commit, no move
   :allowlist        [(assoc conforming-cycle :action-class :open-mission)]
   :agent-present    [(assoc conforming-cycle :agent-present? false)]})

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
                       [cat {:caught? (boolean caught?)
                             :hits (get v cat)}]))
                   adversarial-traces))
        all-caught? (every? :caught? (vals adversarial))]
    {:verified? (and witness-clean? all-caught?)
     :witness {:clean? witness-clean? :violations witness-v}
     :adversarial adversarial
     :summary {:witness-clean? witness-clean?
               :all-adversarial-caught? all-caught?
               :n-invariants (count adversarial-traces)}}))
