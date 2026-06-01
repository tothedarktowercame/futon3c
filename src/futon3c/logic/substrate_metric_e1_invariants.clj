(ns futon3c.logic.substrate-metric-e1-invariants
  "VERIFY model for M-substrate-metric E1 curvature cut.

   House idiom: abstract witness trace -> build-db -> query-violations over
   clojure.core.logic + pldb. This is a design witness only: no serving JVM,
   no substrate writes, no curvature implementation.

   Verified iff the conforming witness is clean and each adversarial trace is
   caught by its own invariant."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

;; -----------------------------------------------------------------------------
;; Relations
;; -----------------------------------------------------------------------------

(pldb/db-rel nodeo             nid)
(pldb/db-rel resolvednesso     nid r)
(pldb/db-rel actionableo       nid)
(pldb/db-rel straino           nid)
(pldb/db-rel proposeo          nid)
(pldb/db-rel min-kappao        nid k)
(pldb/db-rel computed-mino     nid k)
(pldb/db-rel incident-kappao   nid edge k)
(pldb/db-rel has-negativeo     nid)
(pldb/db-rel mu-masso          nid target mass)
(pldb/db-rel mu-sumo           nid total)
(pldb/db-rel mu-negativeo      nid target mass)
(pldb/db-rel snapshoto         sid)
(pldb/db-rel curvature-sigo    sid sig)
(pldb/db-rel permutation-pairo sid-a sid-b)

;; -----------------------------------------------------------------------------
;; Database construction
;; -----------------------------------------------------------------------------

(defn- numeric? [x]
  (number? x))

(defn- nearly= [a b]
  (and (number? a)
       (number? b)
       (< (abs (- (double a) (double b))) 1.0e-9)))

(defn- node-min-kappa [node]
  (when-let [ks (seq (map :kappa (:incident-kappas node)))]
    (apply min ks)))

(defn build-db
  "Assert an abstract E1 VERIFY trace into a pldb fact database.

   Trace shape:
   {:nodes [{:id kw
             :resolvedness number|:unknown
             :actionable? boolean
             :propose? boolean
             :strain? boolean
             :min-kappa number
             :incident-kappas [{:edge kw :kappa number}]
             :mu {target mass}}]
    :snapshots [{:id kw :curvature-signature any}]
    :permutation-pairs [[sid-a sid-b]]}"
  [{:keys [nodes snapshots permutation-pairs]}]
  (let [db0
        (reduce
         (fn [db {:keys [id resolvedness actionable? propose? strain? min-kappa
                         incident-kappas mu]}]
           (let [computed-min (node-min-kappa {:incident-kappas incident-kappas})
                 has-neg? (boolean (some #(< (double (:kappa %)) 0.0) incident-kappas))
                 mu-total (reduce + 0.0 (vals mu))]
             (cond-> (pldb/db-fact db nodeo id)
               (some? resolvedness) (pldb/db-fact resolvednesso id resolvedness)
               actionable?          (pldb/db-fact actionableo id)
               propose?             (pldb/db-fact proposeo id)
               strain?              (pldb/db-fact straino id)
               (some? min-kappa)    (pldb/db-fact min-kappao id min-kappa)
               (some? computed-min) (pldb/db-fact computed-mino id computed-min)
               has-neg?             (pldb/db-fact has-negativeo id)
               (some? mu)           (pldb/db-fact mu-sumo id mu-total)
               true
               (#(reduce
                  (fn [db* {:keys [edge kappa]}]
                    (pldb/db-fact db* incident-kappao id edge kappa))
                  %
                  incident-kappas))
               true
               (#(reduce
                  (fn [db* [target mass]]
                    (cond-> (pldb/db-fact db* mu-masso id target mass)
                      (neg? (double mass)) (pldb/db-fact mu-negativeo id target mass)))
                  %
                  mu)))))
         pldb/empty-db
         nodes)]
    (-> (reduce
         (fn [db {:keys [id curvature-signature]}]
           (cond-> (pldb/db-fact db snapshoto id)
             (some? curvature-signature)
             (pldb/db-fact curvature-sigo id curvature-signature)))
         db0
         snapshots)
        (as-> db
            (reduce
             (fn [db* [a b]]
               (pldb/db-fact db* permutation-pairo a b))
             db
             permutation-pairs)))))

;; -----------------------------------------------------------------------------
;; Queries
;; -----------------------------------------------------------------------------

(defn q-stage-b-guard [db]
  ;; COMPLETE / fully resolved nodes never propose, regardless of curvature.
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [n]
        (proposeo n)
        (resolvednesso n 1.0)
        (l/== q {:node n :violation :resolved-node-proposed})))))

(defn q-conjunct-independence [db]
  ;; Permuting resolution-state must not alter curvature signatures.
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [a b sig-a sig-b]
        (permutation-pairo a b)
        (curvature-sigo a sig-a)
        (curvature-sigo b sig-b)
        (l/!= sig-a sig-b)
        (l/== q {:snapshots [a b] :violation :curvature-changed-under-resolution-permutation
                 :curvature-a sig-a :curvature-b sig-b})))))

(defn q-measure-validity [db]
  (let [negative
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n target mass]
              (mu-negativeo n target mass)
              (l/== q {:node n :target target :mass mass :violation :negative-mu-mass}))))
        bad-sum
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n total]
              (mu-sumo n total)
              (l/project [total]
                (if (nearly= total 1.0) l/fail l/succeed))
              (l/== q {:node n :sum total :violation :mu-does-not-sum-to-1}))))]
    (vec (concat negative bad-sum))))

(defn q-strain-soundness [db]
  (let [strain-without-negative
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n]
              (straino n)
              (l/nafc has-negativeo n)
              (l/== q {:node n :violation :strain-without-negative-edge}))))
        negative-without-strain
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n]
              (has-negativeo n)
              (l/nafc straino n)
              (l/== q {:node n :violation :negative-edge-without-strain}))))
        bad-min
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n declared computed]
              (min-kappao n declared)
              (computed-mino n computed)
              (l/project [declared computed]
                (if (nearly= declared computed) l/fail l/succeed))
              (l/== q {:node n :declared declared :computed computed
                       :violation :min-kappa-mismatch}))))]
    (vec (concat strain-without-negative negative-without-strain bad-min))))

(defn q-no-bypass-composition [db]
  ;; propose-here? must imply strain? AND numeric resolvedness < 1 AND actionable?
  (let [no-strain
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n]
              (proposeo n)
              (l/nafc straino n)
              (l/== q {:node n :violation :proposed-without-strain}))))
        not-actionable
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n]
              (proposeo n)
              (l/nafc actionableo n)
              (l/== q {:node n :violation :proposed-without-actionability}))))
        resolved-or-unknown
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [n r]
              (proposeo n)
              (resolvednesso n r)
              (l/project [r]
                (if (and (numeric? r) (< (double r) 1.0)) l/fail l/succeed))
              (l/== q {:node n :resolvedness r
                       :violation :proposed-without-numeric-unresolvedness}))))]
    (vec (concat no-strain not-actionable resolved-or-unknown))))

(defn q-unknown-safety [db]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [n]
        (proposeo n)
        (resolvednesso n :unknown)
        (l/== q {:node n :violation :unknown-resolution-proposed})))))

(defn query-violations [db]
  {:stage-b-guard        (q-stage-b-guard db)
   :conjunct-independence (q-conjunct-independence db)
   :measure-validity     (q-measure-validity db)
   :strain-soundness     (q-strain-soundness db)
   :no-bypass-composition (q-no-bypass-composition db)
   :unknown-safety       (q-unknown-safety db)})

(defn violations? [violations]
  (some (fn [[_ v]] (seq v)) violations))

;; -----------------------------------------------------------------------------
;; Fixtures
;; -----------------------------------------------------------------------------

(def witness-trace
  {:nodes
   [{:id :complete-bridge
     :resolvedness 1.0
     :actionable? false
     :strain? true
     :min-kappa -0.8
     :propose? false
     :incident-kappas [{:edge :e-complete-file :kappa -0.8}
                       {:edge :e-complete-mission :kappa 0.2}]
     :mu {:complete-bridge 0.5 :file-a 0.25 :mission-b 0.25}}
    {:id :open-sorry-bridge
     :resolvedness 0.0
     :actionable? true
     :strain? true
     :min-kappa -0.4
     :propose? true
     :incident-kappas [{:edge :e-sorry-mission :kappa -0.4}]
     :mu {:open-sorry-bridge 0.5 :mission-b 0.5}}
    {:id :open-flat-sorry
     :resolvedness 0.0
     :actionable? true
     :strain? false
     :min-kappa 0.1
     :propose? false
     :incident-kappas [{:edge :e-flat-mission :kappa 0.1}]
     :mu {:open-flat-sorry 0.5 :mission-c 0.5}}
    {:id :unknown-pattern
     :resolvedness :unknown
     :actionable? false
     :strain? true
     :min-kappa -0.2
     :propose? false
     :incident-kappas [{:edge :e-pattern-mission :kappa -0.2}]
     :mu {:unknown-pattern 1.0}}
    {:id :isolated-file
     :resolvedness :unknown
     :actionable? false
     :strain? false
     :min-kappa nil
     :propose? false
     :incident-kappas []
     :mu {:isolated-file 1.0}}]
   :snapshots [{:id :orig :curvature-signature {:complete -0.8 :sorry -0.4}}
               {:id :permuted-resolution :curvature-signature {:complete -0.8 :sorry -0.4}}]
   :permutation-pairs [[:orig :permuted-resolution]]})

(def adversarial-traces
  {:stage-b-guard
   (assoc-in witness-trace [:nodes 0 :propose?] true)

   :conjunct-independence
   (assoc-in witness-trace [:snapshots 1 :curvature-signature] {:complete -0.1 :sorry -0.4})

   :measure-validity
   (assoc-in witness-trace [:nodes 1 :mu] {:open-sorry-bridge 0.5 :mission-b 0.4})

   :strain-soundness
   (assoc-in witness-trace [:nodes 1 :strain?] false)

   :no-bypass-composition
   (assoc-in witness-trace [:nodes 2 :propose?] true)

   :unknown-safety
   (assoc-in witness-trace [:nodes 3 :propose?] true)})

(defn run-verify []
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
