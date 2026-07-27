#!/usr/bin/env -S clojure -M
(ns wire-math-memory-patterns
  "Append the reviewed mathematics trigger-class attachments for
   M-zai-learning-loop. Safe to replay: stable review ids are verified and
   existing exact pattern/attachment projections are skipped."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-write :as memory-write])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def base-url "http://127.0.0.1:7073")
(def reviewer "joe")
(def default-review-warrant "operator draft review recorded in each memory body")
(def pudding-review-warrant
  "operator-delegated: in-use evaluation (proof-in-pudding, Joe 2026-07-26)")
(def recall-system :v1-enriched)

(def pattern-descriptions
  {"math/rewrite-orientation"
   "rw rewrite fails, motive is not type correct, equation direction, flip arrow, orient rewrite toward goal, hypothesis versus goal direction"
   "math/binder-expression-mismatch"
   "integral binder mismatch, bound variable differs, show to restate goal, convert tactic, definitional equality under binder"
   "math/minmax-normalization"
   "min max unfold, absolute value cases, split_ifs, le_min max_le, linarith after unfolding"
   "math/series-evaluation-api"
   "tsum series evaluation, zeta Basel, index shift, hasSum summable, geometric series, uniform convergence M-test, power series continuity"
   "math/derivative-bounds-api"
   "derivative bound, mean value theorem, Lipschitz from derivative, sup norm L2 derivative, Cauchy-Schwarz integral"
   "math/missing-dependency-protocol"
   "Mathlib lemma not found, missing theorem after search, API gap, bounded search budget, local lemma construction, dependency frontier"
   "math/construction-before-estimates"
   "define auxiliary object first, piecewise construction, explicit witness function, build before bounding"
   "math/proof-architecture"
   "overall proof plan, multi-layer architecture, reduction to lemmas, dyadic decomposition, differentiation theorem, convergence in measure, lacunary boundary"
   "math/uniform-continuity-boundedness"
   "uniform continuity, uniformly continuous, totally bounded interval, bounded image, difference quotient, epsilon delta, TotallyBounded.image, isUniformEmbedding_subtype_val"
   "math/weak-convergence-hilbert"
   "weak convergence Hilbert space, Kadec Klee, norm convergence implies strong convergence, norm_sub_sq_real, continuous sqrt, liminf side conditions"
   "math/complex-liouville-periodicity"
   "entire complex function, holomorphic, bounded entire Liouville, doubly periodic, lattice invariant, compact fundamental parallelogram, exists_const_forall_eq_of_bounded"
   "math/cast-normalization"
   "cast coercion normalization, integer to complex cast, real imaginary projection, Complex.ofReal_intCast, floor fract, simplify re im"
   "math/corpus-trust-protocol"
   "stale blocked assessment, informal solution error, verify corpus against current library, partial trust, reasoned memory non-use, subsumed recalled pattern"})

(def attachments
  [{:memory-id "e-1ac936fb-04e8-460e-a710-37fac474401c"
    :name "symmetric-interval-law-to-dyadic-differentiation"
    :pattern-id "math/proof-architecture"}
   {:memory-id "e-4fda2517-1d61-4bef-9130-4ef3dc32205e"
    :name "orient-rewrite-to-the-goal"
    :pattern-id "math/rewrite-orientation"}
   {:memory-id "e-5736f45a-151c-46b2-8993-f8d3e578b51a"
    :name "let-lean-infer-integral-binder-equality"
    :pattern-id "math/binder-expression-mismatch"}
   {:memory-id "e-69344db6-0685-444d-85d6-8cd4a2b1773a"
    :name "normalize-min-max-before-linear-arithmetic"
    :pattern-id "math/minmax-normalization"}
   {:memory-id "e-31646454-435e-4d34-b4db-ffdfee6af193"
    :name "zeta-two-index-shift-api"
    :pattern-id "math/series-evaluation-api"}
   {:memory-id "e-cae42a27-acb6-4452-b76b-00175593b32f"
    :name "uniform-series-continuity-api"
    :pattern-id "math/series-evaluation-api"}
   {:memory-id "e-88a1af39-53d5-4ac8-a01b-04137a559619"
    :name "schwarz-derivative-bound-api"
    :pattern-id "math/derivative-bounds-api"}
   {:memory-id "e-dfea2de9-8979-4f8f-9343-caabb48487e6"
    :name "stop-research-after-repeated-young-api-miss"
    :pattern-id "math/missing-dependency-protocol"}
   {:memory-id "e-9751e537-f5b7-4c40-a857-0c0b699b93a2"
    :name "inventory-assembly-dependencies-before-polishing-leaves"
    :pattern-id "math/missing-dependency-protocol"}
   {:memory-id "e-78523ab2-4b3a-44b3-ab30-1b51de5f7069"
    :name "define-infinite-piecewise-object-before-estimates"
    :pattern-id "math/construction-before-estimates"}
   {:memory-id "e-a9fc50f9-51a3-4908-a3d3-7ad0a8b46e3a"
    :name "lacunary-series-three-layer-architecture"
    :pattern-id "math/proof-architecture"}
   {:memory-id "e-b3f51bc2-7509-4bc8-9750-3afc57cfdbec"
    :name "convergence-in-measure-through-bounded-transform"
    :pattern-id "math/proof-architecture"}

   {:memory-id "e-909b0800-f5ea-46ac-ae87-7a9f77d46d95"
    :name "bridge-strict-and-closed-convergence-in-measure-thresholds"
    :pattern-id "math/proof-architecture"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-4adf0546-2bed-4f35-93f1-032cd254177f"
    :name "tendsto-in-measure-ae-subsequence-api"
    :pattern-id "math/proof-architecture"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-a86f234d-51cf-444f-8ac8-e0ce8913027a"
    :name "verify-stale-blocked-labels-against-current-library"
    :pattern-id "math/corpus-trust-protocol"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-16654eba-f541-47a5-ae79-3ff08cd2e600"
    :name "record-subsumed-pattern-use-as-challenge"
    :pattern-id "math/corpus-trust-protocol"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-46c09295-d8c9-4d1c-b8e9-a7c3c1767b3c"
    :name "bound-uniformly-continuous-image-before-controlling-quotients"
    :pattern-id "math/uniform-continuity-boundedness"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-3c62f164-174b-4d75-b5bd-67f1bbbbfb5d"
    :name "uniformly-continuous-image-boundedness-api-chain"
    :pattern-id "math/uniform-continuity-boundedness"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-7d94a954-2ccf-4832-9c0e-76afb7c119bf"
    :name "correct-a93a01-direct-uniform-continuity-quotient-argument"
    :pattern-id "math/corpus-trust-protocol"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-afb1eae8-284f-4551-8c49-d0babd83a780"
    :name "kadec-klee-via-norm-sub-square-and-sqrt"
    :pattern-id "math/weak-convergence-hilbert"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-ff95cfd3-c645-4b64-afa1-46605015bb00"
    :name "norm-sub-square-to-norm-limit-api-chain"
    :pattern-id "math/weak-convergence-hilbert"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-30e87097-f843-4341-81c0-a49ee7ce0ef4"
    :name "liminf-le-of-le-zero-lower-bound-side-condition"
    :pattern-id "math/weak-convergence-hilbert"
    :review-id
    "e-review-math-pudding-v2-liminf-le-of-le-zero-lower-bound-scope-correction"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-e5d809a6-3bfc-468a-888a-16e651bfe468"
    :name "use-informal-proof-architecture-with-leaf-level-verification"
    :pattern-id "math/corpus-trust-protocol"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-f7982156-16dc-4863-8504-0f831999a55d"
    :name "conditionally-convergent-series-use-ordered-partial-sums"
    :pattern-id "math/series-evaluation-api"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-95c49e49-5982-4c66-abc0-7bba11a5be35"
    :name "leibniz-formula-template-for-abel-boundary-limits"
    :pattern-id "math/series-evaluation-api"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-ca13d43b-128b-4ca3-a1c9-89ab32584ef6"
    :name "translate-interval-integrals-with-integral-comp-add-right"
    :pattern-id "math/series-evaluation-api"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-ab5afad3-4c0b-4718-9d9b-a3bc5290cd08"
    :name "record-reasoned-non-use-of-surfaced-memories"
    :pattern-id "math/corpus-trust-protocol"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-7b39a396-5b35-4b84-a12f-9032c438c804"
    :name "reduce-lattice-periodic-entire-functions-to-compact-domain"
    :pattern-id "math/complex-liouville-periodicity"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-65b94d16-3cdd-4850-970a-62974766637a"
    :name "liouville-bounded-entire-constant-api"
    :pattern-id "math/complex-liouville-periodicity"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-6b5dd8cb-1f6d-403b-af5b-f0953ef2a7d1"
    :name "normalize-integer-complex-casts-before-projections"
    :pattern-id "math/cast-normalization"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-bb16ffa8-e6da-4956-81c3-011e312d5302"
    :name "young-l2-holder-fubini-translation-api-map"
    :pattern-id "math/missing-dependency-protocol"
    :review-id
    "e-review-math-pudding-v2-young-l2-holder-fubini-translation-api-map-s1b"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-e7805bcc-6d9f-492d-9bd5-b12d0fbb9dde"
    :name "fatou-lp-ae-limit-api-chain"
    :pattern-id "math/weak-convergence-hilbert"
    :review-warrant pudding-review-warrant}
   {:memory-id "e-0e4e32fe-54af-451b-b302-17aa521891fd"
    :name "le-liminf-is-cobounded-under-unbounded-above-gap"
    :pattern-id "math/weak-convergence-hilbert"
    :review-warrant pudding-review-warrant}])

(def pattern-ids
  (->> attachments (map :pattern-id) distinct sort vec))

(defn- encode [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn- parse-body [response]
  (when-let [body (:body response)]
    (edn/read-string {:default (fn [_tag value] value)} body)))

(defn- get-entity [pattern-id]
  (let [response
        (http/get (str base-url "/api/alpha/entity/" (encode pattern-id))
                  {:headers {"accept" "application/edn"}
                   :timeout 60000
                   :throw false})]
    (case (:status response)
      200 (:entity (parse-body response))
      404 nil
      (throw (ex-info "entity read failed"
                      {:pattern-id pattern-id :response response})))))

(defn- post-edn! [path payload]
  (let [response
        (http/post (str base-url path)
                   {:headers {"accept" "application/edn"
                              "content-type" "application/edn"
                              "x-penholder" "api"}
                    :body (pr-str payload)
                    :timeout 60000
                    :throw false})
        body (parse-body response)]
    (when-not (<= 200 (:status response) 299)
      (throw (ex-info "substrate write failed"
                      {:path path :status (:status response) :body body})))
    body))

(defn- pattern-payload [pattern-id entity]
  {:id pattern-id
   :name pattern-id
   :type :pattern/library
   :external-id pattern-id
   :source (or (:source entity)
               "M-zai-learning-loop trigger-class vocabulary")
   ;; The entity API replaces the document at a stable id. Merge the new
   ;; description into the read-back props so unrelated fields survive.
   :props (merge (:props entity)
                 {:domain :mathematics
                  :pattern/kind :trigger-class
                  :description (get pattern-descriptions pattern-id)
                  :recall-system recall-system})})

(defn- pattern-described? [pattern-id entity]
  (and (= pattern-id (:id entity))
       (= pattern-id (:name entity))
       (= :pattern/library (:type entity))
       (= pattern-id (:external-id entity))
       (= (get pattern-descriptions pattern-id)
          (get-in entity [:props :description]))
       (= recall-system (get-in entity [:props :recall-system]))))

(defn- ensure-pattern! [pattern-id]
  (let [before (get-entity pattern-id)]
    (when (and before
               (not (and (= pattern-id (:id before))
                         (= pattern-id (:name before))
                         (= :pattern/library (:type before))
                         (= pattern-id (:external-id before)))))
      (throw (ex-info "pattern id is already occupied by a different entity"
                      {:pattern-id pattern-id :entity before})))
    (if (pattern-described? pattern-id before)
      :existing
      (let [payload (pattern-payload pattern-id before)
            _ (post-edn! "/api/alpha/entity" payload)
            after (get-entity pattern-id)
            unrelated-before
            (apply dissoc (:props before) [:description :recall-system])
            unrelated-after
            (apply dissoc (:props after) [:description :recall-system])]
        (when-not (pattern-described? pattern-id after)
          (throw (ex-info "pattern description was not visible after append"
                          {:pattern-id pattern-id :entity after})))
        (when (and before (not= unrelated-before unrelated-after))
          (throw (ex-info "pattern description append changed unrelated props"
                          {:pattern-id pattern-id
                           :before unrelated-before
                           :after unrelated-after})))
        (if before :described :minted)))))

(defn- description-evidence-id [pattern-id]
  (str "e-pattern-description-v1-"
       (subs pattern-id (count "math/"))))

(defn- description-entry [pattern-id]
  {:evidence/id (description-evidence-id pattern-id)
   :evidence/subject {:ref/type :pattern :ref/id pattern-id}
   :evidence/type :reflection
   :evidence/claim-type :observation
   :evidence/author "ground-control"
   :evidence/session-id "M-zai-learning-loop/recall-v1-enriched"
   :evidence/at (str (Instant/now))
   :evidence/body
   {:event :pattern-description
    :pattern-id pattern-id
    :description (get pattern-descriptions pattern-id)
    :recall-system recall-system}
   :evidence/tags [:memory :pattern-description :mathematics]})

(defn- ensure-description-evidence! [evidence-store pattern-id]
  (let [expected (description-entry pattern-id)
        evidence-id (:evidence/id expected)
        existing (estore/get-entry* evidence-store evidence-id)]
    (if existing
      (do
        (when-not (= (dissoc expected :evidence/at)
                     (dissoc existing :evidence/at))
          (throw (ex-info "pattern description evidence has different content"
                          {:id evidence-id :existing existing})))
        :existing)
      (let [receipt (boundary/append! evidence-store expected)
            visible (estore/get-entry* evidence-store evidence-id)]
        (when-not (:ok receipt)
          (throw (ex-info "pattern description evidence append failed"
                          {:id evidence-id :receipt receipt})))
        (when-not (= (dissoc expected :evidence/at)
                     (dissoc visible :evidence/at))
          (throw (ex-info "pattern description evidence not visible after append"
                          {:id evidence-id :visible visible})))
        :appended))))

(defn- fetch-hyperedges [endpoint {:keys [type limit]
                                   :or {limit 20}}]
  (let [response
        (http/get
         (str base-url "/api/alpha/hyperedges?end=" (encode endpoint)
              (when type (str "&type=" (encode (subs (str type) 1))))
              "&limit=" limit
              "&include-total=false")
         {:headers {"accept" "application/edn"}
          :timeout 60000
          :throw false})]
    (when-not (= 200 (:status response))
      (throw (ex-info "hyperedge read failed"
                      {:endpoint endpoint :response response})))
    (:hyperedges (parse-body response))))

(defn- memory-edge [memory-id]
  (->> (fetch-hyperedges memory-id {:type :memory/assert :limit 20})
       (filter #(= memory-id (get-in % [:hx/props :roles :entry])))
       first))

(defn- exact-projection-visible? [expected]
  (let [actual (memory-edge (get-in expected [:hx/props :roles :entry]))]
    (= (:hx/props expected) (:hx/props actual))))

(defn- post-hyperedge-verified! [ctx edge]
  (let [result (memory-write/post-hyperedge! ctx edge)]
    (if (:ok result)
      result
      (if (exact-projection-visible? edge)
        {:ok true :hyperedge edge :verified-after-ambiguous-response? true}
        result))))

(defn- attach-pattern! [ctx {:keys [memory-id pattern-id]}]
  (let [edge (or (memory-edge memory-id)
                 (throw (ex-info "memory/assert edge missing"
                                 {:memory-id memory-id})))
        existing-patterns
        (vec (or (get-in edge [:hx/props :roles :patterns]) []))]
    (cond
      (= [pattern-id] existing-patterns)
      :existing

      (seq existing-patterns)
      (throw (ex-info "memory already has a different pattern attachment"
                      {:memory-id memory-id
                       :expected pattern-id
                       :actual existing-patterns}))

      :else
      (let [updated
            (-> edge
                (update :hx/endpoints
                        #(vec (distinct (conj (vec %) pattern-id))))
                (update-in [:hx/props :roles :subjects]
                           #(vec (distinct (conj (vec %) pattern-id))))
                (assoc-in [:hx/props :roles :patterns] [pattern-id])
                (assoc-in [:hx/props :attachment-status] :proposed))]
        (when-not (:ok (post-hyperedge-verified! ctx updated))
          (throw (ex-info "pattern attachment projection failed"
                          {:memory-id memory-id :pattern-id pattern-id})))
        :attached))))

(defn- review-id [{:keys [name review-id review-warrant]}]
  (or review-id
      (str "e-review-math-"
           (when review-warrant "pudding-v2-")
           name)))

(defn- review-entry
  [{:keys [memory-id pattern-id review-warrant] :as attachment}]
  {:evidence/id (review-id attachment)
   :evidence/subject {:ref/type :memory :ref/id memory-id}
   :evidence/type :memory
   :evidence/claim-type :observation
   :evidence/author reviewer
   :evidence/session-id "M-zai-learning-loop/operator-review"
   :evidence/at (str (Instant/now))
   :evidence/body
   (cond->
    {:review/event :memory-attachment-review
     :review/memory-id memory-id
     :review/pattern-ids [pattern-id]
     :review/verdict :approve
     :review/witness-status (if review-warrant
                              :self-asserted
                              :independently-witnessed)
     :review/provenance
     {:kind (if review-warrant
              :operator-delegated-in-use-evaluation
              :operator-draft-review)
      :reviewer reviewer
      :date "2026-07-26"
      :warrant (or review-warrant default-review-warrant)}}
     review-warrant (assoc :review/policy-verdict :pudding-delegated))
   :evidence/tags [:memory :memory/review :mathematics]})

(defn- ensure-review-evidence! [evidence-store attachment]
  (let [expected (review-entry attachment)
        existing (estore/get-entry* evidence-store (:evidence/id expected))]
    (if existing
      (do
        (when-not (= (dissoc expected :evidence/at)
                     (dissoc existing :evidence/at))
          (throw (ex-info "review evidence id has different content"
                          {:id (:evidence/id expected)})))
        :existing)
      (let [receipt (boundary/append! evidence-store expected)]
        (when-not (:ok receipt)
          (throw (ex-info "review evidence append failed"
                          {:id (:evidence/id expected)
                           :receipt receipt})))
        :appended))))

(defn- review-attachment! [ctx evidence-store attachment]
  (let [{:keys [memory-id pattern-id]} attachment
        review-evidence-id (review-id attachment)
        edge (memory-edge memory-id)]
    (if (and (= :reviewed (get-in edge [:hx/props :attachment-status]))
             (= review-evidence-id
                (get-in edge [:hx/props :review :evidence-id])))
      :existing
      (let [result
            (lifecycle/review-attachment!
             ctx
             {:memory-id memory-id
              :review-evidence-id review-evidence-id
              :verdict :approve
              :pattern-ids [pattern-id]}
             {:fetch-hyperedges fetch-hyperedges
              :fetch-entry #(estore/get-entry* evidence-store %)
              :post-hyperedge post-hyperedge-verified!})]
        (when-not (:ok result)
          (throw (ex-info "attachment review failed"
                          {:attachment attachment :result result})))
        :reviewed))))

(defn- validate-memory! [evidence-store {:keys [memory-id name]}]
  (let [entry (estore/get-entry* evidence-store memory-id)]
    (when-not (and (= name (get-in entry [:evidence/body :name]))
                   (= "codex-2" (:evidence/author entry))
                   (= :memory (:evidence/type entry))
                   (= :assert (:evidence/claim-type entry)))
      (throw (ex-info "memory mapping does not match the promoted entry"
                      {:memory-id memory-id :expected-name name :entry entry})))))

(defn -main [& _args]
  (let [evidence-store (f1b/make-futon1b-backend base-url)
        ctx {:agent-id reviewer
             :session-id "M-zai-learning-loop/operator-review"
             :domain :mathematics
             :evidence-store evidence-store}
        pattern-results
        (into {}
              (map
               (fn [id]
                 [id {:entity (ensure-pattern! id)
                      :fts-row (ensure-description-evidence! evidence-store id)}])
               pattern-ids))
        results
        (mapv
         (fn [attachment]
           (validate-memory! evidence-store attachment)
           (let [attach-result (attach-pattern! ctx attachment)
                 evidence-result
                 (ensure-review-evidence! evidence-store attachment)
                 review-result
                 (review-attachment! ctx evidence-store attachment)]
             (assoc attachment
                    :attach attach-result
                    :review-evidence evidence-result
                    :review review-result)))
         attachments)]
    (prn {:ok true
          :patterns pattern-results
          :attachments results
          :pattern-count (count pattern-results)
          :attachment-count (count results)
          :reviewed-count
          (count (filter #(contains? #{:reviewed :existing} (:review %))
                         results))})))

(apply -main *command-line-args*)
