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
(def review-warrant "operator draft review recorded in each memory body")

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
    :pattern-id "math/proof-architecture"}])

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

(defn- ensure-pattern! [pattern-id]
  (if-let [entity (get-entity pattern-id)]
    (do
      (when-not (and (= pattern-id (:id entity))
                     (= pattern-id (:name entity))
                     (= :pattern/library (:type entity))
                     (= pattern-id (:external-id entity)))
        (throw (ex-info "pattern id is already occupied by a different entity"
                        {:pattern-id pattern-id :entity entity})))
      :existing)
    (let [payload
          {:id pattern-id
           :name pattern-id
           :type :pattern/library
           :external-id pattern-id
           :source "M-zai-learning-loop trigger-class vocabulary"
           :props {:domain :mathematics
                   :pattern/kind :trigger-class}}]
      (post-edn! "/api/alpha/entity" payload)
      (when-not (get-entity pattern-id)
        (throw (ex-info "new pattern was not visible after append"
                        {:pattern-id pattern-id})))
      :minted)))

(defn- fetch-hyperedges [endpoint {:keys [type limit]
                                   :or {limit 20}}]
  (let [response
        (http/get
         (str base-url "/api/alpha/hyperedges?end=" (encode endpoint)
              (when type (str "&type=" (encode (subs (str type) 1))))
              "&limit=" limit)
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

(defn- review-id [{:keys [name]}]
  (str "e-review-math-" name))

(defn- review-entry [{:keys [memory-id pattern-id] :as attachment}]
  {:evidence/id (review-id attachment)
   :evidence/subject {:ref/type :memory :ref/id memory-id}
   :evidence/type :memory
   :evidence/claim-type :observation
   :evidence/author reviewer
   :evidence/session-id "M-zai-learning-loop/operator-review"
   :evidence/at (str (Instant/now))
   :evidence/body
   {:review/event :memory-attachment-review
    :review/memory-id memory-id
    :review/pattern-ids [pattern-id]
    :review/verdict :approve
    :review/witness-status :independently-witnessed
    :review/provenance
    {:kind :operator-draft-review
     :reviewer reviewer
     :date "2026-07-26"
     :warrant review-warrant}}
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
        (into {} (map (fn [id] [id (ensure-pattern! id)]) pattern-ids))
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
