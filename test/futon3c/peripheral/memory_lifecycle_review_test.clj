(ns futon3c.peripheral.memory-lifecycle-review-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-recall :as recall]))

(def memory-id "e-memory")
(def review-id "e-review")
(def pattern-id "p4ng/R9-independent-witness")

(def memory-entry
  {:evidence/id memory-id
   :evidence/subject {:ref/type :mission :ref/id "mission/live-verify"}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author "codex-4"
   :evidence/session-id "live-memory-selection-verify"
   :evidence/at "2026-07-24T10:00:00Z"
   :evidence/body {:name "R9 live witness"
                   :hook "Require an independently checked trace."
                   :kind :observation
                   :body "A full bootstrap episode."}})

(defn review-entry
  ([id verdict]
   (review-entry id verdict "claude-4" [pattern-id]))
  ([id verdict reviewer patterns]
   (review-entry id verdict reviewer patterns nil))
  ([id verdict reviewer patterns memory-use-kind]
   {:evidence/id id
    :evidence/subject {:ref/type :memory :ref/id memory-id}
    :evidence/type :memory
    :evidence/claim-type :observation
    :evidence/author reviewer
    :evidence/at "2026-07-24T10:05:00Z"
    :evidence/body
    (cond->
     {:review/event :memory-attachment-review
      :review/memory-id memory-id
      :review/pattern-ids patterns
      :review/verdict verdict
      :review/witness-status :independently-witnessed
      :review/provenance
      {:kind :independent-check
       :evidence ["test output" "trace artifact"]}}
      (some? memory-use-kind)
      (assoc :memory-use/kind memory-use-kind))}))

(def proposed-edge
  {:hx/id "hx-memory"
   :hx/type :memory/assert
   :hx/endpoints [memory-id "mission/live-verify" pattern-id]
   :hx/props
   {:domain :war-machine
    :state :current
    :attachment-status :proposed
    :witness-status :self-asserted
    :roles {:entry memory-id
            :subjects ["mission/live-verify" pattern-id]
            :patterns [pattern-id]
            :mission "mission/live-verify"}}})

(def ctx
  {:domain :war-machine
   :agent-id "codex-4"
   :session-id "live-memory-selection-verify"})

(defn graph-fixture
  ([edge] (graph-fixture edge false))
  ([edge stale-after-post?]
   (let [current (atom edge)
         posts (atom [])
         posted? (atom false)]
     {:posts posts
      :fetch
      (fn [endpoint {:keys [type limit]}]
        (let [visible (if (and stale-after-post? @posted?) edge @current)]
          (if (and (= type (:hx/type visible))
                   (some #{endpoint} (:hx/endpoints visible)))
            (vec (take limit [visible]))
            [])))
      :post
      (fn [_ updated]
        (reset! posted? true)
        (reset! current updated)
        (swap! posts conj updated)
        {:ok true :hyperedge updated})})))

(defn opts
  [graph entries]
  {:fetch-hyperedges (:fetch graph)
   :fetch-entry #(get entries %)
   :post-hyperedge (:post graph)})

(def request
  {:memory-id memory-id
   :review-evidence-id review-id
   :verdict :approve
   :pattern-ids [pattern-id]})

(deftest approval-requires-independent-exact-review-evidence
  (testing "memory author cannot review their own attachment"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot review"
           (lifecycle/review-attachment!
            ctx request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve
                                                "codex-4" [pattern-id])}))))))
  (testing "review evidence must exist"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"not found"
           (lifecycle/review-attachment!
            ctx request (opts graph {memory-id memory-entry}))))))
  (testing "review must name the exact attachment"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"exact attachment"
           (lifecycle/review-attachment!
            ctx request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve
                                                "claude-4" ["p4ng/R6"])})))))))

(deftest review-refuses-stale-memory-and-wrong-domain
  (testing "superseded memory"
    (let [edge (assoc-in proposed-edge [:hx/props :state] :superseded)
          graph (graph-fixture edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"only a current"
           (lifecycle/review-attachment!
            ctx request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve)}))))))
  (testing "retracted memory has no current edge to approve"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"edge not found"
         (lifecycle/review-attachment!
          ctx request
          {:fetch-hyperedges (fn [_ _] [])
           :fetch-entry (constantly nil)
           :post-hyperedge (fn [_ _] {:ok true})}))))
  (testing "wrong domain"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"domain mismatch"
           (lifecycle/review-attachment!
            (assoc ctx :domain :mathematics)
            request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve)})))))))

(deftest approval-is-visible-to-recall-and-exact-retry-is-idempotent
  (let [graph (graph-fixture proposed-edge)
        entries {memory-id memory-entry
                 review-id (review-entry review-id :approve)}
        first-result
        (lifecycle/review-attachment! ctx request (opts graph entries))
        replay
        (lifecycle/review-attachment! ctx request (opts graph entries))
        projection
        (recall/recall-by-endpoint
         (assoc ctx :evidence-store nil)
         pattern-id
         {:fetch-hyperedges (:fetch graph)
          :fetch-entry #(get entries %)})]
    (is (true? (:ok first-result)) first-result)
    (is (= :fresh-endpoint-read (:cache-postcondition first-result)))
    (is (= :reviewed (:attachment-status first-result)))
    (is (= :independently-witnessed (:witness-status first-result)))
    (is (true? (:ok replay)) replay)
    (is (true? (:idempotent-replay? replay)))
    (is (= 1 (count @(:posts graph))))
    (is (= [memory-id] (mapv :memory/id (:memories projection))))
    (is (= :reviewed
           (get-in (first (:memories projection))
                   [:memory/attachment-status])))))

(deftest changed-verdict-is-a-new-review-not-a-hidden-replay
  (let [graph (graph-fixture proposed-edge)
        approval (review-entry review-id :approve)
        reject-id "e-review-reject"
        rejection (review-entry reject-id :reject)
        entries {memory-id memory-entry
                 review-id approval
                 reject-id rejection}
        approved
        (lifecycle/review-attachment! ctx request (opts graph entries))
        rejected
        (lifecycle/review-attachment!
         ctx (assoc request
                    :review-evidence-id reject-id
                    :verdict :reject)
         (opts graph entries))]
    (is (true? (:ok approved)))
    (is (true? (:ok rejected)) rejected)
    (is (= :rejected (:attachment-status rejected)))
    (is (not (:idempotent-replay? rejected)))
    (is (= 2 (count @(:posts graph))))))

(deftest successful-repost-must-be-immediately-visible
  (let [graph (graph-fixture proposed-edge true)
        result
        (lifecycle/review-attachment!
         ctx request
         (opts graph {memory-id memory-entry
                      review-id (review-entry review-id :approve)}))]
    (is (false? (:ok result)))
    (is (= :cache-postcondition (:stage result)))
    (is (= :stale-after-successful-repost
           (:cache-postcondition result)))))

(deftest memory-use-kind-is-optional-reviewed-metadata
  (testing "an adjudicated kind is validated, projected, and recalled"
    (let [graph (graph-fixture proposed-edge)
          entries {memory-id memory-entry
                   review-id (review-entry review-id :approve "claude-4"
                                           [pattern-id] :regulative)}
          result (lifecycle/review-attachment! ctx request
                                               (opts graph entries))
          projection (recall/recall-by-endpoint
                      (assoc ctx :evidence-store nil) pattern-id
                      {:fetch-hyperedges (:fetch graph)
                       :fetch-entry #(get entries %)})]
      (is (true? (:ok result)))
      (is (= :regulative (:memory-use/kind result)))
      (is (= :regulative
             (:memory-use/kind (first (:memories projection)))))))
  (testing "an unadjudicated memory remains valid and unmarked"
    (let [graph (graph-fixture proposed-edge)
          entries {memory-id memory-entry
                   review-id (review-entry review-id :approve)}
          result (lifecycle/review-attachment! ctx request
                                               (opts graph entries))]
      (is (true? (:ok result)))
      (is (nil? (:memory-use/kind result)))
      (is (not (contains? (first @(:posts graph)) :memory-use/kind)))
      (is (not (contains? (get-in (first @(:posts graph)) [:hx/props])
                          :memory-use/kind)))))
  (testing "the vocabulary is closed at write time"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"invalid memory-use kind"
           (lifecycle/review-attachment!
            ctx request
            (opts graph
                  {memory-id memory-entry
                   review-id (review-entry review-id :approve "claude-4"
                                           [pattern-id] :both)})))))))
