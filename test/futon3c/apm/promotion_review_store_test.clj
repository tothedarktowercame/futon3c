(ns futon3c.apm.promotion-review-store-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.promotion-review-store :as sut]
            [futon3c.social.shapes :as shapes]))

(deftest returned-reassign-is-persisted-before-attachment-projection
  (let [memory-id "e-memory"
        review-id "e-review"
        old-pattern "math-formalization/old"
        new-pattern "math-formalization-CA/measure-integration-api"
        entries (atom
                 {memory-id
                  {:evidence/id memory-id
                   :evidence/subject {:ref/type :problem :ref/id "p"}
                   :evidence/type :memory :evidence/claim-type :assert
                   :evidence/author "scribe" :evidence/session-id "deposit"
                   :evidence/at "2026-08-25T00:00:00Z"
                   :evidence/body {:name "n" :hook "h" :body "b"}}})
        edges (atom
               {"hx-memory"
                {:hx/id "hx-memory" :hx/type :memory/assert
                 :hx/endpoints [memory-id "p" old-pattern]
                 :hx/props {:domain :mathematics :state :current
                            :attachment-status :proposed
                            :roles {:entry memory-id
                                    :subjects ["p" old-pattern]
                                    :patterns [old-pattern]}}}})
        fetch-edges (fn [end & _]
                      (filterv #(some #{end} (:hx/endpoints %))
                               (vals @edges)))
        review {:memory-id memory-id :review-evidence-id review-id
                :depositor "reported-wrong" :reviewer "reported-wrong"
                :verdict :reassign
                :attachment-status :reviewed :pattern-ids [new-pattern]
                :memory-use/kind :regulative
                :reason "returned reason" :residual "returned residual"}
        result
        (sut/persist!
         {:deposit {:depositor "scribe"} :reviewer "proctor"
          :review-job "review-job" :reviews [review]}
         {:evidence-store :store
          :fetch-entry #(get @entries %)
          :append-entry #(do (swap! entries assoc (:evidence/id %) %)
                             {:ok true :entry %})
          :fetch-hyperedges fetch-edges
          :post-hyperedge (fn [_ edge]
                            (swap! edges assoc (:hx/id edge) edge)
                            {:ok true :hyperedge edge})})
        persisted-review (first (:reviews result))
        canonical-review-id (:review-evidence-id persisted-review)
        evidence (get @entries canonical-review-id)
        edge (get @edges "hx-memory")]
    (is (:ok result) result)
    (is (not= review-id canonical-review-id))
    (is (= review-id (:reported-review-evidence-id persisted-review)))
    (is (= "scribe" (:depositor persisted-review)))
    (is (= "proctor" (:reviewer persisted-review)))
    (is (= "reported-wrong" (:reported-depositor persisted-review)))
    (is (= "reported-wrong" (:reported-reviewer persisted-review)))
    (is (= review-id
           (get-in evidence [:evidence/body
                             :review/reported-evidence-id])))
    (is (= :reassign (get-in evidence [:evidence/body :review/verdict])))
    (is (= "returned reason"
           (get-in evidence [:evidence/body :review/reason])))
    (is (= "returned residual"
           (get-in evidence [:evidence/body :review/residual])))
    (is (= :regulative
           (get-in evidence [:evidence/body :memory-use/kind])))
    (is (= :regulative
           (get-in edge [:hx/props :review :memory-use/kind])))
    (is (= [:memory :memory/attachment-review :apm/promotion-review]
           (:evidence/tags evidence)))
    (is (= canonical-review-id
           (get-in persisted-review
                   [:review-materialization :persistence-receipt-id])))
    (is (= (get-in persisted-review
                   [:review-materialization :content-digest])
           (get-in persisted-review
                   [:review-materialization :read-back-content-digest])))
    (is (shapes/valid? shapes/EvidenceEntry evidence))
    (is (= :reviewed (get-in edge [:hx/props :attachment-status])))
    (is (= :reassign (get-in edge [:hx/props :review :verdict])))
    (is (= [new-pattern] (get-in edge [:hx/props :roles :patterns])))
    (is (some #{new-pattern} (:hx/endpoints edge)))
    (is (not-any? #{old-pattern} (:hx/endpoints edge)))))

(deftest independent-approval-derives-witness-authority
  (let [memory-id "e-memory"
        pattern "math-formalization/pattern"
        entries (atom
                 {memory-id
                  {:evidence/id memory-id
                   :evidence/subject {:ref/type :problem :ref/id "p"}
                   :evidence/type :memory :evidence/claim-type :assert
                   :evidence/author "scribe" :evidence/session-id "deposit"
                   :evidence/at "2026-08-26T00:00:00Z"
                   :evidence/body {:name "n" :hook "h" :body "b"}}})
        edges (atom
               [{:hx/id "hx-memory" :hx/type :memory/assert
                 :hx/endpoints [memory-id "p" pattern]
                 :hx/props {:domain :mathematics :state :current
                            :attachment-status :proposed
                            :roles {:entry memory-id :subjects ["p" pattern]
                                    :patterns [pattern]}}}])
        fetch-edges (fn [end & _]
                      (filterv #(some #{end} (:hx/endpoints %)) @edges))
        result
        (sut/persist!
         {:deposit {:depositor "scribe"
                    :candidates [{:memory-id memory-id
                                  :pattern-ids [pattern]}]}
          :reviewer "proctor" :review-job "review-job"
          :reviews [{:memory-id memory-id :verdict :approve
                     :reason "fits residual" :residual "open residual"
                     :pattern-ids [pattern]
                     :attachment-status :reviewed
                     :witness-status :self-asserted}]}
         {:evidence-store :store
          :fetch-entry #(get @entries %)
          :append-entry #(do (swap! entries assoc (:evidence/id %) %)
                             {:ok true :entry %})
          :fetch-hyperedges fetch-edges
          :post-hyperedge (fn [_ edge]
                            (reset! edges [edge])
                            {:ok true :hyperedge edge})})
        review (first (:reviews result))
        review-body (:evidence/body
                     (get @entries (:review-evidence-id review)))
        edge (first @edges)]
    (is (:ok result) result)
    (is (= :independently-witnessed (:witness-status review)))
    (is (= :self-asserted (:reported-witness-status review)))
    (is (= :independently-witnessed
           (:review/witness-status review-body)))
    (is (= :self-asserted
           (:review/reported-witness-status review-body)))
    (is (= :independently-witnessed
           (get-in edge [:hx/props :witness-status])))))

(deftest absent-agent-review-id-is-derived-before-any-fetch
  (let [fetched (atom [])
        canonical (sut/canonical-review
                   "review-job"
                   {:memory-id "e-memory" :review-evidence-id nil
                    :verdict :reject :reason "not actionable"
                    :residual "open goal"
                    :pattern-ids ["math-formalization/pattern"]})]
    (is (string? (:review-evidence-id canonical)))
    (is (nil? (:reported-review-evidence-id canonical)))
    (is (not-empty (:review-evidence-id canonical)))
    ;; Identity construction is pure: nil is never presented to the store.
    (is (empty? @fetched))))

(deftest unidentifiable-review-fails-before-fetch
  (let [fetches (atom [])
        result
        (sut/persist!
         {:deposit {:depositor "scribe"} :reviewer "proctor"
          :review-job "review-job"
          :reviews [{:memory-id nil :review-evidence-id nil
                     :verdict :reject :reason "bad" :residual "r"}]}
         {:evidence-store :store
          :fetch-entry #(do (swap! fetches conj %) nil)
          :append-entry (constantly {:ok true})
          :fetch-hyperedges (constantly [])
          :post-hyperedge (fn [_ _] {:ok true})})]
    (is (= :promotion-review-identity-invalid (:error/code result)))
    (is (empty? @fetches))))

(deftest verified-delivery-bridges-immediate-query-projection-lag
  (let [memory-id "e-memory"
        pattern "math-formalization/pattern"
        memory-entry {:evidence/id memory-id
                      :evidence/subject {:ref/type :problem :ref/id "p"}
                      :evidence/type :memory :evidence/claim-type :assert
                      :evidence/author "scribe" :evidence/session-id "deposit"
                      :evidence/at "2026-08-25T00:00:00Z"
                      :evidence/body {:name "n" :hook "h" :body "b"}}
        edges (atom
               [{:hx/id "hx-memory" :hx/type :memory/assert
                 :hx/endpoints [memory-id "p" pattern]
                 :hx/props {:domain :mathematics :state :current
                            :attachment-status :proposed
                            :roles {:entry memory-id :subjects ["p" pattern]
                                    :patterns [pattern]}}}])
        result
        (sut/persist!
         {:deposit {:depositor "scribe"
                    :candidates [{:memory-id memory-id
                                  :pattern-ids [pattern]}]}
          :reviewer "proctor"
          :review-job "review-job"
          :reviews [{:memory-id memory-id :review-evidence-id nil
                     :verdict :reject :reason "not actionable"
                     :residual "open goal" :pattern-ids []}]}
         {:evidence-store :store
          ;; The durable append succeeds, while the query projection still
          ;; exposes only the pre-existing memory entry.
          :fetch-entry #(when (= memory-id %) memory-entry)
          :append-entry (fn [entry] {:ok true :entry entry})
          :fetch-hyperedges (fn [end & _]
                              (filterv #(some #{end} (:hx/endpoints %)) @edges))
          :post-hyperedge (fn [_ edge]
                            (reset! edges [edge])
                            {:ok true :hyperedge edge})})]
    (is (:ok result) result)
    (is (= :reject (get-in @edges [0 :hx/props :review :verdict])))))

(deftest successful-hyperedge-post-with-stale-readback-retries-idempotently
  (let [memory-id "e-memory"
        pattern "math-formalization/pattern"
        memory-entry {:evidence/id memory-id
                      :evidence/subject {:ref/type :problem :ref/id "p"}
                      :evidence/type :memory :evidence/claim-type :assert
                      :evidence/author "scribe" :evidence/session-id "deposit"
                      :evidence/at "2026-08-25T00:00:00Z"
                      :evidence/body {:name "n" :hook "h" :body "b"}}
        proposed {:hx/id "hx-memory" :hx/type :memory/assert
                  :hx/endpoints [memory-id "p" pattern]
                  :hx/props {:domain :mathematics :state :current
                             :attachment-status :proposed
                             :roles {:entry memory-id :subjects ["p" pattern]
                                     :patterns [pattern]}}}
        entries (atom {memory-id memory-entry})
        query-edge (atom proposed)
        durable-edge (atom proposed)
        appends (atom 0)
        posts (atom 0)
        request {:deposit {:depositor "scribe"
                           :candidates [{:memory-id memory-id
                                         :pattern-ids [pattern]}]}
                 :reviewer "proctor" :review-job "review-job"
                 :reviews [{:memory-id memory-id
                            :verdict :reject
                            :reason "not actionable"
                            :residual "open goal"
                            :pattern-ids [pattern]}]}
        effects {:evidence-store :store
                 :fetch-entry #(get @entries %)
                 :append-entry (fn [entry]
                                 (swap! appends inc)
                                 (swap! entries assoc (:evidence/id entry) entry)
                                 {:ok true :entry entry})
                 :fetch-hyperedges (fn [end & _]
                                     (if (= memory-id end)
                                       [@query-edge]
                                       []))
                 :post-hyperedge (fn [_ edge]
                                   (swap! posts inc)
                                   (reset! durable-edge edge)
                                   {:ok true :hyperedge edge})}
        first-result (sut/persist! request effects)]
    (is (= :promotion-review-projection-failed
           (:error/code first-result)))
    (is (= 1 @posts))
    (is (= 1 @appends))
    ;; The durable post is now visible. Replaying the exact persisted review
    ;; must observe it, not append evidence or post a second edge version.
    (reset! query-edge @durable-edge)
    (let [second-result (sut/persist! request effects)]
      (is (:ok second-result) second-result)
      (is (= 1 @posts))
      (is (= 1 @appends))
      (is (= :reject
             (get-in @query-edge [:hx/props :review :verdict])))
      (is (true? (get-in second-result
                         [:persisted 0 :idempotent-replay?]))))))

(deftest invalid-candidate-projection-is-an-apparatus-failure
  (let [memory-id "e-memory"
        attached-pattern "math-formalization/attached"
        claimed-pattern "math-formalization/claimed"
        memory-entry {:evidence/id memory-id
                      :evidence/subject {:ref/type :problem :ref/id "p"}
                      :evidence/type :memory :evidence/claim-type :assert
                      :evidence/author "scribe" :evidence/session-id "deposit"
                      :evidence/at "2026-08-26T00:00:00Z"
                      :evidence/body {:name "n" :hook "h" :body "b"}}
        entries (atom {memory-id memory-entry})
        edge {:hx/id "hx-memory" :hx/type :memory/assert
              :hx/endpoints [memory-id "p" attached-pattern]
              :hx/props {:domain :mathematics :state :current
                         :attachment-status :proposed
                         :roles {:entry memory-id
                                 :subjects ["p" attached-pattern]
                                 :patterns [attached-pattern]}}}
        result
        (sut/persist!
         {:deposit {:depositor "scribe"
                    :candidates [{:memory-id memory-id
                                  :pattern-ids [attached-pattern]}]}
          :reviewer "proctor" :review-job "review-job"
          :reviews [{:memory-id memory-id :verdict :approve
                     :reviewer "proctor" :reason "claimed fit"
                     :residual "open" :pattern-ids [claimed-pattern]
                     :attachment-status :reviewed}]}
         {:evidence-store :store
          :fetch-entry #(get @entries %)
          :append-entry #(do (swap! entries assoc (:evidence/id %) %)
                             {:ok true :entry %})
          :fetch-hyperedges (fn [end & _]
                              (if (some #{end} (:hx/endpoints edge)) [edge] []))
          :post-hyperedge (fn [_ _]
                            (throw (ex-info "must not publish" {})))})
        review (first (:reviews result))]
    (is (false? (:ok result)) result)
    (is (= :promotion-review-projection-failed (:error/code result)))
    (is (= false (:projection/valid? review)))
    (is (= :approve (:verdict review)))
    (is (= :promotion-review-projection-failed
           (get-in result [:findings 0 :failure])))
    (is (string? (:review-evidence-id review)))
    (is (map? (:review-materialization review)))))
