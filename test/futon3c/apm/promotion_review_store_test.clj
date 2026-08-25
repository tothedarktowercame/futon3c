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
                :reviewer "proctor" :verdict :reassign
                :attachment-status :reviewed :pattern-ids [new-pattern]
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
        evidence (get @entries review-id)
        edge (get @edges "hx-memory")]
    (is (:ok result) result)
    (is (= :reassign (get-in evidence [:evidence/body :review/verdict])))
    (is (= "returned reason"
           (get-in evidence [:evidence/body :review/reason])))
    (is (= "returned residual"
           (get-in evidence [:evidence/body :review/residual])))
    (is (= [:memory :memory/attachment-review :apm/promotion-review]
           (:evidence/tags evidence)))
    (is (shapes/valid? shapes/EvidenceEntry evidence))
    (is (= :reviewed (get-in edge [:hx/props :attachment-status])))
    (is (= :reassign (get-in edge [:hx/props :review :verdict])))
    (is (= [new-pattern] (get-in edge [:hx/props :roles :patterns])))
    (is (some #{new-pattern} (:hx/endpoints edge)))
    (is (not-any? #{old-pattern} (:hx/endpoints edge)))))
