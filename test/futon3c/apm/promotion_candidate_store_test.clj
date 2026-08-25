(ns futon3c.apm.promotion-candidate-store-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.promotion-candidate-store :as sut]))

(def candidate
  {:memory-id "llm-invented-id"
   :content-digest "llm-invented-digest"
   :name "continuous-extension-tail-integrability"
   :hook "A continuous extension has integrable comparison tails"
   :kind :fact
   :body "Use locallyIntegrable.integrable_of_isBigO_atBot_atTop."
   :pattern-ids ["math-formalization/integrability-by-comparison"]
   :source-attempts [1]})

(deftest controller-persists-and-verifies-candidates-before-review
  (let [entries (atom {}) edges (atom {})
        request {:dispatch/id "dispatch-1" :problem-id "p1"
                 :job-id "scribe-job"}
        result
        (sut/persist!
         {:depositor "scribe" :candidates [candidate] :lanes []}
         request
         {:fetch-entry #(get @entries %)
          :append-entry (fn [entry]
                          (swap! entries assoc (:evidence/id entry) entry)
                          {:ok true :entry entry})
          :post-edge (fn [edge]
                       (swap! edges assoc (:hx/id edge) edge)
                       {:ok true :hyperedge edge})
          :fetch-hyperedges
          (fn [end]
            (filterv #(some #{end} (:hx/endpoints %)) (vals @edges)))})
        persisted (first (:candidates result))
        entry (get @entries (:memory-id persisted))]
    (is (:ok result))
    (is (.startsWith (:memory-id persisted) "e-apm-promotion-"))
    (is (not= "llm-invented-id" (:memory-id persisted)))
    (is (= "llm-invented-id" (:reported-memory-id persisted)))
    (is (= (machine/ledger-digest [(:evidence/body entry)])
           (:content-digest persisted)))
    (is (sut/visible? persisted #(get @entries %)
                      (fn [end]
                        (filterv #(some #{end} (:hx/endpoints %))
                                 (vals @edges)))))))

(deftest incomplete-content-cannot-produce-reviewable-candidate
  (let [writes (atom 0)
        result (sut/persist!
                {:depositor "scribe"
                 :candidates [(dissoc candidate :body)]}
                {:dispatch/id "dispatch" :problem-id "p"}
                {:fetch-entry (constantly nil)
                 :append-entry #(do (swap! writes inc) {:ok true})
                 :post-edge #(do (swap! writes inc) {:ok true})
                 :fetch-hyperedges (constantly [])})]
    (is (= :promotion-candidate-content-invalid (:error/code result)))
    (is (some #{:candidate-body-missing}
              (mapcat :findings (:findings result))))
    (is (zero? @writes))))
