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
         {:depositor "scribe" :candidates [(dissoc candidate :kind)] :lanes []}
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
    (is (= :memory (:kind persisted)))
    (is (nil? (:reported-kind persisted)))
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

(deftest controller-derives-kind-and-retains-agent-claim
  (let [ordinary (sut/canonical-candidate
                  {:dispatch/id "dispatch"} "scribe" 1
                  (dissoc candidate :kind))
        misleading (sut/canonical-candidate
                    {:dispatch/id "dispatch"} "scribe" 2
                    (assoc candidate :kind :proof-text))
        proof-text (sut/canonical-candidate
                    {:dispatch/id "dispatch"} "scribe" 3
                    (assoc candidate :kind :fact
                           :body (apply str (repeat 4 "lemma x : True := by\n"))))]
    (is (= :memory (:kind ordinary)))
    (is (nil? (:reported-kind ordinary)))
    (is (= :memory (:kind misleading)))
    (is (= :proof-text (:reported-kind misleading)))
    (is (= :proof-text (:kind proof-text)))
    (is (= :fact (:reported-kind proof-text)))))

(deftest independently-reassigned-candidate-remains-visible-on-replay
  (let [memory-id "e-memory"
        review-id "e-review"
        original-pattern "math-formalization/original"
        reassigned-pattern "math-formalization/canonical"
        body (select-keys candidate [:name :hook :kind :body :why :how-to-apply])
        persisted (assoc candidate
                         :memory-id memory-id
                         :content-digest (machine/ledger-digest [body])
                         :pattern-ids [original-pattern])
        entries
        {memory-id {:evidence/id memory-id :evidence/body body}
         review-id {:evidence/id review-id
                    :evidence/body
                    {:review/memory-id memory-id
                     :review/verdict :reassign}}}
        edge {:hx/id "hx-memory" :hx/type :memory/assert
              :hx/endpoints [memory-id reassigned-pattern]
              :hx/props
              {:state :current :attachment-status :reviewed
               :roles {:entry memory-id :patterns [reassigned-pattern]}
               :review {:evidence-id review-id :verdict :reassign
                        :pattern-ids [reassigned-pattern]}}}]
    (is (sut/visible? persisted #(get entries %)
                      (constantly [edge])))))

(deftest reviewed-candidate-with-unreadable-review-is-not-visible
  (let [memory-id "e-memory"
        body (select-keys candidate [:name :hook :kind :body :why :how-to-apply])
        persisted (assoc candidate
                         :memory-id memory-id
                         :content-digest (machine/ledger-digest [body]))
        edge {:hx/id "hx-memory" :hx/type :memory/assert
              :hx/endpoints [memory-id "math-formalization/canonical"]
              :hx/props
              {:state :current :attachment-status :reviewed
               :roles {:entry memory-id
                       :patterns ["math-formalization/canonical"]}
               :review {:evidence-id "missing-review" :verdict :reassign
                        :pattern-ids ["math-formalization/canonical"]}}}]
    (is (not (sut/visible? persisted
                           #(when (= memory-id %)
                              {:evidence/id memory-id :evidence/body body})
                           (constantly [edge]))))))
