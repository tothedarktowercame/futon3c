(ns futon3c.apm.live-promotion-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.live-promotion :as sut]))

(deftest deposit-review-publish-is-durable-and-ordered
  (let [saved (atom nil) calls (atom [])
        candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1 2 3]}
        base {:persist-fn #(do (reset! saved %) {:ok true})
              :publish-fn (fn [_] (swap! calls conj :publish)
                            {:ok true :receipt {:receipt/id "promotion"}})}
        r1 (sut/drive! (merge base {:state nil
                                    :deposit-fn #(do (swap! calls conj :deposit)
                                                     {:ok true :job "scribe"})}))
        r2 (sut/drive! (merge base {:state (:state r1)
                                    :deposit-fn (fn [_] {:ok true :report
                                                        {:depositor "scribe"
                                                         :candidates [candidate]}})
                                    :review-fn (fn [_] (swap! calls conj :review)
                                                 {:ok true :job "proctor"})}))
        review {:memory-id "m" :reviewer "proctor" :verdict :approve
                :review-evidence-id "e" :attachment-status :reviewed
                :pattern-ids ["p"]}
        r3 (sut/drive! (merge base {:state (:state r2)
                                    :review-fn (fn [& _] {:ok true
                                                         :reviewer "proctor"
                                                         :reviews [review]})}))]
    (is (= :awaiting-terminal (:status r1)))
    (is (= :independent-review (get-in r2 [:state :stage])))
    (is (= :certified (:status r3)))
    (is (= [:deposit :review :publish] @calls))))
