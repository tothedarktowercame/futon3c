(ns futon3c.apm.live-promotion-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.live-promotion :as sut]))

(deftest deposit-review-publish-is-durable-and-ordered
  (let [saved (atom nil) calls (atom [])
        candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1 2 3]}
        lanes [{:lane :solve :status :ran}
               {:lane :arc :status :ran-empty :reason "no errors"}
               {:lane :trajectory :status :ran}
               {:lane :challenge :status :not-run :reason "no prior claim"}]
        base {:persist-fn #(do (reset! saved %) {:ok true})
              :publish-fn (fn [publication]
                            (is (= "scribe" (get-in publication
                                                     [:deposit :depositor])))
                            (is (= "proctor" (:reviewer publication)))
                            (swap! calls conj :publish)
                            {:ok true :receipt {:receipt/id "promotion"}})}
        r1 (sut/drive! (merge base {:state nil
                                    :deposit-fn #(do (swap! calls conj :deposit)
                                                     {:ok true :job "scribe"})}))
        r2 (sut/drive! (merge base {:state (:state r1)
                                    :deposit-fn (fn [_] {:ok true :report
                                                        {:depositor "scribe"
                                                         :candidates [candidate]
                                                         :lanes lanes}})
                                    :review-fn (fn [_] (swap! calls conj :review)
                                                 {:ok true :job "proctor"})}))
        review {:memory-id "m" :reviewer "proctor" :verdict :approve
                :review-evidence-id "e" :attachment-status :reviewed
                :pattern-ids ["p"] :reason "actionable fact"
                :residual "Main.lean:12"}
        r3 (sut/drive! (merge base {:state (:state r2)
                                    :review-fn (fn [& _] {:ok true
                                                         :reviewer "proctor"
                                                         :reviews [review]})}))]
    (is (= :awaiting-terminal (:status r1)))
    (is (= "scribe" (:job-id r1)))
    (is (= :independent-review (get-in r2 [:state :stage])))
    (is (= "proctor" (:job-id r2)))
    (is (= :certified (:status r3)))
    (is (= [:deposit :review :publish] @calls))))

(deftest malformed-deposit-is-durably-redispatched-without-review
  (let [saved (atom nil)
        review-called? (atom false)
        result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "malformed" :attempt 1}
                 :deposit-fn (fn
                               ([job]
                                (is (= "malformed" job))
                                {:ok false
                                 :error/code :promotion-stage-terminal-invalid})
                               ([] {:ok true :job "scribe-retry"}))
                 :review-fn (fn [& _] (reset! review-called? true))
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (:ok result))
    (is (= :awaiting-terminal (:status result)))
    (is (= "scribe-retry" (:job-id result)))
    (is (= 2 (:attempt @saved)))
    (is (= [{:attempt 1 :job "malformed"
             :failure {:error/code :promotion-stage-terminal-invalid}}]
           (:failed-attempts @saved)))
    (is (false? @review-called?))))

(deftest invalid-deposit-shape-is-bounded
  (let [result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "third-invalid" :attempt 3}
                 :deposit-fn (fn [_] {:ok true :report {:depositor "scribe"}})
                 :persist-fn (fn [_] (throw (ex-info "must not persist" {})))})]
    (is (false? (:ok result)))
    (is (= :promotion-deposit-retries-exhausted (:error/code result)))
    (is (= 3 (:attempts result)))
    (is (= [:candidates-missing :lane-report-invalid] (:findings result)))))

(deftest pinned-proctor-review-shape-normalizes-only-with-exact-digest
  (let [normalize #'sut/normalize-review-report
        reviews [{:memory-id "m" :reviewer "proctor" :verdict :reject
                  :pattern-ids []}]
        accepted (normalize {:candidate-set-digest "digest"
                             :base-problem-blob "blob"
                             :open-residuals []
                             :promotion-reviews reviews}
                            "digest" "blob")]
    (is (:ok accepted))
    (is (= "proctor" (:reviewer accepted)))
    (is (= reviews (:reviews accepted)))
    (is (= :promotion-review-candidate-digest-mismatch
           (:error/code
            (normalize {:candidate-set-digest "other"
                        :base-problem-blob "blob" :open-residuals []
                        :promotion-reviews reviews}
                       "digest" "blob"))))
    (is (= :promotion-review-attribution-ambiguous
           (:error/code
            (normalize {:candidate-set-digest "digest"
                        :base-problem-blob "blob" :open-residuals []
                        :promotion-reviews
                        (conj reviews {:memory-id "n" :reviewer "other"
                                       :verdict :reject :pattern-ids []})}
                       "digest" "blob"))))))
