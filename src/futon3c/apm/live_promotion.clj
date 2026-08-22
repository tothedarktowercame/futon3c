(ns futon3c.apm.live-promotion
  "Durable two-seat promotion dispatcher."
  (:require [futon3c.apm.promotion-pipeline :as pipeline]))

(defn drive!
  [{:keys [state deposit-fn review-fn publish-fn persist-fn]}]
  (cond
    (nil? state)
    (let [r (deposit-fn)]
      (if-not (:ok r) r
        (let [s {:state/type :promotion :stage :deposit :job (:job r)}]
          (persist-fn s) {:ok true :status :awaiting-terminal :state s})))

    (= :deposit (:stage state))
    (let [r (deposit-fn (:job state))]
      (if (= :awaiting-terminal (:status r)) r
        (let [checked (pipeline/validate-deposit (:report r))]
          (if-not (:ok checked) checked
            (let [review (review-fn (:candidates checked))]
              (if-not (:ok review) review
                (let [s {:state/type :promotion :stage :independent-review
                         :deposit (:report r) :candidates (:candidates checked)
                         :job (:job review)}]
                  (persist-fn s)
                  {:ok true :status :awaiting-terminal :state s})))))))

    (= :independent-review (:stage state))
    (let [r (review-fn (:job state) (:candidates state))]
      (if (= :awaiting-terminal (:status r)) r
        (let [checked (pipeline/validate-review
                       (:deposit state) (:reviewer r) (:reviews r))]
          (if-not (:ok checked) checked
            (let [published (publish-fn (:candidates checked))]
              (if-not (:ok published) published
                (let [s {:state/type :promotion-certified
                         :receipt (:receipt published)}]
                  (persist-fn s)
                  {:ok true :status :certified :state s
                   :certificate (:receipt published)})))))))

    (= :promotion-certified (:state/type state))
    {:ok true :status :certified :state state :certificate (:receipt state)}

    :else {:ok false :error/code :live-promotion-state-invalid}))
