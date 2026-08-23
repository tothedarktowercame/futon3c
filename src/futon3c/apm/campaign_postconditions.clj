(ns futon3c.apm.campaign-postconditions
  "Semantic postcondition checks for effectful campaign boundaries."
  (:require [futon3c.apm.campaign-regulator :as regulator]))

(defn validate-open-frame
  [{:keys [obligation before execution after]}]
  (let [action (:obligation/action obligation)
        frame (:active/frame after)
        next-decision (regulator/decide after)
        next-action (get-in next-decision [:obligation :obligation/action])
        checks
        {:execution-completed? (and (:ok execution) (:completed? execution))
         :snapshot-valid? (= :valid (:snapshot/status after))
         :version-advanced-by-two?
         (= (+ 2 (:campaign/version before)) (:campaign/version after))
         :claim-cleared? (nil? (:active/claim after))
         :block-preserved? (= (:block-id action) (:active/block after))
         :frame-id-matches? (= (:frame-id action) (:frame-id frame))
         :problem-id-matches? (= (:problem-id action) (:problem-id frame))
         :arm-matches? (= (:arm action) (:arm frame))
         :registration-hash-matches?
         (= (get-in action [:completion :event/body :registration-hash])
            (:registration-hash frame))
         :harness-hash-matches?
         (= (get-in action [:completion :event/body :harness-hash])
            (:harness-hash frame))
         :next-obligation-dispatches? (= :dispatch (:decision next-decision))
         :next-obligation-frame-matches? (= (:frame-id action)
                                             (:frame-id next-action))
         :next-obligation-block-matches? (= (:block-id action)
                                             (:block-id next-action))
         :next-obligation-problem-matches? (= (:problem-id action)
                                               (:problem-id next-action))
         :next-obligation-is-first-phase?
         (= (first (:campaign/phase-order after)) (:phase next-action))}
        failed (->> checks (keep (fn [[id passed?]] (when-not passed? id))) set)]
    {:ok (empty? failed) :postcondition/type :open-frame
     :checks checks :failed failed
     :next-action (select-keys next-action
                               [:kind :role :frame-id :problem-id
                                :block-id :phase])}))

(defn validate
  [context]
  (case (get-in context [:obligation :obligation/action :kind])
    :open-frame (validate-open-frame context)
    {:ok true :postcondition/type :not-applicable}))
