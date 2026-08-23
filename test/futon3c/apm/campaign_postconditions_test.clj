(ns futon3c.apm.campaign-postconditions-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-postconditions :as postconditions]))

(defn certificate [events]
  (let [p (machine/projection events)
        body {:certificate/type :campaign-projection :certificate/version 1
              :generated-at "2026-08-20T17:00:00Z" :snapshot/status :valid
              :campaign/id (:campaign/id p) :campaign/status (:campaign/status p)
              :campaign/version (:campaign/version p)
              :campaign/phase-order (:campaign/phase-order p)
              :campaign/block-plan (:campaign/block-plan p)
              :campaign/obligation-plan (:campaign/obligation-plan p)
              :campaign/blocks (:campaign/blocks p)
              :campaign/frames (:campaign/frames p)
              :campaign/claims-required? (:campaign/claims-required? p)
              :ledger/digest (:ledger/digest p)
              :ledger/event-count (:ledger/event-count p)
              :facts/digest "facts" :active/block (:active/block p)
              :active/frame (:active/frame p) :active/claim (:active/claim p)
              :counts (:counts p) :reconciliation {:findings []}}]
    (assoc body :certificate/id (machine/ledger-digest [body]))))

(defn event [n type body]
  {:event/id (str "e" n) :event/seq n :event/type type
   :event/campaign-id "apm-countdown" :event/actor "test"
   :event/at "2026-08-20T17:00:00Z" :event/expected-version n
   :event/body body})

(def unit {:frame-id "f18" :problem-id "a97J07" :arm :treatment
           :registration-hash "registration" :harness-hash "harness"})
(def registered
  (event 0 :campaign/registered
         {:phase-order [:preflight :solve]
          :block-plan [{:block-id "countdown-10" :ordinal 1 :units [unit]}]
          :obligation-plan {:preflight {:kind :preflight :role :proctor}
                            :solve {:kind :solve :role :solver}}
          :claims-required? true}))
(def claim-block
  (event 1 :obligation/claimed
         {:obligation {:obligation/id "open-block"
                       :obligation/preconditions {:campaign/version 1}}}))
(def opened-block
  (event 2 :block/opened
         {:block-id "countdown-10" :ordinal 1 :units [unit]
          :obligation/id "open-block"}))
(def claim-frame
  (event 3 :obligation/claimed
         {:obligation {:obligation/id "open-f18"
                       :obligation/preconditions {:campaign/version 3}}}))
(def opened-frame
  (event 4 :frame/opened
         (assoc unit :block-id "countdown-10" :obligation/id "open-f18")))

(deftest open-frame-postconditions-prove-state-and-next-data-shape
  (let [before (certificate [registered claim-block opened-block])
        after (certificate [registered claim-block opened-block
                            claim-frame opened-frame])
        action {:kind :open-frame :block-id "countdown-10"
                :frame-id "f18" :problem-id "a97J07" :arm :treatment
                :completion {:event/body (assoc unit :block-id "countdown-10")}}
        result (postconditions/validate-open-frame
                {:obligation {:obligation/action action}
                 :before before :after after
                 :execution {:ok true :completed? true}})]
    (is (:ok result))
    (is (= {:kind :preflight :role :proctor :frame-id "f18"
            :problem-id "a97J07" :block-id "countdown-10" :phase :preflight}
           (:next-action result)))))

(deftest identity-drift-fails-visibly
  (let [before (certificate [registered claim-block opened-block])
        after (certificate [registered claim-block opened-block
                            claim-frame opened-frame])
        action {:kind :open-frame :block-id "countdown-10"
                :frame-id "f18" :problem-id "wrong" :arm :treatment
                :completion {:event/body (assoc unit :block-id "countdown-10")}}
        result (postconditions/validate-open-frame
                {:obligation {:obligation/action action} :before before :after after
                 :execution {:ok true :completed? true}})]
    (is (false? (:ok result)))
    (is (contains? (:failed result) :problem-id-matches?))))
