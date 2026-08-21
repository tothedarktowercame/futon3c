(ns futon3c.apm.countdown-pre-admission
  "Regression gates derived from the frame-18 failing baseline."
  (:require [futon3c.apm.countdown-manifest :as manifest]
            [futon3c.apm.frame-cycle-contract :as contract]
            [futon3c.apm.campaign-machine :as machine]))

(def required-phases
  [:preflight :solve :verify :student-attempt-1 :guide-intervention-1
   :student-attempt-2 :guide-intervention-2 :student-attempt-3
   :scribe-reduce :close-frame])

(def required-close-artifacts
  #{:verify-receipt :memory-use-1-receipt :memory-use-2-receipt
    :memory-use-3-receipt :guide-intervention-1-receipt
    :guide-intervention-2-receipt :scribe-lane-receipt
    :memory-disposition-receipt
    :promotion-review-receipt})

(defn validate
  [{:keys [countdown-manifest cycle-contract frame-id manifest-check]}]
  (let [manifest-check (or manifest-check (manifest/validate countdown-manifest))
        contract-check (contract/validate-contract cycle-contract)
        unit (some #(when (= frame-id (:frame/id %)) %)
                   (:units countdown-manifest))
        apparatus-observations (:apparatus-observations manifest-check)
        order (:phase-order cycle-contract)
        close-requires (get-in cycle-contract [:phases :close-frame :requires])
        registration-body {:manifest/id (:manifest/id countdown-manifest)
                           :apparatus/pin-id (get-in countdown-manifest
                                                     [:apparatus :pin/id])
                           :unit unit :phase-order order}
        checks
        {:full-cycle-registered? (= required-phases order)
         :close-requires-memory-path?
         (every? #(contains? close-requires %) required-close-artifacts)
         :manifest-valid? (:valid? manifest-check)
         :unit-present? (map? unit)
         :unit-non-topology? (= :non-topology (:classification/value unit))
         :unit-apparatus-matches?
         (= (:apparatus/pin-id unit)
            (get-in countdown-manifest [:apparatus :pin/id]))
         :apparatus-frozen?
         (and (seq apparatus-observations)
              (every? :valid? apparatus-observations))}]
    {:ok (every? true? (vals checks))
     :checks checks
     :frame/id frame-id
     :registration/body registration-body
     :registration/hash (machine/ledger-digest [registration-body])
     :manifest-check manifest-check
     :contract-check contract-check
     :apparatus-observations apparatus-observations}))
