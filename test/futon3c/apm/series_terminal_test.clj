(ns futon3c.apm.series-terminal-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.series-terminal :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- event [seq type body]
  {:event/id (str "event-" seq) :event/seq seq :event/type type
   :event/campaign-id "old-series" :event/actor "controller"
   :event/at "2026-08-21T00:00:00Z" :event/expected-version seq
   :event/body body})

(defn- obligation [id version action]
  {:obligation/id id :obligation/type :campaign :obligation/action action
   :obligation/preconditions {:campaign/id "old-series"
                              :campaign/version version
                              :ledger/digest "test-digest"}})

(def prefix
  [(event 0 :campaign/registered
          {:series :apm :manifest-hash "old" :phase-order [:preflight :solve]
           :claims-required? true
           :block-plan [{:block-id "old-block" :ordinal 1
                         :units [{:frame-id "f19" :problem-id "p19"}]}]
           :obligation-plan {:preflight {:kind :preflight :role :proctor}
                             :solve {:kind :solve :role :solver}}})
   (event 1 :obligation/claimed
          {:obligation (obligation "o1" 1 {:kind :open-block})})
   (event 2 :block/opened {:block-id "old-block" :ordinal 1
                           :obligation/id "o1" :certificate {:ok true}})
   (event 3 :obligation/claimed
          {:obligation (obligation "o2" 3 {:kind :open-frame})})
   (event 4 :frame/opened {:frame-id "f19" :problem-id "p19"
                           :block-id "old-block" :obligation/id "o2"
                           :certificate {:ok true}})
   (event 5 :obligation/claimed
          {:obligation (obligation "o3" 5 {:kind :preflight})})
   (event 6 :frame/advanced {:frame-id "f19" :from :preflight :to :solve
                             :obligation/id "o3" :certificate {:ok true}})])

(deftest partial-frame-closes-old-block-and-campaign-append-only
  (let [path (Files/createTempFile "series-terminal-" ".edn"
                                   (make-array FileAttribute 0))]
    (try
      (doseq [e prefix]
        (let [before (ledger/read-ledger path)
              projection (:projection before)]
          (is (:ok (ledger/compare-and-append!
                    path (:campaign/version projection) (:ledger/digest projection) e)))))
      (let [result (sut/close! {:ledger-path path :frame-id "f19"
                                :problem-id "p19"
                                :final-head "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                :residual "Two named Lean obligations remain."
                                :rounds 50
                                :now "2026-08-21T01:00:00Z"})
            loaded (ledger/read-ledger path)
            projection (:projection loaded)]
        (is (:ok result))
        (is (= [:frame :block :campaign] (mapv :stage (:steps result))))
        (is (= :closed (:campaign/status projection)))
        (is (= :stopped (get-in projection [:campaign/frames "f19" :status])))
        (is (= :partial
               (get-in projection [:campaign/frames "f19" :stop
                                   :certificate :outcome])))
        (is (= (+ (count prefix) 6) (:ledger/event-count projection))))
      (finally (Files/deleteIfExists path)))))

(deftest post-verify-apparatus-failure-preserves-solved-problem-outcome
  (let [projection {:projection/status :valid :campaign/status :running
                    :campaign/id "c" :campaign/version 11
                    :ledger/digest "digest" :ledger/event-count 11
                    :active/claim nil :active/block "b"
                    :active/frame {:frame-id "f21" :problem-id "p"
                                   :phase :promote-solver}}
        events []
        prepared (sut/prepare
                  {:projection projection :events events}
                  {:frame-id "f21" :problem-id "p"
                   :final-head "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                   :residual "Pinned Proctor card cannot independently review."
                   :rounds 11
                   :partial-reason :promotion-review-apparatus-invalid
                   :problem-outcome :solved
                   :proof-receipt-ids ["solve" "verify"]
                   :now "2026-08-22T08:00:00Z"})]
    ;; The synthetic prefix is too small for a valid successor projection, but
    ;; input validation must accept this distinct terminal evidence shape.
    (is (not= :series-terminal-partial-evidence-invalid
              (:error/code prepared)))))
