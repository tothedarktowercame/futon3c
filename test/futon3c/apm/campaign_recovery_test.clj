(ns futon3c.apm.campaign-recovery-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-recovery :as recovery]
            [futon3c.apm.campaign-regulator :as regulator])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(def at "2026-08-20T12:00:00Z")

(defn seed-event [seq type body]
  {:event/id (str "seed-" seq) :event/seq seq :event/type type
   :event/campaign-id "apm-200" :event/actor "seed" :event/at at
   :event/expected-version seq :event/body body})

(defn cert [projection]
  (let [body {:certificate/type :campaign-projection :certificate/version 1
              :generated-at at :snapshot/status :valid
              :campaign/id (:campaign/id projection)
              :campaign/status (:campaign/status projection)
              :campaign/version (:campaign/version projection)
              :campaign/phase-order (:campaign/phase-order projection)
              :campaign/block-plan (:campaign/block-plan projection)
              :campaign/obligation-plan (:campaign/obligation-plan projection)
              :campaign/claims-required? (:campaign/claims-required? projection)
              :campaign/blocks (:campaign/blocks projection)
              :campaign/frames (:campaign/frames projection)
              :ledger/digest (:ledger/digest projection)
              :ledger/event-count (:ledger/event-count projection)
              :facts/digest "facts" :active/block (:active/block projection)
              :active/frame (:active/frame projection)
              :active/claim (:active/claim projection)
              :counts (:counts projection) :reconciliation {:findings []}}]
    (assoc body :certificate/id (machine/ledger-digest [body]))))

(defn temp-dir []
  (Files/createTempDirectory "campaign-recovery-"
                             (make-array FileAttribute 0)))

(defn delete-tree! [^Path dir]
  (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [path (reverse (sort-by #(.getNameCount ^Path %)
                                  (iterator-seq (.iterator paths))))]
      (Files/deleteIfExists path))))

(defn append! [path projection event]
  (ledger/compare-and-append! path (:campaign/version projection)
                              (:ledger/digest projection) event))

(defn fixture []
  (let [dir (temp-dir) path (.resolve dir "ledger.edn")
        registration
        (seed-event 0 :campaign/registered
                    {:phase-order [:probe :close]
                     :block-plan [{:block-id "b1"
                                   :units [{:frame-id "f1" :problem-id "p"}]}]
                     :obligation-plan {:probe {:kind :probe}
                                       :close {:kind :close}}
                     :claims-required? true})
        empty-projection (machine/projection [])
        _ (append! path empty-projection registration)
        before-claim (:projection (ledger/read-ledger path))
        before-cert (cert before-claim)
        obligation (:obligation (regulator/decide before-cert))
        raw {:event/seq 1 :event/type :obligation/claimed
             :event/campaign-id "apm-200" :event/actor "executor"
             :event/at at :event/expected-version 1
             :event/body {:obligation obligation}}
        claim-event (assoc raw :event/id (machine/ledger-digest [raw]))
        _ (append! path before-claim claim-event)
        claimed (:projection (ledger/read-ledger path))]
    {:dir dir :path path :certificate (cert claimed)
     :obligation obligation}))

(defn recover [f assessment & [assessor]]
  (recovery/recover! {:ledger-path (:path f)
                      :current-certificate (:certificate f)
                      :assessment assessment
                      :assessor (or assessor "independent-proctor") :at at}))

(deftest independently-proved-not-started-releases-claim
  (let [f (fixture)]
    (try
      (let [result (recover f {:outcome :not-started
                               :evidence {:job-ledger :absent}})
            projection (:projection (ledger/read-ledger (:path f)))]
        (is (:released? result))
        (is (nil? (:active/claim projection)))
        (is (nil? (:active/block projection)))
        (is (= 3 (:campaign/version projection))))
      (finally (delete-tree! (:dir f))))))

(deftest independently-proved-completed-appends-original-transition
  (let [f (fixture)]
    (try
      (let [result (recover f {:outcome :completed
                               :evidence {:job "done" :artifact "present"}
                               :effect-certificate {:opened true}})
            projection (:projection (ledger/read-ledger (:path f)))]
        (is (:recovered-completion? result))
        (is (= "b1" (:active/block projection)))
        (is (nil? (:active/claim projection))))
      (finally (delete-tree! (:dir f))))))

(deftest unknown-outcome-never-mutates-or-retries
  (let [f (fixture)
        before (ledger/read-ledger (:path f))]
    (try
      (let [result (recover f {:outcome :unknown
                               :evidence {:job :missing :artifact :unknown}})
            after (ledger/read-ledger (:path f))]
        (is (= :campaign-recovery-outcome-unknown (:error/code result)))
        (is (:claim-retained? result))
        (is (= (get-in before [:projection :ledger/digest])
               (get-in after [:projection :ledger/digest]))))
      (finally (delete-tree! (:dir f))))))

(deftest claimant-cannot-assess-own-recovery
  (let [f (fixture)]
    (try
      (let [result (recover f {:outcome :not-started :evidence {:self true}}
                            "executor")]
        (is (= :campaign-recovery-assessor-not-independent
               (:error/code result))))
      (finally (delete-tree! (:dir f))))))

(deftest completed-assessment-without-certificate-retains-claim
  (let [f (fixture)]
    (try
      (let [result (recover f {:outcome :completed :evidence {:job :done}})]
        (is (= :campaign-recovery-effect-certificate-required
               (:error/code result)))
        (is (:claim-retained? result)))
      (finally (delete-tree! (:dir f))))))

(deftest stale-certificate-cannot-recover-after-another-writer
  (let [f (fixture)
        first-result (recover f {:outcome :not-started
                                 :evidence {:job-ledger :absent}})]
    (try
      (is (:released? first-result))
      (is (= :campaign-recovery-certificate-stale
             (:error/code
              (recover f {:outcome :not-started
                          :evidence {:job-ledger :absent}}))))
      (finally (delete-tree! (:dir f))))))
