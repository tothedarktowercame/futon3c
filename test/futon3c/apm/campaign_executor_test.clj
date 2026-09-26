(ns futon3c.apm.campaign-executor-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-executor :as executor]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-regulator :as regulator])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(def at "2026-08-20T12:00:00Z")
(def block-plan [{:block-id "b1" :ordinal 1
                  :units [{:frame-id "f1" :problem-id "p1"}]}])
(def obligation-plan {:probe {:kind :probe :role :proctor}
                      :close {:kind :close :role :proctor}})

(defn event [seq type body]
  {:event/id (str "seed-" seq) :event/seq seq :event/type type
   :event/campaign-id "apm-200" :event/actor "seed" :event/at at
   :event/expected-version seq :event/body body})

(defn temp-dir []
  (Files/createTempDirectory "campaign-executor-"
                             (make-array FileAttribute 0)))

(defn delete-tree! [^Path dir]
  (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [path (reverse (sort-by #(.getNameCount ^Path %)
                                  (iterator-seq (.iterator paths))))]
      (Files/deleteIfExists path))))

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

(defn fixture []
  (let [dir (temp-dir) path (.resolve dir "ledger.edn")
        registration
        (event 0 :campaign/registered
               {:phase-order [:probe :close] :block-plan block-plan
                :obligation-plan obligation-plan :claims-required? true})
        initial-digest (:ledger/digest (machine/projection []))
        _ (ledger/compare-and-append! path 0 initial-digest registration)
        projection (:projection (ledger/read-ledger path))
        certificate (cert projection)
        obligation (:obligation (regulator/decide certificate))]
    {:dir dir :path path :certificate certificate :obligation obligation}))

(defn run-executor [fixture handler]
  (executor/execute!
   {:ledger-path (:path fixture) :obligation (:obligation fixture)
    :current-certificate (:certificate fixture)
    :handlers {:open-block handler} :actor "regulator" :at at}))

(deftest successful-effect-is-claimed-certified-and-appended
  (let [f (fixture) calls (atom [])]
    (try
      (let [result (run-executor
                    f (fn [action]
                        (swap! calls conj action)
                        {:ok true :certificate {:opened true}}))
            projection (:projection (ledger/read-ledger (:path f)))]
        (is (:completed? result))
        (is (= 1 (count @calls)))
        (is (= (:obligation/id (:obligation f))
               (:idempotency-key (first @calls))))
        (is (= "b1" (:active/block projection)))
        (is (nil? (:active/claim projection)))
        (is (= 3 (:campaign/version projection))))
      (finally (delete-tree! (:dir f))))))

(deftest handler-failure-leaves-visible-claim-and-never-completes
  (let [f (fixture)]
    (try
      (let [result (run-executor f (fn [_] {:ok false :error/code :boom}))
            projection (:projection (ledger/read-ledger (:path f)))]
        (is (= :boom (:error/code result)))
        (is (:claim-persisted? result))
        (is (= (:obligation/id (:obligation f))
               (get-in projection [:active/claim :obligation/id])))
        (is (nil? (:active/block projection))))
      (finally (delete-tree! (:dir f))))))

(deftest malformed-effect-certificate-fails-closed-after-claim
  (let [f (fixture)]
    (try
      (let [result (run-executor f (fn [_] {:ok true :certificate "prose"}))]
        (is (= :campaign-obligation-certificate-invalid (:error/code result)))
        (is (:claim-persisted? result)))
      (finally (delete-tree! (:dir f))))))

(deftest competing-claim-wins-before-handler-runs
  (let [f (fixture) calls (atom 0)
        obligation (:obligation f)
        certificate (:certificate f)
        raw {:event/seq (:ledger/event-count certificate)
             :event/type :obligation/claimed
             :event/campaign-id (:campaign/id certificate)
             :event/actor "other" :event/at at
             :event/expected-version (:campaign/version certificate)
             :event/body {:obligation obligation}}
        claim-event (assoc raw :event/id (machine/ledger-digest [raw]))]
    (try
      (is (:ok (ledger/compare-and-append!
                (:path f) (:campaign/version certificate)
                (:ledger/digest certificate) claim-event)))
      (let [result (run-executor f (fn [_] (swap! calls inc)
                                     {:ok true :certificate {}}))]
        (is (= :campaign-obligation-claim-refused (:error/code result)))
        (is (zero? @calls)))
      (finally (delete-tree! (:dir f))))))

(deftest missing-handler-does-not-claim
  (let [f (fixture)]
    (try
      (let [result (executor/execute!
                    {:ledger-path (:path f) :obligation (:obligation f)
                     :current-certificate (:certificate f) :handlers {}
                     :actor "regulator" :at at})]
        (is (= :campaign-obligation-handler-missing (:error/code result)))
        (is (= 1 (:ledger/event-count
                  (:projection (ledger/read-ledger (:path f)))))))
      (finally (delete-tree! (:dir f))))))

(deftest claim-context-cannot-shadow-the-authorized-obligation
  (let [f (fixture) calls (atom 0)]
    (try
      (let [result (executor/execute!
                    {:ledger-path (:path f) :obligation (:obligation f)
                     :current-certificate (:certificate f)
                     :handlers {:open-block (fn [_] (swap! calls inc))}
                     :actor "regulator" :at at
                     :claim-context {:obligation {:obligation/id "forged"}}})]
        (is (= :campaign-executor-claim-context-invalid (:error/code result)))
        (is (zero? @calls))
        (is (= 1 (:campaign/version
                  (:projection (ledger/read-ledger (:path f)))))))
      (finally (delete-tree! (:dir f))))))
