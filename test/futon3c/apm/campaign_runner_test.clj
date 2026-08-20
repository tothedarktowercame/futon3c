(ns futon3c.apm.campaign-runner-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-runner :as runner])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(def now (Instant/parse "2026-08-20T12:00:00Z"))
(def at (str now))

(defn temp-dir []
  (Files/createTempDirectory "campaign-runner-"
                             (make-array FileAttribute 0)))

(defn delete-tree! [^Path dir]
  (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [path (reverse (sort-by #(.getNameCount ^Path %)
                                  (iterator-seq (.iterator paths))))]
      (Files/deleteIfExists path))))

(defn fixture []
  (let [dir (temp-dir)
        ledger-path (.resolve dir "ledger.edn")
        event {:event/id "registered" :event/seq 0
               :event/type :campaign/registered :event/campaign-id "apm-200"
               :event/actor "seed" :event/at at :event/expected-version 0
               :event/body
               {:manifest-hash "manifest-200" :phase-order [:probe :close]
                :block-plan [{:block-id "b1" :ordinal 1 :units []}]
                :obligation-plan {:probe {:kind :probe}
                                  :close {:kind :close}}
                :claims-required? true}}
        initial (machine/projection [])]
    (ledger/compare-and-append! ledger-path 0 (:ledger/digest initial) event)
    {:dir dir :ledger-path ledger-path
     :certificates (.resolve dir "certificates")
     :projection (.resolve dir "projection")
     :registered-projection (:projection (ledger/read-ledger ledger-path))}))

(defn options [fixture handler projected]
  (let [start (:registered-projection fixture)
        permit (batch/issue
                {:campaign-id "apm-200" :manifest-hash "manifest-200"
                 :start-version (:campaign/version start)
                 :start-ledger-digest (:ledger/digest start)
                 :issuer "joe" :actor "runner" :max-actions 10
                 :allowed-kinds [:open-block :close-block :close-campaign]
                 :issued-at "2026-08-20T11:00:00Z"
                 :valid-before "2026-08-20T13:00:00Z"})]
    {:ledger-path (:ledger-path fixture)
   :certificate-directory (:certificates fixture)
   :projection-directory (:projection fixture)
   :observation-fn (fn [_]
                     {:binding-response {:ok true :bound? false}
                      :jobs-response {:ok true :jobs []}})
   :now-fn (fn [] now)
   :project-fn #(swap! projected conj %)
   :handlers {:open-block handler :close-block handler :close-campaign handler}
   :actor "runner" :batch-permit permit :trusted-permit-id (:permit/id permit)
   :trusted-permit-issuer "joe"}))

(deftest successful-step-projects-the-post-transition-ledger
  (let [f (fixture) projected (atom [])]
    (try
      (let [result (runner/step!
                    (options f (fn [_] {:ok true :certificate {:done true}})
                             projected))
            durable (:projection (ledger/read-ledger (:ledger-path f)))
            shown (:certificate (last @projected))]
        (is (= :advanced (:runner/status result)))
        (is (= 2 (count @projected)))
        (is (= (:campaign/version durable) (:campaign/version shown)))
        (is (= (:ledger/digest durable) (:ledger/digest shown)))
        (is (= "b1" (:active/block shown))))
      (finally (delete-tree! (:dir f))))))

(deftest failed-effect-projects-its-durable-claim-and-stops
  (let [f (fixture) projected (atom [])]
    (try
      (let [result (runner/step!
                    (options f (fn [_] {:ok false :error/code :effect-failed})
                             projected))
            shown (:certificate (last @projected))]
        (is (= :campaign-runner-execution-failed (:error/code result)))
        (is (= :stop (:runner/status result)))
        (is (= 2 (:campaign/version shown)))
        (is (some? (:active/claim shown))))
      (finally (delete-tree! (:dir f))))))

(deftest invalid-pre-action-observation-never-runs-handler
  (let [f (fixture) projected (atom []) calls (atom 0)
        opts (assoc (options f (fn [_] (swap! calls inc)) projected)
                    :observation-fn (fn [_] {:binding-response {:ok false}
                                             :jobs-response {:ok true :jobs []}}))]
    (try
      (let [result (runner/step! opts)]
        (is (= :stop (:runner/status result)))
        (is (= :campaign-snapshot-not-valid
               (get-in result [:decision :reason])))
        (is (zero? @calls))
        (is (= :stale (get-in result [:checkpoint :certificate :snapshot/status]))))
      (finally (delete-tree! (:dir f))))))

(deftest bounded-batch-pauses-on-a-synchronized-checkpoint
  (let [f (fixture) projected (atom [])]
    (try
      (let [result (runner/run-batch!
                    (assoc (options f (fn [_] {:ok true :certificate {}}) projected)
                           :max-actions 1))
            durable (:projection (ledger/read-ledger (:ledger-path f)))
            checkpoint (get-in result [:checkpoint :certificate])]
        (is (= :action-bound (:batch/status result)))
        (is (= 1 (:batch/completed-actions result)))
        (is (= (:campaign/version durable) (:campaign/version checkpoint)))
        (is (= (:ledger/digest durable) (:ledger/digest checkpoint))))
      (finally (delete-tree! (:dir f))))))

(deftest refused-batch-permit-cannot-reach-an-effect-or-claim
  (let [f (fixture) projected (atom []) calls (atom 0)
        opts (options f (fn [_] (swap! calls inc)
                          {:ok true :certificate {}}) projected)]
    (try
      (let [result (runner/run-batch!
                    (assoc opts :max-actions 1
                           :batch-permit (assoc (:batch-permit opts)
                                                :permit/max-actions 200)))
            durable (:projection (ledger/read-ledger (:ledger-path f)))]
        (is (= :stopped (:batch/status result)))
        (is (= :campaign-runner-batch-permit-refused (:error/code result)))
        (is (= :campaign-batch-permit-content-invalid
               (get-in result [:authorization :error/code])))
        (is (zero? @calls))
        (is (= 1 (:campaign/version durable)))
        (is (nil? (:active/claim durable))))
      (finally (delete-tree! (:dir f))))))

(deftest batch-refuses-an-unbounded-run
  (let [f (fixture)]
    (try
      (is (= :campaign-runner-action-bound-required
             (:error/code (runner/run-batch! {:max-actions nil}))))
      (finally (delete-tree! (:dir f))))))
