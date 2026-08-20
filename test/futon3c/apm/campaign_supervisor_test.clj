(ns futon3c.apm.campaign-supervisor-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-batch :as batch]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-supervisor :as supervisor])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(def now (Instant/parse "2026-08-20T12:00:00Z"))

(defn temp-dir []
  (Files/createTempDirectory "campaign-supervisor-"
                             (make-array FileAttribute 0)))

(defn delete-tree! [^Path dir]
  (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [path (reverse (sort-by #(.getNameCount ^Path %)
                                  (iterator-seq (.iterator paths))))]
      (Files/deleteIfExists path))))

(defn fixture []
  (let [dir (temp-dir) ledger-path (.resolve dir "ledger.edn")
        event {:event/id "registered" :event/seq 0
               :event/type :campaign/registered :event/campaign-id "apm-200"
               :event/actor "seed" :event/at (str now) :event/expected-version 0
               :event/body
               {:manifest-hash "manifest" :phase-order [:probe :close]
                :block-plan [{:block-id "b1" :units []}]
                :obligation-plan {:probe {:kind :probe}
                                  :close {:kind :close}}
                :claims-required? true}}
        initial (machine/projection [])]
    (ledger/compare-and-append! ledger-path 0 (:ledger/digest initial) event)
    {:dir dir :ledger-path ledger-path
     :certificate-directory (.resolve dir "certificates")
     :projection-directory (.resolve dir "projection")}))

(defn options [fixture projected handler]
  (let [start (:projection (ledger/read-ledger (:ledger-path fixture)))
        permit (batch/issue
                {:campaign-id "apm-200" :manifest-hash "manifest"
                 :start-version (:campaign/version start)
                 :start-ledger-digest (:ledger/digest start)
                 :issuer "joe" :actor "executor" :max-actions 3
                 :allowed-kinds [:open-block :close-block :close-campaign]
                 :issued-at "2026-08-20T11:00:00Z"
                 :valid-before "2026-08-20T13:00:00Z"})]
    (merge fixture
           {:observation-fn (fn [_] {:binding-response {:ok true :bound? false}
                                     :jobs-response {:ok true :jobs []}})
            :now-fn (fn [] now) :project-fn #(swap! projected conj %)
            :handlers {:open-block handler} :actor "executor"
            :batch-permit permit :trusted-permit-id (:permit/id permit)
            :trusted-permit-issuer "joe"
            :trigger-id "tick-001"
            :recovery-assessor "independent-assessor"})))

(defn create-failed-claim! [opts]
  (supervisor/tick!
   (assoc-in opts [:handlers :open-block]
             (fn [_] {:ok false :error/code :dispatched-then-lost}))))

(deftest completed-assessment-recovers-and-projects-without-original-runner
  (let [f (fixture) projected (atom []) opts (options f projected (fn [_]))]
    (try
      (is (= :campaign-runner-execution-failed
             (:error/code (create-failed-claim! opts))))
      (let [result (supervisor/tick!
                    (assoc opts :assessment-fn
                           (fn [_] {:outcome :completed
                                    :evidence {:job-state :done}
                                    :effect-certificate {:job "j1"}})))
            durable (:projection (ledger/read-ledger (:ledger-path f)))
            shown (:certificate (last @projected))]
        (is (= :recovered (:runner/status result)))
        (is (= "b1" (:active/block durable)))
        (is (nil? (:active/claim durable)))
        (is (= (:ledger/digest durable) (:ledger/digest shown))))
      (finally (delete-tree! (:dir f))))))

(deftest unknown-assessment-retains-and-reprojects-claim
  (let [f (fixture) projected (atom []) opts (options f projected (fn [_]))]
    (try
      (create-failed-claim! opts)
      (let [result (supervisor/tick!
                    (assoc opts :assessment-fn
                           (fn [_] {:outcome :unknown
                                    :evidence {:job-state :unobservable}})))
            durable (:projection (ledger/read-ledger (:ledger-path f)))]
        (is (= :campaign-supervisor-recovery-refused (:error/code result)))
        (is (= :campaign-recovery-outcome-unknown
               (get-in result [:recovery :error/code])))
        (is (some? (:active/claim durable)))
        (is (= (:ledger/digest durable)
               (get-in result [:post-checkpoint :certificate :ledger/digest]))))
      (finally (delete-tree! (:dir f))))))

(deftest assessor-failure-never-mutates-claim
  (let [f (fixture) projected (atom []) opts (options f projected (fn [_]))]
    (try
      (create-failed-claim! opts)
      (let [before (:projection (ledger/read-ledger (:ledger-path f)))
            result (supervisor/tick!
                    (assoc opts :assessment-fn
                           (fn [_] (throw (ex-info "offline" {})))))
            after (:projection (ledger/read-ledger (:ledger-path f)))]
        (is (= :campaign-supervisor-assessment-failed (:error/code result)))
        (is (= (:ledger/digest before) (:ledger/digest after)))
        (is (some? (:active/claim after))))
      (finally (delete-tree! (:dir f))))))

(deftest no-claim-delegates-one-normal-step
  (let [f (fixture) projected (atom []) calls (atom 0)
        opts (options f projected (fn [_] (swap! calls inc)
                                    {:ok true :certificate {}}))]
    (try
      (let [result (supervisor/tick! opts)]
        (is (= :advanced (:runner/status result)))
        (is (= 1 @calls)))
      (finally (delete-tree! (:dir f))))))

(deftest normal-progression-cannot-bypass-batch-permit
  (let [f (fixture) projected (atom []) calls (atom 0)
        opts (options f projected (fn [_] (swap! calls inc)
                                    {:ok true :certificate {}}))]
    (try
      (let [result (supervisor/tick!
                    (assoc opts :batch-permit
                           (assoc (:batch-permit opts) :permit/max-actions 200)))]
        (is (= :campaign-runner-batch-permit-refused (:error/code result)))
        (is (zero? @calls))
        (is (= 1 (:campaign/version
                  (:projection (ledger/read-ledger (:ledger-path f)))))))
      (finally (delete-tree! (:dir f))))))

(deftest duplicate-scheduled-trigger-is-single-fire-across-invocations
  (let [f (fixture) projected (atom []) calls (atom 0)
        opts (options f projected (fn [_] (swap! calls inc)
                                    {:ok true :certificate {}}))]
    (try
      (is (= :advanced (:runner/status (supervisor/tick! opts))))
      (let [version (:campaign/version
                     (:projection (ledger/read-ledger (:ledger-path f))))
            duplicate (supervisor/tick! opts)
            durable (:projection (ledger/read-ledger (:ledger-path f)))]
        (is (= :duplicate-trigger (:runner/status duplicate)))
        (is (= 1 @calls))
        (is (= version (:campaign/version durable)))
        (is (contains? (:campaign/consumed-triggers durable) "tick-001")))
      (finally (delete-tree! (:dir f))))))

(deftest missing-trigger-cannot-start-a-supervised-effect
  (let [f (fixture) projected (atom []) calls (atom 0)
        opts (dissoc (options f projected (fn [_] (swap! calls inc)) )
                     :trigger-id)]
    (try
      (let [result (supervisor/tick! opts)]
        (is (= :campaign-runner-trigger-required (:error/code result)))
        (is (zero? @calls))
        (is (= 1 (:campaign/version
                  (:projection (ledger/read-ledger (:ledger-path f)))))))
      (finally (delete-tree! (:dir f))))))

(deftest concurrent-delivery-of-one-trigger-runs-one-effect
  (let [f (fixture) projected (atom []) calls (atom 0)
        opts (options f projected (fn [_] (swap! calls inc)
                                    {:ok true :certificate {}}))]
    (try
      (let [start (promise)
            invoke #(future @start (supervisor/tick! opts))
            a (invoke) b (invoke)
            _ (deliver start true)
            results [@a @b]]
        (is (= #{:advanced :duplicate-trigger}
               (set (map :runner/status results))) (pr-str results))
        (is (= 1 @calls))
        (is (= 3 (:campaign/version
                  (:projection (ledger/read-ledger (:ledger-path f)))))))
      (finally (delete-tree! (:dir f))))))
