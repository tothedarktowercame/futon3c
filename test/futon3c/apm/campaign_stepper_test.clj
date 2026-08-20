(ns futon3c.apm.campaign-stepper-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-stepper :as stepper])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(def now (Instant/parse "2026-08-20T16:00:00Z"))

(defn temp-dir []
  (Files/createTempDirectory "campaign-stepper-" (make-array FileAttribute 0)))

(defn delete-tree! [^Path dir]
  (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [path (reverse (sort-by #(.getNameCount ^Path %)
                                  (iterator-seq (.iterator paths))))]
      (Files/deleteIfExists path))))

(defn fixture []
  (let [dir (temp-dir) path (.resolve dir "ledger.edn")
        initial (machine/projection [])
        registered {:event/id "registered" :event/seq 0
                    :event/type :campaign/registered
                    :event/campaign-id "apm-countdown"
                    :event/actor "seed" :event/at (str now)
                    :event/expected-version 0
                    :event/body
                    {:manifest-hash "frame-18-manifest"
                     :phase-order [:preflight :close]
                     :block-plan [{:block-id "countdown" :ordinal 1 :units []}]
                     :obligation-plan {:preflight {:kind :preflight}
                                       :close {:kind :close}}
                     :claims-required? true}}]
    (ledger/compare-and-append! path 0 (:ledger/digest initial) registered)
    {:dir dir :ledger-path path
     :certificate-directory (.resolve dir "certificates")
     :projection-directory (.resolve dir "projection")}))

(defn options [f gates calls]
  (merge f
         {:observation-fn (fn [_] {:binding-response {:ok true :bound? false}
                                   :jobs-response {:ok true :jobs []}})
          :now-fn (fn [] now) :project-fn identity
          :gate-provider (fn [_] gates)
          :handlers {:open-block (fn [_] (swap! calls inc)
                                   {:ok true :certificate {:opened true}})}
          :actor "frame-18-stepper"}))

(def passing-gates
  [{:gate/id :seat-turn-budget
    :gate/status :pass
    :gate/evidence {:solver-minutes 60 :student-minutes 60}}
   {:gate/id :runtime-coherent
    :gate/status :pass :gate/evidence {:snapshot :valid}}])

(deftest inspection-never-executes-and-failed-baseline-blocks
  (let [f (fixture) calls (atom 0)]
    (try
      (let [gates (assoc-in passing-gates [0 :gate/status] :fail)
            result (stepper/inspect! (options f gates calls))]
        (is (= :blocked (:stepper/status result)))
        (is (= [:seat-turn-budget] (mapv :gate/id (:failed-gates result))))
        (is (zero? @calls))
        (is (= 1 (:campaign/version
                  (:projection (ledger/read-ledger (:ledger-path f)))))))
      (finally (delete-tree! (:dir f))))))

(deftest ready-report-permits-exactly-one-step
  (let [f (fixture) calls (atom 0) opts (options f passing-gates calls)]
    (try
      (let [inspection (stepper/inspect! opts)
            issued (stepper/issue-permit
                    {:report (:report inspection) :issuer "joe"
                     :issued-at (str now)})
            permit (:permit issued)
            result (stepper/step!
                    (assoc opts :permit permit
                           :trusted-permit-id (:permit/id permit)
                           :trusted-issuer "joe"))]
        (is (= :ready (:stepper/status inspection)))
        (is (:ok issued))
        (is (= :advanced (:stepper/status result)))
        (is (= 1 @calls))
        (is (= "countdown"
               (:active/block (get-in result [:post-checkpoint :certificate])))))
      (finally (delete-tree! (:dir f))))))

(deftest failed-postcondition-stops-after-visible-durable-effect
  (let [f (fixture) calls (atom 0)
        opts (assoc (options f passing-gates calls)
                    :postcondition-fn
                    (fn [_] {:ok false :failed #{:active-block-mismatch}}))]
    (try
      (let [inspection (stepper/inspect! opts)
            permit (:permit (stepper/issue-permit
                             {:report (:report inspection) :issuer "joe"
                              :issued-at (str now)}))
            result (stepper/step!
                    (assoc opts :permit permit
                           :trusted-permit-id (:permit/id permit)
                           :trusted-issuer "joe"))]
        (is (= :campaign-stepper-postcondition-failed (:error/code result)))
        (is (= :stop (:stepper/status result)))
        (is (= 1 @calls))
        (is (= "countdown"
               (:active/block (get-in result [:post-checkpoint :certificate])))))
      (finally (delete-tree! (:dir f))))))

(deftest changed-gate-evidence-makes-permit-stale
  (let [f (fixture) calls (atom 0) gate-state (atom passing-gates)
        opts (assoc (options f passing-gates calls)
                    :gate-provider (fn [_] @gate-state))]
    (try
      (let [inspection (stepper/inspect! opts)
            permit (:permit (stepper/issue-permit
                             {:report (:report inspection) :issuer "joe"
                              :issued-at (str now)}))]
        (swap! gate-state assoc-in [0 :gate/evidence :solver-minutes] 5)
        (let [result (stepper/step!
                      (assoc opts :permit permit
                             :trusted-permit-id (:permit/id permit)
                             :trusted-issuer "joe"))]
          (is (= :campaign-stepper-permit-stale (:error/code result)))
          (is (zero? @calls))))
      (finally (delete-tree! (:dir f))))))

(deftest elapsed-clock-time-alone-does-not-stale-a-step-permit
  (let [f (fixture) calls (atom 0)
        clock (atom now)
        opts (assoc (options f passing-gates calls)
                    :now-fn (fn [] @clock))]
    (try
      (let [inspection (stepper/inspect! opts)
            permit (:permit (stepper/issue-permit
                             {:report (:report inspection) :issuer "joe"
                              :issued-at (str now)}))]
        (swap! clock #(.plusSeconds ^Instant % 30))
        (let [result (stepper/step!
                      (assoc opts :permit permit
                             :trusted-permit-id (:permit/id permit)
                             :trusted-issuer "joe"))]
          (is (= :advanced (:stepper/status result)))
          (is (= 1 @calls))))
      (finally (delete-tree! (:dir f))))))
