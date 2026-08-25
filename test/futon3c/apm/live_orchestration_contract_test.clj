(ns futon3c.apm.live-orchestration-contract-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.countdown-control :as control]
            [futon3c.apm.live-orchestration-contract :as sut]
            [futon3c.apm.test-support :refer [with-stubbed-qualification]]))

(def spec-path
  "holes/labs/M-apm-demonstration/countdown-live-orchestration-v1.edn")
(def manifest
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")))
(def spec (:spec (sut/read-spec spec-path)))
(def registration
  ;; Built at load time; stubbed so loading this namespace does not run Lean.
  (with-stubbed-qualification (control/registration-body)))
(def required-kinds
  (into (:administrative-kinds spec) (keys (:phase-kinds spec))))
(def complete-handlers (zipmap required-kinds (repeat (fn [_] {:ok true}))))

(deftest complete-live-orchestration-contract-is-satisfiable
  (let [result (sut/validate
                {:spec spec :registration-body registration
                 :handlers complete-handlers
                 :apparatus-artifacts (get-in manifest [:apparatus :artifacts])})]
    (is (:ok result) (pr-str (:checks result)))
    (is (empty? (:missing-handler-kinds result)))))

(deftest current-partial-controller-is-rejected-before-claim
  (let [result (sut/validate
                {:spec spec :registration-body registration
                 :handlers {:open-block identity :open-frame identity}
                 :apparatus-artifacts (get-in manifest [:apparatus :artifacts])})]
    (is (false? (:ok result)))
    (is (false? (get-in result [:checks :handlers-complete?])))
    (is (= #{:preflight :solve :verify :student-attempt
             :guide-intervention :scribe-reduce :close-frame}
           (:missing-handler-kinds result)))))

(deftest missing-timeout-terminal-or-continuation-declaration-fails
  (doseq [changed [(assoc-in spec [:phase-kinds :solve :timeouts] {})
                   (assoc-in spec [:phase-kinds :verify :terminal :states] #{:failed})
                   (assoc-in spec [:phase-kinds :close-frame :continuation :durable?]
                             false)]]
    (testing "one incomplete phase blocks launch"
      (is (false? (:ok (sut/validate
                        {:spec changed :registration-body registration
                         :handlers complete-handlers
                         :apparatus-artifacts
                         (get-in manifest [:apparatus :artifacts])})))))))
