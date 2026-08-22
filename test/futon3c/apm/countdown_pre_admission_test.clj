(ns futon3c.apm.countdown-pre-admission-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.countdown-pre-admission :as sut]))

(def manifest
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/countdown-10-manifest-v1.edn")))
(def contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")))
(def contract-v2
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")))

(deftest f19-does-not-repeat-frame18-close-failures
  (let [result (sut/validate {:countdown-manifest manifest
                              :cycle-contract contract :frame-id "f19"})]
    (is (:ok result) (pr-str (:checks result)))
    (is (every? true? (vals (:checks result))))
    (is (string? (:registration/hash result)))))

(deftest legacy-four-phase-and-memory-free-registrations-are-known-failing
  (testing "frame 18's four-phase shape cannot pass"
    (let [result (sut/validate
                  {:countdown-manifest manifest :frame-id "f19"
                   :cycle-contract
                   (assoc contract :phase-order
                          [:preflight :solve :verify :close-frame])})]
      (is (false? (:ok result)))
      (is (false? (get-in result [:checks :full-cycle-registered?])))))
  (testing "a close phase without memory dependencies cannot pass"
    (let [result (sut/validate
                  {:countdown-manifest manifest :frame-id "f19"
                   :cycle-contract
                   (assoc-in contract [:phases :close-frame :requires]
                             #{:verify-receipt})})]
      (is (false? (:ok result)))
      (is (false? (get-in result [:checks :close-requires-memory-path?])))))
  (testing "registration cannot silently use mutable apparatus"
    (let [result (sut/validate
                  {:countdown-manifest (assoc-in manifest [:apparatus :revision]
                                                 (apply str (repeat 40 "0")))
                   :cycle-contract contract :frame-id "f19"})]
      (is (false? (:ok result)))
      (is (false? (get-in result [:checks :apparatus-frozen?]))))))

(deftest v2-two-promotion-cycle-is-admissible-but-not-optional
  (let [manifest-check (assoc (sut/validate
                               {:countdown-manifest manifest
                                :cycle-contract contract :frame-id "f19"})
                              :valid? true)
        valid-check (:manifest-check manifest-check)
        result (sut/validate {:countdown-manifest manifest
                              :cycle-contract contract-v2 :frame-id "f19"
                              :manifest-check valid-check})]
    (is (:ok result) (pr-str (:checks result)))
    (is (true? (get-in result [:checks
                               :v2-students-require-promoted-snapshot?]))))
  (let [bad (update contract-v2 :phase-order
                    #(vec (remove #{:promote-solver} %)))
        result (sut/validate {:countdown-manifest manifest
                              :cycle-contract bad :frame-id "f19"})]
    (is (false? (:ok result)))))
