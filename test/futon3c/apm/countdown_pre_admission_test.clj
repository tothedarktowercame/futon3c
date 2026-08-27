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

