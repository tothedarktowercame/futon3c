(ns futon3c.apm.countdown-manifest-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.countdown-manifest :as sut]))

(def manifest-path
  "holes/labs/M-apm-demonstration/countdown-10-manifest-v1.edn")

(defn load-manifest [] (edn/read-string (slurp manifest-path)))

(deftest committed-countdown-manifest-resolves-from-immutable-git-objects
  (let [result (sut/validate (load-manifest))]
    (is (:valid? result) (pr-str (:findings result)))
    (is (= 10 (count (:problem-observations result))))
    (is (every? :valid? (:problem-observations result)))
    (is (every? :valid? (:apparatus-observations result)))
    (is (false? (:worktree-head-consulted? result)))))

(deftest manifest-refuses-identity-and-classification-drift
  (let [manifest (load-manifest)
        readdress #(assoc % :manifest/id
                          (machine/ledger-digest [(dissoc % :manifest/id)]))]
    (testing "duplicate problem"
      (let [changed (readdress
                     (assoc-in manifest [:units 1 :problem/id]
                               (get-in manifest [:units 0 :problem/id])))]
        (is (some #{:countdown-manifest-problem-duplicate}
                  (:findings (sut/validate changed))))))
    (testing "topology classification"
      (let [changed (readdress
                     (assoc-in manifest [:units 1 :classification/value] :topology))]
        (is (some #{:countdown-manifest-classification-invalid}
                  (:findings (sut/validate changed))))))
    (testing "mutable or wrong blob"
      (let [changed (readdress
                     (assoc-in manifest [:units 1 :problem :blob]
                               "0000000000000000000000000000000000000000"))]
        (is (some #{:countdown-manifest-problem-pin-invalid}
                  (:findings (sut/validate changed))))))
    (testing "unaddressed edit"
      (is (some #{:countdown-manifest-content-address-invalid}
                (:findings (sut/validate (assoc manifest :series :other))))))))
