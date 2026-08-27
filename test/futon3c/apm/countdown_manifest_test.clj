(ns futon3c.apm.countdown-manifest-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.countdown-manifest :as sut]))

(def manifest-path
  "holes/labs/M-apm-demonstration/countdown-10-manifest-v1.edn")

(defn load-manifest [] (edn/read-string (slurp manifest-path)))

(defn load-manifest-v2 []
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")))

(deftest qualification-checkout-is-revision-addressed-beside-repository
  (is (= "/home/joe/code/apm-frames/qualification/rev-123"
         (sut/qualification-checkout-path "/home/joe/code/apm-lean"
                                          "rev-123"))))

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

(deftest solved-problem-cannot-be-filed-as-an-eligible-unit
  (let [manifest (load-manifest-v2)
        solved (get-in (load-manifest) [:units 1])
        changed (-> manifest
                    (assoc-in [:units 1 :problem/id] (:problem/id solved))
                    (assoc-in [:units 1 :problem] (:problem solved))
                    (assoc :manifest/id "temporarily-unaddressed"))
        changed (assoc changed :manifest/id
                       (machine/ledger-digest [(dissoc changed :manifest/id)]))
        result (sut/validate changed)]
    (is (some #{:countdown-manifest-eligibility-observation-invalid}
              (:findings result)))
    (is (= 0 (get-in result [:eligibility-observations 1
                             :observation :sorry-warnings])))))

(deftest eligibility-baseline-is-semantic-not-style-warning-exact
  (let [baseline {:exit 0 :warnings 20 :sorry-warnings 2 :errors 0}
        observed {:exit 0 :warnings 38 :sorry-warnings 2 :errors 0
                  :blocking-warnings 0}]
    (is (sut/eligibility-observation-valid? baseline observed))
    (is (not (sut/eligibility-observation-valid?
              baseline (assoc observed :sorry-warnings 1))))
    (is (not (sut/eligibility-observation-valid?
              baseline (assoc observed :blocking-warnings 1))))))
