(ns futon3c.file-change.hygiene-delivery-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.file-change.hygiene-delivery :as sut]))

(def request {:entry-id "registry-entry-7"
              :repo-root "/repo/example"
              :changed-paths ["src/a.clj"]})

(def current-result {:current? true :entry-id "registry-entry-7"
                     :checked-at "2026-11-05T00:00:00Z"})

(deftest current-bound-result-is-admitted
  (let [evidence (sut/currency-evidence request current-result)]
    (is (= :file-change/hygiene-delivery-v1 (:schema evidence)))
    (is (= sut/lean-contract (:lean-contract evidence)))
    (is (true? (:admitted? evidence)))
    (is (every? true? (vals (:checks evidence))))))

(deftest mismatched-or-refused-result-cannot-be-admitted
  (doseq [result [(assoc current-result :entry-id "other-entry")
                  {:record/type :test-registry/refusal :reason :stale-sha}]]
    (let [evidence (sut/currency-evidence request result)]
      (is (false? (:admitted? evidence)))
      (is (false? (get-in evidence [:checks :identity-checked]))))))

(deftest conjunction-fails-closed
  (let [all (zipmap sut/check-keys (repeat true))]
    (is (sut/admitted? all))
    (doseq [key sut/check-keys]
      (testing (name key)
        (is (false? (sut/admitted? (assoc all key false))))
        (is (false? (sut/admitted? (dissoc all key))))
        (is (false? (sut/admitted? (assoc all key :asserted))))))))
