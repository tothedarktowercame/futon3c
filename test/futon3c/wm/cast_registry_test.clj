(ns futon3c.wm.cast-registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.cast-registry :as cast]))

(deftest cast-roles-may-mix-supported-backends
  (let [specs [{:id "wm-author" :role "author" :backend :zai}
               {:id "wm-reviewer" :role "reviewer" :backend :claude}
               {:id "wm-repair-reviewer" :role "repair" :backend :kimi}]]
    (is (:ok (cast/validate-specs specs)))
    (is (= #{:codex :claude :zai :kimi} cast/supported-backends))
    (is (= #{"wm-author" "wm-reviewer" "wm-repair-reviewer"}
           cast/role-identities))))

(deftest malformed-casts-fail-before-registration
  (testing "role identities are distinct"
    (is (= :cast-role-identity-duplicated
           (:reason (cast/validate-specs
                     [{:id "same" :role "a" :backend :codex}
                      {:id "same" :role "b" :backend :zai}
                      {:id "c" :role "c" :backend :kimi}])))))
  (testing "backend names are closed"
    (is (= :cast-backend-unsupported
           (:reason (cast/validate-specs
                     [{:id "wm-author" :role "a" :backend :unknown}
                      {:id "wm-reviewer" :role "b" :backend :zai}
                      {:id "wm-repair-reviewer" :role "c" :backend :kimi}]))))))
