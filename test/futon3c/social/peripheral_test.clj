(ns futon3c.social.peripheral-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.social.peripheral :as p]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

(deftest load-peripherals-loads-all
  (testing "load-peripherals loads 9 peripherals and all validate"
    (let [result (p/load-peripherals "resources/peripherals.edn")]
      (is (map? result))
      (is (contains? result :peripherals))
      (is (= 9 (count (:peripherals result))))
      (doseq [[_ spec] (:peripherals result)]
        (fix/assert-valid! shapes/PeripheralSpec spec)))))

(deftest get-peripheral-known-and-unknown
  (testing "get-peripheral returns PeripheralSpec for known id and SocialError for unknown id"
    (let [peripherals (p/load-peripherals "resources/peripherals.edn")
          explore (p/get-peripheral peripherals :explore)
          missing (p/get-peripheral peripherals :nope)]
      (fix/assert-valid! shapes/PeripheralSpec explore)
      (fix/assert-valid! shapes/SocialError missing)
      (is (= :peripheral (:error/component missing)))
      (is (= :invalid-peripheral-id (:error/code missing))))))

(deftest valid-hop-explore-to-edit
  (testing "explore -> edit succeeds (edit has :from-explore entry)"
    (let [peripherals (p/load-peripherals "resources/peripherals.edn")
          hop-req (fix/make-hop-request {:hop/to :edit
                                         :hop/reason "found target file"
                                         :hop/exit-condition :found-target})
          result (p/validate-hop peripherals :explore hop-req)]
      (fix/assert-valid! shapes/HopResult result)
      (is (= :explore (:hop/from result)))
      (is (= :edit (:hop/to result)))
      (is (true? (:hop/success? result))))))

(deftest invalid-hop-deploy-to-edit
  (testing "deploy -> edit fails (edit has no :from-deploy entry)"
    (let [peripherals (p/load-peripherals "resources/peripherals.edn")
          hop-req (fix/make-hop-request {:hop/to :edit
                                         :hop/reason "deploy complete"})
          result (p/validate-hop peripherals :deploy hop-req)]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :peripheral (:error/component result)))
      (is (= :hop-not-allowed (:error/code result))))))

(deftest transfer-context-preserves-session-id
  (testing "transfer-context always carries :session-id"
    (let [peripherals (p/load-peripherals "resources/peripherals.edn")
          edit (p/get-peripheral peripherals :edit)
          hop-result (fix/make-hop-result {:hop/from :explore
                                           :hop/to :edit
                                           :hop/session-id "sess-123"})
          ctx (p/transfer-context hop-result {:session-id "sess-old" :target-files ["src/a.clj"]} edit)]
      (is (= "sess-123" (:session-id ctx))))))

(deftest user-request-entry-is-accepted
  (testing "target peripheral with :user-request entry accepts hop (edit entry includes :user-request)"
    (let [peripherals (p/load-peripherals "resources/peripherals.edn")
          hop-req (fix/make-hop-request {:hop/to :edit
                                         :hop/reason "user request"
                                         :hop/exit-condition :user-request})
          result (p/validate-hop peripherals :test hop-req)]
      (fix/assert-valid! shapes/HopResult result)
      (is (= :test (:hop/from result)))
      (is (= :edit (:hop/to result))))))

(deftest outputs-are-shape-validated
  (testing "validate-hop returns HopResult or SocialError"
    (let [peripherals (p/load-peripherals "resources/peripherals.edn")
          good (p/validate-hop peripherals :explore (fix/make-hop-request {:hop/to :edit
                                                                          :hop/reason "found target file"
                                                                          :hop/exit-condition :found-target}))
          bad (p/validate-hop peripherals :deploy (fix/make-hop-request {:hop/to :edit
                                                                         :hop/reason "deploy complete"}))
          invalid (p/validate-hop peripherals :explore {:hop/to :edit})]
      (is (shapes/valid? shapes/HopResult good))
      (is (shapes/valid? shapes/SocialError bad))
      (is (shapes/valid? shapes/SocialError invalid)))))
