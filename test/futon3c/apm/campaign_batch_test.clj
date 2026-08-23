(ns futon3c.apm.campaign-batch-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-batch :as batch]))

(def permit
  (batch/issue
   {:campaign-id "apm-200" :manifest-hash "manifest"
    :start-version 7 :start-ledger-digest "ledger-7"
    :issuer "joe" :actor "runner" :max-actions 2
    :allowed-kinds [:open-frame :probe]
    :issued-at "2026-08-20T11:00:00Z"
    :valid-before "2026-08-20T13:00:00Z"}))

(def certificate
  {:campaign/id "apm-200" :campaign/manifest-hash "manifest"
   :campaign/version 7 :ledger/digest "ledger-7"
   :generated-at "2026-08-20T12:00:00Z"})

(def obligation {:obligation/action {:kind :open-frame}})

(defn authorize [& [overrides]]
  (batch/authorize
   (merge {:permit permit :trusted-permit-id (:permit/id permit)
           :trusted-issuer "joe" :actor "runner"
           :certificate certificate :obligation obligation :action-index 0}
          overrides)))

(deftest matching-envelope-authorizes-declared-action
  (is (:authorized? (authorize))))

(deftest content-addressing-refuses-tampering
  (is (= :campaign-batch-permit-content-invalid
         (:error/code (authorize {:permit (assoc permit :permit/max-actions 200)})))))

(deftest content-valid-malformed-permit-is-refused-without-throwing
  (let [malformed (batch/issue
                   {:campaign-id "apm-200" :manifest-hash "manifest"
                    :start-version "seven" :start-ledger-digest "ledger-7"
                    :issuer "joe" :actor "runner" :max-actions 2
                    :allowed-kinds [:open-frame]
                    :issued-at "2026-08-20T11:00:00Z"
                    :valid-before "2026-08-20T13:00:00Z"})]
    (is (= :campaign-batch-permit-shape-invalid
           (:error/code (authorize {:permit malformed
                                    :trusted-permit-id (:permit/id malformed)}))))))

(deftest authority-identity-manifest-and-start-ledger-are-pinned
  (testing "operator-approved permit identity"
    (is (= :campaign-batch-permit-id-untrusted
           (:error/code (authorize {:trusted-permit-id (apply str (repeat 64 "0"))})))))
  (testing "trusted issuer"
    (is (= :campaign-batch-permit-issuer-untrusted
           (:error/code (authorize {:trusted-issuer "other"})))))
  (testing "executing actor"
    (is (= :campaign-batch-permit-actor-mismatch
           (:error/code (authorize {:actor "other"})))))
  (testing "manifest"
    (is (= :campaign-batch-permit-manifest-mismatch
           (:error/code (authorize {:certificate
                                    (assoc certificate :campaign/manifest-hash "other")})))))
  (testing "starting fork"
    (is (= :campaign-batch-permit-start-ledger-mismatch
           (:error/code (authorize {:certificate
                                    (assoc certificate :ledger/digest "fork")}))))))

(deftest quota-kind-and-expiry-fail-closed
  (is (= :campaign-batch-permit-quota-exhausted
         (:error/code (authorize {:action-index 2}))))
  (is (= :campaign-batch-permit-action-forbidden
         (:error/code (authorize {:obligation
                                  {:obligation/action {:kind :delete-world}}}))))
  (is (= :campaign-batch-permit-expired
         (:error/code (authorize {:certificate
                                  (assoc certificate :generated-at
                                         "2026-08-20T13:00:00Z")}))))
  (is (= :campaign-batch-permit-not-yet-valid
         (:error/code (authorize {:certificate
                                  (assoc certificate :generated-at
                                         "2026-08-20T10:59:59Z")})))))

(deftest caller-action-index-must-equal-certified-ledger-usage
  (is (= :campaign-batch-permit-usage-mismatch
         (:error/code
          (authorize {:action-index 1
                      :certificate (assoc certificate :campaign/permit-usage
                                          {(:permit/id permit) 0})})))))
