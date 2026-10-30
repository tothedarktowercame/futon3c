(ns futon3c.agents.claude-platform-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agents.claude-platform :as platform])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(defn- private-file [text]
  (let [path (Files/createTempFile
              "claude-platform-key" ""
              (into-array FileAttribute
                          [(PosixFilePermissions/asFileAttribute
                            (PosixFilePermissions/fromString "rw-------"))]))]
    (spit (str path) text)
    (str path)))

(deftest a-stateless-turn-returns-the-agency-shape-and-records-cost
  (let [key (private-file "secret\n")
        ledger (str (Files/createTempFile "claude-platform-ledger" ""
                                          (make-array FileAttribute 0)))
        seen (atom nil)
        invoke (platform/make-invoke-fn
                {:agent-id "馬-sonnet-platform" :key-file key :ledger ledger
                 :model "claude-sonnet-5-5" :max-usd 1.0
                 :call-fn (fn [request]
                            (reset! seen request)
                            {:text "[{\"act\":\"a\"}]"
                             :usage {:input_tokens 1000 :output_tokens 100}
                             :request-id "req-test"})})
        result (invoke "read this" "old-session")
        receipt (json/parse-string (slurp ledger) true)]
    (is (= "[{\"act\":\"a\"}]" (:result result)))
    (is (.startsWith (:session-id result) "platform-"))
    (is (= "secret" (:api-key @seen)))
    (is (= 0.003 (:cost-usd result)))
    (is (= "馬-sonnet-platform" (:agent-id receipt)))
    (is (= "req-test" (:request-id receipt)))))

(deftest the-named-overspend-case-is-caught-before-key-or-network-access
  (let [called? (atom false)
        invoke (platform/make-invoke-fn
                {:key-file "/does/not/exist" :model "claude-sonnet-5-5"
                 :max-tokens 4096 :max-usd 0.00001
                 :call-fn (fn [_] (reset! called? true))})
        result (invoke "a request" nil)]
    (is (nil? (:result result)))
    (is (= "Claude Platform request exceeds per-call budget" (:error result)))
    (is (false? @called?))))

(deftest the-shared-monthly-ledger-stops-a-new-constructor-before-network-access
  (let [key (private-file "secret")
        ledger (str (Files/createTempFile "claude-platform-month" ""
                                          (make-array FileAttribute 0)))
        _ (spit ledger (json/generate-string
                        {:at (str (java.time.Instant/now)) :cost-usd 0.19}))
        called? (atom false)
        invoke (platform/make-invoke-fn
                {:key-file key :ledger ledger :monthly-limit-usd 0.20
                 :max-usd 1.0 :max-tokens 2048
                 :call-fn (fn [_] (reset! called? true))})
        result (invoke "this cannot fit inside the remaining cent" nil)]
    (is (= "Claude Platform monthly budget exhausted" (:error result)))
    (is (false? @called?))))

(deftest an-empty-provider-response-is-not-published-or-charged-to-the-ledger
  (let [key (private-file "secret")
        ledger (str (Files/createTempFile "claude-platform-empty" ""
                                          (make-array FileAttribute 0)))
        invoke (platform/make-invoke-fn
                {:key-file key :ledger ledger :max-usd 1.0
                 :call-fn (constantly {:text "" :usage {:input_tokens 1
                                                          :output_tokens 0}})})
        result (invoke "x" nil)]
    (is (nil? (:result result)))
    (is (= "Claude Platform response contained no text" (:error result)))
    (is (= "" (slurp ledger)))))

(deftest an-exposed-key-file-is-refused
  (let [path (Files/createTempFile "claude-platform-exposed" ""
                                   (make-array FileAttribute 0))]
    (spit (str path) "secret")
    (Files/setPosixFilePermissions path (PosixFilePermissions/fromString "rw-r--r--"))
    (testing "the credential is rejected before it can reach a request"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"group/world accessible"
                            (platform/read-api-key (str path)))))))

(deftest reader-profiles-pin-the-reviewed-models
  (is (fn? (platform/make-reader-invoke-fn
            {:agent-id "馬-sonnet" :model "claude-sonnet-5-5"})))
  (is (fn? (platform/make-reader-invoke-fn
            {:agent-id "驴-haiku" :model "claude-haiku-5-5"})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"conflicts"
                        (platform/make-reader-invoke-fn
                         {:agent-id "驴-haiku" :model "claude-sonnet-5-5"})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No Claude Platform reader profile"
                        (platform/make-reader-invoke-fn
                         {:agent-id "claude-14"}))))
