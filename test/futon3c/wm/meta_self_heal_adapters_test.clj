(ns futon3c.wm.meta-self-heal-adapters-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.shell]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.meta-self-heal-adapters :as sut]))

(def ready-agent
  {:agent/status :idle :agent/invoke-ready? true :agent/session-id nil})

(deftest readiness-is-read-only-and-names-concrete-authorities
  (let [root (.toString (java.nio.file.Files/createTempDirectory
                         "meta-adapters" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (clojure.java.shell/sh "git" "-C" root "init" "-q")
    (clojure.java.shell/sh "git" "-C" root "config" "user.email" "wm@test")
    (clojure.java.shell/sh "git" "-C" root "config" "user.name" "WM")
    (spit (str root "/seed") "seed")
    (clojure.java.shell/sh "git" "-C" root "add" "seed")
    (clojure.java.shell/sh "git" "-C" root "commit" "-qm" "seed")
    (with-redefs [reg/get-agent (fn [_] ready-agent)]
      (let [queued (atom [])
            result (sut/readiness
                    {:target-repository root
                     :meta-injury-ledger-path "/tmp/ledger.edn"
                     :meta-repair-transaction-path "/tmp/tx.edn"
                     :queue-retry-fn #(swap! queued conj %)})]
        (is (= :ready (:status result)))
        (is (= :futon3c.agency.registry/invoke-agent! (:dispatch result)))
        (is (= :queued-after-parent-release (:retry result)))
        (is (empty? @queued))))))

(deftest restored-string-status-is-ready-after-serving-jvm-restart
  (let [result (with-redefs [reg/get-agent
                             (fn [_] (assoc ready-agent :agent/status "restored"))
                             clojure.java.shell/sh
                             (fn [& args]
                               {:exit 0
                                :out (if (some #{"status"} args) "" "clean-head\n")
                                :err ""})]
                 (sut/readiness
                  {:target-repository "/home/joe/code/futon2"
                   :meta-injury-ledger-path "/tmp/ledger.edn"
                   :meta-repair-transaction-path "/tmp/tx.edn"
                   :queue-retry-fn identity}))]
    (is (= :ready (:status result)))
    (is (empty? (:defects result)))))

(deftest restored-stored-agent-uses-registered-local-invoke-authority
  (let [result (with-redefs [reg/get-agent
                             (fn [_] {:agent/status :restored
                                      :agent/invoke-fn (fn [& _])})
                             clojure.java.shell/sh
                             (fn [& args]
                               {:exit 0
                                :out (if (some #{"status"} args) "" "clean-head\n")
                                :err ""})]
                 (sut/readiness
                  {:target-repository "/home/joe/code/futon2"
                   :meta-injury-ledger-path "/tmp/ledger.edn"
                   :meta-repair-transaction-path "/tmp/tx.edn"
                   :queue-retry-fn identity}))]
    (is (= :ready (:status result)))
    (is (empty? (:defects result)))))

(deftest missing-queue-or-unready-seat-refuses-without-effects
  (with-redefs [reg/get-agent (fn [seat]
                                (assoc ready-agent :agent/status
                                       (if (= seat "wm-reviewer") :invoking :idle)))]
    (let [result (sut/readiness
                  {:target-repository "/home/joe/code/futon2"
                   :meta-injury-ledger-path "/tmp/ledger.edn"
                   :meta-repair-transaction-path "/tmp/tx.edn"})]
      (is (= :refused (:status result)))
      (is (some #{:cast-not-ready} (:defects result)))
      (is (some #{:queued-retry-authority} (:defects result))))))

(deftest agency-adapter-requires-one-machine-readable-map
  (testing "valid EDN is carried with the actual session"
    (with-redefs [reg/reset-session! (fn [_] {:ok true :old-session-id "old-session"})
                  reg/invoke-agent!
                  (fn [_ _ _]
                    {:ok true :result "{:status :succeeded :payload {:patch-sha \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}}"
                     :session-id "fresh-session" :invoke-meta {:job-id "job-1"}})]
      (is (= {:status :succeeded
              :payload {:patch-sha "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}
              :session-id "fresh-session" :job-id "job-1"}
             (sut/agency-invoke! {:seat "wm-author" :session-id "declared"
                                  :request {:phase :author}})))))
  (testing "prose cannot become repair evidence"
    (with-redefs [reg/reset-session! (fn [_] {:ok true})
                  reg/invoke-agent! (fn [& _] {:ok true :result "done" :session-id "fresh"})]
      (is (= :agency-response-not-one-edn-map
             (get-in (sut/agency-invoke! {:seat "wm-author" :request {}})
                     [:payload :reason]))))))
