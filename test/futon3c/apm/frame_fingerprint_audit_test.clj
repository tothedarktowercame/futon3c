(ns futon3c.apm.frame-fingerprint-audit-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.frame-fingerprint-audit :as sut])
  (:import [java.nio.file Files]))

(defn- temp-campaign []
  (str (Files/createTempDirectory
        "fingerprint-campaigns-"
        (make-array java.nio.file.attribute.FileAttribute 0))
       "/campaign-1"))

(defn- temp-named-campaign [campaign]
  (str (Files/createTempDirectory
        "fingerprint-replay-"
        (make-array java.nio.file.attribute.FileAttribute 0))
       "/" campaign))

(def f59-fixture-path
  "holes/labs/M-apm-demonstration/f59-post-f58-boundary-fixture.edn")

(defn- read-f59-fixture []
  (edn/read-string (slurp f59-fixture-path)))

(deftest run-publishes-structured-per-use-evidence
  (let [campaign (temp-campaign)
        calls (atom [])
        payload {:campaign "campaign-1"
                 :summary {:use-events 1}
                 :rows [{:frame "f1" :attempt 2 :memory "e-memory-1"
                         :verdict "fingerprinted" :novel-hits ["foo_bar"]}]}
        result (sut/audit!
                {:state-directory campaign
                 :now-fn (constantly "2026-08-30T00:00:00Z")
                 :run-command-fn
                 (fn [root name]
                   (swap! calls conj [root name])
                   {:exit 0 :out (json/generate-string payload) :err ""})})
        artifact (str campaign "/analysis/fingerprint-audit.json")
        status (edn/read-string
                (slurp (str campaign "/analysis/fingerprint-audit-status.edn")))]
    (is (= [[(str (.getParent (java.nio.file.Path/of campaign (make-array String 0))))
             "campaign-1"]]
           @calls))
    (is (:ok result))
    (is (= 1 (:audit/use-events result)))
    (is (= payload (json/parse-string (slurp artifact) true)))
    (is (= result status))))

(deftest failure-is-recorded-without-publishing-a-false-audit
  (doseq [[label command expected-code]
          [["command failure" (constantly {:exit 7 :out "" :err "substrate down"})
            :fingerprint-audit-command-failed]
           ["invalid output" (constantly {:exit 0 :out "{}" :err ""})
            :fingerprint-audit-invalid-output]]]
    (testing label
      (let [campaign (temp-campaign)
            result (sut/audit! {:state-directory campaign
                              :now-fn (constantly "now")
                              :run-command-fn command})]
        (is (false? (:ok result)))
        (is (= expected-code (:error/code result)))
        (is (not (.exists (java.io.File.
                           (str campaign "/analysis/fingerprint-audit.json")))))
        (is (= result
               (edn/read-string
                (slurp (str campaign
                            "/analysis/fingerprint-audit-status.edn")))))))))

(deftest f59-replay-is-deterministic-and-never-mutates-receipts
  (let [fixture (read-f59-fixture)
        campaign (temp-named-campaign
                  (get-in fixture [:fixture/fingerprint-audit-replay
                                   :campaign]))
        receipt-path (str campaign "/live/student-attempt-1.edn")
        receipt-bytes "frozen-f59-receipt\n"
        payload (:fixture/fingerprint-audit-replay fixture)
        _ (.mkdirs (io/file campaign "live"))
        _ (spit receipt-path receipt-bytes)
        run! (fn [command]
               (sut/audit! {:state-directory campaign
                            :now-fn (constantly "2026-08-30T11:30:00Z")
                            :run-command-fn command}))
        success (run! (constantly {:exit 0
                                   :out (json/generate-string payload)
                                   :err ""}))
        artifact-path (str campaign "/analysis/fingerprint-audit.json")
        artifact-before-failure (slurp artifact-path)
        failure (run! (constantly {:exit 9 :out "" :err "offline substrate"}))
        failure-status (edn/read-string
                        (slurp (str campaign
                                    "/analysis/fingerprint-audit-status.edn")))]
    (testing "the committed boundary fixture pins frozen historical evidence"
      (is (= :f59-post-f58-stop-boundary (:fixture/id fixture)))
      (is (:fixture/preserve-receipts? fixture))
      (is (= [] (get-in fixture [:fixture/frozen-artifacts 0 :used-ids])))
      (is (= 64 (count (get-in fixture
                               [:fixture/frozen-artifacts 0 :sha256])))))
    (testing "valid replay publishes every arbitrary string memory id"
      (is (:ok success))
      (is (= 2 (:audit/use-events success)))
      (is (= 2 (:audit/rows success)))
      (is (= ["e-01c38dee-a698-41e3-af98-d6700d3dd55c"
              "e-apm-promotion-named-memory-id"]
             (mapv :memory (:rows (json/parse-string artifact-before-failure
                                                     true))))))
    (testing "a later failure is explicit and preserves receipts and last good audit"
      (is (false? (:ok failure)))
      (is (= :fingerprint-audit-command-failed (:error/code failure)))
      (is (= failure failure-status))
      (is (= receipt-bytes (slurp receipt-path)))
      (is (= artifact-before-failure (slurp artifact-path))))))
