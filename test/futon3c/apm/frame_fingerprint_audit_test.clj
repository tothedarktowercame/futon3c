(ns futon3c.apm.frame-fingerprint-audit-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.frame-fingerprint-audit :as sut])
  (:import [java.nio.file Files]))

(defn- temp-campaign []
  (str (Files/createTempDirectory
        "fingerprint-campaigns-"
        (make-array java.nio.file.attribute.FileAttribute 0))
       "/campaign-1"))

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
