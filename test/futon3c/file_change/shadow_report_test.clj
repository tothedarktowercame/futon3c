(ns futon3c.file-change.shadow-report-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.file-change.shadow-ledger :as ledger]
            [futon3c.file-change.shadow-report :as report]))

(defn- temp-dir []
  (let [f (java.io.File/createTempFile "shadow-report-" "")]
    (.delete f) (.mkdirs f) (.deleteOnExit f) f))

(deftest no-demand-report-is-typed-not-a-zero-score
  (let [dir (temp-dir)
        logs (doto (io/file dir "logs") .mkdirs)
        demands (io/file dir "absent.jsonl")]
    (spit (io/file logs "click-001-10.log")
          "giraffe field  fresh (run-a)\n")
    (let [r (report/build-report {:demand-path (.getPath demands)
                                  :click-log-root (.getPath logs)})]
      (is (= :no-demands (:status r)))
      (is (= 1 (:probe-count r)))
      (is (not (contains? r :misses))))))

(deftest report-scores-and-round-trips-as-edn
  (let [dir (temp-dir)
        logs (doto (io/file dir "logs") .mkdirs)
        demands (io/file dir "demands.jsonl")
        output (io/file dir "report.edn")]
    (spit (io/file logs "click-001-10.log")
          "giraffe field  fresh (run-a)\n")
    (spit (io/file logs "click-002-20.log")
          "giraffe field  stale (newest run-a) -- minting\nFINDING: mint failed (exit 1)\n")
    (ledger/append! (.getPath demands) {:first-at-ms 15000})
    (let [r (report/build-report {:demand-path (.getPath demands)
                                  :click-log-root (.getPath logs)
                                  :ambiguity-ms 0})]
      (report/write-report! (.getPath output) r)
      (is (= :scored (:status r)))
      (is (= 1 (get-in r [:probe-verdicts :mint-failed])))
      (is (= r (report/read-report (.getPath output)))))))

(deftest history-before-shadow-is-not-scored-and-new-demand-awaits-a-probe
  (let [dir (temp-dir)
        logs (doto (io/file dir "logs") .mkdirs)
        demands (io/file dir "demands.jsonl")]
    (spit (io/file logs "click-001-10.log")
          "giraffe field  stale (newest old) -- minting\ngiraffe field  minted old-2 (ok)\n")
    (ledger/append! (.getPath demands) {:first-at-ms 15000})
    (let [r (report/build-report {:demand-path (.getPath demands)
                                  :click-log-root (.getPath logs)
                                  :ambiguity-ms 0})]
      (is (= :awaiting-probe (:status r)))
      (is (= 1 (:awaiting-probe r)))
      (is (= 0 (:misses r)))
      (is (= 1 (:scoring-probe-count r))))))
