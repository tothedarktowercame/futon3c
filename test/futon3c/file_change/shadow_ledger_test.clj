(ns futon3c.file-change.shadow-ledger-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.file-change.shadow-ledger :as ledger]))

(deftest append-is-readable-jsonl
  (let [f (doto (java.io.File/createTempFile "shadow-ledger-" ".jsonl")
            (.deleteOnExit))]
    (ledger/append! (.getPath f) {:demand/type :giraffe/full-input-recompute
                                  :emitted-at-ms 10})
    (is (= [{:demand/type "giraffe/full-input-recompute" :emitted-at-ms 10}]
           (ledger/read-records (.getPath f))))))

(deftest scorer-distinguishes-miss-covered-change-and-spurious-wake
  (let [runs [{:run-id "a" :at-ms 0 :input-sha256 "x"}
              {:run-id "b" :at-ms 10 :input-sha256 "y"}
              {:run-id "c" :at-ms 20 :input-sha256 "z"}
              {:run-id "d" :at-ms 30 :input-sha256 "z"}]
        demands [{:emitted-at-ms 15} {:emitted-at-ms 25}]
        score (ledger/score-intervals demands runs)]
    (is (= 1 (:misses score)))
    (is (= 1 (:covered-changes score)))
    (is (= 1 (:spurious-wakes score)))))
