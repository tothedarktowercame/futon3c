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

(deftest probe-scorer-attributes-by-trigger-not-delayed-emission
  (let [probes [{:at-ms 10 :fresh? true}
                {:at-ms 20 :fresh? false}
                {:at-ms 30 :fresh? true}]
        demands [{:first-at-ms 19 :emitted-at-ms 21}]
        score (ledger/score-probes demands probes)]
    (is (= 0 (:misses score)))
    (is (= 1 (:covered-stale score)))
    (is (= 0 (:spurious-wakes score)))))
