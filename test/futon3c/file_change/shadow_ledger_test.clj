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
        score (ledger/score-probes demands probes {:ambiguity-ms 0})]
    (is (= 0 (:misses score)))
    (is (= 1 (:covered-stale score)))
    (is (= 0 (:spurious-wakes score)))))

(deftest repeated-unminted-stale-probes-share-the-last-fresh-window
  (let [probes [{:at-ms 10 :fresh? true}
                {:at-ms 20 :fresh? false :minted? false}
                {:at-ms 30 :fresh? false :minted? false}]
        score (ledger/score-probes [{:first-at-ms 15 :emitted-at-ms 21}] probes
                                   {:ambiguity-ms 0})]
    (is (= 0 (:misses score)))
    (is (= 2 (:covered-stale score)))))

(deftest failed-probe-is-unscored-and-does-not-drop-demand
  (let [probes [{:at-ms 10 :fresh? true}
                {:at-ms 20 :fresh? nil}
                {:at-ms 30 :fresh? false}]
        score (ledger/score-probes [{:first-at-ms 15}] probes
                                   {:ambiguity-ms 0})]
    (is (= 1 (:unscored score)))
    (is (= 0 (:misses score)))
    (is (= 1 (:covered-stale score)))))

(deftest probe-duration-band-is-ambiguous-not-evidence
  (let [score (ledger/score-probes [{:first-at-ms 150}]
                                   [{:at-ms 100 :fresh? true}
                                    {:at-ms 300 :fresh? false}]
                                   {:ambiguity-ms 90})]
    (is (= 1 (:ambiguous score)))
    (is (= 0 (:misses score)))
    (is (= 0 (:spurious-wakes score)))))
