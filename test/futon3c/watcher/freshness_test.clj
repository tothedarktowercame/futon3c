(ns futon3c.watcher.freshness-test
  "D7a substrate-2 commit-freshness alarm — pure-logic tests
   (injectable head/idx/epoch fns; no git or live store needed)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.freshness :as f]))

(deftest commit-ingest-off-is-violation
  (testing "commit-ingest OFF short-circuits to :violation — the exact freeze cause"
    (let [o (f/check [{:path "/x" :label "x-d"}] false)]
      (is (= :violation (:outcome o)))
      (is (= :commit-ingest-off (get-in o [:detail :reason]))))))

(deftest staleness-flags-only-genuinely-stale
  (let [now    1000000000000
        roots  [{:path "/cur"  :label "cur-d"}    ;; HEAD in store → current
                {:path "/old"  :label "old-d"}    ;; HEAD not in store, old → STALE
                {:path "/new"  :label "new-d"}    ;; HEAD not in store, recent → grace
                {:path "/gone" :label "gone-d"}]  ;; no git head → skip (no false alarm)
        head-fn      {"/cur" "sha-cur" "/old" "sha-old" "/new" "sha-new" "/gone" nil}
        in-store?-fn #{"sha-cur"}                 ;; only cur's HEAD is in substrate-2
        epoch-fn (fn [path _sha]
                   (case path
                     "/old" (- now (* 60 60 1000))  ;; 60 min old  > 30 min threshold → stale
                     "/new" (- now (* 60 1000))     ;; 1 min old    < threshold      → grace
                     nil))
        stale (f/staleness roots {:now-ms now :head-fn head-fn
                                  :in-store?-fn in-store?-fn :epoch-fn epoch-fn})]
    (testing "only the not-in-store old-HEAD repo is flagged"
      (is (= 1 (count stale)))
      (is (= "old-d" (:label (first stale))))
      (is (= :head-not-in-store (:reason (first stale)))))))

(deftest check-ok-with-no-stale
  (testing "commit-ingest on + no stale repos → :ok"
    (is (= :ok (:outcome (f/check [] true))))))
