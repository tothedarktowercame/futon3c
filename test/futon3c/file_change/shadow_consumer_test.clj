(ns futon3c.file-change.shadow-consumer-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.file-change.journal :as journal]
            [futon3c.file-change.shadow-consumer :as shadow]))

(defn authority [& _] {:run-id "g-1" :fresh? false
                    :computed-sha256 "current123" :run-sha256 "old123"})

(deftest startup-rebuilds-then-retains-a-due-demand-without-dispatch
  (let [j (atom (journal/empty-state 8))
        c (atom (shadow/empty-state 10 30 8))]
    (shadow/run-once! j c {:now-ms 0 :authority-read-fn authority})
    (is (false? (get-in @c [:outcomes 0 :fresh?])))
    (is (= "current123" (get-in @c [:outcomes 0 :computed-sha256])))
    (swap! j #(first (journal/observe % {:repo "r" :worktree "/r"
                                         :path "holes/missions/M-one.md" :content-id "h1"
                                         :operation :write :observed-at-ms 1})))
    (shadow/run-once! j c {:now-ms 1 :authority-read-fn authority})
    (let [health (shadow/run-once! j c {:now-ms 11 :authority-read-fn authority})]
      (is (= :shadow (:status health)))
      (is (false? (:dispatch-enabled? health)))
      (is (= :giraffe/full-input-recompute
             (get-in health [:last-demand :demand/type])))
      (is (= :consumed (get-in @c [:outcomes 1 :outcome]))))))

(deftest change-during-authority-read-is-consumed-after-rebuild
  (let [j (atom (journal/empty-state 8))
        c (atom (shadow/empty-state 10 30 8))
        read-authority (fn [_]
                         (swap! j #(first (journal/observe
                                           % {:repo "r" :worktree "/r"
                                              :path "holes/tickets/T-during.md"
                                              :content-id "h1" :operation :write
                                              :observed-at-ms 1})))
                         (authority))]
    (shadow/run-once! j c {:now-ms 0 :authority-read-fn read-authority})
    (is (= [:rebuilt :consumed] (mapv :outcome (:outcomes @c))))
    (is (= 0 (get-in (journal/health @j 1) [:consumers :giraffe-shadow :lag])))))

(deftest unavailable-authority-remains-held-and-rebuild-required
  (let [j (atom (journal/empty-state 4))
        c (atom (shadow/empty-state 10 30 4))
        health (shadow/run-once! j c {:now-ms 0 :authority-read-fn (constantly nil)})]
    (is (= :held (get-in health [:last-outcome :outcome])))
    (is (true? (get-in health [:consumer :rebuild-required?])))
    (is (empty? (:demands @c)))))

(deftest failed-durable-sink-leaves-due-demand-pending-for-retry
  (let [j (atom (journal/empty-state 8))
        c (atom (shadow/empty-state 10 30 8))]
    (shadow/run-once! j c {:now-ms 0 :authority-read-fn authority})
    (is (thrown? Exception
                 (shadow/run-once! j c
                                   {:now-ms 10 :authority-read-fn authority
                                    :demand-sink-fn
                                    (fn [_] (throw (Exception. "disk full")))})))
    (let [written (atom [])]
      (shadow/run-once! j c {:now-ms 11 :authority-read-fn authority
                             :demand-sink-fn #(swap! written conj %)})
      (is (= 1 (count @written)))
      (is (= 11 (:emitted-at-ms (first @written)))))))

(deftest rebuild-and-due-demand-share-one-authority-result
  (let [j (atom (journal/empty-state 8))
        c (atom (assoc-in (shadow/empty-state 10 30 8)
                          [:projector :giraffe-pending]
                          {:first-at-ms 0 :last-at-ms 0 :changed-paths []
                           :level-jump? true :trigger-basis :restart}))
        authority-calls (atom 0)
        written (atom [])]
    (shadow/run-once! j c {:now-ms 30
                           :authority-read-fn
                           (fn [_] (swap! authority-calls inc) (authority))
                           :demand-sink-fn #(swap! written conj %)})
    (is (= 1 @authority-calls))
    (is (= 1 (count @written)))
    (is (empty? (filter #(= :refused (:outcome %)) (:outcomes @c))))))

(deftest repeated-held-state-is-coalesced
  (let [c (atom (shadow/empty-state 10 30 8))]
    (dotimes [n 100]
      (shadow/record-held! c n :giraffe-freshness-pending))
    (is (= 1 (count (:outcomes @c))))
    (is (= 100 (get-in @c [:outcomes 0 :count])))
    (is (= 99 (get-in @c [:outcomes 0 :last-at-ms])))))
