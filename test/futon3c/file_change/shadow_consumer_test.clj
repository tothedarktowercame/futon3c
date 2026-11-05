(ns futon3c.file-change.shadow-consumer-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.file-change.journal :as journal]
            [futon3c.file-change.shadow-consumer :as shadow]))

(defn authority [] {:run-id "g-1" :input-sha256 "abc123"})

(deftest startup-rebuilds-then-retains-a-due-demand-without-dispatch
  (let [j (atom (journal/empty-state 8))
        c (atom (shadow/empty-state 10 30 8))]
    (shadow/run-once! j c {:now-ms 0 :authority-read-fn authority})
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
        read-authority (fn []
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
