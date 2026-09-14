(ns futon3c.agents.chip-board-live-test
  "Pure tests for the live aggregation layer (inbox-zero-board-live).
  No state.edn is loaded; records are synthetic."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.inbox-zero-board :as izb]
            [futon3c.agents.inbox-zero-board-live :as live])
  (:import [java.time Instant]))

(defn- obs
  [repo path status hours-ago]
    {:record/type :inbox-zero/file-observation
     :path path
     :repo/root (str "/home/joe/code/" repo)
     :git/status status
     :observed-at (.minusSeconds (Instant/now) (long (* 3600 hours-ago)))})

(def now (Instant/now))

(deftest sweep-aggregation
  (testing "a modified doc file older than 24h flags its repo"
    (let [rows (live/sweep-from-records
                {:a (obs "futon3c" "holes/x.md" :modified 30)} now)]
      (is (= 1 (count rows)))
      (is (= "futon3c" (:repo (first rows))))
      (is (not (:clean? (first rows))))
      (is (= [:dirty-older-than-24h] (:clauses-failed (first rows))))))
  (testing "build output is ignored by design, not silently"
    (let [rows (live/sweep-from-records
                {:a (obs "futon2" "target/classes/a.clj" :untracked 100)} now)]
      (is (:clean? (first rows)))))
  (testing "recent WIP is clean — the definition permits a turn's work"
    (let [rows (live/sweep-from-records
                {:a (obs "futon3c" "holes/y.md" :modified 2)} now)]
      (is (:clean? (first rows))))))

(deftest in-flight-channel
  (testing "a repo observed inside the window is in-flight"
    (let [records {:a (obs "futon3c" "holes/z.md" :modified 0.5)}]
      (is (contains? (live/in-flight-from-records records now) "futon3c"))))
  (testing "quiet repos are idle"
    (let [records {:a (obs "futon3c" "holes/z.md" :modified 30)}]
      (is (empty? (live/in-flight-from-records records now))))))

(deftest end-to-end-u59-guard-on-live-data
  (testing "dirty-and-in-flight => report, no commit; dirty-and-idle => commit proposal"
    (let [dirty-old (obs "futon3c" "holes/w.md" :modified 40)
          live-now (obs "futon3c" "holes/w.md" :modified 0.2)
          recs {:a dirty-old :b live-now}
          recs-idle {:a dirty-old}
          run-live-case (izb/run (izb/observation-packet
                                  (live/sweep-from-records recs now)
                                  (live/in-flight-from-records recs now)
                                  false)
                                 (fn [_] nil))
          run-idle-case (izb/run (izb/observation-packet
                                  (live/sweep-from-records recs-idle now)
                                  (live/in-flight-from-records recs-idle now)
                                  false)
                                 (fn [_] nil))
          effs (fn [r] (mapcat :effects (:trace r)))]
      (is (not-any? #(= :commit (first %)) (effs run-live-case)))
      (is (some #(= :report (first %)) (effs run-live-case)))
      (is (some #(= :commit (first %)) (effs run-idle-case))))))

(deftest clean-transition-closes-history
  (let [old (obs "futon3c" "README.md" :modified 100)
        clean (obs "futon3c" "README.md" :clean 40)
        recent (obs "futon3c" "README.md" :modified 2)]
    (is (:clean? (first (live/sweep-from-records {:a old :b clean} now))))
    (is (:clean? (first (live/sweep-from-records {:a old :b clean :c recent} now))))
    (is (not (:clean? (first (live/sweep-from-records {:a old :c recent} now)))))))

(deftest deleted-and-renamed-dirt-counts
  (doseq [status [:deleted :renamed]]
    (is (not (:clean? (first (live/sweep-from-records
                              {:a (obs "futon3c" "README.md" status 40)} now)))))))

(deftest histories-do-not-cross-worktrees
  (let [a (assoc (obs "futon3c" "README.md" :modified 100) :worktree/id "a")
        b (assoc (obs "futon3c" "README.md" :clean 2) :worktree/id "b")]
    (is (not (:clean? (first (live/sweep-from-records {:a a :b b} now)))))))
