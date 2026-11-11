(ns futon3c.logic.turn-coverage-test
  "M-repl-circus invariant 2 (Joe, 2026-11-11): every operator turn gets a 象
   record. Acceptance against RECORDED fixtures: claude-13's session
   ed69fb38, 2026-11-10 16:00Z to 2026-11-11 02:30Z — the window in which
   Joe counted 34 turns lost (the recorded facts say 43; see the test) when
   an Emacs display change switched capture off (futon4 1e786e2dd) — plus one
   recorded park wake for the harness exclusion."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.logic.turn-coverage :as tc])
  (:import [java.time Instant Duration]))

(defn- fixture [name]
  (let [rel (str "fixtures/turn_coverage/" name ".edn")
        f (first (filter #(.exists ^java.io.File %)
                         [(io/file "test" rel)
                          (io/file (or (System/getenv "FUTON3C_ROOT") ".")
                                   "test" rel)]))]
    (tc/load-fixture f)))

(def window (delay (fixture "claude-13-ed69fb38")))
(def wake (delay (fixture "harness-wake")))

(defn- opts [fix & [over]]
  (merge {:agents (:agents fix) :as-of (:as-of fix)} over))

(deftest window-uncovered-count
  ;; The witness case. Joe counted 34 lost turns 17:39 -> 02:00; the recorded
  ;; facts in the fixture window (16:00Z -> 02:30Z) say 43: the outage ran
  ;; past 02:00, with uncovered turns up to 02:01:39 (capture was back before
  ;; the 02:17 turn, which has a record).
  (let [uncovered (tc/uncovered (tc/db @window) (opts @window))]
    (is (= 43 (count uncovered))
        "43 recorded operator turns have no 象 record in the window")
    (is (= "2026-11-10T17:54:58.611725937Z" (:at (first uncovered)))
        "the first uncovered turn is at 17:54:58 (oldest first)")
    (is (= "2026-11-11T02:01:39.020441759Z" (:at (last uncovered))))))

(deftest turns-before-the-outage-are-covered
  (let [db (tc/db @window)
        uncovered-ids (set (map :id (tc/uncovered db (opts @window))))
        before (filter #(neg? (compare (:at %) "2026-11-10T17:38:00Z"))
                       (:turns @window))]
    (is (seq before) "the fixture has turns before 17:38")
    (is (every? #(not (uncovered-ids (:id %))) before)
        "every turn before 17:38 has a 象 record")))

(deftest park-wakes-are-not-operator-turns
  (testing "the recorded claude-13 window contains no harness turns"
    (is (empty? (:harness @window))))
  (testing "a recorded park wake, with no record, is not uncovered"
    (let [db (tc/db @wake)]
      (is (= [] (tc/uncovered db (opts @wake))))
      (is (= {:claude-17 {:turns 0 :covered 0 :uncovered 0
                          :newest-uncovered nil}}
             (tc/coverage-summary db (opts @wake)))))))

(deftest grace-window
  ;; A turn younger than grace at the as-of time is not a violation.
  (let [db (tc/db @window)
        newest (last (tc/uncovered db (opts @window)))
        at (Instant/parse (:at newest))]
    (is (not (some #(= (:id newest) (:id %))
                   (tc/uncovered db (opts @window {:as-of (str (.plus at (Duration/ofMinutes 5)))}))))
        "5 minutes after the turn: too young to be uncovered")
    (is (some #(= (:id newest) (:id %))
              (tc/uncovered db (opts @window {:as-of (str (.plus at (Duration/ofMinutes 11)))})))
        "11 minutes after the turn (grace 10): uncovered")))

(deftest summary-shape
  (let [s (tc/coverage-summary (tc/db @window) (opts @window))]
    (is (= {:claude-13 {:turns 64 :covered 21 :uncovered 43
                        :newest-uncovered "2026-11-11T02:01:39.020441759Z"}}
           s))))
