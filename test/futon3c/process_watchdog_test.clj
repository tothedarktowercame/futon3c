(ns futon3c.process-watchdog-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.cyder :as cyder]
            [futon3c.process-watchdog :as sut])
  (:import [java.time Instant]))

(use-fixtures
  :each
  (fn [f]
    (reset! cyder/!processes {})
    (sut/stop!)
    (f)
    (sut/stop!)
    (reset! cyder/!processes {})))

(defn- seconds-ago [n]
  (.minusSeconds (Instant/now) (long n)))

(deftest process-problems-detects-missing-and-stuck-watcher
  (testing "missing multi-watcher is a critical problem"
    (let [problems (sut/process-problems)]
      (is (= [{:process-id "multi-watcher"
               :kind :missing
               :severity :critical
               :message "process not registered in CYDER"
               :details {}}]
             problems))))
  (testing "open cycle beyond threshold reports cycle-stuck"
    (cyder/register!
     {:id "multi-watcher"
      :type :daemon
      :stop-fn (fn [])
      :state-fn (fn []
                  {:running? true
                   :interval-ms 5000
                   :cycle-n 12
                   :event-n 3
                   :last-subtask {:phase :commit-ingest :repo "demo"}
                   :last-cycle-started-at (str (seconds-ago 40))
                   :last-cycle-finished-at (str (seconds-ago 50))})})
    (swap! cyder/!processes update "multi-watcher"
           assoc :process/last-active (seconds-ago 40))
    (let [problem (first (sut/process-problems))]
      (is (= "multi-watcher" (:process-id problem)))
      (is (= :cycle-stuck (:kind problem)))
      (is (= :critical (:severity problem)))))
  (testing "long open cycle with recent progress is not stuck"
    (cyder/register!
     {:id "multi-watcher"
      :type :daemon
      :stop-fn (fn [])
      :state-fn (fn []
                  {:running? true
                   :interval-ms 5000
                   :cycle-n 19
                   :event-n 8
                   :last-subtask {:phase :file-ingest :repo "demo"}
                   :last-cycle-started-at (str (seconds-ago 70))
                   :last-cycle-finished-at (str (seconds-ago 80))
                   :last-progress-at (str (seconds-ago 5))})})
    (swap! cyder/!processes update "multi-watcher"
           assoc :process/last-active (seconds-ago 5))
    (is (empty? (sut/process-problems)))))

(deftest run-cycle-emits-edge-triggered-alerts
  (testing "notify on new problem, then recovery once cleared"
    (let [notifications (atom [])]
      (cyder/register!
       {:id "multi-watcher"
        :type :daemon
        :stop-fn (fn [])
        :state-fn (fn []
                    {:running? false
                     :interval-ms 5000
                     :cycle-n 1
                     :event-n 0})})
      (sut/run-cycle! {:profiles sut/default-profiles
                       :evidence-store nil
                       :notify-fn #(swap! notifications conj %)})
      (sut/run-cycle! {:profiles sut/default-profiles
                       :evidence-store nil
                       :notify-fn #(swap! notifications conj %)})
      (swap! cyder/!processes update "multi-watcher"
             assoc :process/state-fn (fn []
                                       {:running? true
                                        :interval-ms 5000
                                        :cycle-n 2
                                        :event-n 0
                                        :last-cycle-started-at (str (Instant/now))
                                        :last-cycle-finished-at (str (Instant/now))
                                        :last-subtask {:phase :idle}}))
      (swap! cyder/!processes update "multi-watcher"
             assoc :process/last-active (Instant/now))
      (sut/run-cycle! {:profiles sut/default-profiles
                       :evidence-store nil
                       :notify-fn #(swap! notifications conj %)})
      (is (= 2 (count @notifications)))
      (is (= "critical" (:urgency (first @notifications))))
      (is (= "low" (:urgency (second @notifications)))))))
