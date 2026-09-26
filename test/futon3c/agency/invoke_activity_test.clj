(ns futon3c.agency.invoke-activity-test
  "An invoking lane's activity string must carry its own age.

  2026-08-03: three codex lanes past the 35-minute soft cap read
  `invoke-activity \"using bash\"` with `last-active` stamped BEFORE their jobs
  started, and were reported wedged. They were working — accruing CPU on live
  keepalive'd sockets. Nothing in the projection could tell `using bash (3s
  ago)` from `using bash (51m ago)`, so job wall-clock age was the only signal
  left, and that is an SLA number, not evidence of stuckness."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.test-fixtures :as fix]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (binding [reg/*enable-hop-event-emission?* false]
      (f))))

(defn- register! [name*]
  (reg/register-agent!
   {:agent-id (fix/make-agent-id name*)
    :type :codex
    :invoke-fn (fn [_p _s] {:result "ok" :session-id nil :exit-code 0})
    :capabilities [:edit]}))

(defn- roster-entry [name*]
  (get (:agents (reg/registry-status)) name*))

(deftest activity-is-stamped-with-a-time
  (register! "act-1")
  (reg/update-invoke-activity! "act-1" "using bash")
  (let [e (roster-entry "act-1")]
    (testing "the string itself is still surfaced"
      (is (= "using bash" (:invoke-activity e))))
    (testing "and it now carries when it was observed"
      (is (some? (:invoke-activity-at e))))
    (testing "with a derived age, so a reader need not do the arithmetic"
      (is (number? (:invoke-quiet-ms e)))
      (is (< (long (:invoke-quiet-ms e)) 60000)
          "a just-observed lane reads as quiet for ~0ms, not silent"))))

(deftest a-later-activity-refreshes-the-stamp
  ;; The whole point: a lane that keeps reporting stays visibly alive.
  (register! "act-2")
  (reg/update-invoke-activity! "act-2" "thinking")
  (let [first-at (:invoke-activity-at (roster-entry "act-2"))]
    (Thread/sleep 25)
    (reg/update-invoke-activity! "act-2" "using bash")
    (let [e (roster-entry "act-2")]
      (is (= "using bash" (:invoke-activity e)))
      (is (not= first-at (:invoke-activity-at e))
          "a fresh observation must move the clock, or staleness is unreadable"))))

(deftest quiet-lane-is-distinguishable-from-a-busy-one
  (register! "act-3")
  (register! "act-4")
  (reg/update-invoke-activity! "act-3" "using bash")
  (Thread/sleep 60)
  (reg/update-invoke-activity! "act-4" "using bash")
  (let [older (:invoke-quiet-ms (roster-entry "act-3"))
        newer (:invoke-quiet-ms (roster-entry "act-4"))]
    (is (> (long older) (long newer))
        "identical activity strings must be separable by age — the field that
         would have prevented the wedged-lane misdiagnosis")))

(deftest going-idle-clears-the-stamp-with-the-activity
  (register! "act-5")
  (reg/update-invoke-activity! "act-5" "using bash")
  (is (some? (:invoke-activity-at (roster-entry "act-5"))))
  (reg/mark-agent-idle! "act-5")
  (let [e (roster-entry "act-5")]
    (is (nil? (:invoke-activity e)))
    (is (nil? (:invoke-activity-at e))
        "a stale stamp on an idle agent would be worse than none")))

(deftest activity-on-an-unregistered-agent-is-a-no-op
  (is (some? (reg/update-invoke-activity! "never-registered" "using bash")))
  (is (nil? (roster-entry "never-registered"))))

(deftest authoritative-activity-update-pushes-to-owned-invoke-stream
  (register! "act-stream")
  (let [events (atom [])]
    (reg/set-invoke-event-sink! "act-stream" #(swap! events conj %))
    (reg/update-invoke-activity! "act-stream" "using bash")
    (is (= [{:type "invoke.activity"
             :agent-id "act-stream"
             :activity "using bash"}]
           (mapv #(dissoc % :at) @events)))
    (is (string? (:at (first @events))))))
