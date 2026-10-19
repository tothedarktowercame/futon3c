(ns futon3c.diagramprover.skeleton.ledger-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.skeleton.ledger :as ledger]))

(defn at [s] (str "2026-10-19T" s "Z"))

;; Gate (a): three jobs — A->B done, B->C running (unanswered), and one
;; replied operator turn to A.  Fixture shapes are the raw ledger job maps
;; (the select-keys view recent-invoke-jobs returns) and 象 [{:id :record}].
(def fixture-jobs
  [{:job-id "j-ab" :caller "agent-a" :agent-id "agent-b" :state "done"
    :created-at (at "00:00:00") :started-at (at "00:01:00") :finished-at (at "00:05:00")}
   {:job-id "j-bc" :caller "agent-b" :agent-id "agent-c" :state "running"
    :created-at (at "00:02:00") :started-at (at "00:03:00")}])

(def fixture-turns
  [{:id "turn-1" :record {:turn_id "agent-a-turn-1" :agent_id "agent-a"
                          :created_at (at "09:00:00") :reply_at (at "09:02:00")}}])

(deftest ledger->events-shape-and-order
  (let [events (ledger/ledger->events fixture-jobs fixture-turns)]
    (is (= [:register] (distinct (map :op (take 4 events)))) "lanes register first")
    (is (= ["agent-a" "agent-b" "agent-c" "joe"]
           (map :agent (filter #(= :register (:op %)) events))))
    (is (= [:ring :deliver :reply]
           (map :op (filter #(= "j-ab" (or (:id %) (:bell %) (:ref %))) events))))
    (is (= "done"
           (:state (some #(and (= :reply (:op %)) (= "j-ab" (:ref %)) %) events))))
    ;; the operator turn is a bell from joe, replied at reply_at
    (is (= {:op :ring :id "agent-a-turn-1" :from "joe" :to "agent-a" :type :request}
           (-> (filter #(= :ring (:op %)) events)
               (->> (filter #(= "agent-a-turn-1" (:id %))))
               first
               (dissoc :at :kind))))))

(deftest gate-a-three-job-window
  (testing "one open bell (the running job), no crossings, lawful"
    (let [view (ledger/dispatch-view fixture-jobs fixture-turns)]
      (is (= ["j-bc"] (:open view)) (pr-str view))
      (is (= [] (:crossings view)))
      (is (true? (:lawful? view)) (pr-str (:findings view)))
      (is (= ["joe" "agent-a" "agent-b" "agent-c"] (:lanes view)))
      (is (= 3 (count (:bells view))))
      (is (= "operator"
             (some #(and (= "agent-a-turn-1" (get % "id")) (get % "kind"))
                   (:bells view)))
          "the operator turn is a kind=operator bell")
      (is (= "done" (some #(and (= "j-ab" (get % "id")) (get % "state"))
                          (:bells view)))))))

;; Gate (b): a corrupt ledger row — the reply is timestamped BEFORE the
;; ring of the same bell.  A finding, not a crash.
(def corrupt-jobs
  [{:job-id "j-bad" :caller "agent-a" :agent-id "agent-b" :state "failed"
    :created-at (at "05:00:00") :started-at (at "05:00:30") :finished-at (at "04:00:00")}])

(deftest gate-b-reply-before-ring-is-a-finding
  (let [view (ledger/dispatch-view corrupt-jobs [])]
    (is (map? view) "no crash")
    (is (= [{:finding :reply-before-ring :ref "j-bad"}]
           (get-in view [:findings :ledger])))
    (is (false? (:lawful? view)) "the inversion marks the window unlawful")))

;; Gate (c): the JSON body shape of GET /api/alpha/diagram/dispatch.
(deftest gate-c-dispatch-response-shape
  (let [from (java.time.Instant/parse (at "00:00:00"))
        to (java.time.Instant/parse (at "23:00:00"))
        body (ledger/dispatch-response fixture-jobs fixture-turns from to)]
    (is (true? (:ok body)))
    (is (= {"from" (at "00:00:00") "to" (at "23:00:00") "hours" 23}
           (:window body)))
    (is (= ["joe" "agent-a" "agent-b" "agent-c"] (:lanes body)))
    (is (= 3 (count (:bells body))))
    (doseq [k ["id" "from" "to" "kind" "rung_at"]]
      (is (contains? (first (:bells body)) k) (str "bell carries " k)))
    (is (= ["j-bc"] (:open body)))
    (is (= [] (:crossings body)))
    (is (true? (:lawful body)))
    (is (map? (:findings body)) "findings present (may be empty maps)")
    (is (every? (comp string? key) (:findings body)) "findings keys are JSON-safe")))

;; System callers get lanes rather than being dropped or made unreadable.
(deftest system-callers-become-system-lanes
  (let [jobs [{:job-id "j1" :caller "auto-bellback" :agent-id "agent-b"
               :state "done" :created-at (at "06:00:00") :finished-at (at "06:01:00")}
              {:job-id "j2" :caller nil :agent-id "agent-b"
               :state "queued" :created-at (at "06:02:00")}]
        view (ledger/dispatch-view jobs [])]
    (is (contains? (set (:lanes view)) "system:auto-bellback"))
    (is (contains? (set (:lanes view)) "system:unknown"))
    (is (true? (:lawful? view)) (pr-str (:findings view)))))

;; Incomplete rows are dropped, not guessed at.
(deftest incomplete-rows-are-dropped
  (let [jobs [{:job-id nil :caller "a" :agent-id "b" :state "done"
               :created-at (at "07:00:00")}
              {:job-id "j-no-agent" :agent-id nil :state "done"
               :created-at (at "07:00:00")}
              {:job-id "j-no-time" :caller "a" :agent-id "b" :state "done"}]]
    (is (= [] (ledger/ledger->events jobs [])))))

(deftest a-bell-recorded-as-a-turn-is-not-a-second-bell
  ;; The 象 store records agent bells too (origin "agent", turn id = job id).
  ;; Mapped as operator turns they duplicated every bell (claude-3's review,
  ;; 2026-10-19: a live hour went unlawful on that alone).
  (let [jobs [{:job-id "j-ab" :caller "agent-a" :agent-id "agent-b" :state "done"
               :created-at (at "00:00:00") :started-at (at "00:01:00") :finished-at (at "00:05:00")}]
        turns [{:id "t-op" :record {:turn_id "agent-b-turn-1" :agent_id "agent-b" :origin "operator"
                                    :created_at (at "00:06:00") :reply_at (at "00:07:00")}}
               {:id "t-bell" :record {:turn_id "j-ab" :agent_id "agent-b" :origin "agent"
                                      :created_at (at "00:00:00")}}
               {:id "t-end" :record {:turn_id "agent-b-session-end" :agent_id "agent-b" :origin "system"
                                     :created_at (at "00:08:00")}}]
        rings (filter #(= :ring (:op %)) (ledger/ledger->events jobs turns))]
    (is (= ["j-ab" "agent-b-turn-1"] (map :id rings)) "one ring per bell, plus the operator turn")
    (is (= ["agent-a" "joe"] (map :from rings)))))
