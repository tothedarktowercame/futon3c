(ns futon3c.agents.tickle-queue-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agents.tickle-queue :as tq]))

(use-fixtures
  :each
  (fn [f]
    (tq/clear!)
    (f)
    (tq/clear!)))

;; ---------------------------------------------------------------------------
;; Bell operations
;; ---------------------------------------------------------------------------

(deftest enqueue-adds-to-pending
  (testing "enqueue! adds a bell to pending"
    (tq/enqueue! "codex-1")
    (is (= 1 (tq/pending-count)))
    (let [bell (tq/peek-pending)]
      (is (= "codex-1" (:agent-id bell)))
      (is (some? (:bell-at bell)))
      (is (some? (:nonce bell))))))

(deftest pop-pending-removes-fifo
  (testing "pop-pending! returns bells in FIFO order"
    (tq/enqueue! "codex-1")
    (tq/enqueue! "claude-3")
    (is (= 2 (tq/pending-count)))
    (let [first-bell (tq/pop-pending!)]
      (is (= "codex-1" (:agent-id first-bell)))
      (is (= 1 (tq/pending-count))))
    (let [second-bell (tq/pop-pending!)]
      (is (= "claude-3" (:agent-id second-bell)))
      (is (= 0 (tq/pending-count))))
    (is (nil? (tq/pop-pending!)))))

(deftest drain-pending-empties-queue
  (testing "drain-pending! returns all bells and empties pending"
    (tq/enqueue! "codex-1")
    (tq/enqueue! "claude-3")
    (tq/enqueue! "codex-2")
    (let [bells (tq/drain-pending!)]
      (is (= 3 (count bells)))
      (is (= ["codex-1" "claude-3" "codex-2"] (mapv :agent-id bells)))
      (is (= 0 (tq/pending-count))))))

;; ---------------------------------------------------------------------------
;; Task operations
;; ---------------------------------------------------------------------------

(deftest add-task-creates-task
  (testing "add-task! creates a task in the pool"
    (tq/add-task! {:id "T1" :label "Verify witness"})
    (is (= 1 (tq/task-count)))
    (let [t (tq/task "T1")]
      (is (= "T1" (:id t)))
      (is (= "Verify witness" (:label t)))
      (is (= :pending (:status t)))
      (is (= :normal (:priority t)))
      (is (= "manual" (:source t)))
      (is (= #{} (:depends-on t)))
      (is (nil? (:assignee t)))
      (is (some? (:created-at t))))))

(deftest pick-task-assigns-highest-priority
  (testing "pick-task! assigns highest priority available task"
    (tq/add-task! {:id "T1" :label "Low priority" :priority :low})
    (tq/add-task! {:id "T2" :label "High priority" :priority :high})
    (tq/add-task! {:id "T3" :label "Normal priority"})
    (let [picked (tq/pick-task! "codex-1")]
      (is (= "T2" (:id picked)))
      (is (= :high (:priority picked))))
    (is (= "T2" (tq/agent-assignment "codex-1")))
    (is (= :assigned (:status (tq/task "T2"))))))

(deftest pick-task-respects-dependencies
  (testing "pick-task! skips tasks with unmet dependencies"
    (tq/add-task! {:id "T1" :label "Prerequisite"})
    (tq/add-task! {:id "T2" :label "Depends on T1" :depends-on #{"T1"}})
    ;; T2 should not be pickable yet
    (let [picked (tq/pick-task! "codex-1")]
      (is (= "T1" (:id picked))))
    ;; Complete T1
    (tq/complete-task! "codex-1")
    ;; Now T2 should be available
    (let [picked (tq/pick-task! "codex-1")]
      (is (= "T2" (:id picked))))))

(deftest complete-task-moves-to-completed
  (testing "complete-task! marks task :completed and moves to history"
    (tq/add-task! {:id "T1" :label "Do work"})
    (tq/pick-task! "codex-1")
    (let [completed (tq/complete-task! "codex-1")]
      (is (= "T1" (:id completed)))
      (is (= :completed (:status completed)))
      (is (some? (:completed-at completed))))
    (is (nil? (tq/agent-assignment "codex-1")))
    (is (= :completed (:status (tq/task "T1"))))
    (let [{:keys [completed]} (tq/snapshot)]
      (is (= 1 (count completed)))
      (is (= "codex-1" (:agent-id (first completed)))))))

(deftest fail-task-requeues
  (testing "fail-task! returns task to :pending for another agent"
    (tq/add-task! {:id "T1" :label "Flaky task"})
    (tq/pick-task! "codex-1")
    (is (= :assigned (:status (tq/task "T1"))))
    (tq/fail-task! "codex-1")
    (is (= :pending (:status (tq/task "T1"))))
    (is (nil? (:assignee (tq/task "T1"))))
    (is (nil? (tq/agent-assignment "codex-1")))
    ;; Another agent can now pick it up
    (let [picked (tq/pick-task! "claude-2")]
      (is (= "T1" (:id picked))))))

(deftest available-tasks-sorted-by-priority
  (testing "available-tasks returns priority-sorted unblocked tasks"
    (tq/add-task! {:id "T1" :label "Low" :priority :low})
    (tq/add-task! {:id "T2" :label "High" :priority :high})
    (tq/add-task! {:id "T3" :label "Normal"})
    (tq/add-task! {:id "T4" :label "Blocked" :depends-on #{"T1"}})
    (let [avail (tq/available-tasks)]
      (is (= 3 (count avail)))
      (is (= ["T2" "T3" "T1"] (mapv :id avail))))))

(deftest format-queue-shows-tasks
  (testing "format-queue shows task pool and assignments"
    (tq/add-task! {:id "T1" :label "Verify witness" :priority :high})
    (tq/add-task! {:id "T2" :label "Run SAT solver"})
    (tq/pick-task! "codex-1")
    (let [output (tq/format-queue)]
      (is (string? output))
      (is (.contains output "Task Queue"))
      (is (.contains output "2 total"))
      (is (.contains output "ASSIGNED"))
      (is (.contains output "codex-1"))
      (is (.contains output "T1")))))

(deftest completed-bounded-to-max
  (testing "completed list is bounded"
    (dotimes [i 60]
      (tq/add-task! {:id (str "task-" i) :label "label"})
      (tq/pick-task! (str "agent-" i))
      (tq/complete-task! (str "agent-" i)))
    (let [{:keys [completed]} (tq/snapshot)]
      (is (<= (count completed) 50)))))

;; ---------------------------------------------------------------------------
;; Failure tracking
;; ---------------------------------------------------------------------------

(deftest enqueue-with-error-tracks-failures
  (testing "enqueue with error outcome tracks consecutive failures"
    (tq/enqueue! "codex-1" {:ok false :error "HTTP 502"})
    (let [f (tq/agent-failures "codex-1")]
      (is (= 1 (:count f)))
      (is (= 1 (:consecutive f)))
      (is (= "HTTP 502" (:last-error f)))
      (is (some? (:last-at f))))
    (is (not (tq/agent-healthy? "codex-1")))
    (is (= ["codex-1"] (tq/failing-agents)))))

(deftest enqueue-success-resets-consecutive
  (testing "successful enqueue resets consecutive failure count"
    (tq/enqueue! "codex-1" {:ok false :error "502"})
    (tq/enqueue! "codex-1" {:ok false :error "502"})
    (is (= 2 (:consecutive (tq/agent-failures "codex-1"))))
    ;; Success resets consecutive
    (tq/enqueue! "codex-1" {:ok true})
    (is (= 0 (:consecutive (tq/agent-failures "codex-1"))))
    (is (= 2 (:count (tq/agent-failures "codex-1"))))
    (is (tq/agent-healthy? "codex-1"))
    (is (empty? (tq/failing-agents)))))

(deftest enqueue-with-social-error-extracts-message
  (testing "social error maps get their message extracted"
    (tq/enqueue! "codex-1" {:ok false
                             :error {:social-error/type :invoke-error
                                     :social-error/message "Exit 1: HTTP 502"}})
    (is (= "Exit 1: HTTP 502" (:last-error (tq/agent-failures "codex-1"))))))

(deftest format-queue-shows-failing-agents
  (testing "format-queue includes FAILING section when agents have errors"
    (tq/enqueue! "codex-1" {:ok false :error "HTTP 502"})
    (tq/enqueue! "codex-1" {:ok false :error "timeout"})
    (let [output (tq/format-queue)]
      (is (.contains output "FAILING"))
      (is (.contains output "codex-1"))
      (is (.contains output "2 consecutive")))))
