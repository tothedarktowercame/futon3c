(ns futon3c.agency.bg-process-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.bg-process :as bg]))

(defn- wait-until [pred ms]
  (let [deadline (+ (System/currentTimeMillis) (long ms))]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 25) (recur))))))

(deftest launch-captures-output-and-exits
  (let [t (bg/launch! {:cmd "echo hello-bg && echo line2"
                       :agent-id "test-agent" :label "echo"})
        id (:id t)]
    (is (string? id))
    (is (pos? (:pid t)))
    (is (= "test-agent" (:agent-id t)))
    (is (= :running (:status t)))
    (is (wait-until #(= :exited (:status (bg/status id))) 8000) "process exits")
    (is (= 0 (:exit (bg/status id))) "exit 0")
    (is (re-find #"hello-bg" (bg/tail id)) "stdout captured to the log")
    (is (re-find #"line2" (bg/tail id)))
    (bg/forget! id)))

(deftest list-filters-by-agent
  (let [a (bg/launch! {:cmd "true" :agent-id "agent-A"})
        b (bg/launch! {:cmd "true" :agent-id "agent-B"})]
    (wait-until #(and (= :exited (:status (bg/status (:id a))))
                      (= :exited (:status (bg/status (:id b))))) 8000)
    (is (some #(= (:id a) (:id %)) (bg/list-tasks "agent-A")))
    (is (not (some #(= (:id b) (:id %)) (bg/list-tasks "agent-A"))))
    (bg/forget! (:id a))
    (bg/forget! (:id b))))

(deftest kill-terminates-running
  (let [t (bg/launch! {:cmd "sleep 30" :agent-id "test-agent" :label "sleeper"})
        id (:id t)]
    (is (= :running (:status (bg/status id))))
    (bg/kill! id)
    (is (wait-until #(not (:alive? (bg/status id))) 3000) "killed process is not alive")
    (is (contains? #{:killed :exited} (:status (bg/status id))))
    (bg/forget! id)))

(deftest blank-cmd-rejected
  (is (thrown? clojure.lang.ExceptionInfo (bg/launch! {:cmd "  "}))))

(deftest child-of-jvm-not-caller
  ;; The durability property: the spawned process's parent is THIS JVM, so it is
  ;; not tied to any ephemeral pouch — it survives pouch teardown.
  (let [t (bg/launch! {:cmd "sleep 5" :agent-id "test-agent"})
        id (:id t)
        our-pid (.pid (java.lang.ProcessHandle/current))
        ph (.orElse (java.lang.ProcessHandle/of (long (:pid t))) nil)
        parent (when ph (.orElse (.parent ph) nil))
        ppid (when parent (.pid parent))]
    (is (= our-pid ppid) "spawned process is a direct child of the JVM")
    (bg/kill! id)
    (bg/forget! id)))
