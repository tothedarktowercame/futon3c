(ns futon3c.agency.job-tree-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.job-tree :as jobs])
  (:import [java.lang ProcessHandle]))

(use-fixtures
  :each
  (fn [f]
    (jobs/clear!)
    (try (f) (finally (jobs/clear!)))))

(defn- tool-use-event [tool-id command]
  {:type "assistant"
   :message {:content [{:type "tool_use"
                        :id tool-id
                        :name "Bash"
                        :input {:command command}}]}})

(defn- tool-result-event [tool-id]
  {:type "user"
   :message {:content [{:type "tool_result"
                        :tool_use_id tool-id
                        :content "launched"}]}})

(defn- wait-for [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred) true
        (< (System/currentTimeMillis) deadline)
        (do (Thread/sleep 20) (recur))
        :else false))))

(deftest detached-process-is-attributed-before-it-reparents
  ;; This is the load-bearing case: the shell exits, sleep becomes a child of
  ;; PID 1, and after-the-fact ancestry cannot recover its agent or turn.
  (let [tool-id "tool-detached-sleep"
        root-pid (.pid (ProcessHandle/current))
        command "nohup sleep 60 >/dev/null 2>&1 &"
        common {:agent-id "claude-helper"
                :turn-id "turn-sleep-60"
                :root-pid root-pid}]
    (jobs/observe-event! (assoc common :event (tool-use-event tool-id command)))
    (let [shell (.start (ProcessBuilder. ["bash" "-c" command]))]
      (is (.waitFor shell 5000 java.util.concurrent.TimeUnit/MILLISECONDS)))
    (jobs/observe-event! (assoc common :event (tool-result-event tool-id)))
    (is (wait-for #(seq (jobs/tree-snapshot)) 5000)
        "the launch-time watcher captures the child before nohup reparents it")
    (let [job (first (jobs/tree-snapshot))]
      (is (= "claude-helper" (:job/agent-id job)))
      (is (= "turn-sleep-60" (:job/turn-id job)))
      (is (= :running (:job/status job)))
      (is (seq (:job/pids job)))
      (is (re-find #"nohup sleep 60" (:job/label job)))
      (is (re-find #"claude-helper[\s\S]*turn-sleep-60[\s\S]*running"
                   (jobs/format-tree [job])))
      (is (true? (jobs/stop-job! (:job/id job))))
      (is (wait-for #(= :stopped (:job/status (first (jobs/tree-snapshot))))
                    2000)))))

(deftest foreground-process-is-not-misclassified
  (let [tool-id "tool-foreground"
        root-pid (.pid (ProcessHandle/current))
        command "sleep 0.1"
        common {:agent-id "claude-helper"
                :turn-id "turn-foreground"
                :root-pid root-pid}]
    (jobs/observe-event! (assoc common :event (tool-use-event tool-id command)))
    (let [shell (.start (ProcessBuilder. ["bash" "-c" command]))]
      (.waitFor shell))
    (jobs/observe-event! (assoc common :event (tool-result-event tool-id)))
    (Thread/sleep 700)
    (is (empty? (jobs/tree-snapshot))
        "a foreground child that is gone when Bash returns is not a job")))

(deftest ordinary-command-does-not-claim-a-concurrent-matching-process
  ;; The global PID-window fallback exists only for explicit detachment.  An
  ;; ordinary command must not claim some other user's concurrent `sleep` just
  ;; because its command line contains the same generic token.
  (let [tool-id "tool-ordinary-sleep"
        root (.start (ProcessBuilder. ["bash" "-c" "sleep 2"]))
        command "sleep 0.1"
        common {:agent-id "claude-helper"
                :turn-id "turn-ordinary"
                :root-pid (.pid root)}]
    (try
      (jobs/observe-event! (assoc common :event (tool-use-event tool-id command)))
      (let [unrelated (.start (ProcessBuilder. ["sleep" "60"]))]
        (try
          (jobs/observe-event! (assoc common :event (tool-result-event tool-id)))
          (Thread/sleep 700)
          (is (empty? (jobs/tree-snapshot)))
          (finally (.destroyForcibly unrelated))))
      (finally (.destroyForcibly root)))))

(deftest ssh-target-parsing-keeps-the-machine-boundary-explicit
  (is (= "zone-joe"
         (#'jobs/ssh-target
          "timeout 60 ssh zone-joe 'nohup setsid sleep 60 >/tmp/x 2>&1 &'")))
  (is (= "joe@zone.example"
         (#'jobs/ssh-target
          "ssh -o BatchMode=yes -p 2222 joe@zone.example 'nohup work &'")))
  (is (nil? (#'jobs/ssh-target "ssh -Q cipher")))
  (is (nil? (#'jobs/ssh-target "printf 'ssh zone-joe'"))))

(deftest remote-launch-window-retains-the-job-not-the-ssh-launcher
  (let [command (str "ssh zone-joe 'cd /work; "
                     "nohup setsid clojure -M /tmp/inner_w256.clj &'")
        old {:pid 10 :process-start "Thu Aug  6 08:00:00 2026"
             :remote? true :remote-host "zone-joe"
             :command-line "clojure -M /tmp/unrelated.clj"}
        launched {:pid 20 :session-id 20
                  :process-start "Thu Aug  6 09:00:00 2026"
                  :remote? true :remote-host "zone-joe"
                  :command-line "java clojure.main /tmp/inner_w256.clj"}]
    (is (= [launched]
           (#'jobs/remote-candidates command [old] [old launched])))
    (is (re-find #"(?s)@zone-joe.*pid=20"
                 (jobs/format-tree
                  [{:job/id "job/claude/tool"
                    :job/agent-id "claude"
                    :job/turn-id "turn"
                    :job/status :running
                    :job/label "width 256"
                    :job/remote-host "zone-joe"
                    :job/pids [20]
                    :job/started-at (java.time.Instant/now)}])))))

(deftest remote-baseline-race-is-recovered-only-by-distinctive-token
  ;; If the remote process is born while the asynchronous baseline `ps` is in
  ;; flight, PID difference alone cannot see it.  The script path is the
  ;; launch-specific fallback; a generic old `clojure` process is not claimed.
  (let [command "ssh zone-joe 'nohup clojure -M /tmp/survival.clj &'"
        raced {:pid 30 :process-start "Thu Aug  6 09:01:00 2026"
               :remote? true :remote-host "zone-joe"
               :command-line "java clojure.main /tmp/survival.clj"}
        generic {:pid 31 :process-start "Thu Aug  6 08:01:00 2026"
                 :remote? true :remote-host "zone-joe"
                 :command-line "java clojure.main /tmp/other.clj"}
        hyphen-only {:pid 32 :process-start "Thu Aug  6 08:02:00 2026"
                     :remote? true :remote-host "zone-joe"
                     :command-line "bash restore-plus"}]
    (is (= [raced]
           (#'jobs/remote-candidates command
                                      [raced generic hyphen-only]
                                      [raced generic hyphen-only])))))

(deftest failed-here-doc-launch-does-not-claim-zone-process-table
  ;; Regression for the 2026-08-06 E0 phantom. The here-doc failed in the
  ;; Clojure reader, but broad token matching (`futon5`, `run`, `work`) claimed
  ;; hundreds of unrelated Zone processes and their sessions, so *tree*
  ;; displayed the dead launch forever.
  (let [command (str "ssh zone-joe 'cat > /tmp/e0_bridge.clj <<EOF\n"
                     "(require '[futon5.ca.core :as ca])\n"
                     "(defn rebuild-cache [run work] nil)\n"
                     "EOF\nclojure -M /tmp/e0_bridge.clj'")
        old-java {:pid 71 :session-id 71
                  :process-start "Thu Aug  6 08:00:00 2026"
                  :command-line (str "java -classpath /home/joe/code/futon5/src "
                                     "clojure.main -m futon3c.dev")}
        new-probe {:pid 72 :session-id 72
                   :process-start "Thu Aug  6 12:22:01 2026"
                   :command-line "bash futon5 run work probe"}
        kernel-lookalike {:pid 73 :session-id 0
                          :process-start "Thu Aug  6 12:22:01 2026"
                          :command-line "worker /tmp/e0_bridge.clj"}]
    (is (empty? (#'jobs/remote-candidates
                 command [old-java] [old-java new-probe kernel-lookalike])))))

(deftest artifact-identity-still-captures-a-real-remote-launch
  (let [command "ssh zone-joe 'nohup clojure -M /tmp/e0_bridge.clj &'"
        launched {:pid 80 :session-id 80
                  :process-start "Thu Aug  6 12:22:01 2026"
                  :command-line "java clojure.main /tmp/e0_bridge.clj"}]
    (is (= [launched]
           (#'jobs/remote-candidates command [] [launched])))))

(deftest remote-watch-follows-the-captured-session-not-fuzzy-command-matches
  (let [child {:pid 41 :session-id 20 :command-line "java later-stage"}
        unrelated {:pid 42 :session-id 99
                   :command-line "sleep 60 /tmp/inner_w256.clj"}]
    (is (= [child]
           (#'jobs/remote-session-processes #{20} [child unrelated])))))
