(ns futon3c.agents.codex-enforcement-test
  "Tests for codex enforcement detection logic (M-codex-agent-behaviour).

   These tests validate the H-1 (Q→A completeness) and H-2 (diagnosability)
   invariants by exercising the detection predicates and enforcement prompts
   that are ported into the WS bridge.

   The predicates are defined inline here (same regexes as the bridge script)
   because the bridge script is not on the test classpath. This duplication
   is acknowledged — Option C (codex-cli adapter) will consolidate."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; ---------------------------------------------------------------------------
;; Regexes — exact copies from scripts/codex_ws_invoke_bridge.clj
;; ---------------------------------------------------------------------------

(def ^:private work-claim-re
  #"(?i)\b(i['']?ll|i will|we['']?ll|we will|claiming|i claim|taking|i(?:'m| am) taking|proceeding|starting|kicking off|working on|i(?:'m| am) on it)\b")

(def ^:private planning-only-re
  #"(?i)\b(planning-only|not started|need clarification|need more context|cannot execute yet|blocked)\b")

(def ^:private task-mode-re
  #"(?i)\bmode:\s*task\b")

(def ^:private mission-work-re
  #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b")

(def ^:private separate-message-request-re
  #"(?i)\b(separate\s+irc\s+messages?|one\s+per\s+message|each\s+of\s+them\s+in\s+a\s+separate\s+irc\s+message|post\s+each\s+.*\s+separate\s+message)\b")

(def ^:private format-excuse-re
  #"(?i)\b(surface|interface|mode|channel|irc)\b.{0,60}\b(cap|caps|limit|limited|can't|cannot|unable)\b|\b\d+\s+lines?\s+per\s+turn\b")

;; ---------------------------------------------------------------------------
;; Predicates — exact copies from scripts/codex_ws_invoke_bridge.clj
;; ---------------------------------------------------------------------------

(defn- no-execution-evidence? [result]
  (let [execution (:execution result)
        tool-events (long (or (:tool-events execution) 0))
        command-events (long (or (:command-events execution) 0))]
    (and (nil? (:error result))
         (not (:executed? execution))
         (zero? tool-events)
         (zero? command-events))))

(defn- work-claim-without-execution? [result]
  (let [text (str/trim (or (:result result) ""))]
    (and (no-execution-evidence? result)
         (not (str/blank? text))
         (boolean (re-find work-claim-re text)))))

(defn- task-reply-without-execution? [prompt result]
  (let [text (str/trim (or (:result result) ""))
        prompt-str (str (or prompt ""))]
    (and (or (boolean (re-find task-mode-re prompt-str))
             (boolean (re-find mission-work-re prompt-str)))
         (no-execution-evidence? result)
         (not (str/blank? text))
         (not (boolean (re-find planning-only-re text))))))

(defn- format-refusal? [prompt result]
  (let [text (str/trim (or (:result result) ""))
        prompt-str (str (or prompt ""))]
    (and (boolean (re-find separate-message-request-re prompt-str))
         (not (str/blank? text))
         (boolean (re-find format-excuse-re text)))))

(defn- enforcement-needed? [prompt result]
  (or (work-claim-without-execution? result)
      (task-reply-without-execution? prompt result)
      (format-refusal? prompt result)))

(defn- enforcement-reason [prompt result]
  (cond
    (work-claim-without-execution? result)       :work-claim-without-execution
    (task-reply-without-execution? prompt result) :task-reply-without-execution
    (format-refusal? prompt result)               :format-refusal
    :else                                         nil))

(defn- clip [s max-len]
  (let [txt (str (or s ""))]
    (if (<= (count txt) max-len)
      txt
      (str (subs txt 0 (max 0 (- max-len 3))) "..."))))

(defn- execution-followup-prompt [original-prompt prior-reply]
  (str "Your previous reply made a work/progress claim without execution evidence.\n"
       "Execute one concrete first step now (tool/command activity is required).\n"
       "Then reply in one short line with actual status and artifact refs.\n\n"
       "Original request:\n"
       (clip original-prompt 900)
       "\n\nPrevious reply:\n"
       (clip prior-reply 600)))

;; ---------------------------------------------------------------------------
;; Tool/command event classification — same as bridge
;; ---------------------------------------------------------------------------

(defn- tool-event? [evt]
  (let [t (:type evt)
        item (:item evt)]
    (or (= "command_execution" t)
        (and (contains? #{"item.started" "item.completed"} t)
             (= "command_execution" (:type item)))
        (and (contains? #{"item.started" "item.completed"} t)
             (= "tool_call" (:type item))))))

(defn- command-event? [evt]
  (let [t (:type evt)
        item (:item evt)
        name (:name item)]
    (or (= "command_execution" t)
        (and (contains? #{"item.started" "item.completed"} t)
             (= "command_execution" (:type item)))
        (and (contains? #{"item.started" "item.completed"} t)
             (= "tool_call" (:type item))
             (contains? #{"command_execution" "command-execution" "bash" "shell"}
                        (some-> name str/lower-case))))))

;; ===========================================================================
;; H-1 Tests: Q→A completeness — enforcement detects non-answers
;; ===========================================================================

(deftest work-claim-without-execution-detects-planning-text
  (testing "classic codex planning response triggers enforcement"
    (let [result {:ok true
                  :result "I'll prep the outgoing ArSE prompt in /tmp/arse-fm001.txt"
                  :session-id "sid-1"
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (true? (work-claim-without-execution? result)))
      (is (true? (enforcement-needed? "do the thing" result)))
      (is (= :work-claim-without-execution (enforcement-reason "do the thing" result)))))

  (testing "various work-claim phrases trigger"
    (doseq [phrase ["I'm working on it now"
                    "Proceeding to implement the changes"
                    "Starting the migration script"
                    "I will create the file and run the tests"
                    "We'll kick off the deploy"
                    "I'm taking this task"]]
      (let [result {:ok true :result phrase
                    :execution {:tool-events 0 :command-events 0 :executed? false}}]
        (is (true? (work-claim-without-execution? result))
            (str "Should trigger for: " phrase)))))

  (testing "does NOT trigger when execution evidence present"
    (let [result {:ok true
                  :result "I'll run the tests now"
                  :session-id "sid-1"
                  :execution {:tool-events 3 :command-events 1 :executed? true}}]
      (is (false? (work-claim-without-execution? result)))
      (is (false? (enforcement-needed? "do the thing" result)))))

  (testing "does NOT trigger on error results"
    (let [result {:ok true
                  :result "I'll do it"
                  :error "timeout"
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (false? (work-claim-without-execution? result)))))

  (testing "does NOT trigger on blank result"
    (let [result {:ok true
                  :result ""
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (false? (work-claim-without-execution? result))))))

(deftest task-reply-without-execution-detects-task-mode-non-execution
  (testing "task-mode prompt with non-planning reply triggers"
    (let [prompt "mode: task\nPlease run the migration script"
          result {:ok true
                  :result "Done, the migration has been applied successfully."
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (true? (task-reply-without-execution? prompt result)))
      (is (= :task-reply-without-execution (enforcement-reason prompt result)))))

  (testing "mission-work prompt also triggers"
    (let [prompt "task assignment: FM-001 run the ArSE query"
          result {:ok true
                  :result "Completed the query and stored results."
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (true? (task-reply-without-execution? prompt result)))))

  (testing "planning-only reply does NOT trigger (legitimate deferral)"
    (let [prompt "mode: task\nRun the tests"
          result {:ok true
                  :result "Not started — need clarification on which test suite."
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (false? (task-reply-without-execution? prompt result)))))

  (testing "non-task prompt does NOT trigger"
    (let [prompt "what time is it?"
          result {:ok true
                  :result "It's about 3pm."
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (false? (task-reply-without-execution? prompt result))))))

(deftest format-refusal-detects-invented-limits
  (testing "separate-message request + format excuse triggers"
    (let [prompt "post each item in a separate IRC message"
          result {:ok true
                  :result "Due to IRC channel limitations I can't send multiple messages per turn."
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (true? (format-refusal? prompt result)))
      (is (= :format-refusal (enforcement-reason prompt result)))))

  (testing "no separate-message request → no trigger even with excuse text"
    (let [prompt "list the files"
          result {:ok true
                  :result "IRC surface mode caps limit my output."
                  :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (false? (format-refusal? prompt result))))))

;; ===========================================================================
;; H-1 Tests: Enforcement prompt includes original context
;; ===========================================================================

(deftest enforcement-prompt-includes-original-request-and-prior-reply
  (let [prompt "Please run `!ask what is the capital of France`"
        reply "I'll prep the query in /tmp/arse-fm001.txt"
        followup (execution-followup-prompt prompt reply)]
    (is (str/includes? followup "execution evidence"))
    (is (str/includes? followup prompt))
    (is (str/includes? followup reply))))

(deftest enforcement-prompt-clips-long-inputs
  (let [long-prompt (apply str (repeat 2000 "x"))
        long-reply (apply str (repeat 1500 "y"))
        followup (execution-followup-prompt long-prompt long-reply)]
    (is (< (count followup) 2000) "prompt should be clipped")
    (is (str/includes? followup "...") "clipped text ends with ellipsis")))

;; ===========================================================================
;; H-2 Tests: Event classification produces diagnosable evidence
;; ===========================================================================

(deftest tool-event-classification
  (testing "command_execution events count as both tool and command"
    (let [evt {:type "item.started"
               :item {:type "command_execution"
                      :command "/bin/bash -lc 'ls'"}}]
      (is (true? (tool-event? evt)))
      (is (true? (command-event? evt)))))

  (testing "tool_call with bash name counts as command"
    (let [evt {:type "item.completed"
               :item {:type "tool_call" :name "bash"}}]
      (is (true? (tool-event? evt)))
      (is (true? (command-event? evt)))))

  (testing "tool_call with non-bash name counts as tool but not command"
    (let [evt {:type "item.started"
               :item {:type "tool_call" :name "read_file"}}]
      (is (true? (tool-event? evt)))
      (is (false? (command-event? evt)))))

  (testing "agent_message is not a tool event"
    (let [evt {:type "item.completed"
               :item {:type "agent_message" :text "hello"}}]
      (is (false? (tool-event? evt)))
      (is (false? (command-event? evt)))))

  (testing "reasoning event is not a tool event"
    (is (false? (tool-event? {:type "reasoning"})))
    (is (false? (command-event? {:type "reasoning"})))))

(deftest enforcement-reason-is-diagnosable
  (testing "each failure mode produces a distinct, named reason"
    (let [work-claim {:ok true :result "I'll do it"
                      :execution {:tool-events 0 :command-events 0 :executed? false}}
          task-reply {:ok true :result "Done, all good."
                      :execution {:tool-events 0 :command-events 0 :executed? false}}
          format-ref {:ok true :result "IRC channel limited, can't post multiple"
                      :execution {:tool-events 0 :command-events 0 :executed? false}}]
      (is (= :work-claim-without-execution
             (enforcement-reason "anything" work-claim)))
      (is (= :task-reply-without-execution
             (enforcement-reason "mode: task\ndo it" task-reply)))
      (is (= :format-refusal
             (enforcement-reason "post each in a separate IRC message" format-ref)))))

  (testing "successful execution returns nil reason"
    (let [good {:ok true :result "Done"
                :execution {:tool-events 2 :command-events 1 :executed? true}}]
      (is (nil? (enforcement-reason "mode: task\ndo it" good))))))

;; ===========================================================================
;; H-2 Tests: Execution evidence shape
;; ===========================================================================

(deftest execution-map-shape-is-stable
  (testing "execution map always has the three required fields"
    (doseq [exec [{:tool-events 0 :command-events 0 :executed? false}
                  {:tool-events 5 :command-events 2 :executed? true}
                  {:tool-events 1 :command-events 0 :executed? true
                   :enforced-retry? true}]]
      (is (contains? exec :tool-events))
      (is (contains? exec :command-events))
      (is (contains? exec :executed?))
      (is (integer? (:tool-events exec)))
      (is (integer? (:command-events exec)))
      (is (boolean? (:executed? exec))))))
