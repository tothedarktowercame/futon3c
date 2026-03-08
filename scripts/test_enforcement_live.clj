;; Live enforcement test — exercises the H-1/H-2 contract against real codex.
;; Usage: clojure -M scripts/test_enforcement_live.clj
;;
;; Sends a task prompt to codex, parses the output through the enforcement
;; logic from the WS bridge, retries if enforcement triggers, and reports
;; structured evidence for each step.

(require '[cheshire.core :as json]
         '[clojure.java.shell :as sh]
         '[clojure.string :as str])

;; --- Paste the pure functions from the bridge (same code) ---

(defn extract-agent-text [item]
  (let [content (or (:text item) (:content item))]
    (cond
      (string? content) content
      (sequential? content)
      (->> content
           (keep (fn [part]
                   (when (and (map? part) (= "text" (:type part)))
                     (:text part))))
           (remove str/blank?)
           (str/join ""))
      :else nil)))

(defn tool-event? [evt]
  (let [t (:type evt)
        item (:item evt)]
    (or (= "command_execution" t)
        (and (contains? #{"item.started" "item.completed"} t)
             (= "command_execution" (:type item)))
        (and (contains? #{"item.started" "item.completed"} t)
             (= "tool_call" (:type item))))))

(defn command-event? [evt]
  (let [t (:type evt)
        item (:item evt)
        n (:name item)]
    (or (= "command_execution" t)
        (and (contains? #{"item.started" "item.completed"} t)
             (= "command_execution" (:type item)))
        (and (contains? #{"item.started" "item.completed"} t)
             (= "tool_call" (:type item))
             (contains? #{"command_execution" "command-execution" "bash" "shell"}
                        (some-> n str/lower-case))))))

(defn parse-codex-output [raw-output prior-session-id]
  (let [events (keep (fn [line]
                       (try (json/parse-string line true)
                            (catch Exception _ nil)))
                     (str/split-lines (or raw-output "")))
        session-id (or (some (fn [evt]
                               (when (= "thread.started" (:type evt))
                                 (or (:thread_id evt) (:session_id evt))))
                             events)
                       prior-session-id)
        tool-events (count (filter tool-event? events))
        command-events (count (filter command-event? events))
        text (or (some->> events
                          (filter #(= "item.completed" (:type %)))
                          (map :item)
                          (filter #(= "agent_message" (:type %)))
                          (map extract-agent-text)
                          (remove str/blank?)
                          last)
                 (some->> events
                          (filter #(= "error" (:type %)))
                          (map :message)
                          (remove str/blank?)
                          last)
                 (some-> raw-output str/trim not-empty)
                 "[No assistant message returned]")]
    {:session-id session-id
     :text text
     :execution {:tool-events tool-events
                 :command-events command-events
                 :executed? (pos? (+ tool-events command-events))}}))

(def work-claim-re
  #"(?i)\b(i['']?ll|i will|we['']?ll|we will|claiming|i claim|taking|i(?:'m| am) taking|proceeding|starting|kicking off|working on|i(?:'m| am) on it)\b")

(def planning-only-re
  #"(?i)\b(planning-only|not started|need clarification|need more context|cannot execute yet|blocked)\b")

(def task-mode-re #"(?i)\bmode:\s*task\b")

(def mission-work-re
  #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b")

(defn no-execution-evidence? [result]
  (let [execution (:execution result)
        tool-events (long (or (:tool-events execution) 0))
        command-events (long (or (:command-events execution) 0))]
    (and (nil? (:error result))
         (not (:executed? execution))
         (zero? tool-events)
         (zero? command-events))))

(defn work-claim-without-execution? [result]
  (let [text (str/trim (or (:result result) ""))]
    (and (no-execution-evidence? result)
         (not (str/blank? text))
         (boolean (re-find work-claim-re text)))))

(defn task-reply-without-execution? [prompt result]
  (let [text (str/trim (or (:result result) ""))
        prompt-str (str (or prompt ""))]
    (and (or (boolean (re-find task-mode-re prompt-str))
             (boolean (re-find mission-work-re prompt-str)))
         (no-execution-evidence? result)
         (not (str/blank? text))
         (not (boolean (re-find planning-only-re text))))))

(defn enforcement-needed? [prompt result]
  (or (work-claim-without-execution? result)
      (task-reply-without-execution? prompt result)))

(defn clip [s max-len]
  (let [txt (str (or s ""))]
    (if (<= (count txt) max-len)
      txt
      (str (subs txt 0 (max 0 (- max-len 3))) "..."))))

(defn execution-followup-prompt [original-prompt prior-reply]
  (str "Your previous reply made a work/progress claim without execution evidence.\n"
       "Execute one concrete first step now (tool/command activity is required).\n"
       "Then reply in one short line with actual status and artifact refs.\n\n"
       "Original request:\n"
       (clip original-prompt 900)
       "\n\nPrevious reply:\n"
       (clip prior-reply 600)))

;; --- Invoke codex ---

(def codex-bin (or (System/getenv "CODEX_BIN") "codex"))
(def codex-cwd (or (System/getenv "CODEX_CWD") (System/getProperty "user.dir")))
(def codex-model (or (System/getenv "CODEX_MODEL") ""))
(def codex-sandbox (or (System/getenv "CODEX_SANDBOX") "danger-full-access"))

(defn build-cmd [sid]
  (let [opts (cond-> ["--json" "--skip-git-repo-check"
                       "--sandbox" codex-sandbox
                       "-c" "approval_policy=\"never\""]
               (and (string? codex-model) (not (str/blank? codex-model)))
               (into ["--model" codex-model]))]
    (if (and (string? sid) (not (str/blank? sid)))
      (into [codex-bin "exec"] (concat opts ["resume" sid "-"]))
      (into [codex-bin "exec"] (concat opts ["-"])))))

(defn invoke! [prompt sid]
  (let [cmd (build-cmd sid)
        _ (println "  cmd:" (str/join " " cmd))
        result (apply sh/sh (concat cmd [:in (str prompt "\n") :dir codex-cwd]))
        raw (str (:out result) (:err result))
        parsed (parse-codex-output raw sid)]
    (if (zero? (:exit result))
      {:ok true
       :result (some-> (:text parsed) str/trim not-empty)
       :session-id (:session-id parsed)
       :execution (:execution parsed)}
      {:ok false
       :error (str "exit-" (:exit result))
       :session-id sid
       :execution (:execution parsed)})))

;; --- Main ---

(defn report [label m]
  (println (str "\n=== " label " ==="))
  (doseq [[k v] (sort-by key m)]
    (println (str "  " (name k) ": " (pr-str v))))
  (flush))

(let [sid (str/trim (or (try (slurp "/tmp/futon-codex-session-id") (catch Exception _ nil)) ""))
      ;; Task-mode prompt — the kind that comes through IRC
      prompt (str "[Surface: IRC | Channel: #futon | Speaker: joe | Mode: task]\n"
                  "Run `echo hello-from-codex` and tell me the output.")]
  (println "=== LIVE ENFORCEMENT TEST ===")
  (println "session:" (if (str/blank? sid) "(new)" sid))
  (println "prompt:" prompt)
  (println)
  (flush)

  (println "--- Initial invoke ---")
  (flush)
  (let [initial (invoke! prompt sid)]
    (report "Initial result" initial)

    (if (and (:ok initial) (enforcement-needed? prompt initial))
      ;; Enforcement triggered
      (let [reason (cond
                     (work-claim-without-execution? initial) :work-claim
                     (task-reply-without-execution? prompt initial) :task-reply
                     :else :unknown)]
        (println (str "\n*** ENFORCEMENT TRIGGERED: " (name reason) " ***"))
        (println "--- Retry invoke ---")
        (flush)
        (let [retry-prompt (execution-followup-prompt prompt (:result initial))
              retry-sid (or (:session-id initial) sid)
              retry (invoke! retry-prompt retry-sid)]
          (report "Retry result" retry)
          (if (enforcement-needed? prompt retry)
            (do (println "\n*** RETRY ALSO FAILED ENFORCEMENT — terminal failure ***")
                (println "H-1 FAIL: Q→A contract not met after enforcement"))
            (do (println "\n*** RETRY SUCCEEDED ***")
                (println "H-1 PASS: Q→A contract met after enforcement retry")))
          (println "H-2: enforcement-reason=" (name reason)
                   " tool-events=" (:tool-events (:execution retry))
                   " command-events=" (:command-events (:execution retry)))))

      ;; No enforcement needed
      (do
        (println "\n*** NO ENFORCEMENT NEEDED ***")
        (if (:executed? (:execution initial))
          (println "H-1 PASS: Q→A contract met on first invoke")
          (println "H-1 NOTE: no execution evidence but no enforcement trigger either"))
        (println "H-2: tool-events=" (:tool-events (:execution initial))
                 " command-events=" (:command-events (:execution initial))))))

  (println "\n=== DONE ==="))
