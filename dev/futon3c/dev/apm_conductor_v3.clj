(ns futon3c.dev.apm-conductor-v3
  "APM conductor v3 — stepper with mirror visibility.

   Design: the conductor dispatches one prompt at a time and WAITS.
   The human watches the mirror, then says:
     (continue!)    — accept the result, dispatch the next step
     (backup! msg)  — reject the result, re-dispatch with new instructions
     (skip!)        — abandon this problem, move to the next
     (done!)        — stop the conductor

   No record-kicks. No formal-kicks. No format gates. No sorry-kick loops.
   If the proof needs a sorry-kick, the human says (backup! \"close the sorry\").

   The stepper drives the proof peripheral phases:
     solve → [sorry-kick | backup]* → done

   Mirror: the agent's output is visible in the codex-repl-mirror buffer.
   The stepper just manages the dispatch/wait/advance cycle."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-dispatch :as apm-dispatch]
            [futon3c.dev.apm-frames :as apm-frames]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.agency.registry :as reg])
  (:import [java.time Instant]))

;; =============================================================================
;; State
;; =============================================================================

(defonce !stepper (atom nil))

(def ^:private log-path
  "/home/joe/code/futon3c/data/apm-conductor-v3-log.edn")

(defn- log! [entry]
  (let [entry (assoc entry :at (str (Instant/now)))]
    (spit log-path (str (pr-str entry) "\n") :append true)
    (println (str "[apm-v3] " (:event entry) " " (:problem entry "")
                  (when-let [m (:message entry)] (str " — " m))))))

;; =============================================================================
;; Prompt — one prompt, that's it
;; =============================================================================

(defn- make-solve-prompt
  [problem tex-body]
  (let [{:keys [id subject year session subpart-count subparts]} problem
        subject-names {:analysis "Analysis" :algebra "Algebra"
                       :topology "Topology" :applied "Applied/Functional Analysis"}]
    (str "Problem: apm-" id " (" (get subject-names subject (name subject))
         ", " year ", " (if (= session :fall) "Fall" "Spring") ")\n\n"
         "```latex\n" tex-body "\n```\n\n"
         (when (and subparts (pos? subpart-count))
           (str "Sub-parts: " (str/join ", " (map :label subparts)) "\n\n"))
         "Solve this problem completely. Write for a strong undergrad.\n"
         "Include: why it's hard, the key insight, a complete proof,\n"
         "Lean 4 formalization with Mathlib, and what connects.\n"
         "Write real proofs, not scaffolds.\n")))

;; =============================================================================
;; Core stepper
;; =============================================================================

(defn- dispatch!
  [agent-id prompt]
  (log! {:event :dispatch :agent agent-id})
  (swap! !stepper assoc :dispatch-ms (System/currentTimeMillis)
                        :waiting? true)
  (future
    (try
      (reg/invoke-agent! agent-id prompt (* 20 60 1000))
      (catch Exception e
        (println (str "[apm-v3] dispatch error: " (.getMessage e)))))))

(defn- on-idle
  [agent-id outcome]
  (when (and @!stepper
             (= agent-id (:agent-id @!stepper))
             (:waiting? @!stepper))
    (let [output (or (:result outcome) "")
          elapsed (- (System/currentTimeMillis)
                     (or (:dispatch-ms @!stepper) 0))]
      (swap! !stepper assoc
             :waiting? false
             :last-output output
             :last-elapsed-ms elapsed
             :step-count (inc (or (:step-count @!stepper) 0)))
      (log! {:event :return
             :problem (:problem-id @!stepper)
             :elapsed-ms elapsed
             :output-length (count output)})
      (println (str "\n[apm-v3] === Agent returned ("
                    (long (/ elapsed 1000)) "s, "
                    (count output) " chars) ==="))
      (println "[apm-v3] Waiting. Commands: (continue!) (backup! \"...\") (skip!) (done!)"))))

;; =============================================================================
;; User commands
;; =============================================================================

(defn start!
  "Start the v3 stepper on a problem. Dispatches the solve prompt and waits."
  [& {:keys [agent-id problem-id]
      :or {agent-id "claude-1"}}]
  (when @!stepper
    (apm-dispatch/deregister-conductor! :apm-v3))
  (let [manifest (apm-queue/load-apm-manifest)
        base (if (str/starts-with? (str problem-id) "apm-")
               (subs (str problem-id) 4)
               (str problem-id))
        problem (first (filter #(= base (:id %)) manifest))
        tex-body (apm-queue/load-problem-tex base)
        pid (str "apm-" base)]
    (spit log-path (str ";; APM v3 Stepper — " (Instant/now) "\n") :append true)
    (reset! !stepper
            {:agent-id agent-id
             :problem-id pid
             :problem problem
             :step-count 0
             :waiting? false
             :started-at (System/currentTimeMillis)})
    (apm-dispatch/register-conductor! :apm-v3 agent-id on-idle)
    (log! {:event :started :problem pid :agent agent-id})
    (dispatch! agent-id (make-solve-prompt problem tex-body))
    (println (str "[apm-v3] Started on " pid " with " agent-id))
    (println "[apm-v3] Watch the mirror. (continue!) when ready.")))

(defn continue!
  "Accept the last result and re-dispatch with 'continue working'."
  ([] (continue! "Continue working on this problem. Close any remaining sorry."))
  ([prompt]
   (cond
     (nil? @!stepper)
     (println "[apm-v3] Not running.")

     (:waiting? @!stepper)
     (println "[apm-v3] Still waiting for agent to return.")

     :else
     (let [{:keys [agent-id]} @!stepper]
       (log! {:event :continue :problem (:problem-id @!stepper)})
       (dispatch! agent-id prompt)))))

(defn backup!
  "Reject the last result and re-dispatch with corrective instructions."
  [instructions]
  (cond
    (nil? @!stepper)
    (println "[apm-v3] Not running.")

    (:waiting? @!stepper)
    (println "[apm-v3] Still waiting for agent to return.")

    :else
    (let [{:keys [agent-id problem-id]} @!stepper]
      (log! {:event :backup :problem problem-id :message instructions})
      (dispatch! agent-id
        (str "CORRECTION: Your previous output was not accepted.\n\n"
             instructions "\n\n"
             "Problem: " problem-id "\n"
             "Continue from where you left off, incorporating this feedback.\n")))))

(defn skip!
  "Abandon this problem."
  []
  (if (nil? @!stepper)
    (println "[apm-v3] Not running.")
    (do
      (log! {:event :skipped :problem (:problem-id @!stepper)
             :steps (:step-count @!stepper)})
      (println (str "[apm-v3] Skipped " (:problem-id @!stepper)))
      (apm-dispatch/deregister-conductor! :apm-v3)
      (reset! !stepper nil))))

(defn done!
  "Stop the stepper. The proof state remains in the frame workspace."
  []
  (when @!stepper
    (let [{:keys [problem-id step-count started-at]} @!stepper
          elapsed (- (System/currentTimeMillis) (or started-at 0))]
      (log! {:event :done :problem problem-id
             :steps step-count :total-ms elapsed})
      (println (str "[apm-v3] Done with " problem-id
                    " (" step-count " steps, "
                    (long (/ elapsed 1000)) "s)")))
    (apm-dispatch/deregister-conductor! :apm-v3)
    (reset! !stepper nil)))

(defn status
  "Show current stepper state."
  []
  (if-let [s @!stepper]
    {:problem (:problem-id s)
     :agent (:agent-id s)
     :steps (:step-count s)
     :waiting? (:waiting? s)
     :last-elapsed-ms (:last-elapsed-ms s)
     :last-output-length (count (or (:last-output s) ""))}
    :not-running))
