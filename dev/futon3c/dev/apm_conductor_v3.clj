(ns futon3c.dev.apm-conductor-v3
  "APM conductor v3 — stepper with mirror visibility.

   Multiple steppers can run in parallel (verbal exam mode).
   Each stepper is keyed by a conductor-id (defaults to agent-id).

   Commands:
     (start! :agent-id \"claude-1\" :problem-id \"a03J04\")
     (continue! \"claude-1\")
     (continue! \"claude-1\" \"close the sorry using X\")
     (backup! \"claude-1\" \"that proof is wrong because...\")
     (skip! \"claude-1\")
     (done! \"claude-1\")
     (exam! \"a03J04\")  — start both claude-1 and codex-1 on same problem"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-dispatch :as apm-dispatch]
            [futon3c.agency.registry :as reg])
  (:import [java.time Instant]))

;; {agent-id -> {:problem-id :problem :step-count :waiting? :last-output ...}}
(defonce !steppers (atom {}))

(def ^:private log-path
  "/home/joe/code/futon3c/data/apm-conductor-v3-log.edn")

(defn- log! [entry]
  (let [entry (assoc entry :at (str (Instant/now)))]
    (spit log-path (str (pr-str entry) "\n") :append true)))

;; =============================================================================
;; Mirror
;; =============================================================================

(defn- emacsclient! [elisp]
  (future
    (try (shell/sh "emacsclient" "-e" elisp)
      (catch Exception _ nil))))

(defn- mirror-append! [agent-id role text]
  (let [buf (str "*apm-v3-mirror:" agent-id "*")
        header (case role
                 :agent  "── agent ──────────────────────────────────"
                 :human  "── human ──────────────────────────────────"
                 :system "── system ─────────────────────────────────"
                 "────────────────────────────────────────────")
        escaped (-> (str header "\n" text "\n\n")
                    (str/replace "\\" "\\\\")
                    (str/replace "\"" "\\\"")
                    (str/replace "\n" "\\n"))]
    (emacsclient!
      (str "(let ((buf (get-buffer-create \"" buf "\")))"
           "  (with-current-buffer buf"
           "    (goto-char (point-max))"
           "    (let ((inhibit-read-only t)) (insert \"" escaped "\"))"
           "    (goto-char (point-max))"
           "    (unless (get-buffer-window buf)"
           "      (display-buffer buf '(display-buffer-in-side-window)))))"))))

(defn- mirror-init! [agent-id problem-id]
  (let [buf (str "*apm-v3-mirror:" agent-id "*")]
    (emacsclient!
      (str "(let ((buf (get-buffer-create \"" buf "\")))"
           "  (with-current-buffer buf"
           "    (let ((inhibit-read-only t))"
           "      (erase-buffer)"
           "      (insert \"APM v3 Stepper Mirror\\n"
           "Problem: " problem-id "\\n"
           "Agent: " agent-id "\\n"
           "Started: " (Instant/now) "\\n\\n\"))"
           "    (setq-local buffer-read-only t)"
           "    (display-buffer buf '(display-buffer-in-side-window))))"))))

;; =============================================================================
;; Prompt
;; =============================================================================

(def ^:private subject-names
  {:analysis "Analysis" :algebra "Algebra"
   :topology "Topology" :applied "Applied/Functional Analysis"})

(defn- make-solve-prompt [problem tex-body]
  (let [{:keys [id subject year session subpart-count subparts]} problem]
    (str "Problem: apm-" id " (" (get subject-names subject (name subject))
         ", " year ", " (if (= session :fall) "Fall" "Spring") ")\n\n"
         "```latex\n" tex-body "\n```\n\n"
         (when (and subparts (pos? subpart-count))
           (str "Sub-parts: " (str/join ", " (map :label subparts)) "\n\n"))
         "Solve this problem completely. Write for a strong undergrad.\n"
         "Include:\n"
         "1. Why it's hard\n"
         "2. The key insight\n"
         "3. A complete proof (not a sketch)\n"
         "4. Lean 4 formalization with Mathlib\n"
         "5. What connects\n\n"
         "Return everything inline.\n")))

;; =============================================================================
;; Core dispatch
;; =============================================================================

(defn- dispatch! [agent-id prompt]
  (swap! !steppers assoc-in [agent-id :dispatch-ms] (System/currentTimeMillis))
  (swap! !steppers assoc-in [agent-id :waiting?] true)
  (log! {:event :dispatch :agent agent-id})
  (future
    (try
      (reg/invoke-agent! agent-id prompt (* 20 60 1000))
      (catch Exception e
        (println (str "[apm-v3:" agent-id "] dispatch error: " (.getMessage e)))))))

(defn- make-on-idle [agent-id]
  (fn [idle-agent-id outcome]
    (when-let [stepper (get @!steppers agent-id)]
      (when (and (= idle-agent-id agent-id) (:waiting? stepper))
        (let [output (or (:result outcome) "")
              elapsed (- (System/currentTimeMillis) (or (:dispatch-ms stepper) 0))]
          (swap! !steppers update agent-id assoc
                 :waiting? false
                 :last-output output
                 :last-elapsed-ms elapsed
                 :step-count (inc (or (:step-count stepper) 0)))
          (log! {:event :return :agent agent-id
                 :problem (:problem-id stepper)
                 :elapsed-ms elapsed :output-length (count output)})
          (mirror-append! agent-id :agent output)
          (mirror-append! agent-id :system
            (str "=== " agent-id " returned (" (long (/ elapsed 1000)) "s, "
                 (count output) " chars) ==="))
          (println (str "\n[apm-v3:" agent-id "] returned ("
                        (long (/ elapsed 1000)) "s, " (count output) " chars)")))))))

;; =============================================================================
;; Commands
;; =============================================================================

(defn start!
  "Start a stepper on a problem."
  [& {:keys [agent-id problem-id] :or {agent-id "claude-1"}}]
  ;; Clean up any existing stepper for this agent
  (when (get @!steppers agent-id)
    (apm-dispatch/deregister-conductor! (keyword (str "v3-" agent-id))))
  (let [manifest (apm-queue/load-apm-manifest)
        base (if (str/starts-with? (str problem-id) "apm-")
               (subs (str problem-id) 4) (str problem-id))
        problem (first (filter #(= base (:id %)) manifest))
        tex-body (apm-queue/load-problem-tex base)
        pid (str "apm-" base)
        cid (keyword (str "v3-" agent-id))]
    (swap! !steppers assoc agent-id
           {:agent-id agent-id :problem-id pid :problem problem
            :step-count 0 :waiting? false
            :started-at (System/currentTimeMillis)})
    (apm-dispatch/register-conductor! cid agent-id (make-on-idle agent-id))
    (log! {:event :started :problem pid :agent agent-id})
    (mirror-init! agent-id pid)
    (let [prompt (make-solve-prompt problem tex-body)]
      (mirror-append! agent-id :human (str "solve " pid))
      (dispatch! agent-id prompt))
    (println (str "[apm-v3:" agent-id "] started on " pid))))

(defn continue!
  "Accept and continue."
  ([agent-id] (continue! agent-id "Continue. Close any remaining sorry. Return everything inline."))
  ([agent-id prompt]
   (let [stepper (get @!steppers agent-id)]
     (cond
       (nil? stepper) (println (str "[apm-v3:" agent-id "] not running."))
       (:waiting? stepper) (println (str "[apm-v3:" agent-id "] still waiting."))
       :else (do (log! {:event :continue :agent agent-id})
                 (mirror-append! agent-id :human prompt)
                 (dispatch! agent-id prompt))))))

(defn backup!
  "Reject and re-dispatch with correction."
  [agent-id instructions]
  (let [stepper (get @!steppers agent-id)]
    (cond
      (nil? stepper) (println (str "[apm-v3:" agent-id "] not running."))
      (:waiting? stepper) (println (str "[apm-v3:" agent-id "] still waiting."))
      :else (let [prompt (str "CORRECTION: " instructions "\nContinue from where you left off.\n")]
              (log! {:event :backup :agent agent-id :message instructions})
              (mirror-append! agent-id :human (str "BACKUP: " instructions))
              (dispatch! agent-id prompt)))))

(defn skip! [agent-id]
  (if-let [stepper (get @!steppers agent-id)]
    (do (log! {:event :skipped :agent agent-id :problem (:problem-id stepper)})
        (apm-dispatch/deregister-conductor! (keyword (str "v3-" agent-id)))
        (swap! !steppers dissoc agent-id)
        (println (str "[apm-v3:" agent-id "] skipped.")))
    (println (str "[apm-v3:" agent-id "] not running."))))

(defn done! [agent-id]
  (if-let [stepper (get @!steppers agent-id)]
    (let [elapsed (- (System/currentTimeMillis) (or (:started-at stepper) 0))]
      (log! {:event :done :agent agent-id :problem (:problem-id stepper)
             :steps (:step-count stepper) :total-ms elapsed})
      (apm-dispatch/deregister-conductor! (keyword (str "v3-" agent-id)))
      (swap! !steppers dissoc agent-id)
      (println (str "[apm-v3:" agent-id "] done. " (:step-count stepper)
                    " steps, " (long (/ elapsed 1000)) "s")))
    (println (str "[apm-v3:" agent-id "] not running."))))

(defn status
  ([] (into {} (map (fn [[k v]] [k (select-keys v [:problem-id :step-count :waiting? :last-elapsed-ms])])
                    @!steppers)))
  ([agent-id] (select-keys (get @!steppers agent-id)
                           [:problem-id :step-count :waiting? :last-elapsed-ms :last-output])))

;; =============================================================================
;; Verbal exam — same problem, both agents
;; =============================================================================

(defn exam!
  "Start both claude-1 and codex-1 on the same problem. Watch both mirrors."
  [problem-id]
  (start! :agent-id "claude-1" :problem-id problem-id)
  (start! :agent-id "codex-1" :problem-id problem-id)
  (println "[apm-v3] EXAM started. Watch *apm-v3-mirror:claude-1* and *apm-v3-mirror:codex-1*"))
