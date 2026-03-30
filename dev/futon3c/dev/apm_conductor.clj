(ns futon3c.dev.apm-conductor
  "APM conductor — event-driven proof cycle dispatch.

   Bell-driven: when the agent completes a phase dispatch, the idle
   callback fires, advances the cycle, and dispatches the next phase.

   Execute phase enforcement:
   - 15-minute floor: cannot exit execute with sorry unless 15 min elapsed
   - Zero-sorry early exit: fully closed proof can exit immediately
   - If agent returns early with sorry, re-dispatched with remaining time
   - All phase timings logged to data/apm-conductor-log.edn

   start-apm-conductor! / stop-apm-conductor! pair."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.agency.registry :as reg])
  (:import [java.time Instant]))

;; =============================================================================
;; State
;; =============================================================================

(defonce !apm-conductor (atom nil))
(defonce !apm-state (atom nil))

(declare stop-apm-conductor!)
(declare start-next-problem!)

(def ^:private apm-phases
  [:observe :propose :execute :validate :classify :integrate])

(def ^:private lean-floor-ms
  "Minimum time in execute phase (15 minutes) unless zero sorry."
  (* 15 60 1000))

(def ^:private phase-retry-delay-ms
  30000)

(def ^:private max-phase-failures
  3)

(def ^:private lean-proofs-root
  "/home/joe/code/apm-lean/lean-proofs")

;; =============================================================================
;; Logging
;; =============================================================================

(def ^:private log-path
  "/home/joe/code/futon3c/data/apm-conductor-log.edn")

(defn- log!
  "Append a log entry to the conductor log file."
  [entry]
  (let [entry (assoc entry :at (str (Instant/now)))]
    (spit log-path (str (pr-str entry) "\n") :append true)
    (println (str "[apm-log] " (:event entry) " " (:problem entry "")
                  " " (:phase entry "") " " (:elapsed-ms entry "")
                  (when-let [m (:message entry)] (str " — " m))))))

(defn- append-phase-output
  [existing new-output]
  (let [existing (str/trim (or existing ""))
        new-output (str/trim (or new-output ""))]
    (cond
      (str/blank? existing) new-output
      (str/blank? new-output) existing
      :else (str existing "\n\n---\n\n" new-output))))

;; =============================================================================
;; Phase prompt dispatch
;; =============================================================================

(defn- make-phase-prompt
  [problem tex-body phase phase-notes]
  (case phase
    :observe   (apm-queue/make-observe-prompt problem tex-body)
    :propose   (apm-queue/make-propose-prompt problem tex-body (:observe phase-notes))
    :execute   (apm-queue/make-execute-prompt problem tex-body
                                              (:observe phase-notes) (:propose phase-notes))
    :validate  (apm-queue/make-validate-prompt problem (:execute phase-notes) "see notes")
    :classify  (apm-queue/make-classify-prompt problem (:validate phase-notes))
    :integrate (apm-queue/make-integrate-prompt problem
                 (str/join "\n\n" (for [p [:observe :propose :execute :validate :classify]
                                        :let [n (get phase-notes p)]
                                        :when n]
                                    (str (str/upper-case (name p)) ":\n" n))))))

(defn- make-lean-continue-prompt
  "Prompt for re-dispatch when agent returns early from execute with sorry."
  [problem remaining-ms previous-output]
  (str "You are STILL in the EXECUTE phase. Timer has NOT expired.\n\n"
       "Problem: apm-" (:id problem) "\n\n"
       "You returned with sorry instances but " (long (/ remaining-ms 1000))
       " seconds remain on your 15-minute Lean timer.\n\n"
       "Your previous output (summary):\n"
       (subs previous-output 0 (min 1500 (count previous-output)))
       "\n\n"
       "CONTINUE WORKING on closing sorry instances. Use HtDP:\n"
       "1. Pick the most promising sorry\n"
       "2. Search Mathlib for the API you need\n"
       "3. Wire it: lake build, read error, fix, repeat\n"
       "4. If you close it, move to the next sorry\n\n"
       "When the timer expires, the conductor will advance you.\n"
       "DO NOT rewrite the informal proof — focus entirely on Lean.\n"))

(defn- has-sorry?
  "Check if output mentions sorry (indicating incomplete Lean proof)."
  [output]
  (boolean (re-find #"(?i)\bsorry\b" (or output ""))))

(defn- problem-lean-dir
  [problem]
  (io/file lean-proofs-root (:id problem)))

(defn- mentioned-lean-artifacts
  [output]
  (->> (re-seq #"(?:/home/joe/code/apm-lean/)?lean-proofs/[^\s\]\"']+\.lean" (or output ""))
       (map #(if (str/starts-with? % "/")
               %
               (str "/home/joe/code/apm-lean/" %)))
       distinct
       vec))

(defn- recent-lean-artifacts
  [problem execute-start-ms]
  (let [dir (problem-lean-dir problem)
        threshold-ms (long (max 0 (- (or execute-start-ms 0) 2000)))]
    (if (.exists dir)
      (->> (file-seq dir)
           (filter #(.isFile ^java.io.File %))
           (filter #(str/ends-with? (.getName ^java.io.File %) ".lean"))
           (filter #(>= (.lastModified ^java.io.File %) threshold-ms))
           (map #(.getAbsolutePath ^java.io.File %))
           distinct
           vec)
      [])))

(defn- discover-lean-artifacts
  [problem execute-start-ms output]
  (let [mentioned (mentioned-lean-artifacts output)
        recent (recent-lean-artifacts problem execute-start-ms)]
    (->> (concat mentioned recent)
         distinct
         (filter #(try (.exists (io/file %))
                       (catch Exception _ false)))
         vec)))

(defn- artifact-has-sorry?
  [path]
  (try
    (boolean (re-find #"(?i)\bsorry\b" (slurp path)))
    (catch Exception _
      true)))

(defn- fully-closed-execute?
  [problem execute-start-ms output]
  (let [artifacts (discover-lean-artifacts problem execute-start-ms output)]
    (and (seq artifacts)
         (not (has-sorry? output))
         (every? (complement artifact-has-sorry?) artifacts))))

(defn- dispatch-phase!
  "Dispatch the current phase to the agent. Non-blocking (runs in future).
   Records dispatch timestamp in state."
  [agent-id _evidence-store & {:keys [prompt-override timeout-override]}]
  (let [{:keys [current-problem current-phase phase-notes]} @!apm-state
        tex-body (apm-queue/load-problem-tex (:id current-problem))
        pid (str "apm-" (:id current-problem))
        prompt (or prompt-override
                   (make-phase-prompt current-problem tex-body current-phase phase-notes))
        timeout-ms (or timeout-override
                       (if (= current-phase :execute) lean-floor-ms 300000))
        dispatch-ms (System/currentTimeMillis)]

    ;; Record dispatch time
    (swap! !apm-state
           (fn [state]
             (cond-> (assoc state :phase-dispatch-ms dispatch-ms)
               (and (= current-phase :execute)
                    (nil? (:execute-start-ms state)))
               (assoc :execute-start-ms dispatch-ms))))

    (log! {:event :phase-dispatch :problem pid :phase current-phase
           :timeout-ms timeout-ms})

    (println (str "[apm-conductor] Dispatching " (name current-phase) " for " pid
                  " to " agent-id " (timeout " (/ timeout-ms 1000) "s)"))
    (future
      (try
        (let [result (reg/invoke-agent! agent-id prompt timeout-ms)]
          (when-not (:ok result)
            (println (str "[apm-conductor] " (name current-phase) " FAILED: "
                          (:error result)))))
        (catch Exception e
          (println (str "[apm-conductor] Exception in " (name current-phase) ": "
                        (.getMessage e))))))))

;; =============================================================================
;; Execute phase floor enforcement
;; =============================================================================

(defn- handle-execute-return!
  "Handle agent returning from execute phase.
   If sorry > 0 and < 15 min elapsed, re-dispatch with remaining time.
   If sorry = 0, allow early exit (fully closed proof).
   If >= 15 min elapsed, advance regardless."
  [agent-id agent-output evidence-store]
  (let [{:keys [current-problem phase-dispatch-ms execute-start-ms]} @!apm-state
        pid (str "apm-" (:id current-problem))
        now-ms (System/currentTimeMillis)
        dispatch-elapsed-ms (- now-ms (or phase-dispatch-ms now-ms))
        total-elapsed-ms (- now-ms (or execute-start-ms now-ms))
        remaining-ms (- lean-floor-ms total-elapsed-ms)
        artifacts (discover-lean-artifacts current-problem execute-start-ms agent-output)
        sorry? (or (has-sorry? agent-output)
                   (some artifact-has-sorry? artifacts))
        fully-closed? (fully-closed-execute? current-problem execute-start-ms agent-output)]

    (log! {:event :execute-return :problem pid
           :elapsed-ms dispatch-elapsed-ms
           :total-elapsed-ms total-elapsed-ms
           :artifact-count (count artifacts)
           :sorry? sorry? :fully-closed? fully-closed?
           :remaining-ms (max 0 remaining-ms)})

    (cond
      ;; Zero sorry — early exit allowed
      fully-closed?
      (do
        (log! {:event :execute-early-exit :problem pid :elapsed-ms dispatch-elapsed-ms
               :total-elapsed-ms total-elapsed-ms
               :message "Zero sorry — fully closed, early exit permitted"})
        (println (str "[apm-conductor] Execute: FULLY CLOSED in "
                      (long (/ total-elapsed-ms 1000)) "s total — early exit"))
        ;; Accumulate output and advance
        (swap! !apm-state update-in [:phase-notes :execute]
               append-phase-output agent-output)
        true)  ;; signal: advance

      ;; Timer expired — advance with whatever we have
      (<= remaining-ms 0)
      (do
        (log! {:event :execute-timer-expired :problem pid :elapsed-ms dispatch-elapsed-ms
               :total-elapsed-ms total-elapsed-ms
               :message "Timer expired with sorry — advancing with partial"})
        (println (str "[apm-conductor] Execute: timer expired ("
                      (long (/ total-elapsed-ms 1000)) "s total) with sorry — advancing"))
        (swap! !apm-state update-in [:phase-notes :execute]
               append-phase-output agent-output)
        true)  ;; signal: advance

      ;; Agent returned early with sorry — re-dispatch
      :else
      (do
        (log! {:event :execute-redispatch :problem pid :elapsed-ms dispatch-elapsed-ms
               :total-elapsed-ms total-elapsed-ms
               :remaining-ms remaining-ms
               :message (str "Sorry present, " (long (/ remaining-ms 1000))
                             "s remaining — re-dispatching")})
        (println (str "[apm-conductor] Execute: sorry present, "
                      (long (/ remaining-ms 1000)) "s remaining — re-dispatching"))
        ;; Accumulate output so far
        (swap! !apm-state update-in [:phase-notes :execute]
               append-phase-output agent-output)
        ;; Re-dispatch with remaining time
        (dispatch-phase! agent-id evidence-store
                         :prompt-override (make-lean-continue-prompt
                                           (:current-problem @!apm-state)
                                           remaining-ms agent-output)
                         :timeout-override remaining-ms)
        false))))  ;; signal: don't advance yet, re-dispatched

(defn- handle-phase-failure!
  [agent-id outcome evidence-store]
  (let [{:keys [current-problem current-phase]} @!apm-state
        pid (some-> current-problem :id (#(str "apm-" %)))
        failures (inc (or (:phase-failure-count @!apm-state) 0))
        message (or (get-in outcome [:error :message])
                    (some-> (:error outcome) str)
                    "Unknown invoke failure")]
    (swap! !apm-state assoc :phase-failure-count failures)
    (log! {:event :phase-failure :problem pid :phase current-phase
           :failure-count failures :message message})
    (if (<= failures max-phase-failures)
      (future
        (Thread/sleep phase-retry-delay-ms)
        (when @!apm-conductor
          (dispatch-phase! agent-id evidence-store)))
      (do
        (log! {:event :conductor-stopping :problem pid :phase current-phase
               :message (str "Exceeded retry budget after " failures " failures")})
        (stop-apm-conductor!)))))

;; =============================================================================
;; Phase advancement + next phase logic
;; =============================================================================

(defn- advance-and-dispatch-next!
  "Called when agent completes a phase. Advances the cycle and dispatches next."
  [agent-id agent-output evidence-store]
  (let [{:keys [current-problem current-phase cycle-id backend
                problems-done batch-results target-n
                phase-dispatch-ms execute-start-ms]} @!apm-state
        pid (str "apm-" (:id current-problem))
        output (or agent-output "")
        elapsed-ms (- (System/currentTimeMillis) (or phase-dispatch-ms 0))]

    ;; Log phase completion
    (log! {:event :phase-complete :problem pid :phase current-phase
           :elapsed-ms elapsed-ms})
    (swap! !apm-state assoc :phase-failure-count 0)

    ;; Execute phase has special floor enforcement
    (if (and (= current-phase :execute)
             (not (handle-execute-return! agent-id agent-output evidence-store)))
      ;; Re-dispatched — exit without advancing
      (println "[apm-conductor] Execute re-dispatched, waiting for next return")

      ;; For non-execute phases (or execute that passed the floor):
      (do
        (when (not= current-phase :execute)
          (swap! !apm-state assoc-in [:phase-notes current-phase] output))

    ;; Build phase-data and advance
    (let [accumulated-execute (get-in @!apm-state [:phase-notes :execute])
          execute-artifacts (discover-lean-artifacts current-problem execute-start-ms accumulated-execute)
          execute-total-elapsed (- (System/currentTimeMillis) (or execute-start-ms phase-dispatch-ms 0))
          phase-data
          (case current-phase
            :observe   {:blocker-id "root" :notes output}
            :propose   {:approach (or (first (re-seq #"(?s)KEY INSIGHT.*?(?=\n\n|\z)" output))
                                      output)
                        :notes output}
            :execute   {:artifacts execute-artifacts
                        :dependency-graph [{:lemma "see-notes" :lean-type "see-notes"
                                           :source "tbd" :on-critical-path true}]
                        :lean-elapsed-ms execute-total-elapsed
                        :notes (or accumulated-execute "")
                        :lean-timed-out (when (>= execute-total-elapsed lean-floor-ms)
                                          (str "Timer expired after " (long (/ execute-total-elapsed 1000))
                                               "s — see notes for attempt details"))}
            :validate  {:validation-artifacts ["see-notes"] :notes output}
            :classify  {:classification (cond
                                          (re-find #"(?i)\bproved\b" output) :proved
                                          (re-find #"(?i)\bpartial\b" output) :partial
                                          :else :partial)
                        :rationale output :notes output}
            :integrate {:rationale output :ledger-changes [] :notes output
                        :arse-questions (vec (for [i (range 5)]
                                              {:type (nth [:why-hard :what-crux :why-works
                                                           :what-connects :confidence] i)
                                               :question (str "Q" (inc i))
                                               :answer "see notes"}))})

          advance-result (.execute-tool backend :cycle-advance [pid cycle-id phase-data])]

      (if-not (:ok advance-result)
        (do
          (log! {:event :phase-rejected :problem pid :phase current-phase
                 :message (get-in advance-result [:error :message])})
          (println (str "[apm-conductor] " (name current-phase) " REJECTED: "
                        (get-in advance-result [:error :message]))))

        ;; Advance succeeded — what's next?
        (let [next-idx (inc (.indexOf apm-phases current-phase))]
          (if (< next-idx (count apm-phases))
            ;; Next agent phase
            (do
              (swap! !apm-state assoc :current-phase (nth apm-phases next-idx))
              (dispatch-phase! agent-id evidence-store))

            ;; All agent phases done — mechanical commit + gate-review
            (do
              (log! {:event :mechanical-gates :problem pid})
              (.execute-tool backend :cycle-advance [pid cycle-id {:saved? true}])
              (let [classification (let [cls (get-in @!apm-state [:phase-notes :classify])]
                                    (cond
                                      (and cls (re-find #"(?i)\bproved\b" cls)) :proved
                                      :else :partial))
                    _ (.execute-tool backend :cycle-advance
                        [pid cycle-id {:gates-passed true :result-status classification}])
                    ;; Save
                    _ (let [cache @(.cache backend) state (get cache pid)]
                        (when state
                          (spit (str "/home/joe/code/futon3c/data/proof-state/" pid ".edn")
                                (pr-str state))))
                    total-elapsed (- (System/currentTimeMillis)
                                     (or (:problem-start-ms @!apm-state) 0))]

                ;; Evidence
                (apm-queue/emit-apm-evidence! evidence-store
                  {:problem-id pid :problem-base (:id current-problem)
                   :subject (:subject current-problem)
                   :session-id (str "apm-conductor-" pid)
                   :event-tag :workflow-complete
                   :classification (name classification)})

                (log! {:event :problem-complete :problem pid
                       :classification (name classification)
                       :total-elapsed-ms total-elapsed})

                (println (str "[apm-conductor] === " pid " COMPLETE: "
                              (name classification) " ("
                              (long (/ total-elapsed 1000)) "s) ==="))

                ;; Track + next
                (let [result {:ok true :problem-id pid :classification classification
                              :elapsed-ms total-elapsed}
                      done (inc problems-done)
                      results (conj (or batch-results []) result)]
                  (swap! !apm-state assoc :problems-done done :batch-results results)

                  (when (zero? (mod done 10))
                    (log! {:event :retrospective :batch (/ done 10)}))

                  (if (>= done target-n)
                    (do (log! {:event :batch-complete :done done :target target-n})
                        (println (str "[apm-conductor] Batch complete: " done "/" target-n))
                        (stop-apm-conductor!))
                    (start-next-problem! agent-id evidence-store)))))))))))))

;; =============================================================================
;; Problem lifecycle
;; =============================================================================

(defn- start-next-problem!
  [agent-id evidence-store]
  (let [issues (apm-queue/next-unprocessed evidence-store 1)]
    (if (empty? issues)
      (do (log! {:event :queue-empty})
          (stop-apm-conductor!))
      (let [issue (first issues)
            base (:problem-base issue)
            pid (str "apm-" base)
            manifest (apm-queue/load-apm-manifest)
            problem (first (filter #(= base (:id %)) manifest))
            backend (pb/make-proof-backend
                      {:phase-validator apm-queue/apm-phase-validator})]

        ;; Init proof state
        (log! {:event :problem-start :problem pid})
        (.execute-tool backend :proof-load [pid])
        (let [r (.execute-tool backend :cycle-begin [pid "root"])
              cid (get-in r [:result :cycle/id])]
          (apm-queue/emit-apm-evidence! evidence-store
            {:problem-id pid :problem-base base :subject (:subject problem)
             :session-id (str "apm-conductor-" pid) :event-tag :workflow-start})
          (swap! !apm-state assoc
                 :current-problem problem :current-phase :observe
                 :cycle-id cid :backend backend :phase-notes {}
                 :problem-start-ms (System/currentTimeMillis)
                 :phase-dispatch-ms nil
                 :execute-start-ms nil
                 :phase-failure-count 0)
          (dispatch-phase! agent-id evidence-store))))))

;; =============================================================================
;; Start / Stop
;; =============================================================================

(defn stop-apm-conductor! []
  (when @!apm-conductor
    (reg/set-on-idle! nil)
    (reset! !apm-conductor nil)
    (log! {:event :conductor-stopped})
    (println "[apm-conductor] Stopped.")))

(defn start-apm-conductor!
  "Start the APM conductor.

   Execute phase floor: agent cannot exit execute with sorry before 15 min.
   If agent returns early with sorry, re-dispatched with remaining time.
   Zero-sorry proof can exit immediately.
   All timings logged to data/apm-conductor-log.edn."
  [evidence-store & {:keys [agent-id n] :or {agent-id "claude-1" n 40}}]
  (stop-apm-conductor!)

  ;; Init log file header
  (spit log-path (str ";; APM Conductor Log — " (Instant/now) "\n") :append true)

  (reset! !apm-state {:problems-done 0 :batch-results [] :target-n n})

  (reg/set-on-idle!
    (fn [idle-agent-id outcome]
      (when (and (= idle-agent-id agent-id) @!apm-conductor)
        (if (:ok outcome true)
          (let [output (:result outcome)]
            (advance-and-dispatch-next! agent-id output evidence-store))
          (handle-phase-failure! agent-id outcome evidence-store)))))

  (reset! !apm-conductor {:agent-id agent-id :started-at (System/currentTimeMillis)})
  (log! {:event :conductor-started :agent agent-id :target n})

  (start-next-problem! agent-id evidence-store)
  {:ok true :agent-id agent-id :target n})
