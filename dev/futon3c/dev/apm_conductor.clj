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
(declare discover-lean-artifacts)
(declare extract-bold-section)

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

(def ^:private proof-peripheral-root
  "/home/joe/code/proof_peripheral")

(def ^:private proof-state-root
  "/home/joe/code/futon3c/data/proof-state")

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

(defn- trim-to-nil [s]
  (let [s (some-> s str/trim)]
    (when (seq s) s)))

(defn- phase-sidecar-path
  [problem phase]
  (let [pid (:id problem)]
    (case phase
      :propose (str proof-peripheral-root "/apm-" pid "-propose.md")
      :execute (str proof-peripheral-root "/apm-" pid "-execute.md")
      nil)))

(defn- slurp-if-exists
  [path]
  (when path
    (let [f (io/file path)]
      (when (.exists f)
        (slurp f)))))

(def ^:private indirect-notes-pattern
  #"(?is)\b(updated|recorded|integrated|documented|captured|consolidated)\b.*\b(in|into)\b.*(\.md|\.lean|proof_peripheral|lean-proofs)|\bsee notes\b")

(defn- indirect-notes?
  [text]
  (boolean (re-find indirect-notes-pattern (or text ""))))

(defn- extract-section
  [text re]
  (some-> (re-find re (or text ""))
          second
          trim-to-nil))

(defn- extract-stage
  [text stage-num]
  (extract-section
   text
   (re-pattern
    (str "(?ms)^(?:\\*\\*)?Stage " stage-num " — .*?(?:\\*\\*)?\\s*\n(?:-+\\s*\n)?\\s*(.*?)(?=^(?:\\*\\*)?Stage \\d+ — |\\z)"))))

(defn- normalize-execute-notes
  [text]
  (if-let [stages (seq (keep #(when-let [body (extract-stage text %)]
                                (str "Stage " % " — "
                                     ({1 "THE CLEAN PROOF"
                                       2 "LEMMA DEPENDENCY GRAPH"
                                       3 "LEAN FORMALIZATION"
                                       4 "FORMAL-TO-INFORMAL REVISION"} %)
                                     "\n--------------------------------\n\n"
                                     body))
                             [1 2 3 4]))]
    (str/join "\n\n" stages)
    (trim-to-nil text)))

(defn- execute-stage-heading
  [stage-num]
  (str "Stage " stage-num " — "
       ({1 "THE CLEAN PROOF"
         2 "LEMMA DEPENDENCY GRAPH"
         3 "LEAN FORMALIZATION"
         4 "FORMAL-TO-INFORMAL REVISION"} stage-num)
       "\n--------------------------------\n\n"))

(defn- empty-execute-record
  []
  {:stage1 nil
   :stage2 nil
   :stage3 nil
   :stage4 nil
   :lean-deltas []})

(defn- full-execute-submission?
  [text]
  (every? #(extract-stage text %) [1 2 3 4]))

(defn- execute-record-complete?
  [record]
  (let [{:keys [stage1 stage2 stage3 stage4]} (merge (empty-execute-record) record)]
    (every? #(not (str/blank? %)) [stage1 stage2 stage3 stage4])))

(defn- extract-lean-delta
  [text]
  (cond
    (str/blank? text) nil
    (full-execute-submission? text) (extract-stage text 3)
    (extract-stage text 3) (extract-stage text 3)
    :else (trim-to-nil text)))

(defn- merge-execute-record
  [record text]
  (let [record (merge (empty-execute-record) record)
        text (trim-to-nil text)]
    (cond
      (str/blank? text) record
      (full-execute-submission? text)
      (assoc record
             :stage1 (extract-stage text 1)
             :stage2 (extract-stage text 2)
             :stage3 (extract-stage text 3)
             :stage4 (extract-stage text 4))
      :else
      (if (execute-record-complete? record)
        (if-let [delta (extract-lean-delta text)]
          (if (indirect-notes? delta)
            record
            (update record :lean-deltas conj delta))
          record)
        record))))

(defn- execute-record->notes
  [record]
  (let [{:keys [stage1 stage2 stage3 stage4 lean-deltas]} (merge (empty-execute-record) record)
        stage3-body (reduce append-phase-output stage3 lean-deltas)
        pieces (keep identity
                     [(when stage1 (str (execute-stage-heading 1) stage1))
                      (when stage2 (str (execute-stage-heading 2) stage2))
                      (when stage3-body (str (execute-stage-heading 3) stage3-body))
                      (when stage4 (str (execute-stage-heading 4) stage4))])]
    (when (seq pieces)
      (str/join "\n\n" pieces))))

(defn- update-execute-record!
  [output]
  (let [new-state
        (swap! !apm-state
               (fn [state]
                 (let [updated-record (merge-execute-record (:execute-record state) output)
                       canonical-notes (execute-record->notes updated-record)]
                   (cond-> (assoc state :execute-record updated-record)
                     canonical-notes (assoc-in [:phase-notes :execute] canonical-notes)))))]
    (:execute-record new-state)))

(defn- parse-dependency-entry
  [entry-text]
  (let [[_ lemma body] (re-find #"(?ms)^\s*\d+\.\s+\*\*([^*]+)\*\*\s*\n(.*)\z" entry-text)
        full (trim-to-nil entry-text)
        body (or body "")
        formal (or (extract-section body #"(?m)^\s*-\s+\*\*Formal dependency\*\*:\s*(.+)$")
                   (extract-section body #"(?m)^\s*-\s+\*Formal dependency\*:\s*(.+)$")
                   (extract-section body #"(?m)^\s*Formal dependency:\s*(.+)$"))
        informal (or (extract-section body #"(?m)^\s*-\s+\*\*Informal dependency\*\*:\s*(.+)$")
                     (extract-section body #"(?m)^\s*-\s+\*Informal dependency\*:\s*(.+)$")
                     (extract-section body #"(?m)^\s*Informal dependency:\s*(.+)$"))
        why-now (or (extract-section body #"(?m)^\s*-\s+\*\*Why this becomes thinkable here\*\*:\s*(.+)$")
                    (extract-section body #"(?m)^\s*-\s+\*Why this becomes thinkable here\*:\s*(.+)$")
                    (extract-section body #"(?m)^\s*Why this becomes thinkable here:\s*(.+)$")
                    (extract-section body #"(?m)^\s*-\s+\*\*Why now\*\*:\s*(.+)$")
                    (extract-section body #"(?m)^\s*-\s+\*Why now\*:\s*(.+)$")
                    (extract-section body #"(?m)^\s*Why now:\s*(.+)$"))
        lean-type (or (extract-section body #"(?m)^\s*-\s+\*\*Lean target/type\*\*:\s*(.+)$")
                      (extract-section body #"(?m)^\s*-\s+\*Lean target/type\*:\s*(.+)$")
                      (extract-section body #"(?m)^\s*Lean target/type:\s*(.+)$")
                      (extract-section body #"(?m)^\s*-\s+\*Type(?: \([^)]+\))?\*:\s*(.+)$")
                      (extract-section body #"(?m)^\s*Type:\s*(.+)$"))
        status (or (extract-section body #"(?m)^\s*-\s+\*\*Mathlib status/search terms\*\*:\s*(.+)$")
                   (extract-section body #"(?m)^\s*-\s+\*Mathlib status/search terms\*:\s*(.+)$")
                   (extract-section body #"(?m)^\s*Mathlib status/search terms:\s*(.+)$")
                   (extract-section body #"(?m)^\s*-\s+\*Status\*:\s*(.+)$")
                   (extract-section body #"(?m)^\s*Status:\s*(.+)$"))
        critical (or (extract-section body #"(?m)^\s*-\s+\*Critical path\*:\s*(.+)$")
                     (extract-section body #"(?m)^\s*-\s+\*\*Critical path\*\*:\s*(.+)$")
                     (extract-section body #"(?m)^\s*Critical path:\s*(.+)$"))]
    {:lemma (or (trim-to-nil lemma) "unparsed-lemma")
     :formal-dependency (or formal full)
     :informal-dependency informal
     :why-this-now why-now
     :lean-type (or lean-type full)
     :source (or status "agent-output")
     :search-terms (->> (re-seq #"`([^`]+)`" (or status ""))
                        (map second)
                        distinct
                        vec)
     :on-critical-path (boolean (re-find #"(?i)\b(yes|critical)\b" (or critical "")))
     :notes full}))

(defn- parse-dependency-graph
  [text]
  (let [section (or (extract-stage text 2) text)
        entries (->> (re-seq #"(?ms)^\s*\d+\.\s+\*\*[^*]+\*\*.*?(?=^\s*\d+\.\s+\*\*|\z)" (or section ""))
                     (map parse-dependency-entry)
                     vec)]
    (if (seq entries)
      entries
      (when-let [fallback (trim-to-nil section)]
        [{:lemma "unparsed-dependency-graph"
          :lean-type fallback
          :source "agent-output"
          :on-critical-path false
          :notes fallback}]))))

(defn- parse-arse-questions
  [text]
  (let [section (or (extract-section text #"(?ims)\*\*ArSE Questions\*\*\s*(.*)\z")
                    (extract-section text #"(?ims)###\s*ArSE QUESTIONS\s*(.*)\z")
                    text)
        infer-type (fn [idx title]
                     (let [title (str/lower-case (or title ""))]
                       (cond
                         (str/includes? title "why is this hard") :why-hard
                         (str/includes? title "key insight") :what-crux
                         (str/includes? title "crux") :what-crux
                         (str/includes? title "why does step") :why-works
                         (str/includes? title "what connects") :what-connects
                         (str/includes? title "where is intuition wrong") :confidence
                         :else (nth [:why-hard :what-crux :why-works :what-connects :confidence]
                                    idx :confidence))))]
    (->> (re-seq #"(?ms)^\s*(\d+)\.\s+\*(.+?)\*\s*(.*?)(?=^\s*\d+\.\s+\*|\z)" (or section ""))
         (map-indexed
          (fn [idx [_ _ title body]]
            (let [body (or body "")
                  q (extract-section body #"(?ims)\*\*Q:\*\*\s*(.+?)(?=\n\s*\*\*A:\*\*|\z)")
                  a (or (extract-section body #"(?ims)\*\*A:\*\*\s*(.+?)(?=\n\s*\*\*[A-Z]|^\s*\d+\.\s+\*|\z)")
                        (when-not (re-find #"(?ims)\*\*Q:\*\*" body)
                          (trim-to-nil (str/replace body #"^\s*[:\-]\s*" ""))))
                  title (some-> title
                                (str/replace #"\s*\([^)]*\)\s*$" "")
                                trim-to-nil)]
              {:type (infer-type idx title)
               :question (or q title)
               :answer a})))
         (filter #(and (:question %) (:answer %)))
         vec)))

(defn- proof-state-path
  [problem-base]
  (str proof-state-root "/apm-" problem-base ".edn"))

(defn- load-proof-state
  [problem-base]
  (let [path (proof-state-path problem-base)
        f (io/file path)]
    (when (.exists f)
      (read-string (slurp f)))))

(defn- save-proof-state!
  [problem-base state]
  (let [path (proof-state-path problem-base)
        parent (.getParentFile (io/file path))]
    (when parent
      (.mkdirs parent))
    (spit path (pr-str state)))
  state)

(defn- update-last-cycle
  [state f]
  (let [idx (dec (count (:proof/cycles state)))]
    (if (neg? idx)
      state
      (update-in state [:proof/cycles idx] f))))

(defn repair-proof-state-record!
  "Rebuild persisted execute/integrate data from the canonical notes and sidecars.
   This is for post-run cleanup of finished records; it does not change the proof
   result classification or rerun any agent work."
  [problem-base]
  (let [problem-id (str "apm-" problem-base)
        problem {:id problem-base}
        state (load-proof-state problem-base)]
    (if-not state
      {:ok false :error (str "No proof state for " problem-id)}
      (let [execute-sidecar (slurp-if-exists (phase-sidecar-path problem :execute))
            propose-sidecar (slurp-if-exists (phase-sidecar-path problem :propose))
            repaired-cycle (fn [cycle]
                             (let [phase-data (:cycle/phase-data cycle)
                                   execute-notes (or (some-> execute-sidecar normalize-execute-notes)
                                                     (get-in phase-data [:execute :notes]))
                                   integrate-notes (or (get-in phase-data [:integrate :notes])
                                                       execute-sidecar)
                                   execute-artifacts (or (get-in phase-data [:execute :artifacts])
                                                         (discover-lean-artifacts problem nil execute-notes))]
                               (-> cycle
                                   (assoc-in [:cycle/phase-data :propose :notes]
                                             (or (trim-to-nil propose-sidecar)
                                                 (get-in phase-data [:propose :notes])))
                                   (assoc-in [:cycle/phase-data :execute :notes] execute-notes)
                                   (assoc-in [:cycle/phase-data :execute :dependency-graph]
                                             (parse-dependency-graph execute-notes))
                                   (assoc-in [:cycle/phase-data :execute :artifacts] execute-artifacts)
                                   (assoc-in [:cycle/phase-data :integrate :notes] integrate-notes)
                                   (assoc-in [:cycle/phase-data :integrate :rationale]
                                             (or (extract-bold-section integrate-notes "Connections")
                                                 integrate-notes))
                                   (assoc-in [:cycle/phase-data :integrate :arse-questions]
                                             (parse-arse-questions integrate-notes)))))
            repaired (update-last-cycle state repaired-cycle)
            cycle-idx (dec (count (:proof/cycles repaired)))]
        (save-proof-state! problem-base repaired)
        {:ok true
         :problem-id problem-id
         :arse-question-count (count (get-in repaired
                                             [:proof/cycles cycle-idx :cycle/phase-data :integrate :arse-questions]))
         :dependency-count (count (get-in repaired
                                          [:proof/cycles cycle-idx :cycle/phase-data :execute :dependency-graph]))}))))

(defn- extract-bold-section
  [text title]
  (extract-section
   text
   (re-pattern
    (str "(?ms)^\\*\\*" (java.util.regex.Pattern/quote title)
         "\\*\\*\\s*(.*?)(?=^\\*\\*|\\z)"))))

(defn- classify-status
  [text]
  (cond
    (re-find #"(?i)\bproved\b" (or text "")) :proved
    (re-find #"(?i)\bbuild-failed\b" (or text "")) :inconclusive
    (re-find #"(?i)\btimed-out\b" (or text "")) :partial
    :else :partial))

(defn- normalized-phase-notes
  [problem phase raw-output]
  (let [sidecar (slurp-if-exists (phase-sidecar-path problem phase))
        execute-sidecar (slurp-if-exists (phase-sidecar-path problem :execute))]
    (case phase
      :propose (or (trim-to-nil sidecar) (trim-to-nil raw-output))
      :execute (or (some-> sidecar normalize-execute-notes) (normalize-execute-notes raw-output))
      :classify (if (indirect-notes? raw-output)
                  (or (extract-stage execute-sidecar 5)
                      (trim-to-nil raw-output))
                  (trim-to-nil raw-output))
      :integrate (if (indirect-notes? raw-output)
                   (or (extract-stage execute-sidecar 6)
                       (trim-to-nil raw-output))
                   (trim-to-nil raw-output))
      (trim-to-nil raw-output))))

(defn- validation-artifacts
  [problem execute-artifacts]
  (->> (concat execute-artifacts
               (keep identity [(phase-sidecar-path problem :propose)
                               (phase-sidecar-path problem :execute)]))
       (filter #(try (.exists (io/file %))
                     (catch Exception _ false)))
       distinct
       vec))

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
  (str "Execution evidence required: yes.\n"
       "This phase must include real Lean/tool work in Stage 3. A purely textual reply is insufficient.\n\n"
       "You are STILL in the EXECUTE phase. Timer has NOT expired.\n\n"
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
       "Return a LEAN DELTA ONLY: what changed in Stage 3, what artifacts now exist,\n"
       "which sorrys closed, and what blockers remain. Do NOT replace Stage 1/2/4 with\n"
       "a summary or 'see file' note; the conductor will merge your Lean delta into the\n"
       "canonical execute record.\n\n"
       "When the timer expires, the conductor will advance you.\n"
       "DO NOT rewrite the informal proof — focus entirely on Lean.\n"))

(defn- make-full-execute-resubmission-prompt
  [problem remaining-ms previous-output execute-record]
  (let [canonical (execute-record->notes execute-record)]
    (str "Execution evidence required: no.\n"
         "This is a record-normalization turn inside EXECUTE. The required deliverable is the full authoritative Stage 1-4 writeup inline.\n"
         "Lean execution evidence for this EXECUTE phase must already exist in prior turns; do not rerun tools just to satisfy formatting.\n\n"
         "You are STILL in the EXECUTE phase, but the authoritative Stage 1-4 record\n"
         "has not yet been fully captured.\n\n"
         "Problem: apm-" (:id problem) "\n\n"
         "Remaining Lean timer: " (long (/ (max 0 remaining-ms) 1000)) " seconds.\n\n"
         "You must now return ONE FULL EXECUTE SUBMISSION with all four sections inline:\n"
         "1. Stage 1 — THE CLEAN PROOF\n"
         "2. Stage 2 — LEMMA DEPENDENCY GRAPH\n"
         "3. Stage 3 — LEAN FORMALIZATION\n"
         "4. Stage 4 — FORMAL-TO-INFORMAL REVISION\n\n"
         "Do not answer with a delta, file pointer, ellipsis, or summary. The conductor\n"
         "will not advance execute until the full Stage 1-4 record exists.\n\n"
         (when canonical
           (str "Canonical execute content captured so far:\n"
                canonical "\n\n"))
         "Most recent execute reply:\n"
         (subs previous-output 0 (min 1800 (count previous-output)))
         "\n\n"
         "Reconstruct the full authoritative record from your current work and emit it inline now.\n")))

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

(def ^:private placeholder-artifact-pattern
  #"(?is)\bplaceholder\b|:\s*True\s*:=\s*(?:by\s+)?trivial\b|lemma\s+\S+\s*\([^)]*\)\s*:\s*True\s*:=|theorem\s+\S+\s*\([^)]*\)\s*:\s*True\s*:=")

(defn- declaration-lines
  [text]
  (->> (str/split-lines (or text ""))
       (map str/trim)
       (filter #(re-find #"^(?:lemma|theorem)\b" %))))

(defn- artifact-placeholder?
  [path]
  (try
    (let [text (slurp path)]
      (or (boolean (re-find placeholder-artifact-pattern text))
          (let [decls (declaration-lines text)]
            (or (empty? decls)
                (not-any? #(not (re-find #":\s*True\b" %)) decls)))))
    (catch Exception _
      true)))

(defn- fully-closed-execute?
  [problem execute-start-ms output]
  (let [artifacts (discover-lean-artifacts problem execute-start-ms output)]
    (and (seq artifacts)
         (not (has-sorry? output))
         (every? (complement artifact-has-sorry?) artifacts)
         (every? (complement artifact-placeholder?) artifacts))))

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
        execute-record (update-execute-record! agent-output)
        full-record? (execute-record-complete? execute-record)
        artifacts (discover-lean-artifacts current-problem execute-start-ms agent-output)
        sorry? (or (has-sorry? agent-output)
                   (some artifact-has-sorry? artifacts))
        fully-closed? (fully-closed-execute? current-problem execute-start-ms agent-output)]

    (log! {:event :execute-return :problem pid
           :elapsed-ms dispatch-elapsed-ms
           :total-elapsed-ms total-elapsed-ms
           :artifact-count (count artifacts)
           :full-record? full-record?
           :sorry? sorry? :fully-closed? fully-closed?
           :remaining-ms (max 0 remaining-ms)})

    (cond
      ;; Cannot advance execute at all until we have one authoritative
      ;; full Stage 1-4 record.
      (not full-record?)
      (do
        (log! {:event :execute-missing-full-record :problem pid
               :elapsed-ms dispatch-elapsed-ms
               :total-elapsed-ms total-elapsed-ms
               :message "Full Stage 1-4 execute submission missing — re-dispatching for canonical record"})
        (println "[apm-conductor] Execute: missing full Stage 1-4 record — re-dispatching")
        (dispatch-phase! agent-id evidence-store
                         :prompt-override (make-full-execute-resubmission-prompt
                                           (:current-problem @!apm-state)
                                           remaining-ms agent-output execute-record)
                         :timeout-override (max 60000 remaining-ms))
        false)

      ;; Zero sorry — early exit allowed
      fully-closed?
      (do
        (log! {:event :execute-early-exit :problem pid :elapsed-ms dispatch-elapsed-ms
               :total-elapsed-ms total-elapsed-ms
               :message "Zero sorry — fully closed, early exit permitted"})
        (println (str "[apm-conductor] Execute: FULLY CLOSED in "
                      (long (/ total-elapsed-ms 1000)) "s total — early exit"))
        true)  ;; signal: advance

      ;; Timer expired — advance with whatever we have
      (<= remaining-ms 0)
      (do
        (log! {:event :execute-timer-expired :problem pid :elapsed-ms dispatch-elapsed-ms
               :total-elapsed-ms total-elapsed-ms
               :message "Timer expired with sorry — advancing with partial"})
        (println (str "[apm-conductor] Execute: timer expired ("
                      (long (/ total-elapsed-ms 1000)) "s total) with sorry — advancing"))
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
                    "Unknown invoke failure")
        execute-record (:execute-record @!apm-state)
        execute-start-ms (:execute-start-ms @!apm-state)
        phase-dispatch-ms (:phase-dispatch-ms @!apm-state)
        total-execute-ms (when (= current-phase :execute)
                           (- (System/currentTimeMillis)
                              (or execute-start-ms phase-dispatch-ms 0)))
        remaining-ms (when (= current-phase :execute)
                       (max 0 (- lean-floor-ms (or total-execute-ms 0))))
        retry-override
        (when (= current-phase :execute)
          (cond
            (re-find #"(?i)Stage 1-4|dependency graph|actual Stage" message)
            {:prompt-override (make-full-execute-resubmission-prompt
                               current-problem remaining-ms "" execute-record)
             :timeout-override (max 60000 remaining-ms)}

            (not (execute-record-complete? execute-record))
            {:prompt-override (make-full-execute-resubmission-prompt
                               current-problem remaining-ms "" execute-record)
             :timeout-override (max 60000 remaining-ms)}

            :else
            {:prompt-override (make-lean-continue-prompt
                               current-problem remaining-ms "")
             :timeout-override (max 60000 remaining-ms)}))]
    (swap! !apm-state assoc :phase-failure-count failures)
    (log! {:event :phase-failure :problem pid :phase current-phase
           :failure-count failures :message message})
    (if (<= failures max-phase-failures)
      (future
        (Thread/sleep phase-retry-delay-ms)
        (when @!apm-conductor
          (if retry-override
            (dispatch-phase! agent-id evidence-store
                             :prompt-override (:prompt-override retry-override)
                             :timeout-override (:timeout-override retry-override))
            (dispatch-phase! agent-id evidence-store))))
      (do
        (log! {:event :conductor-stopping :problem pid :phase current-phase
               :message (str "Exceeded retry budget after " failures " failures")})
        (stop-apm-conductor!)))))

(defn- handle-phase-rejection!
  [agent-id rejection-message evidence-store]
  (log! {:event :phase-rejection-retry
         :problem (some-> @!apm-state :current-problem :id (#(str "apm-" %)))
         :phase (:current-phase @!apm-state)
         :message rejection-message})
  (handle-phase-failure! agent-id {:error {:message rejection-message}} evidence-store))

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

    ;; Execute phase has special floor enforcement
    (if (and (= current-phase :execute)
             (not (handle-execute-return! agent-id agent-output evidence-store)))
      ;; Re-dispatched — exit without advancing
      (println "[apm-conductor] Execute re-dispatched, waiting for next return")

      ;; For non-execute phases (or execute that passed the floor):
      (do
        (when (not= current-phase :execute)
          (swap! !apm-state assoc-in [:phase-notes current-phase]
                 (normalized-phase-notes current-problem current-phase output)))

    ;; Build phase-data and advance
    (let [accumulated-execute (get-in @!apm-state [:phase-notes :execute])
          execute-record (:execute-record @!apm-state)
          normalized-notes (case current-phase
                             :execute (or (execute-record->notes execute-record)
                                          (normalized-phase-notes current-problem :execute accumulated-execute))
                             (or (get-in @!apm-state [:phase-notes current-phase])
                                 (normalized-phase-notes current-problem current-phase output)))
          execute-artifacts (discover-lean-artifacts current-problem execute-start-ms accumulated-execute)
          execute-total-elapsed (- (System/currentTimeMillis) (or execute-start-ms phase-dispatch-ms 0))
          execute-notes (or (when (= current-phase :execute) normalized-notes)
                            (get-in @!apm-state [:phase-notes :execute])
                            (normalized-phase-notes current-problem :execute accumulated-execute))
          execute-sidecar (slurp-if-exists (phase-sidecar-path current-problem :execute))
          phase-data
          (case current-phase
            :observe   {:blocker-id "root" :notes normalized-notes}
            :propose   {:approach (or (extract-bold-section normalized-notes "THE KEY INSIGHT")
                                      normalized-notes)
                        :notes normalized-notes}
            :execute   {:artifacts execute-artifacts
                        :dependency-graph (or (some-> execute-record :stage2 parse-dependency-graph)
                                              (parse-dependency-graph execute-notes))
                        :lean-elapsed-ms execute-total-elapsed
                        :notes execute-notes
                        :lean-timed-out (when (>= execute-total-elapsed lean-floor-ms)
                                          (str "Timer expired after " (long (/ execute-total-elapsed 1000))
                                               "s — see notes for attempt details"))}
            :validate  {:validation-artifacts (validation-artifacts current-problem execute-artifacts)
                        :notes normalized-notes}
            :classify  {:classification (classify-status normalized-notes)
                        :rationale normalized-notes
                        :notes normalized-notes}
            :integrate {:rationale (or (extract-bold-section normalized-notes "Connections")
                                       normalized-notes)
                        :ledger-changes []
                        :notes normalized-notes
                        :arse-questions (parse-arse-questions
                                         (or normalized-notes execute-sidecar))})

          advance-result (.execute-tool backend :cycle-advance [pid cycle-id phase-data])]

      (if-not (:ok advance-result)
        (do
          (let [rejection-message (get-in advance-result [:error :message])]
            (log! {:event :phase-rejected :problem pid :phase current-phase
                   :message rejection-message})
            (println (str "[apm-conductor] " (name current-phase) " REJECTED: "
                          rejection-message))
            (handle-phase-rejection! agent-id rejection-message evidence-store)))

        ;; Advance succeeded — what's next?
        (let [next-idx (inc (.indexOf apm-phases current-phase))]
          (swap! !apm-state assoc :phase-failure-count 0)
          (if (< next-idx (count apm-phases))
            ;; Next agent phase
            (do
              (swap! !apm-state assoc :current-phase (nth apm-phases next-idx))
              (dispatch-phase! agent-id evidence-store))

            ;; All agent phases done — mechanical commit + gate-review
            (do
              (log! {:event :mechanical-gates :problem pid})
              (.execute-tool backend :cycle-advance [pid cycle-id {:saved? true}])
              (let [classification (classify-status (get-in @!apm-state [:phase-notes :classify]))
                    _ (.execute-tool backend :cycle-advance
                        [pid cycle-id {:gates-passed true :result-status classification}])
                    ;; Save
                    _ (let [cache @(.cache backend) state (get cache pid)]
                        (when state
                          (save-proof-state! (:id current-problem) state)))
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
  (let [{:keys [problem-bases]} @!apm-state
        explicit-base (first problem-bases)
        issue (if explicit-base
                {:problem-base explicit-base}
                (first (apm-queue/next-unprocessed evidence-store 1)))]
    (if-not issue
      (do (log! {:event :queue-empty})
          (stop-apm-conductor!))
      (let [base (:problem-base issue)
            pid (str "apm-" base)
            manifest (apm-queue/load-apm-manifest)
            problem (first (filter #(= base (:id %)) manifest))
            backend (pb/make-proof-backend
                      {:phase-validator apm-queue/apm-phase-validator})]

        ;; Init proof state
        (log! {:event :problem-start :problem pid})
        (let [load-result (.execute-tool backend :proof-load [pid])]
          (when-not (:ok load-result)
            (log! {:event :problem-start-failed
                   :problem pid
                   :message (get-in load-result [:error :message])})
            (throw (ex-info "APM proof state missing"
                            {:problem-id pid
                             :load-result load-result}))))
        (let [r (.execute-tool backend :cycle-begin [pid "root"])]
          (when-not (:ok r)
            (log! {:event :problem-start-failed
                   :problem pid
                   :message (get-in r [:error :message])})
            (throw (ex-info "APM cycle begin failed"
                            {:problem-id pid
                             :cycle-begin-result r})))
          (let [cid (get-in r [:result :cycle/id])]
            (apm-queue/emit-apm-evidence! evidence-store
              {:problem-id pid :problem-base base :subject (:subject problem)
               :session-id (str "apm-conductor-" pid) :event-tag :workflow-start})
            (swap! !apm-state assoc
                   :current-problem problem :current-phase :observe
                   :cycle-id cid :backend backend :phase-notes {}
                   :execute-record (empty-execute-record)
                   :problem-start-ms (System/currentTimeMillis)
                   :phase-dispatch-ms nil
                   :execute-start-ms nil
                   :phase-failure-count 0
                   :problem-bases (if explicit-base
                                    (vec (rest problem-bases))
                                    problem-bases))
            (dispatch-phase! agent-id evidence-store)))))))

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
  [evidence-store & {:keys [agent-id n problem-ids]
                     :or {agent-id "claude-1" n 40}}]
  (stop-apm-conductor!)

  ;; Init log file header
  (spit log-path (str ";; APM Conductor Log — " (Instant/now) "\n") :append true)

  (let [problem-bases (some->> problem-ids
                               (mapv #(if (str/starts-with? (str %) "apm-")
                                        (subs (str %) 4)
                                        (str %))))]
    (reset! !apm-state {:problems-done 0
                        :batch-results []
                        :target-n (or (some-> problem-bases count) n)
                        :problem-bases problem-bases})

    (reg/set-on-idle!
      (fn [idle-agent-id outcome]
        (when (and (= idle-agent-id agent-id) @!apm-conductor)
          (if (:ok outcome true)
            (let [output (:result outcome)]
              (advance-and-dispatch-next! agent-id output evidence-store))
            (handle-phase-failure! agent-id outcome evidence-store)))))

    (reset! !apm-conductor {:agent-id agent-id :started-at (System/currentTimeMillis)})
    (log! (cond-> {:event :conductor-started :agent agent-id :target (or (some-> problem-bases count) n)}
            (seq problem-bases) (assoc :problem-bases problem-bases)))

    (start-next-problem! agent-id evidence-store)
    {:ok true
     :agent-id agent-id
     :target (or (some-> problem-bases count) n)
     :problem-bases problem-bases}))
