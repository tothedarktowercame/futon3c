(ns futon3c.dev.apm-conductor-v2
  "APM conductor v2 — single-dispatch, post-hoc extraction.

   Design difference from v1:
   - v1 dispatches 6 separate phase prompts, enforces format at each gate.
   - v2 dispatches ONE 'solve and explain' prompt. The agent does mathematics
     in whatever natural form it wants. The conductor extracts structured data
     (phases, dep graph, ArSE questions) from the output after the fact.

   Sorry-kick loop: if the Lean proof has sorry, v2 re-dispatches with a
   focused 'close these sorries' prompt (up to max-sorry-kicks). After
   sorry-kick-threshold, switches to ArSE-kick diagnostic prompt.

   Post-hoc extraction: reuses v1's parsers (parse-dependency-graph,
   parse-arse-questions, extract-stage, classify-status) on the agent's
   natural output. Missing structure is logged but does not block advancement.

   A/B testing: start-apm-conductor-v2! has the same interface as v1's
   start-apm-conductor! so the same problem sets can be run through both."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-dispatch :as apm-dispatch]
            [futon3c.dev.apm-frames :as apm-frames]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.agency.registry :as reg])
  (:import [java.time Instant]))

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:private max-sorry-kicks
  "Max re-dispatches for sorry closure."
  4)

(def ^:private max-record-kicks
  "Max re-dispatches for frame-record normalization."
  2)

(def ^:private arse-kick-threshold
  "After this many sorry-kicks, switch to diagnostic QP prompt."
  2)

(def ^:private default-problem-timeout-ms
  "Default overall problem timeout (20 minutes)."
  (* 20 60 1000))

(def ^:private lean-proofs-root
  "/home/joe/code/apm-lean/lean-proofs")

(def ^:private proof-state-root
  "/home/joe/code/futon3c/data/proof-state")

(def ^:private subject-names
  {:analysis "Analysis"
   :algebra "Algebra"
   :topology "Topology"
   :applied "Applied/Functional Analysis"})

;; =============================================================================
;; State (per-agent conductor instances)
;; =============================================================================

(defonce ^{:doc "Map of {conductor-id -> {:agent-id ... :version :v2 :started-at ...}}."} !conductor
  (atom {}))

(defonce ^{:doc "Map of {conductor-id -> per-run state}."} !state
  (atom {}))

(defn- conductor-id [agent-id]
  [:apm-v2 agent-id])

(defn state-for-agent
  "Diagnostic helper for REPL inspection."
  [agent-id]
  (get @!state (conductor-id agent-id)))

(defn active-conductors
  "Diagnostic helper for REPL inspection."
  []
  @!conductor)

(defn- conductor-running?
  [cid]
  (contains? @!conductor cid))

(defn- conductor-state
  [cid]
  (get @!state cid))

(defn- assoc-state!
  [cid & kvs]
  (swap! !state update cid #(apply assoc (or % {}) kvs)))

(defn- update-state!
  [cid f & args]
  (swap! !state update cid #(apply f (or % {}) args)))

;; =============================================================================
;; Logging (same format as v1, different prefix)
;; =============================================================================

(def ^:private log-path
  "/home/joe/code/futon3c/data/apm-conductor-v2-log.edn")

(def ^:private worked-example-root
  "/home/joe/code/proof_peripheral/worked-solutions")

(def ^:private htdp-reference-card
  (str
   "HtDP reference card for frame artifacts:\n"
   "\n"
   "proof-plan.edn must have exactly this kind of shape:\n"
   "```clojure\n"
   "{:goal \"...\"\n"
   " :terms [{:name \"...\" :meaning \"...\" :needed-because \"...\"}]\n"
   " :strategy [{:id :s1\n"
   "             :formal-dependency \"...\"\n"
   "             :informal-dependency \"...\"\n"
   "             :why-this-now \"...\"\n"
   "             :lean-target \"...\"\n"
   "             :mathlib-status \"...\"}]\n"
   " :stage-status {:stage1 :done :stage2 :done :stage3 :in-progress :stage4 :pending}}\n"
   "```\n"
   "Do not use `:checkpoints`. Do not use bare symbols or plain string lists for terms/strategy.\n"
   "\n"
   "changelog.edn must be a vector of maps like:\n"
   "```clojure\n"
   "[{:kind :stage1-completed\n"
   "  :summary \"...\"\n"
   "  :full-record? true\n"
   "  :sorry? false\n"
   "  :fully-closed? false}]\n"
   "```\n"
   "Every changelog entry must include `:full-record?`, `:sorry?`, and `:fully-closed?`.\n"
   "\n"
   "execute.md must contain real Stage 1, Stage 2, Stage 3, and Stage 4 sections with no template placeholders.\n"))

(defn- log!
  [entry]
  (let [entry (assoc entry :at (str (Instant/now)))]
    (spit log-path (str (pr-str entry) "\n") :append true)
    (println (str "[apm-v2] " (:event entry) " " (:problem entry "")
                  " " (:phase entry "") " " (:elapsed-ms entry "")
                  (when-let [m (:message entry)] (str " — " m))))))

;; =============================================================================
;; Utilities (reused from v1 patterns)
;; =============================================================================

(defn- trim-to-nil [s]
  (let [s (some-> s str/trim)]
    (when (seq s) s)))

(defn- slurp-if-exists [path]
  (when path
    (let [f (io/file path)]
      (when (.exists f) (slurp f)))))

(defn- proof-state-path [problem-base]
  (str proof-state-root "/apm-" problem-base ".edn"))

(defn- save-proof-state! [problem-base state]
  (let [path (proof-state-path problem-base)
        parent (.getParentFile (io/file path))]
    (when parent (.mkdirs parent))
    (spit path (pr-str state)))
  state)

(defn- read-edn-if-exists [path]
  (when-let [text (slurp-if-exists path)]
    (try (edn/read-string text)
         (catch Exception _ nil))))

(defn- placeholder-execute-notes? [text]
  (boolean
   (or (str/blank? (str/trim (or text "")))
       (re-find #"\[Fill in" text)
       (re-find #"\[Record formal dependency" text)
       (re-find #"\[Back-port" text))))

(defn- valid-execute-notes? [text]
  (and (not (placeholder-execute-notes? text))
       (str/includes? text "Stage 1")
       (str/includes? text "Stage 2")
       (str/includes? text "Stage 3")
       (str/includes? text "Stage 4")
       (> (count (str/trim text)) 400)))

(defn- frame-record-status
  [frame-workspace]
  (let [execute-path (apm-frames/workspace-sidecar-path frame-workspace :execute)
        plan-path (apm-frames/proof-plan-path frame-workspace)
        changelog-path (apm-frames/changelog-path frame-workspace)
        execute-notes (slurp-if-exists execute-path)
        proof-plan (read-edn-if-exists plan-path)
        changelog (read-edn-if-exists changelog-path)
        execute-ok? (valid-execute-notes? execute-notes)
        plan-ok? (apm-queue/valid-proof-plan? proof-plan)
        changelog-ok? (apm-queue/valid-changelog? changelog)]
    {:execute-path execute-path
     :proof-plan-path plan-path
     :changelog-path changelog-path
     :execute-notes execute-notes
     :proof-plan proof-plan
     :changelog changelog
     :execute-ok? execute-ok?
     :proof-plan-ok? plan-ok?
     :changelog-ok? changelog-ok?
     :full-record? (and execute-ok? plan-ok? changelog-ok?)}))

;; =============================================================================
;; Lean artifact inspection (same as v1)
;; =============================================================================

(defn- problem-lean-dir [problem]
  (io/file lean-proofs-root (:id problem)))

(defn- current-frame-workspace
  [cid problem]
  (let [state (conductor-state cid)]
    (when (= (:id problem) (get-in state [:current-problem :id]))
      (:frame-workspace state))))

(defn- mentioned-lean-artifacts [output]
  (->> (re-seq #"(?:/home/joe/code/apm-lean/)?(?:lean-proofs|ApmCanaries/Frames|ApmCanaries/Local)/[^\s\]\"']+\.lean" (or output ""))
       (map #(if (str/starts-with? % "/") %
               (str "/home/joe/code/apm-lean/" %)))
       distinct vec))

(defn- recent-lean-artifacts [cid problem execute-start-ms]
  (let [dir (problem-lean-dir problem)
        frame-artifacts (apm-frames/workspace-lean-artifacts (current-frame-workspace cid problem))
        threshold-ms (long (max 0 (- (or execute-start-ms 0) 2000)))]
    (->> (concat
          frame-artifacts
          (when (.exists dir)
            (->> (file-seq dir)
                 (filter #(.isFile ^java.io.File %))
                 (filter #(str/ends-with? (.getName ^java.io.File %) ".lean"))
                 (filter #(>= (.lastModified ^java.io.File %) threshold-ms))
                 (map #(.getAbsolutePath ^java.io.File %)))))
         distinct vec)))

(defn- discover-lean-artifacts [cid problem execute-start-ms output]
  (let [mentioned (mentioned-lean-artifacts output)
        recent (recent-lean-artifacts cid problem execute-start-ms)]
    (->> (concat mentioned recent)
         distinct
         (filter #(try (.exists (io/file %)) (catch Exception _ false)))
         vec)))

(defn- artifact-has-sorry? [path]
  (try (boolean (re-find #"(?i)\bsorry\b" (slurp path)))
    (catch Exception _ true)))

(defn- has-sorry? [output]
  (boolean (re-find #"(?i)\bsorry\b" (or output ""))))

;; =============================================================================
;; Post-hoc extraction (reuses v1 parser patterns)
;; =============================================================================

;; Import v1's parsers by requiring the namespace — they're pure functions
;; on text, no state dependency. We call them via the ns alias.
;; (For now, inline lightweight versions to keep v2 self-contained.)

(defn- extract-section [text re]
  (some-> (re-find re (or text "")) second trim-to-nil))

(defn- extract-bold-section [text title]
  (extract-section text
    (re-pattern
     (str "(?ms)^\\*\\*" (java.util.regex.Pattern/quote title)
          "\\*\\*\\s*(.*?)(?=^\\*\\*|\\z)"))))

(defn- classify-status [text]
  (cond
    (re-find #"(?i)\bproved\b" (or text "")) :proved
    (re-find #"(?i)\bbuild-failed\b" (or text "")) :inconclusive
    (re-find #"(?i)\btimed-out\b" (or text "")) :partial
    :else :partial))

(defn- extract-phases
  "Extract phase-like sections from natural agent output.
   Handles both structured (### OBSERVE) and natural (## 1. Why it's hard) formats.
   Returns a map of phase keywords to extracted text."
  [output]
  (let [try-section
        (fn [& labels]
          (some (fn [label]
                  (let [escaped (java.util.regex.Pattern/quote label)]
                    (or
                     ;; ### HEADING or ## HEADING (exact)
                     (extract-section output
                       (re-pattern (str "(?ims)###?\\s*" escaped "\\s*\n(.*?)(?=###?\\s|\\z)")))
                     ;; ## N. HEADING (numbered)
                     (extract-section output
                       (re-pattern (str "(?ims)###?\\s*\\d+\\.?\\s*" escaped ".*?\n(.*?)(?=###?\\s*\\d|\\z)")))
                     ;; **HEADING** (bold)
                     (extract-bold-section output label)
                     ;; **HEADING:** (bold with colon, e.g. **Connections:**)
                     (extract-section output
                       (re-pattern (str "(?ims)\\*\\*" escaped "\\s*:?\\*\\*\\s*:?\\s*\n?(.*?)(?=\\*\\*[A-Z]|###?\\s|\\z)"))))))
                labels))]
    (into {}
      (filter (comp some? val)
        {:observe   (try-section "OBSERVE" "What is really being asked" "Why it's hard"
                                 "Why it.s hard" "WHY IT IS HARD")
         :propose   (try-section "PROPOSE" "THE KEY INSIGHT" "Key Insight"
                                 "The key insight" "Key insight")
         :execute   (try-section "EXECUTE" "THE CLEAN PROOF" "Clean Proof"
                                 "A complete proof" "Complete proof" "Proof")
         :validate  (try-section "VALIDATE" "Non-circularity" "Validation"
                                 "Lean status" "Lean 4")
         :classify  (try-section "CLASSIFY" "Classification" "CONFIDENCE INVERSION"
                                 "Confidence inversion" "Confidence Inversion")
         :integrate (try-section "INTEGRATE" "Connections" "EXAM-DAY FIELD KIT"
                                 "Exam-day field kit" "What connects"
                                 "What Connects")}))))

(defn- extract-dep-field
  "Extract a dependency-graph field by trying multiple label prefixes."
  [body prefixes]
  (some (fn [prefix]
          (let [escaped (java.util.regex.Pattern/quote prefix)
                patterns [(re-pattern (str "(?im)^\\s*-\\s+\\*\\*" escaped "\\*\\*\\s*:\\s*(.+)$"))
                          (re-pattern (str "(?im)^\\s*-\\s+\\*" escaped "\\*\\s*:\\s*(.+)$"))
                          (re-pattern (str "(?im)^\\s*" escaped "\\s*:\\s*(.+)$"))]]
            (some #(extract-section body %) patterns)))
        prefixes))

(defn- parse-dependency-entry [entry-text]
  (let [[_ lemma body] (re-find #"(?ms)^\s*\d+\.\s+\*\*([^*]+)\*\*\s*\n(.*)\z" entry-text)
        full (trim-to-nil entry-text)
        body (or body "")]
    {:lemma (or (trim-to-nil lemma) "unparsed-lemma")
     :formal-dependency (or (extract-dep-field body ["Formal dependency"]) full)
     :informal-dependency (extract-dep-field body ["Informal dependency" "Informal strategy"
                                                    "Recognition heuristic"])
     :why-this-now (extract-dep-field body ["Why this becomes thinkable here" "Why thinkable here"
                                             "Why thinkable" "Why now" "Why here"
                                             "Why relevant here" "Local cue"])
     :lean-type (or (extract-dep-field body ["Lean target/type" "Lean target" "Lean type" "Type"]) full)
     :notes full}))

(defn- parse-dependency-graph [text]
  (let [entries (->> (re-seq #"(?ms)^\s*\d+\.\s+\*\*[^*]+\*\*.*?(?=^\s*\d+\.\s+\*\*|\z)" (or text ""))
                     (map parse-dependency-entry)
                     vec)]
    (when (seq entries) entries)))

(defn- parse-arse-questions [text]
  (let [section (or (extract-section text #"(?ims)\*\*ArSE Questions\*\*\s*(.*)\z")
                    (extract-section text #"(?ims)###\s*ArSE(?:\s+QUESTIONS)?\s*(.*)\z")
                    text)]
    (->> (re-seq #"(?ms)^\s*(\d+)\.\s+\*(.+?)\*\s*(.*?)(?=^\s*\d+\.\s+\*|\z)" (or section ""))
         (map-indexed
          (fn [idx [_ _ title body]]
            (let [body (or body "")
                  q (extract-section body #"(?ims)\*\*Q:\*\*\s*(.+?)(?=\n\s*\*\*A:\*\*|\z)")
                  a (or (extract-section body #"(?ims)\*\*A:\*\*\s*(.+?)(?=\n\s*\*\*[A-Z]|^\s*\d+\.\s+\*|\z)")
                        (when-not (re-find #"(?ims)\*\*Q:\*\*" body)
                          (trim-to-nil (str/replace body #"^\s*[:\-]\s*" ""))))]
              {:question (or q (trim-to-nil title))
               :answer a})))
         (filter #(and (:question %) (:answer %)))
         vec)))

(defn- run-extraction
  "Run all post-hoc extraction passes on the agent's combined output.
   Returns a map of extracted structured data. Missing fields are nil, not errors.
   Dep-graph extraction scopes to the execute/proof phase section to avoid
   parsing 'Connections' numbered lists as dependency entries."
  [output]
  (let [phases (extract-phases output)
        ;; Scope dep-graph to execute phase text, falling back to full output
        execute-text (or (:execute phases) output)
        dep-graph (parse-dependency-graph execute-text)
        ;; Scope ArSE to the ArSE section explicitly
        arse-section (or (extract-section output #"(?ims)###?\s*ArSE.*?\n(.*)\z")
                         (extract-section output #"(?ims)\*\*ArSE[^*]*\*\*\s*\n?(.*)\z"))
        arse (when arse-section (parse-arse-questions arse-section))
        classification (classify-status output)]
    {:phases phases
     :dependency-graph dep-graph
     :arse-questions arse
     :classification classification
     :phase-count (count phases)
     :dep-graph-count (count dep-graph)
     :arse-count (count arse)}))

;; =============================================================================
;; Prompts
;; =============================================================================

(defn- make-solve-prompt
  "Single 'solve and explain' prompt. The agent does everything in one shot."
  [cid problem tex-body]
  (let [{:keys [id subject year session subpart-count subparts]} problem]
    (str "Problem: apm-" id " (" (get subject-names subject (name subject))
         ", " year ", " (if (= session :fall) "Fall" "Spring") ")\n\n"
         "```latex\n" tex-body "\n```\n\n"
         (when (and subparts (pos? subpart-count))
           (str "Sub-parts: " (str/join ", " (map :label subparts)) "\n\n"))

         "## Your task\n\n"
         "Solve this problem completely. Write for a strong undergrad studying for prelims.\n\n"

         "Include:\n"
         "1. **Why it's hard** — where students get stuck, what trap the problem sets\n"
         "2. **The key insight** — the single idea that unlocks it\n"
         "3. **A complete proof** — readable, line-by-line, not a sketch\n"
         "4. **Lean 4 formalization** — prove it in Lean with Mathlib. Build with `lake build`.\n"
         "   Use the HtDP recipe: type signature → search Mathlib → sketch composition → wire it.\n"
         "   Write real proofs, not scaffolds. If a sorry is genuinely needed (Mathlib gap),\n"
         "   explain exactly what's missing and what would close it.\n"
         "5. **What connects** — where else this technique appears, exam-day field kit\n\n"
         "A problem is not complete until the frame-local record is populated.\n"
         "Before replying, update all three frame artifacts with real content:\n"
         "- `execute.md` with non-placeholder Stage 1-4 text\n"
         "- `proof-plan.edn` with goal, terms, strategy, and `:stage-status`\n"
         "- `changelog.edn` with concrete progress entries\n\n"
         htdp-reference-card "\n"
         "Style anchors:\n"
         "- " worked-example-root "/a02J04\n"
         "- " worked-example-root "/a02J04-full\n\n"
         (when-let [frame-context (apm-frames/prompt-context (current-frame-workspace cid problem))]
           (str "Use the isolated frame workspace for this problem:\n"
                "- workspace root: " (:workspace-root frame-context) "\n"
                "- Lean module root: " (:module-root frame-context) "\n"
                "- Lean main file: " (:lean-main-path frame-context) "\n"
                "- Lean scratch file: " (:lean-scratch-path frame-context) "\n"
                "- proof-plan.edn: " (:proof-plan-path frame-context) "\n"
                "- changelog.edn: " (:changelog-path frame-context) "\n"
                "- execute.md: " (:execute-notes-path frame-context) "\n"
                "- shared extension root: " (:shared-extension-root frame-context) "\n"
                "Do not use shared scratch paths like `ApmCanaries/Current.lean`.\n\n"))
         "If no explicit frame paths are given, write Lean files to " lean-proofs-root "/" id "/Main.lean\n\n"

         "Time budget: use the full budget unless you close the formal work early and have already populated the frame record.\n"
         "Quality over speed. A single well-proved problem\n"
         "is worth more than a scaffold.\n")))

(defn- make-sorry-kick-prompt
  "Re-dispatch for sorry closure."
  [problem remaining-ms previous-output sorry-count]
  (str "Your Lean proof for apm-" (:id problem) " has sorry instances.\n"
       (long (/ remaining-ms 1000)) " seconds remain.\n\n"
       "Close the sorries. Use HtDP:\n"
       "1. Pick the most promising sorry\n"
       "2. Search Mathlib (`exact?`, `apply?`, grep)\n"
       "3. Sketch the composition\n"
       "4. Wire it: `lake build`, read error, fix, repeat\n\n"
       "Previous output (summary):\n"
       (subs previous-output 0 (min 1500 (count previous-output)))
       "\n"))

(defn- make-arse-kick-prompt
  "Diagnostic prompt when agent is stuck in sorry loop."
  [problem remaining-ms previous-output sorry-count]
  (str "Your Lean proof for apm-" (:id problem) " still has sorry after "
       sorry-count " attempts. " (long (/ remaining-ms 1000)) " seconds remain.\n\n"
       "STOP repeating the same approach. Diagnose what's blocking you:\n\n"
       "1. **What is the exact Lean goal state** for the most promising sorry?\n"
       "   Copy it from `lake build` output.\n\n"
       "2. **What is the gap** between what you have and what the goal needs?\n"
       "   Missing Mathlib lemma? Type mismatch? Universe issue?\n\n"
       "3. **What did you assume would work that didn't?**\n\n"
       "Then choose:\n"
       "- **Pivot**: different proof strategy\n"
       "- **Extend**: write a helper lemma bridging the gap\n"
       "- **Accept**: this is a genuine Mathlib boundary — explain what's missing\n\n"
       "A well-diagnosed sorry with a boundary explanation is a real result.\n\n"
       "Previous output (summary):\n"
       (subs previous-output 0 (min 1500 (count previous-output)))
       "\n"))

(defn- make-record-kick-prompt
  "Prompt for filling the frame-local HtDP record after mathematical work exists
   but the workspace artifacts are still placeholders or empty."
  [problem remaining-ms record-status previous-output record-kick-count]
  (str "The frame-local record for apm-" (:id problem) " is incomplete. "
       (long (/ remaining-ms 1000)) " seconds remain.\n\n"
       "You must populate the workspace artifacts before this problem can count as complete.\n"
       "Do not leave template placeholders. Do not just mention the paths in your reply.\n\n"
       "Invalid or missing artifacts:\n"
       (when-not (:execute-ok? record-status)
         (str "- execute.md needs real Stage 1-4 content at "
              (:execute-path record-status) "\n"))
       (when-not (:proof-plan-ok? record-status)
         (str "- proof-plan.edn needs goal, terms, strategy, and :stage-status at "
              (:proof-plan-path record-status) "\n"))
       (when-not (:changelog-ok? record-status)
         (str "- changelog.edn needs concrete progress entries at "
              (:changelog-path record-status) "\n"))
       "\n"
       htdp-reference-card
       "\nUse these worked examples as the style anchors:\n"
       "- " worked-example-root "/a02J04\n"
       "- " worked-example-root "/a02J04-full\n\n"
       "After updating the files, reply with a short confirmation of what you wrote.\n\n"
       "Previous output (summary):\n"
       (subs previous-output 0 (min 1500 (count previous-output)))
       "\n\nRecord normalization attempt #" record-kick-count ".\n"))

;; =============================================================================
;; Core dispatch loop
;; =============================================================================

(declare start-next-problem!)
(declare stop-apm-conductor-v2!)

(defn- dispatch!
  "Dispatch a prompt to the agent. Non-blocking."
  [agent-id prompt timeout-ms]
  (log! {:event :dispatch :agent agent-id :timeout-ms timeout-ms})
  (future
    (try
      (let [result (reg/invoke-agent! agent-id prompt timeout-ms)]
        (when-not (:ok result)
          (println (str "[apm-v2] dispatch FAILED: " (:error result)))))
      (catch Exception e
        (println (str "[apm-v2] dispatch exception: " (.getMessage e)))))))

(defn- current-problem-timeout-ms [cid]
  (or (:problem-timeout-ms (conductor-state cid)) default-problem-timeout-ms))

(defn- problem-timed-out? [cid]
  (when-let [start-ms (:problem-start-ms (conductor-state cid))]
    (> (- (System/currentTimeMillis) start-ms) (current-problem-timeout-ms cid))))

(defn- handle-solve-return!
  "Handle return from the solve or sorry-kick dispatch."
  [cid agent-id agent-output evidence-store]
  (let [{:keys [current-problem problem-start-ms dispatch-start-ms
                sorry-kick-count record-kick-count problems-done batch-results target-n
                accumulated-output backend]} (conductor-state cid)
        pid (str "apm-" (:id current-problem))
        now-ms (System/currentTimeMillis)
        dispatch-elapsed (- now-ms (or dispatch-start-ms now-ms))
        total-elapsed (- now-ms (or problem-start-ms now-ms))
        output (or agent-output "")

        ;; Accumulate all output from all dispatches for this problem
        all-output (str (or accumulated-output "") "\n\n---\n\n" output)
        _ (assoc-state! cid :accumulated-output all-output)

        ;; Check Lean status — only check artifacts, not prose (which may discuss sorry)
        artifacts (discover-lean-artifacts cid current-problem problem-start-ms all-output)
        sorry? (boolean (some artifact-has-sorry? artifacts))
        kick-count (or sorry-kick-count 0)
        record-kicks (or record-kick-count 0)
        record-status (frame-record-status (:frame-workspace (conductor-state cid)))
        remaining-ms (- (current-problem-timeout-ms cid) total-elapsed)]

    (log! {:event :solve-return :problem pid
           :dispatch-elapsed-ms dispatch-elapsed
           :total-elapsed-ms total-elapsed
           :artifact-count (count artifacts)
           :sorry? sorry?
           :sorry-kick-count kick-count
           :record-kick-count record-kicks
           :full-record? (:full-record? record-status)})

    (cond
      ;; Problem timeout
      (problem-timed-out? cid)
      (do
        (log! {:event :problem-timed-out :problem pid
               :total-elapsed-ms total-elapsed})
        (let [extraction (run-extraction all-output)
              frame-workspace (:frame-workspace (conductor-state cid))
              done (inc problems-done)
              results (conj (or batch-results [])
                            {:ok false :problem-id pid
                             :classification :timed-out
                             :extraction extraction
                             :frame-id (:frame/id frame-workspace)
                             :elapsed-ms total-elapsed})]
          (assoc-state! cid :problems-done done :batch-results results)
          (if (>= done target-n)
            (do (log! {:event :batch-complete :done done})
                (stop-apm-conductor-v2! agent-id))
            (start-next-problem! cid agent-id evidence-store))))

      ;; The frame-local HtDP record is incomplete. If the mathematical work is
      ;; otherwise ready to classify, spend bounded extra clicks normalizing the
      ;; record instead of silently calling the problem solved.
      (and (not (:full-record? record-status))
           (or (and (seq artifacts) (not sorry?))
               (>= kick-count max-sorry-kicks)))
      (if (< record-kicks max-record-kicks)
        (let [new-count (inc record-kicks)]
          (assoc-state! cid :record-kick-count new-count
                            :dispatch-start-ms (System/currentTimeMillis))
          (log! {:event :record-kick
                 :problem pid
                 :record-kick-count new-count
                 :remaining-ms remaining-ms
                 :message (str "execute-ok=" (:execute-ok? record-status)
                               ", plan-ok=" (:proof-plan-ok? record-status)
                               ", changelog-ok=" (:changelog-ok? record-status))})
          (dispatch! agent-id
                     (make-record-kick-prompt current-problem remaining-ms record-status output new-count)
                     (max 60000 remaining-ms)))
        (let [done (inc problems-done)
              results (conj (or batch-results [])
                            {:ok false :problem-id pid
                             :classification :abandoned
                             :message "Frame record incomplete after normalization retries"
                             :elapsed-ms total-elapsed})]
          (assoc-state! cid :problems-done done :batch-results results)
          (log! {:event :problem-abandoned :problem pid
                 :message "Frame record incomplete after normalization retries"})
          (if (>= done target-n)
            (do (log! {:event :batch-complete :done done})
                (stop-apm-conductor-v2! agent-id))
            (start-next-problem! cid agent-id evidence-store))))

      ;; Lean clean — no sorry, artifacts exist
      (and (seq artifacts) (not sorry?))
      (do
        (log! {:event :problem-complete :problem pid
               :classification "proved"
               :total-elapsed-ms total-elapsed
               :sorry-kicks kick-count})
        (let [extraction (run-extraction all-output)
              frame-workspace (:frame-workspace (conductor-state cid))
              ;; Save proof state
              _ (when backend
                  (.execute-tool backend :proof-load [pid])
                  (let [r (.execute-tool backend :cycle-begin [pid "root"])]
                    (when (:ok r)
                      (save-proof-state! (:id current-problem)
                        {:proof/problem-id pid
                         :proof/classification :proved
                         :proof/output all-output
                         :proof/extraction extraction
                         :proof/frame-workspace frame-workspace
                         :proof/artifacts (mapv str artifacts)
                         :proof/elapsed-ms total-elapsed}))))
              ;; Evidence
              _ (apm-queue/emit-apm-evidence! evidence-store
                  {:problem-id pid :problem-base (:id current-problem)
                   :subject (:subject current-problem)
                   :session-id (str "apm-v2-" pid)
                   :event-tag :workflow-complete
                   :classification "proved"})
              done (inc problems-done)
              results (conj (or batch-results [])
                            {:ok true :problem-id pid
                             :classification :proved
                             :extraction extraction
                             :sorry-kicks kick-count
                             :elapsed-ms total-elapsed})]
          (assoc-state! cid :problems-done done :batch-results results)
          (println (str "[apm-v2] === " pid " PROVED ("
                        (long (/ total-elapsed 1000)) "s, "
                        kick-count " sorry-kicks) ==="))
          (if (>= done target-n)
            (do (log! {:event :batch-complete :done done})
                (stop-apm-conductor-v2! agent-id))
            (start-next-problem! cid agent-id evidence-store))))

      ;; Sorry-kick budget exhausted — save as partial
      (>= kick-count max-sorry-kicks)
      (do
        (log! {:event :problem-complete :problem pid
               :classification "partial"
               :total-elapsed-ms total-elapsed
               :sorry-kicks kick-count
               :message "Sorry-kick budget exhausted"})
        (let [extraction (run-extraction all-output)
              frame-workspace (:frame-workspace (conductor-state cid))
              _ (save-proof-state! (:id current-problem)
                  {:proof/problem-id pid
                   :proof/classification :partial
                   :proof/output all-output
                   :proof/extraction extraction
                   :proof/frame-workspace frame-workspace
                   :proof/artifacts (mapv str artifacts)
                   :proof/elapsed-ms total-elapsed})
              _ (apm-queue/emit-apm-evidence! evidence-store
                  {:problem-id pid :problem-base (:id current-problem)
                   :subject (:subject current-problem)
                   :session-id (str "apm-v2-" pid)
                   :event-tag :workflow-complete
                   :classification "partial"})
              done (inc problems-done)
              results (conj (or batch-results [])
                            {:ok true :problem-id pid
                             :classification :partial
                             :extraction extraction
                             :sorry-kicks kick-count
                             :elapsed-ms total-elapsed})]
          (assoc-state! cid :problems-done done :batch-results results)
          (println (str "[apm-v2] === " pid " PARTIAL ("
                        (long (/ total-elapsed 1000)) "s, "
                        kick-count " sorry-kicks) ==="))
          (if (>= done target-n)
            (do (log! {:event :batch-complete :done done})
                (stop-apm-conductor-v2! agent-id))
            (start-next-problem! cid agent-id evidence-store))))

      ;; Sorry present, budget remains — kick
      :else
      (let [new-count (inc kick-count)
            use-arse? (>= kick-count arse-kick-threshold)
            prompt-fn (if use-arse? make-arse-kick-prompt make-sorry-kick-prompt)]
        (assoc-state! cid :sorry-kick-count new-count
                          :dispatch-start-ms (System/currentTimeMillis))
        (log! {:event (if use-arse? :arse-kick :sorry-kick)
               :problem pid
               :sorry-kick-count new-count
               :remaining-ms remaining-ms})
        (println (str "[apm-v2] " (if use-arse? "ArSE KICK" "sorry-kick")
                      " #" new-count " for " pid
                      " (" (long (/ remaining-ms 1000)) "s remaining)"))
        (dispatch! agent-id
                   (prompt-fn current-problem remaining-ms output new-count)
                   (max 60000 remaining-ms))))))

(defn- handle-failure!
  "Handle agent invoke failure."
  [cid agent-id outcome evidence-store]
  (let [{:keys [current-problem failure-count]} (conductor-state cid)
        pid (some-> current-problem :id (#(str "apm-" %)))
        failures (inc (or failure-count 0))]
    (assoc-state! cid :failure-count failures)
    (log! {:event :dispatch-failure :problem pid :failure-count failures
           :message (str (:error outcome))})
    (if (<= failures 3)
      ;; Retry after delay
      (future
        (Thread/sleep 30000)
        (when (conductor-running? cid)
          (let [{:keys [current-problem]} (conductor-state cid)
                tex-body (apm-queue/load-problem-tex (:id current-problem))]
            (assoc-state! cid :dispatch-start-ms (System/currentTimeMillis))
            (dispatch! agent-id
                       (make-solve-prompt cid current-problem tex-body)
                       (current-problem-timeout-ms cid)))))
      ;; Budget exhausted — skip
      (let [state (conductor-state cid)
            frame-workspace (:frame-workspace state)
            done (inc (:problems-done state))
            results (conj (or (:batch-results state) [])
                          {:ok false :problem-id pid :classification :abandoned
                           :frame-id (:frame/id frame-workspace)})]
        (assoc-state! cid :problems-done done :batch-results results)
        (log! {:event :problem-abandoned :problem pid})
        (if (>= done (:target-n state))
          (do (log! {:event :batch-complete :done done})
              (stop-apm-conductor-v2! agent-id))
          (start-next-problem! cid agent-id evidence-store))))))

;; =============================================================================
;; Problem lifecycle
;; =============================================================================

(defn- start-next-problem!
  [cid agent-id evidence-store]
  (let [{:keys [problem-bases]} (conductor-state cid)
        explicit-base (first problem-bases)
        issue (if explicit-base
                {:problem-base explicit-base}
                (first (apm-queue/next-unprocessed evidence-store 1)))]
    (if-not issue
      (do (log! {:event :queue-empty})
          (stop-apm-conductor-v2! agent-id))
      (let [base (:problem-base issue)
            pid (str "apm-" base)
            manifest (apm-queue/load-apm-manifest)
            problem (first (filter #(= base (:id %)) manifest))
            tex-body (apm-queue/load-problem-tex base)
            frame-workspace (apm-frames/init-frame-workspace!
                             {:problem-base base
                              :conductor-tag "apm-v2"})
            backend (pb/make-proof-backend {})]
        (log! {:event :problem-start :problem pid})
        (let [receipt-path (apm-frames/emit-frame-receipt!
                            {:problem-base base
                             :frame-workspace frame-workspace
                             :frame-label (str "APM v2 frame for " base)})]
          (log! {:event :frame-workspace-ready :problem pid :message receipt-path}))
        (apm-queue/emit-apm-evidence! evidence-store
          {:problem-id pid :problem-base base :subject (:subject problem)
           :session-id (str "apm-v2-" pid) :event-tag :workflow-start})
        (assoc-state! cid
                      :current-problem problem
                      :frame-workspace frame-workspace
                      :backend backend
                      :accumulated-output ""
                      :sorry-kick-count 0
                      :record-kick-count 0
                      :failure-count 0
                      :problem-start-ms (System/currentTimeMillis)
                      :dispatch-start-ms (System/currentTimeMillis)
                      :problem-bases (if explicit-base
                                       (vec (rest problem-bases))
                                       problem-bases))
        (dispatch! agent-id
                   (make-solve-prompt cid problem tex-body)
                   (current-problem-timeout-ms cid))))))

;; =============================================================================
;; Triage — effort estimation pre-pass
;; =============================================================================

(def ^:private triage-path
  "/home/joe/code/futon3c/data/apm-triage.edn")

(defn- make-triage-prompt
  "Prompt for batch triage. Given N problems, classify each as quick/medium/hard."
  [problems]
  (str "You are triaging " (count problems) " graduate math prelim problems for Lean 4\n"
       "formalization difficulty. For each problem, estimate how hard it will be to\n"
       "produce a COMPLETE Lean 4 proof with Mathlib (not a scaffold — a real proof).\n\n"

       "Lanes:\n"
       "- **quick**: Standard Mathlib lemmas exist, straightforward wiring. Under 5 min.\n"
       "- **medium**: Mathlib has the pieces but composition is nontrivial. 5-20 min.\n"
       "- **hard**: Genuine Mathlib gap, custom lemmas or new theory needed. 20+ min.\n\n"

       "For each problem, respond with exactly one line and nothing else:\n"
       "  <problem-id> <lane> <one-sentence reason>\n"
       "No bullets. No numbering. No markdown table. No headings. No code fences.\n"
       "Use lane names in lowercase: quick, medium, or hard.\n\n"

       "Example:\n"
       "  a01J01 quick Schur test — MeasureTheory.integral_mul_le covers it\n"
       "  a02J04 hard Pushforward measure Radon-Nikodym — needs custom decomposition lemma\n\n"

       "Problems:\n\n"
       (str/join "\n\n"
         (map (fn [{:keys [id subject year] :as p}]
                (let [tex (apm-queue/load-problem-tex id)]
                  (str "### " id " (" (get subject-names subject (name subject)) ", " year ")\n"
                       "```latex\n" tex "\n```")))
              problems))
       "\n"))

(defn- normalize-triage-line
  [line]
  (-> (or line "")
      str/trim
      (str/replace #"^\s*(?:[-*•]+|\d+[.)])\s*" "")
      (str/replace #"`" "")
      (str/replace #"\*\*" "")
      (str/replace #"\s*[|]\s*" " ")
      (str/replace #"\s+" " ")))

(defn- parse-triage-line
  "Parse one triage line: 'a01J01 quick Reason here' -> {:id :lane :reason}"
  [line]
  (let [line (normalize-triage-line line)]
    (when-let [[_ id lane reason]
               (or (re-find #"(?i)^\s*([a-z]\d\d[A-Z]\d\d)\s+[:\-]?\s*(quick|medium|hard)\s*[:\-]?\s+(.+)$" line)
                   (re-find #"(?i)^\s*([a-z]\d\d[A-Z]\d\d)\s*\|\s*(quick|medium|hard)\s*\|\s*(.+)$" line))]
      {:id id :lane (keyword (str/lower-case lane)) :reason (str/trim reason)})))

(defn- parse-triage-output
  "Parse triage agent output into a map of {problem-id {:lane :reason}}."
  [output]
  (->> (str/split-lines (or output ""))
       (keep parse-triage-line)
       (reduce (fn [m {:keys [id lane reason]}]
                 (assoc m id {:lane lane :reason reason}))
               {})))

(defn load-triage
  "Load triage manifest from disk. Returns {problem-id {:lane :reason}} or nil."
  []
  (when-let [text (slurp-if-exists triage-path)]
    (try (clojure.edn/read-string text) (catch Exception _ nil))))

(defn save-triage!
  "Save triage manifest to disk."
  [triage-map]
  (spit triage-path (pr-str triage-map))
  triage-map)

(defn triage-summary
  "Summarize a triage manifest by lane."
  [triage-map manifest]
  (let [by-lane (group-by (comp :lane val) triage-map)]
    {:quick (count (get by-lane :quick))
     :medium (count (get by-lane :medium))
     :hard (count (get by-lane :hard))
     :unclassified (- (count manifest)
                      (count (get by-lane :quick))
                      (count (get by-lane :medium))
                      (count (get by-lane :hard)))
     :total (count manifest)}))

(defn unclassified-problem-ids
  "Return manifest problem ids not yet present in the saved triage map."
  []
  (let [triage (or (load-triage) {})
        manifest (apm-queue/load-apm-manifest)
        classified (set (keys triage))]
    (->> manifest
         (remove #(contains? classified (:id %)))
         (mapv :id))))

(defn run-triage!
  "Run triage pre-pass: dispatch all problems (or a subset) to an agent for
   effort estimation. Returns the triage manifest.

   Usage:
     (run-triage! :agent-id \"claude-1\")
     (run-triage! :agent-id \"claude-1\" :subject :analysis)
     (run-triage! :agent-id \"claude-1\" :problem-ids [\"a01J01\" \"a02J04\"])"
  [& {:keys [agent-id subject problem-ids batch-size max-retries force?]
      :or {agent-id "claude-1" batch-size 50 max-retries 2 force? false}}]
  (let [existing (or (load-triage) {})
        explicit-ids (when problem-ids (set problem-ids))
        existing-ids (set (keys existing))
        target-manifest (cond->> (apm-queue/load-apm-manifest)
                          subject (filter #(= subject (:subject %)))
                          explicit-ids (filter #(contains? explicit-ids (:id %))))
        manifest (cond->> target-manifest
                   (not force?) (remove #(contains? existing-ids (:id %))))
        ;; Batch to avoid context overflow
        batches (partition-all batch-size manifest)
        results (atom existing)]
    (log! {:event :triage-started
           :total (count target-manifest)
           :pending (count manifest)
           :batches (count batches)
           :agent agent-id})
    (doseq [[batch-idx batch] (map-indexed vector batches)]
      (loop [attempt 1
             pending batch]
        (when (seq pending)
          (let [prompt (make-triage-prompt pending)
                response (reg/invoke-agent! agent-id prompt (* 5 60 1000))]
            (if (:ok response)
              (let [parsed (parse-triage-output (:result response))
                    pending-ids (set (map :id pending))
                    parsed-ids (set (keys parsed))
                    missing-ids (set/difference pending-ids parsed-ids)
                    missing (filterv #(contains? missing-ids (:id %)) pending)]
                (swap! results merge parsed)
                (log! {:event :triage-batch-complete
                       :batch (inc batch-idx)
                       :attempt attempt
                       :requested (count pending)
                       :classified (count parsed)
                       :missing (count missing)})
                (cond
                  (empty? missing) nil
                  (< attempt max-retries)
                  (do
                    (log! {:event :triage-batch-retry
                           :batch (inc batch-idx)
                           :attempt (inc attempt)
                           :reason :parse-miss
                           :remaining (count missing)})
                    (recur (inc attempt) missing))
                  :else
                  (log! {:event :triage-batch-incomplete
                         :batch (inc batch-idx)
                         :attempt attempt
                         :remaining (count missing)
                         :missing-ids (mapv :id missing)})))
              (if (< attempt max-retries)
                (do
                  (log! {:event :triage-batch-retry
                         :batch (inc batch-idx)
                         :attempt (inc attempt)
                         :reason :invoke-failure
                         :error (str (:error response))
                         :remaining (count pending)})
                  (recur (inc attempt) pending))
                (log! {:event :triage-batch-failed
                       :batch (inc batch-idx)
                       :attempt attempt
                       :remaining (count pending)
                       :error (str (:error response))})))))))
    (let [final @results]
      (save-triage! final)
      (log! {:event :triage-complete :summary (triage-summary final target-manifest)})
      (println (str "[apm-v2] Triage complete: " (triage-summary final target-manifest)))
      final)))

(defn problems-by-lane
  "Filter the manifest to problems in a given lane. Requires triage to have run."
  [lane]
  (let [triage (load-triage)
        manifest (apm-queue/load-apm-manifest)]
    (when triage
      (->> manifest
           (filter #(= lane (get-in triage [(:id %) :lane])))
           (mapv :id)))))

;; =============================================================================
;; Start / Stop
;; =============================================================================

(defn stop-apm-conductor-v2!
  ([] (doseq [agent-id (map :agent-id (vals @!conductor))]
        (stop-apm-conductor-v2! agent-id)))
  ([agent-id]
   (let [cid (conductor-id agent-id)]
     (when (conductor-running? cid)
       (apm-dispatch/deregister-conductor! cid)
       (swap! !conductor dissoc cid)
       (swap! !state dissoc cid)
       (log! {:event :conductor-stopped :agent agent-id})
       (println (str "[apm-v2] Stopped " agent-id "."))))))

(defn start-apm-conductor-v2!
  "Start the v2 APM conductor.

   Single-dispatch architecture: one 'solve and explain' prompt per problem,
   sorry-kick loop for Lean closure, post-hoc extraction of structured data.

   Same interface as v1's start-apm-conductor! for A/B testing.
  Additional :lane option filters to a triage lane (:quick/:medium/:hard)."
  [evidence-store & {:keys [agent-id n problem-ids lane problem-timeout-ms]
                     :or {agent-id "codex-1"
                          n 40
                          problem-timeout-ms default-problem-timeout-ms}}]
  (let [cid (conductor-id agent-id)]
    (stop-apm-conductor-v2! agent-id)
  (spit log-path (str ";; APM Conductor v2 Log — " (Instant/now) "\n") :append true)

    (let [problem-bases (or (some->> problem-ids
                                     (mapv #(if (str/starts-with? (str %) "apm-")
                                              (subs (str %) 4)
                                              (str %))))
                            (when lane (problems-by-lane lane)))]
      (swap! !state assoc cid {:problems-done 0
                               :batch-results []
                               :target-n (or (some-> problem-bases count) n)
                               :problem-timeout-ms problem-timeout-ms
                               :problem-bases problem-bases})

      (apm-dispatch/register-conductor! cid agent-id
        (fn [idle-agent-id outcome]
          (when (and (= idle-agent-id agent-id) (conductor-running? cid))
            (if (:ok outcome true)
              (handle-solve-return! cid agent-id (:result outcome) evidence-store)
              (handle-failure! cid agent-id outcome evidence-store)))))

      (swap! !conductor assoc cid {:agent-id agent-id
                                   :version :v2
                                   :started-at (System/currentTimeMillis)})
      (log! (cond-> {:event :conductor-started :version :v2
                     :agent agent-id
                     :target (or (some-> problem-bases count) n)}
              (seq problem-bases) (assoc :problem-bases problem-bases)))

      (start-next-problem! cid agent-id evidence-store)
      {:ok true
       :version :v2
       :agent-id agent-id
       :target (or (some-> problem-bases count) n)
       :problem-bases problem-bases})))
