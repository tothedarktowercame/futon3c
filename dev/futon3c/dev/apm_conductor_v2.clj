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

(def ^:private max-formal-kicks
  "Max re-dispatches for formal/informal alignment repair."
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
   "formal-alignment.edn must have exactly this kind of shape:\n"
   "```clojure\n"
   "{:main-claim {:informal-claim \"reader-facing statement\"\n"
   "             :formal-name \"main_theorem_name\"\n"
   "             :formal-target \"exact Lean theorem statement without the proof body\"\n"
   "             :sanity-check {:mentions-problem-objects? true\n"
   "                            :avoids-assuming-conclusion? true\n"
   "                            :meaningful-without-prose? true\n"
   "                            :notes \"One sentence explaining why this is the right theorem to build.\"}}\n"
   " :alignments [{:formal-name \"main_theorem_name\"\n"
   "              :formal-statement \"exact Lean theorem statement\"\n"
   "              :informal-clause \"which sentence in Stage 1 this certifies\"\n"
   "              :role :main-theorem}\n"
   "             {:formal-name \"helper_lemma_name\"\n"
   "              :formal-statement \"exact Lean helper statement\"\n"
   "              :informal-clause \"which bridge step it justifies\"\n"
   "              :role :helper-lemma}]}\n"
   "```\n"
   "The main theorem entry must align the Stage 1 claim with a real Lean declaration.\n"
   "Before you spend serious Lean time, do the HtDP target-theorem sanity check:\n"
   "- Does the theorem still mention the real hypotheses and objects from the problem?\n"
   "- Does it avoid assuming the desired conclusion?\n"
   "- Would it still count as meaningful progress if the prose were deleted?\n"
   "If the answer to any of these is no, repair the theorem statement first.\n"
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
        formal-alignment-path (apm-frames/formal-alignment-path frame-workspace)
        changelog-path (apm-frames/changelog-path frame-workspace)
        execute-notes (slurp-if-exists execute-path)
        proof-plan (read-edn-if-exists plan-path)
        formal-alignment (read-edn-if-exists formal-alignment-path)
        changelog (read-edn-if-exists changelog-path)
        execute-ok? (valid-execute-notes? execute-notes)
        plan-ok? (apm-queue/valid-proof-plan? proof-plan)
        formal-alignment-ok? (apm-queue/valid-formal-alignment? formal-alignment)
        changelog-ok? (apm-queue/valid-changelog? changelog)]
    {:execute-path execute-path
     :proof-plan-path plan-path
     :formal-alignment-path formal-alignment-path
     :changelog-path changelog-path
     :execute-notes execute-notes
     :proof-plan proof-plan
     :formal-alignment formal-alignment
     :changelog changelog
     :execute-ok? execute-ok?
     :proof-plan-ok? plan-ok?
     :formal-alignment-ok? formal-alignment-ok?
     :changelog-ok? changelog-ok?
     :full-record? (and execute-ok? plan-ok? formal-alignment-ok? changelog-ok?)}))

(defn- target-check-status
  [frame-workspace output]
  (let [record-status (frame-record-status frame-workspace)
        target-sanity (get-in record-status [:formal-alignment :main-claim :sanity-check])
        phase-data {:proof-plan (:proof-plan record-status)
                    :formal-alignment (:formal-alignment record-status)
                    :target-sanity target-sanity
                    :notes (or output "")}
        validation-error (apm-queue/apm-phase-validator :target-check phase-data nil)]
    (assoc record-status
           :target-sanity target-sanity
           :target-sanity-ok? (apm-queue/valid-target-sanity? target-sanity)
           :validation-error validation-error
           :ok? (nil? validation-error))))

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

(def ^:private placeholder-artifact-pattern
  #"(?is)\bplaceholder\b|:\s*True\s*:=\s*(?:by\s+)?trivial\b|lemma\s+\S+\s*\([^)]*\)\s*:\s*True\s*:=|theorem\s+\S+\s*\([^)]*\)\s*:\s*True\s*:=")

(defn- declaration-lines
  [text]
  (->> (str/split-lines (or text ""))
       (map str/trim)
       (filter #(re-find #"^(?:lemma|theorem)\b" %))))

(defn- declaration-name
  [line]
  (some-> (re-find #"^(?:lemma|theorem)\s+([^\s\[:(]+)" (or line "")) second))

(defn- artifact-placeholder?
  [path]
  (try
    (let [text (slurp path)
          decls (declaration-lines text)]
      (or (boolean (re-find placeholder-artifact-pattern text))
          (empty? decls)
          (not-any? #(not (re-find #":\s*True\b" %)) decls)))
    (catch Exception _
      true)))

(defn- normalize-formal-text [s]
  (-> (or s "")
      (str/replace #"\s+" " ")
      str/trim))

(defn- artifact-declaration-names
  [path]
  (try
    (->> (declaration-lines (slurp path))
         (keep declaration-name)
         set)
    (catch Exception _
      #{})))

(defn- artifact-contains-formal-target?
  [path formal-name formal-target]
  (try
    (let [text (normalize-formal-text (slurp path))
          formal-name (str/trim (str (or formal-name "")))
          formal-target (normalize-formal-text formal-target)]
      (and (not (str/blank? formal-name))
           (not (str/blank? formal-target))
           (str/includes? text formal-name)
           (str/includes? text formal-target)))
    (catch Exception _
      false)))

(defn- formalization-status
  [artifacts formal-alignment]
  (let [main-claim (:main-claim formal-alignment)
        main-name (:formal-name main-claim)
        main-target (:formal-target main-claim)
        artifact-summaries (mapv (fn [path]
                                   {:path path
                                    :placeholder? (artifact-placeholder? path)
                                    :declaration-names (artifact-declaration-names path)})
                                 artifacts)
        decl-names (apply clojure.set/union #{} (map :declaration-names artifact-summaries))
        non-placeholder? (and (seq artifacts)
                              (every? (complement :placeholder?) artifact-summaries))
        substantive-artifacts (filter (fn [{:keys [placeholder? declaration-names]}]
                                        (and (not placeholder?) (seq declaration-names)))
                                      artifact-summaries)
        substantive-decl-names (apply clojure.set/union #{} (map :declaration-names substantive-artifacts))
        main-name-present? (contains? decl-names main-name)
        main-target-present? (some #(artifact-contains-formal-target? % main-name main-target) artifacts)]
    {:artifacts-present? (seq artifacts)
     :declaration-names decl-names
     :substantive-declaration-names substantive-decl-names
     :substantive-artifact-count (count substantive-artifacts)
     :substantive? (boolean (seq substantive-artifacts))
     :has-declarations? (boolean (seq decl-names))
     :non-placeholder? (boolean non-placeholder?)
     :main-name-present? (boolean main-name-present?)
     :main-target-present? (boolean main-target-present?)
     :aligned? (boolean (and main-name-present? main-target-present?))
     :meaningful? (boolean
                   (and (seq artifacts)
                        (seq substantive-artifacts)))}))

(defn- extract-stage-fragment
  [text n]
  (some-> (re-find (re-pattern
                     (str "(?is)Stage\\s+" n "\\s+[—-].*?\\n[-]+\\n\\n(.*?)(?=\\n\\nStage\\s+\\d+\\s+[—-]|\\z)"))
                   (or text ""))
          second
          trim-to-nil))

(defn- formal-attempt-trace?
  [record-status output]
  (let [execute-notes (:execute-notes record-status)
        stage3 (or (extract-stage-fragment execute-notes 3)
                   (extract-stage-fragment output 3))
        changelog (:changelog record-status)
        non-init-entries (remove #(= :workspace-initialized (:kind %)) changelog)
        trace-pattern #"(?i)\b(tried|search|searched|mathlib|api|goal|blocked|blocker|lemma|theorem|exact\?|apply\?|grep|lake build)\b"]
    (boolean
      (or (and stage3
               (> (count stage3) 40)
               (re-find trace-pattern stage3))
          (some (fn [entry]
                  (and (string? (:summary entry))
                       (re-find trace-pattern (:summary entry))))
                non-init-entries)))))

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
         "   Before real Lean work, state the intended main theorem in formal-alignment.edn and pass\n"
         "   the three-question HtDP sanity check: right objects/hypotheses, no assumed conclusion,\n"
         "   meaningful even without the prose.\n"
         "   Write real proofs, not scaffolds. If a sorry is genuinely needed (Mathlib gap),\n"
         "   explain exactly what's missing and what would close it.\n"
         "5. **What connects** — where else this technique appears, exam-day field kit\n\n"
         "A problem is not complete until the frame-local record is populated.\n"
         "Before replying, update all four frame artifacts with real content:\n"
         "- `execute.md` with non-placeholder Stage 1-4 text\n"
         "- `proof-plan.edn` with goal, terms, strategy, and `:stage-status`\n"
         "- `formal-alignment.edn` with the main claim aligned to exact Lean declarations\n"
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
                "- formal-alignment.edn: " (:formal-alignment-path frame-context) "\n"
                "- changelog.edn: " (:changelog-path frame-context) "\n"
                "- execute.md: " (:execute-notes-path frame-context) "\n"
                "- shared extension root: " (:shared-extension-root frame-context) "\n"
                "Do not use shared scratch paths like `ApmCanaries/Current.lean`.\n\n"))
         "If no explicit frame paths are given, write Lean files to " lean-proofs-root "/" id "/Main.lean\n\n"

         "Time budget: use the full budget unless you close the formal work early and have already populated the frame record.\n"
         "Quality over speed. A single well-proved problem\n"
         "is worth more than a scaffold.\n")))

(defn- make-target-check-prompt
  [cid problem tex-body]
  (let [{:keys [id subject year session subpart-count subparts]} problem]
    (str "Problem: apm-" id " (" (get subject-names subject (name subject))
         ", " year ", " (if (= session :fall) "Fall" "Spring") ")\n\n"
         "```latex\n" tex-body "\n```\n\n"
         (when (and subparts (pos? subpart-count))
           (str "Sub-parts: " (str/join ", " (map :label subparts)) "\n\n"))
         "You are in TARGET-CHECK.\n\n"
         "Goal: decide whether you are building the right formal program before serious Lean work starts.\n"
         "Populate the authoritative machine-readable artifacts now:\n"
         "- `proof-plan.edn`\n"
         "- `formal-alignment.edn`\n"
         "and reply inline with the target-sanity analysis.\n\n"
         (when-let [frame-context (apm-frames/prompt-context (current-frame-workspace cid problem))]
           (str "Use the isolated frame workspace for this problem:\n"
                "- workspace root: " (:workspace-root frame-context) "\n"
                "- proof-plan.edn: " (:proof-plan-path frame-context) "\n"
                "- formal-alignment.edn: " (:formal-alignment-path frame-context) "\n"
                "- execute.md: " (:execute-notes-path frame-context) "\n\n"))
         "Hard gate: do not proceed with a tautology, a theorem that assumes the conclusion,\n"
         "or a helper theorem unrelated to the main claim.\n\n"
         "Reply with:\n"
         "1. `TARGET SANITY CHECK` answering:\n"
         "   - mentions-problem-objects?\n"
         "   - avoids-assuming-conclusion?\n"
         "   - meaningful-without-prose?\n"
         "   - notes\n"
         "2. fenced `PROOF-PLAN.EDN`\n"
         "3. fenced `FORMAL-ALIGNMENT.EDN`\n\n"
         htdp-reference-card
         "\nUse these worked examples as style anchors:\n"
         "- " worked-example-root "/a02J04\n"
         "- " worked-example-root "/a02J04-full\n")))

(defn- make-target-check-kick-prompt
  [problem remaining-ms target-status previous-output failure-count]
  (str "TARGET-CHECK for apm-" (:id problem) " is still invalid. "
       (long (/ remaining-ms 1000)) " seconds remain.\n\n"
       "You must fix the target artifacts before any real Lean loop can begin.\n\n"
       "Current issue:\n"
       (or (get-in target-status [:validation-error :error :message])
           "The target-check artifacts are incomplete or inconsistent.")
       "\n\n"
       "Write or repair:\n"
       "- proof-plan.edn\n"
       "- formal-alignment.edn\n"
       "- inline TARGET SANITY CHECK\n\n"
       htdp-reference-card
       "\nPrevious output (summary):\n"
       (subs previous-output 0 (min 1500 (count previous-output)))
       "\n\nTarget-check repair attempt #" failure-count ".\n"))

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
       (when-not (:formal-alignment-ok? record-status)
         (str "- formal-alignment.edn must map the main informal claim to exact Lean declarations at "
              (:formal-alignment-path record-status) "\n"))
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

(defn- make-formal-kick-prompt
  [problem frame-workspace remaining-ms formal-status record-status previous-output formal-kick-count]
  (str "The current Lean artifact for apm-" (:id problem) " does not yet certify the problem you claim to have solved. "
       (long (/ remaining-ms 1000)) " seconds remain.\n\n"
       "This is not a record-format issue anymore. You must either write a real theorem/lemma or leave an honest attempt trace.\n\n"
       "Current formal status:\n"
       "- declaration names seen: " (pr-str (vec (:declaration-names formal-status))) "\n"
       "- non-placeholder Lean artifact? " (:non-placeholder? formal-status) "\n"
       "- main theorem name present? " (:main-name-present? formal-status) "\n"
       "- main theorem statement aligned? " (:main-target-present? formal-status) "\n\n"
       "Required fixes:\n"
       "1. Put at least one real `theorem`/`lemma` declaration in the frame-local Lean file if you can.\n"
       "2. If the actual formal theorem differs slightly from the planned target, lightly revise the informal/formal alignment to match what you really proved.\n"
       "3. If you still cannot close the main theorem, record an honest formal attempt trace in Stage 3 / changelog:\n"
       "   - what theorem you tried to state,\n"
       "   - what search/API route you attempted,\n"
       "   - why you concluded the route was blocked.\n"
       "4. Re-run the HtDP target-theorem sanity check: right objects/hypotheses, no assumed conclusion, meaningful without prose.\n"
       "5. Keep execute.md / proof-plan.edn / changelog.edn in sync with the actual formal work.\n\n"
       "formal-alignment.edn path: " (:formal-alignment-path record-status) "\n"
       "Lean main file: " (get-in frame-workspace [:artifacts :lean-main]) "\n\n"
       htdp-reference-card
       "\nReply only after you have either written a substantive theorem/lemma or recorded the failed formal route concretely.\n\n"
       "Previous output (summary):\n"
       (subs previous-output 0 (min 1500 (count previous-output)))
       "\n\nFormal alignment repair attempt #" formal-kick-count ".\n"))

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
                sorry-kick-count record-kick-count formal-kick-count problems-done batch-results target-n
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
        formal-kicks (or formal-kick-count 0)
        record-status (frame-record-status (:frame-workspace (conductor-state cid)))
        formal-status (formalization-status artifacts (:formal-alignment record-status))
        attempt-trace? (formal-attempt-trace? record-status all-output)
        remaining-ms (- (current-problem-timeout-ms cid) total-elapsed)
        substantive-partial-message "Recorded substantive Lean subtheorems without a fully aligned main theorem"
        attempt-trace-message "Recorded formal attempt trace without substantive Lean theorem"]

    (log! {:event :solve-return :problem pid
           :dispatch-elapsed-ms dispatch-elapsed
           :total-elapsed-ms total-elapsed
           :artifact-count (count artifacts)
           :sorry? sorry?
           :sorry-kick-count kick-count
           :record-kick-count record-kicks
           :formal-kick-count formal-kicks
           :full-record? (:full-record? record-status)
           :meaningful-formal? (:meaningful? formal-status)
           :substantive-formal? (:substantive? formal-status)
           :aligned-formal? (:aligned? formal-status)
           :attempt-trace? attempt-trace?})

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
                               ", formal-alignment-ok=" (:formal-alignment-ok? record-status)
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

      ;; The record is populated, but the Lean artifact still does not contain
      ;; substantive formal work and there is no honest attempt trace yet.
      (and (:full-record? record-status)
           (not (:meaningful? formal-status))
           (not attempt-trace?))
      (if (< formal-kicks max-formal-kicks)
        (let [new-count (inc formal-kicks)
              frame-workspace (:frame-workspace (conductor-state cid))]
          (assoc-state! cid :formal-kick-count new-count
                            :dispatch-start-ms (System/currentTimeMillis))
          (log! {:event :formal-kick
                 :problem pid
                 :formal-kick-count new-count
                 :remaining-ms remaining-ms
                 :message (str "main-name-present=" (:main-name-present? formal-status)
                               ", main-target-present=" (:main-target-present? formal-status)
                               ", declaration-count=" (count (:declaration-names formal-status))
                               ", attempt-trace?=" attempt-trace?)})
          (dispatch! agent-id
                     (make-formal-kick-prompt current-problem frame-workspace remaining-ms formal-status record-status output new-count)
                     (max 60000 remaining-ms)))
        (let [done (inc problems-done)
              results (conj (or batch-results [])
                            {:ok false :problem-id pid
                             :classification :abandoned
                             :message "No substantive Lean theorem and no formal attempt trace"
                             :elapsed-ms total-elapsed})]
          (assoc-state! cid :problems-done done :batch-results results)
          (log! {:event :problem-abandoned :problem pid
                 :message "No substantive Lean theorem and no formal attempt trace"})
          (if (>= done target-n)
            (do (log! {:event :batch-complete :done done})
                (stop-apm-conductor-v2! agent-id))
            (start-next-problem! cid agent-id evidence-store))))

      ;; Lean clean — no sorry, artifacts exist
      (and (seq artifacts) (not sorry?) (:meaningful? formal-status))
      (do
        (when-not (:aligned? formal-status)
          (log! {:event :alignment-warning
                 :problem pid
                 :message "Accepted proved result with non-exact post-Lean alignment; revise informal statement lightly if needed."}))
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

      ;; Sorry-kick budget exhausted — save as partial if the Lean artifact is
      ;; substantive, even if the post-hoc alignment is imperfect.
      (and (>= kick-count max-sorry-kicks) (:meaningful? formal-status))
      (do
        (when-not (:aligned? formal-status)
          (log! {:event :alignment-warning
                 :problem pid
                 :message "Accepted partial result with non-exact post-Lean alignment; revise informal statement lightly if needed."}))
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

      ;; No substantive theorem, but there is an honest Stage 3 attempt trace.
      attempt-trace?
      (do
        (let [message (if (:substantive? formal-status)
                        substantive-partial-message
                        attempt-trace-message)]
        (log! {:event :problem-complete :problem pid
               :classification "partial"
               :total-elapsed-ms total-elapsed
               :message message})
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
              done (inc problems-done)
              results (conj (or batch-results [])
                            {:ok true :problem-id pid
                             :classification :partial
                             :extraction extraction
                             :elapsed-ms total-elapsed
                             :message message})]
          (assoc-state! cid :problems-done done :batch-results results)
          (if (>= done target-n)
            (do (log! {:event :batch-complete :done done})
                (stop-apm-conductor-v2! agent-id))
            (start-next-problem! cid agent-id evidence-store)))))

      ;; Sorry budget exhausted, but still no meaningful formal artifact.
      (>= kick-count max-sorry-kicks)
      (let [done (inc problems-done)
            results (conj (or batch-results [])
                          {:ok false :problem-id pid
                           :classification :abandoned
                           :message "No meaningful formalization after sorry budget exhausted"
                           :elapsed-ms total-elapsed})]
        (assoc-state! cid :problems-done done :batch-results results)
        (log! {:event :problem-abandoned :problem pid
               :message "No meaningful formalization after sorry budget exhausted"})
        (if (>= done target-n)
          (do (log! {:event :batch-complete :done done})
              (stop-apm-conductor-v2! agent-id))
          (start-next-problem! cid agent-id evidence-store)))

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

(defn- handle-target-check-return!
  [cid agent-id agent-output evidence-store]
  (let [{:keys [current-problem problem-start-ms dispatch-start-ms
                accumulated-output frame-workspace failure-count]} (conductor-state cid)
        pid (str "apm-" (:id current-problem))
        now-ms (System/currentTimeMillis)
        dispatch-elapsed (- now-ms (or dispatch-start-ms now-ms))
        total-elapsed (- now-ms (or problem-start-ms now-ms))
        output (or agent-output "")
        all-output (str (or accumulated-output "") "\n\n---\n\n" output)
        target-status (target-check-status frame-workspace output)
        failures (or failure-count 0)
        remaining-ms (- (current-problem-timeout-ms cid) total-elapsed)]
    (assoc-state! cid :accumulated-output all-output)
    (log! {:event :target-check-return
           :problem pid
           :dispatch-elapsed-ms dispatch-elapsed
           :total-elapsed-ms total-elapsed
           :proof-plan-ok? (:proof-plan-ok? target-status)
           :formal-alignment-ok? (:formal-alignment-ok? target-status)
           :target-sanity-ok? (:target-sanity-ok? target-status)
           :ok? (:ok? target-status)})
    (if (:ok? target-status)
      (let [tex-body (apm-queue/load-problem-tex (:id current-problem))]
        (assoc-state! cid
                      :current-phase :solve
                      :failure-count 0
                      :dispatch-start-ms (System/currentTimeMillis))
        (log! {:event :target-check-complete :problem pid})
        (dispatch! agent-id
                   (make-solve-prompt cid current-problem tex-body)
                   (current-problem-timeout-ms cid)))
      (let [new-failures (inc failures)]
        (assoc-state! cid :failure-count new-failures)
        (log! {:event :target-check-rejected
               :problem pid
               :failure-count new-failures
               :message (get-in target-status [:validation-error :error :message])})
        (if (<= new-failures 3)
          (let [tex-body (apm-queue/load-problem-tex (:id current-problem))]
            (assoc-state! cid :dispatch-start-ms (System/currentTimeMillis))
            (dispatch! agent-id
                       (make-target-check-kick-prompt current-problem remaining-ms target-status output new-failures)
                       (min 300000 (max 60000 remaining-ms))))
          (let [{:keys [problems-done batch-results target-n]} (conductor-state cid)
                done (inc problems-done)
                results (conj (or batch-results [])
                              {:ok false :problem-id pid
                               :classification :abandoned
                               :message "Target-check never converged to a valid formal target"
                               :elapsed-ms total-elapsed})]
            (assoc-state! cid :problems-done done :batch-results results)
            (log! {:event :problem-abandoned :problem pid
                   :message "Target-check never converged to a valid formal target"})
            (if (>= done target-n)
              (do (log! {:event :batch-complete :done done})
                  (stop-apm-conductor-v2! agent-id))
              (start-next-problem! cid agent-id evidence-store))))))))

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
          (let [{:keys [current-problem current-phase]} (conductor-state cid)
                tex-body (apm-queue/load-problem-tex (:id current-problem))]
            (assoc-state! cid :dispatch-start-ms (System/currentTimeMillis))
            (dispatch! agent-id
                       (if (= current-phase :target-check)
                         (make-target-check-prompt cid current-problem tex-body)
                         (make-solve-prompt cid current-problem tex-body))
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
                      :current-phase :target-check
                      :frame-workspace frame-workspace
                      :backend backend
                      :accumulated-output ""
                      :sorry-kick-count 0
                      :record-kick-count 0
                      :formal-kick-count 0
                      :failure-count 0
                      :problem-start-ms (System/currentTimeMillis)
                      :dispatch-start-ms (System/currentTimeMillis)
                      :problem-bases (if explicit-base
                                       (vec (rest problem-bases))
                                       problem-bases))
        (dispatch! agent-id
                   (make-target-check-prompt cid problem tex-body)
                   (min 300000 (current-problem-timeout-ms cid)))))))

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
;; Post-hoc reclassification
;; =============================================================================

(defn reclassify-problem
  "Re-evaluate a single problem's classification by checking the actual Lean
   artifacts on disk. Returns {:id :old :new :reason :decl-count :sorry?}."
  [problem-base]
  (let [pid (str "apm-" problem-base)
        ;; Find frame workspace Lean files
        uc (str/upper-case (subs problem-base 0 1))
        rest-id (subs problem-base 1)
        base-dir (io/file "/home/joe/code/apm-lean/ApmCanaries/Frames" (str uc rest-id))
        frame-dir (when (.exists base-dir) (first (.listFiles base-dir)))
        ;; Also check legacy path
        legacy-main (io/file lean-proofs-root problem-base "Main.lean")
        artifacts (cond-> []
                    frame-dir
                    (into (->> [(io/file frame-dir "Main.lean")
                                (io/file frame-dir "Scratch.lean")]
                               (filter #(.exists %))
                               (map #(.getAbsolutePath %))))
                    (.exists legacy-main)
                    (conj (.getAbsolutePath legacy-main)))
        ;; Load existing proof state
        state-path (str proof-state-root "/apm-" problem-base ".edn")
        old-state (when (.exists (io/file state-path))
                    (try (clojure.edn/read-string (slurp state-path))
                      (catch Exception _ nil)))
        old-classification (or (:proof/classification old-state) :unknown)
        ;; Re-evaluate
        formal-status (when (seq artifacts) (formalization-status artifacts nil))
        sorry? (boolean (some artifact-has-sorry? artifacts))
        meaningful? (boolean (:meaningful? formal-status))
        new-classification (cond
                             (and meaningful? (not sorry?)) :proved
                             (and meaningful? sorry?) :partial
                             :else :partial)
        upgraded? (and (= old-classification :partial) (= new-classification :proved))]
    {:id problem-base
     :pid pid
     :old old-classification
     :new new-classification
     :upgraded? upgraded?
     :sorry? sorry?
     :meaningful? meaningful?
     :decl-count (count (:declaration-names formal-status))
     :reason (cond
               upgraded? "Zero sorry with substantive declarations — reclassified as proved"
               (and (not meaningful?) (not sorry?)) "No substantive Lean declarations"
               sorry? (str "Sorry present in artifacts")
               :else "No change")}))

(defn reclassify-batch!
  "Re-evaluate and update proof states for a list of problem-bases.
   Upgrades partial→proved where Lean artifacts have zero sorry and
   substantive declarations. Returns a summary."
  [problem-bases]
  (let [results (mapv reclassify-problem problem-bases)
        upgraded (filter :upgraded? results)
        ;; Update proof state files for upgraded problems
        _ (doseq [{:keys [id]} upgraded]
            (let [path (str proof-state-root "/apm-" id ".edn")]
              (when (.exists (io/file path))
                (let [state (clojure.edn/read-string (slurp path))
                      updated (assoc state :proof/classification :proved
                                          :proof/reclassified-at (str (Instant/now))
                                          :proof/reclassification-reason
                                          "Post-hoc: zero sorry with substantive declarations")]
                  (spit path (pr-str updated))))))
        summary {:total (count results)
                 :upgraded (count upgraded)
                 :still-partial (count (filter #(= :partial (:new %)) results))
                 :upgraded-ids (mapv :id upgraded)}]
    (log! {:event :reclassification-complete :summary summary})
    (println (str "[apm-v2] Reclassified: " (:upgraded summary) " upgraded to proved out of " (:total summary)))
    {:summary summary :details results}))

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
              (if (= :target-check (:current-phase (conductor-state cid)))
                (handle-target-check-return! cid agent-id (:result outcome) evidence-store)
                (handle-solve-return! cid agent-id (:result outcome) evidence-store))
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
