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

(def ^:private apm-problem-manifest-path
  "/home/joe/code/futon3c/data/apm-problem-manifest.tsv")

(def ^:private cleanup-log-path
  "/home/joe/code/futon3c/data/apm-codex-cleanup-log.edn")

(def ^:private cleanup-triage-output-dir
  "/home/joe/code/futon3c/data/apm-codex-cleanup-replies")

(def ^:private cleanup-close-output-dir
  "/home/joe/code/futon3c/data/apm-codex-close-replies")

(def ^:private cleanup-scaffold-output-dir
  "/home/joe/code/futon3c/data/apm-codex-scaffold-replies")

(def ^:private dojo-handoff-output-dir
  "/home/joe/code/futon3c/data/apm-lean-dojo-handoffs")

(def ^:private manifest-builder-path
  "/home/joe/code/futon3c/dev/build_apm_problem_manifest.sh")

(defn- log! [entry]
  (let [entry (assoc entry :at (str (Instant/now)))]
    (spit log-path (str (pr-str entry) "\n") :append true)))

(defn- load-problem-manifest-tsv []
  (let [lines (-> apm-problem-manifest-path slurp str/split-lines)
        header (str/split (first lines) #"\t")]
    (mapv (fn [line]
            (zipmap (map keyword header) (str/split line #"\t" -1)))
          (rest lines))))

(defn- not-blank [s]
  (when-not (str/blank? (str s))
    s))

(defn- started-proof-row?
  "Rows eligible for cleanup must already have a Main.lean proof file."
  [row]
  (or (not-blank (:latest_canary_main_path row))
      (= "true" (:has_legacy_main row))))

(defn- machine-processing-available-row?
  [row]
  (str/blank? (:machine_processing_state row)))

(defn- cleanup-complete-row? [row]
  (= "complete" (:cleanup_state row)))

(defn- cleanup-blocked-row? [row]
  (= "blocked" (:cleanup_state row)))

(defn- dojo-scaffold-row? [row]
  (= "needs_codex_scaffold" (:dojo_state row)))

(defn- cleanup-mode-allows-row?
  [row mode requested? include-blocked?]
  (cond
    (= mode :scaffold)
    (if requested?
      true
      (dojo-scaffold-row? row))
    include-blocked? true
    (not (cleanup-blocked-row? row)) true
    (= mode :triage) requested?
    :else false))

(defn cleanup-candidates
  "Ordered manifest rows for already-started Lean proofs.
   Mode:
   - :triage allows explicitly requested blocked rows
   - :close skips blocked rows unless :include-blocked? true"
  [& {:keys [n problem-ids include-blocked? mode]
      :or {include-blocked? false
           mode :triage}}]
  (let [requested? (seq problem-ids)
        problem-id-set (set problem-ids)
        rows (load-problem-manifest-tsv)
        selected (filter
                  (fn [row]
                    (and (started-proof-row? row)
                         (machine-processing-available-row? row)
                         (not (cleanup-complete-row? row))
                         (or (not requested?)
                             (contains? problem-id-set (:id row)))
                         (cleanup-mode-allows-row? row mode requested? include-blocked?)))
                  rows)]
    (vec (if n (take n selected) selected))))

(defn- preferred-main-path [row]
  (or (not-blank (:latest_canary_main_path row))
      (not-blank (:legacy_main_path row))))

(defn- candidate-main-paths [row]
  (->> [(not-blank (:latest_canary_main_path row))
        (not-blank (:legacy_main_path row))]
       (remove nil?)
       distinct
       vec))

(defn- maybe-section [label path]
  (when-let [path (not-blank path)]
    (str "- " label ": " path "\n")))

(defn- read-tex-body [row]
  (when-let [path (not-blank (:tex_path row))]
    (when (.exists (io/file path))
      (slurp path))))

(defn- refresh-problem-manifest! []
  (let [{:keys [exit out err]} (shell/sh "bash" manifest-builder-path)]
    (when-not (zero? exit)
      (println (str "[apm-v3:cleanup] manifest refresh failed: "
                    (str/trim (or err out)))))
    {:ok (zero? exit) :out out :err err}))

(defn- cleanup-mode-label [mode]
  (case mode
    :close "close"
    :scaffold "scaffold"
    "cleanup"))

(defn- cleanup-output-dir-for-mode [mode]
  (case mode
    :close cleanup-close-output-dir
    :scaffold cleanup-scaffold-output-dir
    cleanup-triage-output-dir))

(defn- cleanup-output-path-for-mode [mode problem-id]
  (io/file (cleanup-output-dir-for-mode mode) (str "apm-" problem-id ".md")))

(defn- save-cleanup-output! [mode problem-id output]
  (let [dir (io/file (cleanup-output-dir-for-mode mode))]
    (.mkdirs dir)
    (spit (io/file dir (str "apm-" problem-id ".md")) output)))

(def ^:private dojo-skeletal-markers
  ["currently empty"
   "no prior lean development"
   "no existing lean development"
   "none of the"
   "only contains the helper"
   "only contains the trivial"
   "only contains the auxiliary lemma"
   "only contains the placeholder"
   "placeholder lemma"
   "tautology"
   "brand-new development"
   "outside a “closing pass”"
   "outside a closing pass"
   "only sets up `problemdata`"
   "only sets up problemdata"
   "the entire construction"])

(def ^:private dojo-deep-markers
  ["argument-principle"
   "finite blaschke-product"
   "jensen/weierstrass"
   "branch cuts"
   "conformal-map construction"
   "higher-order schwarz lemma"
   "full lebesgue-measure completion api"
   "jordan’s lemma"
   "jordan's lemma"])

(defn- slurp-if-exists [path]
  (when (and path (.exists (io/file path)))
    (slurp path)))

(defn- contains-any-substring?
  [s markers]
  (let [s (str/lower-case (or s ""))]
    (boolean (some #(str/includes? s %) markers))))

(defn- build-command-for-row [_row]
  "cd /home/joe/code/apm-lean && lake build ApmCanaries")

(defn- extract-sorry-obligations
  [path]
  (let [content (slurp-if-exists path)
        lines (vec (str/split-lines (or content "")))]
    (->> (range (count lines))
         (keep (fn [idx]
                 (let [line (get lines idx "")
                       trimmed (str/trim line)]
                   (when (and (not (str/starts-with? trimmed "--"))
                              (re-find #"(^|\s)sorry($|\s)" trimmed))
                     (let [theorem-idx
                           (loop [j idx]
                             (cond
                               (neg? j) nil
                               (re-find #"^\s*(lemma|theorem)\s+([A-Za-z0-9_'.]+)"
                                        (get lines j ""))
                               j
                               :else
                               (recur (dec j))))
                           theorem-line (when theorem-idx (get lines theorem-idx))
                           theorem-name (second
                                         (re-find #"^\s*(?:lemma|theorem)\s+([A-Za-z0-9_'.]+)"
                                                  (or theorem-line "")))
                           context-start (max 0 (- idx 3))
                           context-end (min (count lines) (+ idx 1))]
                       {:line (inc idx)
                        :theorem-line (when theorem-idx (inc theorem-idx))
                        :theorem-name (or theorem-name "unknown")
                        :context (->> (subvec lines context-start context-end)
                                      (str/join "\n")
                                      str/trim)})))))
         vec)))

(defn- dojo-state-for-row
  [row close-output target-main obligations]
  (cond
    (= "complete" (:cleanup_state row)) "complete"
    (or (str/blank? target-main)
        (not (.exists (io/file target-main))))
    "needs_codex_scaffold"
    (empty? obligations)
    "needs_codex_scaffold"
    (contains-any-substring? close-output dojo-skeletal-markers)
    "needs_codex_scaffold"
    (contains-any-substring? close-output dojo-deep-markers)
    "deep_mathlib_blocked"
    :else
    "dojo_ready"))

(defn- dojo-target-main-path-and-obligations
  [row]
  (let [candidates (candidate-main-paths row)
        analyzed (mapv (fn [path]
                         {:path path
                          :obligations (extract-sorry-obligations path)})
                       candidates)
        ranked (sort-by (fn [{:keys [path obligations]}]
                          [(- (count obligations))
                           (if (= path (not-blank (:latest_canary_main_path row))) 0 1)])
                        analyzed)
        best (or (first ranked)
                 {:path (preferred-main-path row)
                  :obligations []})]
    [(:path best) (:obligations best)]))

(defn- handoff-body
  [row close-output target-main obligations dojo-state]
  (let [problem-id (:id row)
        build-command (build-command-for-row row)
        notes (-> (or close-output "")
                  str/trim)
        obligation-lines
        (if (seq obligations)
          (str/join
           "\n"
           (map-indexed
            (fn [idx {:keys [line theorem-line theorem-name context]}]
              (str (inc idx) ". `" theorem-name "`"
                   " — `" target-main ":" (or theorem-line line) "`\n"
                   "   Line with `sorry`: " line "\n"
                   "   Local context:\n"
                   "```lean\n" context "\n```"))
            obligations))
          "None detected.")
        supporting
        (->> [(maybe-section "Target main" target-main)
              (maybe-section "Informal outline" (:informal_proof_path row))
              (maybe-section "Legacy statement" (:legacy_statement_path row))
              (maybe-section "Pipeline statement" (:pipeline_statement_path row))
              (maybe-section "TeX source" (:tex_path row))]
             (remove nil?)
             (apply str))]
    (str "# LeanDojo Handoff: " problem-id "\n\n"
         "problem_id: " problem-id "\n"
         "dojo_state: " dojo-state "\n"
         "generated_at: " (Instant/now) "\n\n"
         "## Status\n"
         "- `dojo_state`: `" dojo-state "`\n"
         "- `build_command`: `" build-command "`\n"
         "- `remaining_sorries`: `" (count obligations) "`\n\n"
         "## Scope\n"
         "- Work in the existing target file first.\n"
         "- Do not weaken theorem statements silently.\n"
         "- If a statement is structurally false, repair it minimally and record the change.\n"
         "- If the file is too skeletal, stop and send it back for Codex scaffolding.\n\n"
         "## Remaining Holes\n"
         obligation-lines "\n\n"
         "## Current Blocker Summary\n"
         (if (str/blank? notes)
           "No close-pass summary was available.\n"
           (str notes "\n"))
         "\n## Supporting Files\n"
         supporting
         "\n## Suggested Next Step\n"
         (case dojo-state
           "dojo_ready"
           "LeanDojo can work directly on the named local holes above.\n"
           "needs_codex_scaffold"
           "Do not hand this to LeanDojo yet; expand the Lean scaffold first.\n"
           "deep_mathlib_blocked"
           "Do not hand this to LeanDojo first; it appears to need new library/theory development.\n"
           "complete"
           "No handoff needed; the file is already complete.\n"
           "Review manually.\n"))))

(defn- generate-dojo-handoff-for-row!
  [row]
  (let [problem-id (:id row)
        [target-main obligations] (dojo-target-main-path-and-obligations row)
        close-output (or (slurp-if-exists (cleanup-output-path-for-mode :scaffold problem-id))
                         (slurp-if-exists (cleanup-output-path-for-mode :close problem-id))
                         (slurp-if-exists (cleanup-output-path-for-mode :triage problem-id))
                         "")
        dojo-state (dojo-state-for-row row close-output target-main obligations)
        handoff-path (io/file dojo-handoff-output-dir (str "apm-" problem-id ".md"))]
    (.mkdirs (io/file dojo-handoff-output-dir))
    (if (= dojo-state "complete")
      (do
        (when (.exists handoff-path)
          (.delete handoff-path))
        {:problem-id problem-id :dojo-state dojo-state :handoff-path nil})
      (let [body (handoff-body row close-output target-main obligations dojo-state)]
        (spit handoff-path body)
        {:problem-id problem-id
         :dojo-state dojo-state
         :handoff-path (.getAbsolutePath handoff-path)
         :remaining (count obligations)}))))

(defn rebuild-lean-dojo-handoffs!
  "Rebuild LeanDojo handoff files from existing close/triage replies and current target files.
   Returns a summary map and refreshes the manifest."
  [& {:keys [problem-ids]
      :or {problem-ids nil}}]
  (let [problem-id-set (when (seq problem-ids) (set problem-ids))
        rows (->> (load-problem-manifest-tsv)
                  (filter started-proof-row?)
                  (filter #(or (nil? problem-id-set)
                               (contains? problem-id-set (:id %)))))
        results (mapv generate-dojo-handoff-for-row! rows)
        summary (reduce (fn [acc {:keys [dojo-state]}]
                          (update acc dojo-state (fnil inc 0)))
                        {}
                        results)]
    (refresh-problem-manifest!)
    {:count (count results)
     :states summary
     :results results}))

(defn- make-proof-triage-prompt [row tex-body]
  (let [problem-id (:id row)
        target-main (preferred-main-path row)]
    (str "Problem: apm-" problem-id "\n\n"
         "This is a Lean proof cleanup pass for an already-started formalization.\n"
         "Work only on the existing proof artifact unless it is unusable.\n\n"
         "Primary target:\n"
         "- Main file: " target-main "\n"
         (or (maybe-section "Informal outline" (:informal_proof_path row)) "")
         (or (maybe-section "Legacy statement" (:legacy_statement_path row)) "")
         (or (maybe-section "Pipeline statement" (:pipeline_statement_path row)) "")
         "- Cleanup log: " cleanup-log-path "\n"
         "- Manifest builder: " manifest-builder-path "\n\n"
         "Required outcome:\n"
         "1. Try to close the existing Lean proof in the target file.\n"
         "2. If a helper statement is structurally false or missing hypotheses, revise it structurally and prove the revised version.\n"
         "3. If a helper is inessential, remove or bypass it only by proving the main result without it. Do not leave dead false lemmas around.\n"
         "4. If the remaining blocker is a genuine deep dive, stop there and leave a precise note instead of forcing a fake proof.\n"
         "5. Run the appropriate `lake build` for the target module and confirm the result.\n"
         "6. Append one EDN line to the cleanup log in this exact shape:\n"
         "   {:problem-id \"" problem-id "\" :closed <n> :remaining <m> :notes \"...\"}\n"
         "7. Run the manifest builder after updating the cleanup log.\n\n"
         "Constraints:\n"
         "- No workarounds.\n"
         "- Do not weaken invariants silently.\n"
         "- If you change a theorem statement, say exactly why the original statement was invalid.\n"
         "- Prefer editing the current target file over creating a new frame.\n\n"
         (when tex-body
           (str "Problem statement:\n\n```latex\n" tex-body "\n```\n\n"))
         "Return a short final summary with:\n"
         "- target file edited\n"
         "- build command run\n"
         "- how many lemmas/theorems were closed\n"
         "- how many remain\n"
         "- whether any remaining blocker is essential or inessential\n")))

(defn- make-proof-close-prompt [row tex-body]
  (let [problem-id (:id row)
        target-main (preferred-main-path row)]
    (str "Problem: apm-" problem-id "\n\n"
         "This is a Lean proof closing pass for an already-started formalization.\n"
         "The goal is to finish the current proof file if that is structurally possible.\n\n"
         "Primary target:\n"
         "- Main file: " target-main "\n"
         (or (maybe-section "Informal outline" (:informal_proof_path row)) "")
         (or (maybe-section "Legacy statement" (:legacy_statement_path row)) "")
         (or (maybe-section "Pipeline statement" (:pipeline_statement_path row)) "")
         "- Cleanup log: " cleanup-log-path "\n"
         "- Manifest builder: " manifest-builder-path "\n\n"
         "Closing protocol:\n"
         "1. Start by building the target module or the smallest enclosing `lake build` target you can infer.\n"
         "2. Focus on the remaining `sorry`s in the existing file.\n"
         "3. If a helper statement is false as written, make the smallest structurally necessary repair and then prove the repaired statement.\n"
         "4. If a helper is unnecessary, remove it from the critical path by proving the main result without it.\n"
         "5. Stay local to the file and its direct dependencies. Do not spend time on broad repo-wide absence proofs.\n"
         "6. If after a couple of real build-fix attempts the remaining blocker is a deep missing theorem, stop immediately and record it as blocked.\n"
         "7. Append one EDN line to the cleanup log in this exact shape:\n"
         "   {:problem-id \"" problem-id "\" :closed <n> :remaining <m> :notes \"...\"}\n"
         "8. Run the manifest builder after updating the cleanup log.\n\n"
         "9. If blocked, classify the file in your summary as one of: `ready_local_holes`, `skeletal_file`, or `deep_mathlib_gap`.\n\n"
         "Constraints:\n"
         "- No workarounds.\n"
         "- No fake progress.\n"
         "- Do not rewrite the task into a research program.\n"
         "- If closure fails, name the exact theorem or API gap that stopped you.\n\n"
         (when tex-body
           (str "Problem statement:\n\n```latex\n" tex-body "\n```\n\n"))
         "Return a short final summary with:\n"
         "- target file edited\n"
         "- build command run\n"
         "- whether the file now closes fully\n"
         "- how many lemmas/theorems were closed\n"
         "- how many remain\n"
         "- if blocked, the exact essential blocker\n"
         "- if blocked, whether the file is `ready_local_holes`, `skeletal_file`, or `deep_mathlib_gap`\n")))

(defn- make-proof-scaffold-prompt [row tex-body]
  (let [problem-id (:id row)
        target-main (preferred-main-path row)]
    (str "Problem: apm-" problem-id "\n\n"
         "This is a Lean proof scaffolding pass for an already-started formalization.\n"
         "The goal is not to finish the proof. The goal is to make the file a viable LeanDojo handoff.\n\n"
         "Primary target:\n"
         "- Main file: " target-main "\n"
         (or (maybe-section "Informal outline" (:informal_proof_path row)) "")
         (or (maybe-section "Legacy statement" (:legacy_statement_path row)) "")
         (or (maybe-section "Pipeline statement" (:pipeline_statement_path row)) "")
         "- Cleanup log: " cleanup-log-path "\n"
         "- Manifest builder: " manifest-builder-path "\n\n"
         "Scaffold protocol:\n"
         "1. Start by building the smallest relevant `lake build` target you can infer.\n"
         "2. If the file is skeletal, add the missing main theorem statement(s) and any obvious helper lemma statements needed to mirror the informal/pipeline target.\n"
         "3. Reduce the file to explicit, local `sorry`s in the real proof path. Prefer a few meaningful holes over one vague placeholder theorem.\n"
         "4. Keep the work inside the existing file unless the current file is unusable.\n"
         "5. Do not attempt a full proof search. Stop once the file has clear local obligations and still builds.\n"
         "6. If the task still depends on major missing theory or the file cannot be localized, record that precisely instead of forcing fake structure.\n"
         "7. Append one EDN line to the cleanup log in this exact shape:\n"
         "   {:problem-id \"" problem-id "\" :closed <n> :remaining <m> :notes \"...\"}\n"
         "8. Run the manifest builder after updating the cleanup log.\n\n"
         "Constraints:\n"
         "- No workarounds.\n"
         "- Do not silently weaken theorem statements.\n"
         "- If you revise a statement structurally, say why.\n"
         "- Optimize for `dojo_ready`, not for closure.\n\n"
         (when tex-body
           (str "Problem statement:\n\n```latex\n" tex-body "\n```\n\n"))
         "Return a short final summary with:\n"
         "- target file edited\n"
         "- build command run\n"
         "- whether the file is now `dojo_ready`, still `skeletal_file`, or `deep_mathlib_gap`\n"
         "- how many explicit local holes remain\n"
         "- the exact next handoff recommendation\n")))

(defn- prompt-builder-for-mode [mode]
  (case mode
    :close make-proof-close-prompt
    :scaffold make-proof-scaffold-prompt
    make-proof-triage-prompt))

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
        pid (str "apm-" base)]
    (cond
      (nil? problem)
      (println (str "[apm-v3:" agent-id "] unknown problem " pid))

      (not (apm-queue/machine-processable? problem))
      (println (str "[apm-v3:" agent-id "] refusing to dispatch " pid
                    " because machine_processing_state="
                    (:machine-processing-state problem)))

      :else
      (let [tex-body (apm-queue/load-problem-tex base)
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
        (println (str "[apm-v3:" agent-id "] started on " pid))))))

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

;; =============================================================================
;; Batch mode — informal proof + Lean statements, no proofs
;; =============================================================================

(def ^:private batch-output-dir
  "/home/joe/code/futon3c/data/apm-informal-proofs")

(defn- make-informal-lean-prompt [problem tex-body]
  (let [{:keys [id subject year session subpart-count subparts]} problem]
    (str "Problem: apm-" id " (" (get subject-names subject (name subject))
         ", " year ", " (if (= session :fall) "Fall" "Spring") ")\n\n"
         "```latex\n" tex-body "\n```\n\n"
         (when (and subparts (pos? subpart-count))
           (str "Sub-parts: " (str/join ", " (map :label subparts)) "\n\n"))
         "This is a STATEMENT-ONLY pass. Do not attempt any proofs.\n"
         "Do not run lake build. Do not use tools. Just write the reply inline.\n\n"
         "Three deliverables, all inline in your reply:\n\n"
         "## 1. Informal proof\n"
         "- Why it's hard (1-2 sentences)\n"
         "- The key insight (1-2 sentences)\n"
         "- Complete readable proof (not a sketch)\n"
         "- What connects (1 paragraph)\n\n"
         "## 2. Lean 4 theorem statement\n"
         "State the main theorem as a Lean 4 declaration with full type\n"
         "signature, using real Mathlib types. Body is `sorry`. Include\n"
         "any helper lemmas needed as separate `sorry`-bodied statements.\n\n"
         "```lean\n"
         "-- example shape:\n"
         "theorem apm_" id "_main\n"
         "    (f : ℝ → ℝ) (hf : Integrable f volume) :\n"
         "    ... := by sorry\n"
         "```\n\n"
         "## 3. Mathlib cross-references\n"
         "List the Mathlib names you used (or would use) in the statement\n"
         "and that would appear in the proof. Group by purpose:\n"
         "- **Types/structures**: `Measure`, `Integrable`, `Lp`, `NormedSpace`, ...\n"
         "- **Key lemmas**: specific theorem names from Mathlib that would\n"
         "  close or advance the proof (e.g., `lintegral_norm_eq_lintegral_meas_lt`,\n"
         "  `Real.rpow_add`, `MeasureTheory.integral_mul_le_Lp_mul_Lq`).\n"
         "- **Tactic hints**: which tactics you'd reach for (`field_simp`,\n"
         "  `nlinarith`, `exact?`, `positivity`, `gcongr`).\n\n"
         "These cross-references are the most valuable part — they index\n"
         "the problem into the Mathlib API surface for later retrieval.\n"
         "Be specific. Real names, not hand-waving.\n\n"
         "Remember: NO PROOFS. Statements + cross-refs only. Inline reply.\n")))

(defonce !batch (atom nil))

(defn- save-batch-output! [problem-id output]
  (let [dir (io/file batch-output-dir)]
    (.mkdirs dir)
    (spit (io/file dir (str problem-id ".md")) output)))

(defn- batch-on-idle [agent-id outcome]
  (when-let [batch @!batch]
    (when (and (= agent-id (:agent-id batch)) (:running? batch))
      (let [output (or (:result outcome) "")
            pid (:current-problem-id batch)
            elapsed (- (System/currentTimeMillis) (or (:dispatch-ms batch) 0))]
        ;; Save output
        (save-batch-output! pid output)
        (log! {:event :batch-return :problem pid :elapsed-ms elapsed
               :output-length (count output)})
        (println (str "[apm-v3:batch] " pid " done (" (long (/ elapsed 1000)) "s, "
                      (count output) " chars)"))

        ;; Advance to next problem
        (let [remaining (rest (:queue batch))
              done (inc (:done-count batch))]
          (if (empty? remaining)
            (do
              (log! {:event :batch-complete :done done})
              (println (str "[apm-v3:batch] COMPLETE. " done " problems processed."))
              (apm-dispatch/deregister-conductor! :v3-batch)
              (reset! !batch nil))
            (let [next-base (first remaining)
                  manifest (apm-queue/load-apm-manifest)
                  problem (first (filter #(= next-base (:id %)) manifest))
                  tex-body (apm-queue/load-problem-tex next-base)
                  next-pid (str "apm-" next-base)]
              (swap! !batch assoc
                     :queue remaining
                     :current-problem-id next-pid
                     :done-count done
                     :dispatch-ms (System/currentTimeMillis))
              (log! {:event :batch-dispatch :problem next-pid :remaining (count remaining)})
              (println (str "[apm-v3:batch] dispatching " next-pid
                            " (" (count remaining) " remaining)"))
              (future
                (try
                  (reg/invoke-agent! agent-id
                    (make-informal-lean-prompt problem tex-body)
                    (* 5 60 1000)) ;; 5 min timeout per problem
                  (catch Exception e
                    (println (str "[apm-v3:batch] error: " (.getMessage e)))))))))))))

(defn batch!
  "Run informal proof + Lean statements batch over a list of problem IDs.
   Output saved to data/apm-informal-proofs/<problem-id>.md

   Usage:
     (batch! :agent-id \"claude-1\")                     ;; all remaining
     (batch! :agent-id \"claude-1\" :n 5)                ;; next 5 unprocessed
     (batch! :agent-id \"claude-1\" :problem-ids [...])   ;; specific list
     (batch! :agent-id \"claude-1\" :subject :topology)   ;; by subject"
  [& {:keys [agent-id problem-ids subject n]
      :or {agent-id "claude-1"}}]
  (when @!batch
    (apm-dispatch/deregister-conductor! :v3-batch))
  (let [manifest (apm-queue/load-apm-manifest)
        problems (cond
                   problem-ids (filter #((set problem-ids) (:id %)) manifest)
                   subject (filter #(= subject (:subject %)) manifest)
                   :else manifest)
        unavailable (remove apm-queue/machine-processable? problems)
        ;; Skip already-processed
        done-dir (io/file batch-output-dir)
        already-done (if (.exists done-dir)
                       (->> (.listFiles done-dir)
                            (map #(.getName %))
                            (filter #(str/ends-with? % ".md"))
                            (map #(str/replace % #"^apm-|\.md$" ""))
                            set)
                       #{})
        queue (cond->> (->> problems
                            (filter apm-queue/machine-processable?)
                            (remove #(already-done (:id %)))
                            (map :id)
                            vec)
                n (take n)
                true vec)]
    (if (empty? queue)
      (println (str "[apm-v3:batch] Nothing to do — no machine-processable problems remain"
                    (when (seq unavailable)
                      (str " (" (count unavailable) " unavailable)"))
                    "."))
      (let [first-base (first queue)
            problem (first (filter #(= first-base (:id %)) manifest))
            tex-body (apm-queue/load-problem-tex first-base)
            first-pid (str "apm-" first-base)]
        (reset! !batch {:agent-id agent-id
                        :queue queue
                        :current-problem-id first-pid
                        :done-count 0
                        :total (count queue)
                        :running? true
                        :dispatch-ms (System/currentTimeMillis)
                        :started-at (System/currentTimeMillis)})
        (apm-dispatch/register-conductor! :v3-batch agent-id batch-on-idle)
        (spit log-path (str ";; APM v3 Batch — " (Instant/now) "\n") :append true)
        (log! {:event :batch-started :agent agent-id :total (count queue)
               :skipped (count already-done)
               :unavailable (count unavailable)})
        (println (str "[apm-v3:batch] Starting " (count queue) " problems"
                      " (" (count already-done) " already done, skipped; "
                      (count unavailable) " unavailable)"))
        (future
          (try
            (reg/invoke-agent! agent-id
              (make-informal-lean-prompt problem tex-body)
              (* 5 60 1000))
            (catch Exception e
              (println (str "[apm-v3:batch] error: " (.getMessage e))))))))))

(defn batch-status []
  (if-let [b @!batch]
    {:running? (:running? b)
     :current (:current-problem-id b)
     :done (:done-count b)
     :remaining (count (:queue b))
     :total (:total b)
     :elapsed-s (long (/ (- (System/currentTimeMillis)
                            (or (:started-at b) 0)) 1000))}
    :not-running))

(defn batch-stop! []
  (when @!batch
    (apm-dispatch/deregister-conductor! :v3-batch)
    (let [{:keys [done-count total]} @!batch]
      (println (str "[apm-v3:batch] Stopped. " done-count "/" total " done."))
      (reset! !batch nil))))

;; =============================================================================
;; Cleanup batch — existing Lean proofs only
;; =============================================================================

(defonce !cleanup (atom nil))

(defn- cleanup-agent-busy?
  [agent-id]
  (or (contains? @!steppers agent-id)
      (and @!batch (= agent-id (:agent-id @!batch)))
      (and @!cleanup (= agent-id (:agent-id @!cleanup)))))

(defn- cleanup-on-idle [agent-id outcome]
  (when-let [cleanup @!cleanup]
    (when (and (= agent-id (:agent-id cleanup)) (:running? cleanup))
      (let [mode (:mode cleanup)
            mode-label (cleanup-mode-label mode)
            output (or (:result outcome) "")
            row (:current-row cleanup)
            pid (:id row)
            elapsed (- (System/currentTimeMillis) (or (:dispatch-ms cleanup) 0))]
        (save-cleanup-output! mode pid output)
        (log! {:event :cleanup-return
               :mode mode
               :agent agent-id
               :problem-id pid
               :elapsed-ms elapsed
               :output-length (count output)})
        (println (str "[apm-v3:" mode-label "] " pid " returned ("
                      (long (/ elapsed 1000)) "s, " (count output) " chars)"))
        (refresh-problem-manifest!)
        (when (#{:close :scaffold} mode)
          (when-let [updated-row (some #(when (= pid (:id %)) %) (load-problem-manifest-tsv))]
            (generate-dojo-handoff-for-row! updated-row)
            (refresh-problem-manifest!)))
        (let [remaining (rest (:queue cleanup))
              done (inc (:done-count cleanup))]
          (if (empty? remaining)
            (do
              (log! {:event :cleanup-complete
                     :mode mode
                     :agent agent-id
                     :done done})
              (println (str "[apm-v3:" mode-label "] COMPLETE. " done " problem(s) dispatched."))
              (apm-dispatch/deregister-conductor! :v3-cleanup)
              (reset! !cleanup nil))
            (let [next-row (first remaining)
                  next-pid (:id next-row)
                  tex-body (read-tex-body next-row)
                  prompt ((prompt-builder-for-mode mode) next-row tex-body)]
              (swap! !cleanup assoc
                     :queue remaining
                     :current-row next-row
                     :done-count done
                     :dispatch-ms (System/currentTimeMillis))
              (log! {:event :cleanup-dispatch
                     :mode mode
                     :agent agent-id
                     :problem-id next-pid
                     :remaining (count remaining)})
              (println (str "[apm-v3:" mode-label "] dispatching " next-pid
                            " (" (count remaining) " remaining after this)"))
              (future
                (try
                  (reg/invoke-agent! agent-id prompt (:timeout-ms cleanup))
                  (catch Exception e
                    (println (str "[apm-v3:" mode-label "] error: " (.getMessage e)))))))))))))

(defn- start-cleanup-mode!
  [mode & {:keys [agent-id n problem-ids include-blocked? timeout-ms]
           :or {agent-id "codex-1" n 1 include-blocked? false
                timeout-ms (* 45 60 1000)}}]
  (let [mode-label (cleanup-mode-label mode)]
    (when @!cleanup
      (apm-dispatch/deregister-conductor! :v3-cleanup))
    (reset! !cleanup nil)
    (cond
      (cleanup-agent-busy? agent-id)
      (println (str "[apm-v3:" mode-label "] Agent " agent-id
                    " is already in use by another v3 flow."))

      :else
      (let [queue (cleanup-candidates :n n
                                      :problem-ids problem-ids
                                      :include-blocked? include-blocked?
                                      :mode mode)]
        (if (empty? queue)
          (println (str "[apm-v3:" mode-label "] Nothing to do."))
          (let [first-row (first queue)
                tex-body (read-tex-body first-row)
                prompt ((prompt-builder-for-mode mode) first-row tex-body)]
            (reset! !cleanup {:agent-id agent-id
                              :mode mode
                              :queue queue
                              :current-row first-row
                              :done-count 0
                              :total (count queue)
                              :running? true
                              :timeout-ms timeout-ms
                              :dispatch-ms (System/currentTimeMillis)
                              :started-at (System/currentTimeMillis)})
            (apm-dispatch/register-conductor! :v3-cleanup agent-id cleanup-on-idle)
            (log! {:event :cleanup-started
                   :mode mode
                   :agent agent-id
                   :total (count queue)
                   :problem-ids (mapv :id queue)})
            (println (str "[apm-v3:" mode-label "] Starting " (count queue)
                          " problem(s): "
                          (str/join ", " (map :id queue))))
            (future
              (try
                (reg/invoke-agent! agent-id prompt timeout-ms)
                (catch Exception e
                  (println (str "[apm-v3:" mode-label "] error: " (.getMessage e))))))))))))

(defn cleanup-batch!
  "Run codex triage over the next N already-started Lean proofs.

   Defaults:
   - agent-id: codex-1
   - n: 1

   Eligible rows are those with an existing Main.lean proof file and no
   cleanup_state=complete. Blocked rows are skipped by default unless
   :include-blocked? true or explicit :problem-ids are provided.

   Usage:
     (cleanup-batch!)                       ;; next one
     (cleanup-batch! :n 5)                 ;; five more
     (cleanup-batch! :problem-ids [\"a94A07\"])
     (cleanup-batch! :n 3 :include-blocked? true)"
  [& {:keys [agent-id n problem-ids include-blocked? timeout-ms]
      :or {agent-id "codex-1" n 1 include-blocked? false
           timeout-ms (* 45 60 1000)}}]
  (start-cleanup-mode! :triage
                       :agent-id agent-id
                       :n n
                       :problem-ids problem-ids
                       :include-blocked? include-blocked?
                       :timeout-ms timeout-ms))

(defn close-batch!
  "Run codex close mode over the next N already-started Lean proofs.

   This variant is stricter than triage:
   - skips blocked rows by default, even if explicitly named
   - uses a narrow 'finish the file or stop quickly' prompt

   Usage:
     (close-batch!)                       ;; next closeable one
     (close-batch! :n 5)                 ;; five more
     (close-batch! :problem-ids [\"a94J01\"])
     (close-batch! :problem-ids [\"a94A07\"] :include-blocked? true)"
  [& {:keys [agent-id n problem-ids include-blocked? timeout-ms]
      :or {agent-id "codex-1" n 1 include-blocked? false
           timeout-ms (* 30 60 1000)}}]
  (start-cleanup-mode! :close
                       :agent-id agent-id
                       :n n
                       :problem-ids problem-ids
                       :include-blocked? include-blocked?
                       :timeout-ms timeout-ms))

(defn scaffold-batch!
  "Run codex scaffold mode over started Lean proofs that need Codex scaffolding.

   Defaults:
   - agent-id: codex-1
   - n: 1

   By default, this selects rows with dojo_state=needs_codex_scaffold.
   You can also target specific problems explicitly.

   Usage:
     (scaffold-batch!)                       ;; next scaffold-needed one
     (scaffold-batch! :n 5)                 ;; five more
     (scaffold-batch! :problem-ids [\"a03J04\"])"
  [& {:keys [agent-id n problem-ids timeout-ms]
      :or {agent-id "codex-1" n 1
           timeout-ms (* 30 60 1000)}}]
  (start-cleanup-mode! :scaffold
                       :agent-id agent-id
                       :n n
                       :problem-ids problem-ids
                       :timeout-ms timeout-ms))

(defn cleanup-status []
  (if-let [cleanup @!cleanup]
    {:running? (:running? cleanup)
     :mode (:mode cleanup)
     :agent-id (:agent-id cleanup)
     :current (get-in cleanup [:current-row :id])
     :done (:done-count cleanup)
     :remaining (count (:queue cleanup))
     :total (:total cleanup)
     :queue (mapv :id (:queue cleanup))
     :elapsed-s (long (/ (- (System/currentTimeMillis)
                            (or (:started-at cleanup) 0)) 1000))}
    :not-running))

(defn cleanup-stop! []
  (when @!cleanup
    (let [mode-label (cleanup-mode-label (:mode @!cleanup))]
      (apm-dispatch/deregister-conductor! :v3-cleanup)
      (let [{:keys [done-count total]} @!cleanup]
        (println (str "[apm-v3:" mode-label "] Stopped. " done-count "/" total " dispatched."))
        (reset! !cleanup nil)))))
