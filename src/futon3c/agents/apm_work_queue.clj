(ns futon3c.agents.apm-work-queue
  "APM (UT Austin Preliminary Exam) work queue for Tickle — feeds 489
   math problems through the orchestrator for proof-peripheral inhabitation.

   The queue reads problem metadata from storage/apm/manifest.edn and
   LaTeX bodies from storage/apm/{id}.tex. Each problem is dispatched
   to an agent (typically claude-1) which inhabits the proof peripheral
   through all 9 phases: observe → propose → execute → validate →
   classify → integrate → commit → gate-review → completed.

   Progress is tracked via evidence store: problems with existing APM
   evidence are skipped on subsequent runs. This makes batch runs resumable.

   Design: this module provides work items and prompts. The actual agent
   invocation flows through tickle_orchestrate.clj's existing machinery."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]))

;; =============================================================================
;; Paths
;; =============================================================================

(def ^:private apm-dir
  "/home/joe/code/storage/apm")

(def ^:private manifest-path
  (str apm-dir "/manifest.edn"))

;; =============================================================================
;; Manifest loading
;; =============================================================================

(defn load-apm-manifest
  "Load the APM problem manifest (489 entries).
   Returns a vector of maps with :id, :subject, :year, :filename, etc."
  ([] (load-apm-manifest manifest-path))
  ([path]
   (edn/read-string (slurp path))))

(defn load-problem-tex
  "Read the LaTeX body for a problem ID (e.g. \"t01A01\").
   Returns the raw LaTeX string, or nil if file not found."
  [problem-id]
  (let [f (io/file apm-dir (str problem-id ".tex"))]
    (when (.exists f)
      (slurp f))))

(def ^:private subject-names
  {:analysis "Analysis"
   :algebra "Algebra"
   :topology "Topology"
   :applied "Applied/Functional Analysis"})

;; =============================================================================
;; Work item synthesis
;; =============================================================================

(defn- make-inhabitation-prompt
  "Build the proof-peripheral inhabitation prompt for an APM problem.
   The agent receives this as a single dispatch and drives through all
   9 proof phases, maintaining context throughout.

   The prompt encodes the quality standard from M-apm-solutions: each phase
   has labeled sections matching the cheatsheet pipeline's expectations.
   The canary run (t01A01 C002) is the quality baseline."
  [problem tex-body]
  (let [{:keys [id subject year session subpart-count subparts]} problem
        problem-id (str "apm-" id)]
    (str "Runtime surface contract:\n"
         "- Agent: claude-1 (APM proof peripheral — inhabitation mode)\n"
         "- Problem: " problem-id " (" (get subject-names subject (name subject))
         ", " year ", " (if (= session :fall) "Fall" "Spring") ")\n"
         "- Time budget: 15 minutes. Use it. Quality over speed.\n"
         "- Your phase notes feed the cheatsheet pipeline and ArSE questions.\n"
         "- Proof state persists at data/proof-state/" problem-id ".edn\n\n"

         "## Problem Statement\n\n"
         "```latex\n" tex-body "\n```\n\n"

         (when (and subparts (pos? subpart-count))
           (str "This problem has " subpart-count " sub-parts: "
                (str/join ", " (map :label subparts)) "\n\n"))

         "## Phase Instructions\n\n"
         "You are inhabiting the proof peripheral. Each phase below has a specific\n"
         "pedagogical function — the notes you write become the cheatsheet content.\n"
         "Write for a strong undergrad who is naive about graduate prerequisites.\n\n"

         "### OBSERVE\n"
         "Two labeled sections required:\n"
         "- **WHAT IS REALLY BEING ASKED**: Restate the problem in your own words.\n"
         "  What mathematical objects are involved? What is the real question\n"
         "  underneath the formal statement?\n"
         "- **WHY IT IS HARD**: Where do students get stuck? What trap does the\n"
         "  problem set? What prerequisite is being tested?\n\n"

         "### PROPOSE\n"
         "Two labeled sections required:\n"
         "- **THE KEY INSIGHT**: The single idea that unlocks the problem. What do\n"
         "  you need to see? State the proof strategy and why it works.\n"
         "- **THE NAIVE APPROACH THAT FAILS**: What does a student try first that\n"
         "  leads to a dead end? Explain WHY it fails — what structural obstruction\n"
         "  blocks it. This becomes a failed-route record.\n\n"

         "### EXECUTE\n"
         "Three stages in order:\n\n"
         "**Stage 1 — THE CLEAN PROOF**: A complete, readable informal proof.\n"
         "Not a sketch — a proof a reader could follow line by line. For\n"
         "multi-part problems, prove each part explicitly.\n\n"
         "**Stage 2 — LEMMA DEPENDENCY GRAPH**: Before touching Lean, extract\n"
         "the proof's logical structure as an explicit build plan:\n"
         "- List each lemma/fact the proof uses, in dependency order.\n"
         "- For each entry, distinguish the FORMAL dependency from the INFORMAL\n"
         "  strategy trigger. We are not only recording 'what theorem is used',\n"
         "  but also 'how did you know to reach for it here?'\n"
         "- Use this exact template for every dependency entry:\n"
         "  1. **Dependency Name**\n"
         "     - **Formal dependency**: the theorem/lemma/definition actually used\n"
         "     - **Informal dependency**: the recognition heuristic, pattern, or\n"
         "       hidden proof strategy that made this move visible\n"
         "     - **Why this becomes thinkable here**: the local cue in this problem\n"
         "       that tells you the formal dependency is relevant\n"
         "     - **Lean target/type**: the type signature or goal shape you expect\n"
         "     - **Mathlib status/search terms**: likely in Mathlib vs custom,\n"
         "       plus concrete search terms\n"
         "     - **Critical path**: yes/no + why\n"
         "- Identify the critical path: which dependency, if closed, unblocks the\n"
         "  most downstream work?\n"
         "This is HtDP step 1 for the entire proof. The informal proof IS your\n"
         "design document — this stage makes the connection explicit.\n\n"
         "**Stage 3 — LEAN FORMALIZATION** under exam conditions.\n"
         "- At the start of the Lean phase, set a timer for 15 minutes.\n"
         "- Use as much of the time as you need, but at the end of 15 minutes\n"
         "  you must stop whatever you are doing and move on to the next problem.\n"
         "  (You may finish a Write operation already in progress.)\n"
         "- Use the HtDP (How to Design Programs) recipe to attack each sorry:\n"
         "  1. Write the type signature — what exactly are you trying to prove?\n"
         "  2. Search Mathlib — what lemmas exist for this type? (`exact?`,\n"
         "     `apply?`, grep for key terms)\n"
         "  3. Sketch the composition — which lemmas chain together to close\n"
         "     this goal? What are the intermediate types?\n"
         "  4. Wire it — fill in the arguments, match the types, run `lake build`,\n"
         "     read the error, fix, repeat.\n"
         "  This is how you SOLVE sorry, not how you document giving up.\n"
         "- Most qualifying-exam problems CAN be fully closed in Lean with Mathlib\n"
         "  in 15 minutes. The work is wiring, not invention.\n"
         "- A 'real partial' means you did steps 1-3 for every sorry, got stuck\n"
         "  on a genuine API gap at step 4, and can name exactly what blocked you.\n"
         "  A 'dunno partial' means you wrote sorry without trying. Do not submit\n"
         "  dunno partials.\n\n"
         "Finally: **FORMAL-TO-INFORMAL REVISION**. After the Lean work, reread\n"
         "THE CLEAN PROOF and add light annotations reflecting what you learned\n"
         "from the formalization. The annotations should read as natural-language\n"
         "difficulty signals — no Lean or Mathlib references in the proof itself.\n"
         "Examples:\n"
         "- 'This step is the crux — it looks like a single application of X\n"
         "   but actually requires assembling three separate facts.'\n"
         "- 'This is routine: the bound follows directly from the definition.'\n"
         "- 'This is where most of the work is. The composition is conceptually\n"
         "   simple but the details are fiddly — expect to spend time here.'\n"
         "The Lean-specific details (which Mathlib APIs, how many lines of\n"
         "tactic wiring, what gap blocked closure) belong in the VALIDATE and\n"
         "CLASSIFY notes, which feed Column 3 of the cheatsheet. Column 1\n"
         "(THE CLEAN PROOF) should be a clean narrative that a reader can\n"
         "follow without knowing Lean exists.\n\n"

         "### VALIDATE\n"
         "Non-circularity analysis:\n"
         "- List the independent facts the proof composes.\n"
         "- Confirm none secretly assumes the conclusion.\n"
         "- For Lean: report build status, sorry count, what blocked closure.\n\n"

         "### CLASSIFY\n"
         "Two labeled sections required:\n"
         "- **Classification**: `proved` (zero sorry, builds — the expected outcome)\n"
         "  / `partial` (sorry remains after 15 min of genuine effort — explain what\n"
         "  resisted and what you tried) / `build-failed` / `timed-out`.\n"
         "- **CONFIDENCE INVERSION**: What is surprising about this problem? Where\n"
         "  is intuition wrong? Name the surprise — that is what makes it stick.\n\n"

         "### INTEGRATE\n"
         "Two labeled sections required:\n"
         "- **Connections**: What other mathematics does this connect to? Where else\n"
         "  does this technique appear?\n"
         "- **EXAM-DAY FIELD KIT**: The 3-5 facts a student needs to have ready.\n"
         "  Not deep theory — the specific lemmas/tricks that compose in this problem.\n\n"

         "### ArSE QUESTIONS\n"
         "After completing the phases above, generate these 5 pedagogical questions\n"
         "and then REVIEW your phase notes to ensure they actually answer them:\n\n"
         "1. **why-hard** (from observe): Why is this problem hard? What trap does\n"
         "   it set? — Does your OBSERVE section answer this clearly?\n"
         "2. **what-crux** (from propose): What is the key insight you need to see?\n"
         "   — Does your PROPOSE section make the crux obvious?\n"
         "3. **why-works** (from validate): Why does step N work? Pick the least\n"
         "   obvious step in your proof and explain why it is valid. — Does your\n"
         "   EXECUTE section explain this, or does it just assert it?\n"
         "4. **what-connects** (from integrate): What other mathematics does this\n"
         "   connect to? — Does your INTEGRATE section answer this?\n"
         "5. **confidence** (from classify): Where is intuition wrong here? What\n"
         "   looks easy but isn't, or looks hard but is? — Does your CLASSIFY\n"
         "   section name the surprise?\n\n"
         "If any question exposes a gap in your notes — a step you assert without\n"
         "explaining, a connection you missed, a trap you didn't name — go back\n"
         "and revise the relevant phase notes BEFORE finalizing. The questions are\n"
         "a quality check on your own work, not just output artifacts.\n\n"
         "Include the 5 questions and brief answers in an ### ArSE section at the\n"
         "end. These feed the ArSE question-generation pipeline.\n\n"

         "## Quality Standard\n\n"
         "Your notes are the PRIMARY content of the two-column cheatsheet.\n"
         "Left column = exposition (from observe/propose/execute/validate notes).\n"
         "Right column = discipline annotations (from classify/integrate notes).\n"
         "The ArSE questions above feed the right column's cross-references.\n"
         "Thin notes = thin questions = thin cheatsheet.\n\n"
         "DO NOT rush. A single well-done problem is worth more than ten\n"
         "rubber-stamped ones. Use the full time budget.\n\n"
         "Format each section with the exact headers above (### OBSERVE, etc).\n"
         "For Lean files, use fenced code blocks labeled `lean4-statement`,\n"
         "`lean4-lemmas`, `lean4-main`.\n")))

(defn entity->issue
  "Convert a manifest entry into an orchestrator-compatible issue map."
  [problem idx]
  (let [tex-body (load-problem-tex (:id problem))]
    {:number (+ 30000 idx)  ;; offset: 10000 = CT, 20000 = ArSE, 30000 = APM
     :title (str "APM proof: " (:id problem)
                 " (" (name (:subject problem)) " " (:year problem) ")")
     :body (make-inhabitation-prompt problem tex-body)
     :labels ["apm-proof" "tickle-work-queue"]
     :problem-id (str "apm-" (:id problem))
     :problem-base (:id problem)
     :subject (:subject problem)
     :year (:year problem)
     :subpart-count (:subpart-count problem)}))

;; =============================================================================
;; Per-phase prompts (for phase-by-phase dispatch)
;; =============================================================================

(defn- problem-header [problem tex-body]
  (let [{:keys [id subject year session subpart-count subparts]} problem]
    (str "Problem: apm-" id " (" (get subject-names subject (name subject))
         ", " year ", " (if (= session :fall) "Fall" "Spring") ")\n\n"
         "```latex\n" tex-body "\n```\n"
         (when (and subparts (pos? subpart-count))
           (str "\nSub-parts: " (str/join ", " (map :label subparts)) "\n")))))

(defn make-observe-prompt
  ([problem tex-body] (make-observe-prompt problem tex-body nil))
  ([problem tex-body frame-context]
   (str "Execution evidence required: no.\n"
        "This is a text-authoring phase inside the proof peripheral. The required deliverable is substantive inline notes, not tool activity.\n\n"
        "You are in the OBSERVE phase of the proof peripheral.\n\n"
        (problem-header problem tex-body)
        (when frame-context
          (str "\nFrame workspace root:\n"
               (or (:workspace-root frame-context) "") "\n"))
        "\nWrite two labeled sections:\n"
        "- **WHAT IS REALLY BEING ASKED**: Restate the problem. What mathematical objects? What is the real question?\n"
        "- **WHY IT IS HARD**: Where do students get stuck? What trap? What prerequisite is being tested?\n\n"
        "Your reply is the authoritative phase record. Do not answer with a summary like "
        "\"wrote this to file X\" or \"see notes\". Emit the actual OBSERVE content inline. "
        "If you also write a sidecar file, mirror the same content here.\n\n"
        "Write for a strong undergrad. This becomes the cheatsheet's opening exposition.\n")))

(defn make-propose-prompt
  ([problem tex-body observe-notes] (make-propose-prompt problem tex-body observe-notes nil))
  ([problem tex-body observe-notes frame-context]
   (str "Execution evidence required: no.\n"
        "This is a text-authoring phase inside the proof peripheral. The required deliverable is substantive inline notes, not tool activity.\n\n"
        "You are in the PROPOSE phase of the proof peripheral.\n\n"
        (problem-header problem tex-body)
        (when frame-context
          (str "\nFrame workspace root:\n"
               (or (:workspace-root frame-context) "") "\n"))
        "\nYour OBSERVE notes:\n" observe-notes "\n\n"
        "Write two labeled sections:\n"
        "- **THE KEY INSIGHT**: The single idea that unlocks this problem. Proof strategy and why.\n"
        "- **THE NAIVE APPROACH THAT FAILS**: What does a student try first? Why does it fail? What structural obstruction blocks it?\n\n"
        "Your reply is the authoritative phase record. Do not summarize the file you wrote; "
        "emit the actual PROPOSE notes inline.\n")))

(defn make-execute-prompt
  ([problem tex-body observe-notes propose-notes]
   (make-execute-prompt problem tex-body observe-notes propose-notes nil))
  ([problem tex-body observe-notes propose-notes frame-context]
   (str "Execution evidence required: yes.\n"
        "This phase must include real Lean/tool work in Stage 3. A purely textual reply is insufficient.\n\n"
        "You are in the EXECUTE phase of the proof peripheral.\n\n"
        (problem-header problem tex-body)
        (when frame-context
          (str "\nFrame workspace root:\n"
               (or (:workspace-root frame-context) "") "\n"
               "Frame-local Lean module root:\n"
               (or (:module-root frame-context) "") "\n"
               "Frame-local Lean files:\n"
               "- " (or (:lean-main-path frame-context) "") "\n"
               "- " (or (:lean-scratch-path frame-context) "") "\n"
               "Frame-local proof plan path:\n"
               (or (:proof-plan-path frame-context) "") "\n"
               "Frame-local changelog path:\n"
               (or (:changelog-path frame-context) "") "\n"
               "Shared extension root for promoted lemmas:\n"
               (or (:shared-extension-root frame-context) "") "\n\n"))
        "\nYour OBSERVE notes:\n" observe-notes "\n"
        "\nYour PROPOSE notes:\n" propose-notes "\n\n"
        "THREE STAGES IN ORDER:\n\n"
        "**Stage 1 — THE CLEAN PROOF**: Complete, readable informal proof. Not a sketch. "
        "For multi-part problems, prove each part. Write for a reader who could follow line by line.\n\n"
        "**Stage 2 — LEMMA DEPENDENCY GRAPH**: Before Lean, extract the logical structure:\n"
        "- List each lemma/fact used, in dependency order\n"
        "- For every entry, record both the FORMAL dependency and the INFORMAL dependency\n"
        "  that made it visible. We are tracing proof strategy, not just theorem lookup.\n"
        "- Use this exact template for each entry:\n"
        "  1. **Dependency Name**\n"
        "     - **Formal dependency**: theorem/lemma/definition actually used\n"
        "     - **Informal dependency**: recognition heuristic, pattern, or hidden move\n"
        "     - **Why this becomes thinkable here**: local cue in the problem\n"
        "     - **Lean target/type**: expected Lean type signature or goal shape\n"
        "     - **Mathlib status/search terms**: likely Mathlib vs custom, plus search terms\n"
        "     - **Critical path**: yes/no + why\n\n"
        "After the dependency graph, emit a machine-readable `proof-plan.edn` block in\n"
        "this exact form:\n"
        "```edn\n"
        "{:goal \"...\"\n"
        " :terms [{:name \"...\" :meaning \"...\" :needed-because \"...\"}]\n"
        " :strategy [{:id :step-1\n"
        "             :formal-dependency \"...\"\n"
        "             :informal-dependency \"...\"\n"
        "             :why-this-now \"...\"\n"
        "             :lean-target \"...\"\n"
        "             :mathlib-status \"...\"\n"
        "             :critical-path true}]\n"
        " :stage-status {:stage1 :done :stage2 :done :stage3 :in-progress :stage4 :pending}}\n"
        "```\n"
        "This is the HtDP artifact. It must stay valid throughout execute.\n\n"
        "**Stage 3 — LEAN FORMALIZATION**: 15-minute exam timer starts NOW.\n"
        "Build the skeleton from your dependency graph. Use HtDP to attack each sorry:\n"
        "1. Type signature (from dependency graph)\n"
        "2. Search Mathlib (exact?, apply?, grep)\n"
        "3. Sketch composition\n"
        "4. Wire and build (lake build, read error, fix, repeat)\n"
        "The skeleton must type-check. Sorry is localized to specific claims.\n"
        (when frame-context
          (str "Work only in the frame-local Lean files above. Do not use shared scratch paths "
               "such as `ApmCanaries/Current.lean` or global `lean-proofs/<problem>/Main.lean`. "
               "If material becomes reusable, promote it explicitly into "
               (or (:shared-extension-root frame-context) "the shared extension root")
               ", but keep exploratory work frame-local.\n"))
        "\nAt EACH execute return, update the plan if needed and show concrete progress.\n"
        "The conductor maintains a `changelog.edn`; your Stage 3 text must make clear what\n"
        "changed this click: which lemma closed, which search failed, which blocker moved,\n"
        "or how the plan changed.\n\n"
        "**Stage 4 — FORMAL-TO-INFORMAL REVISION**: Reread THE CLEAN PROOF. Add "
        "natural-language difficulty annotations grounded in the Lean work. "
        "No Lean references in Column 1 prose — just calibrated difficulty signals. "
        "Lean details go in VALIDATE/CLASSIFY (Column 3).\n\n"
        "Your reply is the authoritative EXECUTE record. Do not answer with a summary such as "
        "\"Stage 1 is in file X\" or \"see notes\". Emit the actual Stage 1-4 content inline. "
        "If you also maintain a sidecar file, keep it in correspondence with this reply.\n")))

(defn make-validate-prompt [_problem execute-notes lean-status]
  (str "Execution evidence required: no.\n"
       "This is a text-authoring phase inside the proof peripheral. The required deliverable is substantive inline notes, not tool activity.\n\n"
       "You are in the VALIDATE phase.\n\n"
       "Your EXECUTE output:\n" execute-notes "\n"
       "\nLean build status: " lean-status "\n\n"
       "Write:\n"
       "- Non-circularity analysis: list independent facts, confirm none assumes conclusion\n"
       "- Lean status: build result, sorry count, what blocked each sorry\n"
       "- If sorry > 0: for each sorry, an HtDP sketch (which API, what composition, what gap)\n\n"
       "Your reply is the authoritative VALIDATE record. Do not answer with a summary like "
       "\"documented in file X\". Emit the actual validation text inline.\n"))

(defn make-classify-prompt [_problem validate-notes]
  (str "Execution evidence required: no.\n"
       "This is a text-authoring phase inside the proof peripheral. The required deliverable is substantive inline notes, not tool activity.\n\n"
       "You are in the CLASSIFY phase.\n\n"
       "Your VALIDATE notes:\n" validate-notes "\n\n"
       "Write two labeled sections:\n"
       "- **Classification**: proved / partial (explain what resisted + what you tried) / build-failed / timed-out\n"
       "- **CONFIDENCE INVERSION**: What is surprising? Where is intuition wrong? Name the surprise.\n\n"
       "Your reply is the authoritative CLASSIFY record. Do not replace it with file pointers or a summary.\n"))

(defn make-integrate-prompt [_problem all-phase-notes]
  (str "Execution evidence required: no.\n"
       "This is a text-authoring phase inside the proof peripheral. The required deliverable is substantive inline notes, not tool activity.\n\n"
       "You are in the INTEGRATE phase.\n\n"
       "Summary of all phases so far:\n" all-phase-notes "\n\n"
       "Write two labeled sections:\n"
       "- **Connections**: What other mathematics? Where else does this technique appear?\n"
       "- **EXAM-DAY FIELD KIT**: 3-5 facts a student needs ready. Specific lemmas/tricks that compose.\n\n"
       "Then write 5 ArSE questions:\n"
       "1. why-hard (from observe): Why is this hard?\n"
       "2. what-crux (from propose): What is the key insight?\n"
       "3. why-works (from validate): Why does step N work?\n"
       "4. what-connects (from integrate): What connects to this?\n"
       "5. confidence (from classify): Where is intuition wrong?\n"
       "For each: the question AND a brief answer. Review your notes — if a question "
       "exposes a gap, note what needs revision.\n\n"
       "Your reply is the authoritative INTEGRATE record. Do not answer with placeholders "
       "such as \"Q1\"/\"see notes\" or with a summary that points to another file.\n"))

;; =============================================================================
;; Canaries
;; =============================================================================

(def canary-ids
  "The 4 canary problems used for smoke testing."
  #{"t01A01" "a96A03" "b03J02" "m96A04"})

;; =============================================================================
;; APM phase validator — structural enforcement for proof peripheral
;; =============================================================================

(defn- has-lean-artifacts?
  "Check if artifacts list contains at least one .lean file path."
  [artifacts]
  (some #(str/ends-with? (str %) ".lean") artifacts))

(def ^:private indirect-notes-pattern
  "Detect notes that are just pointers to external files, not substantive content.
   The file-path trigger requires the path to appear within ~80 chars of the verb,
   so mathematical prose ('documented that the bound holds...') does not false-positive."
  #"(?is)\bsee\s+(?:notes?|file|artifact|writeup|sidecar)\b|\b(?:documented|recorded|captured|consolidated|saved|written)\b\s+(?:in|into)\b.{0,80}?(?:\.md|\.lean|proof_peripheral|lean-proofs)")

(defn- indirect-notes?
  [notes]
  (boolean (re-find indirect-notes-pattern (or notes ""))))

(defn- placeholder-dependency-graph?
  [dep-graph]
  (or (empty? dep-graph)
      (every? (fn [entry]
                (let [lemma (str/lower-case (str (or (:lemma entry) "")))
                      lean-type (str/lower-case (str (or (:lean-type entry) "")))
                      notes (str/lower-case (str (or (:notes entry) "")))]
                  (or (= lemma "see-notes")
                      (= lean-type "see-notes")
                      (str/includes? notes "see-notes"))))
              dep-graph)))

(defn- rich-dependency-entry?
  [entry]
  (let [lemma (str/trim (str (or (:lemma entry) "")))
        formal (str/trim (str (or (:formal-dependency entry) "")))
        informal (str/trim (str (or (:informal-dependency entry) "")))
        why-now (str/trim (str (or (:why-this-now entry) "")))
        lean-type (str/trim (str (or (:lean-type entry) "")))]
    (and (not (str/blank? lemma))
         (not= "unparsed-lemma" (str/lower-case lemma))
         (not (str/blank? formal))
         (not (str/blank? informal))
         (not (str/blank? why-now))
         (not (str/blank? lean-type)))))

(defn- strategic-dependency-graph?
  [dep-graph]
  (and (seq dep-graph)
       (every? rich-dependency-entry? dep-graph)))

(def ^:private valid-stage-statuses
  #{:pending :in-progress :done :revised :blocked})

(defn valid-proof-plan?
  [plan]
  (let [goal (str/trim (str (or (:goal plan) "")))
        terms (:terms plan)
        strategy (:strategy plan)
        stage-status (:stage-status plan)
        valid-term? (fn [term]
                      (every? #(not (str/blank? (str/trim (str (or (% term) "")))))
                              [:name :meaning :needed-because]))
        valid-step? (fn [step]
                      (and (contains? step :id)
                           (every? #(not (str/blank? (str/trim (str (or (% step) "")))))
                                   [:formal-dependency
                                    :informal-dependency
                                    :why-this-now
                                    :lean-target
                                    :mathlib-status])))
        valid-stage-status? (fn [m]
                              (and (map? m)
                                   (every? #(contains? m %) [:stage1 :stage2 :stage3 :stage4])
                                   (every? valid-stage-statuses (vals m))))]
    (and (map? plan)
         (not (str/blank? goal))
         (vector? terms)
         (seq terms)
         (every? valid-term? terms)
         (vector? strategy)
         (seq strategy)
         (every? valid-step? strategy)
         (valid-stage-status? stage-status))))

(defn valid-changelog?
  [changelog]
  (and (vector? changelog)
       (seq changelog)
       (every? (fn [entry]
                 (and (keyword? (:kind entry))
                      (not (str/blank? (str/trim (str (or (:summary entry) "")))))
                      (contains? entry :full-record?)
                      (contains? entry :sorry?)
                      (contains? entry :fully-closed?)))
               changelog)))

(defn- placeholder-arse-questions?
  [arse]
  (or (empty? arse)
      (some (fn [{:keys [question answer]}]
              (let [q (str/lower-case (str/trim (or question "")))
                    a (str/lower-case (str/trim (or answer "")))]
                (or (re-matches #"q\d+" q)
                    (= a "see notes")
                    (= q "see notes"))))
            arse)))

(defn apm-phase-validator
  "Phase validator for APM problems. Returns nil on success, or an error
   map {:ok false :error {...}} on validation failure.

   Enforces:
   - Execute: Lean artifacts required (or :lean-timed-out report),
     :dependency-graph required, :notes required, :lean-elapsed-ms required
   - Validate: :notes required; if sorry > 0, :sorry-blockers required
   - Classify: :notes required
   - Integrate: :notes required, :arse-questions required (5 items)"
  [phase phase-data _cycle]
  (case phase
    :execute
    (let [artifacts (:artifacts phase-data)
          has-lean (has-lean-artifacts? artifacts)
          timed-out (:lean-timed-out phase-data)
          dep-graph (:dependency-graph phase-data)
          proof-plan (:proof-plan phase-data)
          changelog (:changelog phase-data)
          notes (:notes phase-data)
          elapsed (:lean-elapsed-ms phase-data)]
      (cond
        (str/blank? notes)
        {:ok false :error {:code :missing-notes
                           :message "Execute phase requires :notes (THE CLEAN PROOF)"}}
        (indirect-notes? notes)
        {:ok false :error {:code :indirect-notes
                           :message "Execute phase notes must contain the actual Stage 1-4 content, not a file summary or 'see notes' indirection"}}
        (or (nil? dep-graph) (empty? dep-graph))
        {:ok false :error {:code :missing-dependency-graph
                           :message "Execute phase requires :dependency-graph (lemma build plan)"}}
        (placeholder-dependency-graph? dep-graph)
        {:ok false :error {:code :placeholder-dependency-graph
                           :message "Execute phase dependency graph must contain concrete lemma entries, not placeholders like 'see-notes'"}}
        (not (strategic-dependency-graph? dep-graph))
        {:ok false :error {:code :underpowered-dependency-graph
                           :message "Execute phase dependency graph must record formal dependencies, informal strategy triggers, why-they-apply-here cues, and Lean targets for every entry"}}
        (not (valid-proof-plan? proof-plan))
        {:ok false :error {:code :invalid-proof-plan
                           :message "Execute phase requires a valid proof-plan.edn artifact with goal, terms, strategy, and stage-status"}}
        (not (valid-changelog? changelog))
        {:ok false :error {:code :invalid-changelog
                           :message "Execute phase requires a non-empty changelog.edn artifact showing concrete progress across clicks"}}
        (and (not has-lean) (nil? timed-out))
        {:ok false :error {:code :no-lean-work
                           :message "Execute phase requires Lean artifacts (.lean files) or :lean-timed-out report. Cannot advance without attempting Lean formalization."}}
        (nil? elapsed)
        {:ok false :error {:code :missing-lean-elapsed
                           :message "Execute phase requires :lean-elapsed-ms (time spent on Lean)"}}
        (and elapsed (< elapsed 120000) (not has-lean) (nil? timed-out))
        {:ok false :error {:code :rushed-lean
                           :message (str "Lean phase completed in " (/ elapsed 1000)
                                         "s — minimum 2 minutes expected")}}
        :else nil))

    :validate
    (cond
      (str/blank? (:notes phase-data))
      {:ok false :error {:code :missing-notes
                         :message "Validate phase requires :notes (non-circularity analysis + Lean status)"}}
      (indirect-notes? (:notes phase-data))
      {:ok false :error {:code :indirect-notes
                         :message "Validate phase notes must contain the actual validation analysis, not a file summary or 'see notes' indirection"}})

    :classify
    (cond
      (str/blank? (:notes phase-data))
      {:ok false :error {:code :missing-notes
                         :message "Classify phase requires :notes (CONFIDENCE INVERSION)"}}
      (indirect-notes? (:notes phase-data))
      {:ok false :error {:code :indirect-notes
                         :message "Classify phase notes must contain the actual classification content, not a file summary or 'see notes' indirection"}})

    :integrate
    (let [notes (:notes phase-data)
          arse (:arse-questions phase-data)]
      (cond
        (str/blank? notes)
        {:ok false :error {:code :missing-notes
                           :message "Integrate phase requires :notes (EXAM-DAY FIELD KIT)"}}
        (indirect-notes? notes)
        {:ok false :error {:code :indirect-notes
                           :message "Integrate phase notes must contain the actual connections/field-kit text, not a file summary or 'see notes' indirection"}}
        (or (nil? arse) (< (count arse) 5))
        {:ok false :error {:code :missing-arse-questions
                           :message (str "Integrate phase requires :arse-questions (5 items, got "
                                         (count arse) ")")}}
        (placeholder-arse-questions? arse)
        {:ok false :error {:code :placeholder-arse-questions
                           :message "Integrate phase ArSE questions must contain real question/answer text, not placeholders like 'Q1' or 'see notes'"}}
        :else nil))

    ;; Other phases: no APM-specific enforcement
    nil))

;; =============================================================================
;; Progress tracking via evidence store
;; =============================================================================

(defn completed-problem-ids
  "Query evidence store for APM problem IDs already processed.
   Returns a set of problem-base strings (e.g. \"t01A01\")."
  [evidence-store]
  (if-not evidence-store
    #{}
    (let [entries (estore/query* evidence-store {})
          apm-entries (->> entries
                          (filter #(some #{:apm-proof} (:evidence/tags %)))
                          (filter #(some #{:workflow-complete :kick-complete}
                                         (:evidence/tags %))))]
      (->> apm-entries
           (keep #(get-in % [:evidence/body :problem-base]))
           set))))

(defn queue-status
  "Return a summary of APM queue progress.
   Optional :subject filters by subject keyword."
  [evidence-store & {:keys [subject]}]
  (let [manifest (cond->> (load-apm-manifest)
                   subject (filter #(= subject (:subject %))))
        done (completed-problem-ids evidence-store)
        total (count manifest)]
    {:total total
     :completed (count (filter #(done (:id %)) manifest))
     :remaining (count (remove #(done (:id %)) manifest))
     :completed-ids done
     :subject (or subject :all)}))

(defn next-unprocessed
  "Return the next N unprocessed APM problems as issue maps.
   Optional :subject filters by subject keyword.
   Optional :canary restricts to the 4 canary problems."
  ([evidence-store] (next-unprocessed evidence-store 10))
  ([evidence-store n & {:keys [subject canary]}]
   (let [manifest (cond->> (load-apm-manifest)
                    subject (filter #(= subject (:subject %)))
                    canary  (filter #(canary-ids (:id %))))
         done (completed-problem-ids evidence-store)]
     (->> manifest
          (map-indexed (fn [idx p]
                         (when-not (done (:id p))
                           (entity->issue p idx))))
          (remove nil?)
          (take n)
          vec))))

;; =============================================================================
;; Evidence emission
;; =============================================================================

(defn emit-apm-evidence!
  "Emit APM proof evidence."
  [evidence-store {:keys [problem-id problem-base subject session-id event-tag
                          agent-result lean-status sorry-count classification]}]
  (when evidence-store
    (estore/append* evidence-store
                    {:subject {:ref/type :task
                               :ref/id problem-id}
                     :type :coordination
                     :claim-type (case event-tag
                                   :workflow-start :goal
                                   :cycle-complete :observation
                                   :lean-complete :observation
                                   :workflow-complete :observation
                                   :observation)
                     :author "tickle-1"
                     :tags [:tickle :apm-proof event-tag]
                     :session-id session-id
                     :body (cond-> {:problem-id problem-id
                                    :problem-base problem-base
                                    :subject subject
                                    :at (str (Instant/now))}
                             agent-result (assoc :result-preview
                                                 (subs agent-result
                                                       0 (min 500 (count agent-result))))
                             lean-status (assoc :lean-status lean-status)
                             sorry-count (assoc :sorry-count sorry-count)
                             classification (assoc :classification classification))})))

;; =============================================================================
;; Batch helpers
;; =============================================================================

(defn problems-by-subject
  "Group manifest entries by subject."
  []
  (group-by :subject (load-apm-manifest)))

(defn subject-summary
  "Return problem counts by subject."
  []
  (->> (problems-by-subject)
       (map (fn [[s ps]] [s (count ps)]))
       (into (sorted-map))))
