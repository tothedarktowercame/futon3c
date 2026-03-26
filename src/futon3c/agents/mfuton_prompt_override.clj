(ns futon3c.agents.mfuton-prompt-override
  "mfuton-local prompt rewrites for FrontierMath-local bringup.

   Organization rule:
   - keep the grouping/order in sync with the companion dependency map under
     `mfuton/.../frontiermath-local-github-dependency-map-2026-03-19.md`
   - keep original prompt text in the source call sites and apply bounded
     rewrites here when mfuton mode is `mfuton`."
  (:require [clojure.string :as str]
            [futon3c.dev.config :as config]
            [futon3c.mfuton-mode :as mfuton-mode]))

(def ^:private local-t3-general-label
  "Tier 3: general algorithm for all n, <10min for n<=100")

(def ^:private local-t3-helper-path
  "mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/2026-03-26-generated-witness/scripts/fm001/generate_witness.py")

(def ^:private local-t3-note-path
  "mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/2026-03-26-n3-search.md")

(def ^:private local-t3-iterations-path
  "mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/2026-03-26-n3-search-iterations.md")

(def ^:private generic-git-tail
  ". Git is truth; run the commit algorithm for gh when done. Signal @tickle BELL SPEC_VERIFIED when done.")

(def ^:private local-mfuton-gitlab-issue-ref
  "mfuton gitlab issue #")

(def ^:private upstream-frontiermath-source-root-ref
  "futon6")

(def ^:private upstream-strategy-doc-ref
  "futon6/data/frontiermath-pilot/FM-001-strategy.md")

(def ^:private upstream-problem-state-doc-ref
  "futon6/data/first-proof/frontiermath-pilot/FM-001-ramsey-book-graphs-state.md")

(def ^:private local-t3-dispatch-re
  #"^(@[^\s]+)\s+T3-general:\s+Tier 3: general algorithm for all n, <10min for n<=100\.\s+Push results to git when done\.$")

(defn- local-t3-dispatch-message
  [recipient]
  (str recipient
       " T3-general control: run exactly one n=3 control using "
       local-t3-helper-path
       " --check 3 --method wesley, then update "
       local-t3-note-path
       " and "
       local-t3-iterations-path
       ". Report exact file and one-line result."
       generic-git-tail))

(defn- local-t3-task-prompt?
  [prompt]
  (and (str/includes? prompt "Task ID: T3-general")
       (str/includes? prompt (str "Task: " local-t3-general-label))))

(defn- local-t3-task-prompt
  [original-prompt]
  (-> original-prompt
      (str/replace
       (str "Task: " local-t3-general-label)
       "Task: FrontierMath local n=3 orchestration control")
      (str/replace
       "\nWork this task. Push results to git when done. "
       (str "\nUse the mfuton helper at "
            local-t3-helper-path
            " with `--check 3 --method wesley`.\n"
            "Update "
            local-t3-note-path
            " and "
            local-t3-iterations-path
            " with the result.\n"
            "Work this task"
            generic-git-tail
            " "))))

(defn- configured-frontiermath-root
  []
  (some-> (config/env "FUTON3C_FRONTIERMATH_ROOT") str/trim not-empty))

(defn- frontiermath-source-root-ref
  []
  (let [configured (configured-frontiermath-root)
        mfuton-home (some-> (config/env "MFUTON_HOME") str/trim not-empty)]
    (cond
      (str/blank? configured)
      upstream-frontiermath-source-root-ref

      (and mfuton-home
           (str/starts-with? (str/lower-case configured)
                             (str/lower-case mfuton-home)))
      (let [suffix (subs configured (count mfuton-home))
            normalized (-> suffix
                           (str/replace "\\" "/")
                           (str/replace #"^/+" ""))]
        (if (str/blank? normalized)
          "mfuton"
          (str "mfuton/" normalized)))

      :else
      (str/replace configured "\\" "/"))))

(defn- frontiermath-doc-ref
  [relative-path]
  (let [root-ref (frontiermath-source-root-ref)
        relative-path (-> (or relative-path "")
                          (str/replace "\\" "/")
                          (str/replace #"^/+" ""))]
    (if (str/blank? relative-path)
      root-ref
      (str root-ref "/" relative-path))))

(defn- frontiermath-strategy-doc-ref
  []
  (frontiermath-doc-ref "data/frontiermath-pilot/FM-001-strategy.md"))

(defn- frontiermath-problem-state-doc-ref
  []
  (frontiermath-doc-ref
   "data/first-proof/frontiermath-pilot/FM-001-ramsey-book-graphs-state.md"))

;; ---------------------------------------------------------------------------
;; Group 1 — Theorem Artifact Authority and Durable Citation
;; ---------------------------------------------------------------------------
;;
;; No active rewrite is implemented here yet. The current recovery path keeps
;; the generic upstream artifact-ref slot intact while the prompt seam is
;; narrowed separately.

;; ---------------------------------------------------------------------------
;; Group 2 — Conductor / Assignment / Review Semantics
;; ---------------------------------------------------------------------------

(defn build-fm-context-override
  [original-prompt]
  (-> original-prompt
      (str/replace
       (str "Problem state doc: " upstream-problem-state-doc-ref)
       (str "Problem state doc: " (frontiermath-problem-state-doc-ref)))
      (str/replace
       (str "Strategy doc: " upstream-strategy-doc-ref)
       (str "Strategy doc: " (frontiermath-strategy-doc-ref)))
      (str/replace
       "6. Remind agents to push artifacts to git — never reference local paths or /tmp."
       "6. Remind agents that Git is truth and they should run the commit algorithm for gh to publish artifacts — never reference local paths or /tmp.")))

(defn maybe-build-fm-context
  [original-prompt]
  (if (mfuton-mode/mfuton-mode?)
    (build-fm-context-override original-prompt)
    original-prompt))

(defn fm-dispatch-message-override
  [original-message]
  (if-let [[_ recipient] (re-find local-t3-dispatch-re original-message)]
    (local-t3-dispatch-message recipient)
    (str/replace
     original-message
     ". Push results to git when done."
     generic-git-tail)))

(defn maybe-fm-dispatch-message
  [original-message]
  (if (mfuton-mode/mfuton-mode?)
    (fm-dispatch-message-override original-message)
    original-message))

(defn task-prompt-override
  [original-prompt]
  (if (local-t3-task-prompt? original-prompt)
    (local-t3-task-prompt original-prompt)
    (str/replace
     original-prompt
     "\nWork this task. Push results to git when done. "
     (str "\nWork this task" generic-git-tail " "))))

(defn maybe-task-prompt
  [original-prompt]
  (if (mfuton-mode/mfuton-mode?)
    (task-prompt-override original-prompt)
    original-prompt))

(defn assign-prompt-override
  [original-prompt]
  (-> original-prompt
      (str/replace
       "- Task: Implement GitHub issue #"
       (str "- Task: Implement " local-mfuton-gitlab-issue-ref))
      (str/replace
       "--- GitHub Issue #"
       (str "--- " local-mfuton-gitlab-issue-ref))))

(defn maybe-assign-prompt
  [original-prompt]
  (if (mfuton-mode/mfuton-mode?)
    (assign-prompt-override original-prompt)
    original-prompt))

(defn review-prompt-override
  [original-prompt]
  (-> original-prompt
      (str/replace
       "- Task: Review implementation of GitHub issue #"
       (str "- Task: Review implementation of " local-mfuton-gitlab-issue-ref))
      (str/replace
       "--- GitHub Issue #"
       (str "--- " local-mfuton-gitlab-issue-ref))))

(defn maybe-review-prompt
  [original-prompt]
  (if (mfuton-mode/mfuton-mode?)
    (review-prompt-override original-prompt)
    original-prompt))

;; ---------------------------------------------------------------------------
;; Group 3 — Room and Job Projection Surfaces
;; ---------------------------------------------------------------------------
;;
;; Deferred for now. The current room/job prompt leaves include Python surfaces
;; and require a separate implementation decision that must still respect the
;; mission's default-preserving override rules.

;; ---------------------------------------------------------------------------
;; Group 4 — External Engineering-Shell / Work-Item Surfaces
;; ---------------------------------------------------------------------------
;;
;; Deferred for now. The current recovery path keeps the deeper read/publish
;; shell on upstream `gh` issue surfaces while the prompt seam stays generic.
