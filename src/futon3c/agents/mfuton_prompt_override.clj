(ns futon3c.agents.mfuton-prompt-override
  "mfuton-local prompt rewrites for FrontierMath-local bringup.

   Organization rule:
   - keep the grouping/order in sync with the companion dependency map under
     `mfuton/.../frontiermath-local-github-dependency-map-2026-03-19.md`
   - keep original prompt text in the source call sites and apply bounded
     rewrites here when mfuton mode is `mfuton`."
  (:require [clojure.string :as str]
            [futon3c.mfuton-mode :as mfuton-mode]))

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
       "6. Remind agents to push artifacts to git — never reference local paths or /tmp."
       "6. Remind agents that Git is truth and they should run the commit algorithm for gh to publish artifacts — never reference local paths or /tmp.")))

(defn maybe-build-fm-context
  [original-prompt]
  (if (mfuton-mode/mfuton-mode?)
    (build-fm-context-override original-prompt)
    original-prompt))

(defn fm-dispatch-message-override
  [original-message]
  (str/replace
   original-message
   ". Push results to git when done."
   ". Git is truth; run the commit algorithm for gh when done."))

(defn maybe-fm-dispatch-message
  [original-message]
  (if (mfuton-mode/mfuton-mode?)
    (fm-dispatch-message-override original-message)
    original-message))

(defn task-prompt-override
  [original-prompt]
  (str/replace
   original-prompt
   "\nWork this task. Push results to git when done. "
   "\nWork this task. Git is truth; run the commit algorithm for gh when done. "))

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
       "- Task: Implement tracked work item #")
      (str/replace
       "--- GitHub Issue #"
       "--- tracked work item #")))

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
       "- Task: Review implementation of tracked work item #")
      (str/replace
       "--- GitHub Issue #"
       "--- tracked work item #")))

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
