(ns futon3c.apm.apparatus-pin
  "Validate frozen experimental apparatus by Git object identity.

  The developer checkout is deliberately irrelevant: a pin is a repository,
  branch ancestry witness, commit, and path->blob map resolved at that commit."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def sha1-pattern #"[0-9a-f]{40}")

(defn- git [repository & args]
  (apply shell/sh (concat ["git" "-C" repository] args)))

(defn- successful-output [result]
  (when (zero? (:exit result)) (str/trim (:out result))))

(defn- artifact-pins [apparatus]
  (concat
   (map (fn [[role pin]] (assoc pin :artifact/id role :artifact/kind :role-card))
        (:role-cards apparatus))
   (map (fn [[id pin]] (assoc pin :artifact/id id :artifact/kind :file))
        (:files apparatus))))

(defn validate
  "Return a replayable observation of APPARATUS without reading the worktree.

  The pinned commit need only be an ancestor of the named branch: advancing
  that branch does not invalidate an already-open frame, while deleting or
  rewriting away the ancestry does. Every registered artifact is resolved as
  REVISION:PATH and compared with its pinned blob."
  [apparatus]
  (let [{:keys [repository branch revision]} apparatus
        shape-valid? (and (string? repository) (not (str/blank? repository))
                          (string? branch) (not (str/blank? branch))
                          (string? revision) (re-matches sha1-pattern revision)
                          (map? (:role-cards apparatus)))
        repo-check (when shape-valid? (git repository "rev-parse" "--git-dir"))
        commit-check (when (zero? (or (:exit repo-check) 1))
                       (git repository "cat-file" "-e"
                            (str revision "^{commit}")))
        branch-ref (str "refs/heads/" branch)
        branch-check (when (zero? (or (:exit commit-check) 1))
                       (git repository "rev-parse" "--verify" branch-ref))
        ancestry-check (when (zero? (or (:exit branch-check) 1))
                         (git repository "merge-base" "--is-ancestor"
                              revision branch-ref))
        artifacts
        (when (zero? (or (:exit ancestry-check) 1))
          (mapv
           (fn [{:keys [artifact/id artifact/kind path blob]}]
             (let [observed-result (git repository "rev-parse"
                                        (str revision ":" path))
                   observed (successful-output observed-result)]
               {:artifact/id id :artifact/kind kind :path path
                :expected-blob blob :observed-blob observed
                :exists? (zero? (:exit observed-result))
                :matches? (and (re-matches sha1-pattern (or blob ""))
                               (= blob observed))}))
           (artifact-pins apparatus)))
        findings
        (cond-> []
          (not shape-valid?) (conj :apparatus-pin-shape-invalid)
          (and shape-valid? (not (zero? (or (:exit repo-check) 1))))
          (conj :apparatus-repository-unavailable)
          (and repo-check (zero? (:exit repo-check))
               (not (zero? (or (:exit commit-check) 1))))
          (conj :apparatus-commit-unavailable)
          (and commit-check (zero? (:exit commit-check))
               (not (zero? (or (:exit branch-check) 1))))
          (conj :apparatus-branch-unavailable)
          (and branch-check (zero? (:exit branch-check))
               (not (zero? (or (:exit ancestry-check) 1))))
          (conj :apparatus-commit-not-on-branch)
          (some #(not (:matches? %)) artifacts)
          (conj :apparatus-artifact-blob-mismatch))]
    {:valid? (empty? findings)
     :repository repository :branch branch :revision revision
     :branch-head (successful-output branch-check)
     :commit-on-branch? (zero? (or (:exit ancestry-check) 1))
     :artifacts (or artifacts [])
     :findings findings
     :worktree-consulted? false}))
