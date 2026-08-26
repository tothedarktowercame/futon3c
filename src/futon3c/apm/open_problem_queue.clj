(ns futon3c.apm.open-problem-queue
  "Derive a content-addressable JIT queue from the pinned APM Lean corpus."
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def excluded-classifications
  "Classifications that keep a problem OUT of the open non-topology queue.

  \"construction-blocked\" is not a defect in the statement: it means the Solver
  reached the round limit against absent library infrastructure rather than
  against the mathematics. Such a problem belongs with the topology/construction
  work until the missing target exists, and re-admitting it costs ~50 solver
  rounds to rediscover the same gap. f36/a96A07 and f43/a97J08 were each parked
  for want of the same planar Jordan/winding machinery, and both remained
  SELECTABLE afterwards -- the park lives in queue-state.edn, which this
  selection never reads."
  #{"defective" "partial-invalid-statement" "statement-defective"
    "construction-blocked"})

(defn- git-out [repository & args]
  (let [result (apply shell/sh (concat ["git" "-C" repository] args))]
    (when (zero? (:exit result)) (str/trim (:out result)))))

(defn- status-blobs
  "Read every problems/*/status.json AT REVISION, as [path content] pairs.

  Deliberately NOT the working tree. Reading the checkout made the corpus
  depend on whichever branch happened to be checked out: on 2026-08-26
  ~/code/apm-lean sat on a colleague's `repair/m97A06-energy-regularity`, so a
  launch would have derived the corpus from that branch and silently re-admitted
  a96A07 and a97J08 -- both just classified construction-blocked on master after
  each burned ~50 solver rounds against the same absent planar-topology
  machinery -- along with four problems already banked as solved. The pinned
  :revision would have recorded that branch, so the frame records would have
  looked perfectly consistent."
  [repository revision]
  (->> (or (git-out repository "ls-tree" "-r" "--name-only" revision "problems/")
           "")
       str/split-lines
       (filter #(re-matches #"problems/[^/]+/status\.json" %))
       sort
       (keep (fn [path]
               (when-let [content (git-out repository "show"
                                           (str revision ":" path))]
                 [path content])))
       vec))

(defn derive-queue
  "Return immutable problem pins plus an auditable exclusion ledger.

  Open means the canonical bundle status records at least one remaining Lean
  sorry. Topology is the corpus's `t` subject. Defective/invalid statements
  are retained in :excluded rather than silently disappearing."
  ([repository] (derive-queue repository nil))
  ([repository rev]
  (let [rev (or rev "origin/master")
        revision (git-out repository "rev-parse" rev)
        ;; :branch must stay a name a worktree can be created ON.
        ;; queued-frame-adapter uses :base-branch for exactly that, and a
        ;; remote-tracking ref like origin/master is not checkout-able — so
        ;; report the local branch the rev corresponds to, not the rev we read.
        branch (str/replace-first (str rev) #"^origin/" "")
        rows (mapv (fn [[_path content]]
                     (let [status (json/read-str content :key-fn keyword)
                           id (:problem_id status)
                           classification (:classification status)
                           sorry-count (long (or (get-in status [:lean :sorry_count_total]) 0))
                           path (str "problems/" id "/lean/Main.lean")
                           reason (cond
                                    (zero? sorry-count) :not-open
                                    (str/starts-with? id "t") :topology
                                    (= "construction-blocked" classification)
                                    ;; Not a defect. The Solver ran out of
                                    ;; rounds against absent library
                                    ;; infrastructure, so the exclusion ledger
                                    ;; must not label it the same as a broken
                                    ;; statement -- these come back when the
                                    ;; construction target exists.
                                    :construction-blocked
                                    (contains? excluded-classifications classification)
                                    :defective-or-invalid-statement
                                    :else nil)]
                       {:problem/id id :classification classification
                        :sorry-count sorry-count :path path :reason reason}))
                   (status-blobs repository revision))
        selected (filterv (comp nil? :reason) rows)
        excluded (filterv (comp some? :reason) rows)
        problems (mapv (fn [{:keys [problem/id path]}]
                         {:problem/id id :repository repository
                          :base-branch branch :revision revision :path path
                          :blob (git-out repository "rev-parse" (str revision ":" path))
                          :classification :non-excluded})
                       selected)]
    (cond
      (or (str/blank? revision) (str/blank? branch))
      {:ok false :error/code :open-problem-corpus-git-unavailable}

      (some (comp str/blank? :blob) problems)
      {:ok false :error/code :open-problem-blob-unavailable
       :problem-ids (mapv :problem/id (filter (comp str/blank? :blob) problems))}

      :else
      {:ok true :repository repository :branch branch :revision revision
       :selection/rule {:open :positive-status-sorry-count
                        :non-topology :problem-id-not-prefixed-t
                        :excluded-classifications excluded-classifications}
       :problems problems
       :excluded (mapv #(select-keys % [:problem/id :classification
                                        :sorry-count :reason]) excluded)}))))
