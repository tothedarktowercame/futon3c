(ns futon3c.inbox-zero.batch-dispatch
  "Operator-dispatched batch preparation. Authorization is recorded separately
  from admissibility. Stop at the existing atomicity blocker; never weaken it.
  No commit/push execution is enabled while that structural blocker exists."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.inbox-zero.projection :as projection]
            [futon3c.agents.inbox-zero-board-live :as live]
            [futon3c.inbox-zero.board-consumer :as consumer]
            [futon3c.inbox-zero.turn-promotion :as turn])
  (:import [java.time Instant]
           [java.util Date]))

(def ruling "futon2@6ae963d485ecb8565cbb86a54f70ec3ef53b92ae:holes/labs/wm-contract/NOTE-inbox-zero-aif.md")

(defn dispatch? [cue]
  ;; This boundary records an operator message supplied by the trusted caller;
  ;; it does not authenticate arbitrary network requests based on these strings.
  (and (= :package-commit-and-push (:action cue))
       (every? #(and (string? %) (not (str/blank? %)))
               (map cue [:id :operator :message :source]))))

(defn packages [state cue now]
  (let [sets (:dirty-sets (projection/project-dirty-sets state (Date/from now)))
        owners (into {} (for [s sets m (:members s)]
                          [[(:worktree/id m) (:path m)] (:seat/id s)]))
        classify (fn [row]
                   (let [owner (get owners [(:worktree/id row) (:path row)])
                         generated? (boolean (re-find #"(^|/)(__pycache__/|[^/]+\.(pyc|class|lock|log)$)"
                                                      (:path row)))]
                     [(cond owner :attributed generated? :janitorial :else :content-review)
                      owner (or (.getParent (io/file (:path row))) ".")]))]
    (->> (live/dirty-paths (:records state))
         (remove #(live/ignored-by-design? (:path %)))
         (group-by (fn [row] [(:repo/root row) (:worktree/id row) (classify row)]))
         (map (fn [[[root worktree [kind owner concern]] rows]]
                {:repo/root root :worktree/id worktree :concern concern :kind kind
                 :seat/id owner :dispatch/id (:id cue)
                 :paths (mapv :path (sort-by :path rows))
                 :message-draft (str (.getName (io/file root)) ": "
                                     (case kind :attributed "checkpoint " :janitorial "review generated files in "
                                           "review unattributed work in ") concern)
                 :review/reason (case kind
                                  :janitorial :generated-artifacts-need-disposition
                                  :content-review :unattributed-content-needs-review
                                  nil)}))
         (sort-by (juxt :repo/root :worktree/id :concern :kind :seat/id))
         vec)))

(defn prepare-batch!
  "Record the dispatch and current packages, then stop at the existing guard.
  All packages are held: janitorial classification is not an authorship claim
  or permission to commit generated files. Push intent is explicit, never a
  fabricated successful push. Read-only collaborators are injectable for tests.
  A fresh Git scan supplies activity the background watcher may not yet know."
  [{:keys [dispatch state-path ledger-path record! load-fn refresh-fn now-fn]
    :or {state-path turn/default-state-path load-fn consumer/read-state
         refresh-fn consumer/refresh-repo now-fn #(Instant/now)}}]
  (let [record! (or record! #(turn/append-escalation! ledger-path %))
        refusal (fn [reason details]
                  {:record/type :inbox-zero/refusal :refusal/reason reason
                   :dispatch dispatch :ruling ruling :details details})]
    (if-not (dispatch? dispatch)
      (let [r (refusal :operator-dispatch-required {})] (record! r) r)
      (do
        (record! {:record/type :inbox-zero/batch-dispatch :dispatch dispatch :ruling ruling
                  :push/requested? true :at (str (now-fn))})
        (let [state (load-fn state-path)
              pressure (live/batch-pressure (:records state))]
          (if-not (:threshold-reached? pressure)
            (let [r (refusal :below-batch-threshold pressure)] (record! r) r)
            (let [roots (sort (keys (:by-repo pressure)))
                  refreshed (reduce (fn [s root] (:state (refresh-fn s root (now-fn)))) state roots)
                  now (now-fn)
                  run (consumer/proposal-run refreshed now)
                  busy (live/in-flight-from-records (:records refreshed) now)
                  guard (consumer/atomic-commit! nil {:dispatch dispatch})
                  ;; No alternate executable path is implemented here. A future
                  ;; guard change requires a separate review of packaging/push.
                  blocker (or (:held/reason guard) :batch-execution-not-implemented)
                  held (mapv (fn [p]
                               (assoc p :record/type :inbox-zero/refusal
                                      :refusal/reason (if (busy (.getName (io/file (:repo/root p))))
                                                        :in-flight blocker)))
                             (packages refreshed dispatch now))
                  result {:record/type :inbox-zero/batch-result :dispatch dispatch :ruling ruling
                          :pressure/before pressure :pressure/after-scan (live/batch-pressure (:records refreshed))
                          :board/run run :committed [] :pushed [] :held held
                          :push/requested? true :push/status :blocked
                          :blocker (refusal blocker (:blocker guard))}]
              (record! (:blocker result))
              (record! result)
              result)))))))
