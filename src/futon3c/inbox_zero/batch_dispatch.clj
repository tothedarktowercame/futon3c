(ns futon3c.inbox-zero.batch-dispatch
  "Operator-dispatched preparation and explicit compensating execution.
  Authorization and admissibility remain distinct. Atomic callers retain the
  strict refusal; compensating callers carry detector/undo assumptions."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.escalation :as escalation]
            [futon3.inbox-zero.gates :as gates]
            [futon3.inbox-zero.promote-push :as push]
            [futon3c.agents.inbox-zero-board :as board]
            [futon3c.inbox-zero.compensating-commit :as compensating]
            [futon3c.agents.inbox-zero-board-live :as live]
            [futon3c.inbox-zero.board-consumer :as consumer]
            [futon3c.inbox-zero.turn-promotion :as turn])
  (:import [java.nio.channels FileChannel]
           [java.nio.file StandardOpenOption]
           [java.time Instant]
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
  [{:keys [dispatch state-path ledger-path record! load-fn refresh-fn now-fn safety]
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
                  guard (if (= :compensating safety)
                          {:held/reason :package-review-required}
                          (consumer/atomic-commit! nil {:dispatch dispatch}))
                  ;; Preparation never executes. execute-batch! selects the
                  ;; separately reviewed compensating mode explicitly.
                  blocker (or (:held/reason guard) :batch-execution-not-implemented)
                  held (mapv (fn [p]
                               (assoc p :record/type :inbox-zero/refusal
                                      :refusal/reason (if (busy (.getName (io/file (:repo/root p))))
                                                        :in-flight blocker)))
                             (packages refreshed dispatch now))
                  result {:record/type :inbox-zero/batch-result :dispatch dispatch :ruling ruling
                          :pressure/before pressure :pressure/after-scan (live/batch-pressure (:records refreshed))
                          :board/run run :safety (or safety :atomic)
                          :committed [] :pushed [] :held held
                          :push/requested? true :push/status :blocked
                          :blocker (refusal blocker (:blocker guard))}]
              (record! (:blocker result))
              (record! result)
              result)))))))


(defn- execute-batch-unlocked!
  "Explicit compensating mode only. Each package needs an exact reviewed path
  set, Git blob IDs, meaningful message, and passing gates. Operator review is
  recorded as such; it never mints an author claim. Atomic-mode callers keep
  the original refusal. Push uses the standing promotion push boundary."
  [{:keys [dispatch state-path ledger-path reviews safety]
    :or {state-path turn/default-state-path} :as options}]
  (let [record! #(turn/append-escalation! ledger-path %)
        prepared (prepare-batch! options)]
    (if (or (not= :compensating safety) (not= :inbox-zero/batch-result (:record/type prepared)))
      prepared
      (let [outcomes
            (loop [remaining (:held prepared) results [] stopped? false]
              (if-let [package (first remaining)]
                (let [root (:repo/root package)
                      repo (.getName (io/file root))
                      review (get reviews [root (:paths package)])
                      result
                      (cond
                        stopped? {:verdict :held :held/reason :batch-stopped-after-compensation}
                        (= :in-flight (:refusal/reason package))
                        {:verdict :held :held/reason :in-flight}
                        (nil? review) {:verdict :held :held/reason (or (:review/reason package) :package-review-required)}
                        :else
                        (try
                          (when-not (and (seq (:gates review)) (not (str/blank? (:message review)))
                                         (not (str/blank? (:rationale review))))
                            (throw (ex-info "Review requires message, rationale and gates"
                                            {:reason :package-review-required})))
                          (let [fresh (:state (consumer/refresh-repo (consumer/read-state state-path) root (Instant/now)))
                                now (Instant/now)
                                rows (live/sweep-from-records (:records fresh) now)
                                inputs (board/observation-packet (filterv #(= repo (:repo %)) rows)
                                                                (live/in-flight-from-records (:records fresh) now) false)
                                run (assoc (board/run inputs (constantly nil)) :inputs inputs)
                                _ (when-not (and (consumer/verified-proposal? run)
                                                 (some #(= [:commit {:repo repo :mode :commit}] %)
                                                       (mapcat :effects (:trace run))))
                                    (throw (ex-info "Board did not propose this repository"
                                                    {:reason (if (some #{repo} (:in-flight-repos inputs))
                                                               :in-flight :board-did-not-propose)})))
                                observations (projection/current-observations fresh)
                                plan {:record/type :inbox-zero/promotion-plan :verdict :proposed
                                      :repo/id repo :worktree/id (:worktree/id package)
                                      :attribution/basis :operator-dispatched-review
                                      :dispatch/id (:id dispatch)
                                      :include (mapv (fn [path]
                                                       {:path path :git/status (:git/status (get observations [(:worktree/id package) path]))
                                                        :size (.length (io/file root path))}) (:paths package))}
                                screened (escalation/screen-sensitivity plan escalation/default-rules)
                                _ (when (= :held (:verdict screened))
                                    (throw (ex-info "Sensitive package held" {:reason (:held/reason screened)})))
                                validation (gates/run-gates root (gates/validate-gates! (:gates review)) (:paths package))
                                _ (when-not (:passed? validation)
                                    (throw (ex-info "Package gates failed" {:reason :gate-failed :validation validation})))
                                _ (record! {:record/type :inbox-zero/package-review :dispatch dispatch
                                            :package package :review review :validation validation :board/run run
                                            :assumptions compensating/assumptions})
                                message (str (:message review)
                                             "\n\nInbox-zero-dispatch: " (:id dispatch)
                                             "\nInbox-zero-board-digest: " (get-in run [:certificate :board/digest])
                                             "\nInbox-zero-inputs-digest: " (get-in run [:certificate :inputs/digest])
                                             "\nInbox-zero-verbs-digest: " (:verbs/digest run))
                                watcher-read! (fn [] (:state (consumer/refresh-repo (consumer/read-state state-path)
                                                                                 root (Instant/now))))
                                idle-check! (fn []
                                              (let [s (watcher-read!)]
                                                (when ((live/in-flight-from-records (:records s) (Instant/now)) repo)
                                                  (throw (ex-info "Repository became active" {:reason :in-flight})))
                                                s))
                                executed (compensating/execute! plan {:repo-root root :message message
                                                                     :reviewed-blobs (:blobs review) :idle-check! idle-check!
                                                                     :watcher-read! watcher-read!
                                                                     :record! record! :certificate (:certificate run)})]
                            (if (and (= :committed (:verdict executed)) (:compensating/verified? executed))
                              (assoc executed :push
                                     (if (= (:commit/sha executed) (compensating/git! root "rev-parse" "HEAD"))
                                       (try (push/push-promoted! {:repo-root root})
                                            (catch Exception e {:verdict :escalate :escalate/reason :push-error
                                                                :error (.getMessage e)}))
                                       {:verdict :escalate :escalate/reason :push-head-moved}))
                              executed))
                          (catch Exception e {:verdict :held :held/reason (or (:reason (ex-data e)) :batch-execution-error)
                                              :details {:message (.getMessage e) :data (ex-data e)}})))
                      result (assoc result :package package)]
                  (record! (assoc result :refusal/reason (:held/reason result)
                                  :record/type (if (= :committed (:verdict result))
                                                       :inbox-zero/batch-commit :inbox-zero/refusal)))
                  (recur (next remaining) (conj results result)
                         (or stopped? (contains? #{:compensated :compensation-failed} (:verdict result)))))
                results))
            state (consumer/read-state state-path)
            refreshed (reduce (fn [s root] (:state (consumer/refresh-repo s root (Instant/now)))) state
                              (keys (get-in prepared [:pressure/after-scan :by-repo])))
            after (consumer/proposal-run refreshed (Instant/now))
            result {:record/type :inbox-zero/compensating-batch-result :dispatch dispatch
                    :assumptions compensating/assumptions :outcomes outcomes
                    :committed (filterv #(= :committed (:verdict %)) outcomes)
                    :pushed (filterv #(= :pushed (get-in % [:push :verdict])) outcomes)
                    :held (filterv #(not= :committed (:verdict %)) outcomes)
                    :flags/before (count (remove :clean? (get-in prepared [:board/run :inputs :sweep])))
                    :flags/after (count (remove :clean? (get-in after [:inputs :sweep])))
                    :before (:board/run prepared) :after after}]
        (record! result)
        result))))


(defn execute-batch!
  "Serialize consumer mutations with the standing consumer lock. This lock
  excludes cooperating consumers only; external edits require compensation."
  [{:keys [state-path ledger-path] :or {state-path turn/default-state-path} :as options}]
  (with-open [channel (FileChannel/open (.toPath (io/file (str state-path ".consumer.lock")))
                                       (into-array StandardOpenOption
                                                   [StandardOpenOption/CREATE StandardOpenOption/WRITE]))]
    (with-open [lock (.tryLock channel)]
      (if lock
        (execute-batch-unlocked! options)
        (let [r {:record/type :inbox-zero/refusal :refusal/reason :consumer-busy}]
          (turn/append-escalation! ledger-path r)
          r)))))
