(ns futon3c.inbox-zero.compensating-commit
  "Opt-in compensating safety, distinct from atomic exclusion. Forward commits
  use the standing promotion executor. Compensation writes an inverse-tree
  commit using a compare-and-swap ref update, preserving worktree and index.
  Detector failure/timeout is inconclusive, never a clean postcondition."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3.inbox-zero.promote-exec :as executor]
            [futon3.inbox-zero.projection :as projection])
  (:import [java.lang ProcessBuilder$Redirect]
           [java.util UUID]
           [java.util.concurrent TimeUnit TimeoutException]
           [java.nio.file Files]))

(def detector-response-budget-ms 5000)
(defn- monotonic-ms [] (/ (System/nanoTime) 1000000.0))
(def assumptions
  {:safety/form :compensating
   :detection/response-budget-ms detector-response-budget-ms
   :detection/deadline-policy :compensate-on-timeout-or-inconclusive
   :detection/window [:armed-before-final-idle-check :post-commit-kernel-queue-drain]
   :detection/sources [:standing-watcher-history :fresh-watcher-git-scan :inotify :git-blobs-and-head]
   :detection/scope :local-ext4-filesystem-api-events
   :detection/exclusions [:mmap-only-transient-writes :remote-filesystems :mount-replacement
                          :writes-via-external-hardlinks]
   :detection/not-a-bound :background-watcher-poll-interval
   :runtime/assumptions [:process-survives :os-schedules-detector :git-commands-terminate
                         :compensation-cas-succeeds :no-concurrent-git-index-writers]
   :hard-real-time-guarantee? false
   :survival/theorem "mathlib4@cf88292da5:T2-prime-conditional"
   :survival/conditional-bound :detection-plus-compensation
   :survival/proved-production-bound-ms nil
   :transient-cost :local-commit-visible-until-verification-or-compensation
   :push-policy :only-after-clean-postcheck})

(defn git! [root & args]
  (let [{:keys [exit out err]} (apply shell/sh (concat ["git" "-C" root] args))]
    (when-not (zero? exit)
      (throw (ex-info "Compensating Git boundary failed"
                      {:reason :git-failed :args args :exit exit :stderr err})))
    (str/trim out)))

(defn blob-id [root path]
  (when (or (.isAbsolute (io/file path))
            (some #{".."} (str/split path #"/"))
            (Files/isSymbolicLink (.toPath (io/file root path)))
            (not (.isFile (io/file root path))))
    (throw (ex-info "Review requires a regular repository-relative file"
                    {:reason :unsupported-path-semantics :path path})))
  (git! root "hash-object" (str "--path=" path) "--" path))

(defn- response! [{:keys [reader process]}]
  (let [reading (future (.readLine ^java.io.BufferedReader reader))]
    (try
      (let [line (.get ^java.util.concurrent.Future reading
                       detector-response-budget-ms TimeUnit/MILLISECONDS)
            response (when line (json/parse-string line true))]
        (when (or (nil? response) (:error response))
          (throw (ex-info "Detector unavailable" {:reason :detector-unavailable :response response})))
        response)
      (catch TimeoutException e
        (.destroyForcibly ^Process process)
        (future-cancel reading)
        (throw (ex-info "Detector response deadline exceeded"
                        {:reason :detector-timeout :deadline-ms detector-response-budget-ms} e))))))

(defn close-detector! [{:keys [process]}]
  (.destroyForcibly ^Process process))

(defn start-detector! [root]
  (let [script (str (System/getProperty "user.dir") "/scripts/inbox_zero_detector.py")
        builder (ProcessBuilder. ^java.util.List ["python3" "-u" script root])
        _ (.redirectError builder ProcessBuilder$Redirect/INHERIT)
        process (.start builder)
        detector {:process process :reader (io/reader (.getInputStream process))
                  :writer (io/writer (.getOutputStream process))}]
    (try
      (let [ready (response! detector)]
        (when-not (and (:ready ready) (= detector-response-budget-ms (:response_budget_ms ready)))
          (throw (ex-info "Detector did not arm" {:reason :detector-unavailable :response ready})))
        (assoc detector :ready ready))
      (catch Exception e (close-detector! detector) (throw e)))))

(defn drain! [{:keys [writer] :as detector}]
  (.write ^java.io.Writer writer "drain\n")
  (.flush ^java.io.Writer writer)
  (response! detector))

(defn clean-drain? [d]
  (and (true? (:complete d)) (false? (:invalid d)) (zero? (:event_count d))))

(defn compensate!
  "Revert exactly SHA's tree change, only if SHA remains the branch tip and its
  parent is BASE. Keep every working-tree and index byte: concurrent edits are
  not checked out, reset, stashed or discarded. The former commit's contents
  consequently appear as staged work after compensation; report that explicitly."
  [root ref base sha attempt]
  (when-not (= base (git! root "rev-parse" (str sha "^")))
    (throw (ex-info "Optimistic commit has an unexpected parent" {:reason :compensation-parent-mismatch})))
  (when-not (= sha (git! root "rev-parse" ref))
    (throw (ex-info "Branch moved before compensation" {:reason :compensation-head-moved})))
  (let [tree (git! root "rev-parse" (str base "^{tree}"))
        message (str "Revert raced inbox-zero batch " attempt "\n\n"
                     "Reverts: " sha "\nInbox-zero-attempt: " attempt "\n"
                     "Compensation preserves worktree and index; no checkout or reset.\n")
        {:keys [exit out err]} (shell/sh "git" "-C" root "commit-tree" tree "-p" sha :in message)]
    (when-not (zero? exit)
      (throw (ex-info "Inverse commit creation failed" {:reason :compensation-commit-failed :stderr err})))
    (let [revert-sha (str/trim out)]
      (git! root "update-ref" "-m" (str "inbox-zero compensation " attempt) ref revert-sha sha)
      {:reverted/sha sha :revert/sha revert-sha :revert/tree tree
       :worktree/preserved? true :index/preserved? true
       :index/note :reverted-content-remains-staged})))

(defn- execute-existing! [plan options base attempt]
  (try
    (executor/execute-plan! plan options)
    (catch Exception e
      ;; Git may have advanced HEAD before its wrapper failed. Locate only a
      ;; tip carrying this attempt's unique trailer; never compensate another
      ;; writer's commit on the strength of timing alone.
      (try
        (let [root (:repo-root options)
              head (git! root "rev-parse" "HEAD")
              own? (str/includes? (git! root "show" "-s" "--format=%B" head)
                                  (str "Inbox-zero-attempt: " attempt))]
          (cond
            (and (not= head base) own?)
            {:verdict :committed :commit/sha head :execution/error (.getMessage e)}
            (= head base)
            {:verdict :held :held/reason :execution-failed :error (.getMessage e)}
            :else {:verdict :compensation-failed :held/reason :execution-outcome-unknown
                   :base/sha base :observed/head head :error (.getMessage e)}))
        (catch Exception recovery-error
          {:verdict :compensation-failed :held/reason :execution-outcome-unknown
           :error (.getMessage e) :recovery/error (.getMessage recovery-error)})))))

(defn watcher-window
  "Inspect every new observation, including intermediate edits later restored.
  Only unchanged file tuples and the planned commit's clean transition are
  expected. History loss is inconclusive; a latest-only view is insufficient."
  [before after root paths base sha]
  (when-not (and (map? (:records before)) (map? (:records after)))
    (throw (ex-info "Watcher snapshots required" {:reason :watcher-history-unavailable})))
  (let [root (.getCanonicalPath (io/file root))
        observations (fn [s] (into {} (filter (fn [[_ r]]
                                              (and (= :inbox-zero/file-observation (:record/type r))
                                                   (= root (:repo/root r))))) (:records s)))
        old (observations before) new (observations after)
        current (projection/current-observations {:records old})
        added (remove (fn [[id _]] (contains? old id)) new)
        missing (remove #(contains? new %) (keys old))
        changed (keep (fn [[id r]] (when (and (contains? new id) (not= r (get new id))) id)) old)
        complete? (and (empty? missing) (empty? changed))
        unexpected (keep (fn [[_ r]]
                           (let [previous (get current [(:worktree/id r) (:path r)])
                                 same-content? (and previous
                                                    (= (:content/hash previous) (:content/hash r))
                                                    (= (:index/hash previous) (:index/hash r)))
                                 expected? (and same-content? (contains? #{base sha} (:head/sha r))
                                                (or (= (:git/status previous) (:git/status r))
                                                    (and (contains? paths (:path r))
                                                         (= sha (:head/sha r))
                                                         (= :clean (:git/status r)))))]
                             (when-not expected? r))) added)]
    {:complete? complete? :missing-observation-ids (vec missing)
     :changed-observation-ids (vec changed)
     :new-observation-count (count added)
     :unexpected-observations (vec unexpected)
     :paths (vec (distinct (map :path unexpected)))
     :clean? (and complete? (empty? unexpected))}))

(defn execute!
  "An operator-approved plan with reviewed blob IDs, completed gates, a durable
  record sink and a fresh idle-check callback. Strict callers use atomic-commit!.
  Hooks/tests are not disabled. A postcheck finding is compensated before return;
  compensation failure returns a distinct fatal outcome and must stop the batch."
  [plan {:keys [repo-root message reviewed-blobs idle-check! watcher-read! record!]
         :as options}]
  (let [attempt (str (UUID/randomUUID))
        certificate (:certificate options)
        record! (fn [r] (record! (assoc r :attempt/id attempt :certificate certificate
                                        :assumptions assumptions)))
        held (fn [reason details] {:verdict :held :held/reason reason :refusal/reason reason :details details})
        watcher-before (atom nil)
        started (System/currentTimeMillis)
        prepared (try
                   (when-not (fn? watcher-read!)
                     (throw (ex-info "Standing watcher postcheck required" {:reason :watcher-history-unavailable})))
                   (when-not (and (= (set (map :path (:include plan))) (set (keys reviewed-blobs)))
                                  (seq reviewed-blobs))
                     (throw (ex-info "Exact reviewed blob IDs required" {:reason :review-required})))
                   (let [ref (git! repo-root "symbolic-ref" "HEAD")
                         base (git! repo-root "rev-parse" "HEAD")
                         detector (start-detector! repo-root)]
                     {:ref ref :base base :detector detector})
                   (catch Exception e {:error (held (or (:reason (ex-data e)) :preflight-failed)
                                                    {:message (.getMessage e) :data (ex-data e)})}))]
    (if-let [error (:error prepared)]
      (do (record! (assoc error :record/type :inbox-zero/refusal)) error)
      (let [{:keys [detector base ref]} prepared]
        (try
          (let [pre (try
                      (reset! watcher-before (idle-check!))
                      (when-not (map? (:records @watcher-before))
                        (throw (ex-info "Idle check must return watcher snapshot" {:reason :watcher-history-unavailable})))
                      (doseq [[path expected] reviewed-blobs]
                        (when-not (= expected (blob-id repo-root path))
                          (throw (ex-info "Reviewed content changed" {:reason :review-stale :path path}))))
                      (when-not (= base (git! repo-root "rev-parse" "HEAD"))
                        (throw (ex-info "HEAD moved during preparation" {:reason :head-moved})))
                      (let [d (drain! detector)]
                        (when-not (clean-drain? d)
                          (throw (ex-info "Activity during final check" {:reason :in-flight :detector d}))))
                      nil
                      (catch Exception e (held (or (:reason (ex-data e)) :preflight-failed) (ex-data e))))]
            (if pre
              (do (record! (assoc pre :record/type :inbox-zero/refusal)) pre)
              (do
                (record! {:record/type :inbox-zero/compensating-intent :base/sha base
                          :plan plan :reviewed-blobs reviewed-blobs :started-ms started})
                (let [commit-started (monotonic-ms)
                      executed (execute-existing!
                                plan {:repo-root repo-root :gates []
                                      :message (str message "\nInbox-zero-attempt: " attempt
                                                    "\nInbox-zero-safety: compensating"
                                                    "\nInbox-zero-detector-budget-ms: " detector-response-budget-ms)} base attempt)
                      committed-at (System/currentTimeMillis)
                      returned-at (monotonic-ms)]
                  (if-not (= :committed (:verdict executed))
                    (do (record! executed) executed)
                    (let [sha (:commit/sha executed)
                          post (try
                                 (let [history (watcher-window @watcher-before (watcher-read!) repo-root
                                                               (set (keys reviewed-blobs)) base sha)
                                       d (drain! detector)
                                       mismatches (vec (for [[path expected] reviewed-blobs
                                                              :when (or (not= expected (git! repo-root "rev-parse" (str sha ":" path)))
                                                                        (not= expected (blob-id repo-root path)))] path))
                                       head-valid? (and (= sha (git! repo-root "rev-parse" "HEAD"))
                                                        (= base (git! repo-root "rev-parse" (str sha "^"))))
                                       final-drain (drain! detector)
                                       elapsed (- (monotonic-ms) returned-at)]
                                   {:watcher history :detector d :final-drain final-drain
                                    :paths (vec (distinct (concat (:paths history) mismatches (keep :path (:events d))
                                                                 (keep :path (:events final-drain)))))
                                    :elapsed-ms elapsed
                                    :deadline-met? (<= elapsed detector-response-budget-ms)
                                    :inconclusive? (or (not (:complete? history)) (:execution/error executed) (> elapsed detector-response-budget-ms)
                                                       (not (:complete d)) (:invalid d)
                                                       (not (:complete final-drain)) (:invalid final-drain))
                                    :clean? (and (:clean? history) (nil? (:execution/error executed)) (clean-drain? d) (clean-drain? final-drain)
                                                 (empty? mismatches) head-valid?
                                                 (<= elapsed detector-response-budget-ms))})
                                 (catch Exception e {:clean? false :inconclusive? true
                                                     :error (.getMessage e) :data (ex-data e)}))]
                      (if (:clean? post)
                        (let [r (assoc executed :postcheck post :assumptions assumptions
                                               :attempt/id attempt :compensating/verified? true
                                               :timing/commit-call-ms (- returned-at commit-started))]
                          (record! (assoc r :record/type :inbox-zero/commit-witness)) r)
                        (let [finding (if (:inconclusive? post) :detector-inconclusive :raced-with-edit)
                              detection-finished (monotonic-ms)
                              compensation (try
                                             (compensate! repo-root ref base sha attempt)
                                             (catch Exception e {:error (.getMessage e) :data (ex-data e)}))
                              compensation-finished (monotonic-ms)
                              r {:timing/commit-call-ms (- returned-at commit-started)
                                 :timing/detection-ms (- detection-finished returned-at)
                                 :timing/detection-deadline-met? (<= (- detection-finished returned-at) detector-response-budget-ms)
                                 :timing/compensation-ms (- compensation-finished detection-finished)
                                 :timing/after-return-survival-ms (- compensation-finished returned-at)
                                 :verdict (if (:error compensation) :compensation-failed :compensated)
                                 :held/reason (if (:error compensation) :compensation-failed finding)
                                 :finding/type finding :commit/sha sha :postcheck post
                                 :compensation compensation :started-ms started
                                 :commit-returned-ms committed-at :finished-ms (System/currentTimeMillis)}]
                          (record! (assoc r :record/type :inbox-zero/finding)) r))))))))
          (finally (close-detector! detector)))))))
