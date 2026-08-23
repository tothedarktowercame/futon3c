(ns futon3c.apm.library-lane-runner
  "Fail-closed orchestration of exactly one live library-lane problem."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.bank-driver :as bank-driver]
            [futon3c.apm.library-lane :as lane]
            [futon3c.apm.library-lane-phases :as lane-phases]))

(def library-card
  {:path "holes/labs/M-apm-demonstration/role-cards/codex-solver-library-v1.md"
   :blob "a03d58e9fb261fb78b1ee90d9e497d395e4f1dd2"})

(def solver-restrategize-card
  {:path "holes/labs/M-apm-demonstration/role-cards/solver-restrategize-v1.md"
   :blob "27c82729df0c575bc42cc13bd5ac93790a8e1524"})

(defn- blocked [problem-id seam finding]
  {:ok false :ruling :blocked :problem-id problem-id
   :seam seam :finding finding})

(defn- declaration-before [lines line-number]
  (some->> (subvec lines 0 (min line-number (count lines)))
           reverse
           (some #(second (re-find
                           #"^\s*(?:noncomputable\s+)?(?:theorem|lemma|def)\s+([^\s:(]+)"
                           %)))))

(defn elaborate-targets
  "Run Lean and map its sorry-warning source locations back to declarations.
  Named bridge holes are co-targets; the returned vector is their deterministic
  union with all other elaborated sorry declarations."
  [{:keys [corpus-root problem-id run-fn]}]
  ;; `or`, not a destructuring `:or` default: callers thread :run-fn through
  ;; unconditionally, so the key is PRESENT and nil rather than absent, and an
  ;; :or default never fires. That produced a bare NPE inside run-one!'s
  ;; catch-all -- reported as :blocked/:exception with the cause discarded.
  (let [run-fn (or run-fn
                   (fn [dir argv]
                     (apply shell/sh (concat argv [:dir (str dir)]))))
        path (str "problems/" problem-id "/lean/Main.lean")
        file (io/file corpus-root path)]
    (if-not (.isFile file)
      {:ok false :finding :lean-source-missing :path path}
      (let [result (run-fn corpus-root ["lake" "env" "lean" path])
            output (str (:out result) "\n" (:err result))
            warning-lines (map #(Long/parseLong %)
                               (map second
                                    (re-seq (re-pattern
                                             (str (java.util.regex.Pattern/quote path)
                                                  ":([0-9]+):[0-9]+: warning: declaration uses `sorry`"))
                                            output)))
            lines (vec (str/split-lines (slurp file)))
            targets (->> warning-lines
                         (keep #(declaration-before lines %))
                         distinct sort vec)]
        (cond
          (not (zero? (:exit result)))
          {:ok false :finding :target-elaboration-failed
           :exit (:exit result) :stdout (:out result) :stderr (:err result)}
          (empty? targets)
          {:ok false :finding :no-remaining-sorries :path path}
          :else {:ok true :targets targets :warning-count (count warning-lines)})))))

(def default-phase-timeouts
  "Wall-clock budget per phase, in ms. Solve is a multi-round siege and needs
  hours; the read-only phases do not."
  {:preflight 900000 :solve 14400000 :verify 1800000})

(defn drive-to-certified
  "Call PHASE-RUN-FN until the phase is CERTIFIED, not merely dispatched.

  live-job-driver/drive! is a durable STEP machine: it returns
  {:ok true :status :awaiting-terminal} once a job is announced and activated,
  and only yields {:status :certified :certificate ...} after the job reaches a
  terminal state. Treating the first :ok as phase completion marches straight
  past a running solver and then fails at the next phase with a missing
  receipt, which is exactly what happened on the first live run."
  [phase-run-fn inputs {:keys [poll-ms timeout-ms sleep-fn now-fn]
                        :or {poll-ms 15000
                             sleep-fn #(Thread/sleep ^long %)
                             now-fn #(System/currentTimeMillis)}}]
  (let [deadline (+ (now-fn) (or timeout-ms 1800000))]
    (loop []
      (let [result (phase-run-fn inputs)]
        (cond
          (not (:ok result)) result
          (or (= :certified (:status result)) (:certificate result)) result
          (>= (now-fn) deadline)
          {:ok false :error/code :phase-awaiting-terminal-timeout
           :status (:status result) :timeout-ms timeout-ms}
          :else (do (sleep-fn poll-ms) (recur)))))))

(defn step-one!
  "Advance one library problem to its next durable boundary and return.

   Unlike `run-one!`, this function never polls or sleeps. It replays already
   certified phase receipts, performs at most one non-certified phase driver
   call, and banks only after all three phase receipts are certified. This is
   the tick boundary used by a JVM-owned durable coordinator."
  [{:keys [corpus-root contract seat phase-inputs-fn bank-request-fn
           target-fn phase-run-fn bank-fn phase-limit]
    :or {target-fn elaborate-targets
         phase-run-fn lane-phases/run-live!
         bank-fn bank-driver/execute!}
    :as options}]
  (let [available (lane/queue corpus-root :library)
        requested (:problem-id options)
        problem-id (if requested
                     (when (some #{requested} available) requested)
                     (first available))]
    (cond
      (nil? problem-id) (blocked requested :queue
                                 (if requested :library-problem-not-current
                                     :library-queue-empty))
      (nil? phase-inputs-fn) (blocked problem-id :configuration
                                      :phase-inputs-fn-missing)
      (nil? bank-request-fn) (blocked problem-id :configuration
                                     :bank-request-fn-missing)
      :else
      (try
        (let [target (target-fn {:corpus-root corpus-root
                                 :problem-id problem-id
                                 :run-fn (:run-fn options)})]
          (if-not (:ok target)
            (blocked problem-id :target-elaboration target)
            (loop [phases [:preflight :solve :verify] receipts {}]
              (if-let [kind (first phases)]
                (let [inputs (phase-inputs-fn
                              {:kind kind :problem-id problem-id
                               :targets (:targets target) :contract contract
                               :seat seat :role-card library-card
                               :checkpoint-role-card solver-restrategize-card
                               :receipts receipts})
                      result (if (:ok inputs)
                               (phase-run-fn (dissoc inputs :ok))
                               inputs)]
                  (cond
                    (not (:ok result))
                    (assoc (blocked problem-id kind result)
                           :keying-targets (:targets target))

                    (or (= :certified (:status result))
                        (:certificate result))
                    (if (= phase-limit kind)
                      {:ok true :ruling :phase-certified :phase kind
                       :problem-id problem-id :keying-targets (:targets target)
                       :receipt (:certificate result)}
                      (recur (next phases)
                             (assoc receipts kind (:certificate result))))

                    :else
                    {:ok true :ruling :awaiting :phase kind
                     :status (:status result) :problem-id problem-id
                     :keying-targets (:targets target)}))
                (let [request (bank-request-fn
                               {:problem-id problem-id :targets (:targets target)
                                :contract contract :seat seat :receipts receipts})
                      bank-result (if (:ok request)
                                    (bank-fn (dissoc request :ok)) request)]
                  (if (:ok bank-result)
                    (assoc bank-result :problem-id problem-id
                           :keying-targets (:targets target)
                           :ruling (get-in bank-result
                                           [:receipt :receipt/ruling]))
                    (assoc (blocked problem-id :bank bank-result)
                           :keying-targets (:targets target))))))))
        (catch Throwable t
          (blocked problem-id :exception
                   {:class (.getName (class t)) :message (.getMessage t)}))))))

(defn run-one!
  "Run one current :library problem. Effects are injectable for fixture tests.

  phase-inputs-fn must return the complete input expected by run-live! for
  each phase; bank-request-fn must return execute!'s request. Missing adapters
  fail closed instead of guessing workspace, branch, or receipt authority."
  [{:keys [corpus-root contract seat phase-inputs-fn bank-request-fn
           target-fn phase-run-fn bank-fn]
    :or {target-fn elaborate-targets
         phase-run-fn lane-phases/run-live!
         bank-fn bank-driver/execute!}
    :as options}]
  (let [available (lane/queue corpus-root :library)
        requested (:problem-id options)
        problem-id (if requested
                     (when (some #{requested} available) requested)
                     (first available))]
    (cond
      (nil? problem-id) (blocked requested :queue
                                 (if requested
                                   :library-problem-not-current
                                   :library-queue-empty))
      (nil? phase-inputs-fn) (blocked problem-id :configuration
                                      :phase-inputs-fn-missing)
      (nil? bank-request-fn) (blocked problem-id :configuration
                                     :bank-request-fn-missing)
      :else
      (try
        (let [target (target-fn {:corpus-root corpus-root
                                 :problem-id problem-id
                                 :run-fn (:run-fn options)})]
          (if-not (:ok target)
            (blocked problem-id :target-elaboration target)
            (loop [phases [:preflight :solve :verify] receipts {}]
              (if-let [kind (first phases)]
                (let [inputs (phase-inputs-fn
                              {:kind kind :problem-id problem-id
                               :targets (:targets target) :contract contract
                               :seat seat :role-card library-card
                               :checkpoint-role-card solver-restrategize-card
                               :receipts receipts})
                      result (if (:ok inputs)
                               (drive-to-certified
                                phase-run-fn (dissoc inputs :ok)
                                (assoc (select-keys options [:poll-ms :sleep-fn :now-fn])
                                       :timeout-ms
                                       (or (get (:phase-timeouts options) kind)
                                           (get default-phase-timeouts kind))))
                               inputs)]
                  (if-not (:ok result)
                    (assoc (blocked problem-id kind result)
                           :keying-targets (:targets target))
                    (recur (next phases)
                           (assoc receipts kind (:certificate result)))))
                (let [request (bank-request-fn
                               {:problem-id problem-id :targets (:targets target)
                                :contract contract :seat seat :receipts receipts})
                      bank-result (if (:ok request)
                                    (bank-fn (dissoc request :ok)) request)]
                  (if (:ok bank-result)
                    (assoc bank-result :problem-id problem-id
                           :keying-targets (:targets target)
                           :ruling (get-in bank-result [:receipt :receipt/ruling]))
                    (assoc (blocked problem-id :bank bank-result)
                           :keying-targets (:targets target))))))))
        (catch Throwable t
          (blocked problem-id :exception
                   {:class (.getName (class t)) :message (.getMessage t)}))))))
