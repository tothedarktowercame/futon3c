(ns futon3c.apm.library-lane-runner
  "Fail-closed orchestration of exactly one live library-lane problem."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.bank-driver :as bank-driver]
            [futon3c.apm.library-lane :as lane]
            [futon3c.apm.live-proof-phases :as live-proof-phases]))

(def library-card
  {:path "holes/labs/M-apm-demonstration/role-cards/codex-solver-library-v1.md"
   :blob "a03d58e9fb261fb78b1ee90d9e497d395e4f1dd2"})

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
  [{:keys [corpus-root problem-id run-fn]
    :or {run-fn (fn [dir argv]
                  (apply shell/sh
                         (concat argv [:dir (str dir)])))}}]
  (let [path (str "problems/" problem-id "/lean/Main.lean")
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

(defn run-one!
  "Run one current :library problem. Effects are injectable for fixture tests.

  phase-inputs-fn must return the complete input expected by run-live! for
  each phase; bank-request-fn must return execute!'s request. Missing adapters
  fail closed instead of guessing workspace, branch, or receipt authority."
  [{:keys [corpus-root contract seat phase-inputs-fn bank-request-fn
           target-fn phase-run-fn bank-fn]
    :or {target-fn elaborate-targets
         phase-run-fn live-proof-phases/run-live!
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
                               :receipts receipts})
                      result (if (:ok inputs)
                               (phase-run-fn (dissoc inputs :ok)) inputs)]
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
