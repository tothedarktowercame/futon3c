(ns futon3c.apm.library-loop-adapter
  "Canonical production adapter for the files-only Library Loop.

  Configuration is read from RUN-DIR/config.edn. All commands are argv vectors
  (or {:argv vector :cwd absolute-path}); shell strings are never evaluated."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.library-loop-exec :as exec]
            [futon3c.apm.library-loop-runner :as runner])
  (:import (java.io File PushbackReader)
           (java.nio.file Files Path)))

(defn- refuse! [finding data]
  (throw (ex-info (name finding) (assoc data :finding finding))))

(defn- read-edn [path]
  (with-open [reader (PushbackReader. (io/reader path))]
    (edn/read {:eof nil} reader)))

(defn- canonical-file [path]
  (.getCanonicalFile (io/file path)))

(defn- git-path [workspace value]
  (let [file (io/file value)]
    (canonical-file (if (.isAbsolute file) file (io/file workspace value)))))

(defn run-process
  "Runs argv without a shell, in an explicit cwd, returning captured evidence."
  ([cwd argv] (run-process {:cwd cwd :argv argv}))
  ([command]
   (let [{:keys [cwd argv]} command]
     (when-not (and (vector? argv) (seq argv) (every? string? argv))
       (refuse! :invalid-command {:command command}))
     (let [directory (canonical-file cwd)]
       (when-not (.isDirectory directory)
         (refuse! :invalid-command-cwd {:cwd (str directory)}))
       (let [process (-> (ProcessBuilder. ^java.util.List argv)
                         (.directory directory)
                         (.start))
             stdout (future (slurp (.getInputStream process)))
             stderr (future (slurp (.getErrorStream process)))
             exit (.waitFor process)]
         {:exit exit :stdout @stdout :stderr @stderr
          :argv argv :cwd (str directory)})))))

(defn- git! [workspace & argv]
  (let [result (run-process workspace (into ["git"] argv))]
    (when-not (zero? (:exit result))
      (refuse! :git-command-failed {:command (:argv result)
                                    :cwd (:cwd result)
                                    :stderr (:stderr result)}))
    (str/trim-newline (:stdout result))))

(defn validate-workspace
  "Returns the canonical apm-lean worktree directory or fails closed."
  [workspace]
  (let [supplied (io/file workspace)
        file (canonical-file workspace)]
    (when-not (.isAbsolute supplied)
      (refuse! :workspace-not-absolute {:workspace workspace}))
    (when-not (.isDirectory file)
      (refuse! :workspace-not-directory {:workspace (str file)}))
    (let [top (canonical-file (git! file "rev-parse" "--show-toplevel"))]
      (when-not (= file top)
        (refuse! :workspace-not-git-root {:workspace (str file) :git-root (str top)})))
    (when-not (and (or (.isFile (io/file file "lakefile.lean"))
                       (.isFile (io/file file "lakefile.toml")))
                   (.isDirectory (io/file file "ConstructionTargets"))
                   (.isDirectory (io/file file "problems")))
      (refuse! :workspace-not-apm-lean {:workspace (str file)}))
    file))

(defn- substitute [argv values]
  (mapv (fn [arg]
          (reduce-kv (fn [s key value]
                       (str/replace s (str "{" (name key) "}") (str value)))
                     arg values))
        argv))

(defn- config [run-dir]
  (let [path (io/file run-dir "config.edn")]
    (when-not (.isFile path)
      (refuse! :adapter-config-missing {:path (str path)}))
    (let [value (read-edn path)]
      (when-not (= 1 (:schema value))
        (refuse! :adapter-config-invalid {:path (str path) :config value}))
      value)))

(defn- lean-files [workspace]
  (let [root (.toPath ^File workspace)]
    (with-open [paths (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
      (into {}
            (comp (filter #(Files/isRegularFile ^Path %
                                                (make-array java.nio.file.LinkOption 0)))
                  (map (fn [^Path path]
                         [(.toString (.relativize root path))
                          (slurp (.toFile path))]))
                  (filter (fn [[path _]]
                            (and (not (str/starts-with? path ".git/"))
                                 (not (str/starts-with? path ".lake/"))
                                 (or (str/ends-with? path ".lean")
                                     (= path "ConstructionTargets.lean")
                                     (and (str/starts-with? path "ConstructionTargets/")
                                          (str/ends-with? path ".md")))))))
            (iterator-seq (.iterator paths))))))

(defn- audit-evidence [run-dir head-sha]
  (let [path (io/file run-dir "audits" (str head-sha ".edn"))]
    (if-not (.isFile path)
      {}
      (let [evidence (read-edn path)]
        (when-not (and (= 1 (:schema evidence))
                       (= head-sha (:head-sha evidence))
                       (map? (:modules evidence)))
          (refuse! :axiom-audit-evidence-invalid
                   {:path (str path) :evidence evidence}))
        (:modules evidence)))))

(defn- targets-ledger [workspace problem-id]
  (let [path (io/file workspace "problems" problem-id "targets.edn")]
    (if (.isFile path) (read-edn path) [])))

(defn- ancestor? [workspace ancestor descendant]
  (let [result (run-process workspace
                            ["git" "merge-base" "--is-ancestor"
                             ancestor descendant])]
    (case (:exit result)
      0 true
      1 false
      (refuse! :git-command-failed
               {:command (:argv result) :cwd (:cwd result)
                :stderr (:stderr result)}))))

(defn- successful-turn-receipts [run-dir problem-id]
  (let [directory (io/file run-dir "receipts" "turn")
        receipts (if (.isDirectory directory)
                   (->> (.listFiles directory)
                        (filter #(.isFile ^File %))
                        (sort-by #(.getName ^File %))
                        (mapv (fn [^File path]
                                (let [receipt (read-edn path)
                                      id (str/replace (.getName path) #"\.edn$" "")]
                                  (when-not (and (= 1 (:schema receipt))
                                                 (= problem-id (:problem-id receipt))
                                                 (= :turn (:action receipt))
                                                 (= id (:intent-id receipt))
                                                 (pos-int? (:turn receipt))
                                                 (= :ok (get-in receipt [:result :outcome]))
                                                 (re-matches #"[0-9a-f]{40}"
                                                             (get-in receipt [:result :head-sha])))
                                    (refuse! :turn-provenance-receipt-invalid
                                             {:path (str path)}))
                                  receipt))))
                   [])
        by-turn (group-by :turn receipts)]
    (when-let [[turn duplicates]
               (first (filter (fn [[_ values]] (not= 1 (count values))) by-turn))]
      (refuse! :turn-provenance-receipt-ambiguous
               {:turn turn :receipt-ids (mapv :intent-id duplicates)}))
    (into {} (map (fn [[turn values]] [turn (first values)])) by-turn)))

(defn- module-path [module]
  (str (str/replace module "." "/") ".lean"))

(defn- delayed-target-provenance
  [run-dir workspace state targets]
  (let [delayed (filter #(and (pos-int? (:created-turn %))
                              (< (:created-turn %) (:turn state)))
                        targets)]
    (if-not (seq delayed)
      {}
      (let [receipts (successful-turn-receipts run-dir (:problem-id state))]
        (into {}
              (keep
               (fn [{:keys [module created-turn]}]
                 (let [path (module-path module)
                       additions (->> (str/split-lines
                                       (git! workspace "log" "--format=%H"
                                             "--diff-filter=A" "--reverse"
                                             (str (:base-sha state) ".."
                                                  (:head-sha state)) "--" path))
                                      (remove str/blank?) vec)
                       creation (first additions)
                       receipt (get receipts created-turn)
                       turn-head (get-in receipt [:result :head-sha])
                       earlier-heads (->> receipts
                                          (filter (fn [[turn _]] (< turn created-turn)))
                                          (map #(get-in (second %) [:result :head-sha])))]
                   ;; Rows already present at the bank base are outside this
                   ;; candidate's new-target registration set.
                   (when creation
                     (when-not (and receipt
                                    (ancestor? workspace creation turn-head)
                                    (not-any? #(ancestor? workspace creation %)
                                              earlier-heads))
                       (refuse! :target-created-turn-provenance-unproved
                                {:module module :created-turn created-turn
                                 :creation-commit-sha creation
                                 :turn-receipt-id (:intent-id receipt)
                                 :turn-head-sha turn-head}))
                     [module {:schema 1
                              :problem-id (:problem-id state)
                              :module module
                              :created-turn created-turn
                              :turn-receipt-id (:intent-id receipt)
                              :turn-head-sha turn-head
                              :creation-commit-sha creation
                              :first-observed-at-turn? true}])))
               delayed))))))

(defn observe
  "Collects the exact Git and repository snapshot used by gate/bank planning.
  The configured audit command runs first and must emit audits/<HEAD>.edn."
  [run-dir workspace config state]
  (let [head (git! workspace "rev-parse" "HEAD")]
    (when-not (= head (:head-sha state))
      (refuse! :workspace-head-drift
               {:expected (:head-sha state) :observed head}))
    (let [values {:head head :base (:base-sha state)
                  :problem (:problem-id state)
                  :run-dir (.getCanonicalPath (io/file run-dir))}
          audit-command (:audit-command config)
          audit-result (if (vector? audit-command)
                         (run-process workspace (substitute audit-command values))
                         {:exit 127 :stdout "" :stderr "audit command not configured"
                          :argv [] :cwd (str workspace)})
          files (lean-files workspace)
          targets (targets-ledger workspace (:problem-id state))]
      {:base-sha (:base-sha state)
       :head-sha head
       :name-status (git! workspace "-c" "core.quotePath=false" "diff"
                          "--name-status" (str (:base-sha state) ".." head))
       :porcelain (git! workspace "-c" "core.quotePath=false" "status"
                        "--porcelain=v1" "--untracked-files=all")
       :files files
       :problem-main (str "problems/" (:problem-id state) "/lean/Main.lean")
       :targets targets
       :target-provenance
       (delayed-target-provenance run-dir workspace state targets)
       :axiom-audits (if (zero? (:exit audit-result))
                       (audit-evidence run-dir head)
                       {})
       :observation/commands [audit-result]})))

(defn- status-evidence [run-dir candidate]
  (let [path (io/file run-dir "status" (str candidate ".edn"))]
    (when (.isFile path)
      (let [value (read-edn path)]
        (when-not (and (= 1 (:schema value))
                       (= candidate (:candidate-sha value))
                       (contains? #{:closed :partial-banked} (:ruling value))
                       (string? (:status-sha value)))
          (refuse! :status-evidence-invalid {:path (str path) :evidence value}))
        value))))

(defn- observe-bank [run-dir trunk intent]
  (let [trunk-head (git! trunk "rev-parse" "HEAD")
        candidate (:head-sha intent)
        base (:base-sha intent)]
    (cond
      (= trunk-head candidate)
      (if-let [status (status-evidence run-dir candidate)]
        (merge {:landed? true :bank-sha candidate} status)
        {:landed? false :candidate-landed? true :bank-sha candidate})

      (= trunk-head base) {:landed? false :candidate-landed? false}

      :else (refuse! :bank-trunk-race
                     {:expected-base base :candidate candidate
                      :observed-trunk trunk-head}))))

(defn- clean! [workspace finding]
  (let [status (git! workspace "-c" "core.quotePath=false" "status"
                     "--porcelain=v1" "--untracked-files=all")]
    (when-not (str/blank? status)
      (refuse! finding {:workspace (str workspace) :status status}))))

(defn- bank-command [workspace trunk intent]
  (clean! workspace :bank-candidate-dirty)
  (clean! trunk :bank-trunk-dirty)
  (let [base (:base-sha intent)
        candidate (:head-sha intent)
        trunk-head (git! trunk "rev-parse" "HEAD")]
    (when-not (= base trunk-head)
      (refuse! :bank-trunk-race {:expected base :observed trunk-head}))
    (git! workspace "merge-base" "--is-ancestor" base candidate)
    {:cwd (str trunk) :argv ["git" "merge" "--ff-only" candidate]}))

(defn- status-command [run-dir trunk config intent]
  (let [argv (:status-command config)]
    (when-not (vector? argv)
      (refuse! :status-command-missing {:config config}))
    {:cwd (str trunk)
     :argv (substitute argv {:head (:head-sha intent)
                             :problem (:problem-id intent)
                             :run-dir (.getCanonicalPath (io/file run-dir))})}))

(defn- render-gate-feedback [feedback]
  (when feedback
    (str "\n\n# Durable feedback from the immediately preceding gate\n\n"
         "This is a state-bound runner receipt, not a new mathematical goal. "
         "Correct the reported infrastructure or registration finding while "
         "preserving valid work.\n\n"
         (pr-str feedback)
         "\n")))

(defn- codex-command [run-dir config state]
  (let [prompt (canonical-file (io/file run-dir "standing-goal.md"))
        argv (:codex-command config)
        feedback (runner/preceding-gate-feedback run-dir state)
        prompt-text (str (slurp prompt) (render-gate-feedback feedback))]
    (when-not (and (.isFile prompt) (pos? (.length prompt)))
      (refuse! :standing-goal-missing {:path (str prompt)}))
    (when-not (and (vector? argv) (>= (count argv) 2)
                   (= "codex" (.getName (io/file (first argv))))
                   (= "exec" (second argv)))
      (refuse! :codex-command-invalid {:command argv}))
    (substitute argv {:prompt (str prompt)
                      :prompt-text prompt-text})))

(defn deps
  "Deployment adapter entry point loaded via LIBRARY_LOOP_ADAPTER_NS."
  [{:keys [root problem-id]}]
  (let [run-dir (exec/run-dir root problem-id)
        state (runner/read-state run-dir)
        workspace (validate-workspace (:workspace state))
        config (config run-dir)
        trunk (validate-workspace (:trunk-worktree config))
        workspace-common (git-path workspace (git! workspace "rev-parse" "--git-common-dir"))
        trunk-common (git-path trunk (git! trunk "rev-parse" "--git-common-dir"))
        trunk-branch (git! trunk "symbolic-ref" "--short" "HEAD")
        runner-command (fn [command]
                         (let [command (if (and (vector? command)
                                                (= "lake" (first command))
                                                (:lake-executable config))
                                         (assoc command 0 (:lake-executable config))
                                         command)]
                           (if (map? command)
                             (run-process command)
                             (run-process workspace command))))]
    (when-not (= workspace-common trunk-common)
      (refuse! :bank-worktrees-not-same-repository
               {:workspace-common (str workspace-common)
                :trunk-common (str trunk-common)}))
    (when-not (= (:trunk-branch config) trunk-branch)
      (refuse! :bank-trunk-branch-mismatch
               {:expected (:trunk-branch config) :observed trunk-branch}))
    {:run-command runner-command
     :turn-command (fn [turn-state] (codex-command run-dir config turn-state))
     :observe-head #(git! workspace "rev-parse" "HEAD")
     :reconcile-turn (fn [intent]
                       {:outcome :failed
                        :finding :turn-observation-unavailable
                        :head-sha (:head-sha intent)
                        :recovery :inspect-codex-output-and-record-explicit-disposition})
     :observe #(observe run-dir workspace config %)
     :observe-bank #(observe-bank run-dir trunk %)
     :bank-command #(bank-command workspace trunk %)
     :status-command #(status-command run-dir trunk config %)
     :slate-path (or (:slate-path config)
                     (str (io/file root "data" "apm-lane"
                                   "demonstrators.edn")))
     :cadence (or (:checkpoint-cadence config) 20)}))
