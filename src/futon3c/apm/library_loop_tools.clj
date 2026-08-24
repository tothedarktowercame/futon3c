(ns futon3c.apm.library-loop-tools
  "Production audit and status evidence commands for the files-only Library Loop."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.library-loop-adapter :as adapter]
            [futon3c.apm.library-loop-rebuild :as rebuild]
            [futon3c.apm.library-loop-runner :as runner]
            [futon3c.apm.toolchain-port :as toolchain])
  (:import (java.io PushbackReader)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.security MessageDigest)))

(def ^:private closed-classifications #{"solved"})
(def ^:private partial-classifications #{"partial" "partial-banked"})

(defn- tool-workspace []
  (or (System/getenv "LIBRARY_LOOP_TOOL_WORKSPACE") "."))

(defn- refuse! [finding data]
  (throw (ex-info (name finding) (assoc data :finding finding))))

(defn- read-edn [path]
  (with-open [reader (PushbackReader. (io/reader path))]
    (edn/read {:eof nil} reader)))

(defn- sha256 [value]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str value) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- command! [run-process workspace argv finding]
  (let [result (run-process workspace argv)]
    (when-not (zero? (:exit result))
      (refuse! finding {:argv argv :cwd (str workspace)
                        :exit (:exit result) :stdout (:stdout result)
                        :stderr (:stderr result)}))
    result))

(defn- git! [run-process workspace & args]
  (-> (command! run-process workspace (into ["git"] args)
                :library-loop-tool-git-failed)
      :stdout str/trim-newline))

(defn- state-bound! [run-dir workspace expected-head]
  (let [state (read-edn (io/file run-dir "state.edn"))
        actual-workspace (.getCanonicalPath workspace)
        state-workspace (.getCanonicalPath (io/file (:workspace state)))]
    (when-not (= actual-workspace state-workspace)
      (refuse! :tool-workspace-state-mismatch
               {:state-workspace state-workspace :cwd actual-workspace}))
    (when-not (= expected-head (:head-sha state))
      (refuse! :tool-head-state-mismatch
               {:state-head (:head-sha state) :expected expected-head}))
    state))

(defn- changed-modules [name-status porcelain]
  (->> (concat (rebuild/parse-name-status name-status)
               (rebuild/parse-porcelain porcelain))
       (filter (fn [{:keys [kind path]}]
                 (and (not (contains? #{"D" "R-old"} kind))
                      (str/starts-with? path "ConstructionTargets/")
                      (str/ends-with? path ".lean"))))
       (map (fn [{:keys [path]}]
              (-> path (str/replace #"\.lean$" "")
                  (str/replace "/" "."))))
       set
       sort
       vec))

(defn- ledger-index [workspace problem-id]
  (let [path (io/file workspace "problems" problem-id "targets.edn")]
    (when-not (.isFile path)
      (refuse! :audit-target-ledger-missing {:path (str path)}))
    (let [rows (read-edn path)]
      (when-not (vector? rows)
        (refuse! :audit-target-ledger-invalid {:path (str path)}))
      (let [groups (group-by :module rows)]
        (when-let [[module entries]
                   (first (filter (fn [[_ entries]] (not= 1 (count entries))) groups))]
          (refuse! :audit-target-ledger-ambiguous
                   {:module module :entries entries}))
        (into {} (map (fn [[module entries]] [module (first entries)])) groups)))))

(defn- declaration-names [ledger module]
  (let [names (:declarations (get ledger module))]
    (when-not (and (vector? names) (seq names)
                   (= (count names) (count (distinct names)))
                   (every? #(or (symbol? %) (and (string? %) (not (str/blank? %))))
                           names))
      (refuse! :audit-ledger-declarations-invalid
               {:module module :declarations names}))
    (mapv str names)))

(defn- audit-source [module declarations]
  (str "import " module "\n"
       (str/join "\n" (map #(str "#print axioms " %) declarations))
       "\n"))

(defn- audit-module! [run-process workspace run-dir head ledger module]
  (let [declarations (declaration-names ledger module)
        build-result (command! run-process workspace ["lake" "build" module]
                               :axiom-audit-module-build-failed)
        audit-dir (io/file run-dir "audit-inputs")
        _ (.mkdirs audit-dir)
        path (Files/createTempFile (.toPath audit-dir) "axioms-" ".lean"
                                   (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile path) (audit-source module declarations))
      (let [result (command! run-process workspace
                             ["lake" "env" "lean" (str path)]
                             :axiom-audit-elaboration-failed)
            output (str (:stdout result) "\n" (:stderr result))]
        (when (str/includes? output "sorryAx")
          (refuse! :axiom-audit-sorry-axiom
                   {:module module :declarations declarations :output output}))
        [module {:ok? true :head-sha head
                 :declarations (mapv symbol declarations)
                 :build (select-keys build-result
                                     [:argv :cwd :exit :stdout :stderr])
                 :audit (select-keys result
                                     [:argv :cwd :exit :stdout :stderr])}])
      (finally
        (Files/deleteIfExists path)))))

(defn audit!
  ([base head run-dir]
   (audit! base head run-dir adapter/run-process))
  ([base head run-dir run-process]
   (audit! base head run-dir run-process (tool-workspace)))
  ([base head run-dir run-process workspace-path]
   (let [workspace (adapter/validate-workspace workspace-path)
         run-dir (.getCanonicalFile (io/file run-dir))
         state (state-bound! run-dir workspace head)
         observed-head (git! run-process workspace "rev-parse" "HEAD")]
     (when-not (= head observed-head)
       (refuse! :audit-head-mismatch {:expected head :observed observed-head}))
     (command! run-process workspace ["git" "merge-base" "--is-ancestor" base head]
               :audit-base-not-ancestor)
     (let [name-status (git! run-process workspace "-c" "core.quotePath=false"
                             "diff" "--name-status" (str base ".." head))
           porcelain (git! run-process workspace "-c" "core.quotePath=false"
                           "status" "--porcelain=v1" "--untracked-files=all")
           modules (changed-modules name-status porcelain)
           ledger (when (seq modules)
                    (ledger-index workspace (:problem-id state)))
           evidence {:schema 1 :head-sha head
                     :modules (into (sorted-map)
                                    (map #(audit-module! run-process workspace run-dir
                                                         head ledger %))
                                    modules)}
           output (io/file run-dir "audits" (str head ".edn"))]
       (runner/append-edn-once! output evidence)
       evidence))))

(defn- clean-worktree! [run-process workspace]
  (let [porcelain (git! run-process workspace "-c" "core.quotePath=false"
                        "status" "--porcelain=v1" "--untracked-files=all")]
    (when-not (str/blank? porcelain)
      (refuse! :status-worktree-dirty {:status porcelain}))))

(defn- status-workspace! [run-process run-dir state workspace]
  (let [config-path (io/file run-dir "config.edn")]
    (when-not (.isFile config-path)
      (refuse! :status-config-missing {:path (str config-path)}))
    (let [config (read-edn config-path)
          configured-trunk (.getCanonicalPath
                            (io/file (:trunk-worktree config)))
          actual-trunk (.getCanonicalPath workspace)]
      (when-not (and (= 1 (:schema config))
                     (string? (:trunk-worktree config))
                     (string? (:trunk-branch config)))
        (refuse! :status-config-invalid {:config config}))
      (when-not (= configured-trunk actual-trunk)
        (refuse! :status-trunk-workspace-mismatch
                 {:configured configured-trunk :cwd actual-trunk}))
      (let [branch (git! run-process workspace "symbolic-ref" "--short" "HEAD")]
        (when-not (= (:trunk-branch config) branch)
          (refuse! :status-trunk-branch-mismatch
                   {:configured (:trunk-branch config) :observed branch})))
      (let [solver (adapter/validate-workspace (:workspace state))
            trunk-common (.getCanonicalPath
                          (io/file workspace
                                   (git! run-process workspace "rev-parse"
                                         "--git-common-dir")))
            solver-common (.getCanonicalPath
                           (io/file solver
                                    (git! run-process solver "rev-parse"
                                          "--git-common-dir")))]
        (when-not (= trunk-common solver-common)
          (refuse! :status-worktrees-not-same-repository
                   {:trunk-common trunk-common :solver-common solver-common})))
      config)))

(defn status!
  ([candidate run-dir]
   (status! candidate run-dir adapter/run-process))
  ([candidate run-dir run-process]
   (status! candidate run-dir run-process (tool-workspace)))
  ([candidate run-dir run-process workspace-path]
   (let [workspace (adapter/validate-workspace workspace-path)
         run-dir (.getCanonicalFile (io/file run-dir))
         state (read-edn (io/file run-dir "state.edn"))
         observed-head (git! run-process workspace "rev-parse" "HEAD")
         problem-id (:problem-id state)]
     (status-workspace! run-process run-dir state workspace)
     (when-not (= candidate (:head-sha state))
       (refuse! :status-head-state-mismatch
                {:candidate candidate :state-head (:head-sha state)}))
     (when-not (= candidate observed-head)
       (refuse! :status-candidate-not-landed
                {:candidate candidate :observed observed-head}))
     (command! run-process workspace
               ["git" "merge-base" "--is-ancestor" (:base-sha state) candidate]
               :status-base-not-ancestor)
     (clean-worktree! run-process workspace)
     (let [main-path (str "problems/" problem-id "/lean/Main.lean")
           status-path (io/file workspace "problems" problem-id "status.json")]
       (when-not (and (.isFile (io/file workspace main-path)) (.isFile status-path))
         (refuse! :canonical-problem-evidence-missing
                  {:main main-path :status (str status-path)}))
       (let [result (run-process workspace ["lake" "env" "lean" main-path])
             classified (toolchain/classify-output
                         (:exit result) (str (:stdout result) "\n" (:stderr result)))]
         (when-not (and (zero? (:exit classified)) (zero? (:errors classified)))
           (refuse! :status-main-elaboration-failed
                    {:command ["lake" "env" "lean" main-path]
                     :result classified}))
         (let [status-text (slurp status-path)
               canonical (json/parse-string status-text true)
               classification (:classification canonical)
               recorded-sorries (get-in canonical [:lean :sorry_count_total])
               sorries (:sorry-warnings classified)]
           (when-not (= sorries recorded-sorries)
             (refuse! :status-sorry-count-mismatch
                      {:elaborated sorries :recorded recorded-sorries}))
           (let [ruling (cond
                          (and (zero? sorries)
                               (contains? closed-classifications classification))
                          :closed

                          (and (pos? sorries)
                               (contains? partial-classifications classification))
                          :partial-banked

                          :else
                          (refuse! :status-classification-inconsistent
                                   {:classification classification
                                    :sorry-count sorries}))
                 status-sha (sha256 (pr-str
                                     {:candidate-sha candidate
                                      :problem-id problem-id
                                      :status-json-sha (sha256 status-text)
                                      :main-sha (sha256 (slurp (io/file workspace main-path)))
                                      :elaboration-output-sha
                                      (sha256 (:output classified))
                                      :classification classification
                                      :sorry-count sorries}))
                 evidence {:schema 1 :candidate-sha candidate
                           :ruling ruling :status-sha status-sha}
                 output (io/file run-dir "status" (str candidate ".edn"))]
             (runner/append-edn-once! output evidence)
             evidence)))))))

(defn -main [& args]
  (try
    (let [[command & rest] args
          result (case command
                   "audit" (apply audit! rest)
                   "status" (apply status! rest)
                   (refuse! :library-loop-tool-command-invalid
                            {:command command}))]
      (println (pr-str result)))
    (catch Throwable ex
      (binding [*out* *err*]
        (println (pr-str {:error (.getMessage ex) :data (ex-data ex)})))
      (System/exit 2))))
