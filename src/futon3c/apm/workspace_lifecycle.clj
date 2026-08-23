(ns futon3c.apm.workspace-lifecycle
  "Lease-backed provisioning and fail-closed retirement of APM worktrees."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files LinkOption OpenOption Path
            StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(def required-retirement-preconditions
  #{:frame-terminal :no-running-or-parked-job-references-workspace
    :no-active-ledger-claim-references-workspace :worktree-clean
    :head-commit-recorded-in-terminal-receipt :branch-ref-exists
    :required-artifacts-content-addressed :independent-retirement-audit-passed})

(defn- run [repository & args]
  (apply shell/sh (map str (concat ["git" "-C" repository] args))))

(defn- out [result]
  (when (zero? (:exit result)) (str/trim (:out result))))

(defn- canonical [value]
  (some-> (Path/of (str value) (make-array String 0)) .toAbsolutePath .normalize))

(defn- beneath? [root child]
  (and root child (.startsWith child root) (not= root child)))

(defn- address [body]
  (machine/ledger-digest [body]))

(defn provision!
  "Create one exact branch-backed worktree and return its content-addressed lease."
  [{:keys [unit role workspace-root substrate-path now]}]
  (let [problem (:problem unit)
        repository (canonical (:repository problem))
        root (canonical workspace-root)
        frame-id (:frame/id unit)
        problem-id (:problem/id unit)
        branch (str "exp/countdown-" frame-id "-" problem-id "-" (name role))
        workspace (canonical (.resolve root (str frame-id "-" problem-id "-" (name role))))
        shape? (and (contains? #{:solver :student} role)
                    (every? #(and (string? %) (not (str/blank? %)))
                            [frame-id problem-id (:revision problem) (:blob problem)
                             (str repository) (str root) (str substrate-path)])
                    (beneath? root workspace))]
    (cond
      (not shape?) {:ok false :error/code :workspace-provision-shape-invalid}
      (Files/exists workspace (make-array LinkOption 0))
      {:ok false :error/code :workspace-provision-path-exists :path (str workspace)}
      :else
      (let [branch-head (out (run repository "rev-parse" "--verify"
                                  (str "refs/heads/" branch)))
            branch-mismatch? (and branch-head (not= branch-head (:revision problem)))
            added (when-not branch-mismatch?
                    (if branch-head
                      (run repository "worktree" "add" (str workspace) branch)
                      (run repository "worktree" "add" "-b" branch (str workspace)
                           (:revision problem))))]
        (if branch-mismatch?
          {:ok false :error/code :workspace-provision-retained-branch-mismatch
           :branch branch :expected (:revision problem) :observed branch-head}
        (if-not (zero? (:exit added))
          {:ok false :error/code :workspace-provision-git-failed
           :finding {:exit (:exit added) :stderr (:err added)}}
          (try
            (Files/createSymbolicLink (.resolve workspace ".lake")
                                      (canonical substrate-path)
                                      (make-array FileAttribute 0))
            (let [body {:workspace/id nil
                        :workspace/path (str workspace)
                        :repository/path (str repository)
                        :branch branch :base-revision (:revision problem)
                        :problem/id problem-id :problem/path (:path problem)
                        :problem/blob (:blob problem) :frame/id frame-id :role role
                        :created-at (str (or now (Instant/now)))
                        :retention/state :provisioned
                        :substrate/path (str (canonical substrate-path))}
                  lease (assoc body :workspace/id (address (dissoc body :workspace/id)))]
              {:ok true :lease lease})
            (catch Throwable t
              ;; A partially provisioned worktree is reported, never force-removed.
              {:ok false :error/code :workspace-provision-substrate-link-failed
               :workspace/path (str workspace) :finding {:message (.getMessage t)}}))))))))

(defn validate
  "Validate a lease against Git registration, clean state, source blob, and substrate."
  ([lease] (validate lease {}))
  ([lease {:keys [probe-fn expected-head]}]
   (let [repository (canonical (:repository/path lease))
         workspace (canonical (:workspace/path lease))
         root-id (address (dissoc lease :workspace/id))
         registered (run repository "worktree" "list" "--porcelain")
         registered? (some #{(str "worktree " workspace)}
                           (str/split-lines (or (:out registered) "")))
         branch (out (run workspace "branch" "--show-current"))
         head (out (run workspace "rev-parse" "HEAD"))
         clean? (= "" (or (out (run workspace "status" "--porcelain=v1")) ""))
         blob (out (run workspace "rev-parse"
                        (str "HEAD:" (:problem/path lease))))
         expected-blob (if expected-head
                         (out (run repository "rev-parse"
                                   (str expected-head ":" (:problem/path lease))))
                         (:problem/blob lease))
         lake-link (.resolve workspace ".lake")
         substrate (canonical (:substrate/path lease))
         link-target (when (Files/isSymbolicLink lake-link)
                       (canonical (.resolve (.getParent lake-link)
                                            (Files/readSymbolicLink lake-link))))
         manifest-path (.resolve substrate "../lake-manifest.json")
         manifest-readable? (Files/isRegularFile (.normalize manifest-path)
                                                 (make-array LinkOption 0))
         probe (if probe-fn
                 (probe-fn lease)
                 (apply shell/sh (concat ["lake" "env" "lean" (:problem/path lease)
                                          :dir (str workspace)])))
         findings (cond-> []
                    (not= (:workspace/id lease) root-id) (conj :workspace-lease-address-invalid)
                    (not registered?) (conj :workspace-not-registered)
                    (not= (:branch lease) branch) (conj :workspace-branch-mismatch)
                    (not= (or expected-head (:base-revision lease)) head)
                    (conj :workspace-head-mismatch)
                    (not clean?) (conj :workspace-dirty)
                    (not= expected-blob blob) (conj :workspace-problem-blob-mismatch)
                    (not= substrate link-target) (conj :workspace-substrate-link-mismatch)
                    (not manifest-readable?) (conj :workspace-substrate-manifest-missing)
                    (not (zero? (:exit probe))) (conj :workspace-probe-failed))]
     {:valid? (empty? findings) :findings findings :head head :branch branch
      :worktree-clean? clean? :problem/blob blob :probe/exit (:exit probe)
      :substrate/path (some-> link-target str)})))

(defn- atomic-edn! [target value]
  (let [target (canonical target) directory (.getParent target)]
    (Files/createDirectories directory (make-array FileAttribute 0))
    (let [temporary (Files/createTempFile directory ".receipt-" ".edn"
                                          (make-array FileAttribute 0))]
      (Files/writeString temporary (str (pr-str value) "\n") StandardCharsets/UTF_8
                         (into-array OpenOption [StandardOpenOption/WRITE
                                                 StandardOpenOption/TRUNCATE_EXISTING
                                                 StandardOpenOption/SYNC]))
      (Files/move temporary target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                           StandardCopyOption/REPLACE_EXISTING]))
      (str target))))

(defn certify-retirement-audit
  "Content-address an independent observation of every retirement precondition."
  [{:keys [lease validation observations terminal-head context audited-at]}]
  (let [passed (->> observations (keep (fn [[k v]] (when (true? v) k))) set)]
    (if-not (and (:valid? validation)
                 (= terminal-head (:head validation))
                 (= required-retirement-preconditions passed)
                 (keyword? context))
      {:ok false :error/code :workspace-retirement-audit-invalid
       :passed passed :required required-retirement-preconditions}
      (let [body {:audit/type :workspace-retirement
                  :workspace/id (:workspace/id lease)
                  :terminal-head terminal-head :context context
                  :passed-preconditions passed
                  :audited-at (str (or audited-at (Instant/now)))}]
        {:ok true :audit (assoc body :audit/id (address body))}))))

(defn retire!
  "Remove only the leased worktree after all policy preconditions are certified."
  [{:keys [lease audit receipt-directory now]}]
  (let [validation (validate lease {:probe-fn (fn [_] {:exit 0})
                                    :expected-head (:terminal-head audit)})
        repository (canonical (:repository/path lease))
        workspace (canonical (:workspace/path lease))
        branch-ref (str "refs/heads/" (:branch lease))
        audit-valid? (and (= (:audit/id audit) (address (dissoc audit :audit/id)))
                          (= :workspace-retirement (:audit/type audit))
                          (= (:workspace/id lease) (:workspace/id audit))
                          (= required-retirement-preconditions
                             (:passed-preconditions audit)))]
    (cond
      (not audit-valid?)
      {:ok false :error/code :workspace-retirement-audit-certificate-invalid}
      (not (:valid? validation))
      {:ok false :error/code :workspace-retirement-validation-failed
       :validation validation}
      (not= (:terminal-head audit) (:head validation))
      {:ok false :error/code :workspace-retirement-terminal-head-mismatch}
      :else
      (let [removed (run repository "worktree" "remove" (str workspace))]
        (if-not (zero? (:exit removed))
          {:ok false :error/code :workspace-retirement-remove-failed
           :finding {:exit (:exit removed) :stderr (:err removed)}}
          (let [_ (run repository "worktree" "prune")
                branch-head (out (run repository "rev-parse" "--verify" branch-ref))
                path-absent? (not (Files/exists workspace (make-array LinkOption 0)))
                body {:receipt/type :workspace-retired
                      :workspace/id (:workspace/id lease) :workspace/path (str workspace)
                      :frame/id (:frame/id lease) :role (:role lease)
                      :branch (:branch lease) :retained-head branch-head
                      :retired-at (str (or now (Instant/now)))
                      :audit/id (:audit/id audit) :audit/context (:context audit)
                      :preconditions (:passed-preconditions audit)}
                receipt (assoc body :receipt/id (address body))]
            (if-not (and path-absent? (= (:terminal-head audit) branch-head))
              {:ok false :error/code :workspace-retirement-postcondition-failed
               :path-absent? path-absent? :branch-head branch-head}
              (let [receipt-path (atomic-edn!
                                  (.resolve (canonical receipt-directory)
                                            (str (:receipt/id receipt) ".edn")) receipt)]
                {:ok true :receipt receipt :receipt/path receipt-path
                 :path-absent? true :branch-head branch-head}))))))))

(defn read-receipt [receipt-path]
  (edn/read-string (slurp receipt-path)))
