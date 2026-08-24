(ns futon3c.apm.workspace-lifecycle
  "Lease-backed provisioning and fail-closed retirement of APM worktrees."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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
            (let [local-lake (.resolve workspace ".lake")]
              (Files/createDirectories local-lake (make-array FileAttribute 0))
              (Files/createSymbolicLink
               (.resolve local-lake "packages")
               (.resolve (canonical substrate-path) "packages")
               (make-array FileAttribute 0)))
            (let [body {:workspace/id nil
                        :workspace/path (str workspace)
                        :repository/path (str repository)
                        :branch branch :base-revision (:revision problem)
                        :problem/id problem-id :problem/path (:path problem)
                        :problem/blob (:blob problem) :frame/id frame-id :role role
                        :created-at (str (or now (Instant/now)))
                        :retention/state :provisioned
                        :substrate/path (str (.resolve workspace ".lake"))
                        :substrate/source (str (canonical substrate-path))}
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
         build-path (.resolve lake-link "build")
         packages-link (.resolve lake-link "packages")
         expected-packages (.resolve (canonical (:substrate/source lease))
                                     "packages")
         packages-target (when (Files/isSymbolicLink packages-link)
                           (canonical (Files/readSymbolicLink packages-link)))
         manifest-path (.resolve workspace "lake-manifest.json")
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
                    (Files/isSymbolicLink lake-link) (conj :workspace-substrate-not-isolated)
                    (not= substrate (canonical lake-link))
                    (conj :workspace-substrate-path-mismatch)
                    (Files/isSymbolicLink build-path)
                    (conj :workspace-build-not-isolated)
                    (not= expected-packages packages-target)
                    (conj :workspace-packages-authority-mismatch)
                    (not manifest-readable?) (conj :workspace-substrate-manifest-missing)
                    (not (zero? (:exit probe))) (conj :workspace-probe-failed))]
     {:valid? (empty? findings) :findings findings :head head :branch branch
      :worktree-clean? clean? :problem/blob blob :probe/exit (:exit probe)
      :substrate/path (some-> substrate str)})))

(defn archive-problem-source!
  "Copy the worktree's problem file into ARCHIVE-DIRECTORY, named by its git
  blob id, so the source survives the worktree being reset or retired.

  Student worktrees are never committed: on f27 the Student's independent
  proof existed only as an uncommitted modification and was destroyed by the
  retirement, leaving a lemma name in a receipt as its only trace."
  [{:keys [archive-directory] :as lease}]
  (let [workspace (canonical (:workspace/path lease))
        problem-path (:problem/path lease)
        source (when (and workspace (string? problem-path))
                 (.resolve workspace problem-path))]
    (cond
      (not (and source (string? archive-directory)
                (not (str/blank? archive-directory))))
      {:ok false :error/code :workspace-source-archive-shape-invalid}
      (not (Files/isRegularFile source (make-array LinkOption 0)))
      {:ok false :error/code :workspace-source-missing :path (str source)}
      :else
      (let [blob (out (run workspace "hash-object" problem-path))
            head (out (run workspace "rev-parse" "HEAD"))
            status (or (out (run workspace "status" "--porcelain=v1" "--"
                                problem-path))
                       "")
            directory (canonical archive-directory)
            target (.resolve directory
                             (str blob "-" (.getFileName
                                            (Path/of problem-path
                                                     (make-array String 0)))))]
        (if-not (and blob head)
          {:ok false :error/code :workspace-source-git-failed}
          (try
            (Files/createDirectories directory (make-array FileAttribute 0))
            (Files/copy source target
                        (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
            (if (java.util.Arrays/equals (Files/readAllBytes source)
                                         (Files/readAllBytes target))
              {:ok true
               :source {:path (str target) :blob blob :head head
                        :problem/path problem-path
                        :dirty? (not (str/blank? status))}}
              {:ok false :error/code :workspace-source-archive-postcondition-failed
               :path (str target)})
            (catch Throwable t
              {:ok false :error/code :workspace-source-archive-failed
               :finding {:message (.getMessage t)}})))))))

(defn preserve-student-candidate!
  "Commit and certify the exact Student worktree at a terminal boundary.

  A dirty tree is committed on its leased branch before validation; a clean
  tree reuses its current head. The resulting head is pinned under a
  content-addressed, attempt-specific ref and validated against the canonical
  workspace substrate. Replaying after a crash is idempotent because the
  already-clean head and exact ref produce the same certificate."
  [{:keys [lease attempt-ordinal probe-fn]}]
  (let [workspace (canonical (:workspace/path lease))
        frame-id (:frame/id lease)
        problem-id (:problem/id lease)
        shape? (and (map? lease) workspace
                    (string? frame-id) (not (str/blank? frame-id))
                    (string? problem-id) (not (str/blank? problem-id))
                    (contains? #{1 2 3} attempt-ordinal)
                    (= :student (:role lease)))]
    (if-not shape?
      {:ok false :error/code :student-candidate-shape-invalid}
      (let [status-result (run workspace "status" "--porcelain=v1")
            status-before (when (zero? (:exit status-result))
                            (vec (remove str/blank?
                                         (str/split-lines (:out status-result)))))
            dirty? (seq status-before)
            staged (when dirty? (run workspace "add" "--all"))
            committed (when (and dirty? (zero? (:exit staged)))
                        (run workspace
                             "-c" "user.name=APM Student Candidate Controller"
                             "-c" "user.email=apm-controller@invalid"
                             "commit" "-m"
                             (str "preserve " frame-id " " problem-id
                                  " student attempt " attempt-ordinal)))
            head (when (and (zero? (:exit status-result))
                            (or (not dirty?) (zero? (:exit committed))))
                   (out (run workspace "rev-parse" "HEAD")))
            ref (when head
                  (str "refs/apm/student-candidates/" frame-id "/" problem-id
                       "/attempt-" attempt-ordinal "/" head))
            pinned (when ref (run workspace "update-ref" ref head))]
        (cond
          (not (zero? (:exit status-result)))
          {:ok false :error/code :student-candidate-status-failed
           :finding {:exit (:exit status-result) :stderr (:err status-result)}}

          (and dirty? (not (zero? (:exit staged))))
          {:ok false :error/code :student-candidate-stage-failed
           :finding {:exit (:exit staged) :stderr (:err staged)}}

          (and dirty? (not (zero? (:exit committed))))
          {:ok false :error/code :student-candidate-commit-failed
           :finding {:exit (:exit committed) :stderr (:err committed)}}

          (not (and head (re-matches #"[0-9a-f]{40}" head)))
          {:ok false :error/code :student-candidate-head-invalid :head head}

          (not (zero? (:exit pinned)))
          {:ok false :error/code :student-candidate-ref-failed
           :finding {:exit (:exit pinned) :stderr (:err pinned)}}

          :else
          (let [validation (validate lease
                                     (cond-> {:expected-head head}
                                       probe-fn (assoc :probe-fn probe-fn)))]
            (if-not (:valid? validation)
              {:ok false :error/code :student-candidate-validation-failed
               :head head :ref ref :validation validation}
              (let [body {:candidate/type :student-terminal
                          :workspace/id (:workspace/id lease)
                          :frame/id frame-id :problem/id problem-id
                          :attempt/ordinal attempt-ordinal
                          :candidate/head head :candidate/ref ref
                          :candidate/problem-blob (:problem/blob validation)
                          :candidate/lean-exit (:probe/exit validation)
                          :candidate/worktree-clean? (:worktree-clean? validation)
                          :candidate/persisted-before-receipt? true}]
                {:ok true
                 :candidate (assoc body :candidate/id (address body))
                 :created-commit? (boolean dirty?)
                 :status-before status-before}))))))))

(defn reset-to-base!
  "Return a worktree to its registered base revision with a clean tree.

  Before changing the branch or deleting untracked files, preserve the complete
  tracked/untracked attempt state behind a durable Git ref. Ignored paths such
  as the `.lake` substrate link are kept. Fails closed unless preservation
  succeeds and the result is exactly the base with nothing outstanding."
  [lease]
  (let [workspace (canonical (:workspace/path lease))
        base-revision (:base-revision lease)
        problem-path (:problem/path lease)]
    (cond
      (not (and workspace (string? base-revision)
                (re-matches #"[0-9a-f]{40}" base-revision)))
      {:ok false :error/code :workspace-reset-shape-invalid}
      (not (Files/isDirectory workspace (make-array LinkOption 0)))
      {:ok false :error/code :workspace-reset-path-missing :path (str workspace)}
      :else
      (let [head-before (out (run workspace "rev-parse" "HEAD"))
            status-before (vec (remove str/blank?
                                       (str/split-lines
                                        (or (out (run workspace "status"
                                                      "--porcelain=v1"))
                                            ""))))
            dirty? (seq status-before)
            stashed (when dirty?
                      (run workspace "stash" "push" "--include-untracked"
                           "--message" "APM fresh Student attempt preservation"))
            preserved-commit (if dirty?
                               (when (zero? (:exit stashed))
                                 (out (run workspace "rev-parse" "refs/stash")))
                               (when (not= head-before base-revision) head-before))
            preservation-ref (when preserved-commit
                               (str "refs/apm/preserved-student-attempts/"
                                    (or (:frame/id lease) "unknown-frame") "/"
                                    (or (:problem/id lease) "unknown-problem") "/"
                                    preserved-commit))
            preserved (when preservation-ref
                        (run workspace "update-ref" preservation-ref
                             preserved-commit))
            preservation-ok? (and (or (not dirty?)
                                      (and stashed (zero? (:exit stashed))))
                                  (or (nil? preservation-ref)
                                      (and preserved (zero? (:exit preserved)))))
            reset (when preservation-ok?
                    (run workspace "reset" "--hard" base-revision))
            clean (when (and reset (zero? (:exit reset)))
                    (run workspace "clean" "-fd"))
            head (out (run workspace "rev-parse" "HEAD"))
            status-after (out (run workspace "status" "--porcelain=v1"))
            blob (when (string? problem-path)
                   (out (run workspace "rev-parse" (str "HEAD:" problem-path))))]
        (cond
          (not preservation-ok?)
          {:ok false :error/code :workspace-reset-preservation-failed
           :finding {:stash/exit (:exit stashed)
                     :stash/stderr (:err stashed)
                     :ref/exit (:exit preserved)
                     :ref/stderr (:err preserved)}}
          (not (zero? (:exit reset)))
          {:ok false :error/code :workspace-reset-git-failed
           :finding {:exit (:exit reset) :stderr (:err reset)}}
          (not (and clean (zero? (:exit clean))))
          {:ok false :error/code :workspace-clean-git-failed
           :finding {:exit (:exit clean) :stderr (:err clean)}}
          (not (and (= base-revision head) (= "" (or status-after ""))))
          {:ok false :error/code :workspace-reset-postcondition-failed
           :head head :status status-after}
          :else
          {:ok true :head head :problem/blob blob
           :preserved (when preservation-ref
                        {:ref preservation-ref :commit preserved-commit})
           :discarded {:head head-before :status status-before}})))))

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
       :passed passed :required required-retirement-preconditions
       :validation/valid? (:valid? validation)
       :validation/findings (:findings validation)
       :terminal-head terminal-head
       :validation/head (:head validation)
       :context context}
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

(defn retirement-status
  "Recover a completed retirement from its durable receipt.

  A replay may observe the worktree already absent because a prior execution
  removed it and persisted the receipt before a later role failed.  Only an
  addressed receipt for this exact lease and terminal head, together with the
  retained branch and absent path postconditions, counts as completed."
  [{:keys [lease terminal-head receipt-directory]}]
  (let [repository (canonical (:repository/path lease))
        workspace (canonical (:workspace/path lease))
        branch-ref (str "refs/heads/" (:branch lease))
        branch-head (out (run repository "rev-parse" "--verify" branch-ref))
        directory (io/file (str receipt-directory))
        receipts (if (.isDirectory directory)
                   (keep (fn [file]
                           (when (and (.isFile file)
                                      (str/ends-with? (.getName file) ".edn"))
                             (try (read-receipt file)
                                  (catch Throwable _ nil))))
                         (.listFiles directory))
                   [])
        receipt (some (fn [candidate]
                        (when (and (= :workspace-retired
                                      (:receipt/type candidate))
                                   (= (:receipt/id candidate)
                                      (address (dissoc candidate :receipt/id)))
                                   (= (:workspace/id lease)
                                      (:workspace/id candidate))
                                   (= (str workspace)
                                      (:workspace/path candidate))
                                   (= (:frame/id lease) (:frame/id candidate))
                                   (= (:role lease) (:role candidate))
                                   (= (:branch lease) (:branch candidate))
                                   (= terminal-head (:retained-head candidate))
                                   (= required-retirement-preconditions
                                      (:preconditions candidate))
                                   (string? (:audit/id candidate))
                                   (keyword? (:audit/context candidate)))
                          candidate))
                      receipts)
        path-absent? (not (Files/exists workspace (make-array LinkOption 0)))]
    (cond
      (nil? receipt) {:ok true :status :not-retired}
      (and path-absent? (= terminal-head branch-head))
      {:ok true :status :already-retired :receipt receipt
       :path-absent? true :branch-head branch-head}
      :else
      {:ok false :error/code :workspace-retirement-replay-postcondition-failed
       :receipt/id (:receipt/id receipt) :path-absent? path-absent?
       :expected-head terminal-head :branch-head branch-head})))
