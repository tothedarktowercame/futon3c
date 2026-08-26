(ns futon3c.apm.workspace-lifecycle-test
  (:require [clojure.java.shell :as shell]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.workspace-lifecycle :as sut])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(defn- sh [& args] (apply shell/sh args))

(defn- addressed? [value id-key]
  (= (get value id-key) (machine/ledger-digest [(dissoc value id-key)])))

(defn- fixture []
  (let [root (Files/createTempDirectory "apm-lifecycle-" (make-array FileAttribute 0))
        repo (.resolve root "repo")
        workspaces (.resolve root "workspaces")
        lake (.resolve repo ".lake")]
    (Files/createDirectories repo (make-array FileAttribute 0))
    (sh "git" "init" "-b" "main" (str repo))
    (spit (str (.resolve repo ".gitignore")) "/.lake\n")
    (Files/createDirectories (.resolve repo "problems/p1/lean")
                             (make-array FileAttribute 0))
    (spit (str (.resolve repo "problems/p1/lean/Main.lean")) "theorem p1 : True := by trivial\n")
    (spit (str (.resolve repo "lake-manifest.json")) "{}\n")
    (Files/createDirectories lake (make-array FileAttribute 0))
    (sh "git" "-C" (str repo) "add" ".")
    (sh "git" "-C" (str repo) "-c" "user.name=Test" "-c"
        "user.email=test@example.invalid" "commit" "-m" "base")
    (let [revision (:out (sh "git" "-C" (str repo) "rev-parse" "HEAD"))
          revision (.trim revision)
          blob (-> (sh "git" "-C" (str repo) "rev-parse"
                       (str revision ":problems/p1/lean/Main.lean")) :out .trim)]
      {:root root :repo repo :workspaces workspaces :lake lake
       :unit {:frame/id "f19" :problem/id "p1"
              :problem {:repository (str repo) :branch "main" :revision revision
                        :path "problems/p1/lean/Main.lean" :blob blob}}})))

(deftest provision-validate-retire-retains-branch-and-receipt
  (let [{:keys [root repo workspaces lake unit]} (fixture)
        provisioned (sut/provision! {:unit unit :role :solver
                                     :workspace-root workspaces
                                     :substrate-path lake
                                     :now (Instant/parse "2026-08-21T00:00:00Z")})
        lease (:lease provisioned)
        validation (sut/validate lease {:probe-fn (fn [_] {:exit 0})})
        receipt-dir (.resolve root "receipts")
        audit (:audit (sut/certify-retirement-audit
                       {:lease lease :validation validation
                        :observations (zipmap sut/required-retirement-preconditions
                                              (repeat true))
                        :terminal-head (:head validation)
                        :context :qualification-rehearsal
                        :audited-at (Instant/parse "2026-08-21T00:00:30Z")}))
        retired (sut/retire! {:lease lease :audit audit
                              :receipt-directory receipt-dir
                              :now (Instant/parse "2026-08-21T00:01:00Z")})]
    (is (:ok provisioned))
    (is (false? (Files/isSymbolicLink
                 (.resolve (Path/of (:workspace/path lease)
                                    (make-array String 0)) ".lake"))))
    (is (not= (str lake) (:substrate/path lease)))
    (is (:valid? validation) (pr-str (:findings validation)))
    (is (:ok retired))
    (is (:path-absent? retired))
    (is (= (:head validation) (:branch-head retired)))
    (is (= (:receipt retired) (sut/read-receipt (:receipt/path retired))))
    (let [replayed (sut/retirement-status
                    {:lease lease :terminal-head (:head validation)
                     :receipt-directory receipt-dir})]
      (is (:ok replayed) (pr-str replayed))
      (is (= :already-retired (:status replayed)))
      (is (= (:receipt/id (:receipt retired))
             (get-in replayed [:receipt :receipt/id]))))
    (is (zero? (:exit (sh "git" "-C" (str repo) "show-ref" "--verify"
                           (str "refs/heads/" (:branch lease))))))
    (testing "retained exact branch can be reprovisioned"
      (let [again (sut/provision! {:unit unit :role :solver
                                   :workspace-root workspaces
                                   :substrate-path lake})]
        (is (:ok again))
        (is (= (:base-revision lease) (:base-revision (:lease again))))))))

(deftest operations-fail-closed-on-scope-state-and-audit-mismatch
  (let [{:keys [root workspaces lake unit]} (fixture)]
    (testing "path collision"
      (Files/createDirectories (.resolve workspaces "f19-p1-solver")
                               (make-array FileAttribute 0))
      (is (= :workspace-provision-path-exists
             (:error/code (sut/provision! {:unit unit :role :solver
                                           :workspace-root workspaces
                                           :substrate-path lake})))))
    (testing "missing retirement evidence"
      (let [other-root (.resolve root "other")
            provisioned (sut/provision! {:unit unit :role :student
                                         :workspace-root other-root
                                         :substrate-path lake})
            lease (:lease provisioned)]
        (is (= :workspace-retirement-audit-certificate-invalid
               (:error/code (sut/retire! {:lease lease :audit {}
                                          :receipt-directory (.resolve root "receipts")}))))))))

(deftest exact-partial-provision-is-recovered-idempotently
  (let [{:keys [workspaces lake unit]} (fixture)
        now "2026-08-26T05:00:00Z"
        first-result (sut/provision! {:unit unit :role :student
                                      :workspace-root workspaces
                                      :substrate-path lake :now now})
        lease (:lease first-result)
        packages (.resolve (Path/of (:workspace/path lease)
                                    (make-array String 0))
                           ".lake/packages")]
    (is (:ok first-result))
    ;; Model interruption between Git worktree creation and substrate linking.
    (Files/delete packages)
    (let [recovered (sut/provision! {:unit unit :role :student
                                     :workspace-root workspaces
                                     :substrate-path lake :now now})]
      (is (:ok recovered))
      (is (= :recovered-partial (:status recovered)))
      (is (= (:workspace/id lease) (get-in recovered [:lease :workspace/id])))
      (is (= (.resolve lake "packages") (Files/readSymbolicLink packages))))))

(deftest retirement-binds-the-recorded-terminal-head-not-the-lease-base
  (let [{:keys [root repo workspaces lake unit]} (fixture)
        provisioned (sut/provision! {:unit unit :role :solver
                                     :workspace-root workspaces
                                     :substrate-path lake})
        lease (:lease provisioned)
        workspace (:workspace/path lease)]
    (spit (str workspace "/problems/p1/lean/Main.lean")
          "theorem p1 : True := by\n  exact True.intro\n")
    (is (zero? (:exit (sh "git" "-C" workspace "add"
                           "problems/p1/lean/Main.lean"))))
    (is (zero? (:exit (sh "git" "-C" workspace
                           "-c" "user.name=Test"
                           "-c" "user.email=test@example.invalid"
                           "commit" "-m" "solve p1"))))
    (let [terminal-head (-> (sh "git" "-C" workspace "rev-parse" "HEAD")
                            :out .trim)
          base-validation (sut/validate lease {:probe-fn (fn [_] {:exit 0})})
          terminal-validation (sut/validate lease
                                            {:probe-fn (fn [_] {:exit 0})
                                             :expected-head terminal-head})
          audit (:audit (sut/certify-retirement-audit
                         {:lease lease
                          :validation terminal-validation
                          :observations
                          (zipmap sut/required-retirement-preconditions
                                  (repeat true))
                          :terminal-head terminal-head
                          :context :terminal-head-regression
                          :audited-at (Instant/parse "2026-08-23T00:00:00Z")}))
          retired (sut/retire! {:lease lease :audit audit
                                :receipt-directory (.resolve root "receipts")})]
      (is (false? (:valid? base-validation)))
      (is (some #{:workspace-head-mismatch} (:findings base-validation)))
      (is (:valid? terminal-validation) (pr-str (:findings terminal-validation)))
      (is (= terminal-head (:head terminal-validation)))
      (is (:ok retired) (pr-str retired))
      (is (= terminal-head (:branch-head retired)))
      (is (zero? (:exit (sh "git" "-C" (str repo) "show-ref" "--verify"
                           (str "refs/heads/" (:branch lease)))))))))

(deftest committed-f19-rehearsal-evidence-is-self-contained-and-addressed
  (let [evidence (edn/read-string
                  (slurp "holes/labs/M-apm-demonstration/countdown-f19-workspace-evidence-v1.edn"))
        report (edn/read-string
                (slurp "holes/labs/M-apm-demonstration/countdown-f19-workspace-rehearsal-v1.edn"))]
    (is (addressed? report :rehearsal/id))
    (is (= [:solver :student] (mapv :role (:seats evidence))))
    (doseq [{:keys [lease validation audit receipt]} (:seats evidence)]
      (is (addressed? lease :workspace/id))
      (is (:valid? validation))
      (is (addressed? audit :audit/id))
      (is (addressed? receipt :receipt/id))
      (is (= (:audit/id audit) (:audit/id receipt)))
      (is (= (:workspace/id lease) (:workspace/id receipt))))))

(deftest invalid-retirement-audit-reports-the-hidden-validation-failure
  (let [validation {:valid? false :findings [:workspace-probe-failed]
                    :head "observed"}
        result (sut/certify-retirement-audit
                {:lease {:workspace/id "w"}
                 :validation validation
                 :observations
                 (zipmap sut/required-retirement-preconditions (repeat true))
                 :terminal-head "expected"
                 :context :test-auditor})]
    (is (= :workspace-retirement-audit-invalid (:error/code result)))
    (is (= [:workspace-probe-failed] (:validation/findings result)))
    (is (= "expected" (:terminal-head result)))
    (is (= "observed" (:validation/head result)))))

(deftest student-source-is-archived-then-worktree-reset-to-base
  (let [{:keys [workspaces lake unit]} (fixture)
        lease (:lease (sut/provision! {:unit unit :role :student
                                       :workspace-root workspaces
                                       :substrate-path lake
                                       :now (Instant/parse "2026-08-23T00:00:00Z")}))
        workspace (:workspace/path lease)
        problem (str workspace "/" (:problem/path lease))
        archive (str workspaces "/archive")]
    (spit problem "theorem p1 : True := by\n  exact trivial\n")
    (spit (str workspace "/scratch.lean") "-- untracked\n")
    (testing "archive names the file by blob and reports the dirty tree"
      (let [archived (sut/archive-problem-source!
                      {:workspace/path workspace :problem/path (:problem/path lease)
                       :archive-directory archive})
            source (:source archived)]
        (is (:ok archived))
        (is (= (slurp problem) (slurp (:path source))))
        (is (re-matches #"[0-9a-f]{40}" (:blob source)))
        (is (.endsWith ^String (:path source) (str (:blob source) "-Main.lean")))
        (is (= (:base-revision lease) (:head source)))
        (is (true? (:dirty? source)))))
    (testing "reset discards tracked and untracked work, keeps the substrate link"
      (let [reset (sut/reset-to-base! lease)]
        (is (:ok reset))
        (is (= (:base-revision lease) (:head reset)))
        (is (= (:problem/blob lease) (:problem/blob reset)))
        (is (= (:base-revision lease) (get-in reset [:discarded :head])))
        (is (= 2 (count (get-in reset [:discarded :status]))))
        (is (re-matches #"refs/apm/preserved-student-attempts/f19/p1/[0-9a-f]{40}"
                        (get-in reset [:preserved :ref])))
        (is (= "-- untracked\n"
               (:out (sh "git" "-C" workspace "show"
                         (str (get-in reset [:preserved :ref])
                              "^3:scratch.lean"))))
            "the preservation ref retains untracked attempt evidence")
        (is (= "theorem p1 : True := by trivial\n" (slurp problem)))
        (is (not (.exists (java.io.File. (str workspace "/scratch.lean")))))
        (is (Files/isSymbolicLink (Path/of (str workspace "/.lake/packages")
                                           (make-array String 0)))
            "ignored substrate link survives git clean")
        (is (true? (:worktree-clean? (sut/validate lease {:probe-fn (fn [_] {:exit 0})}))))))
    (testing "reset fails closed on a malformed base"
      (is (= :workspace-reset-shape-invalid
             (:error/code (sut/reset-to-base! (assoc lease :base-revision "main"))))))))

(deftest f30-shaped-dirty-student-candidate-is-committed-certified-and-idempotent
  (let [{:keys [repo workspaces lake unit]} (fixture)
        lease (:lease (sut/provision! {:unit unit :role :student
                                       :workspace-root workspaces
                                       :substrate-path lake}))
        workspace (:workspace/path lease)
        problem (str workspace "/" (:problem/path lease))]
    (spit problem "theorem p1 : True := by\n  exact True.intro\n")
    (spit (str workspace "/student-notes.txt") "preserve the whole attempt\n")
    (let [first-result (sut/preserve-student-candidate!
                        {:lease lease :attempt-ordinal 3
                         :probe-fn (fn [_] {:exit 0})})
          candidate (:candidate first-result)
          second-result (sut/preserve-student-candidate!
                         {:lease lease :attempt-ordinal 3
                          :probe-fn (fn [_] {:exit 0})})]
      (is (:ok first-result) (pr-str first-result))
      (is (true? (:created-commit? first-result)))
      (is (addressed? candidate :candidate/id))
      (is (= 0 (:candidate/lean-exit candidate)))
      (is (true? (:candidate/worktree-clean? candidate)))
      (is (true? (:candidate/persisted-before-receipt? candidate)))
      (is (= "preserve the whole attempt\n"
             (:out (sh "git" "-C" (str repo) "show"
                       (str (:candidate/ref candidate) ":student-notes.txt")))))
      (is (:ok second-result) (pr-str second-result))
      (is (false? (:created-commit? second-result)))
      (is (= candidate (:candidate second-result))
          "a crash before receipt persistence reuses the exact candidate"))))
