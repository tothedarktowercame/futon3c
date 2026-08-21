(ns futon3c.apm.workspace-lifecycle-test
  (:require [clojure.java.shell :as shell]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.workspace-lifecycle :as sut])
  (:import [java.nio.file Files]
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
    (is (:valid? validation) (pr-str (:findings validation)))
    (is (:ok retired))
    (is (:path-absent? retired))
    (is (= (:head validation) (:branch-head retired)))
    (is (= (:receipt retired) (sut/read-receipt (:receipt/path retired))))
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
