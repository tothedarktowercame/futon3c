(ns futon3c.apm.library-loop-adapter-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.library-loop-adapter :as adapter]
            [futon3c.apm.library-loop-checkpoint :as checkpoint]
            [futon3c.apm.library-loop-exec :as exec]
            [futon3c.apm.library-loop-runner :as runner])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (.getCanonicalFile
   (.toFile (Files/createTempDirectory "library-loop-adapter-"
                                       (make-array FileAttribute 0)))))

(defn- sh! [cwd & args]
  (let [result (apply shell/sh (concat args [:dir (str cwd)]))]
    (when-not (zero? (:exit result))
      (throw (ex-info "test-command-failed" (assoc result :args args))))
    (str/trim (:out result))))

(defn- write! [path content]
  (.mkdirs (.getParentFile (io/file path)))
  (spit path content)
  path)

(defn- executable! [path content]
  (write! path content)
  (.setExecutable (io/file path) true)
  (str (io/file path)))

(defn- repository! []
  (let [root (temp-dir)
        workspace (io/file root "workspace")
        trunk (io/file root "trunk")]
    (.mkdirs workspace)
    (sh! workspace "git" "init" "-q")
    (sh! workspace "git" "config" "user.email" "test@example.test")
    (sh! workspace "git" "config" "user.name" "Test")
    (write! (io/file workspace "lakefile.lean") "package Fake\n")
    (write! (io/file workspace "ConstructionTargets/A.lean") "import Mathlib\n")
    (write! (io/file workspace "ConstructionTargets.lean")
            "import ConstructionTargets.A\n")
    (write! (io/file workspace "problems/t00J02/lean/Main.lean")
            "import ConstructionTargets.A\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "base")
    (let [base (sh! workspace "git" "rev-parse" "HEAD")]
      (sh! workspace "git" "branch" "trunk")
      (sh! workspace "git" "worktree" "add" "-q" (str trunk) "trunk")
      {:root root :workspace workspace :trunk trunk :base base})))

(defn- tools! [root]
  (let [bin (io/file root "bin")
        log (io/file root "commands.log")
        codex (io/file bin "codex")
        lake (io/file bin "lake")
        audit (io/file bin "audit")
        status (io/file bin "status")]
    (.mkdirs bin)
    (executable! codex
                 (str "#!/usr/bin/env bash\nset -euo pipefail\n"
                      "printf 'codex cwd=%s args=%s\\n' \"$PWD\" \"$*\" >> '" log "'\n"
                      "printf '\\n-- codex turn\\n' >> ConstructionTargets/A.lean\n"
                      "git add ConstructionTargets/A.lean\n"
                      "git commit -q -m codex-turn\n"))
    (executable! lake
                 (str "#!/usr/bin/env bash\nset -euo pipefail\n"
                      "printf 'lake cwd=%s args=%s\\n' \"$PWD\" \"$*\" >> '" log "'\n"))
    (executable! audit
                 (str "#!/usr/bin/env bash\nset -euo pipefail\n"
                      "base=$1; head=$2; run_dir=$3\nmkdir -p \"$run_dir/audits\"\n"
                      "printf '{:schema 1 :head-sha \"%s\" :modules {\"ConstructionTargets.A\" {:ok? true :head-sha \"%s\" :declarations [a]}}}\\n' \"$head\" \"$head\" > \"$run_dir/audits/$head.edn\"\n"
                      "printf 'audit cwd=%s args=%s\\n' \"$PWD\" \"$*\" >> '" log "'\n"))
    (executable! status
                 (str "#!/usr/bin/env bash\nset -euo pipefail\n"
                      "head=$1; run_dir=$2\nmkdir -p \"$run_dir/status\"\n"
                      "printf '{:schema 1 :candidate-sha \"%s\" :ruling :partial-banked :status-sha \"status-%s\"}\\n' \"$head\" \"$head\" > \"$run_dir/status/$head.edn\"\n"
                      "printf 'status cwd=%s args=%s\\n' \"$PWD\" \"$*\" >> '" log "'\n"))
    {:codex (str codex) :lake (str lake) :audit (str audit)
     :status (str status) :log log}))

(defn- configure! [{:keys [root workspace trunk base] :as repo} tools]
  (let [run-dir (exec/run-dir root "t00J02")]
    (exec/init! root {:problem-id "t00J02" :workspace (str workspace)
                      :base-sha base :head-sha base})
    (runner/atomic-write-edn!
     (io/file run-dir "config.edn")
     {:schema 1
      :trunk-worktree (str trunk)
      :trunk-branch "trunk"
      :codex-command [(:codex tools) "exec" "{prompt-text}"]
      :lake-executable (:lake tools)
      :audit-command [(:audit tools) "{base}" "{head}" "{run-dir}"]
      :status-command [(:status tools) "{head}" "{run-dir}"]
      :checkpoint-cadence 1
      :slate-path (str (io/file root "slate.edn"))})
    (write! (io/file run-dir "standing-goal.md") "Solve exactly one turn.\n")
    (runner/atomic-write-edn!
     (io/file root "slate.edn")
     {:schema 1 :demonstrators [{:problem-id "t00J02" :success? false}]})
    (assoc repo :run-dir run-dir)))

(defn- checkpoint-claim []
  (let [statement "def Producer : Prop := True"]
    {:id :t00J02/producer
     :declaration 'OrientedSurfacePreimageDuality.Producer
     :statement statement
     :statement-digest
     (checkpoint/statement-digest statement)
     :dependencies #{:surface/duality}
     :strength :equivalent
     :reduction-witness "Discharged a concrete premise."
     :next-plan "Construct the remaining producer."}))

(deftest adapter-turn-and-gate-use-exact-workspace-cwd-and-argv
  (let [repo (repository!)
        tools (tools! (:root repo))
        {:keys [root workspace base] :as configured} (configure! repo tools)
        deps (adapter/deps {:root root :problem-id "t00J02"})]
    (exec/resume-one! root "t00J02" deps)
    (let [candidate (sh! workspace "git" "rev-parse" "HEAD")]
      (is (not= base candidate))
      (is (= :gating (:phase (runner/read-state (:run-dir configured)))))
      (exec/resume-one! root "t00J02" deps)
      (is (= :checkpoint-ready
             (:phase (runner/read-state (:run-dir configured)))))
      (let [log (slurp (:log tools))]
        (is (str/includes? log (str "codex cwd=" workspace)))
        (is (str/includes? log "args=exec Solve exactly one turn."))
        (is (str/includes? log (str "lake cwd=" workspace)))
        (is (str/includes? log "build ConstructionTargets.A"))
        (is (str/includes? log "env lean problems/t00J02/lean/Main.lean")))
      (let [state (runner/read-state (:run-dir configured))]
        (is (= candidate (:head-sha state))))
      (let [claim-file (io/file root "checkpoint.edn")]
        (runner/atomic-write-edn! claim-file (checkpoint-claim))
        (let [{:keys [request-path identity]}
              (exec/cli! ["checkpoint" "t00J02" (str claim-file)] {:root root})
              review-file (io/file root "review.edn")]
          (is (.isFile (io/file request-path)))
          (runner/atomic-write-edn!
           review-file {:checkpoint-digest (:checkpoint-digest identity)
                        :obligation-id (:obligation-id identity)
                        :ruling :reduced
                        :rationale "The dependency set decreased."
                        :approved? true})
          (exec/cli! ["apply-review" "t00J02" (str claim-file)
                      (str review-file)] {:root root})
          (is (= :review-pending
                 (:phase (runner/read-state (:run-dir configured))))))))))

(deftest restarted-turn-fails-closed-without-duplicate-codex
  (let [repo (repository!) tools (tools! (:root repo))
        {:keys [root run-dir] :as configured} (configure! repo tools)
        deps (adapter/deps {:root root :problem-id "t00J02"})]
    (runner/begin-action! run-dir :turn)
    (exec/resume-one! root "t00J02" deps)
    (is (= :paused (:phase (runner/read-state run-dir))))
    (is (= :turn-failed (get-in (runner/read-state run-dir)
                                [:pause/finding :type])))
    (is (not (.exists (:log tools))))
    (is (= (:base configured) (sh! (:workspace configured) "git" "rev-parse" "HEAD")))))

(deftest observation-sees-committed-ct-and-missing-audit-never-green
  (let [repo (repository!) tools (tools! (:root repo))
        {:keys [root workspace run-dir]} (configure! repo tools)]
    (write! (io/file workspace "ConstructionTargets/New.lean") "import Mathlib\n")
    (write! (io/file workspace "ConstructionTargets/New.md") "# New seam\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "new-target")
    (let [head (sh! workspace "git" "rev-parse" "HEAD")]
      (runner/write-state! run-dir (assoc (runner/read-state run-dir)
                                         :phase :gating :head-sha head))
      ;; Audit command only emits A evidence; New and its ledger are absent.
      (exec/run-gate! run-dir (adapter/deps {:root root :problem-id "t00J02"}))
      (is (= :turn-ready (:phase (runner/read-state run-dir))))
      (is (= 1 (:consecutive-same-failures (runner/read-state run-dir))))
      (let [receipt (->> (file-seq (io/file run-dir "receipts/gate"))
                         (filter #(.isFile %)) first slurp edn/read-string)]
        (is (= :red (get-in receipt [:result :outcome])))
        (is (= :missing-axiom-audit
               (get-in receipt [:result :registration :finding])))))))

(deftest missing-target-ledger-is-refused-after-valid-audit
  (let [repo (repository!) tools (tools! (:root repo))
        {:keys [root workspace run-dir]} (configure! repo tools)
        audit-all (executable!
                   (io/file root "bin/audit-all")
                   (str "#!/usr/bin/env bash\nset -euo pipefail\n"
                        "base=$1; head=$2; run_dir=$3\n"
                        "mkdir -p \"$run_dir/audits\"\n"
                        "printf '{:schema 1 :head-sha \"%s\" :modules {\"ConstructionTargets.New\" {:ok? true :head-sha \"%s\" :declarations [newTarget]}}}\\n' \"$head\" \"$head\" > \"$run_dir/audits/$head.edn\"\n"))
        config-file (io/file run-dir "config.edn")]
    (runner/atomic-write-edn!
     config-file (assoc (edn/read-string (slurp config-file))
                        :audit-command [audit-all "{base}" "{head}" "{run-dir}"]))
    (write! (io/file workspace "ConstructionTargets/New.lean") "import Mathlib\n")
    (write! (io/file workspace "ConstructionTargets/New.md") "# New seam\n")
    (write! (io/file workspace "ConstructionTargets.lean")
            "import ConstructionTargets.A\nimport ConstructionTargets.New\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "new-target-without-ledger")
    (let [head (sh! workspace "git" "rev-parse" "HEAD")]
      (runner/write-state! run-dir (assoc (runner/read-state run-dir)
                                         :phase :gating :head-sha head))
      (exec/run-gate! run-dir (adapter/deps {:root root :problem-id "t00J02"}))
      (let [receipt (->> (file-seq (io/file run-dir "receipts/gate"))
                         (filter #(.isFile %)) first slurp edn/read-string)]
        (is (= :missing-target-ledger-record
               (get-in receipt [:result :registration :finding])))))))

(defn- bank-ready! [run-dir candidate]
  (let [state (runner/read-state run-dir)
        identity {:schema 1 :problem-id "t00J02" :turn (:turn state)
                  :checkpoint (:checkpoint state) :head-sha candidate
                  :obligation-id :t00J02/producer
                  :checkpoint-digest (apply str (repeat 64 "a"))}]
    (runner/write-state! run-dir
                         (assoc state :phase :review-pending
                                :head-sha candidate
                                :obligation/id :t00J02/producer
                                :pending-checkpoint identity
                                :intent nil))))

(deftest exact-bank-fast-forwards-no-push-and-maps-status
  (let [repo (repository!) tools (tools! (:root repo))
        {:keys [root workspace trunk run-dir]} (configure! repo tools)]
    (write! (io/file workspace "ConstructionTargets/A.lean")
            "import Mathlib\n-- candidate\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "candidate")
    (let [candidate (sh! workspace "git" "rev-parse" "HEAD")]
      (bank-ready! run-dir candidate)
      (exec/run-bank! run-dir (adapter/deps {:root root :problem-id "t00J02"}))
      (is (= candidate (sh! trunk "git" "rev-parse" "HEAD")))
      (is (= :turn-ready (:phase (runner/read-state run-dir))))
      (is (= :partial-banked
             (get-in (edn/read-string (slurp (io/file root "slate.edn")))
                     [:demonstrators 0 :last-ruling])))
      (is (not (str/includes? (slurp (:log tools)) "push"))))))

(deftest bank-refuses-trunk-race-before-mutation
  (let [repo (repository!) tools (tools! (:root repo))
        {:keys [root workspace trunk run-dir]} (configure! repo tools)]
    (write! (io/file workspace "ConstructionTargets/A.lean")
            "import Mathlib\n-- candidate\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "candidate")
    (let [candidate (sh! workspace "git" "rev-parse" "HEAD")]
      (write! (io/file trunk "race.txt") "race\n")
      (sh! trunk "git" "add" ".")
      (sh! trunk "git" "-c" "user.email=test@example.test"
           "-c" "user.name=Test" "commit" "-q" "-m" "race")
      (bank-ready! run-dir candidate)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"bank-trunk-race"
                            (exec/run-bank!
                             run-dir (adapter/deps {:root root
                                                    :problem-id "t00J02"})))))))

(deftest bank-restart-after-fast-forward-runs-status-only
  (let [repo (repository!) tools (tools! (:root repo))
        {:keys [root workspace trunk run-dir]} (configure! repo tools)]
    (write! (io/file workspace "ConstructionTargets/A.lean")
            "import Mathlib\n-- candidate\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "candidate")
    (let [candidate (sh! workspace "git" "rev-parse" "HEAD")]
      (bank-ready! run-dir candidate)
      (runner/begin-action! run-dir :bank)
      ;; Simulate process death after the exact mutation but before status.
      (sh! trunk "git" "merge" "--ff-only" candidate)
      (exec/run-bank! run-dir (adapter/deps {:root root :problem-id "t00J02"}))
      (is (= :turn-ready (:phase (runner/read-state run-dir))))
      (let [log (slurp (:log tools))]
        (is (str/includes? log (str "status cwd=" trunk)))
        (is (not (str/includes? log "lake cwd=")))))))
