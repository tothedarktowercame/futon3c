(ns futon3c.apm.library-loop-tools-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.library-loop-adapter :as adapter]
            [futon3c.apm.library-loop-runner :as runner]
            [futon3c.apm.library-loop-tools :as tools])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (.getCanonicalFile
   (.toFile (Files/createTempDirectory "library-loop-tools-"
                                       (make-array FileAttribute 0)))))

(defn- sh! [cwd & args]
  (let [result (apply shell/sh (concat args [:dir (str cwd)]))]
    (when-not (zero? (:exit result))
      (throw (ex-info "test-command-failed" (assoc result :args args))))
    (str/trim (:out result))))

(defn- write! [path content]
  (.mkdirs (.getParentFile (io/file path)))
  (spit path content))

(defn- repository! []
  (let [root (temp-dir)
        workspace (io/file root "apm-lean")
        run-dir (io/file root "run")]
    (.mkdirs workspace)
    (sh! workspace "git" "init" "-q")
    (sh! workspace "git" "config" "user.email" "test@example.test")
    (sh! workspace "git" "config" "user.name" "Test")
    (write! (io/file workspace "lakefile.lean") "package Fake\n")
    (write! (io/file workspace "ConstructionTargets/A.lean")
            "import Mathlib\ntheorem oldTarget : True := by trivial\n")
    (write! (io/file workspace "problems/t01A03/lean/Main.lean")
            "import ConstructionTargets.A\ntheorem main : True := by sorry\n")
    (write! (io/file workspace "problems/t01A03/status.json")
            "{\"classification\":\"partial\",\"lean\":{\"sorry_count_total\":1}}\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "base")
    (let [base (sh! workspace "git" "rev-parse" "HEAD")]
      (write! (io/file workspace "ConstructionTargets/A.lean")
              "import Mathlib\ntheorem promoted : True := by trivial\n")
      (write! (io/file workspace "problems/t01A03/targets.edn")
              "[{:module \"ConstructionTargets.A\" :created-turn 1 :status :active :obligation :t01A03/circle-h1 :declarations [promoted]}]\n")
      (sh! workspace "git" "add" ".")
      (sh! workspace "git" "commit" "-q" "-m" "candidate")
      (let [head (sh! workspace "git" "rev-parse" "HEAD")]
        (runner/write-state!
         run-dir (runner/initial-state
                  {:problem-id "t01A03" :workspace (str workspace)
                   :base-sha base :head-sha head}))
        (runner/atomic-write-edn!
         (io/file run-dir "config.edn")
         {:schema 1 :trunk-worktree (str workspace)
          :trunk-branch (sh! workspace "git" "symbolic-ref" "--short" "HEAD")})
        {:root root :workspace workspace :run-dir run-dir
         :base base :head head}))))

(defn- injected-runner [lean-output]
  (fn [cwd argv]
    (if (= "lake" (first argv))
      {:exit 0 :stdout lean-output :stderr "" :argv argv :cwd (str cwd)}
      (adapter/run-process cwd argv))))

(deftest audit-is-state-bound-and-refuses-sorry-axioms
  (let [{:keys [workspace run-dir base head]} (repository!)
        evidence (tools/audit! base head run-dir
                               (injected-runner
                                "'promoted' depends on axioms: [propext]\n")
                               workspace)]
    (is (= {:schema 1 :head-sha head
            :modules {"ConstructionTargets.A"
                      {:ok? true :head-sha head
                       :declarations ['promoted]}}}
           evidence))
    (is (= evidence
           (edn/read-string
            (slurp (io/file run-dir "audits" (str head ".edn"))))))
    (testing "real Lean output containing sorryAx never writes green evidence"
      (.delete (io/file run-dir "audits" (str head ".edn")))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"axiom-audit-sorry-axiom"
           (tools/audit! base head run-dir
                         (injected-runner
                          "'promoted' depends on axioms: [sorryAx]\n")
                         workspace)))
      (is (not (.exists (io/file run-dir "audits" (str head ".edn"))))))))

(deftest audit-refuses-missing-ledger-declaration-authority
  (let [{:keys [workspace run-dir base head]} (repository!)]
    (write! (io/file workspace "problems/t01A03/targets.edn")
            "[{:module \"ConstructionTargets.A\" :created-turn 1 :status :active :obligation :t01A03/circle-h1}]\n")
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"audit-ledger-declarations-invalid"
         (tools/audit! base head run-dir (injected-runner "") workspace)))))

(deftest status-derives-only-state-bound-partial-or-closed
  (let [{:keys [workspace run-dir head]} (repository!)
        partial-output
        "problems/t01A03/lean/Main.lean:2:9: warning: declaration uses `sorry`\n"
        partial (tools/status! head run-dir (injected-runner partial-output)
                               workspace)]
    (is (= :partial-banked (:ruling partial)))
    (is (= head (:candidate-sha partial)))
    (is (= partial
           (edn/read-string
            (slurp (io/file run-dir "status" (str head ".edn"))))))
    (testing "zero sorries cannot manufacture closure from partial status"
      (.delete (io/file run-dir "status" (str head ".edn")))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"status-sorry-count-mismatch"
           (tools/status! head run-dir (injected-runner "") workspace))))))

(deftest status-accepts-solved-only-when-main-elaborates-without-sorry
  (let [{:keys [workspace run-dir]} (repository!)]
    (write! (io/file workspace "problems/t01A03/lean/Main.lean")
            "import ConstructionTargets.A\ntheorem main : True := by trivial\n")
    (write! (io/file workspace "problems/t01A03/status.json")
            "{\"classification\":\"solved\",\"lean\":{\"sorry_count_total\":0}}\n")
    (sh! workspace "git" "add" ".")
    (sh! workspace "git" "commit" "-q" "-m" "solved")
    (let [head (sh! workspace "git" "rev-parse" "HEAD")
          state (runner/read-state run-dir)]
      (runner/write-state! run-dir (assoc state :head-sha head))
      (is (= :closed (:ruling
                      (tools/status! head run-dir (injected-runner "")
                                     workspace)))))))
