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

(defn- injected-runner
  ([lean-output] (injected-runner lean-output (atom [])))
  ([lean-output calls]
   (let [built (atom #{})]
     (fn [cwd argv]
       (if (= "lake" (first argv))
         (do
           (swap! calls conj argv)
           (cond
             (= "build" (second argv))
             (do (swap! built conj (nth argv 2))
                 {:exit 0 :stdout "built\n" :stderr ""
                  :argv argv :cwd (str cwd)})

             (and (= ["env" "lean"] (subvec argv 1 3))
                  (or (not (str/includes? (last argv) "audit-inputs"))
                      (seq @built)))
             {:exit 0 :stdout lean-output :stderr ""
              :argv argv :cwd (str cwd)}

             :else
             {:exit 1 :stdout "" :stderr "missing olean"
              :argv argv :cwd (str cwd)}))
         (adapter/run-process cwd argv))))))

(deftest audit-builds-first-time-target-before-state-bound-axiom-check
  (let [{:keys [workspace run-dir base head]} (repository!)
        calls (atom [])
        evidence (tools/audit! base head run-dir
                               (injected-runner
                                "'promoted' depends on axioms: [propext]\n"
                                calls)
                               workspace)]
    (is (= ["lake" "build" "ConstructionTargets.A"] (first @calls)))
    (is (= ["lake" "env" "lean"] (subvec (second @calls) 0 3)))
    (is (= ['promoted]
           (get-in evidence [:modules "ConstructionTargets.A" :declarations])))
    (is (= ["lake" "build" "ConstructionTargets.A"]
           (get-in evidence [:modules "ConstructionTargets.A" :build :argv])))
    (is (= 0 (get-in evidence
                     [:modules "ConstructionTargets.A" :audit :exit])))
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
