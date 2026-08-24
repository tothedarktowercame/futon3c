(ns futon3c.apm.library-loop-targets-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.library-loop-targets :as targets]))

(def module "ConstructionTargets.NewTarget")
(def lean-path "ConstructionTargets/NewTarget.lean")

(defn- input [overrides]
  (merge {:head-sha "candidate"
          :turn 20
          :changes [{:kind "A" :path lean-path}]
          :files {lean-path "def newTarget : True := trivial\n"
                  "ConstructionTargets/NewTarget.md" "# Seam\n"
                  "ConstructionTargets.lean" "import ConstructionTargets.NewTarget\n"
                  "ConstructionTargets/PARTIAL.md" "# Partial construction targets\n"}
          :targets [{:module module :created-turn 20 :status :active
                     :obligation :t00J02/producer}]
          :axiom-audits {module {:ok? true :head-sha "candidate"
                                 :declarations ['newTarget]}}}
         overrides))

(deftest new-target-rollup-registration-passes-and-is-snapshot-bound
  (let [result (targets/check (input {}))]
    (is (= :green (:outcome result)))
    (is (= #{module} (:checked-modules result)))
    (is (= "candidate" (get-in result [:snapshot :head-sha])))
    (is (string? (get-in result [:snapshot :files-digest])))))

(deftest deliberate-partial-registration-passes
  (let [result (targets/check
                (input {:files {lean-path "def newTarget : True := by sorry\n"
                                "ConstructionTargets/NewTarget.md" "# Seam\n"
                                "ConstructionTargets.lean" "import Mathlib\n"
                                "ConstructionTargets/PARTIAL.md"
                                (str "| Module | Lean `sorry` count | Remaining boundary |\n"
                                     "|---|---:|---|\n"
                                     "| `ConstructionTargets.NewTarget` | 1 | Producer |\n")}}))]
    (is (= :green (:outcome result)))))

(deftest missing-or-ambiguous-new-target-artifacts-fail-closed
  (testing "paired seam document"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"missing-target-seam-doc"
         (targets/check
          (input {:files {lean-path "def newTarget : True := trivial\n"
                          "ConstructionTargets.lean"
                          "import ConstructionTargets.NewTarget\n"}})))))
  (testing "both roll-up and PARTIAL is ambiguous"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"ambiguous-target-disposition"
         (targets/check
          (input {:files {lean-path "def newTarget : True := trivial\n"
                          "ConstructionTargets/NewTarget.md" "# Seam\n"
                          "ConstructionTargets.lean"
                          "import ConstructionTargets.NewTarget\n"
                          "ConstructionTargets/PARTIAL.md"
                          "| `ConstructionTargets.NewTarget` | 0 | unclear |\n"}})))))
  (testing "targets ledger record"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"missing-target-ledger-record"
                          (targets/check (input {:targets []})))))
  (testing "duplicate ledger records are ambiguous"
    (let [row {:module module :created-turn 20 :status :active
               :obligation :t00J02/producer}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ambiguous-target-record"
                            (targets/check (input {:targets [row row]})))))))

(deftest modified-target-still-requires-current-axiom-audit
  (let [modified (input {:changes [{:kind "M" :path lean-path}]
                         :files {lean-path "def newTarget : True := trivial\n"}
                         :targets []})]
    (is (= :green (:outcome (targets/check modified))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"missing-axiom-audit"
                          (targets/check (assoc modified :axiom-audits {}))))))
