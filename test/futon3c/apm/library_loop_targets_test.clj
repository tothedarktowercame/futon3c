(ns futon3c.apm.library-loop-targets-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.library-loop-targets :as targets]))

(def module "ConstructionTargets.NewTarget")
(def lean-path "ConstructionTargets/NewTarget.lean")

(defn- input [overrides]
  (merge {:head-sha "candidate"
          :problem-id "t00J02"
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

(defn- delayed-provenance [overrides]
  (merge {:schema 1
          :problem-id "t00J02"
          :module module
          :created-turn 19
          :turn-receipt-id (apply str (repeat 64 "a"))
          :turn-head-sha (apply str (repeat 40 "b"))
          :creation-commit-sha (apply str (repeat 40 "c"))
          :first-observed-at-turn? true}
         overrides))

(deftest delayed-correction-retains-proven-creation-turn
  (let [provenance {module (delayed-provenance {})}
        result (targets/check
                (input {:targets [{:module module :created-turn 19
                                   :status :active
                                   :obligation :t00J02/producer}]
                        :target-provenance provenance}))]
    (is (= :green (:outcome result)))
    (is (string? (get-in result [:snapshot :target-provenance-digest])))
    (is (not= (get-in result [:snapshot :target-provenance-digest])
              (get-in (targets/check (input {}))
                      [:snapshot :target-provenance-digest])))))

(deftest future-and-fabricated-created-turns-fail-closed
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"target-created-turn-in-future"
       (targets/check
        (input {:targets [{:module module :created-turn 21 :status :active
                           :obligation :t00J02/producer}]}))))
  (doseq [[label provenance]
          [[:missing nil]
           [:wrong-problem (delayed-provenance {:problem-id "other"})]
           [:wrong-module (delayed-provenance {:module "ConstructionTargets.Other"})]
           [:wrong-turn (delayed-provenance {:created-turn 18})]
           [:malformed (delayed-provenance {:turn-receipt-id "hand-written"})]
           [:not-first (delayed-provenance {:first-observed-at-turn? false})]]]
    (testing (name label)
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"target-created-turn-provenance-invalid"
           (targets/check
            (input {:targets [{:module module :created-turn 19 :status :active
                               :obligation :t00J02/producer}]
                    :target-provenance (if provenance {module provenance} {})})))))))

(defn- rollup-with-consumers [rows status]
  (str "import ConstructionTargets.NewTarget\n"
       "## Status\n\n" status "\n\n"
       "The consumer column is DERIVED from problem Main files.\n\n"
       "| Module | Consumers (measured) |\n|---|---|\n"
       (str/join "\n" rows) "\n"))

(deftest rollup-derived-consumers-require-exact-main-evidence
  (let [rollup (rollup-with-consumers
                ["| `NewTarget` | t00J02 |"]
                "All modules verified at committed HEAD.")
        valid-files (assoc (:files (input {}))
                           "ConstructionTargets.lean" rollup
                           "problems/t00J02/lean/Main.lean"
                           "import ConstructionTargets.NewTarget\n")]
    (is (= :green (:outcome (targets/check (input {:files valid-files})))))
    (doseq [[label files]
            [[:false-reference
              (assoc valid-files "problems/t00J02/lean/Main.lean" "import Mathlib\n")]
             [:missing-main (dissoc valid-files
                                    "problems/t00J02/lean/Main.lean")]]]
      (testing (name label)
        (try
          (targets/check (input {:files files}))
          (is false "expected consumer evidence refusal")
          (catch clojure.lang.ExceptionInfo ex
            (is (= :construction-target-consumer-evidence-mismatch
                   (:finding (ex-data ex))))
            (is (= {:module "ConstructionTargets.NewTarget"
                    :problem-id "t00J02"
                    :main-path "problems/t00J02/lean/Main.lean"}
                   (:diagnostic (ex-data ex))))))))))

(deftest rollup-consumer-refusal-order-is-deterministic
  (let [rollup (rollup-with-consumers
                ["| `ZTarget` | t02A02, t01A01 |"
                 "| `ATarget` | t03J03 |"]
                "All modules verified at committed HEAD.")
        files (assoc (:files (input {})) "ConstructionTargets.lean" rollup)]
    (dotimes [_ 2]
      (try
        (targets/check (input {:files files}))
        (is false "expected consumer evidence refusal")
        (catch clojure.lang.ExceptionInfo ex
          (is (= {:module "ConstructionTargets.ATarget"
                  :problem-id "t03J03"
                  :main-path "problems/t03J03/lean/Main.lean"}
                 (:diagnostic (ex-data ex)))))))))

(deftest intention-only-production-header-fails-closed
  (let [rollup (rollup-with-consumers
                [] "Every imported module is intended to remain production.")]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"construction-target-production-status-non-evidentiary"
         (targets/check
          (input {:files (assoc (:files (input {}))
                                "ConstructionTargets.lean" rollup)}))))))

(deftest live-rollup-shape-refuses-false-derived-consumer
  (let [rollup (rollup-with-consumers
                ["| `OrientedSurfacePreimageDuality` | t00J02 (elected closure in progress) |"
                 "| `TransversePreimageDuality` | t00J02 (elected closure in progress) |"
                 "| `DiskBoundedPreimageDuality` | t00J02 (elected closure in progress) |"]
                "Every imported module is intended to remain production.")]
    (try
      (targets/check
       (input {:files (assoc (:files (input {}))
                             "ConstructionTargets.lean" rollup
                             "problems/t00J02/lean/Main.lean" "import Mathlib\n")}))
      (is false "expected live consumer mismatch")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :construction-target-consumer-evidence-mismatch
               (:finding (ex-data ex))))
        (is (= "ConstructionTargets.DiskBoundedPreimageDuality"
               (get-in (ex-data ex) [:diagnostic :module])))))))
