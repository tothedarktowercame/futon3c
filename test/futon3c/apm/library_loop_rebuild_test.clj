(ns futon3c.apm.library-loop-rebuild-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.library-loop-rebuild :as rebuild]))

(def base-files
  {"ConstructionTargets/A.lean" "import Mathlib\n"
   "ConstructionTargets/B.lean" "import ConstructionTargets.A\n"
   "ConstructionTargets/C.lean" "import ConstructionTargets.B\n"
   "problems/t00J02/lean/Main.lean" "import ConstructionTargets.C\n"})

(defn- plan [overrides]
  (rebuild/plan
   (merge {:base-sha "base"
           :head-sha "head"
           :name-status ""
           :porcelain ""
           :files base-files
           :problem-main "problems/t00J02/lean/Main.lean"}
          overrides)))

(deftest committed-and-uncommitted-changes-resolve-transitive-consumers
  (let [result (plan {:name-status "M\tConstructionTargets/A.lean\n"
                      :porcelain (str " M ConstructionTargets/B.lean\n"
                                      " M ConstructionTargets/B.md\n")})]
    (is (= #{"ConstructionTargets.A" "ConstructionTargets.B"}
           (:seed result)))
    (is (= #{"ConstructionTargets.A" "ConstructionTargets.B"
             "ConstructionTargets.C" "problems.t00J02.lean.Main"}
           (:closure result)))
    (is (= [["lake" "build" "ConstructionTargets.A"]
            ["lake" "build" "ConstructionTargets.B"]
            ["lake" "build" "ConstructionTargets.C"]
            ["lake" "env" "lean" "problems/t00J02/lean/Main.lean"]]
           (:commands result)))
    (is (= "base" (get-in result [:inputs :base-sha])))
    (is (= "head" (get-in result [:inputs :head-sha])))))

(deftest new-module-builds-before-rollup-and-main-is-unconditionally-last
  (let [files {"ConstructionTargets.lean"
               "import ConstructionTargets.OrientedSurfacePreimageDuality\n"
               "ConstructionTargets/OrientedSurfacePreimageDuality.lean"
               "import Mathlib\n"
               "problems/t00J02/lean/Main.lean"
               "import ConstructionTargets\n"}
        result (plan {:name-status (str "M\tConstructionTargets.lean\n"
                                        "A\tConstructionTargets/OrientedSurfacePreimageDuality.lean\n")
                      :files files})]
    (is (= [["lake" "build"
             "ConstructionTargets.OrientedSurfacePreimageDuality"]
            ["lake" "build" "ConstructionTargets"]
            ["lake" "env" "lean" "problems/t00J02/lean/Main.lean"]]
           (:commands result)))))

(deftest dependency-order-is-stable-across-chains-and-diamonds
  (let [files {"ConstructionTargets/A.lean" "import Mathlib\n"
               "ConstructionTargets/B.lean" "import ConstructionTargets.A\n"
               "ConstructionTargets/C.lean" "import ConstructionTargets.A\n"
               "ConstructionTargets/D.lean"
               "import ConstructionTargets.B ConstructionTargets.C\n"
               "ConstructionTargets/Z.lean" "import Mathlib\n"
               "problems/t00J02/lean/Main.lean" "import Mathlib\n"}
        result (plan {:name-status (str "M\tConstructionTargets/A.lean\n"
                                        "M\tConstructionTargets/Z.lean\n")
                      :files files})]
    (is (= [["lake" "build" "ConstructionTargets.A"]
            ["lake" "build" "ConstructionTargets.B"]
            ["lake" "build" "ConstructionTargets.C"]
            ["lake" "build" "ConstructionTargets.D"]
            ["lake" "build" "ConstructionTargets.Z"]
            ["lake" "env" "lean" "problems/t00J02/lean/Main.lean"]]
           (:commands result)))))

(deftest problem-main-is-exactly-once-and-last-even-outside-consumer-closure
  (let [result (plan {:name-status "M\tConstructionTargets/A.lean\n"
                      :files (assoc base-files
                                    "problems/t00J02/lean/Main.lean"
                                    "import Mathlib\n")})
        main-command ["lake" "env" "lean" "problems/t00J02/lean/Main.lean"]]
    (is (= main-command (last (:commands result))))
    (is (= 1 (count (filter #{main-command} (:commands result)))))))

(deftest missing-or-malformed-problem-main-fails-closed
  (testing "configured Main must be present in the exact snapshot"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"missing-problem-main"
         (plan {:files (dissoc base-files "problems/t00J02/lean/Main.lean")}))))
  (testing "configured Main must have the canonical problem path shape"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"malformed-problem-main"
         (plan {:problem-main "ConstructionTargets/A.lean"})))))

(deftest rename-contributes-both-module-identities
  (let [files (-> base-files
                  (dissoc "ConstructionTargets/A.lean")
                  (assoc "ConstructionTargets/RenamedA.lean" "import Mathlib\n")
                  (assoc "ConstructionTargets/B.lean"
                         "import ConstructionTargets.RenamedA\n"))
        result (plan {:name-status (str "R100\tConstructionTargets/A.lean\t"
                                        "ConstructionTargets/RenamedA.lean\n")
                      :files files})]
    (is (= #{"ConstructionTargets.A" "ConstructionTargets.RenamedA"}
           (:seed result)))
    (is (some #{["lake" "build" "ConstructionTargets.RenamedA"]}
              (:commands result)))
    (is (= ["lake" "env" "lean" "problems/t00J02/lean/Main.lean"]
           (last (:commands result))))))

(deftest deletions-and-unresolved-imports-fail-closed
  (testing "deleted CT modules require explicit review"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"deleted-construction-target-requires-review"
         (plan {:name-status "D\tConstructionTargets/A.lean\n"
                :files (dissoc base-files "ConstructionTargets/A.lean")}))))
  (testing "a CT import absent from the repository snapshot is refusal"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"unresolved-construction-target-import"
         (plan {:name-status "M\tConstructionTargets/B.lean\n"
                :files (assoc base-files "ConstructionTargets/B.lean"
                              "import ConstructionTargets.Missing\n")})))))

(deftest malformed-construction-target-path-fails-closed
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"unparseable-construction-target-path"
       (plan {:name-status "M\tConstructionTargets/bad path.lean\n"}))))
