(ns futon3c.apm.workspace-build-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.workspace-build :as sut]))

(defn- temp-workspace [files]
  (let [root (java.nio.file.Files/createTempDirectory
              "workspace-build-"
              (make-array java.nio.file.attribute.FileAttribute 0))]
    (doseq [[path content] files]
      (let [file (.toFile (.resolve root path))]
        (.mkdirs (.getParentFile file))
        (spit file content)))
    (str root)))

(deftest derives-only-checkout-local-imports
  (let [workspace
        (temp-workspace
         {"problems/p/lean/Main.lean"
          (str "import Mathlib\n"
               "import ConstructionTargets.YoungConvolution\n"
               "import ConstructionTargets.RadialMajorantAE\n"
               "import YoungL2\n")
          "ConstructionTargets/YoungConvolution.lean" ""
          "ConstructionTargets/RadialMajorantAE.lean" ""
          "YoungL2.lean" ""})]
    (is (= ["ConstructionTargets.YoungConvolution"
            "ConstructionTargets.RadialMajorantAE"
            "YoungL2"]
           (sut/local-imports workspace "problems/p/lean/Main.lean")))))

(deftest bootstrap-builds-derived-modules-and-preserves-missing-import-for-probe
  (let [workspace
        (temp-workspace
         {"problems/p/lean/Main.lean"
          (str "import Mathlib\n"
               "import ConstructionTargets.A -- local\n"
               "import DoesNotExist\n")
          "ConstructionTargets/A.lean" ""})
        calls (atom [])
        result (sut/bootstrap!
                {:workspace/path workspace
                 :problem/path "problems/p/lean/Main.lean"}
                (fn [dir argv]
                  (swap! calls conj [dir argv])
                  {:exit 0}))]
    (is (= {:ok true :built/modules ["ConstructionTargets.A"]} result))
    (is (= [[workspace ["lake" "build" "ConstructionTargets.A"]]] @calls))))

(deftest failed-local-build-is-typed
  (let [workspace
        (temp-workspace
         {"problems/p/lean/Main.lean" "import Local.Module\n"
          "Local/Module.lean" ""})
        result (sut/bootstrap!
                {:workspace/path workspace
                 :problem/path "problems/p/lean/Main.lean"}
                (fn [_ _] {:exit 1 :err "broken"}))]
    (is (= :workspace-bootstrap-failed (:error/code result)))
    (is (= "Local.Module" (:module result)))))

(deftest probe-builds-an-unbuilt-import-before-elaboration
  (let [workspace
        (temp-workspace
         {"problems/p/lean/Main.lean" "import Local.Dependency\ntheorem ok : True := by trivial\n"
          "Local/Dependency.lean" "theorem dependency : True := by trivial\n"})
        built? (atom false)
        calls (atom [])
        result (sut/probe!
                {:workspace/path workspace
                 :problem/path "problems/p/lean/Main.lean"}
                (fn [_ argv]
                  (swap! calls conj argv)
                  (case (first (drop 1 argv))
                    "build" (do (reset! built? true) {:exit 0 :out "built" :err ""})
                    "env" (if @built?
                            {:exit 0 :out "" :err ""}
                            {:exit 1 :out "" :err "unknown module"}))))]
    (is (zero? (:exit result)))
    (is (= ["Local.Dependency"] (:built/modules result)))
    (is (= [["lake" "build" "Local.Dependency"]
            ["lake" "env" "lean" "problems/p/lean/Main.lean"]]
           @calls))))

(deftest probe-preserves-a-genuine-elaboration-failure
  (let [workspace
        (temp-workspace
         {"problems/p/lean/Main.lean"
          "import Local.Dependency\ntheorem broken : False := by trivial\n"
          "Local/Dependency.lean" "theorem dependency : True := by trivial\n"})
        result (sut/probe!
                {:workspace/path workspace
                 :problem/path "problems/p/lean/Main.lean"}
                (fn [_ argv]
                  (if (= "build" (second argv))
                    {:exit 0 :out "built" :err ""}
                    {:exit 1 :out "" :err "type mismatch"})))]
    (is (= 1 (:exit result)))
    (is (= "type mismatch" (:err result)))
    (is (= ["Local.Dependency"] (:built/modules result)))))
