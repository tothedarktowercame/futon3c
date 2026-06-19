(ns futon3c.scripts.reachable-from-boot-test
  "Tests for the reachable-from-boot pre-commit hook siblings.

   Mission: M-reachable-from-boot (futon3c/holes/missions/)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]))

(def repo-root "/home/joe/code/futon3c")

(defn- temp-dir
  [prefix]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      prefix
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit dir)
    dir))

(defn- write-file!
  [^java.io.File repo relative-path content]
  (let [f (io/file repo relative-path)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    (.getPath f)))

(defn- init-git-repo!
  [^java.io.File dir]
  (let [{:keys [exit err]} (shell/sh "git" "init" (.getPath dir))]
    (is (zero? exit) (str "git init failed: " err))
    dir))

(defn- copy-into-repo!
  [^java.io.File repo relative-path]
  (let [src (io/file repo-root relative-path)
        dst (io/file repo relative-path)]
    (.mkdirs (.getParentFile dst))
    (io/copy src dst)
    (.setExecutable dst true)
    (.getPath dst)))

(defn- stage-file!
  [^java.io.File repo relative-path]
  (shell/sh "git" "-C" (.getPath repo) "add" relative-path))

(defn- run-script!
  [^java.io.File repo relative-path]
  (shell/sh "bash" relative-path :dir (.getPath repo)))

(defn- minimal-futon3c-layout!
  [^java.io.File repo]
  (doseq [relative-path ["dev/futon3c/dev/bootstrap.clj"
                         "dev/futon3c/dev.clj"
                         "src/futon3c/evidence/store.clj"
                         "src/futon3c/evidence/backend.clj"
                         "src/futon3c/evidence/xtdb_backend.clj"
                         "src/futon3c/logic/probe.clj"
                         "src/futon3c/logic/probe_taps.clj"
                         "src/futon3c/logic/archaeology.clj"
                         "src/futon3c/logic/locus.clj"
                         "src/futon3c/agency/registry.clj"]]
    (write-file! repo relative-path ";; placeholder\n"))
  repo)

(deftest family-check-fns-hook-clean-and-violation
  (let [repo (-> (temp-dir "reachable-family-check-fns-")
                 (init-git-repo!)
                 (minimal-futon3c-layout!))]
    (copy-into-repo! repo "scripts/check-reachable-from-boot-family-check-fns.sh")
    (testing "clean tree passes"
      (let [{:keys [exit err]} (run-script! repo "scripts/check-reachable-from-boot-family-check-fns.sh")]
        (is (zero? exit) (str "expected clean pass, got err=" err))))
    (testing "staged ad hoc registration fails"
      (write-file! repo "src/futon3c/some_bad.clj"
                   "(ns futon3c.some-bad)\n(probe/register-family-check! :x identity)\n")
      (stage-file! repo "src/futon3c/some_bad.clj")
      (let [{:keys [exit out]} (run-script! repo "scripts/check-reachable-from-boot-family-check-fns.sh")]
        (is (= 1 exit))
        (is (re-find #"reachable-from-boot/family-check-fns VIOLATION" out))))))

(deftest agent-registry-hook-clean-and-violation
  (let [repo (-> (temp-dir "reachable-agent-registry-")
                 (init-git-repo!)
                 (minimal-futon3c-layout!))]
    (copy-into-repo! repo "scripts/check-reachable-from-boot-agent-registry.sh")
    (testing "clean tree passes"
      (let [{:keys [exit err]} (run-script! repo "scripts/check-reachable-from-boot-agent-registry.sh")]
        (is (zero? exit) (str "expected clean pass, got err=" err))))
    (testing "staged direct !registry mutation fails"
      (write-file! repo "src/futon3c/some_bad.clj"
                   "(ns futon3c.some-bad)\n(reset! futon3c.agency.registry/!registry {})\n")
      (stage-file! repo "src/futon3c/some_bad.clj")
      (let [{:keys [exit out]} (run-script! repo "scripts/check-reachable-from-boot-agent-registry.sh")]
        (is (= 1 exit))
        (is (re-find #"reachable-from-boot/agent-registry VIOLATION" out))))))

(deftest dev-evidence-store-hook-clean-and-violation
  (let [repo (-> (temp-dir "reachable-dev-evidence-store-")
                 (init-git-repo!)
                 (minimal-futon3c-layout!))]
    (copy-into-repo! repo "scripts/check-reachable-from-boot-dev-evidence-store.sh")
    (testing "clean tree passes"
      (let [{:keys [exit err]} (run-script! repo "scripts/check-reachable-from-boot-dev-evidence-store.sh")]
        (is (zero? exit) (str "expected clean pass, got err=" err))))
    (testing "staged direct !evidence-store mutation fails"
      (write-file! repo "src/futon3c/some_bad.clj"
                   "(ns futon3c.some-bad)\n(reset! futon3c.dev/!evidence-store nil)\n")
      (stage-file! repo "src/futon3c/some_bad.clj")
      (let [{:keys [exit out]} (run-script! repo "scripts/check-reachable-from-boot-dev-evidence-store.sh")]
        (is (= 1 exit))
        (is (re-find #"reachable-from-boot/dev-evidence-store VIOLATION" out))))))

(deftest pre-commit-wrapper-runs-all-reachable-from-boot-checks
  (let [repo (-> (temp-dir "reachable-wrapper-")
                 (init-git-repo!)
                 (minimal-futon3c-layout!))]
    (doseq [relative-path ["scripts/check-autostash-obsolescence.sh"
                           "scripts/check-reachable-from-boot.sh"
                           "scripts/check-reachable-from-boot-family-check-fns.sh"
                           "scripts/check-reachable-from-boot-agent-registry.sh"
                           "scripts/check-reachable-from-boot-dev-evidence-store.sh"
                           "scripts/check-pre-commit-wrapper.sh"]]
      (copy-into-repo! repo relative-path))
    (testing "clean tree passes"
      (let [{:keys [exit err]} (run-script! repo "scripts/check-pre-commit-wrapper.sh")]
        (is (zero? exit) (str "expected wrapper clean pass, got err=" err))))
    (testing "staged sibling violation fails the wrapper"
      (write-file! repo "src/futon3c/some_bad.clj"
                   "(ns futon3c.some-bad)\n(reset! futon3c.logic.probe/family-check-fns {})\n")
      (stage-file! repo "src/futon3c/some_bad.clj")
      (let [{:keys [exit out]} (run-script! repo "scripts/check-pre-commit-wrapper.sh")]
        (is (= 1 exit))
        (is (re-find #"reachable-from-boot/family-check-fns VIOLATION" out))))))
