(ns futon3c.scripts.reachable-from-boot-test
  "Tests for the reachable-from-boot pre-commit hook siblings.

   Mission: M-reachable-from-boot (futon3c/holes/missions/)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]))

;; Checkout-relative (C4, 2026-10-31): the tests must exercise THIS
;; checkout's scripts, so a worktree run validates the worktree's guard
;; rather than silently copying the canonical checkout's copy.
;; *file* is test/futon3c/scripts/reachable_from_boot_test.clj relative to
;; the classpath, so four parents up from the file is the checkout root.
(def repo-root (-> (io/resource *file*)
                   (io/file)
                   (.getParentFile)  ; scripts/
                   (.getParentFile)  ; futon3c/
                   (.getParentFile)  ; test/
                   (.getParentFile)  ; checkout root
                   (.getPath)))

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
                         "src/futon3c/evidence/futon1b_backend.clj"
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

(deftest evidence-store-guard-fully-qualified-alias-and-clean-controls
  ;; C4 (2026-10-31): the evidence-store guard's old character class
  ;; [a-z./-] omitted digits, so a fully-qualified mutation —
  ;; (reset! futon3c.evidence.store/!store {}) — passed the guard while
  ;; the alias form refused. The real guard entry point (the pre-commit
  ;; script, driven over a staged file in a fresh git repo) must refuse
  ;; BOTH forms and pass a clean control.
  (let [repo (-> (temp-dir "reachable-evidence-store-c4-")
                 (init-git-repo!)
                 (minimal-futon3c-layout!))]
    (copy-into-repo! repo "scripts/check-reachable-from-boot.sh")
    (testing "clean tree passes"
      (let [{:keys [exit err]} (run-script! repo "scripts/check-reachable-from-boot.sh")]
        (is (zero? exit) (str "expected clean pass, got err=" err))))
    (testing "staged fully-qualified !store mutation is refused (the C4 loophole)"
      (write-file! repo "src/futon3c/some_bad.clj"
                   "(ns futon3c.some-bad)\n(reset! futon3c.evidence.store/!store {})\n")
      (stage-file! repo "src/futon3c/some_bad.clj")
      (let [{:keys [exit out]} (run-script! repo "scripts/check-reachable-from-boot.sh")]
        (is (= 1 exit))
        (is (re-find #"reachable-from-boot/evidence-store VIOLATION" out))))
    (testing "staged alias-form !store mutation is still refused"
      (write-file! repo "src/futon3c/some_bad2.clj"
                   "(ns futon3c.some-bad2)\n(require '[futon3c.evidence.store :as estore])\n(swap! estore/!store assoc :x 1)\n")
      (stage-file! repo "src/futon3c/some_bad2.clj")
      (let [{:keys [exit out]} (run-script! repo "scripts/check-reachable-from-boot.sh")]
        (is (= 1 exit))
        (is (re-find #"reachable-from-boot/evidence-store VIOLATION" out))))
    (testing "an ordinary staged Clojure file is not flagged (clean control, fresh repo)"
      (let [repo2 (-> (temp-dir "reachable-evidence-store-c4-clean-")
                      (init-git-repo!)
                      (minimal-futon3c-layout!))]
        (copy-into-repo! repo2 "scripts/check-reachable-from-boot.sh")
        (write-file! repo2 "src/futon3c/innocent.clj"
                     "(ns futon3c.innocent)\n(defn f [x] (reset! (atom x) 3))\n(reset! some-other/!counter 0)\n")
        (stage-file! repo2 "src/futon3c/innocent.clj")
        (let [{:keys [exit err]} (run-script! repo2 "scripts/check-reachable-from-boot.sh")]
          (is (zero? exit) (str "ordinary staged file must pass, got err=" err)))))))

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
