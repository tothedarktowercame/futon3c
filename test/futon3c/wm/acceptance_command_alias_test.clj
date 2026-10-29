(ns futon3c.wm.acceptance-command-alias-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.wm.meta-self-heal-adapters :as sut]))

(defn- file-set [root]
  (let [root-file (io/file root)]
    (if (.exists root-file)
      (set (map (juxt #(.getCanonicalPath ^java.io.File %)
                      #(.length ^java.io.File %)
                      #(.lastModified ^java.io.File %))
                (filter #(.isFile ^java.io.File %) (file-seq root-file))))
      #{})))

(deftest real-acceptance-process-isolates-strategic-selection-stores
  (let [repo "/home/joe/worktrees/futon2/acceptance-store-isolation"
        namespace "futon2.aif.acceptance-store-isolation-probe-test"
        path "test/futon2/aif/acceptance_store_isolation_probe_test.clj"
        text (slurp (io/file repo path))
        production-phase-root "/home/joe/code/futon2/data/wm-phase-replay-inputs"
        logs (.toFile (java.nio.file.Files/createTempDirectory
                       "acceptance-isolation-logs-"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        before (file-set production-phase-root)
        result (binding [sut/*acceptance-repository* repo
                         sut/*acceptance-runs-root* (.getPath logs)]
                 (#'sut/run-acceptance! 60000
                  {:id "acceptance-store-isolation"
                   :command ["clojure" "-M:test" "-n" namespace]
                   :source {:namespace namespace :path path :text text}}))]
    (is (= [:ran :passed 0] ((juxt :status :outcome :exit) result)) (pr-str result))
    (is (= :isolated (get-in result [:store-isolation :status])))
    (is (true? (get-in result [:store-isolation :removed?])))
    (is (= before (file-set production-phase-root))
        "the real strategic-selection process must not change production replay inputs")
    (doseq [file (reverse (file-seq logs))] (io/delete-file file true))))

(deftest alias-owns-the-runner-and-port-runs-only-requested-namespace
  (let [command ["clojure" "-M:test" "-n" "futon2.aif.wm.meta-injury-04e1-acceptance-test"]
        deps (edn/read-string (slurp "/home/joe/code/futon2/deps.edn"))
        root (.toFile (java.nio.file.Files/createTempDirectory "m2a-port" (make-array java.nio.file.attribute.FileAttribute 0)))
        logs (io/file root "logs")
        target "fixture.m2a-only-test"
        text "(ns fixture.m2a-only-test (:require [clojure.test :refer [deftest is]]))\n(deftest passes (println \"M2A-ONLY-NAMESPACE\") (is true))\n"]
    (try
      (is (= ["-m" "cognitect.test-runner"] (get-in deps [:aliases :test :main-opts])))
      (is (= command (sut/executed-command command)))
      (is (= (conj command "-v" "fixture.test/a")
             (sut/executed-command (conj command "-v" "fixture.test/a"))))
      (is (= (conj command "-v" (str (nth command 3) "/a"))
             (sut/executed-command (conj command "-v" "a")))
          "a bare admitted var is qualified with the -n namespace")
      ;; Copy the actual alias (including main-opts), not a fixture inventing
      ;; the old alias shape. Only test-runner dependency is needed here.
      (spit (io/file root "deps.edn")
            (pr-str {:paths ["test"]
                     :aliases {:test (assoc (get-in deps [:aliases :test])
                                            :extra-deps (select-keys (get-in deps [:aliases :test :extra-deps])
                                                                    ['io.github.cognitect-labs/test-runner]))}}))
      (.mkdirs (io/file root "test/fixture"))
      (spit (io/file root "test/fixture/m2a-unrequested-test.clj")
            "(ns fixture.m2a-unrequested-test) (throw (ex-info \"UNREQUESTED-NAMESPACE-LOADED\" {}))")
      ;; The bounded port requires an attributable Git basis even for fixtures.
      (doseq [args [["init" "-q"] ["add" "."]
                    ["-c" "user.name=M2a fixture" "-c" "user.email=m2a@example.invalid"
                     "commit" "-qm" "Fixture basis"]]]
        (let [result (apply shell/sh "git" "-C" (.getPath root) args)]
          (assert (zero? (:exit result)) (pr-str result))))
      (let [r (binding [sut/*acceptance-repository* (.getPath root)
                       sut/*acceptance-runs-root* (.getPath logs)]
                (#'sut/run-acceptance! 60000
                 {:id "m2a-single-ns" :command ["clojure" "-M:test" "-n" target]
                  :source {:namespace target :path "test/fixture/m2a_only_test.clj" :text text}}))
            output (when-let [p (get-in r [:run-pin :path])] (slurp p))]
        (is (= :ran (:status r)) (pr-str r))
        (is (= :passed (:outcome r)) (pr-str r))
        (is (= 0 (:exit r)))
        (is (true? (:source-clean? r)))
        (is (str/includes? (or output "") "M2A-ONLY-NAMESPACE"))
        (is (str/includes? (or output "") "Ran 1 tests containing 1 assertions"))
        (is (not (str/includes? (or output "") "UNREQUESTED-NAMESPACE-LOADED")))
        (spit "/home/joe/notes/M2a-real-port.log" (str (pr-str r) "\n" output)))
      ;; The proposal form the prompt asks for: -v with a bare var name,
      ;; through the real runner (E3 click stopped on exactly this).
      (let [r (binding [sut/*acceptance-repository* (.getPath root)
                       sut/*acceptance-runs-root* (.getPath logs)]
                (#'sut/run-acceptance! 60000
                 {:id "m2a-bare-var" :command ["clojure" "-M:test" "-n" target "-v" "passes"]
                  :source {:namespace target :path "test/fixture/m2a_only_test.clj" :text text}}))
            output (when-let [p (get-in r [:run-pin :path])] (slurp p))]
        (is (= :passed (:outcome r)) (pr-str r))
        (is (not (str/includes? (or output "") "Could not resolve var")))
        (is (str/includes? (or output "") "Ran 1 tests containing 1 assertions")))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f))))))
