(ns futon3c.wm.code-identity-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.wm.code-identity :as identity]))

(use-fixtures :each (fn [f] (identity/reset-for-test!) (f)))

(deftest absence-is-unavailable-not-current-disk-guess
  (is (= {:schema 1
          :required-file "src/futon2/aif/full_loop_runner.clj"
          :availability :unavailable
          :reason :not-recorded-in-this-process-image
          :identity nil}
         (identity/status))))

(deftest recorded-runner-identity-is-returned-verbatim
  (let [record {:schema 1 :git-head "abc" :dirty? false :stable? true}]
    (identity/install-for-test! record)
    (is (= :available (:availability (identity/status))))
    (is (= record (:identity (identity/status))))))

(defn- git! [root & args]
  (let [{:keys [exit err]} (apply shell/sh "git" "-C" root args)]
    (when-not (zero? exit) (throw (ex-info "fixture git failed" {:args args :err err})))))

(defn- fixture-repo []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "wm-code-identity-" (make-array java.nio.file.attribute.FileAttribute 0)))
        source (io/file root identity/production-runner)]
    (io/make-parents source)
    (spit source "(ns futon2.aif.full-loop-runner)\n(def fixture-loaded true)\n")
    (git! (.getPath root) "init" "-q")
    (git! (.getPath root) "config" "user.email" "test@example.invalid")
    (git! (.getPath root) "config" "user.name" "Test")
    (git! (.getPath root) "add" identity/production-runner)
    (git! (.getPath root) "commit" "-qm" "fixture")
    {:root root :source source}))

(deftest recorded-loader-captures-clean-basis-and-refuses-dirty-or-outside
  (let [{:keys [root source]} (fixture-repo)]
    (binding [identity/*futon2-root* (.getPath root)]
      (testing "clean canonical source records the loaded identity"
        (identity/load-file-recorded! (.getPath source))
        (let [record (:identity (identity/status))]
          (is (= :available (:availability (identity/status))))
          (is (false? (:dirty? record)))
          (is (true? (:stable? record)))
          (is (= identity/production-runner (:path record)))
          (is (re-matches #"[0-9a-f]{64}" (:content-sha256 record)))))
      (testing "dirty canonical source is refused and records nothing new"
        (identity/reset-for-test!)
        (spit source "\n;; dirty" :append true)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"dirty repository"
                              (identity/load-file-recorded! (.getPath source))))
        (is (= :unavailable (:availability (identity/status)))))
      (testing "noncanonical source is refused and records nothing"
        (identity/reset-for-test!)
        (let [outside (java.io.File/createTempFile "wm-code-outside-" ".clj")]
          (spit outside "(def outside true)\n")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a readable canonical"
                                (identity/load-file-recorded! (.getPath outside))))
          (is (= :unavailable (:availability (identity/status)))))))))
