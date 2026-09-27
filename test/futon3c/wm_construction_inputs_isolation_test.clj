(ns futon3c.wm-construction-inputs-isolation-test
  "Fresh JVM checks: loading the assembly witnesses must not load reporting."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            ;; Include dependencies in the registry parent process's load closure;
            ;; the absence assertions themselves run only in fresh children.
            [futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-sources-wants-test]
            [futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-universes-test]))

(def wire-namespaces
  '[futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-sources-wants-test
    futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-universes-test])

(deftest ^:slow construction-witnesses-do-not-load-reporting
  (doseq [n wire-namespaces]
    (testing (str n)
      (let [form (pr-str
                  `(do
                     (require '~n)
                     (assert (nil? (find-ns 'futon2.report.war-machine)))
                     (assert (nil? (find-ns 'futon2.aif.full-loop-runner)))
                     (println :isolated '~n)))
            {:keys [exit out err]}
            (shell/sh "timeout" "90s"
                      (str (io/file (System/getProperty "java.home") "bin" "java"))
                      "-cp" (System/getProperty "java.class.path")
                      "clojure.main" "-e" form)]
        (println out)
        (is (= 0 exit) (str n " exited " exit "\n" out err))))))
