(ns futon3c.diagramprover.wm-split-cascade-decision-isolation-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]))

(deftest retargeted-wire-support-does-not-load-war-machine
  (let [form (pr-str '(do
                        (require 'futon3c.diagramprover.wm-wire-selection-products-support)
                        (assert (nil? (find-ns 'futon2.report.war-machine)))
                        (println :isolated)))
        {:keys [exit out err]}
        (shell/sh "timeout" "90s"
                  (str (io/file (System/getProperty "java.home") "bin" "java"))
                  "-cp" (System/getProperty "java.class.path")
                  "clojure.main" "-e" form)]
    (is (= 0 exit) (str out err))))
