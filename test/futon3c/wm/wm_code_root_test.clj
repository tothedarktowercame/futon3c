(ns futon3c.wm.wm-code-root-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.meta-self-heal-adapters :as self-heal]
            [futon3c.wm.runner-service :as runner]))

(deftest configured-code-root-contains-every-click-path
  (let [root (str (.toFile (java.nio.file.Files/createTempDirectory
                            "wm-code-root" (make-array java.nio.file.attribute.FileAttribute 0))))
        under #(str (apply io/file root %&))
        paths (merge (runner/configured-paths nil root)
                     (self-heal/configured-paths root))]
    (is (= {:click-acceptance-dir
            (under "futon3c" "data" "wm-click-acceptances")
            :click-run-binding-dir
            (under "futon3c" "data" "wm-click-run-bindings")
            :run-record-dir (under "futon2" "data" "wm-runs")
            :run4-terminal-projection-dir
            (under "futon3c" "data" "wm-run4-terminal-projections")
            :run4-historical-projection-dir
            (under "futon3c" "data" "wm-run4-historical-projections")
            :p4ng-repo (under "p4ng")
            :futon2-repo (under "futon2")
            :route-check-script
            (under "futon2" "scripts" "wm_route_check.py")
            :repository (under "futon2")
            :bounded-test-runner
            (under "futon3c" "scripts" "bounded_test_job.py")}
           paths))))

(deftest canonical-defaults-are-unchanged
  (let [paths (merge (runner/configured-paths nil nil)
                     (self-heal/configured-paths nil))]
    (is (= {:click-acceptance-dir
            "/home/joe/code/futon3c/data/wm-click-acceptances"
            :click-run-binding-dir
            "/home/joe/code/futon3c/data/wm-click-run-bindings"
            :run-record-dir "/home/joe/code/futon2/data/wm-runs"
            :run4-terminal-projection-dir
            "/home/joe/code/futon3c/data/wm-run4-terminal-projections"
            :run4-historical-projection-dir
            "/home/joe/code/futon3c/data/wm-run4-historical-projections"
            :p4ng-repo "/home/joe/code/p4ng"
            :futon2-repo "/home/joe/code/futon2"
            :route-check-script
            "/home/joe/code/futon2/scripts/wm_route_check.py"
            :repository "/home/joe/code/futon2"
            :bounded-test-runner
            "/home/joe/code/futon3c/scripts/bounded_test_job.py"}
           paths))))

(deftest explicit-futon2-data-root-wins-for-run-records
  (is (= "/tmp/futon2-data/wm-runs"
         (:run-record-dir
          (runner/configured-paths "/tmp/futon2-data" "/tmp/wm-code-root")))))
