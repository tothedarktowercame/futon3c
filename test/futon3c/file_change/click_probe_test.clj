(ns futon3c.file-change.click-probe-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.file-change.click-probe :as probe]))

(defn- log! [dir name text]
  (let [f (io/file dir name)] (spit f text) f))

(deftest parses-existing-click-log-verdicts-and-skips-repeat
  (let [dir (doto (java.io.File/createTempFile "click-probes-" "")
              (.delete) (.mkdirs) (.deleteOnExit))]
    (log! dir "click-001-10.log" "  giraffe field  fresh (run-a)\n")
    (log! dir "click-002-20.log"
          "giraffe field  stale (newest run-a) -- minting\ngiraffe field  minted run-b (ok)\n")
    (log! dir "click-003-30.log" "freshness probe failed (exit 1)\n")
    (log! dir "click-004-40.log" "giraffe field  already checked this launch\n")
    (is (= [[10000 true false "run-a"] [20000 false true "run-b"]
            [30000 nil false nil]]
           (mapv (juxt :at-ms :fresh? :minted? :run-id)
                 (probe/read-loop-probes dir))))))
