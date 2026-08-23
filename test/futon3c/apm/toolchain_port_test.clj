(ns futon3c.apm.toolchain-port-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.toolchain-port :as sut]))

(deftest sorry-plus-style-lint-is-an-admissible-baseline
  (let [observation
        (sut/classify-output
         0 (str "Main.lean:1: warning: declaration uses `sorry`\n"
                "Main.lean:2: warning: try 'simp' instead of 'simpa'\n"))]
    (is (= 1 (:sorry-warnings observation)))
    (is (= [:linter] (mapv :kind (:other-warnings observation))))
    (is (zero? (:blocking-warnings observation)))
    (is (sut/acceptable-preflight? observation))))

(deftest errors-and-empty-hole-sets-are-not-baselines
  (is (false? (sut/acceptable-preflight?
               (sut/classify-output 1 "Main.lean:1: error: bad"))))
  (is (false? (sut/acceptable-preflight?
               (sut/classify-output 0 "")))))
