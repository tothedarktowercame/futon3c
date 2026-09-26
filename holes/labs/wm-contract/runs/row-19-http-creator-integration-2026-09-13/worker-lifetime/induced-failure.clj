(ns codex23-worker-lifetime-induced-test
  (:require [clojure.test :refer [deftest is]]))

(deftest deliberate-failure
  (is (= :worker-exited :worker-alive)))
