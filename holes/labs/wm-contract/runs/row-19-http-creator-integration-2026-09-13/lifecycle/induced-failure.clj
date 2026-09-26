(ns codex23-http-lifecycle-induced-test
  (:require [clojure.test :refer [deftest is]]))

(deftest deliberate-failure
  (is (= :drained :work-remains)))
