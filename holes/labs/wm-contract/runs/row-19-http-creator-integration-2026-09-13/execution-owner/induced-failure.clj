(ns codex23-owner-induced-test
  (:require [clojure.test :refer [deftest is]]))
(deftest deliberate-failure (is (= :owner :reused-caller)))
