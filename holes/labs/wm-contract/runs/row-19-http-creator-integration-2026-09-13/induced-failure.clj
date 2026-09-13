(ns codex23-http-ingress-induced-test
  (:require [clojure.test :refer [deftest is]]))

(deftest deliberate-failure
  (is (= :accepted :not-accepted)))
