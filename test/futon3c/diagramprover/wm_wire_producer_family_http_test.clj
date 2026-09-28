(ns futon3c.diagramprover.wm-wire-producer-family-http-test
  (:require [clojure.test :as test :refer [deftest]]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-cast-click-start-author-test-literal-fixture :as author]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-cast-click-start-repair-reviewer-test-literal :as repair-reviewer]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-cast-click-start-reviewer-test-literal-fixtur :as reviewer]))

(deftest wm-wire-flight-cast-click-start-author-test-literal-fixture
  (test/test-vars [#'author/producer-test]))

(deftest wm-wire-flight-cast-click-start-repair-reviewer-test-literal
  (test/test-vars [#'repair-reviewer/producer-test]))

(deftest wm-wire-flight-cast-click-start-reviewer-test-literal-fixtur
  (test/test-vars [#'reviewer/producer-test]))
