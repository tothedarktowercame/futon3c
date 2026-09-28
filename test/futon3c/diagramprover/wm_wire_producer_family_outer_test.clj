(ns futon3c.diagramprover.wm-wire-producer-family-outer-test
  (:require [clojure.test :as t :refer [deftest testing]]
            [futon3c.diagramprover.wm-wire-producer-outer-inputs-observe :as outer-inputs]
            [futon3c.diagramprover.wm-wire-producer-plan-observe :as plan]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix :as loop-entry]))

(deftest outer-inputs-observe
  (testing "producer outer-inputs-observe"
    (t/test-vars [#'outer-inputs/outer-inputs-observe-producer])))

(deftest plan-observe
  (testing "producer plan-observe"
    (t/test-vars [#'plan/plan-observe-producer])))

(deftest wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix
  (testing "producer wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix"
    (t/test-vars [#'loop-entry/wm-wire-loop-entry-r1-outer-cascade-trigger-test-literal-fix-producer])))
