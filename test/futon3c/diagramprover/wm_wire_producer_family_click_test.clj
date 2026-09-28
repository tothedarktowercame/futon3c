(ns futon3c.diagramprover.wm-wire-producer-family-click-test
  (:require [clojure.test :as t :refer [deftest testing]]
            [futon3c.diagramprover.wm-wire-producer-r9-run-tick :as run-tick]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-click-reason-test-failure-test :as click-reason]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-click-close-test-chosen :as click-close-chosen]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-record-click-chosen-tes :as record-click-chosen]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-record-summary-flight-record-click-failure-te :as record-click-failure]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-r9-candidate-enact-test-literal-fixture :as candidate-enact]))

(deftest r9-run-tick
  (testing "producer r9-run-tick"
    (t/test-vars [#'run-tick/r9-run-tick-producer])))

(deftest wm-wire-flight-record-summary-click-reason-test-failure-test
  (testing "producer wm-wire-flight-record-summary-click-reason-test-failure-test"
    (t/test-vars [#'click-reason/wm-wire-flight-record-summary-click-reason-test-failure-test-producer])))

(deftest wm-wire-flight-record-summary-flight-click-close-test-chosen
  (testing "producer wm-wire-flight-record-summary-flight-click-close-test-chosen"
    (t/test-vars [#'click-close-chosen/producer-test])))

(deftest wm-wire-flight-record-summary-flight-record-click-chosen-tes
  (testing "producer wm-wire-flight-record-summary-flight-record-click-chosen-tes"
    (t/test-vars [#'record-click-chosen/producer-test])))

(deftest wm-wire-flight-record-summary-flight-record-click-failure-te
  (testing "producer wm-wire-flight-record-summary-flight-record-click-failure-te"
    (t/test-vars [#'record-click-failure/producer-test])))

(deftest wm-wire-r9-candidate-enact-test-literal-fixture
  (testing "producer wm-wire-r9-candidate-enact-test-literal-fixture"
    (t/test-vars [#'candidate-enact/producer-test])))
