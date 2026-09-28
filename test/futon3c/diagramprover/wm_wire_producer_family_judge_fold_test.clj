(ns futon3c.diagramprover.wm-wire-producer-family-judge-fold-test
  (:require [clojure.test :as t :refer [deftest testing]]
            [futon3c.diagramprover.wm-wire-producer-c2-refusal-box :as c2]
            [futon3c.diagramprover.wm-wire-producer-fold-in-observe :as fold-in]
            [futon3c.diagramprover.wm-wire-producer-measured-cleanup :as cleanup]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-r7-fold-call-enactment-fold-test-literal-fixture :as enactment-fold]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture :as wc-verdict]))

(deftest c2-refusal-box
  (testing "producer c2-refusal-box"
    (t/test-vars [#'c2/c2-refusal-box-producer])))

(deftest fold-in-observe
  (testing "producer fold-in-observe"
    (t/test-vars [#'fold-in/fold-in-observe-producer])))

(deftest measured-cleanup
  (testing "producer measured-cleanup"
    (t/test-vars [#'cleanup/measured-cleanup-producer])))

(deftest wm-wire-r7-fold-call-enactment-fold-test-literal-fixture
  (testing "producer wm-wire-r7-fold-call-enactment-fold-test-literal-fixture"
    (t/test-vars [#'enactment-fold/enactment-fold-producer])))

(deftest wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture
  (testing "producer wm-wire-wc-checker-r7-test-wc-verdict-test-literal-fixture"
    (t/test-vars [#'wc-verdict/wc-verdict-producer])))
