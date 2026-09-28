(ns futon3c.diagramprover.wm-wire-producer-family-flight-test
  "FAMILY-flight (E-kimi-task-124, M-warrant-limit): the 24 flight producer
  namespaces merged into one test namespace.

  Each producer below was renamed from wm_wire_producer_<stem>_test.clj to
  wm_wire_producer_<stem>.clj (namespace likewise, dropping -test) and is
  otherwise unchanged: same code, same deftest, same immutable record under
  test/fixtures/wire-producers/. One warrant is now carried by this family
  namespace instead of 24, so an edit to flight code makes one producer
  warrant stale rather than up to 24.

  One deftest per producer, named after the producer's record stem. Each
  runs its producer's deftest through clojure.test/test-vars, named as a
  literal var: the warrant's dependency record follows literal var
  references only, so a runtime ns-interns lookup would leave the flight
  code out of this warrant (claude-1, 2026-09-28: it did, 48 definitions
  reached), so each assertion is reported once, a failure
  still names the record field path (the producers wrap each leaf
  comparison in (testing (pr-str path))), and the producer is named by the
  (testing producer <ns>) context wrapped around the run."
  (:require [clojure.test :as t :refer [deftest testing]]
            [futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g32 :as ask-out-live-census-g32]
            [futon3c.diagramprover.wm-wire-producer-ask-out-live-census :as ask-out-live-census]
            [futon3c.diagramprover.wm-wire-producer-ask-out-step :as ask-out-step]
            [futon3c.diagramprover.wm-wire-producer-c2-chosen-precedence :as c2-chosen-precedence]
            [futon3c.diagramprover.wm-wire-producer-fold-out-decision :as fold-out-decision]
            [futon3c.diagramprover.wm-wire-producer-fold-out-simple :as fold-out-simple]
            [futon3c.diagramprover.wm-wire-producer-measured-live-kind-pair :as measured-live-kind-pair]
            [futon3c.diagramprover.wm-wire-producer-measured-step-observe :as measured-step-observe]
            [futon3c.diagramprover.wm-wire-producer-measured-tick-observe :as measured-tick-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-enact-observe :as publication-enact-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-increment-observe :as publication-increment-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-publication-observe :as publication-publication-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-r0-test-observe :as publication-r0-test-observe]
            [futon3c.diagramprover.wm-wire-producer-small-observe :as small-observe]
            [futon3c.diagramprover.wm-wire-producer-target-observe-g49 :as target-observe-g49]
            [futon3c.diagramprover.wm-wire-producer-target-observe :as target-observe]
            [futon3c.diagramprover.wm-wire-producer-temporal-courier-publication-paths :as temporal-courier-publication-paths]
            [futon3c.diagramprover.wm-wire-producer-temporal-courier :as temporal-courier]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt :as wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-record-click-cast-test-literal-f :as wm-wire-flight-click-flight-record-click-cast-test-literal-f]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-needs-test-literal- :as wm-wire-flight-run-flight-driver-summary-needs-test-literal-]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-readings-test-liter :as wm-wire-flight-run-flight-driver-summary-readings-test-liter]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-r10-publication-observed-test-literal-fixture :as wm-wire-r10-publication-observed-test-literal-fixture]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit :as wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit]))

(deftest ask-out-live-census-g32
  (testing "producer futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g32"
    (t/test-vars [#'ask-out-live-census-g32/ask-out-live-census-g32-producer])))

(deftest ask-out-live-census
  (testing "producer futon3c.diagramprover.wm-wire-producer-ask-out-live-census"
    (t/test-vars [#'ask-out-live-census/ask-out-live-census-producer])))

(deftest ask-out-step
  (testing "producer futon3c.diagramprover.wm-wire-producer-ask-out-step"
    (t/test-vars [#'ask-out-step/ask-out-step-producer])))

(deftest c2-chosen-precedence
  (testing "producer futon3c.diagramprover.wm-wire-producer-c2-chosen-precedence"
    (t/test-vars [#'c2-chosen-precedence/producer-test])))

(deftest fold-out-decision
  (testing "producer futon3c.diagramprover.wm-wire-producer-fold-out-decision"
    (t/test-vars [#'fold-out-decision/fold-out-decision-producer])))

(deftest fold-out-simple
  (testing "producer futon3c.diagramprover.wm-wire-producer-fold-out-simple"
    (t/test-vars [#'fold-out-simple/fold-out-simple-producer])))

(deftest measured-live-kind-pair
  (testing "producer futon3c.diagramprover.wm-wire-producer-measured-live-kind-pair"
    (t/test-vars [#'measured-live-kind-pair/producer-test])))

(deftest measured-step-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-measured-step-observe"
    (t/test-vars [#'measured-step-observe/producer-test])))

(deftest measured-tick-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-measured-tick-observe"
    (t/test-vars [#'measured-tick-observe/measured-tick-observe-producer])))

(deftest publication-enact-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-publication-enact-observe"
    (t/test-vars [#'publication-enact-observe/producer-test])))

(deftest publication-increment-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-publication-increment-observe"
    (t/test-vars [#'publication-increment-observe/increment-producer])))

(deftest publication-publication-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-publication-publication-observe"
    (t/test-vars [#'publication-publication-observe/publication-observe-producer])))

(deftest publication-r0-test-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-publication-r0-test-observe"
    (t/test-vars [#'publication-r0-test-observe/r0-test-observe-producer])))

(deftest small-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-small-observe"
    (t/test-vars [#'small-observe/small-observe-producer])))

(deftest target-observe-g49
  (testing "producer futon3c.diagramprover.wm-wire-producer-target-observe-g49"
    (t/test-vars [#'target-observe-g49/target-observe-g49-producer])))

(deftest target-observe
  (testing "producer futon3c.diagramprover.wm-wire-producer-target-observe"
    (t/test-vars [#'target-observe/target-observe-producer])))

(deftest temporal-courier-publication-paths
  (testing "producer futon3c.diagramprover.wm-wire-producer-temporal-courier-publication-paths"
    (t/test-vars [#'temporal-courier-publication-paths/temporal-courier-publication-paths-producer])))

(deftest temporal-courier
  (testing "producer futon3c.diagramprover.wm-wire-producer-temporal-courier"
    (t/test-vars [#'temporal-courier/temporal-courier-producer])))

(deftest wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt
  (testing "producer futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt"
    (t/test-vars [#'wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt/flight-cast-producer])))

(deftest wm-wire-flight-click-flight-record-click-cast-test-literal-f
  (testing "producer futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-record-click-cast-test-literal-f"
    (t/test-vars [#'wm-wire-flight-click-flight-record-click-cast-test-literal-f/literal-f-producer])))

(deftest wm-wire-flight-run-flight-driver-summary-needs-test-literal-
  (testing "producer futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-needs-test-literal-"
    (t/test-vars [#'wm-wire-flight-run-flight-driver-summary-needs-test-literal-/producer-test])))

(deftest wm-wire-flight-run-flight-driver-summary-readings-test-liter
  (testing "producer futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-readings-test-liter"
    (t/test-vars [#'wm-wire-flight-run-flight-driver-summary-readings-test-liter/producer-test])))

(deftest wm-wire-r10-publication-observed-test-literal-fixture
  (testing "producer futon3c.diagramprover.wm-wire-producer-wm-wire-r10-publication-observed-test-literal-fixture"
    (t/test-vars [#'wm-wire-r10-publication-observed-test-literal-fixture/wm-wire-r10-publication-observed-test-literal-fixture-producer])))

(deftest wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit
  (testing "producer futon3c.diagramprover.wm-wire-producer-wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit"
    (t/test-vars [#'wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit/wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit-producer])))
