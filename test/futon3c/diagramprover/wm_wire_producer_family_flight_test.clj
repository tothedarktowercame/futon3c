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
  runs every test var of its producer namespace through
  clojure.test/test-vars, so each assertion is reported once, a failure
  still names the record field path (the producers wrap each leaf
  comparison in (testing (pr-str path))), and the producer is named by the
  (testing producer <ns>) context wrapped around the run."
  (:require [clojure.test :as t :refer [deftest testing]]
            [futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g32]
            [futon3c.diagramprover.wm-wire-producer-ask-out-live-census]
            [futon3c.diagramprover.wm-wire-producer-ask-out-step]
            [futon3c.diagramprover.wm-wire-producer-c2-chosen-precedence]
            [futon3c.diagramprover.wm-wire-producer-fold-out-decision]
            [futon3c.diagramprover.wm-wire-producer-fold-out-simple]
            [futon3c.diagramprover.wm-wire-producer-measured-live-kind-pair]
            [futon3c.diagramprover.wm-wire-producer-measured-step-observe]
            [futon3c.diagramprover.wm-wire-producer-measured-tick-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-enact-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-increment-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-publication-observe]
            [futon3c.diagramprover.wm-wire-producer-publication-r0-test-observe]
            [futon3c.diagramprover.wm-wire-producer-small-observe]
            [futon3c.diagramprover.wm-wire-producer-target-observe-g49]
            [futon3c.diagramprover.wm-wire-producer-target-observe]
            [futon3c.diagramprover.wm-wire-producer-temporal-courier-publication-paths]
            [futon3c.diagramprover.wm-wire-producer-temporal-courier]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-record-click-cast-test-literal-f]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-needs-test-literal-]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-readings-test-liter]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-r10-publication-observed-test-literal-fixture]
            [futon3c.diagramprover.wm-wire-producer-wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit]))

(defn- run-producer!
  "Run every test var of producer namespace ns-sym, inside a testing context
  that names the producer, so a failure reports the producer and the record
  field path."
  [ns-sym]
  (testing (str "producer " ns-sym)
    (let [vars (->> (ns-interns ns-sym)
                    vals
                    (filter (comp :test meta))
                    (sort-by (comp str symbol)))]
      (t/test-vars vars))))

(deftest ask-out-live-census-g32
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-ask-out-live-census-g32))

(deftest ask-out-live-census
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-ask-out-live-census))

(deftest ask-out-step
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-ask-out-step))

(deftest c2-chosen-precedence
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-c2-chosen-precedence))

(deftest fold-out-decision
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-fold-out-decision))

(deftest fold-out-simple
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-fold-out-simple))

(deftest measured-live-kind-pair
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-measured-live-kind-pair))

(deftest measured-step-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-measured-step-observe))

(deftest measured-tick-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-measured-tick-observe))

(deftest publication-enact-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-publication-enact-observe))

(deftest publication-increment-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-publication-increment-observe))

(deftest publication-publication-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-publication-publication-observe))

(deftest publication-r0-test-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-publication-r0-test-observe))

(deftest small-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-small-observe))

(deftest target-observe-g49
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-target-observe-g49))

(deftest target-observe
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-target-observe))

(deftest temporal-courier-publication-paths
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-temporal-courier-publication-paths))

(deftest temporal-courier
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-temporal-courier))

(deftest wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt))

(deftest wm-wire-flight-click-flight-record-click-cast-test-literal-f
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-record-click-cast-test-literal-f))

(deftest wm-wire-flight-run-flight-driver-summary-needs-test-literal-
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-needs-test-literal-))

(deftest wm-wire-flight-run-flight-driver-summary-readings-test-liter
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-run-flight-driver-summary-readings-test-liter))

(deftest wm-wire-r10-publication-observed-test-literal-fixture
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-wm-wire-r10-publication-observed-test-literal-fixture))

(deftest wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit
  (run-producer! 'futon3c.diagramprover.wm-wire-producer-wm-wire-r1-outer-cascade-flight-entry-chosen-target-test-lit))
