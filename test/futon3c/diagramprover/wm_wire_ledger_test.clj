(ns futon3c.diagramprover.wm-wire-ledger-test
  "The War Machine's wires, and which of them something has been sent over.

  A wire is one entry of the adjacency matrix with one field, [a b f]: box a
  declares that it writes f and box b that it reads f (wm-adjacency.edn,
  generated from the map by spike/wm_adjacency.bb). The map test checks the
  names are present at the sites; a wire test checks that a value went
  across. What the value means is the second layer and not this one.

  A wire is VERIFIED when a registered test shows, for one live record (a
  click record, flight record, run record, enactment record or repair
  finding under holes/labs/M-wm-wiring/spike/ or holes/labs/M-futon-seams/
  exemplar/), that the reader's value under f is present, is not a typed
  absence ({:absent ...}, or the older {:status :absent ...}), and is the
  value the writer wrote: the record carries both ends, or the writer's
  value is recoverable from the same record. The record is named by path
  and sha256.

  When no live record carries both ends, a wire is WITNESSED-HERMETICALLY
  when a test drives the writer's var through the reader's var in a
  hermetic run and observes the same three things, and the test says so,
  naming the records it read and why each lacks an end.

  Otherwise the wire is UNVERIFIED. That is the ledger's truthful state, not
  a failure of this test.

  Each wire test namespace defines `wire`: {:wire [a b f] :kind
  :verified|:witnessed-hermetically :test <its deftest> :check <fn returning
  {:writer v :reader v}>, and for :verified :record {:path :sha256}}. The
  ledger runs each check through wm-wire/received?, so a wire's status is
  what its check observes now, and a registered wire whose check fails is
  recorded :unverified (its own test fails beside it). Adding a wire test is
  adding its namespace to wire-test-nses.

  This test reads the matrix from git (wm-adjacency.edn at adjacency-rev,
  generated from the map at map-rev), checks each wire against the map at
  map-rev, writes holes/labs/M-wm-wiring/wm-wire-ledger.edn, and asserts the
  ledger's counts equal what the checks found."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pp]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-morning-brief-fold-r7-fold-call-belief-test]
            [futon3c.diagramprover.wm-wire-r13-policy-depth-anticipation-r7-fold-call-horizon-steps-test]
            [futon3c.diagramprover.wm-wire-r3-aggregate-driver-r7-fold-call-driver-test]
            [futon3c.diagramprover.wm-wire-trace-record-r7-fold-call-mu-post-test]
            [futon3c.diagramprover.wm-wire-construction-construct-r9-decision-construction-receipt-test]
            [futon3c.diagramprover.wm-wire-construction-construct-selection-candidate-derivations-construction-receipt-test]
            [futon3c.diagramprover.wm-wire-construction-assemble-one-r9-decision-want-test]
            [futon3c.diagramprover.wm-wire-construction-assemble-one-r4-kernel-want-test]
            [futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-beta-test]
            [futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-horizon-steps-test]
            [futon3c.diagramprover.wm-wire-r9-decision-flight-conditioning-step-measured-a-test]
            [futon3c.diagramprover.wm-wire-r9-measured-a-version-flight-conditioning-step-rates-test]
            [futon3c.diagramprover.wm-wire-r9-measured-a-version-flight-conditioning-step-measurement-test]
            [futon3c.diagramprover.wm-wire-flight-click-r9-measured-a-version-status-test]
            [futon3c.diagramprover.wm-wire-r9-decision-flight-record-click-kind-test]
            [futon3c.diagramprover.wm-wire-flight-run-flight-steps-source-step-test]
            [futon3c.diagramprover.wm-wire-r2-store-criteria-r3-store-criteria-criteria-test]
            [futon3c.diagramprover.wm-wire-r2-store-coverage-r3-store-coverage-coverage-test]
            [futon3c.diagramprover.wm-wire-r2-store-locators-r3-store-locators-locators-test]
            [futon3c.diagramprover.wm-wire-r2-store-locator-questions-r3-store-locator-questions-locator-questions-test]
            [futon3c.diagramprover.wm-wire-r2-store-locator-declines-r3-store-locator-declines-locator-declines-test]
            [futon3c.diagramprover.wm-wire-r2-store-constraints-read-r3-store-constraints-read-constraints-read-test]
            [futon3c.diagramprover.wm-wire-flight-entry-flight-click-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-flight-judge-opts-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-flight-run-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-r2-flight-read-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-flight-ask-fn-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-tick-flight-assembly-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-r9-close-cause-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-r0-enact-step-target-test]
            [futon3c.diagramprover.wm-wire-flight-entry-r10-observe-publication-target-test]
            [futon3c.diagramprover.wm-wire-dispatch-clock-in-mission-id-test]
            [futon3c.diagramprover.wm-wire-r3-flight-ask-r3-prompt-library-root-test]
            [futon3c.diagramprover.wm-wire-r3-flight-ask-r3-test-library-root-test]
            [futon3c.diagramprover.wm-wire-r13-sources-horizon-construction-assemble-horizon-steps-test]
            [futon3c.diagramprover.wm-wire-r4-evaluate-state-r4-push-forward-kernel-test]
            [futon3c.diagramprover.wm-wire-flight-record-summary-flight-run-chosen-test]
            [futon3c.diagramprover.wm-wire-r4-kernel-fpi-policy-free-energy-rates-test]
            [futon3c.diagramprover.wm-wire-r6-cascade-lane-r4-kernel-adjudication-rates-test]
            [futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-measurement-test]
            [futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-scoped-rates-test]
            [futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-test-measurement-test]
            [futon3c.diagramprover.wm-wire-loop-entry-loop-plan-trigger-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-target-selection-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-plan-chosen-target-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-plan-draw-seed-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-plan-target-selection-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-chosen-target-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-draw-seed-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-target-selection-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-r1-test-chosen-target-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-r1-test-target-selection-test]
            [futon3c.diagramprover.wm-wire-c8-registry-get-entry-message-test]
            [futon3c.diagramprover.wm-wire-c8-registry-get-entry-timeout-ms-test]
            [futon3c.diagramprover.wm-wire-c8-registry-get-latest-message-test]
            [futon3c.diagramprover.wm-wire-c8-registry-get-latest-timeout-ms-test]
            [futon3c.diagramprover.wm-wire-r2-flight-read-text-sha256-test]
            [futon3c.diagramprover.wm-wire-r2-test-text-sha256-test]
            [futon3c.diagramprover.wm-wire-r2-test-want-span-test]
            [futon3c.diagramprover.wm-wire-r2-verifier-text-sha256-test]
            [futon3c.diagramprover.wm-wire-r2-verifier-want-span-test]
            [futon3c.diagramprover.wm-wire-r7-fold-selection-test]
            [futon3c.diagramprover.wm-wire-r9-candidate-enact-test]
            [futon3c.diagramprover.wm-wire-gate-refuse-gate-refusal-read-error-test]
            [futon3c.diagramprover.wm-wire-r9-close-cause-failure-cause-record-test-test]
            [futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-cause-read-failure-cause-test]
            [futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-store-failure-cause-test]
            [futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-abstention-carrier-judge-refusal-test]
            [futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-judge-refusal-test-judge-refusal-test]
            [futon3c.diagramprover.wm-wire-r9-judge-refusal-abstention-r9-failure-classifier-outcome-test]
            [futon3c.diagramprover.wm-wire-r9-phase-kind-phase-kind-test-failure-kind-test]
            [futon3c.diagramprover.wm-wire-r9-phase-kind-r9-failure-classifier-failure-kind-test]
            [futon3c.diagramprover.wm-wire-r9-classify-target-decision-class-test]
            [futon3c.diagramprover.wm-wire-r9-classify-target-relation-test-class-test]
            [futon3c.diagramprover.wm-wire-r9-decision-class-model-target-class-test]
            [futon3c.diagramprover.wm-wire-r9-embedding-neighbour-classify-target-derived-via-test]
            [futon3c.diagramprover.wm-wire-r9-embedding-neighbour-relation-test-derived-via-test]
            [futon3c.diagramprover.wm-wire-r9-selection-law-decision-per-policy-argmax-test]
            [futon3c.diagramprover.wm-wire-r9-selection-law-r9-test-candidate-test]
            [futon3c.diagramprover.wm-wire-r9-selection-law-r9-test-enacted-steps-test]
            [futon3c.diagramprover.wm-wire-r9-selection-law-r9-test-per-policy-argmax-test]
            [futon3c.diagramprover.wm-wire-r10-publication-observed-test]
            [futon3c.diagramprover.wm-wire-r7-flight-call-increment-test]
            [futon3c.diagramprover.wm-wire-r7-flight-call-wc-test]
            [futon3c.diagramprover.wm-wire-r7-fold-call-enactment-fold-test]
            [futon3c.diagramprover.wm-wire-r7-increment-call-delta-test]
            [futon3c.diagramprover.wm-wire-r7-increment-call-wc-failures-test]
            [futon3c.diagramprover.wm-wire-r7-increment-fold-delta-test]
            [futon3c.diagramprover.wm-wire-r7-increment-r7-test-delta-test]
            [futon3c.diagramprover.wm-wire-r7-selection-e-source-test]
            [futon3c.diagramprover.wm-wire-eligibility-r1-outer-cascade-eligible-test]
            [futon3c.diagramprover.wm-wire-flight-entry-loop-test-target-source-test]
            [futon3c.diagramprover.wm-wire-loop-entry-r1-outer-cascade-trigger-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-chosen-target-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-draw-seed-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-test-draw-seed-test]
            [futon3c.diagramprover.wm-wire-r1-outer-cascade-r1-test-draw-seed-test]
            [futon3c.diagramprover.wm-wire-r1-target-field-r1-outer-cascade-next-step-test]
            [futon3c.diagramprover.wm-wire-r8-overlap-r1-outer-cascade-pair-overlap-test]
            [futon3c.diagramprover.wm-wire-r8-overlap-r8-test-pair-overlap-test]
            [futon3c.diagramprover.wm-wire-r0-test-attempts-test]
            [futon3c.diagramprover.wm-wire-r0-test-grain-gate-test]
            [futon3c.diagramprover.wm-wire-r0-r5-test-attempts-test]
            [futon3c.diagramprover.wm-wire-r0-r5-test-grain-gate-test]
            [futon3c.diagramprover.wm-wire-r0-wc-attempts-test]
            [futon3c.diagramprover.wm-wire-r0-wc-grain-gate-test]
            [futon3c.diagramprover.wm-wire-r5-gate-grain-test]
            [futon3c.diagramprover.wm-wire-r5-test-grain-test]
            [futon3c.diagramprover.wm-wire-construct-order-use-receipt-test]
            [futon3c.diagramprover.wm-wire-constructor-coapply-descent-test]
            [futon3c.diagramprover.wm-wire-constructor-coapply-units-test]
            [futon3c.diagramprover.wm-wire-constructor-order-use-descent-test]
            [futon3c.diagramprover.wm-wire-constructor-order-use-precedence-violations-test]
            [futon3c.diagramprover.wm-wire-constructor-order-use-units-test]
            [futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-sources-wants-test]
            [futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-universes-test]
            [futon3c.diagramprover.wm-wire-judge-observation-channel-pe-test]
            [futon3c.diagramprover.wm-wire-prediction-error-weighted-error-test]
            [futon3c.diagramprover.wm-wire-weighted-error-aggregate-driver-test]
            [futon3c.diagramprover.wm-wire-flight-cast-click-start-author-test]
            [futon3c.diagramprover.wm-wire-flight-cast-click-start-reviewer-test]
            [futon3c.diagramprover.wm-wire-flight-cast-click-start-repair-reviewer-test]
            [futon3c.diagramprover.wm-wire-flight-click-flight-cast-test-cast-test]
            [futon3c.diagramprover.wm-wire-flight-click-flight-record-click-cast-test]
            [futon3c.diagramprover.wm-wire-flight-click-flight-record-click-detail-test]
            [futon3c.diagramprover.wm-wire-flight-click-flight-record-click-status-test]
            [futon3c.diagramprover.wm-wire-flight-record-summary-click-reason-test-failure-test]
            [futon3c.diagramprover.wm-wire-flight-record-summary-flight-click-close-test-chosen-test]
            [futon3c.diagramprover.wm-wire-flight-record-summary-flight-record-click-chosen-test]
            [futon3c.diagramprover.wm-wire-flight-record-summary-flight-record-click-failure-test]
            [futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-needs-test]
            [futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-readings-test]))

(def wire-test-nses
  '[
    futon3c.diagramprover.wm-wire-morning-brief-fold-r7-fold-call-belief-test
    futon3c.diagramprover.wm-wire-r13-policy-depth-anticipation-r7-fold-call-horizon-steps-test
    futon3c.diagramprover.wm-wire-r3-aggregate-driver-r7-fold-call-driver-test
    futon3c.diagramprover.wm-wire-trace-record-r7-fold-call-mu-post-test
    
   futon3c.diagramprover.wm-wire-construction-construct-r9-decision-construction-receipt-test
   futon3c.diagramprover.wm-wire-construction-construct-selection-candidate-derivations-construction-receipt-test
   futon3c.diagramprover.wm-wire-construction-assemble-one-r9-decision-want-test
   futon3c.diagramprover.wm-wire-construction-assemble-one-r4-kernel-want-test
   futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-beta-test
   futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-horizon-steps-test
   futon3c.diagramprover.wm-wire-r9-decision-flight-conditioning-step-measured-a-test
    futon3c.diagramprover.wm-wire-r9-measured-a-version-flight-conditioning-step-rates-test
    futon3c.diagramprover.wm-wire-r9-measured-a-version-flight-conditioning-step-measurement-test
    futon3c.diagramprover.wm-wire-flight-click-r9-measured-a-version-status-test
    futon3c.diagramprover.wm-wire-r9-decision-flight-record-click-kind-test
    futon3c.diagramprover.wm-wire-flight-run-flight-steps-source-step-test
    futon3c.diagramprover.wm-wire-r2-store-criteria-r3-store-criteria-criteria-test
    futon3c.diagramprover.wm-wire-r2-store-coverage-r3-store-coverage-coverage-test
    futon3c.diagramprover.wm-wire-r2-store-locators-r3-store-locators-locators-test
    futon3c.diagramprover.wm-wire-r2-store-locator-questions-r3-store-locator-questions-locator-questions-test
    futon3c.diagramprover.wm-wire-r2-store-locator-declines-r3-store-locator-declines-locator-declines-test
    futon3c.diagramprover.wm-wire-r2-store-constraints-read-r3-store-constraints-read-constraints-read-test
    futon3c.diagramprover.wm-wire-flight-entry-flight-click-target-test
    futon3c.diagramprover.wm-wire-flight-entry-flight-judge-opts-target-test
    futon3c.diagramprover.wm-wire-flight-entry-flight-run-target-test
    futon3c.diagramprover.wm-wire-flight-entry-r2-flight-read-target-test
    futon3c.diagramprover.wm-wire-flight-entry-flight-ask-fn-target-test
    futon3c.diagramprover.wm-wire-flight-entry-tick-flight-assembly-target-test
    futon3c.diagramprover.wm-wire-flight-entry-r9-close-cause-target-test
    futon3c.diagramprover.wm-wire-flight-entry-r0-enact-step-target-test
    futon3c.diagramprover.wm-wire-flight-entry-r10-observe-publication-target-test
    futon3c.diagramprover.wm-wire-dispatch-clock-in-mission-id-test
    futon3c.diagramprover.wm-wire-r3-flight-ask-r3-prompt-library-root-test
    futon3c.diagramprover.wm-wire-r3-flight-ask-r3-test-library-root-test
    futon3c.diagramprover.wm-wire-r13-sources-horizon-construction-assemble-horizon-steps-test
    futon3c.diagramprover.wm-wire-r4-evaluate-state-r4-push-forward-kernel-test
    futon3c.diagramprover.wm-wire-flight-record-summary-flight-run-chosen-test
    futon3c.diagramprover.wm-wire-r4-kernel-fpi-policy-free-energy-rates-test
    futon3c.diagramprover.wm-wire-r6-cascade-lane-r4-kernel-adjudication-rates-test
    futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-measurement-test
    futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-scoped-rates-test
    futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-test-measurement-test
    futon3c.diagramprover.wm-wire-loop-entry-loop-plan-trigger-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-target-selection-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-plan-chosen-target-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-plan-draw-seed-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-plan-target-selection-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-chosen-target-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-draw-seed-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-target-selection-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-r1-test-chosen-target-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-r1-test-target-selection-test
    futon3c.diagramprover.wm-wire-r2-flight-read-text-sha256-test
    futon3c.diagramprover.wm-wire-r2-test-text-sha256-test
    futon3c.diagramprover.wm-wire-r2-test-want-span-test
    futon3c.diagramprover.wm-wire-r2-verifier-text-sha256-test
    futon3c.diagramprover.wm-wire-r2-verifier-want-span-test
    futon3c.diagramprover.wm-wire-r7-fold-selection-test
    futon3c.diagramprover.wm-wire-r9-candidate-enact-test
    futon3c.diagramprover.wm-wire-gate-refuse-gate-refusal-read-error-test
    futon3c.diagramprover.wm-wire-r9-close-cause-failure-cause-record-test-test
    futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-cause-read-failure-cause-test
    futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-store-failure-cause-test
    futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-abstention-carrier-judge-refusal-test
    futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-judge-refusal-test-judge-refusal-test
    futon3c.diagramprover.wm-wire-r9-judge-refusal-abstention-r9-failure-classifier-outcome-test
    futon3c.diagramprover.wm-wire-r9-phase-kind-phase-kind-test-failure-kind-test
    futon3c.diagramprover.wm-wire-r9-phase-kind-r9-failure-classifier-failure-kind-test
    futon3c.diagramprover.wm-wire-c8-registry-get-entry-message-test
    futon3c.diagramprover.wm-wire-c8-registry-get-entry-timeout-ms-test
    futon3c.diagramprover.wm-wire-c8-registry-get-latest-message-test
    futon3c.diagramprover.wm-wire-c8-registry-get-latest-timeout-ms-test
    futon3c.diagramprover.wm-wire-r9-classify-target-decision-class-test
    futon3c.diagramprover.wm-wire-r9-classify-target-relation-test-class-test
    futon3c.diagramprover.wm-wire-r9-decision-class-model-target-class-test
    futon3c.diagramprover.wm-wire-r9-embedding-neighbour-classify-target-derived-via-test
    futon3c.diagramprover.wm-wire-r9-embedding-neighbour-relation-test-derived-via-test
    futon3c.diagramprover.wm-wire-r9-selection-law-decision-per-policy-argmax-test
    futon3c.diagramprover.wm-wire-r9-selection-law-r9-test-candidate-test
    futon3c.diagramprover.wm-wire-r9-selection-law-r9-test-enacted-steps-test
    futon3c.diagramprover.wm-wire-r9-selection-law-r9-test-per-policy-argmax-test
    futon3c.diagramprover.wm-wire-r10-publication-observed-test
    futon3c.diagramprover.wm-wire-r7-flight-call-increment-test
    futon3c.diagramprover.wm-wire-r7-flight-call-wc-test
    futon3c.diagramprover.wm-wire-r7-fold-call-enactment-fold-test
    futon3c.diagramprover.wm-wire-r7-increment-call-delta-test
    futon3c.diagramprover.wm-wire-r7-increment-call-wc-failures-test
    futon3c.diagramprover.wm-wire-r7-increment-fold-delta-test
    futon3c.diagramprover.wm-wire-r7-increment-r7-test-delta-test
    futon3c.diagramprover.wm-wire-r7-selection-e-source-test
    futon3c.diagramprover.wm-wire-eligibility-r1-outer-cascade-eligible-test
    futon3c.diagramprover.wm-wire-flight-entry-loop-test-target-source-test
    futon3c.diagramprover.wm-wire-loop-entry-r1-outer-cascade-trigger-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-chosen-target-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-flight-entry-draw-seed-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-test-draw-seed-test
    futon3c.diagramprover.wm-wire-r1-outer-cascade-r1-test-draw-seed-test
    futon3c.diagramprover.wm-wire-r1-target-field-r1-outer-cascade-next-step-test
    futon3c.diagramprover.wm-wire-r8-overlap-r1-outer-cascade-pair-overlap-test
    futon3c.diagramprover.wm-wire-r8-overlap-r8-test-pair-overlap-test
    futon3c.diagramprover.wm-wire-r0-test-attempts-test
    futon3c.diagramprover.wm-wire-r0-test-grain-gate-test
    futon3c.diagramprover.wm-wire-r0-r5-test-attempts-test
    futon3c.diagramprover.wm-wire-r0-r5-test-grain-gate-test
    futon3c.diagramprover.wm-wire-r0-wc-attempts-test
    futon3c.diagramprover.wm-wire-r0-wc-grain-gate-test
    futon3c.diagramprover.wm-wire-r5-gate-grain-test
    futon3c.diagramprover.wm-wire-r5-test-grain-test
    futon3c.diagramprover.wm-wire-construct-order-use-receipt-test
    futon3c.diagramprover.wm-wire-constructor-coapply-descent-test
    futon3c.diagramprover.wm-wire-constructor-coapply-units-test
    futon3c.diagramprover.wm-wire-constructor-order-use-descent-test
    futon3c.diagramprover.wm-wire-constructor-order-use-precedence-violations-test
    futon3c.diagramprover.wm-wire-constructor-order-use-units-test
    futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-sources-wants-test
    futon3c.diagramprover.wm-wire-flight-assembly-assemble-one-universes-test
    futon3c.diagramprover.wm-wire-judge-observation-channel-pe-test
    futon3c.diagramprover.wm-wire-prediction-error-weighted-error-test
    futon3c.diagramprover.wm-wire-weighted-error-aggregate-driver-test
    futon3c.diagramprover.wm-wire-gate-refuse-gate-refusal-read-error-test
    futon3c.diagramprover.wm-wire-r9-close-cause-failure-cause-record-test-test
    futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-cause-read-failure-cause-test
    futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-store-failure-cause-test
    futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-abstention-carrier-judge-refusal-test
    futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-judge-refusal-test-judge-refusal-test
    futon3c.diagramprover.wm-wire-r9-judge-refusal-abstention-r9-failure-classifier-outcome-test
    futon3c.diagramprover.wm-wire-r9-phase-kind-phase-kind-test-failure-kind-test
    futon3c.diagramprover.wm-wire-r9-phase-kind-r9-failure-classifier-failure-kind-test
    futon3c.diagramprover.wm-wire-flight-cast-click-start-author-test
    futon3c.diagramprover.wm-wire-flight-cast-click-start-reviewer-test
    futon3c.diagramprover.wm-wire-flight-cast-click-start-repair-reviewer-test
    futon3c.diagramprover.wm-wire-flight-click-flight-cast-test-cast-test
    futon3c.diagramprover.wm-wire-flight-click-flight-record-click-cast-test
    futon3c.diagramprover.wm-wire-flight-click-flight-record-click-detail-test
    futon3c.diagramprover.wm-wire-flight-click-flight-record-click-status-test
    futon3c.diagramprover.wm-wire-flight-record-summary-click-reason-test-failure-test
    futon3c.diagramprover.wm-wire-flight-record-summary-flight-click-close-test-chosen-test
    futon3c.diagramprover.wm-wire-flight-record-summary-flight-record-click-chosen-test
    futon3c.diagramprover.wm-wire-flight-record-summary-flight-record-click-failure-test
    futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-needs-test
    futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-readings-test])

(def adjacency-rev "1103e7a3")
(def adjacency-path "holes/labs/M-wm-wiring/wm-adjacency.edn")
(def map-rev "54f7e7cd")
(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def ledger-path "holes/labs/M-wm-wiring/wm-wire-ledger.edn")

(defn- git-show [rev path]
  (let [{:keys [exit out err]} (sh/sh "git" "show" (str rev ":" path))]
    (when-not (zero? exit) (throw (ex-info "git show failed" {:rev rev :path path :err err})))
    out))

(defn adjacency [] (edn/read-string (git-show adjacency-rev adjacency-path)))

(defn wires
  "The matrix's wires, [a b f], sorted."
  [adj]
  (vec (sort-by pr-str (for [[[a b] fs] (:matrix adj) f fs] [a b f]))))

(defn registered []
  (into {} (for [n wire-test-nses :let [wire @(ns-resolve n 'wire)]] [(:wire wire) wire])))

(defn ledger []
  (let [adj (adjacency)
        reg (registered)
        entries (vec (for [wire (wires adj)
                           :let [r (get reg wire)
                                 ok? (and r (w/received? ((:check r))))]]
                       (cond-> {:wire wire :status (if ok? (:kind r) :unverified)}
                         (and ok? (= :verified (:kind r))) (assoc :record (:record r))
                         ok? (assoc :test (:test r))
                         (:note r) (assoc :note (:note r))
                         (and r (not ok?)) (assoc :registered-test-failed (:test r)))))]
    {:adjacency {:path adjacency-path :rev adjacency-rev :map (:map adj)}
     :definition 'futon3c.diagramprover.wm-wire-ledger-test
     :counts (merge {:verified 0 :witnessed-hermetically 0 :unverified 0}
                    (frequencies (map :status entries))
                    {:wires (count entries)})
     :wires entries}))

(deftest the-matrix-is-the-maps-at-its-pin
  (let [adj (adjacency)
        boxes (into {} (map (juxt :box/id identity)) (:boxes (edn/read-string (git-show map-rev map-path))))]
    (is (= map-rev (:map adj)))
    (is (= 173 (:wires adj) (count (wires adj))))
    (doseq [[a b f] (wires adj)]
      (is (some #{f} (:writes (boxes a))) (pr-str [a b f]))
      (is (some #{f} (:reads (boxes b))) (pr-str [a b f])))))

(deftest every-registered-wire-is-in-the-matrix
  (let [ws (set (wires (adjacency)))]
    (doseq [wire (keys (registered))] (is (ws wire) (pr-str wire)))))

(deftest the-ledger
  (let [l (ledger)
        c (:counts l)]
    (spit ledger-path (with-out-str (pp/pprint l)))
    (is (= l (edn/read-string (slurp ledger-path))) "the ledger on disk is the one computed")
    (is (= 173 (:wires c) (+ (:verified c) (:witnessed-hermetically c) (:unverified c))))
    (is (= (frequencies (map :status (:wires l)))
           (select-keys c (keys (frequencies (map :status (:wires l)))))))
    (doseq [[wire r] (registered)]
      (is (= (:kind r) (:status (first (filter #(= wire (:wire %)) (:wires l)))))
          (str wire " is recorded as its test found it")))))

(def coverage-path "holes/labs/M-wm-wiring/wm-wire-coverage.edn")

(defn coverage-join-problems
  "What is wrong with joining COV (wm-wire-coverage.edn) to the ledger's wires at
  map AT-MAP: [] when every ledger wire has exactly one class at the same map."
  [ledger-wires at-map cov]
  (let [rows (:wires cov)
        by-wire (group-by :wire rows)
        classes #{:witness :conditional :failure-path :unreachable}]
    (vec (concat
          (when (not= at-map (get-in cov [:inputs :ledger :map]))
            [[:coverage-at-another-map (get-in cov [:inputs :ledger :map]) :ledger-at at-map]])
          (for [w ledger-wires :when (not (by-wire w))] [:ledger-wire-without-a-class w])
          (for [w (keys by-wire) :when (not ((set ledger-wires) w))] [:class-for-a-wire-not-in-the-ledger w])
          (for [[w rs] by-wire :when (< 1 (count rs))] [:wire-classified-twice w])
          (for [r rows :when (not (classes (:coverage r)))] [:no-class (:wire r)])
          (when (not= (frequencies (map :coverage rows)) (into {} (dissoc (:counts cov) :wires)))
            [[:counts-do-not-match-the-rows]])))))

(deftest every-ledger-wire-has-a-coverage-class-at-the-same-map
  ;; ORG-PATHS-I (PROOF-2a-PLAN <3>0): the ledger and the coverage partition are two
  ;; files with one writer each, joined here, not by a :coverage field in the ledger's
  ;; rows. wm-wire-coverage.edn is generated by spike/wm_coverage.bb from the ledger and
  ;; the organisation layer; when the matrix moves (a new adjacency-rev: new wires or a
  ;; new map) it must be regenerated in the same change, and this refuses the state
  ;; where it was not.
  (let [adj (adjacency)
        ledger-wires (wires adj)
        cov (edn/read-string {:default (fn [_ v] v)} (slurp coverage-path))]
    (is (= [] (coverage-join-problems ledger-wires (:map adj) cov)))
    (is (= (count ledger-wires) (get-in cov [:inputs :ledger :wires]) (count (:wires cov))))
    (is (= (count (:wires cov)) (reduce + (vals (dissoc (:counts cov) :wires)))) "the counts sum to the wire count")
    (testing "planted: each way the join can be wrong is refused"
      (let [w (first ledger-wires)]
        (is (some #{[:ledger-wire-without-a-class w]}
                  (coverage-join-problems ledger-wires (:map adj) (update cov :wires #(vec (remove (fn [r] (= w (:wire r))) %)))))
            "a ledger wire with no coverage row")
        (is (seq (coverage-join-problems (conj ledger-wires [:x :y :z]) (:map adj) cov))
            "a wire added to the ledger since the coverage was derived")
        (is (seq (coverage-join-problems ledger-wires "0000000" cov)) "the coverage at another map revision")
        (is (seq (coverage-join-problems ledger-wires (:map adj) (update-in cov [:wires 0] dissoc :coverage))) "a row with no class")))))
