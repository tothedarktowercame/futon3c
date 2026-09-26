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
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
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
            [futon3c.diagramprover.wm-wire-gate-refuse-gate-refusal-read-error-test]
            [futon3c.diagramprover.wm-wire-r9-close-cause-failure-cause-record-test-test]
            [futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-cause-read-failure-cause-test]
            [futon3c.diagramprover.wm-wire-r9-close-cause-r9-finding-store-failure-cause-test]
            [futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-abstention-carrier-judge-refusal-test]
            [futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-judge-refusal-test-judge-refusal-test]
            [futon3c.diagramprover.wm-wire-r9-judge-refusal-abstention-r9-failure-classifier-outcome-test]
            [futon3c.diagramprover.wm-wire-r9-phase-kind-phase-kind-test-failure-kind-test]
            [futon3c.diagramprover.wm-wire-r9-phase-kind-r9-failure-classifier-failure-kind-test]
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
  '[futon3c.diagramprover.wm-wire-r2-flight-read-text-sha256-test
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

(def adjacency-rev "84cd6426")
(def adjacency-path "holes/labs/M-wm-wiring/wm-adjacency.edn")
(def map-rev "06d451d6")
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
    (is (= 140 (:wires adj) (count (wires adj))))
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
    (is (= 140 (:wires c) (+ (:verified c) (:witnessed-hermetically c) (:unverified c))))
    (is (= (frequencies (map :status (:wires l)))
           (select-keys c (keys (frequencies (map :status (:wires l)))))))
    (doseq [[wire r] (registered)]
      (is (= (:kind r) (:status (first (filter #(= wire (:wire %)) (:wires l)))))
          (str wire " is recorded as its test found it")))))
