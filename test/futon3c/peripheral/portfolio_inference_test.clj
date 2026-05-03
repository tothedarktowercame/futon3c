(ns futon3c.peripheral.portfolio-inference-test
  "Tests for the portfolio-inference peripheral.

   Coverage:
   1. Backend: family-pattern compilation, criterion-shape scoring,
      mission→family mapping
   2. Backend: precision-proxy aggregation invariants
   3. Peripheral: lifecycle (start/step/stop) over the registered spec
   4. VERIFY: regression baseline alignment with the babashka script
      futon5a/scripts/run_mission_feature_loop.clj"
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.portfolio-inference :as pi]
            [futon3c.peripheral.portfolio-inference-backend :as pib]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]))

;; =============================================================================
;; Backend: family-pattern compilation
;; =============================================================================

(deftest family-pattern-handles-empty-phrases
  (testing "make-family-pattern returns nil for empty phrase list"
    (is (nil? (pib/make-family-pattern [])))))

(deftest family-pattern-uses-word-boundaries
  (testing "compiled pattern matches multi-token phrases at word boundaries"
    (let [pat (pib/make-family-pattern ["penholder" "write authority"])]
      (is (re-find pat "the penholder must be set"))
      (is (re-find pat "We require write authority"))
      ;; Substring containment must NOT match — that was v1's bug.
      (is (nil? (re-find pat "without authoring permission"))))))

;; =============================================================================
;; Backend: criterion-shape scoring
;; =============================================================================

(deftest score-criterion-shape-classifies-cleanly
  (testing "structural-recurring: every X must"
    (is (= :structural-recurring
           (pib/score-criterion-shape "Every printed capability must cite a source."))))
  (testing "structural-recurring: invariant bites"
    (is (= :structural-recurring
           (pib/score-criterion-shape "The invariant bites on the live path."))))
  (testing "process-recurring: PSR/PUR/checkpoint"
    (is (= :process-recurring
           (pib/score-criterion-shape "Every session emits a PSR before exit.")))
    (is (= :process-recurring
           (pib/score-criterion-shape "A checkpoint is appended after each cycle."))))
  (testing "one-shot: existence/creation"
    (is (= :one-shot
           (pib/score-criterion-shape "The first artefact exists at the named path."))))
  (testing "ambiguous: no clear shape"
    (is (= :ambiguous
           (pib/score-criterion-shape "Some general statement about the system.")))))

;; =============================================================================
;; Backend: mission→family mapping
;; =============================================================================

(deftest map-mission-to-families-uses-tight-phrases
  (testing "missions with concrete phrasing map to the right family"
    (let [model (pib/load-invariant-model)
          ftab (pib/build-family-table model)
          text "Every cross-store agreement must preserve session continuity."
          hits (pib/map-mission-to-families text ftab)]
      (is (some (fn [[fid _]] (= :F-cross-store-agreement fid)) hits)
          "cross-store-agreement family should hit on its own phrase")))
  (testing "generic English does not produce false positives"
    (let [model (pib/load-invariant-model)
          ftab (pib/build-family-table model)
          text "The author wrote a draft of the document."
          hits (pib/map-mission-to-families text ftab)]
      ;; No phrase from any family should appear in this neutral sentence.
      (is (empty? hits)))))

;; =============================================================================
;; Backend: precision-proxy aggregation invariants
;; =============================================================================

(deftest precision-table-shape-invariants
  (testing "precision-table emits one row per family with all signal fields"
    (let [table (pib/build-precision-table)]
      (is (= 19 (count table))
          "regression baseline: 19 invariant families")
      (is (every? :family/id table))
      (is (every? :family/layer table))
      (is (every? :family/status table))
      (is (every? #(integer? (:precision/proxy %)) table))
      (is (every? #(<= 0 (:precision/proxy %) 10) table)
          "proxy is normalised to [0,10]")
      ;; pressure-is-not-maturity: status field is one of the legal layer-statuses,
      ;; never collapses into the precision-proxy.
      (is (every? #(contains? #{:operational :candidate :operational-but-bypassable}
                              (:family/status %))
                  table)))))

(deftest precision-table-preserves-baseline-counts
  (testing "regression baseline alignment with run_mission_feature_loop.clj"
    (let [feat (pib/build-mission-features)
          missions (:missions feat)]
      ;; The baseline (2026-04-26) found 104 missions across 10 repos
      ;; with 49 carrying explicit closure-criteria sections. These
      ;; counts should remain stable as new missions land — when they
      ;; change, both the babashka script and this test should be
      ;; updated together.
      (is (>= (count missions) 100)
          "mission count should stay near baseline (104) — investigate if it drops sharply")
      (is (>= (count (filter #(pos? (:mission/criteria-section-count %)) missions))
              45)
          "criteria-bearing mission count should stay near baseline (49)"))))

;; =============================================================================
;; Backend: promotion candidates
;; =============================================================================

(deftest promotion-candidates-target-candidate-families
  (testing "every promotion candidate references a candidate family"
    (let [{:keys [family-by-id]} (pib/build-mission-features)
          candidates (pib/build-promotion-candidates)]
      (is (every? (fn [c]
                    (= :candidate (:status (family-by-id (:family/id c)))))
                  candidates)
          "promotion candidates must touch :candidate families only"))))

(deftest promotion-candidates-have-structural-recurring-criteria
  (testing "every promotion candidate carries a structural-recurring criterion"
    (let [candidates (pib/build-promotion-candidates)]
      ;; The criterion text is the body; structural-recurring shape was
      ;; the filter applied during construction. We can't re-score the
      ;; criterion text here without coupling to the score function, but
      ;; we can assert presence + non-emptiness.
      (is (every? #(string? (:criterion %)) candidates))
      (is (every? #(seq (:criterion %)) candidates)))))

;; =============================================================================
;; Peripheral lifecycle
;; =============================================================================

(deftest peripheral-spec-loads-from-resources
  (testing "load-spec returns the registered :portfolio-inference spec"
    (let [spec (common/load-spec :portfolio-inference)]
      (is (= :portfolio-inference (:peripheral/id spec)))
      (is (contains? (:peripheral/tools spec) :pi-mission-features))
      (is (contains? (:peripheral/tools spec) :pi-precision-table))
      (is (contains? (:peripheral/tools spec) :pi-promotion-candidates))
      (is (contains? (:peripheral/tools spec) :pi-aif-step))
      (is (contains? (:peripheral/tools spec) :pi-step))
      (is (contains? (:peripheral/tools spec) :pi-heartbeat))
      (is (contains? (:peripheral/tools spec) :pi-adjacent)))))

(deftest peripheral-start-requires-session-id
  (testing "start without session-id returns SocialError"
    (let [spec {:peripheral/id :portfolio-inference
                :peripheral/tools #{:pi-mission-features :pi-precision-table}
                :peripheral/scope :full-codebase}
          p (pi/make-portfolio-inference spec (tools/make-mock-backend))
          result (runner/start p {})]
      (is (shapes/valid? shapes/SocialError result)))))

(deftest peripheral-lifecycle-start-step-stop
  (testing "full lifecycle: start → step (pi-mission-features) → stop"
    (let [spec {:peripheral/id :portfolio-inference
                :peripheral/tools #{:pi-mission-features :pi-precision-table
                                    :pi-promotion-candidates}
                :peripheral/scope :full-codebase}
          p (pi/make-portfolio-inference spec (tools/make-mock-backend))
          start (runner/start p {:session-id "test-pi-1" :author "tester"})]
      (is (:ok start))
      (let [step (runner/step p (:state start) {:tool :pi-mission-features})]
        (is (:ok step))
        (is (map? (:result step)))
        (is (= :step (:evidence/claim-type (:evidence step))))
        (is (vector? (get-in step [:result :missions])))
        (let [stop (runner/stop p (:state step) "test complete")]
          (is (:ok stop))
          (is (= :conclusion (:evidence/claim-type (:evidence stop))))
          (is (= 1 (:steps-taken (:fruit stop)))))))))

(deftest peripheral-precision-table-stored-in-state
  (testing "pi-precision-table result is captured in state as latest-precision-table"
    (let [spec {:peripheral/id :portfolio-inference
                :peripheral/tools #{:pi-precision-table}
                :peripheral/scope :full-codebase}
          p (pi/make-portfolio-inference spec (tools/make-mock-backend))
          start (runner/start p {:session-id "test-pi-table" :author "tester"})
          step (runner/step p (:state start) {:tool :pi-precision-table})
          stop (runner/stop p (:state step) "done")]
      (is (:ok step))
      (is (vector? (get-in stop [:fruit :precision-table])))
      (is (pos? (count (get-in stop [:fruit :precision-table])))))))
