(ns futon3c.peripheral.proof-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.store :as store]
            [futon3c.peripheral.proof :as proof]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- make-proof-mock
  "Create a mock backend with proof tool results pre-configured."
  ([]
   (make-proof-mock {}))
  ([extra-results]
   (tools/make-mock-backend
    (merge
     {:proof-load {:proof/problem-id "P-test"
                   :proof/version 1
                   :proof/canonical {:statement "Prove X"
                                     :closure-criterion "All cases"
                                     :statement-hash "sha256:abc"
                                     :version-history []}
                   :proof/ledger {"L-main" {:item/id "L-main"
                                            :item/label "Main"
                                            :item/status :open
                                            :item/depends-on #{}
                                            :item/unlocks #{}
                                            :item/artifact-paths []}}
                   :proof/cycles []
                   :proof/failed-routes []
                   :proof/updated-at "2024-01-01T00:00:00Z"}
      :ledger-query {"L-main" {:item/id "L-main"
                                :item/label "Main"
                                :item/status :open}}
      :dag-impact [{:item-id "L-main" :score 0 :unlocks #{}}]
      :cycle-begin {:cycle/id "P-test-C001"
                    :cycle/blocker-id "L-main"
                    :cycle/phase :observe
                    :cycle/phases-completed []
                    :cycle/phase-data {}
                    :cycle/started-at "2024-01-01T00:00:00Z"
                    :cycle/updated-at "2024-01-01T00:00:00Z"}
      :cycle-advance {:cycle/id "P-test-C001"
                      :cycle/blocker-id "L-main"
                      :cycle/phase :propose
                      :cycle/phases-completed [:observe]
                      :cycle/phase-data {:observe {:blocker-id "L-main"}}
                      :cycle/started-at "2024-01-01T00:00:00Z"
                      :cycle/updated-at "2024-01-01T00:00:00Z"}
      :read "file contents"
      :grep [{:file "src/a.clj" :line 1 :content "match"}]}
     extra-results))))

;; =============================================================================
;; Peripheral start/stop lifecycle
;; =============================================================================

(deftest proof-start-produces-goal-evidence
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-proof-1" :agent-id "claude-1"})]
    (is (:ok start))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (is (= :coordination (get-in start [:evidence :evidence/type])))))

(deftest proof-stop-returns-context
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-proof-2" :problem-id "P4"})
        stop (runner/stop p (:state start) "cycle completed")]
    (is (:ok stop))
    (is (= "sess-proof-2" (get-in stop [:context :session-id])))
    (is (= "P4" (get-in stop [:context :problem-id])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))))

;; =============================================================================
;; Phase gating — tool restrictions per phase
;; =============================================================================

(deftest setup-phase-allows-proof-tools
  (testing "no active cycle allows setup tools"
    (let [p (proof/make-proof (make-proof-mock))
          start (runner/start p {:session-id "sess-phase-1"})
          ;; proof-load should work in setup
          step (runner/step p (:state start) {:tool :proof-load :args ["P-test"]})]
      (is (:ok step)))))

(deftest setup-phase-allows-read-tools
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-phase-2"})
        step (runner/step p (:state start) {:tool :read :args ["src/a.clj"]})]
    (is (:ok step))))

(deftest observe-phase-rejects-write-tools
  (testing "after cycle-begin, phase is :observe which doesn't allow :write"
    (let [p (proof/make-proof (make-proof-mock))
          start (runner/start p {:session-id "sess-phase-3"})
          ;; Begin a cycle — enters :observe phase
          cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["P-test" "L-main"]})
          ;; Try to write in observe phase — should be rejected
          write-step (runner/step p (:state cycle-step) {:tool :write :args ["foo.txt" "content"]})]
      (is (:ok cycle-step))
      (is (= :observe (get-in cycle-step [:state :current-phase])))
      (fix/assert-valid! shapes/SocialError write-step)
      (is (= :phase-tool-not-allowed (:error/code write-step))))))

(deftest observe-phase-allows-read-tools
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-phase-4"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["P-test" "L-main"]})
        read-step (runner/step p (:state cycle-step) {:tool :read :args ["src/a.clj"]})]
    (is (:ok read-step))))

(deftest observe-phase-allows-ledger-query
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-phase-5"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["P-test" "L-main"]})
        query-step (runner/step p (:state cycle-step) {:tool :ledger-query :args ["P-test"]})]
    (is (:ok query-step))))

;; =============================================================================
;; Phase transitions via cycle-advance
;; =============================================================================

(deftest cycle-advance-updates-phase
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-advance-1"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["P-test" "L-main"]})
        ;; Advance from observe to propose
        advance-step (runner/step p (:state cycle-step) {:tool :cycle-advance :args ["P-test" "P-test-C001" {:blocker-id "L-main"}]})]
    (is (:ok advance-step))
    ;; State should reflect the new phase
    (is (= :propose (get-in advance-step [:state :current-phase])))))

;; =============================================================================
;; Cycle completion clears active phase
;; =============================================================================

(deftest cycle-completion-clears-phase
  (let [;; Mock backend that returns :completed phase on advance
        backend (tools/make-mock-backend
                 {:cycle-begin {:cycle/id "C1"
                                :cycle/blocker-id "L-b"
                                :cycle/phase :observe
                                :cycle/phases-completed []
                                :cycle/phase-data {}
                                :cycle/started-at "t"
                                :cycle/updated-at "t"}
                  :cycle-advance (fn [_tool _args]
                                   {:ok true
                                    :result {:cycle/id "C1"
                                             :cycle/blocker-id "L-b"
                                             :cycle/phase :completed
                                             :cycle/phases-completed [:observe :propose :execute
                                                                      :validate :classify :integrate
                                                                      :commit :gate-review]
                                             :cycle/phase-data {}
                                             :cycle/result-status :proved
                                             :cycle/started-at "t"
                                             :cycle/updated-at "t"}})
                  :ledger-query {}
                  :dag-impact []})
        p (proof/make-proof backend)
        start (runner/start p {:session-id "sess-complete-1"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["P-test" "L-b"]})
        ;; Simulate advancing to completed
        advance (runner/step p (:state cycle-step) {:tool :cycle-advance :args ["P-test" "C1" {:gates-passed true}]})]
    (is (:ok advance))
    ;; Phase should be cleared (back to setup)
    (is (nil? (get-in advance [:state :current-phase])))
    (is (= 1 (get-in advance [:state :cycles-completed])))))

;; =============================================================================
;; Evidence accumulation
;; =============================================================================

(deftest proof-accumulates-evidence
  (let [evidence-store (atom {:entries {} :order []})
        p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-ev-1" :evidence-store evidence-store})
        step (runner/step p (:state start) {:tool :ledger-query :args ["P-test"]})
        stop (runner/stop p (:state step) "done")
        entries (store/query* evidence-store {})]
    (is (:ok start))
    (is (:ok step))
    (is (:ok stop))
    (is (= 3 (count entries)))
    (is (= [:goal :step :conclusion]
           (mapv :evidence/claim-type (sort-by :evidence/at entries))))
    (let [step-entry (first (filter #(= :step (:evidence/claim-type %)) entries))]
      (is (= :ledger-query (get-in step-entry [:evidence/body :tool])))
      (is (= :observe (get-in step-entry [:evidence/body :proof/operation-kind]))))))

(deftest proof-step-evidence-tags-action-tools
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-ev-action-1"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["P-test" "L-main"]})]
    (is (:ok cycle-step))
    (is (= :cycle-begin (get-in cycle-step [:evidence :evidence/body :tool])))
    (is (= :action (get-in cycle-step [:evidence :evidence/body :proof/operation-kind])))))

;; =============================================================================
;; Rejects tools outside peripheral spec
;; =============================================================================

(deftest proof-rejects-unknown-tools
  (let [p (proof/make-proof (make-proof-mock))
        start (runner/start p {:session-id "sess-reject-1"})
        step (runner/step p (:state start) {:tool :bash-deploy :args ["deploy.sh"]})]
    ;; :bash-deploy is not in proof's tool set
    (fix/assert-valid! shapes/SocialError step)))

;; =============================================================================
;; Full cycle walk (with mock)
;; =============================================================================

(deftest full-cycle-walk-mock
  (testing "walk through cycle phases from setup → cycle-begin → advance"
    (let [p (proof/make-proof (make-proof-mock))
          start (runner/start p {:session-id "sess-walk-1" :problem-id "P-test"})
          ;; Load proof state
          load-step (runner/step p (:state start) {:tool :proof-load :args ["P-test"]})
          ;; Query ledger
          query-step (runner/step p (:state load-step) {:tool :ledger-query :args ["P-test"]})
          ;; Begin cycle
          cycle-step (runner/step p (:state query-step) {:tool :cycle-begin :args ["P-test" "L-main"]})
          ;; Advance observe → propose
          advance-step (runner/step p (:state cycle-step) {:tool :cycle-advance :args ["P-test" "P-test-C001" {:blocker-id "L-main"}]})]
      (is (:ok start))
      (is (:ok load-step))
      (is (:ok query-step))
      (is (:ok cycle-step))
      (is (= :observe (get-in cycle-step [:state :current-phase])))
      (is (:ok advance-step))
      (is (= :propose (get-in advance-step [:state :current-phase]))))))

;; =============================================================================
;; Proof shapes validation
;; =============================================================================

(deftest phase-order-is-complete
  (is (= 9 (count ps/phase-order)))
  (is (= :observe (first ps/phase-order)))
  (is (= :completed (last ps/phase-order))))

(deftest phase-transitions-cover-all-phases
  (doseq [phase (butlast ps/phase-order)]
    (is (contains? ps/phase-transitions phase)
        (str "Missing transition for phase: " phase))))

(deftest phase-allowed-tools-cover-all-phases
  (doseq [phase ps/phase-order]
    (is (contains? ps/phase-allowed-tools phase)
        (str "Missing tool set for phase: " phase))))

(deftest proof-tool-operation-classification-covers-runtime-tools
  (let [setup-tools #{:proof-load :proof-save :ledger-query :ledger-upsert
                      :dag-check :dag-impact :canonical-get :canonical-update
                      :cycle-begin :cycle-list :cycle-get :failed-route-add
                      :read :glob :grep :bash-readonly}
        phase-tools (apply set/union #{} (vals ps/phase-allowed-tools))
        runtime-tools (set/union setup-tools phase-tools)
        unclassified (remove ps/tool-operation-kind runtime-tools)
        invalid-kinds (remove #{:observe :action}
                              (map ps/tool-operation-kind runtime-tools))]
    (is (empty? unclassified)
        (str "Every proof runtime tool must be classified: " (vec unclassified)))
    (is (empty? invalid-kinds)
        (str "Classification must be :observe or :action: " (vec invalid-kinds)))))

(deftest status-validation-rules
  (testing "valid transitions"
    (is (ps/valid-status-transition? :open :partial :analytical))
    (is (ps/valid-status-transition? :open :proved :analytical))
    (is (ps/valid-status-transition? :open :numerically-verified :numerical)))
  (testing "invalid transitions"
    (is (not (ps/valid-status-transition? :open :proved :numerical)))
    (is (not (ps/valid-status-transition? :false :open :analytical)))
    (is (not (ps/valid-status-transition? :open :basically-solved :analytical)))))
