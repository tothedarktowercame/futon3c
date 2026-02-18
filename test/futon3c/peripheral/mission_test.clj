(ns futon3c.peripheral.mission-test
  "Tests for the mission peripheral — cycle machine + mission domain config."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.threads :as threads]
            [futon3c.peripheral.cycle :as cyc]
            [futon3c.peripheral.mission :as mission]
            [futon3c.peripheral.mission-shapes :as ms]
            [futon3c.peripheral.proof-shapes :as ps]
            [futon3c.peripheral.round-trip :as rt]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix])
  (:import [java.time Instant]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- make-mission-mock
  "Create a mock backend with mission tool results pre-configured."
  ([]
   (make-mission-mock {}))
  ([extra-results]
   (tools/make-mock-backend
    (merge
     {:mission-load {:mission/id "M-test"
                     :mission/version 1
                     :mission/spec {:title "Test Mission"
                                    :success-criteria ["Pass all tests"]
                                    :scope-in ["src/"]
                                    :scope-out ["vendor/"]
                                    :version 1
                                    :version-history []}
                     :mission/obligations {"O-main" {:item/id "O-main"
                                                      :item/label "Main task"
                                                      :item/status :open
                                                      :item/depends-on #{}
                                                      :item/unlocks #{}
                                                      :item/artifact-paths []}}
                     :mission/cycles []
                     :mission/failed-approaches []
                     :mission/updated-at "2024-01-01T00:00:00Z"}
      :obligation-query {"O-main" {:item/id "O-main"
                                    :item/label "Main task"
                                    :item/status :open}}
      :dag-impact [{:item-id "O-main" :score 0 :unlocks #{}}]
      :cycle-begin {:cycle/id "M-test-C001"
                    :cycle/blocker-id "O-main"
                    :cycle/phase :observe
                    :cycle/phases-completed []
                    :cycle/phase-data {}
                    :cycle/started-at "2024-01-01T00:00:00Z"
                    :cycle/updated-at "2024-01-01T00:00:00Z"}
      :cycle-advance {:cycle/id "M-test-C001"
                      :cycle/blocker-id "O-main"
                      :cycle/phase :propose
                      :cycle/phases-completed [:observe]
                      :cycle/phase-data {:observe {:blocker-id "O-main"}}
                      :cycle/started-at "2024-01-01T00:00:00Z"
                      :cycle/updated-at "2024-01-01T00:00:00Z"}
      :evidence-query {:entries [] :query nil}
      :read "file contents"
      :grep [{:file "src/a.clj" :line 1 :content "match"}]}
     extra-results))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(deftest mission-start-produces-goal-evidence
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-mission-1" :agent-id "claude-1"})]
    (is (:ok start))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))
    (is (= :goal (get-in start [:evidence :evidence/claim-type])))
    (is (= :coordination (get-in start [:evidence :evidence/type])))))

(deftest mission-stop-returns-context
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-mission-2" :mission-id "M-test"})
        stop (runner/stop p (:state start) "cycle completed")]
    (is (:ok stop))
    (is (= "sess-mission-2" (get-in stop [:context :session-id])))
    (is (= "M-test" (get-in stop [:context :mission-id])))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))))

;; =============================================================================
;; Phase gating
;; =============================================================================

(deftest setup-phase-allows-mission-tools
  (testing "no active cycle allows setup tools"
    (let [p (mission/make-mission (make-mission-mock))
          start (runner/start p {:session-id "sess-phase-1"})
          step (runner/step p (:state start) {:tool :mission-load :args ["M-test"]})]
      (is (:ok step)))))

(deftest setup-phase-allows-read-tools
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-phase-2"})
        step (runner/step p (:state start) {:tool :read :args ["src/a.clj"]})]
    (is (:ok step))))

(deftest observe-phase-rejects-write-tools
  (testing "after cycle-begin, phase is :observe which doesn't allow :write"
    (let [p (mission/make-mission (make-mission-mock))
          start (runner/start p {:session-id "sess-phase-3"})
          cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "O-main"]})
          write-step (runner/step p (:state cycle-step) {:tool :write :args ["foo.txt" "content"]})]
      (is (:ok cycle-step))
      (is (= :observe (get-in cycle-step [:state :current-phase])))
      (fix/assert-valid! shapes/SocialError write-step)
      (is (= :phase-tool-not-allowed (:error/code write-step))))))

(deftest observe-phase-allows-obligation-query
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-phase-4"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "O-main"]})
        query-step (runner/step p (:state cycle-step) {:tool :obligation-query :args ["M-test"]})]
    (is (:ok query-step))))

(deftest observe-phase-allows-evidence-query
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-phase-5"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "O-main"]})
        step (runner/step p (:state cycle-step) {:tool :evidence-query :args ["test query"]})]
    (is (:ok step))))

;; =============================================================================
;; Phase transitions
;; =============================================================================

(deftest cycle-advance-updates-phase
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-advance-1"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "O-main"]})
        advance-step (runner/step p (:state cycle-step)
                                  {:tool :cycle-advance
                                   :args ["M-test" "M-test-C001" {:blocker-id "O-main"}]})]
    (is (:ok advance-step))
    (is (= :propose (get-in advance-step [:state :current-phase])))))

(deftest cycle-completion-clears-phase
  (let [backend (tools/make-mock-backend
                 {:cycle-begin {:cycle/id "C1"
                                :cycle/blocker-id "O-b"
                                :cycle/phase :observe
                                :cycle/phases-completed []
                                :cycle/phase-data {}
                                :cycle/started-at "t"
                                :cycle/updated-at "t"}
                  :cycle-advance (fn [_tool _args]
                                   {:ok true
                                    :result {:cycle/id "C1"
                                             :cycle/blocker-id "O-b"
                                             :cycle/phase :completed
                                             :cycle/phases-completed [:observe :propose :execute
                                                                      :validate :classify :integrate
                                                                      :commit :gate-review]
                                             :cycle/phase-data {}
                                             :cycle/result-status :done
                                             :cycle/started-at "t"
                                             :cycle/updated-at "t"}})
                  :obligation-query {} :dag-impact []})
        p (mission/make-mission backend)
        start (runner/start p {:session-id "sess-complete-1"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "O-b"]})
        advance (runner/step p (:state cycle-step)
                             {:tool :cycle-advance
                              :args ["M-test" "C1" {:gates-passed true}]})]
    (is (:ok advance))
    (is (nil? (get-in advance [:state :current-phase])))
    (is (= 1 (get-in advance [:state :cycles-completed])))))

;; =============================================================================
;; Evidence accumulation with Table 25 tags
;; =============================================================================

(deftest mission-accumulates-evidence-with-tags
  (let [evidence-store (atom {:entries {} :order []})
        p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-ev-1" :evidence-store evidence-store})
        step (runner/step p (:state start) {:tool :obligation-query :args ["M-test"]})
        stop (runner/stop p (:state step) "done")
        entries (store/query* evidence-store {})]
    (is (:ok start))
    (is (:ok step))
    (is (:ok stop))
    (is (= 3 (count entries)))
    (is (= [:goal :step :conclusion]
           (mapv :evidence/claim-type (sort-by :evidence/at entries))))
    (let [step-entry (first (filter #(= :step (:evidence/claim-type %)) entries))]
      (is (= :obligation-query (get-in step-entry [:evidence/body :tool])))
      (is (= :observe (get-in step-entry [:evidence/body :mission/operation-kind]))))))

(deftest mission-observe-phase-adds-sigil-tags
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-sigil-1"})
        cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "O-main"]})
        ;; In :observe phase, should get Table 25 tags
        step (runner/step p (:state cycle-step) {:tool :obligation-query :args ["M-test"]})]
    (is (:ok step))
    (let [tags (get-in step [:evidence :evidence/tags])]
      (is (some #{:sigil/getting-information} tags))
      (is (some #{:sigil/perception} tags)))))

;; =============================================================================
;; Evidence landscape — state snapshots
;; =============================================================================

(deftest mission-save-emits-snapshot-evidence
  (testing "mission-save produces a snapshot evidence entry in the store"
    (let [evidence-store (atom {:entries {} :order []})
          save-result {:mission/id "M-test"
                       :mission/version 2
                       :mission/obligations {"O-main" {:item/id "O-main"
                                                        :item/label "Main task"
                                                        :item/status :partial
                                                        :item/depends-on #{}
                                                        :item/unlocks #{}
                                                        :item/artifact-paths []}}
                       :mission/cycles [{:cycle/id "C001"}]
                       :mission/failed-approaches []
                       :mission/updated-at "2024-01-01T01:00:00Z"}
          backend (tools/make-mock-backend
                   {:mission-load {:mission/id "M-test"
                                   :mission/version 1
                                   :mission/obligations {}
                                   :mission/cycles []
                                   :mission/failed-approaches []
                                   :mission/updated-at "t"}
                    :mission-save save-result})
          p (mission/make-mission backend)
          start (runner/start p {:session-id "sess-snap-1"
                                 :mission-id "M-test"
                                 :evidence-store evidence-store})
          load-step (runner/step p (:state start) {:tool :mission-load :args ["M-test"]})
          save-step (runner/step p (:state load-step) {:tool :mission-save :args ["M-test"]})
          entries (store/query* evidence-store {})]
      (is (:ok save-step))
      ;; Should have: start-evidence, load-step-evidence, save-step-evidence, snapshot-evidence
      (is (= 4 (count entries)))
      ;; The save step should have produced a snapshot evidence entry
      (is (some? (:snapshot-evidence save-step)))
      (let [snap (:snapshot-evidence save-step)]
        (fix/assert-valid! shapes/EvidenceEntry snap)
        (is (= :observation (:evidence/claim-type snap)))
        (is (= {:ref/type :mission :ref/id "M-test"} (:evidence/subject snap)))
        (is (= "M-test" (get-in snap [:evidence/body :mission/id])))
        (is (= 2 (get-in snap [:evidence/body :mission/version])))
        (is (= 1 (get-in snap [:evidence/body :obligations :total])))
        (is (= {:partial 1} (get-in snap [:evidence/body :obligations :by-status])))
        (is (= 1 (get-in snap [:evidence/body :cycles-count])))
        (is (some #{:snapshot} (:evidence/tags snap)))
        (is (some #{:mission/M-test} (:evidence/tags snap)))))))

(deftest non-save-tools-produce-no-snapshot
  (testing "tools other than mission-save do not produce snapshot evidence"
    (let [evidence-store (atom {:entries {} :order []})
          p (mission/make-mission (make-mission-mock))
          start (runner/start p {:session-id "sess-snap-2"
                                 :evidence-store evidence-store})
          step (runner/step p (:state start) {:tool :obligation-query :args ["M-test"]})]
      (is (:ok step))
      (is (nil? (:snapshot-evidence step)))
      ;; Only 2 entries: start + step (no snapshot)
      (is (= 2 (count (store/query* evidence-store {})))))))

;; =============================================================================
;; Mission shapes validation
;; =============================================================================

(deftest phase-order-is-complete
  (is (= 9 (count ms/phase-order)))
  (is (= :observe (first ms/phase-order)))
  (is (= :completed (last ms/phase-order))))

(deftest phase-allowed-tools-cover-all-phases
  (doseq [phase ms/phase-order]
    (is (contains? ms/phase-allowed-tools phase)
        (str "Missing tool set for phase: " phase))))

(deftest mission-tool-operation-classification-covers-runtime-tools
  (let [phase-tools (apply set/union #{} (vals ms/phase-allowed-tools))
        runtime-tools (set/union mission/setup-tools phase-tools)
        unclassified (remove ms/tool-operation-kind runtime-tools)
        invalid-kinds (remove #{:observe :action}
                              (map ms/tool-operation-kind runtime-tools))]
    (is (empty? unclassified)
        (str "Every mission runtime tool must be classified: " (vec unclassified)))
    (is (empty? invalid-kinds)
        (str "Classification must be :observe or :action: " (vec invalid-kinds)))))

(deftest status-validation-rules
  (testing "valid transitions"
    (is (ms/valid-status-transition? :open :partial :test))
    (is (ms/valid-status-transition? :open :done :test))
    (is (ms/valid-status-transition? :open :done :review)))
  (testing "invalid transitions"
    (is (not (ms/valid-status-transition? :open :done :assertion)))
    (is (not (ms/valid-status-transition? :abandoned :open :test)))
    (is (not (ms/valid-status-transition? :open :invalid-status :test)))))

;; =============================================================================
;; Domain config validity
;; =============================================================================

(deftest mission-domain-config-is-valid
  (is (cyc/valid-domain-config? mission/mission-domain-config)))

;; =============================================================================
;; Rejects tools outside peripheral spec
;; =============================================================================

(deftest mission-rejects-unknown-tools
  (let [p (mission/make-mission (make-mission-mock))
        start (runner/start p {:session-id "sess-reject-1"})
        step (runner/step p (:state start) {:tool :bash-deploy :args ["deploy.sh"]})]
    (fix/assert-valid! shapes/SocialError step)))

;; =============================================================================
;; Full cycle walk (with mock)
;; =============================================================================

(deftest full-cycle-walk-mock
  (testing "walk through cycle phases from setup → cycle-begin → advance"
    (let [p (mission/make-mission (make-mission-mock))
          start (runner/start p {:session-id "sess-walk-1" :mission-id "M-test"})
          load-step (runner/step p (:state start) {:tool :mission-load :args ["M-test"]})
          query-step (runner/step p (:state load-step) {:tool :obligation-query :args ["M-test"]})
          cycle-step (runner/step p (:state query-step) {:tool :cycle-begin :args ["M-test" "O-main"]})
          advance-step (runner/step p (:state cycle-step)
                                    {:tool :cycle-advance
                                     :args ["M-test" "M-test-C001" {:blocker-id "O-main"}]})]
      (is (:ok start))
      (is (:ok load-step))
      (is (:ok query-step))
      (is (:ok cycle-step))
      (is (= :observe (get-in cycle-step [:state :current-phase])))
      (is (:ok advance-step))
      (is (= :propose (get-in advance-step [:state :current-phase]))))))

;; =============================================================================
;; VERIFY — Proof expressibility as CycleDomainConfig
;; =============================================================================

(deftest proof-is-expressible-as-cycle-domain-config
  (testing "proof.clj's hardcoded behavior maps to a valid CycleDomainConfig"
    (let [proof-config {:domain-id :proof
                        :phase-order ps/phase-order
                        :phase-tools ps/phase-allowed-tools
                        :setup-tools #{:proof-load :proof-save :ledger-query :ledger-upsert
                                       :dag-check :dag-impact :canonical-get :canonical-update
                                       :cycle-begin :cycle-list :cycle-get :failed-route-add
                                       :read :glob :grep :bash-readonly}
                        :tool-ops ps/proof-tool-operation-kinds
                        :required-outputs ps/phase-required-outputs
                        :cycle-begin-tool :cycle-begin
                        :cycle-advance-tool :cycle-advance
                        :fruit-fn (fn [state]
                                    {:problem-id (:problem-id state)
                                     :cycles-completed (:cycles-completed state)
                                     :steps-taken (count (:steps state))
                                     :final-phase (:current-phase state)})
                        :exit-context-fn (fn [state]
                                           {:session-id (:session-id state)
                                            :problem-id (:problem-id state)})}]
      (is (cyc/valid-domain-config? proof-config)
          "proof's behavior is fully expressible as CycleDomainConfig"))))

(deftest proof-config-and-mission-config-share-structure
  (testing "proof and mission configs have same phase count and cycle tools"
    (is (= (count ps/phase-order) (count ms/phase-order))
        "same number of phases")
    (is (= (first ps/phase-order) (first ms/phase-order))
        "both start with :observe")
    (is (= (last ps/phase-order) (last ms/phase-order))
        "both end with :completed")
    (is (= (:cycle-begin-tool mission/mission-domain-config)
           :cycle-begin)
        "same cycle begin tool")
    (is (= (:cycle-advance-tool mission/mission-domain-config)
           :cycle-advance)
        "same cycle advance tool")))

;; =============================================================================
;; VERIFY — Round-trip (←) verification
;; =============================================================================

(deftest mission-passes-backward-verification
  (testing "mission peripheral evidence passes ← verification against spec"
    (let [p (mission/make-mission (make-mission-mock))
          result (rt/run-and-verify
                   p
                   {:session-id "sess-rt-1"}
                   [{:tool :mission-load :args ["M-test"]}
                    {:tool :obligation-query :args ["M-test"]}]
                   "verification complete")]
      ;; run-and-verify collects evidence and checks constraints
      (is (= 4 (count (:evidence result)))
          "start + 2 steps + stop")
      (is (= [:goal :step :step :conclusion]
             (mapv :evidence/claim-type (:evidence result)))))))

(deftest mission-backward-verification-with-cycle
  (testing "mission with active cycle passes ← verification"
    (let [p (mission/make-mission (make-mission-mock))
          result (rt/run-and-verify
                   p
                   {:session-id "sess-rt-2" :mission-id "M-test"}
                   [{:tool :mission-load :args ["M-test"]}
                    {:tool :cycle-begin :args ["M-test" "O-main"]}
                    {:tool :obligation-query :args ["M-test"]}
                    {:tool :cycle-advance
                     :args ["M-test" "M-test-C001" {:blocker-id "O-main"}]}]
                   "cycle in progress")]
      (is (= 6 (count (:evidence result)))
          "start + 4 steps + stop")
      (is (= [:goal :step :step :step :step :conclusion]
             (mapv :evidence/claim-type (:evidence result)))))))

;; =============================================================================
;; VERIFY — Proof-tree invariants on mission evidence
;; =============================================================================

(deftest mission-evidence-satisfies-proof-tree-invariants
  (testing "mission evidence chain satisfies all 7 proof-tree invariants"
    (let [evidence-store (atom {:entries {} :order []})
          p (mission/make-mission (make-mission-mock))
          session-id "sess-inv-1"
          start (runner/start p {:session-id session-id
                                 :mission-id "M-test"
                                 :evidence-store evidence-store})
          load-step (runner/step p (:state start)
                                 {:tool :mission-load :args ["M-test"]})
          query-step (runner/step p (:state load-step)
                                  {:tool :obligation-query :args ["M-test"]})
          cycle-step (runner/step p (:state query-step)
                                  {:tool :cycle-begin :args ["M-test" "O-main"]})
          advance-step (runner/step p (:state cycle-step)
                                    {:tool :cycle-advance
                                     :args ["M-test" "M-test-C001" {:blocker-id "O-main"}]})
          stop (runner/stop p (:state advance-step) "verification complete")
          subj {:ref/type :session :ref/id session-id}
          tp (threads/project-thread evidence-store subj)]

      (is (some? tp) "thread projection exists")

      (let [entries (:thread/entries tp)
            by-id (into {} (map (juxt :evidence/id identity) entries))]

        ;; Invariant 1: Tree validity — every in-reply-to references an entry
        (testing "invariant 1: tree validity"
          (doseq [e entries]
            (when-let [parent (:evidence/in-reply-to e)]
              (is (contains? by-id parent)
                  (str (:evidence/id e) " references missing " parent)))))

        ;; Invariant 2: Root invariant — goal entry exists
        (testing "invariant 2: root"
          (is (= :goal (:evidence/claim-type (:thread/goal tp)))))

        ;; Invariant 3: No orphans — all entries have session subject
        (testing "invariant 3: no orphans"
          (is (every? #(= subj (:evidence/subject %)) entries)))

        ;; Invariant 4: Claim ordering — no conclusion replies to conclusion
        (testing "invariant 4: claim ordering"
          (let [conclusions (filter #(= :conclusion (:evidence/claim-type %)) entries)]
            (doseq [c conclusions]
              (when-let [parent-id (:evidence/in-reply-to c)]
                (let [parent (get by-id parent-id)]
                  (is (not= :conclusion (:evidence/claim-type parent))
                      "conclusion should not reply to conclusion"))))))

        ;; Invariant 5: Author tracking
        (testing "invariant 5: author tracking"
          (is (seq (:thread/participants tp))))

        ;; Invariant 6: Monotonic timestamps
        (testing "invariant 6: monotonic timestamps"
          (let [instants (map #(Instant/parse (:evidence/at %)) entries)]
            (is (= instants (sort instants)) "timestamps are non-decreasing")))

        ;; Invariant 7: Entry count consistency
        (testing "invariant 7: count consistency"
          (is (= (:thread/entry-count tp) (count entries))))

        ;; Thread status
        (is (= :closed (:thread/status tp)))))))

;; =============================================================================
;; VERIFY — Edge cases
;; =============================================================================

(deftest mid-cycle-stop-produces-valid-evidence
  (testing "stopping mid-cycle (mission parking / 味→未@0) produces valid fruit"
    (let [evidence-store (atom {:entries {} :order []})
          p (mission/make-mission (make-mission-mock))
          start (runner/start p {:session-id "sess-park-1"
                                 :mission-id "M-test"
                                 :evidence-store evidence-store})
          load-step (runner/step p (:state start)
                                 {:tool :mission-load :args ["M-test"]})
          cycle-step (runner/step p (:state load-step)
                                  {:tool :cycle-begin :args ["M-test" "O-main"]})
          ;; Stop mid-cycle — mission parking
          stop (runner/stop p (:state cycle-step) "boundary reached: 味→未@0")]
      (is (:ok stop))
      (is (= :observe (get-in stop [:fruit :final-phase]))
          "fruit records the phase where we parked")
      (is (= 0 (get-in stop [:fruit :cycles-completed]))
          "no cycles completed")
      (fix/assert-valid! shapes/EvidenceEntry (:evidence stop))
      (is (= "boundary reached: 味→未@0"
             (get-in stop [:evidence :evidence/body :reason]))))))

(deftest multi-cycle-session-increments-counter
  (testing "completing a cycle and starting another increments cycles-completed"
    (let [;; Backend that returns :completed phase on advance
          backend (tools/make-mock-backend
                   {:mission-load {:mission/id "M-test"
                                   :mission/version 1
                                   :mission/obligations {}
                                   :mission/cycles []
                                   :mission/failed-approaches []
                                   :mission/updated-at "t"}
                    :cycle-begin (fn [_tool _args]
                                   {:ok true
                                    :result {:cycle/id "C1"
                                             :cycle/blocker-id "O-b"
                                             :cycle/phase :observe
                                             :cycle/phases-completed []
                                             :cycle/phase-data {}
                                             :cycle/started-at "t"
                                             :cycle/updated-at "t"}})
                    :cycle-advance (fn [_tool _args]
                                    {:ok true
                                     :result {:cycle/id "C1"
                                              :cycle/blocker-id "O-b"
                                              :cycle/phase :completed
                                              :cycle/phases-completed
                                              [:observe :propose :execute
                                               :validate :classify :integrate
                                               :commit :gate-review]
                                              :cycle/phase-data {}
                                              :cycle/result-status :done
                                              :cycle/started-at "t"
                                              :cycle/updated-at "t"}})
                    :obligation-query {}})
          p (mission/make-mission backend)
          start (runner/start p {:session-id "sess-multi-1" :mission-id "M-test"})
          ;; First cycle
          c1-begin (runner/step p (:state start)
                                {:tool :cycle-begin :args ["M-test" "O-b"]})
          c1-complete (runner/step p (:state c1-begin)
                                   {:tool :cycle-advance :args ["M-test" "C1" {}]})
          ;; Second cycle
          c2-begin (runner/step p (:state c1-complete)
                                {:tool :cycle-begin :args ["M-test" "O-b"]})
          c2-complete (runner/step p (:state c2-begin)
                                   {:tool :cycle-advance :args ["M-test" "C1" {}]})
          stop (runner/stop p (:state c2-complete) "two cycles done")]
      (is (:ok c1-complete))
      (is (= 1 (get-in c1-complete [:state :cycles-completed])))
      (is (nil? (get-in c1-complete [:state :current-phase]))
          "phase cleared after cycle completes")
      (is (:ok c2-begin))
      (is (= :observe (get-in c2-begin [:state :current-phase]))
          "second cycle starts at :observe")
      (is (:ok c2-complete))
      (is (= 2 (get-in c2-complete [:state :cycles-completed])))
      (is (= 2 (get-in stop [:fruit :cycles-completed]))))))

(deftest snapshot-evidence-chains-to-step-evidence
  (testing "snapshot evidence is chained via in-reply-to to the save step"
    (let [evidence-store (atom {:entries {} :order []})
          save-result {:mission/id "M-test"
                       :mission/version 2
                       :mission/obligations {"O-main" {:item/id "O-main"
                                                        :item/label "Main"
                                                        :item/status :open
                                                        :item/depends-on #{}
                                                        :item/unlocks #{}
                                                        :item/artifact-paths []}}
                       :mission/cycles []
                       :mission/failed-approaches []
                       :mission/updated-at "t"}
          backend (tools/make-mock-backend
                   {:mission-load {:mission/id "M-test"
                                   :mission/version 1
                                   :mission/obligations {}
                                   :mission/cycles []
                                   :mission/failed-approaches []
                                   :mission/updated-at "t"}
                    :mission-save save-result})
          p (mission/make-mission backend)
          start (runner/start p {:session-id "sess-chain-1"
                                 :mission-id "M-test"
                                 :evidence-store evidence-store})
          load-step (runner/step p (:state start)
                                 {:tool :mission-load :args ["M-test"]})
          save-step (runner/step p (:state load-step)
                                 {:tool :mission-save :args ["M-test"]})]
      (is (:ok save-step))
      (let [step-ev (:evidence save-step)
            snap-ev (:snapshot-evidence save-step)]
        (is (some? snap-ev) "snapshot evidence was emitted")
        (is (= (:evidence/id step-ev) (:evidence/in-reply-to snap-ev))
            "snapshot chains to the save step via in-reply-to")
        ;; Verify the chain is reconstructable from the store
        (let [chain (store/get-reply-chain* evidence-store (:evidence/id snap-ev))]
          (is (>= (count chain) 2)
              "reply chain includes at least the snapshot and its parent"))))))
