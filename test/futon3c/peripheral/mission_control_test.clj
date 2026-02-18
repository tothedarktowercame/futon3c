(ns futon3c.peripheral.mission-control-test
  "Tests for the mission control peripheral.

   Coverage:
   1. Backend: mission file parsing, devmap reading, coverage analysis
   2. Peripheral: lifecycle (start/step/stop), evidence emission
   3. Integration: portfolio review across real repo data
   4. VERIFY: backward verification (←), invariant checks, real-data validation"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.mission-control :as mc]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.peripheral.round-trip :as rt]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.evidence.store :as evidence-store]
            [futon3c.social.shapes :as shapes]))

;; =============================================================================
;; Backend: status classification
;; =============================================================================

(deftest classify-status-recognizes-all-categories
  (testing "status classification covers the observed vocabulary"
    (is (= :complete (mcb/classify-status "Complete")))
    (is (= :complete (mcb/classify-status "Complete (b69ede4)")))
    (is (= :complete (mcb/classify-status "Complete (31 tests, 89 assertions)")))
    (is (= :complete (mcb/classify-status "PASS")))
    (is (= :complete (mcb/classify-status "INSTANTIATE complete — derivation xenotype finished")))
    (is (= :blocked  (mcb/classify-status "Blocked on Part I")))
    (is (= :blocked  (mcb/classify-status "Blocked on Parts II-III")))
    (is (= :ready    (mcb/classify-status "Ready")))
    (is (= :in-progress (mcb/classify-status "IDENTIFY (mission proposal)")))
    (is (= :in-progress (mcb/classify-status "MAP (landscape survey complete)")))
    (is (= :in-progress (mcb/classify-status "DERIVE")))
    (is (= :unknown  (mcb/classify-status "Some other status")))
    (is (nil? (mcb/classify-status nil)))))

;; =============================================================================
;; Backend: mission file parsing
;; =============================================================================

(deftest parse-mission-md-extracts-fields
  (testing "parsing a real mission file extracts status, date, blocked-by"
    (let [path (str (System/getProperty "user.home") "/code/futon3c/holes/missions/M-mission-control.md")
          file (io/file path)]
      (when (.exists file)
        (let [entry (mcb/parse-mission-md path :futon3c)]
          (is (= "mission-control" (:mission/id entry)))
          (is (= :md-file (:mission/source entry)))
          (is (= "futon3c" (:mission/repo entry)))
          (is (string? (:mission/date entry)))
          (is (keyword? (:mission/status entry))))))))

(deftest parse-devmap-edn-extracts-fields
  (testing "parsing a real devmap EDN extracts mission/id and state"
    (let [path (str (System/getProperty "user.home") "/code/futon5/data/missions/social-exotype.edn")
          file (io/file path)]
      (when (.exists file)
        (let [entry (mcb/parse-devmap-edn path)]
          (is (= "social-exotype" (:mission/id entry)))
          (is (= :in-progress (:mission/status entry)))
          (is (= :devmap-edn (:mission/source entry)))
          (is (= :social-exotype (:mission/devmap-id entry))))))))

;; =============================================================================
;; Backend: inventory scanning
;; =============================================================================

(deftest scan-mission-files-finds-futon3c-missions
  (testing "scanning futon3c finds mission files"
    (let [root (str (System/getProperty "user.home") "/code/futon3c")
          file (io/file root "holes" "missions")]
      (when (.isDirectory file)
        (let [missions (mcb/scan-mission-files root :futon3c)]
          (is (pos? (count missions)))
          (is (every? #(= "futon3c" (:mission/repo %)) missions))
          (is (every? #(= :md-file (:mission/source %)) missions))
          ;; mission-control should be in the list
          (is (some #(= "mission-control" (:mission/id %)) missions)))))))

(deftest scan-devmap-files-finds-futon5-devmaps
  (testing "scanning futon5 finds devmap EDN files"
    (let [root (str (System/getProperty "user.home") "/code/futon5")
          file (io/file root "data" "missions")]
      (when (.isDirectory file)
        (let [devmaps (mcb/scan-devmap-files root)]
          (is (pos? (count devmaps)))
          (is (every? #(= :devmap-edn (:mission/source %)) devmaps))
          (is (some #(= "social-exotype" (:mission/id %)) devmaps)))))))

(deftest build-inventory-cross-repo
  (testing "full inventory spans futon3c and futon5"
    (let [inventory (mcb/build-inventory)]
      (is (pos? (count inventory)))
      ;; Should have both md-file and devmap-edn sources
      (is (some #(= :md-file (:mission/source %)) inventory))
      (is (some #(= :devmap-edn (:mission/source %)) inventory))
      ;; Should have missions from futon3c
      (is (some #(= "futon3c" (:mission/repo %)) inventory)))))

;; =============================================================================
;; Backend: devmap reading
;; =============================================================================

(deftest read-devmap-extracts-structural-summary
  (testing "read-devmap produces a structural summary with components"
    (let [path (str (System/getProperty "user.home") "/code/futon5/data/missions/social-exotype.edn")
          file (io/file path)]
      (when (.exists file)
        (let [dm (mcb/read-devmap path)]
          (is (= :social-exotype (:devmap/id dm)))
          (is (= :active (:devmap/state dm)))
          (is (pos? (:devmap/input-count dm)))
          (is (pos? (:devmap/output-count dm)))
          (is (pos? (:devmap/component-count dm)))
          (is (pos? (:devmap/edge-count dm)))
          (is (boolean? (:devmap/all-valid dm)))
          (is (vector? (:devmap/failed-checks dm)))
          (is (vector? (:devmap/components dm)))
          ;; social-exotype has 7 components (S-presence through S-default)
          (is (= 7 (count (:devmap/components dm)))))))))

(deftest read-all-devmaps-skips-grounding-functors
  (testing "read-all-devmaps excludes grounding functor files"
    (let [root (str (System/getProperty "user.home") "/code/futon5")
          file (io/file root "data" "missions")]
      (when (.isDirectory file)
        (let [devmaps (mcb/read-all-devmaps root)]
          (is (pos? (count devmaps)))
          ;; No grounding functors
          (is (not-any? #(re-find #"grounding" (name (:devmap/id %))) devmaps)))))))

;; =============================================================================
;; Backend: coverage analysis
;; =============================================================================

(deftest compute-coverage-produces-pct
  (testing "coverage analysis produces per-devmap coverage percentages"
    (let [devmaps [{:devmap/id :test-dm
                    :devmap/state :active
                    :devmap/input-count 2
                    :devmap/output-count 2
                    :devmap/component-count 3
                    :devmap/edge-count 4
                    :devmap/all-valid true
                    :devmap/failed-checks []
                    :devmap/components [{:component/id :C-alpha :component/name "Alpha"}
                                        {:component/id :C-beta :component/name "Beta"}
                                        {:component/id :C-gamma :component/name "Gamma"}]}]
          missions [{:mission/id "alpha" :mission/status :complete :mission/source :md-file}
                    {:mission/id "beta" :mission/status :in-progress :mission/source :md-file}]
          coverage (mcb/compute-coverage devmaps missions)]
      (is (= 1 (count coverage)))
      (let [c (first coverage)]
        (is (= :test-dm (:coverage/devmap-id c)))
        (is (= 3 (:coverage/total-components c)))
        ;; "alpha" matches :C-alpha, "beta" matches :C-beta
        (is (= 2 (:coverage/covered-components c)))
        (is (= [:C-gamma] (:coverage/uncovered c)))
        ;; 2/3 ≈ 0.667
        (is (< 0.6 (:coverage/coverage-pct c) 0.7))))))

;; =============================================================================
;; Backend: mana queries
;; =============================================================================

(deftest query-mana-returns-snapshot
  (testing "mana query returns availability status"
    (let [mana (mcb/query-mana (str (System/getProperty "user.home") "/code/futon5"))]
      (is (boolean? (:mana/available mana))))))

;; =============================================================================
;; Backend: full portfolio review
;; =============================================================================

(deftest build-portfolio-review-produces-complete-structure
  (testing "portfolio review has all required fields"
    (let [review (mcb/build-portfolio-review)]
      (is (vector? (:portfolio/missions review)))
      (is (vector? (:portfolio/devmap-summaries review)))
      (is (vector? (:portfolio/coverage review)))
      (is (map? (:portfolio/mana review)))
      (is (string? (:portfolio/summary review)))
      (is (vector? (:portfolio/gaps review)))
      (is (vector? (:portfolio/actionable review)))
      ;; Substantive: we should have missions and devmaps
      (is (pos? (count (:portfolio/missions review))))
      (is (pos? (count (:portfolio/devmap-summaries review)))))))

;; =============================================================================
;; Peripheral lifecycle
;; =============================================================================

(deftest peripheral-start-requires-session-id
  (testing "start without session-id returns error"
    (let [spec {:peripheral/id :mission-control
                :peripheral/tools #{:mc-inventory :mc-review}
                :peripheral/scope :full-codebase}
          p (mc/make-mission-control spec (tools/make-mock-backend))
          result (runner/start p {})]
      (is (shapes/valid? shapes/SocialError result)))))

(deftest peripheral-lifecycle-start-step-stop
  (testing "full lifecycle: start → step (mc-inventory) → stop"
    (let [spec {:peripheral/id :mission-control
                :peripheral/tools #{:mc-inventory :mc-review :mc-devmaps
                                    :mc-coverage :mc-mana :mc-bulletin
                                    :read :glob :grep :bash-readonly}
                :peripheral/scope :full-codebase}
          p (mc/make-mission-control spec (tools/make-mock-backend))
          context {:session-id "test-mc-session"
                   :author "test-agent"}
          start-result (runner/start p context)]
      (is (:ok start-result))
      (is (map? (:state start-result)))
      (is (map? (:evidence start-result)))
      ;; Step: run mc-inventory
      (let [step-result (runner/step p (:state start-result)
                                     {:tool :mc-inventory})]
        (is (:ok step-result))
        (is (vector? (:result step-result)))
        (is (map? (:evidence step-result)))
        ;; Evidence should be a step claim
        (is (= :step (:evidence/claim-type (:evidence step-result))))
        ;; Stop
        (let [stop-result (runner/stop p (:state step-result) "review complete")]
          (is (:ok stop-result))
          (is (map? (:fruit stop-result)))
          (is (= :conclusion (:evidence/claim-type (:evidence stop-result))))
          ;; Fruit should record steps taken
          (is (= 1 (:steps-taken (:fruit stop-result)))))))))

(deftest peripheral-mc-review-stores-in-state
  (testing "mc-review result is captured in state as latest-review"
    (let [spec {:peripheral/id :mission-control
                :peripheral/tools #{:mc-review}
                :peripheral/scope :full-codebase}
          p (mc/make-mission-control spec (tools/make-mock-backend))
          start (runner/start p {:session-id "test-review" :author "agent"})
          step (runner/step p (:state start) {:tool :mc-review})]
      (is (:ok step))
      ;; The review should be in the fruit on stop
      (let [stop (runner/stop p (:state step) "done")]
        (is (some? (get-in stop [:fruit :review])))
        (is (vector? (get-in stop [:fruit :review :portfolio/missions])))))))

(deftest peripheral-mc-bulletin-emits-evidence
  (testing "mc-bulletin tool produces step evidence"
    (let [spec {:peripheral/id :mission-control
                :peripheral/tools #{:mc-bulletin}
                :peripheral/scope :full-codebase}
          p (mc/make-mission-control spec (tools/make-mock-backend))
          start (runner/start p {:session-id "test-bulletin" :author "agent"})
          step (runner/step p (:state start)
                            {:tool :mc-bulletin
                             :args ["Portfolio health: 15 missions, 8 complete, 2 blocked"]})]
      (is (:ok step))
      (is (= {:bulletin "Portfolio health: 15 missions, 8 complete, 2 blocked"}
             (:result step))))))

(deftest peripheral-rejects-disallowed-tools
  (testing "tools not in the spec are rejected"
    (let [spec {:peripheral/id :mission-control
                :peripheral/tools #{:mc-inventory}
                :peripheral/scope :full-codebase}
          p (mc/make-mission-control spec (tools/make-mock-backend))
          start (runner/start p {:session-id "test-reject" :author "agent"})
          ;; :write is not in the tool set
          step (runner/step p (:state start) {:tool :write :args ["foo" "bar"]})]
      (is (shapes/valid? shapes/SocialError step)))))

;; =============================================================================
;; Evidence integration
;; =============================================================================

(deftest evidence-chain-is-linked
  (testing "evidence entries form a reply chain via in-reply-to"
    (let [spec {:peripheral/id :mission-control
                :peripheral/tools #{:mc-inventory :mc-review}
                :peripheral/scope :full-codebase}
          store (atom {:entries {} :order []})
          p (mc/make-mission-control spec (tools/make-mock-backend))
          start (runner/start p {:session-id "test-chain"
                                 :author "agent"
                                 :evidence-store store})
          step1 (runner/step p (:state start) {:tool :mc-inventory})
          step2 (runner/step p (:state step1) {:tool :mc-review})
          stop (runner/stop p (:state step2) "done")
          entries (vals (:entries @store))]
      ;; Should have 4 entries: start + 2 steps + stop
      (is (= 4 (count entries)))
      ;; Step1 replies to start
      (is (= (:evidence/id (:evidence start))
             (:evidence/in-reply-to (:evidence step1))))
      ;; Step2 replies to step1
      (is (= (:evidence/id (:evidence step1))
             (:evidence/in-reply-to (:evidence step2))))
      ;; Stop replies to step2
      (is (= (:evidence/id (:evidence step2))
             (:evidence/in-reply-to (:evidence stop)))))))

(deftest evidence-entries-are-valid-shape
  (testing "all emitted evidence entries conform to EvidenceEntry shape"
    (let [spec {:peripheral/id :mission-control
                :peripheral/tools #{:mc-inventory}
                :peripheral/scope :full-codebase}
          store (atom {:entries {} :order []})
          p (mc/make-mission-control spec (tools/make-mock-backend))
          start (runner/start p {:session-id "test-shapes"
                                 :author "agent"
                                 :evidence-store store})
          step (runner/step p (:state start) {:tool :mc-inventory})
          _ (runner/stop p (:state step) "done")]
      (doseq [[_ entry] (:entries @store)]
        (is (shapes/valid? shapes/EvidenceEntry entry)
            (str "Entry failed validation: "
                 (:error (shapes/validate shapes/EvidenceEntry entry))))))))

;; =============================================================================
;; VERIFY — Backward verification (←)
;; =============================================================================

(deftest mc-passes-backward-verification
  (testing "mission control evidence passes ← verification against spec"
    (let [spec (common/load-spec :mission-control)
          p (mc/make-mission-control spec (tools/make-mock-backend))
          result (rt/run-and-verify
                  p
                  {:session-id "verify-backward"}
                  [{:tool :mc-inventory}]
                  "backward verification")]
      (is (:ok result)
          (str "← verification failed: " (:violations result)))
      ;; Evidence chain should be: goal → step → conclusion
      (is (= [:goal :step :conclusion]
             (mapv :evidence/claim-type (:evidence result)))))))

(deftest mc-backward-verification-with-all-tools
  (testing "← verification passes when exercising all mc-* tools"
    (let [spec (common/load-spec :mission-control)
          p (mc/make-mission-control spec (tools/make-mock-backend))
          result (rt/run-and-verify
                  p
                  {:session-id "verify-all-tools"}
                  [{:tool :mc-inventory}
                   {:tool :mc-devmaps}
                   {:tool :mc-coverage}
                   {:tool :mc-mana}
                   {:tool :mc-review}
                   {:tool :mc-bulletin :args ["test bulletin"]}]
                  "all tools verified")]
      (is (:ok result)
          (str "← verification failed: " (:violations result)))
      ;; goal + 6 steps + conclusion = 8
      (is (= 8 (count (:evidence result))))
      ;; Fruit should have a review
      (is (some? (get-in result [:fruit :review])))
      (is (= 6 (:steps-taken (:fruit result)))))))

(deftest mc-backward-verification-rejects-write-tool
  (testing "← verification would catch a disallowed tool (if it got through)"
    (let [spec (common/load-spec :mission-control)
          ;; Verify that :write is NOT in the tool set
          write-tools #{:edit :write :bash-git :bash-deploy}]
      (is (empty? (clojure.set/intersection (:peripheral/tools spec) write-tools))
          "mission-control spec must not contain write tools"))))

;; =============================================================================
;; VERIFY — Evidence invariants
;; =============================================================================

(deftest evidence-invariants-hold
  (testing "evidence entries satisfy structural invariants"
    (let [spec (common/load-spec :mission-control)
          store (atom {:entries {} :order []})
          p (mc/make-mission-control spec (tools/make-mock-backend))
          start (runner/start p {:session-id "verify-inv"
                                 :author "verify-agent"
                                 :evidence-store store})
          s1 (runner/step p (:state start) {:tool :mc-inventory})
          s2 (runner/step p (:state s1) {:tool :mc-review})
          _ (runner/stop p (:state s2) "verify done")
          entries (mapv (fn [id] (get-in @store [:entries id]))
                        (:order @store))]
      ;; I1: All entries share session-id
      (testing "all entries share session-id"
        (is (= 1 (count (set (map :evidence/session-id entries))))))

      ;; I2: Claim type ordering: goal → step* → conclusion
      (testing "claim types follow goal → step* → conclusion"
        (let [claims (mapv :evidence/claim-type entries)]
          (is (= :goal (first claims)))
          (is (= :conclusion (last claims)))
          (is (every? #{:step} (butlast (rest claims))))))

      ;; I3: Evidence type is :coordination (not :reflection)
      (testing "all entries have evidence type :coordination"
        (is (every? #(= :coordination (:evidence/type %)) entries)))

      ;; I4: Reply chain is contiguous
      (testing "reply chain links each entry to its predecessor"
        (let [pairs (partition 2 1 entries)]
          (doseq [[prev curr] pairs]
            (is (= (:evidence/id prev) (:evidence/in-reply-to curr))
                (str "broken chain: " (:evidence/id curr)
                     " should reply to " (:evidence/id prev))))))

      ;; I5: Monotonic timestamps
      (testing "timestamps are monotonically non-decreasing"
        (let [timestamps (map :evidence/at entries)]
          (doseq [[a b] (partition 2 1 timestamps)]
            (is (<= (compare a b) 0)
                (str "non-monotonic: " a " > " b)))))

      ;; I6: Author consistency
      (testing "all entries have the same author"
        (is (= 1 (count (set (map :evidence/author entries))))))

      ;; I7: Peripheral tag present
      (testing "all entries are tagged with :mission-control"
        (is (every? #(some #{:mission-control} (:evidence/tags %)) entries))))))

;; =============================================================================
;; VERIFY — Real data validation
;; =============================================================================

(deftest inventory-finds-known-missions
  (testing "inventory contains missions we know exist"
    (let [inventory (mcb/build-inventory)
          mission-ids (set (map :mission/id inventory))]
      ;; Known futon3c missions
      (is (contains? mission-ids "mission-control")
          "missing mission-control")
      (is (contains? mission-ids "mission-peripheral")
          "missing mission-peripheral")
      (is (contains? mission-ids "agency-refactor")
          "missing agency-refactor")
      ;; Known futon5 devmaps (stable as of 2026-02-18)
      (is (contains? mission-ids "social-exotype")
          "missing social-exotype")
      (is (some mission-ids ["coordination-exotype" "futon3-coordination" "f6-ingest"])
          "missing expected futon5 devmap mission id"))))

(deftest inventory-status-distribution-is-plausible
  (testing "status distribution matches what we know about the portfolio"
    (let [inventory (mcb/build-inventory)
          by-status (group-by :mission/status inventory)]
      ;; We know there are complete missions
      (is (pos? (count (:complete by-status)))
          "should have at least one complete mission")
      ;; We know there are in-progress missions (devmaps are :active)
      (is (pos? (count (:in-progress by-status)))
          "should have at least one in-progress mission")
      ;; Total should be reasonable (15 md + 9 devmaps = ~24, minus grounding functors)
      (is (>= (count inventory) 20)
          (str "expected >= 20 missions, got " (count inventory))))))

(deftest devmap-summaries-match-known-structure
  (testing "social-exotype devmap has expected structure"
    (let [devmaps (mcb/read-all-devmaps
                    (str (System/getProperty "user.home") "/code/futon5"))
          social (first (filter #(= :social-exotype (:devmap/id %)) devmaps))]
      (when social
        ;; social-exotype.edn: 4 inputs, 4 outputs, 7 components, 28 edges
        (is (= 4 (:devmap/input-count social)))
        (is (= 4 (:devmap/output-count social)))
        (is (= 7 (:devmap/component-count social)))
        (is (>= (:devmap/edge-count social) 28))
        ;; Known component names
        (let [comp-ids (set (map :component/id (:devmap/components social)))]
          (is (contains? comp-ids :S-presence))
          (is (contains? comp-ids :S-dispatch))
          (is (contains? comp-ids :S-default)))))))

(deftest portfolio-review-summary-is-coherent
  (testing "portfolio review summary numbers match the underlying data"
    (let [review (mcb/build-portfolio-review)
          missions (:portfolio/missions review)
          summary (:portfolio/summary review)]
      ;; Summary should mention the mission count
      (is (str/includes? summary (str (count missions) " missions"))
          (str "summary doesn't match: " summary))
      ;; Actionable missions should be a subset of in-progress + ready
      (let [actionable-ids (set (map #(first (str/split % #" ")) (:portfolio/actionable review)))
            in-progress-or-ready (set (map :mission/id
                                           (filter #(#{:ready :in-progress} (:mission/status %))
                                                   missions)))]
        (is (every? in-progress-or-ready actionable-ids)
            "actionable missions should all be in-progress or ready")))))

(deftest coverage-pct-is-bounded
  (testing "all coverage percentages are in [0.0, 1.0]"
    (let [review (mcb/build-portfolio-review)]
      (doseq [c (:portfolio/coverage review)]
        (is (<= 0.0 (:coverage/coverage-pct c) 1.0)
            (str "coverage out of range for " (:coverage/devmap-id c)
                 ": " (:coverage/coverage-pct c)))
        ;; covered + uncovered = total
        (is (= (:coverage/total-components c)
               (+ (:coverage/covered-components c)
                  (count (:coverage/uncovered c))))
            (str "coverage counts don't add up for " (:coverage/devmap-id c)))))))

;; =============================================================================
;; VERIFY — Gap tracking (known weaknesses from ARGUE A10)
;; =============================================================================

(deftest gap-coverage-heuristic-has-false-positives
  (testing "GAP G1: coverage heuristic can over-match (known weakness)"
    ;; This test documents the gap, not a failure.
    ;; Short component IDs like :C1 will match many mission names.
    (let [devmaps [{:devmap/id :test-gap
                    :devmap/state :active
                    :devmap/input-count 1
                    :devmap/output-count 1
                    :devmap/component-count 2
                    :devmap/edge-count 2
                    :devmap/all-valid true
                    :devmap/failed-checks []
                    :devmap/components [{:component/id :C1 :component/name "C1"}
                                        {:component/id :C2 :component/name "C2"}]}]
          missions [{:mission/id "some-c1-related-thing"
                     :mission/status :complete
                     :mission/source :md-file}]
          coverage (mcb/compute-coverage devmaps missions)
          c (first coverage)]
      ;; :C1 matches "some-c1-related-thing" — this is a false positive
      ;; (the mission may not actually address component C1)
      (is (= 1 (:coverage/covered-components c))
          "GAP G1: heuristic matches short component IDs too liberally"))))

(deftest gap-mana-query-is-stub
  (testing "GAP G2: mana query returns availability but not live pool stats"
    (let [tmp-dir (-> (java.nio.file.Files/createTempDirectory
                       "mission-control-mana-gap"
                       (make-array java.nio.file.attribute.FileAttribute 0))
                      (.toFile))
          root (.getAbsolutePath tmp-dir)
          no-db (mcb/query-mana root)
          data-dir (io/file tmp-dir "data")
          db-file (io/file data-dir "nonstarter.db")]
      (try
        ;; Deterministic "db absent" branch.
        (is (false? (:mana/available no-db))
            "GAP G2: mana should be unavailable when nonstarter.db is absent")

        ;; Deterministic "db present" branch still uses stubbed zero values.
        (.mkdirs data-dir)
        (spit db-file "")
        (let [with-db (mcb/query-mana root)]
          (is (true? (:mana/available with-db))
              "GAP G2: mana marks available when nonstarter.db exists")
          (is (= 0.0 (:mana/pool-balance with-db)))
          (is (= 0.0 (:mana/total-donated with-db)))
          (is (= 0.0 (:mana/total-funded with-db)))
          (is (= 0 (:mana/active-proposals with-db))))
        (finally
          (doseq [f (reverse (file-seq tmp-dir))]
            (.delete f)))))))

(deftest gap-single-status-extraction
  (testing "GAP G3: multi-part missions only get top-level status"
    ;; M-peripheral-phenomenology has parts I-V with individual statuses.
    ;; extract-header takes the first **Status:** line only.
    (let [inventory (mcb/build-inventory)
          phenom (first (filter #(= "peripheral-phenomenology" (:mission/id %)) inventory))]
      (when phenom
        ;; It should have SOME status, but it's only the first one
        (is (keyword? (:mission/status phenom))
            "GAP G3: multi-part mission has a status (but only the first)")))))

(deftest gap-no-subject-type-filter
  (testing "GAP G4: evidence query can't filter by subject-type alone"
    ;; The EvidenceQuery shape has :query/subject (exact ArtifactRef)
    ;; but no :query/subject-type for "all missions" queries.
    ;; This documents the gap at the evidence store level.
    ;; Malli open maps accept unknown keys, so we verify the shape
    ;; does NOT define :query/subject-type as a known key.
    (let [query-shape shapes/EvidenceQuery
          known-keys (->> (rest query-shape)  ; skip :map tag
                          (filter vector?)
                          (map first)
                          set)]
      (is (not (contains? known-keys :query/subject-type))
          "GAP G4: :query/subject-type is not in EvidenceQuery (need to add it)"))))

(deftest gap-devmap-validation-simplified
  (testing "GAP G5: minimal reimpl doesn't reproduce full futon5 validation"
    ;; Our read-devmap checks orphan inputs, dead components, spec coverage.
    ;; It does NOT check: completeness (graph traversal), type safety,
    ;; timescale ordering (I3), exogeneity (I4), closure (I6).
    ;; The social-exotype should pass our checks but might fail futon5's.
    (let [dm (mcb/read-devmap
               (str (System/getProperty "user.home")
                    "/code/futon5/data/missions/social-exotype.edn"))]
      ;; We report some failed checks (simplified validation is stricter
      ;; in some ways, more lenient in others)
      (is (vector? (:devmap/failed-checks dm))
          "GAP G5: validation runs but is simplified"))))

;; =============================================================================
;; INSTANTIATE — backfill and first portfolio review
;; =============================================================================

(deftest backfill-produces-valid-evidence-entries
  (testing "mission->evidence creates valid EvidenceEntry from MissionEntry"
    (let [mission {:mission/id "test-mission"
                   :mission/status :complete
                   :mission/source :md-file
                   :mission/repo "futon3c"
                   :mission/path "/tmp/M-test-mission.md"
                   :mission/date "2026-02-18"
                   :mission/raw-status "Complete"}
          ev (mcb/mission->evidence mission)]
      (is (shapes/valid? shapes/EvidenceEntry ev)
          "backfill entry must conform to EvidenceEntry shape")
      (is (= :coordination (:evidence/type ev)))
      (is (= :observation (:evidence/claim-type ev)))
      (is (= {:ref/type :mission :ref/id "test-mission"} (:evidence/subject ev)))
      (is (= "mission-control/backfill" (:evidence/author ev)))
      (is (= [:mission :backfill :snapshot] (:evidence/tags ev)))
      (is (= "test-mission" (get-in ev [:evidence/body :mission/id]))))))

(deftest backfill-inventory-produces-entries-for-all-missions
  (testing "backfill-inventory creates one evidence entry per mission"
    (let [missions (mcb/build-inventory)
          entries (mcb/backfill-inventory missions)]
      (is (= (count missions) (count entries)))
      (is (every? #(shapes/valid? shapes/EvidenceEntry %) entries)
          "all backfill entries must conform to EvidenceEntry")
      (is (every? #(= :coordination (:evidence/type %)) entries))
      (is (every? #(= "mission-control/backfill" (:evidence/author %)) entries))
      ;; Each entry has a unique evidence ID
      (is (= (count entries) (count (set (map :evidence/id entries))))))))

(deftest backfill-entries-appendable-to-store
  (testing "backfill entries can be appended to an evidence store"
    (let [store (atom {:entries {} :order []})
          missions [{:mission/id "m1"
                     :mission/status :complete
                     :mission/source :md-file
                     :mission/repo "futon3c"
                     :mission/path "/tmp/M-m1.md"
                     :mission/raw-status "Complete"}
                    {:mission/id "m2"
                     :mission/status :in-progress
                     :mission/source :devmap-edn
                     :mission/repo "futon5"
                     :mission/path "/tmp/m2.edn"
                     :mission/raw-status "active"}]
          entries (mcb/backfill-inventory missions)]
      (doseq [e entries]
        (evidence-store/append* store e))
      (is (= 2 (count (:entries @store))))
      (is (= 2 (count (:order @store)))))))
