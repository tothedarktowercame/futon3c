(ns futon3c.agents.tickle-work-queue-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [futon3c.agents.tickle-work-queue :as wq]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]
           [java.util UUID]))

(use-fixtures
  :each
  (fn [f]
    (estore/reset-store!)
    (f)))

(defn- make-evidence-store
  []
  (atom {:entries {} :order []}))

;; =============================================================================
;; Entity loading
;; =============================================================================

(deftest load-ct-entities-test
  (testing "loads 313 CT entities from disk"
    (let [entities (wq/load-ct-entities)]
      (is (pos? (count entities))
          "should load entities from entities.json")
      (is (<= 300 (count entities))
          "should have ~313 CT entities")
      (is (every? :entity-id entities)
          "every entity should have an entity-id")
      (is (every? :title entities)
          "every entity should have a title")
      (is (every? :source-file entities)
          "every entity should have a source-file"))))

(deftest entity-fields-test
  (testing "entity maps have expected fields"
    (let [entity (first (wq/load-ct-entities))]
      (is (string? (:entity-id entity)))
      (is (string? (:title entity)))
      (is (string? (:type entity)))
      (is (string? (:source-file entity)))
      (is (number? (:body-length entity)))
      (is (number? (:ner-count entity)))
      (is (number? (:wire-count entity)))
      (is (number? (:port-count entity)))
      (is (vector? (:msc-codes entity))))))

;; =============================================================================
;; .tex body loading
;; =============================================================================

(deftest load-tex-body-test
  (testing "loads full .tex body from PlanetMath corpus"
    (let [entities (wq/load-ct-entities)
          ;; Find one with a short body for fast test
          short-entity (first (filter #(< (:body-length %) 3000) entities))
          body (wq/load-tex-body short-entity)]
      (is (some? body) "should find .tex file for entity")
      (is (pos? (count body)) "body should not be empty")
      (is (> (count body) 100) "body should have substantial content"))))

;; =============================================================================
;; Prompt construction
;; =============================================================================

(deftest make-extraction-prompt-test
  (testing "generates valid extraction prompts"
    (let [entities (wq/load-ct-entities)
          entity (first (filter #(< (:body-length %) 3000) entities))
          prompt (wq/make-extraction-prompt entity)]
      (is (some? prompt) "should generate prompt")
      (is (> (count prompt) 1000) "prompt should include taxonomy + body")
      ;; Check taxonomy sections are present
      (is (re-find #"COMPONENT TYPES" prompt)
          "should include component types taxonomy")
      (is (re-find #"PORT TYPES" prompt)
          "should include port types taxonomy")
      (is (re-find #"WIRE TYPES" prompt)
          "should include wire types taxonomy")
      (is (re-find #"TEXT:" prompt)
          "should include TEXT: separator before entity body"))))

(deftest make-review-prompt-test
  (testing "generates review prompts with ground truth"
    (let [entities (wq/load-ct-entities)
          entity (first (filter #(< (:body-length %) 3000) entities))
          review (wq/make-review-prompt entity "{\"components\":[],\"ports\":[],\"wires\":[]}")]
      (is (some? review))
      (is (re-find #"Ground Truth Counts" review))
      (is (re-find #"APPROVE" review))
      (is (re-find #"REQUEST_CHANGES" review)))))

;; =============================================================================
;; Issue synthesis
;; =============================================================================

(deftest entity-to-issue-test
  (testing "converts entity to orchestrator-compatible issue"
    (let [entities (wq/load-ct-entities)
          entity (first (filter #(< (:body-length %) 3000) entities))
          idx 42
          issue (wq/entity->issue entity idx)]
      (is (some? issue) "should produce issue")
      (is (= (+ 10000 idx) (:number issue))
          "issue number should be offset from entity index")
      (is (string? (:title issue)))
      (is (re-find #"CT-extract:" (:title issue)))
      (is (string? (:body issue)))
      (is (string? (:entity-id issue)))
      (is (map? (:ground-truth issue)))
      (is (number? (get-in issue [:ground-truth :scopes]))))))

;; =============================================================================
;; Progress tracking
;; =============================================================================

(deftest completed-entity-ids-empty-store
  (testing "empty store returns empty set"
    (let [store (make-evidence-store)]
      (is (= #{} (wq/completed-entity-ids store))))))

(deftest completed-entity-ids-with-evidence
  (testing "returns entity IDs from completed CT extraction evidence"
    (let [store (make-evidence-store)]
      ;; Simulate a completed extraction
      (estore/append* store
                      {:subject {:ref/type :task
                                 :ref/id "pm-ct-TestEntity"}
                       :type :coordination
                       :claim-type :observation
                       :author "tickle-1"
                       :tags [:tickle :ct-extraction :workflow-complete]
                       :session-id "test-session"
                       :body {:entity-id "pm-ct-TestEntity"
                              :at (str (Instant/now))}})
      (let [done (wq/completed-entity-ids store)]
        (is (contains? done "pm-ct-TestEntity"))))))

(deftest queue-status-test
  (testing "queue status reflects evidence store state"
    (let [store (make-evidence-store)
          status (wq/queue-status store)]
      (is (pos? (:total status)))
      (is (zero? (:completed status)))
      (is (= (:total status) (:remaining status))))))

(deftest next-unprocessed-skips-done
  (testing "next-unprocessed skips already-completed entities"
    (let [store (make-evidence-store)
          entities (wq/load-ct-entities)
          first-id (:entity-id (first entities))]
      ;; Mark the first entity as done
      (estore/append* store
                      {:subject {:ref/type :task :ref/id first-id}
                       :type :coordination
                       :claim-type :observation
                       :author "tickle-1"
                       :tags [:tickle :ct-extraction :workflow-complete]
                       :session-id "test"
                       :body {:entity-id first-id :at (str (Instant/now))}})
      (let [next-issues (wq/next-unprocessed store 5)]
        (is (pos? (count next-issues)))
        (is (not-any? #(= first-id (:entity-id %)) next-issues)
            "should skip the completed entity")))))

;; =============================================================================
;; Golden reference data
;; =============================================================================

(deftest golden-entity-ids-test
  (testing "finds golden reference entity IDs"
    (let [golden (wq/golden-entity-ids)]
      (is (set? golden))
      (is (pos? (count golden))
          "should find golden entity IDs")
      (is (<= 15 (count golden))
          "should have ~20 golden entities"))))

;; =============================================================================
;; CT evidence emission
;; =============================================================================

(deftest emit-ct-evidence-test
  (testing "emits CT-specific evidence"
    (let [store (make-evidence-store)]
      (wq/emit-ct-evidence! store
                            {:entity-id "pm-ct-TestEntry"
                             :entity-type "Definition"
                             :session-id "test-session"
                             :event-tag :workflow-start
                             :ground-truth {:scopes 5 :wires 3 :ports 1 :ner-terms 20}})
      (let [entries (estore/query* store {})
            ct-entry (first (filter #(some #{:ct-extraction} (:evidence/tags %)) entries))]
        (is (some? ct-entry) "should emit evidence")
        (is (= "tickle-1" (:evidence/author ct-entry)))
        (is (some #{:ct-extraction} (:evidence/tags ct-entry)))
        (is (= "pm-ct-TestEntry" (get-in ct-entry [:evidence/body :entity-id])))))))

;; =============================================================================
;; Complexity sorting
;; =============================================================================

(deftest entities-by-complexity-test
  (testing "sorts entities by body length"
    (let [asc (wq/entities-by-complexity :asc)
          desc (wq/entities-by-complexity :desc)]
      (is (pos? (count asc)))
      (is (<= (:body-length (first asc))
              (:body-length (last asc)))
          "ascending should have shortest first")
      (is (>= (:body-length (first desc))
              (:body-length (last desc)))
          "descending should have longest first"))))
