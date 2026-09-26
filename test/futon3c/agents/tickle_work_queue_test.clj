(ns futon3c.agents.tickle-work-queue-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [futon3c.apm.checked-handoff :as checked-handoff]
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

(deftest ct-evidence-preserves-legacy-shapes-and-validates-checked-events
  ;; ABSENCE PIN, rerun 2026-09-03T11:04:24Z:
  ;; tags=tickle,ct-extraction => {:count 0 :checked 0}, index cursor
  ;; e-e785d6ba-871a-449d; tags=ct-extraction => {:count 0 :checked 0},
  ;; index cursor e-9ecfdbff-65f0-48ee. Both queries scanned 20,000 entries.
  (testing "legacy bodies remain exact with and without verdict"
    (doseq [verdict [nil :approve]]
      (let [store (make-evidence-store)
            input (cond-> {:entity-id "pm-ct-Pin"
                           :entity-type "Definition"
                           :session-id "u14e3-pin"
                           :event-tag :review-complete
                           :ground-truth {:scopes 1}
                           :extraction-result "captured result"}
                    verdict (assoc :verdict verdict))]
        (wq/emit-ct-evidence! store input)
        (let [entry (first (estore/query* store {}))
              body (:evidence/body entry)]
          ;; These are the two complete pre-change bodies; only :at varies.
          (is (= (cond-> {:entity-id "pm-ct-Pin"
                          :entity-type "Definition"
                          :at (:at body)
                          :ground-truth {:scopes 1}
                          :result-preview "captured result"}
                   verdict (assoc :verdict verdict))
                 body))
          (is (= {:evidence/author "tickle-1"
                  :evidence/tags [:tickle :ct-extraction :review-complete]
                  :evidence/claim-type :observation}
                 (select-keys entry [:evidence/author :evidence/tags
                                     :evidence/claim-type])))))))
  (testing "validated event and computed grade are additive"
    (let [store (make-evidence-store)
          event (checked-handoff/verdict-event
                 {:worker-seat "f75-scribe"
                  :author-seat "f75-promotion-proctor"
                  :proposal {:ref "ct-fixture"}
                  :verdict :approve
                  :adjudication {:rerun-witness :absent}})]
      (wq/emit-ct-evidence!
       store {:entity-id "pm-ct-Checked" :entity-type "Definition"
              :session-id "u14e3-checked" :event-tag :review-complete
              :verdict :approve :checked-handoff/event event})
      (let [entry (first (estore/query* store {}))
            body (:evidence/body entry)]
        (is (= "tickle-1" (:evidence/author entry)))
        (is (= "f75-promotion-proctor"
               (get-in body [:checked-handoff/event :author-seat])))
        (is (= event (:checked-handoff/event body)))
        (is (= :seat-string-distinctness (:independence/grade body)))
        (is (= :approve (:verdict body))))))
  (testing "worker-authored event is refused before append"
    (let [store (make-evidence-store)
          event (checked-handoff/verdict-event
                 {:worker-seat "f75-scribe"
                  :author-seat "f75-scribe"
                  :proposal {:ref "ct-forged"}
                  :verdict :approve
                  :adjudication {:rerun-witness :absent}})
          result (wq/emit-ct-evidence!
                  store {:entity-id "pm-ct-Forged"
                         :entity-type "Definition"
                         :session-id "u14e3-forged"
                         :event-tag :review-complete
                         :verdict :approve
                         :checked-handoff/event event})]
      (is (= :r9/worker-authored-verdict-refused (:error/code result)))
      (is (empty? (estore/query* store {}))))))

;; =============================================================================
;; arXiv entity loading
;; =============================================================================

(def ^:private sample-arxiv-entries
  [{"entity_id" "arxiv-2301.00001"
    "title" "On Functorial Semantics of Algebraic Theories"
    "source_file" "2301.00001.tex"
    "type" "Article"
    "body_length" 12345
    "arxiv_id" "2301.00001"
    "categories" ["math.CT"]
    "authors" ["A. Mathematician"]
    "ner_count" 0 "scope_count" 0 "wire_count" 0 "port_count" 0}
   {"entity_id" "arxiv-2301.00002"
    "title" "Monoidal Categories and Topological Field Theories"
    "source_file" "2301.00002.tex"
    "type" "Article"
    "body_length" 28901
    "arxiv_id" "2301.00002"
    "categories" ["math.CT" "math.QA"]
    "ner_count" 0 "scope_count" 0 "wire_count" 0 "port_count" 0}])

(deftest load-arxiv-entities-from-file
  (testing "loads arXiv entities from a temp manifest"
    (let [tmp (java.io.File/createTempFile "arxiv-test" ".json")]
      (try
        (spit tmp (json/generate-string sample-arxiv-entries))
        (let [entities (wq/load-arxiv-entities (.getAbsolutePath tmp))]
          (is (= 2 (count entities)))
          (is (= "arxiv-2301.00001" (:entity-id (first entities))))
          (is (= "Article" (:type (first entities))))
          (is (= 12345 (:body-length (first entities))))
          (is (= "2301.00001" (:arxiv-id (first entities))))
          (is (= ["math.CT"] (:categories (first entities))))
          (is (= ["A. Mathematician"] (:authors (first entities))))
          (is (zero? (:ner-count (first entities)))))
        (finally
          (.delete tmp))))))

(deftest load-arxiv-entities-missing-file
  (testing "returns nil when manifest doesn't exist"
    (is (nil? (wq/load-arxiv-entities "/nonexistent/entities.json")))))

(deftest load-all-entities-tags-corpus
  (testing "load-all-entities tags each entry with :corpus"
    (let [all (wq/load-all-entities)]
      (is (pos? (count all)))
      (is (every? :corpus all))
      (is (every? #{:planetmath :arxiv} (map :corpus all)))
      (is (some #(= :planetmath (:corpus %)) all)))))

(deftest queue-status-by-corpus
  (testing "queue-status filters by corpus"
    (let [store (make-evidence-store)
          pm-status (wq/queue-status store :corpus :planetmath)
          all-status (wq/queue-status store :corpus :all)]
      (is (pos? (:total pm-status)))
      (is (= :planetmath (:corpus pm-status)))
      (is (>= (:total all-status) (:total pm-status))))))

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
