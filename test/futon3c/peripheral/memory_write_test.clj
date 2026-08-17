(ns futon3c.peripheral.memory-write-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agents.zai-api :as zai]
            [futon3c.dispatch-with-recall :as dispatch]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.adapter :as adapter]
            [futon3c.peripheral.memory-backend :as memory-backend]
            [futon3c.peripheral.memory-write :as memory-write]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]))

(def fixture-path
  "holes/labs/M-typed-memories/p0-fixtures.edn")

(def test-ctx
  {:agent-id "zai-fixture"
   :domain :mathematics
   :session-id "zai-session-fixture"
   :turn-id "zai-turn-fixture"
   :round 7
   :mission-id "M-typed-memories"})

(defn- fixtures []
  (edn/read-string (slurp (io/file fixture-path))))

(defn- empty-store []
  (atom {:entries {} :order []}))

(defn- graph-stub
  [hyperedges]
  (fn [_ctx hyperedge]
    (swap! hyperedges assoc (:hx/id hyperedge) hyperedge)
    {:ok true :hyperedge hyperedge}))

(use-fixtures
  :each
  (fn [f]
    (estore/reset-store!)
    (try (f) (finally (estore/reset-store!)))))

(deftest all-fourteen-fixtures-land-unmodified
  (let [payloads (fixtures)
        store (empty-store)
        hyperedges (atom {})]
    (is (= 14 (count payloads)))
    (with-redefs [memory-write/post-hyperedge! (graph-stub hyperedges)]
      (doseq [payload payloads]
        (let [receipt (memory-write/record-memory!
                       (assoc test-ctx :evidence-store store)
                       payload)
              entry (estore/get-entry* store (:id receipt))
              hx (get @hyperedges (:hx-id receipt))
              subject-ids (mapv :ref/id (:subjects payload))]
          (is (true? (:ok receipt)) (:name payload))
          (is (str/starts-with? (:id receipt) "e-") (:name payload))
          (is (str/starts-with? (:hx-id receipt) "hx-mem-") (:name payload))
          (is (= :memory (:evidence/type entry)) (:name payload))
          (is (= :assert (:evidence/claim-type entry)) (:name payload))
          (is (= (first (:subjects payload)) (:evidence/subject entry)) (:name payload))
          (is (= [:memory :memory/assert] (:evidence/tags entry)) (:name payload))
          (is (= (:id receipt) (get-in hx [:hx/props :roles :entry])) (:name payload))
          (is (= subject-ids (get-in hx [:hx/props :roles :subjects])) (:name payload))
          (is (= [{:turn-id "zai-turn-fixture" :round 7}]
                 (get-in hx [:hx/props :roles :distills]))
              (:name payload))
          (is (= (boolean (:volatile? payload))
                 (get-in hx [:hx/props :volatile?]))
              (:name payload))
          (is (= :mathematics (get-in hx [:hx/props :domain]))
              (:name payload))
          (is (every? (set (:hx/endpoints hx))
                      (concat [(:id receipt)] subject-ids
                              [(:session-id test-ctx) (:mission-id test-ctx)]))
              (:name payload))))
    (is (= 14 (count (:order @store))))
    (is (= 14 (count @hyperedges))))))

(deftest identity-is-stamped-from-context-and-smuggled-values-are-ignored
  (let [store (empty-store)
        hyperedges (atom {})
        payload (assoc (first (fixtures))
                       :author "mallory"
                       :session-id "stolen-session"
                       :domain :war-machine
                       :evidence/author "also-mallory")]
    (with-redefs [memory-write/post-hyperedge! (graph-stub hyperedges)]
      (let [receipt (memory-write/record-memory!
                     (assoc test-ctx :evidence-store store)
                     payload)
            entry (estore/get-entry* store (:id receipt))]
        (is (true? (:ok receipt)))
        (is (= "zai-fixture" (:evidence/author entry)))
        (is (= "zai-session-fixture" (:evidence/session-id entry)))
        (is (= :mathematics
               (get-in (first (vals @hyperedges)) [:hx/props :domain])))
        (is (not (contains? (:evidence/body entry) :author)))
        (is (not (contains? (:evidence/body entry) :session-id)))))))

(deftest explicit-distills-are-endpoints-and-current-round-is-a-reference
  (let [store (empty-store)
        hyperedges (atom {})
        payload (assoc (first (fixtures))
                       :distills ["e-prior" "@current-round"])]
    (with-redefs [memory-write/post-hyperedge! (graph-stub hyperedges)]
      (let [receipt (memory-write/record-memory!
                     (assoc test-ctx :evidence-store store)
                     payload)
            hx (get @hyperedges (:hx-id receipt))]
        (is (= ["e-prior" {:turn-id "zai-turn-fixture" :round 7}]
               (get-in hx [:hx/props :roles :distills])))
        (is (some #{"e-prior"} (:hx/endpoints hx)))
        (is (not (some map? (:hx/endpoints hx))))))))

(deftest agent-supplied-pattern-attachment-is-only-a-proposal
  (let [store (empty-store)
        hyperedges (atom {})
        payload (assoc (first (fixtures))
                       :subjects
                       [{:ref/type :problem :ref/id "apm/a94A06"}
                        {:ref/type :pattern
                         :ref/id
                         "math-formalization/tactic-algebra-interference"}])]
    (with-redefs [memory-write/post-hyperedge! (graph-stub hyperedges)]
      (let [receipt (memory-write/record-memory!
                     (assoc test-ctx :evidence-store store)
                     payload)
            edge (get @hyperedges (:hx-id receipt))]
        (is (= :proposed
               (get-in edge [:hx/props :attachment-status])))
        (is (= ["math-formalization/tactic-algebra-interference"]
               (get-in edge [:hx/props :roles :patterns])))))))

(deftest problem-subject-vocabulary-joins-both-writers-and-legacy-rows
  (let [problem-id "a94A06"
        store (empty-store)
        hyperedges (atom {})
        payload (assoc (first (fixtures))
                       :subjects [{:ref/type :apm-problem :ref/id problem-id}])
        offered (dispatch/offered-evidence
                 {:problem problem-id :from "guide" :memory-channel :push}
                 {:memories [] :eligible-memory-ids [] :query {}}
                 "job-vocabulary" "session-vocabulary")]
    (with-redefs [memory-write/post-hyperedge! (graph-stub hyperedges)]
      (let [receipt (memory-write/record-memory!
                     (assoc test-ctx :evidence-store store) payload)
            written (estore/get-entry* store (:id receipt))
            canonical {:ref/type :problem :ref/id problem-id}
            legacy (assoc written
                          :evidence/id "e-legacy-apm-problem"
                          :evidence/subject
                          {:ref/type :apm-problem :ref/id problem-id})]
        (is (:ok receipt))
        (is (= canonical (:evidence/subject written)))
        (is (= canonical (:subject offered)))
        ;; Simulate an existing pre-normalization row without rewriting it.
        (swap! store (fn [state]
                       (-> state
                           (assoc-in [:entries (:evidence/id legacy)] legacy)
                           (update :order conj (:evidence/id legacy)))))
        (is (= #{(:id receipt) "e-legacy-apm-problem"}
               (set (map :evidence/id
                         (estore/query* store {:query/subject canonical})))))))))

(deftest evidence-failure-returns-fixed-id-and-social-error
  (let [receipt (memory-write/record-memory!
                 (assoc test-ctx :evidence-store (empty-store))
                 {:name "bad" :hook "bad" :kind :reference :body "bad"})]
    (is (false? (:ok receipt)))
    (is (str/starts-with? (:id receipt) "e-"))
    (is (shapes/valid? shapes/SocialError (:error receipt)))
    (is (nil? (:hx-id receipt)))))

(deftest hyperedge-failure-keeps-landed-evidence-and-surfaces-repairable-error
  (let [store (empty-store)
        calls (atom 0)
        hx-error {:error/component :E-store
                  :error/code :test-hx-failure
                  :error/message "graph unavailable"
                  :error/at "2026-07-22T00:00:00Z"}]
    (with-redefs [memory-write/post-hyperedge!
                  (fn [_ctx _hx]
                    (swap! calls inc)
                    {:ok false :error hx-error})]
      (let [receipt (memory-write/record-memory!
                     (assoc test-ctx :evidence-store store)
                     (first (fixtures)))]
        (is (true? (:ok receipt)))
        (is (= 1 @calls))
        (is (= hx-error (:hx-error receipt)))
        (is (some? (estore/get-entry* store (:id receipt))))))))

(deftest duplicate-names-create-distinct-memories
  (let [store (empty-store)
        hyperedges (atom {})
        payload (first (fixtures))]
    (with-redefs [memory-write/post-hyperedge! (graph-stub hyperedges)]
      (let [a (memory-write/record-memory! (assoc test-ctx :evidence-store store) payload)
            b (memory-write/record-memory! (assoc test-ctx :evidence-store store) payload)]
        (is (true? (:ok a)))
        (is (true? (:ok b)))
        (is (not= (:id a) (:id b)))
        (is (not= (:hx-id a) (:hx-id b)))
        (is (= 2 (count (:order @store))))))))

(deftest existing-memory-search-retrieves-recorded-memory-by-type-filter
  (with-redefs [memory-write/post-hyperedge! (graph-stub (atom {}))]
    (let [receipt (memory-write/record-memory!
                   (assoc test-ctx :evidence-store estore/!store)
                   (first (fixtures)))
          result (memory-backend/memory-search {} {:type "memory" :limit 10})
          ids (set (map :id (get-in result [:result :items])))]
      (is (true? (:ok receipt)))
      (is (contains? ids (:id receipt))))))

(deftest memory-record-tool-obeys-ablation-and-adapter-boundary
  (let [names (fn [mode] (set (map :name (#'zai/specs-for-mode mode))))
        spec {:peripheral/id :memory-test
              :peripheral/tools #{:memory-record}
              :peripheral/scope {:paths ["src/"]}}
        action (adapter/codex-tool-call->action
                spec {:name "memory_record"
                      :input {:name "n" :hook "h" :kind "reference" :body "b"}})]
    (is (contains? (names :full) "memory_record"))
    (is (not (contains? (names :files) "memory_record")))
    (is (not (contains? (names :none) "memory_record")))
    (is (= :memory-record (:tool action)))
    (is (= "n" (get-in action [:args 0 :name])))
    (is (tools/in-scope? :memory-record (:args action) spec))))

(deftest tool-description-carries-kind-boundary-rule-verbatim
  (let [spec (first (filter #(= "memory_record" (:name %))
                            @#'zai/tool-specs))]
    (is (str/includes?
         (:description spec)
         "derived-from-a-failure-with-a-why → feedback; documented contract/scope fact → reference"))))

(deftest tool-description-names-every-required-field
  (let [spec (first (filter #(= "memory_record" (:name %))
                            @#'zai/tool-specs))]
    (doseq [required ["name (non-blank)"
                      "body (content)"
                      "subjects [{ref/type, ref/id}]"]]
      (is (str/includes? (:description spec) required)))))

(deftest zai-dispatch-stamps-context-and-uses-peripheral-tool-path
  (let [backend (tools/make-mock-backend
                 {:memory-record (fn [_tool _args]
                                   {:ok true :result "recorded"})})
        payload {:name "dispatch-memory"
                 :hook "dispatch reaches memory writer"
                 :kind "reference"
                 :body "The dispatch path is wired."
                 :subjects [{:ref/type "tool" :ref/id "memory_record"}]}
        result (#'zai/execute-tool
                backend
                {:agent-id "zai-dispatch"
                 :session-id-atom (atom "sid-dispatch")
                 :turn-id "turn-dispatch"
                 :round 3
                 :evidence-store (empty-store)}
                {:id "tc-memory"
                 :function {:name "memory_record"
                            :arguments (json/generate-string payload)}})
        [{:keys [tool args]}] (tools/recorded-calls backend)]
    (is (= :memory-record tool))
    (is (= "zai-dispatch" (get-in args [0 :agent-id])))
    (is (= "sid-dispatch" (get-in args [0 :session-id])))
    (is (= "turn-dispatch" (get-in args [0 :turn-id])))
    (is (= 3 (get-in args [0 :round])))
    (is (= :zaif-work (get-in args [0 :domain])))
    (is (= "dispatch-memory" (get-in args [1 :name])))
    (is (false? (:error? result)))))

(deftest malformed-payload-fails-fast-with-actionable-fields
  ;; zai-1's live memory_record attempts (2026-07-24T21:50Z) reached the store
  ;; with a nil :evidence/subject and bounced as an opaque EvidenceEntry shape
  ;; error the agent could not act on. Validation now happens before any write
  ;; and names the missing fields.
  (let [store (empty-store)
        ctx (assoc test-ctx :evidence-store store)
        call (fn [payload]
               (memory-write/record-memory! ctx payload))
        fields (fn [receipt]
                 (set (map :field (get-in receipt [:error :error/context :fields]))))]
    (let [r (call {:hook "a hook only" :kind :observation})]
      (is (false? (:ok r)))
      (is (= :invalid-memory-payload (get-in r [:error :error/code])))
      (is (= #{:name :body :subjects} (fields r)))
      (is (str/includes? (get-in r [:error :error/message]) "name"))
      ;; nothing minted into the store on refusal
      (is (empty? (:entries @store))))
    (let [r (call {:name "has name" :body "has body"})]
      (is (false? (:ok r)))
      (is (= #{:subjects} (fields r))))
    (let [r (call {:name "has name" :body "has body"
                   :subjects [{:ref/type "mission"}]})]
      (is (false? (:ok r)))
      (is (= #{:subjects} (fields r))))
    (let [r (call {:name "valid" :body "valid content"
                   :subjects [{:ref/type "mission" :ref/id "M-typed-memories"}]})]
      (is (true? (:ok r)) (pr-str (:error r))))))

(deftest rejected-memory-payload-names-invalid-artifact-ref
  ;; Structurally present fields are not enough: a :ref/type outside
  ;; EvidenceEntry's ArtifactRefType enum must be NAMED, not bounced as an
  ;; opaque "did not conform to shape". Originally written against the exact
  ;; 2026-08-03 a96J01 payload, whose :git-commit subject was invalid at the
  ;; time; :git-commit was admitted to the enum the same day, so the
  ;; diagnosability regression now runs against a type that is still invalid.
  (let [store (empty-store)
        receipt
        (memory-write/record-memory!
         (assoc test-ctx :agent-id "zai-1"
                         :session-id "zai-bec940299024470eb815607f8b13b650"
                         :evidence-store store)
         {:name "a96j01-tent-disjoint-counterexample"
          :kind "feedback"
          :body "...via summable_nat_add_iff. Commit: d1606d0."
          :hook "APM a96J01 or Weierstrass M-test converse formalization in Lean 4"
          :why "Prior candidate sessions (6 of them) all left sorries; ..."
          :subjects [{:ref/id "d1606d0" :ref/type "git-blob"}]})
        fields (get-in receipt [:error :error/context :fields])
        subject-error (some #(when (= :subjects (:field %)) %) fields)]
    (is (false? (:ok receipt)))
    (is (= :invalid-memory-payload (get-in receipt [:error :error/code])))
    (is (some? subject-error))
    (is (str/includes? (:want subject-error) "ArtifactRef"))
    (is (= :git-blob
           (get-in subject-error [:got 0 :subject :ref/type])))
    (is (contains? (get-in subject-error [:got 0 :validation]) :ref/type))
    (is (empty? (:entries @store)))))

(deftest the-2026-08-03-refused-memory-can-now-be-recorded
  ;; The incident closed, not merely diagnosable. This is the exact payload
  ;; zai-1 sent mid-session on a96J01 and had refused twice; the content it
  ;; carried (the hasSum_single route) was lost to the write path. With
  ;; :git-commit in ArtifactRefType a runner can name the commit that
  ;; witnessed its own proof, which is what it was trying to do.
  (let [store (empty-store)
        receipt
        (memory-write/record-memory!
         (assoc test-ctx :agent-id "zai-1"
                         :session-id "zai-bec940299024470eb815607f8b13b650"
                         :evidence-store store)
         {:name "a96j01-tent-disjoint-counterexample"
          :kind "feedback"
          :body "...via summable_nat_add_iff. Commit: d1606d0."
          :hook "APM a96J01 or Weierstrass M-test converse formalization in Lean 4"
          :why "Prior candidate sessions (6 of them) all left sorries; ..."
          :subjects [{:ref/id "d1606d0" :ref/type "git-commit"}]})]
    (is (true? (:ok receipt)) (pr-str (:error receipt)))
    (is (seq (:entries @store)))))
