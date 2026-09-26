(ns futon3c.peripheral.memory-lifecycle-review-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.memory-lifecycle :as lifecycle]
            [futon3c.peripheral.memory-recall :as recall]))

(def memory-id "e-memory")
(def review-id "e-review")
(def pattern-id "p4ng/R9-independent-witness")

(def memory-entry
  {:evidence/id memory-id
   :evidence/subject {:ref/type :mission :ref/id "mission/live-verify"}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author "codex-4"
   :evidence/session-id "live-memory-selection-verify"
   :evidence/at "2026-07-24T10:00:00Z"
   :evidence/body {:name "R9 live witness"
                   :hook "Require an independently checked trace."
                   :kind :observation
                   :body "A full bootstrap episode."}})

(defn review-entry
  ([id verdict]
   (review-entry id verdict "claude-4" [pattern-id]))
  ([id verdict reviewer patterns]
   (review-entry id verdict reviewer patterns nil))
  ([id verdict reviewer patterns memory-use-kind]
   {:evidence/id id
    :evidence/subject {:ref/type :memory :ref/id memory-id}
    :evidence/type :memory
    :evidence/claim-type :observation
    :evidence/author reviewer
    :evidence/session-id "live-memory-selection-verify"
    :evidence/at "2026-07-24T10:05:00Z"
    :evidence/body
    (cond->
     {:review/event :memory-attachment-review
      :review/memory-id memory-id
      :review/pattern-ids patterns
      :review/verdict verdict
      :review/witness-status :independently-witnessed
      :review/provenance
      {:kind :independent-check
       :evidence ["test output" "trace artifact"]}}
      (some? memory-use-kind)
      (assoc :memory-use/kind memory-use-kind))}))

(def proposed-edge
  {:hx/id "hx-memory"
   :hx/type :memory/assert
   :hx/endpoints [memory-id "mission/live-verify" pattern-id]
   :hx/props
   {:domain :war-machine
    :state :current
    :attachment-status :proposed
    :witness-status :self-asserted
    :roles {:entry memory-id
            :subjects ["mission/live-verify" pattern-id]
            :patterns [pattern-id]
            :mission "mission/live-verify"}}})

(def ctx
  {:domain :war-machine
   :agent-id "claude-4"
   :session-id "live-memory-selection-verify"})

(defn graph-fixture
  ([edge] (graph-fixture edge false))
  ([edge stale-after-post?]
   (let [current (atom edge)
         posts (atom [])
         posted? (atom false)]
     {:posts posts
      :fetch
      (fn [endpoint {:keys [type limit]}]
        (let [visible (if (and stale-after-post? @posted?) edge @current)]
          (if (and (= type (:hx/type visible))
                   (some #{endpoint} (:hx/endpoints visible)))
            (vec (take limit [visible]))
            [])))
      :post
      (fn [_ updated]
        (reset! posted? true)
        (reset! current updated)
        (swap! posts conj updated)
        {:ok true :hyperedge updated})})))

(defn opts
  [graph entries]
  {:fetch-hyperedges (:fetch graph)
   :fetch-entry #(get entries %)
   :post-hyperedge (:post graph)})

(def request
  {:memory-id memory-id
   :review-evidence-id review-id
   :verdict :approve
   :pattern-ids [pattern-id]})

(def statusless-edge
  (-> proposed-edge
      (update :hx/endpoints #(vec (remove #{pattern-id} %)))
      (update-in [:hx/props :roles :subjects]
                 #(vec (remove #{pattern-id} %)))
      (assoc-in [:hx/props :roles :patterns] [])
      (update :hx/props dissoc :attachment-status :witness-status)))

(defn- promote-opts [graph entries]
  (assoc (opts graph entries)
         :fetch-entry #(get @entries %)
         :append-evidence (fn [entry]
                            (swap! entries assoc (:evidence/id entry) entry)
                            {:ok true :entry entry})))

(deftest promotion-reviewer-authentication-is-explicit
  (let [promotion {:memory-id memory-id
                   :pattern-id pattern-id
                   :reviewer "claude-4"}]
    (testing "a direct call without authenticated actor records that fact"
      (let [graph (graph-fixture statusless-edge)
            entries (atom {memory-id memory-entry})
            result (lifecycle/promote-memory-attachment!
                    ctx promotion (promote-opts graph entries))]
        (is (:ok result))
        (is (= [:reviewer-unauthenticated] (:findings result)))))
    (testing "an authenticated matching actor passes without the finding"
      (let [graph (graph-fixture statusless-edge)
            entries (atom {memory-id memory-entry})
            result (lifecycle/promote-memory-attachment!
                    (assoc ctx :acting-identity "claude-4") promotion
                    (promote-opts graph entries))]
        (is (:ok result))
        (is (empty? (:findings result)))))
    (testing "an authenticated mismatched actor is refused before writes"
      (let [graph (graph-fixture statusless-edge)
            entries (atom {memory-id memory-entry})
            result (lifecycle/promote-memory-attachment!
                    (assoc ctx :acting-identity "claude-7") promotion
                    (promote-opts graph entries))]
        (is (false? (:ok result)))
        (is (= :reviewer-not-actor (get-in result [:finding :failure])))
        (is (empty? @(:posts graph)))
        (is (= #{memory-id} (set (keys @entries))))))))

(deftest promotion-refuses-the-actual-seat-depositor
  (let [seat "f9-guide"
        graph (graph-fixture statusless-edge)
        entries (atom {memory-id (assoc memory-entry
                                        :evidence/author seat
                                        :evidence/via "problem-peripheral")})
        result (lifecycle/promote-memory-attachment!
                (assoc ctx :acting-identity seat)
                {:memory-id memory-id :pattern-id pattern-id :reviewer seat}
                (promote-opts graph entries))]
    (is (false? (:ok result)))
    (is (= :promotion-reviewer-is-depositor
           (get-in result [:finding :failure])))
    (is (= seat (get-in result [:finding :depositor])))))

(deftest promotion-consumes-independent-review-of-proposed-attachment
  (let [graph (graph-fixture proposed-edge)
        entries (atom {memory-id (assoc memory-entry :evidence/author "frame-guide")
                       review-id (review-entry review-id :approve)})
        result
        (lifecycle/promote-memory-attachment!
         (assoc ctx :acting-identity "frame-guide"
                    :agent-id "frame-guide"
                    :session-id "guide-session")
         {:memory-id memory-id
          :pattern-ids [pattern-id]
          :verdict :approve
          :reviewer "forged-caller-claim"
          :review-evidence-id review-id}
         (promote-opts graph entries))]
    (is (true? (:ok result)) result)
    (is (= "claude-4" (:reviewer result)))
    (is (= :reviewed (:attachment-status result)))
    (is (= review-id (get-in (first @(:posts graph))
                             [:hx/props :review :evidence-id])))
    (is (= "claude-4" (get-in (first @(:posts graph))
                               [:hx/props :review :reviewer])))))

(deftest promotion-existing-review-refuses-depositor-authorship
  (let [graph (graph-fixture proposed-edge)
        self-review (review-entry review-id :approve "codex-4" [pattern-id])
        entries (atom {memory-id memory-entry review-id self-review})
        result
        (lifecycle/promote-memory-attachment!
         (assoc ctx :acting-identity "frame-guide")
         {:memory-id memory-id
          :pattern-ids [pattern-id]
          :verdict :approve
          :review-evidence-id review-id}
         (promote-opts graph entries))]
    (is (false? (:ok result)))
    (is (= :promotion-reviewer-is-depositor
           (get-in result [:finding :failure])))
    (is (= "codex-4" (get-in result [:finding :reviewer])))
    (is (empty? @(:posts graph)))))

(deftest approval-requires-independent-exact-review-evidence
  (testing "memory author cannot review their own attachment"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"cannot review"
           (lifecycle/review-attachment!
            ctx request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve
                                                "codex-4" [pattern-id])}))))))
  (testing "review evidence must exist"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"not found"
           (lifecycle/review-attachment!
            ctx request (opts graph {memory-id memory-entry}))))))
  (testing "review must name the exact attachment"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"exact attachment"
           (lifecycle/review-attachment!
            ctx request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve
                                                "claude-4" ["p4ng/R6"])})))))))

(deftest review-refuses-stale-memory-and-wrong-domain
  (testing "superseded memory"
    (let [edge (assoc-in proposed-edge [:hx/props :state] :superseded)
          graph (graph-fixture edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"only a current"
           (lifecycle/review-attachment!
            ctx request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve)}))))))
  (testing "retracted memory has no current edge to approve"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"edge not found"
         (lifecycle/review-attachment!
          ctx request
          {:fetch-hyperedges (fn [_ _] [])
           :fetch-entry (constantly nil)
           :post-hyperedge (fn [_ _] {:ok true})}))))
  (testing "wrong domain"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"domain mismatch"
           (lifecycle/review-attachment!
            (assoc ctx :domain :mathematics)
            request
            (opts graph {memory-id memory-entry
                         review-id (review-entry review-id :approve)})))))))

(deftest approval-is-visible-to-recall-and-exact-retry-is-idempotent
  (let [graph (graph-fixture proposed-edge)
        entries {memory-id memory-entry
                 review-id (review-entry review-id :approve)}
        first-result
        (lifecycle/review-attachment! ctx request (opts graph entries))
        replay
        (lifecycle/review-attachment! ctx request (opts graph entries))
        projection
        (recall/recall-by-endpoint
         (assoc ctx :evidence-store nil)
         pattern-id
         {:fetch-hyperedges (:fetch graph)
          :fetch-entry #(get entries %)})]
    (is (true? (:ok first-result)) first-result)
    (is (= :fresh-endpoint-read (:cache-postcondition first-result)))
    (is (= :reviewed (:attachment-status first-result)))
    (is (= :independently-witnessed (:witness-status first-result)))
    (is (true? (:ok replay)) replay)
    (is (true? (:idempotent-replay? replay)))
    (is (= 1 (count @(:posts graph))))
    (is (= [memory-id] (mapv :memory/id (:memories projection))))
    (is (= :reviewed
           (get-in (first (:memories projection))
                   [:memory/attachment-status])))))

(deftest rejection-is-recorded-without-promoting-the-proposed-attachment
  (let [graph (graph-fixture proposed-edge)
        approval (review-entry review-id :approve)
        reject-id "e-review-reject"
        rejection (review-entry reject-id :reject)
        entries {memory-id memory-entry
                 review-id approval
                 reject-id rejection}
        approved
        (lifecycle/review-attachment! ctx request (opts graph entries))
        rejected
        (lifecycle/review-attachment!
         ctx (assoc request
                    :review-evidence-id reject-id
                    :verdict :reject)
         (opts graph entries))]
    (is (true? (:ok approved)))
    (is (true? (:ok rejected)) rejected)
    (is (= :proposed (:attachment-status rejected)))
    (is (= :reject (get-in (second @(:posts graph))
                           [:hx/props :review :verdict])))
    (is (= "live-memory-selection-verify"
           (get-in (second @(:posts graph))
                   [:hx/props :review :session-id])))
    (is (not (:idempotent-replay? rejected)))
    (is (= 2 (count @(:posts graph))))))

(deftest reassign-records-the-returned-verdict-and-repoints-the-attachment
  (let [new-pattern "p4ng/R10-canonical"
        graph (graph-fixture proposed-edge)
        evidence (review-entry review-id :reassign "claude-4" [new-pattern])
        result
        (lifecycle/review-attachment!
         ctx (assoc request :verdict :reassign :pattern-ids [new-pattern])
         (opts graph {memory-id memory-entry review-id evidence}))
        edge (first @(:posts graph))]
    (is (:ok result) result)
    (is (= :reviewed (:attachment-status result)))
    (is (= :reassign (get-in edge [:hx/props :review :verdict])))
    (is (= [new-pattern] (get-in edge [:hx/props :roles :patterns])))
    (is (some #{new-pattern} (:hx/endpoints edge)))
    (is (not-any? #{pattern-id} (:hx/endpoints edge)))))

(deftest differing-patterns-are-constrained-only-by-publishing-verdicts
  (let [new-pattern "p4ng/R10-canonical"]
    (doseq [[verdict expected-status expected-patterns]
            [[:reject :proposed [pattern-id]]
             [:challenge :challenged [pattern-id]]
             [:reassign :reviewed [new-pattern]]]]
      (let [id (str "e-review-" (name verdict))
            graph (graph-fixture proposed-edge)
            evidence (review-entry id verdict "claude-4" [new-pattern])
            result
            (lifecycle/review-attachment!
             ctx {:memory-id memory-id :review-evidence-id id
                  :verdict verdict :pattern-ids [new-pattern]}
             (opts graph {memory-id memory-entry id evidence}))
            edge (first @(:posts graph))]
        (is (:ok result) (str verdict " " (pr-str result)))
        (is (= expected-status (:attachment-status result)) (str verdict))
        (is (= expected-patterns
               (get-in edge [:hx/props :roles :patterns])) (str verdict))))
    (let [id "e-review-approve-mismatch"
          graph (graph-fixture proposed-edge)
          evidence (review-entry id :approve "claude-4" [new-pattern])]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"pattern set does not match"
           (lifecycle/review-attachment!
            ctx {:memory-id memory-id :review-evidence-id id
                 :verdict :approve :pattern-ids [new-pattern]}
            (opts graph {memory-id memory-entry id evidence}))))
      (is (empty? @(:posts graph))))))

(deftest invocation-reviewer-must-match-evidence-author
  (let [graph (graph-fixture proposed-edge)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"identity does not match"
         (lifecycle/review-attachment!
          (assoc ctx :agent-id "claude-9") request
          (opts graph {memory-id memory-entry
                       review-id (review-entry review-id :approve)}))))))

(deftest successful-repost-must-be-immediately-visible
  (let [graph (graph-fixture proposed-edge true)
        result
        (lifecycle/review-attachment!
         ctx request
         (opts graph {memory-id memory-entry
                      review-id (review-entry review-id :approve)}))]
    (is (false? (:ok result)))
    (is (= :cache-postcondition (:stage result)))
    (is (= :stale-after-successful-repost
           (:cache-postcondition result)))))

(deftest memory-use-kind-is-optional-reviewed-metadata
  (testing "an adjudicated kind is validated, projected, and recalled"
    (let [graph (graph-fixture proposed-edge)
          entries {memory-id memory-entry
                   review-id (review-entry review-id :approve "claude-4"
                                           [pattern-id] :regulative)}
          result (lifecycle/review-attachment! ctx request
                                               (opts graph entries))
          projection (recall/recall-by-endpoint
                      (assoc ctx :evidence-store nil) pattern-id
                      {:fetch-hyperedges (:fetch graph)
                       :fetch-entry #(get entries %)})]
      (is (true? (:ok result)))
      (is (= :regulative (:memory-use/kind result)))
      (is (= :regulative
             (:memory-use/kind (first (:memories projection)))))))
  (testing "an unadjudicated memory remains valid and unmarked"
    (let [graph (graph-fixture proposed-edge)
          entries {memory-id memory-entry
                   review-id (review-entry review-id :approve)}
          result (lifecycle/review-attachment! ctx request
                                               (opts graph entries))]
      (is (true? (:ok result)))
      (is (nil? (:memory-use/kind result)))
      (is (not (contains? (first @(:posts graph)) :memory-use/kind)))
      (is (not (contains? (get-in (first @(:posts graph)) [:hx/props])
                          :memory-use/kind)))))
  (testing "the vocabulary is closed at write time"
    (let [graph (graph-fixture proposed-edge)]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"invalid memory-use kind"
           (lifecycle/review-attachment!
            ctx request
            (opts graph
                  {memory-id memory-entry
                   review-id (review-entry review-id :approve "claude-4"
                                           [pattern-id] :both)})))))))
