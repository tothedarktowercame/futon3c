(ns futon3c.apm.promotion-candidate-store-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-promotion :as live-promotion]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.apm.promotion-candidate-store :as sut]))

(def candidate
  {:memory-id "llm-invented-id"
   :content-digest "llm-invented-digest"
   :name "continuous-extension-tail-integrability"
   :hook "A continuous extension has integrable comparison tails"
   :kind :fact
   :body "Use locallyIntegrable.integrable_of_isBigO_atBot_atTop."
   :pattern-ids ["math-formalization/integrability-by-comparison"]
   :source-attempts [1]})

(def deposit-request
  {:dispatch/id "dispatch-atomic" :problem-id "p-atomic"
   :job-id "scribe-job" :agent-id "scribe"
   :source-attempt-ids ["solver-job"]})

(def deposit {:depositor "scribe" :candidates [candidate]})

(deftest controller-persists-and-verifies-candidates-before-review
  (let [entries (atom {}) edges (atom {})
        request {:dispatch/id "dispatch-1" :problem-id "p1"
                 :job-id "scribe-job" :agent-id "scribe"
                 :source-attempt-ids ["solver-job"]}
        result
        (sut/persist!
         {:depositor "scribe" :candidates [(dissoc candidate :kind)] :lanes []}
         request
         {:fetch-entry #(get @entries %)
          :post-memory-assert
          (fn [entry edge]
            (swap! entries assoc (:evidence/id entry) entry)
            (swap! edges assoc (:hx/id edge) edge)
            {:ok true :receipt {:evidence/id (:evidence/id entry)
                                :hx/id (:hx/id edge)}})
          :fetch-hyperedges
          (fn [end]
            (filterv #(some #{end} (:hx/endpoints %)) (vals @edges)))})
        persisted (first (:candidates result))
        entry (get @entries (:memory-id persisted))]
    (is (:ok result))
    (is (.startsWith (:memory-id persisted) "e-apm-promotion-"))
    (is (not= "llm-invented-id" (:memory-id persisted)))
    (is (= "llm-invented-id" (:reported-memory-id persisted)))
    (is (= :memory (:kind persisted)))
    (is (= pipeline/durable-memory-admission-schema
           (:admission/schema persisted)))
    (is (nil? (:reported-kind persisted)))
    (is (= ["solver-job"] (:source-attempts persisted)))
    (is (= [1] (:reported-source-attempts persisted)))
    (is (= "scribe" (get-in result [:deposit :depositor])))
    (is (= "scribe" (get-in result [:deposit :reported-depositor])))
    (is (= pipeline/durable-memory-admission-schema
           (get-in entry [:evidence/body :admission/schema])))
    (is (= (machine/ledger-digest [(:evidence/body entry)])
           (:content-digest persisted)))
    (is (= {:artifact-id (:memory-id persisted)
            :content-digest (:content-digest persisted)
            :persisted-content-digest (:content-digest persisted)
            :read-back-content-digest (:content-digest persisted)
            :persistence-receipt-id (:memory-id persisted)}
           (:materialization persisted)))
    (is (sut/visible? persisted #(get @entries %)
                      (fn [end]
                        (filterv #(some #{end} (:hx/endpoints %))
                                 (vals @edges)))))))

(deftest incomplete-content-cannot-produce-reviewable-candidate
  (let [writes (atom 0)
        result (sut/persist!
                {:depositor "scribe"
                 :candidates [(dissoc candidate :body)]}
                {:dispatch/id "dispatch" :problem-id "p"
                 :agent-id "scribe"
                 :source-attempt-ids ["source"]}
                {:fetch-entry (constantly nil)
                 :post-memory-assert
                 (fn [_ _] (swap! writes inc) {:ok true})
                 :fetch-hyperedges (constantly [])})]
    (is (= :promotion-candidate-content-invalid (:error/code result)))
    (is (some #{:candidate-body-missing}
              (mapcat :findings (:findings result))))
    (is (zero? @writes))))

(deftest controller-derives-student-and-repair-source-attempts
  (is (= ["student-1" "repair-1" "student-2"]
         (sut/controller-source-attempts
          {:student-attempts [{:job-id "student-1"
                               :repair-job-ids ["repair-1"]}
                              {:job-id "student-2"
                               :repair-job-ids []}]}))))

(deftest missing-controller-source-authority-fails-before-write
  (let [writes (atom 0)
        result (sut/persist!
                {:depositor "scribe" :candidates [(dissoc candidate :source-attempts)]}
                {:dispatch/id "dispatch" :problem-id "p"
                 :agent-id "scribe"}
                {:fetch-entry (constantly nil)
                 :post-memory-assert
                 (fn [_ _] (swap! writes inc) {:ok true})
                 :fetch-hyperedges (constantly [])})]
    (is (= :promotion-candidate-content-invalid (:error/code result)))
    (is (some #(= :controller-source-attempts-missing (:finding %))
              (:findings result)))
    (is (zero? @writes))))

(deftest depositor-is-derived-from-dispatch-authority
  (let [entries (atom {}) edges (atom {})
        result
        (sut/persist!
         {:depositor "reported-other" :candidates [candidate]}
         {:dispatch/id "dispatch" :problem-id "p" :agent-id "f40-scribe"
          :source-attempt-ids ["solver-job"]}
         {:fetch-entry #(get @entries %)
          :post-memory-assert
          (fn [entry edge]
            (swap! entries assoc (:evidence/id entry) entry)
            (swap! edges assoc (:hx/id edge) edge)
            {:ok true})
          :fetch-hyperedges
          (fn [end]
            (filterv #(some #{end} (:hx/endpoints %)) (vals @edges)))})]
    (is (:ok result) result)
    (is (= "f40-scribe" (get-in result [:deposit :depositor])))
    (is (= "reported-other" (get-in result [:deposit :reported-depositor])))
    (is (= "f40-scribe"
           (:evidence/author
            (get @entries (get-in result [:candidates 0 :memory-id])))))))

(deftest controller-derives-kind-and-retains-agent-claim
  (let [ordinary (sut/canonical-candidate
                  {:dispatch/id "dispatch"} "scribe" 1
                  (dissoc candidate :kind))
        misleading (sut/canonical-candidate
                    {:dispatch/id "dispatch"} "scribe" 2
                    (assoc candidate :kind :proof-text))
        proof-text (sut/canonical-candidate
                    {:dispatch/id "dispatch"} "scribe" 3
                    (assoc candidate :kind :fact
                           :body (apply str (repeat 4 "lemma x : True := by\n"))))]
    (is (= :memory (:kind ordinary)))
    (is (nil? (:reported-kind ordinary)))
    (is (= :memory (:kind misleading)))
    (is (= :proof-text (:reported-kind misleading)))
    (is (= :proof-text (:kind proof-text)))
    (is (= :fact (:reported-kind proof-text)))))

(deftest independently-reassigned-candidate-remains-visible-on-replay
  (let [memory-id "e-memory"
        review-id "e-review"
        original-pattern "math-formalization/original"
        reassigned-pattern "math-formalization/canonical"
        body (select-keys candidate [:name :hook :kind :body :why :how-to-apply])
        persisted (assoc candidate
                         :memory-id memory-id
                         :content-digest (machine/ledger-digest [body])
                         :pattern-ids [original-pattern])
        entries
        {memory-id {:evidence/id memory-id :evidence/body body}
         review-id {:evidence/id review-id
                    :evidence/body
                    {:review/memory-id memory-id
                     :review/verdict :reassign}}}
        edge {:hx/id "hx-memory" :hx/type :memory/assert
              :hx/endpoints [memory-id reassigned-pattern]
              :hx/props
              {:state :current :attachment-status :reviewed
               :roles {:entry memory-id :patterns [reassigned-pattern]}
               :review {:evidence-id review-id :verdict :reassign
                        :pattern-ids [reassigned-pattern]}}}]
    (is (sut/visible? persisted #(get entries %)
                      (constantly [edge])))))

(deftest reviewed-candidate-with-unreadable-review-is-not-visible
  (let [memory-id "e-memory"
        body (select-keys candidate [:name :hook :kind :body :why :how-to-apply])
        persisted (assoc candidate
                         :memory-id memory-id
                         :content-digest (machine/ledger-digest [body]))
        edge {:hx/id "hx-memory" :hx/type :memory/assert
              :hx/endpoints [memory-id "math-formalization/canonical"]
              :hx/props
              {:state :current :attachment-status :reviewed
               :roles {:entry memory-id
                       :patterns ["math-formalization/canonical"]}
               :review {:evidence-id "missing-review" :verdict :reassign
                        :pattern-ids ["math-formalization/canonical"]}}}]
    (is (not (sut/visible? persisted
                           #(when (= memory-id %)
                              {:evidence/id memory-id :evidence/body body})
                           (constantly [edge]))))))

(deftest rejected-pair-never-falls-back-to-separate-entry-or-edge-writes
  (let [separate-entry-writes (atom 0)
        separate-edge-writes (atom 0)
        result
        (sut/persist!
         deposit deposit-request
         {:fetch-entry (constantly nil)
          ;; Deliberately supplied sentinels prove the migrated store no longer
          ;; has a two-call fallback.
          :append-entry (fn [_] (swap! separate-entry-writes inc))
          :post-edge (fn [_] (swap! separate-edge-writes inc))
          :post-memory-assert
          (fn [_ _]
            {:ok false
             :error {:error/component :E-store
                     :error/code :memory-assert-rejected
                     :error/context {:status 400 :body {:error "invalid"}}}})
          :fetch-hyperedges (constantly [])})]
    (is (= :promotion-candidate-evidence-write-failed (:error/code result)))
    (is (zero? @separate-entry-writes))
    (is (zero? @separate-edge-writes))))

(deftest atomic-route-transport-failure-remains-retryable
  (let [result
        (sut/persist!
         deposit deposit-request
         {:fetch-entry (constantly nil)
          :post-memory-assert
          (fn [_ _]
            {:ok false
             :error {:error/component :transport
                     :error/code :memory-assert-unreachable}})
          :fetch-hyperedges (constantly [])})]
    (is (= :promotion-candidate-edge-write-failed (:error/code result)))
    (is (= :transport
           (get-in result [:finding :error :error/component])))
    (is (#'live-promotion/transport-failure? result))))

(deftest already-persisted-pair-replays-without-writing
  (let [entries (atom {}) edges (atom {}) writes (atom 0)
        ports {:fetch-entry #(get @entries %)
               :post-memory-assert
               (fn [entry edge]
                 (swap! writes inc)
                 (swap! entries assoc (:evidence/id entry) entry)
                 (swap! edges assoc (:hx/id edge) edge)
                 {:ok true})
               :fetch-hyperedges
               (fn [end]
                 (filterv #(some #{end} (:hx/endpoints %)) (vals @edges)))}
        first-result (sut/persist! deposit deposit-request ports)
        replay (sut/persist! deposit deposit-request ports)]
    (is (:ok first-result))
    (is (:ok replay))
    (is (= 1 @writes))))

(deftest preserved-write-error-codes-remain-reachable
  (let [base {:fetch-hyperedges (constantly [])}
        evidence-not-visible
        (sut/persist! deposit deposit-request
                      (merge base {:fetch-entry (constantly nil)
                                   :post-memory-assert
                                   (fn [_ _] {:ok true})}))
        stored-entry (atom nil)
        edge-not-visible
        (sut/persist! deposit deposit-request
                      (merge base
                             {:fetch-entry (fn [_] @stored-entry)
                              :post-memory-assert
                              (fn [entry _]
                                (reset! stored-entry entry)
                                {:ok true})}))
        edge-write-failed
        (sut/persist!
         deposit deposit-request
         (merge base {:fetch-entry (constantly nil)
                      :post-memory-assert
                      (fn [_ _]
                        {:ok false
                         :error {:error/component :E-store
                                 :error/code :memory-assert-rejected
                                 :error/context
                                 {:body {:error
                                         {:reason :invalid-hyperedge}}}}})}))
        id-conflict
        (sut/persist! deposit deposit-request
                      (merge base {:fetch-entry
                                   (constantly {:evidence/id "occupied"
                                                :evidence/body {:body "other"}})
                                   :post-memory-assert
                                   (fn [_ _] {:ok true})}))]
    (is (= :promotion-candidate-evidence-not-visible
           (:error/code evidence-not-visible)))
    (is (= :promotion-candidate-edge-not-visible
           (:error/code edge-not-visible)))
    (is (= :promotion-candidate-edge-write-failed
           (:error/code edge-write-failed)))
    (is (= :promotion-candidate-id-conflict (:error/code id-conflict)))
    ;; Content validation is exercised independently before any port call.
    (is (= :promotion-candidate-content-invalid
           (:error/code
            (sut/persist! (assoc deposit :candidates [(dissoc candidate :body)])
                          deposit-request
                          {:fetch-entry (constantly nil)
                           :post-memory-assert (fn [_ _] {:ok true})
                           :fetch-hyperedges (constantly [])}))))))

(deftest review-inputs-require-a-full-persisted-body
  (let [body {:name "n" :hook "h" :kind :memory :body "complete body"}
        candidate {:memory-id "m" :content-digest (machine/ledger-digest [body])}
        accepted (sut/review-inputs [candidate]
                                    (constantly {:evidence/id "m"
                                                 :evidence/body body})
                                    "http://substrate/")]
    (is (:ok accepted))
    (is (= body (get-in accepted [:candidate-evidence 0 :entry :evidence/body])))
    (is (= "http://substrate/api/alpha/evidence/m"
           (get-in accepted [:candidate-evidence 0 :read-ref])))
    (is (= :promotion-review-candidate-evidence-unfetchable
           (:error/code (sut/review-inputs [candidate] (constantly nil)
                                           "http://substrate"))))
    (is (= :promotion-review-candidate-body-missing
           (:error/code
            (sut/review-inputs [candidate]
                               (constantly {:evidence/id "m"
                                            :evidence/body {:hook "only"}})
                               "http://substrate"))))))

(deftest review-input-timeout-is-a-typed-retryable-transport-outcome
  (let [candidate {:memory-id "m" :content-digest "digest"}
        timeout (java.net.http.HttpTimeoutException. "request timed out")
        result (sut/review-inputs
                [candidate]
                (fn [_]
                  (throw (java.util.concurrent.ExecutionException. timeout)))
                "http://substrate")]
    (is (= :promotion-review-candidate-evidence-timeout
           (:error/code result)))
    (is (= :transport (:error/component result)))
    (is (= :timeout (:transport/acquired-outcome result)))
    (is (= :not-obtained (:transport/evidence result)))
    (is (#'live-promotion/transport-failure? result))))

(deftest review-input-nontransport-exception-still-escapes
  (is (thrown-with-msg?
       IllegalStateException #"programmer fault"
       (sut/review-inputs [{:memory-id "m" :content-digest "digest"}]
                          (fn [_] (throw (IllegalStateException.
                                         "programmer fault")))
                          "http://substrate"))))

;; A pair written before the atomic route existed can be half-present: the
;; evidence committed and the edge did not. The atomic route refuses that
;; evidence id with 409, so the pair write cannot supply the missing edge --
;; only an edge-only write repairs it. Without one this state is permanent.
(deftest half-written-legacy-pair-is-repaired-by-an-edge-only-write
  (let [entries (atom {}) edges (atom {})
        pair-writes (atom 0) edge-writes (atom 0)
        ;; seed the orphan: evidence present, no edge
        seeded (sut/persist! deposit deposit-request
                             {:fetch-entry (constantly nil)
                              :post-memory-assert
                              (fn [entry _edge]
                                (swap! entries assoc (:evidence/id entry) entry)
                                {:ok true})
                              :fetch-hyperedges (constantly [])})
        result (sut/persist!
                deposit deposit-request
                {:fetch-entry #(get @entries %)
                 :post-memory-assert
                 (fn [_ _]
                   (swap! pair-writes inc)
                   {:ok false :duplicate? true
                    :error {:error/component :E-store
                            :error/code :memory-assert-duplicate}})
                 :post-edge (fn [edge]
                              (swap! edge-writes inc)
                              (swap! edges assoc (:hx/id edge) edge)
                              {:ok true})
                 :fetch-hyperedges
                 (fn [end]
                   (filterv #(some #{end} (:hx/endpoints %)) (vals @edges)))})]
    (is (false? (:ok seeded)) "seeding leaves the edge missing")
    (is (:ok result) "the orphaned entry gets its edge and the deposit completes")
    (is (= 1 @edge-writes) "repaired by exactly one edge-only write")))
