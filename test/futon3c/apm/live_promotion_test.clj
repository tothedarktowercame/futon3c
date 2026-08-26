(ns futon3c.apm.live-promotion-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-promotion :as sut]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.apm.typed-role-submission :as submission]))

(defn materialization [id digest]
  {:artifact-id id :content-digest digest
   :persisted-content-digest digest :read-back-content-digest digest
   :persistence-receipt-id id})

(deftest dispatch-failure-retains-the-failing-boundary
  (with-redefs [job-port/announce!
                (constantly {:ok false :error/code :announcement-refused})
                submission/register!
                (fn [& _] (throw (ex-info "must not register" {})))
                job-port/activate!
                (fn [& _] (throw (ex-info "must not activate" {})))]
    (let [stage (#'sut/agency-stage
                 "http://agency" {:agent-id "scribe" :submission/job-id "job"}
                 "prompt")
          result (stage)]
      (is (= :promotion-stage-dispatch-failed (:error/code result)))
      (is (= :announcement-refused
             (get-in result [:dispatch :announced :error/code])))
      (is (nil? (get-in result [:dispatch :registered])))
      (is (nil? (get-in result [:dispatch :activated]))))))

(deftest typed-evidence-receipt-is-expanded-before-promotion-validation
  (let [report (#'sut/submitted-report
                {:authority {:frame-id "f27" :role :scribe}
                 :payload {:command-own-exit 0 :outcome "deposited"
                           :failure-account []
                           :evidence
                           {:receipt
                            "{:depositor \"f27-scribe\" :candidates [{:memory-id \"m\"}] :lanes []}"}}})]
    (is (= "f27-scribe" (:depositor report)))
    (is (= [{:memory-id "m"}] (:candidates report)))
    (is (= [] (:lanes report)))
    (is (= "f27" (:frame-id report)))
    (is (= 0 (:command-own-exit report)))))

(deftest scribe-promotion-requires-authoritative-typed-receipt
  (let [auth {:phase :promote-solver :role :scribe}
        missing (submission/validate-payload
                 auth {:command-own-exit 0 :outcome "deposited"
                       :failure-account []
                       :evidence {:memory-search-receipt-ids []}})]
    (is (= #{:receipt} (:evidence/missing missing)))
    (is (contains? (submission/evidence-required auth) :receipt))))

(deftest f29-string-enum-deposit-is-normalized-at-typed-boundary
  (let [fixture (edn/read-string
                 (slurp "test/fixtures/apm/f29-promotion-deposit-string-enums.edn"))
        raw (pipeline/validate-deposit fixture)
        report (#'sut/submitted-report
                {:authority {:frame-id "f29" :role :scribe}
                 :payload {:evidence fixture}})
        validated (pipeline/validate-deposit report)]
    (is (= [:lane-report-invalid] (:findings raw))
        "the pure promotion gate remains keyword-typed")
    (is (:ok validated))
    (is (= #{:solve :arc :trajectory :challenge}
           (set (map :lane (:lanes report)))))
    (is (= #{:ran :ran-empty :not-run}
           (set (map :status (:lanes report)))))))

(deftest promotion-wire-keyword-also-accepts-json-without-edn-colon
  (is (= :solve (#'sut/wire-keyword "solve")))
  (is (= :solve (#'sut/wire-keyword ":solve"))))

(deftest relative-frozen-card-path-is-resolved-against-control-root
  (is (= "/control/holes/cards/scribe-v3.md"
         (sut/resolved-role-card-path
          "/control" {:role-card-path "holes/cards/scribe-v3.md"})))
  (is (= "/frozen/scribe-v3.md"
         (sut/resolved-role-card-path
          "/control" {:role-card-path "/frozen/scribe-v3.md"}))))

(deftest deposit-review-publish-is-durable-and-ordered
  (let [saved (atom nil) calls (atom [])
        candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1 2 3]}
        lanes [{:lane :solve :status :ran}
               {:lane :arc :status :ran-empty :reason "no errors"}
               {:lane :trajectory :status :ran}
               {:lane :challenge :status :not-run :reason "no prior claim"}]
        base {:persist-fn #(do (reset! saved %) {:ok true})
              :publish-fn (fn [publication]
                            (is (= "scribe" (get-in publication
                                                     [:deposit :depositor])))
                            (is (= "proctor" (:reviewer publication)))
                            (swap! calls conj :publish)
                            {:ok true :receipt {:receipt/id "promotion"}})}
        r1 (sut/drive! (merge base {:state nil
                                    :deposit-fn #(do (swap! calls conj :deposit)
                                                     {:ok true :job "scribe"})}))
        r2 (sut/drive! (merge base {:state (:state r1)
                                    :deposit-fn (fn [_] {:ok true :report
                                                        {:depositor "scribe"
                                                         :candidates [candidate]
                                                         :lanes lanes}})
                                    :review-fn (fn [_] (swap! calls conj :review)
                                                 {:ok true :job "proctor"})}))
        review {:memory-id "m" :reviewer "proctor" :verdict :approve
                :review-evidence-id "e" :attachment-status :reviewed
                :pattern-ids ["p"] :reason "actionable fact"
                :residual "Main.lean:12"}
        r3 (sut/drive! (merge base {:state (:state r2)
                                    :review-fn (fn [& _] {:ok true
                                                         :reviewer "proctor"
                                                         :reviews [review]})}))]
    (is (= :awaiting-terminal (:status r1)))
    (is (= "scribe" (:job-id r1)))
    (is (= :independent-review (get-in r2 [:state :stage])))
    (is (= "proctor" (:job-id r2)))
    (is (= :certified (:status r3)))
    (is (= [:deposit :review :publish] @calls))))

(deftest malformed-deposit-is-durably-redispatched-without-review
  (let [saved (atom nil)
        review-called? (atom false)
        repair-attempt (atom nil)
        result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "malformed" :attempt 1}
                 :deposit-fn (fn
                               ([value]
                               (if (string? value)
                                  (do (is (= "malformed" value))
                                      {:ok false
                                       :error/code :promotion-stage-terminal-invalid})
                                  (do (reset! repair-attempt
                                              (:submission/attempt value))
                                      {:ok true :job "scribe-retry"})))
                               ([] {:ok true :job "scribe-retry"}))
                 :review-fn (fn [& _] (reset! review-called? true))
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (:ok result))
    (is (= :awaiting-terminal (:status result)))
    (is (= "scribe-retry" (:job-id result)))
    (is (= {:job-id "scribe-retry"} (:ticket @saved)))
    (is (= 2 (:attempt @saved)))
    (is (= 1 @repair-attempt))
    (is (= [{:attempt 1 :job "malformed"
             :failure {:error/code :promotion-stage-terminal-invalid}}]
           (:failed-attempts @saved)))
    (is (false? @review-called?))))

(deftest invisible-candidate-redispatches-scribe-before-observing-review
  (let [saved (atom nil)
        reviewed? (atom false)
        state {:state/type :promotion :stage :independent-review
               :deposit {:depositor "scribe" :job-id "scribe-original"}
               :deposit-job "scribe-original"
               :candidates [{:memory-id "missing"}]
               :job "review-that-must-not-be-observed"
               :attempt 1}
        result
        (sut/drive!
         {:state state
          :candidate-visible-fn (constantly false)
          :deposit-fn (fn [failure]
                        (is (= :promotion-candidates-not-persisted
                               (:error/code failure)))
                        {:ok true :job "scribe-repair"})
          :review-fn (fn [& _] (reset! reviewed? true))
          :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (:ok result))
    (is (= :awaiting-terminal (:status result)))
    (is (= "scribe-repair" (:job-id result)))
    (is (= :deposit (:stage @saved)))
    (is (= "review-that-must-not-be-observed"
           (:abandoned-review-job @saved)))
    (is (= :promotion-candidates-not-persisted
           (get-in @saved [:failed-attempts 0 :failure :error/code])))
    (is (false? @reviewed?))))

(deftest repair-attempt-produces-a-distinct-stable-job-identity
  (let [request {:dispatch/id "dispatch" :agent-id "scribe"
                 :phase :promote-solver}]
    (is (= (submission/canonical-job-id request)
           (submission/canonical-job-id request)))
    (is (not= (submission/canonical-job-id request)
              (submission/canonical-job-id
               (assoc request :submission/attempt 1))))
    (is (= (submission/canonical-job-id
            (assoc request :submission/attempt 1))
           (submission/canonical-job-id
            (assoc request :submission/attempt 1))))))

(deftest repaired-deposit-launches-review-as-append-only-successor
  (let [saved (atom nil)
        launched (atom nil)
        predecessor "review-terminal"
        state {:state/type :promotion :stage :deposit
               :job "scribe-repair"
               :deposit-job "scribe-original"
               :deposit-request {:role :scribe}
               :abandoned-review-job predecessor
               :attempt 2}
        candidate {:name "general pattern" :hook "when formalizing"
                   :body "Use the reviewed library pattern."
                   :kind :pattern/strategy :pattern-ids ["pattern"]
                   :source-attempts [1]}
        result
        (sut/drive!
         {:state state
          :deposit-request {:role :scribe}
          :reviewer-request {:role :promotion-proctor}
          :deposit-fn (fn [job-id]
                        (is (= "scribe-repair" job-id))
                        {:ok true :job job-id
                         :report {:depositor "scribe" :candidates [candidate]
                                  :lanes [{:lane :solve :status :ran}
                                          {:lane :arc :status :ran-empty
                                           :reason "no arc"}
                                          {:lane :trajectory :status :ran-empty
                                           :reason "no trajectory"}
                                          {:lane :challenge :status :ran-empty
                                           :reason "no challenge"}]}})
          :persist-candidates-fn
          (fn [deposit]
            (let [persisted (assoc candidate :memory-id "memory-successor"
                                             :content-digest "digest-successor")]
              {:ok true
               :deposit (assoc deposit :candidates [persisted])
               :candidates [persisted]}))
          :prepare-patterns-fn (fn [_] {:ok true})
          :review-fn (fn
                       ([_] (throw (ex-info "ordinary replay forbidden" {})))
                       ([_ _] (throw (ex-info "observation forbidden" {})))
                       ([candidates prior attempt]
                        (reset! launched {:candidates candidates
                                          :predecessor prior
                                          :attempt attempt})
                        {:ok true :job "review-successor"}))
          :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :awaiting-terminal (:status result)))
    (is (= "review-successor" (:job-id result)))
    (is (= predecessor (:predecessor @launched)))
    (is (= 1 (:attempt @launched)))
    (is (= predecessor (:predecessor-job-id @saved)))
    (is (= 1 (:review-successor-attempt @saved)))
    (is (= :independent-review (:stage @saved)))))

(deftest f28-string-enum-reviews-cannot-silently-publish-empty
  (let [fixture (edn/read-string
                 (slurp "test/fixtures/apm/f28-solver-promotion-string-enums.edn"))
        candidates (:candidates fixture)
        raw-reviews (:reviews fixture)
        raw (pipeline/validate-review* candidates (:depositor fixture)
                                       (:reviewer fixture) raw-reviews)
        reviews (mapv (partial #'sut/normalize-review-entry
                               (:reviewer fixture))
                      raw-reviews)
        validated (pipeline/validate-review* candidates (:depositor fixture)
                                             (:reviewer fixture) reviews)]
    (is (= [:review-verdict-invalid]
           (:findings raw))
        "the F28 wire representation must fail closed before filtering")
    (is (:ok validated))
    (is (= 4 (count (:candidates validated))))
    (is (= :promotion-publication-accounting-invalid
           (:error/code
            (pipeline/validate-publication-accounting reviews [])))
        "four approvals may not certify an empty snapshot")
    (is (:ok (pipeline/validate-publication-accounting
              reviews (:candidates validated))))))

(deftest cannot-judge-is-a-valid-nonpublishing-review
  (let [candidate {:memory-id "candidate" :content-digest "digest"
                   :pattern-ids ["pattern"]}
        review {:memory-id "candidate" :reviewer "proctor"
                :verdict :cannot-judge
                :reason "persisted candidate evidence is unavailable"
                :residual "retain the unresolved candidate for a later review"}
        validated (pipeline/validate-review* [candidate] "zai-scribe"
                                             "proctor" [review])]
    (is (:ok validated))
    (is (empty? (:candidates validated)))
    (is (:ok (pipeline/validate-publication-accounting [review] [])))))

(deftest coined-pattern-is-published-before-independent-review
  (let [visible? (atom false)
        reviewed? (atom false)
        deposit {:depositor "scribe"
                 :new-pattern-rationales {"new-pattern" "No existing fit."}
                 :candidates [{:memory-id "memory" :content-digest "digest"
                               :pattern-ids ["new-pattern"]
                               :source-attempts [1]}]
                 :lanes [{:lane :solve :status :ran}
                         {:lane :arc :status :ran-empty :reason "none"}
                         {:lane :trajectory :status :ran-empty :reason "none"}
                         {:lane :challenge :status :ran-empty :reason "none"}]}
        saved (atom nil)
        published (atom nil)
        result
        (sut/drive!
         {:state {:state/type :promotion :stage :deposit :job "deposit-job"}
          :deposit-fn (fn [_] {:ok true :report deposit})
          :prepare-patterns-fn (fn [observed]
                                 (is (= deposit observed))
                                 (reset! visible? true)
                                 {:ok true})
          :review-fn (fn [_]
                       (is @visible?)
                       (reset! reviewed? true)
                       {:ok true :job "review-job"})
          :persist-fn #(reset! saved %)
          :deposit-request {}})]
    (is @reviewed?)
    (is (= :awaiting-terminal (:status result)))
    (is (= :independent-review (:stage @saved)))
    (let [completed
          (sut/drive!
           {:state @saved
            :review-fn
            (fn [_ _]
              {:ok true :reviewer "proctor"
               :reviews [{:memory-id "memory" :reviewer "proctor"
                           :verdict :approve :reason "coherent and witnessed"
                           :residual "none"
                           :review-evidence-id "review-evidence"
                           :attachment-status :reviewed
                           :pattern-ids ["new-pattern"]}]})
            :publish-fn (fn [value]
                          (reset! published value)
                          {:ok true :receipt {:receipt/id "published"}})
            :persist-fn #(reset! saved %)})]
      (is (= :certified (:status completed)))
      (is (= :approve (get-in @published [:reviews 0 :verdict]))))))

(deftest invalid-deposit-shape-is-bounded
  (let [result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "third-invalid" :attempt 3}
                 :deposit-fn (fn [_] {:ok true :report {:depositor "scribe"}})
                 :persist-fn (fn [_] (throw (ex-info "must not persist" {})))})]
    (is (false? (:ok result)))
    (is (= :promotion-deposit-retries-exhausted (:error/code result)))
    (is (= 3 (:attempts result)))
    (is (= [:candidates-missing :lane-report-invalid] (:findings result)))))

(deftest final-semantic-attempt-gets-one-linter-feedback-repair
  (let [saved (atom nil) feedback (atom nil)
        result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "invalid-edn" :attempt 3}
                 :deposit-fn (fn
                               ([value]
                                (if (string? value)
                                  {:ok false
                                   :error/code :promotion-stage-terminal-invalid
                                   :report/error {:error/code :report-edn-lint-failed
                                                  :error/message "1:9 missing value for key"}}
                                  (do (reset! feedback value)
                                      {:ok true :job "format-repair"})))
                               ([] (throw (ex-info "feedback required" {}))))
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :awaiting-terminal (:status result)))
    (is (= "format-repair" (:job-id result)))
    (is (= :report-edn-lint-failed
           (get-in @feedback [:report/error :error/code])))
    (is (= 3 (:attempt @saved)))
    (is (= 1 (:format-repairs @saved)))))

(deftest final-attempt-gets-one-typed-lane-shape-repair
  (let [saved (atom nil) feedback (atom nil)
        result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "parseable-wrong-shape" :attempt 3
                         :format-repairs 1}
                 :deposit-fn (fn
                               ([value]
                                (if (string? value)
                                  {:ok true :report
                                   {:depositor "scribe"
                                    :candidates [{:memory-id "m" :content-digest "d"
                                                  :pattern-ids ["p"]}]
                                    :lanes [{:lane :solve :ran true}]}}
                                  (do (reset! feedback value)
                                      {:ok true :job "schema-repair"})))
                               ([] (throw (ex-info "feedback required" {}))))
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :awaiting-terminal (:status result)))
    (is (= "schema-repair" (:job-id result)))
    (is (= [:lane-report-invalid] (:findings @feedback)))
    (is (= 3 (:attempt @saved)))
    (is (= 1 (:schema-repairs @saved)))))

(deftest final-attempt-without-authoritative-receipt-gets-one-schema-repair
  (let [saved (atom nil) feedback (atom nil)
        result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "typed-receipt-missing" :attempt 3}
                 :deposit-fn (fn
                               ([value]
                                (if (string? value)
                                  {:ok true :report {}}
                                  (do (reset! feedback value)
                                      {:ok true :job "typed-receipt-repair"})))
                               ([] (throw (ex-info "feedback required" {}))))
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :awaiting-terminal (:status result)))
    (is (= "typed-receipt-repair" (:job-id result)))
    (is (= [:depositor-missing :candidates-missing :lane-report-invalid]
           (:findings @feedback)))
    (is (= 3 (:attempt @saved)))
    (is (= 1 (:schema-repairs @saved)))))

(deftest pinned-proctor-review-shape-normalizes-only-with-exact-digest
  (let [normalize #'sut/normalize-review-report
        reviews [{:memory-id "m" :reviewer "proctor" :verdict :reject
                  :pattern-ids []}]
        accepted (normalize {:candidate-set-digest "digest"
                             :base-problem-blob "blob"
                             :open-residuals []
                             :promotion-reviews reviews}
                            "digest" "blob" "proctor")]
    (is (:ok accepted))
    (is (= "proctor" (:reviewer accepted)))
    (is (= :proposed (get-in accepted [:reviews 0 :attachment-status])))
    (is (= "proctor" (get-in accepted [:reviews 0 :reported-reviewer])))
    (is (= :promotion-review-candidate-digest-mismatch
           (:error/code
            (normalize {:candidate-set-digest "other"
                        :base-problem-blob "blob" :open-residuals []
                        :promotion-reviews reviews}
                       "digest" "blob" "proctor"))))
    (let [reported-other
          (normalize {:candidate-set-digest "digest"
                      :base-problem-blob "blob" :open-residuals []
                      :promotion-reviews
                      [{:memory-id "n" :reviewer "other"
                        :verdict :reject :pattern-ids []}]}
                     "digest" "blob" "proctor")]
      (is (:ok reported-other))
      (is (= "proctor" (get-in reported-other [:reviews 0 :reviewer])))
      (is (= "other"
             (get-in reported-other [:reviews 0 :reported-reviewer]))))))

(deftest reviewer-authority-carries-full-persisted-evidence
  (let [candidate {:memory-id "m" :content-digest "digest"}
        entry {:evidence/id "m"
               :evidence/body {:hook "hook" :body "full persisted body"}}
        request (#'sut/reviewer-authority
                 {:frame-id "f"} [candidate]
                 [{:memory-id "m" :read-ref "http://store/evidence/m"
                   :entry entry}])]
    (is (= entry (get-in request [:candidate-evidence 0 :entry])))
    (is (= "full persisted body"
           (get-in request [:candidate-evidence 0 :entry :evidence/body :body])))
    (is (= "http://store/evidence/m"
           (get-in request [:candidate-evidence 0 :read-ref])))))

(deftest mechanical-rejection-is-recorded-without-llm-self-review
  (let [saved (atom nil) published (atom nil)
        result (sut/drive!
                {:state {:state/type :promotion :stage :deposit
                         :job "unbound-candidates" :attempt 3
                         :format-repairs 1}
                 :deposit-fn (fn
                               ([value]
                                (if (string? value)
                                  {:ok true :report
                                   {:depositor "scribe"
                                    :candidates [{:memory-id "m" :content-digest "d"
                                                  :pattern-ids []
                                                  :source-attempts [1]}]
                                    :lanes [{:lane :solve :status :ran}
                                            {:lane :arc :status :ran}
                                            {:lane :trajectory :status :ran}
                                            {:lane :challenge :status :ran}]}}
                                  (throw (ex-info "no repair expected" {:value value}))))
                               ([] (throw (ex-info "feedback required" {}))))
                 :review-fn (fn [& _]
                              (throw (ex-info "no LLM review expected" {})))
                 :publish-fn #(do (reset! published %) {:ok true :receipt :done})
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :certified (:status result)))
    (is (= [] (:candidates @published)))
    (is (= [:no-parent-pattern]
           (get-in @published [:reviews 0 :finding-codes])))
    (is (= :promotion-certified (:state/type @saved)))))

(deftest review-pending-state-dispatches-the-reviewer-without-a-deposit-job
  ;; A Guide's store-mode candidates enter here: gated already, no Scribe job.
  (let [saved (atom nil) reviewed (atom nil)
        candidates [{:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                     :source-attempts [1]}]
        result (sut/drive!
                {:state {:state/type :promotion :stage :review-pending
                         :deposit {:depositor "f27-guide"}
                         :candidates candidates}
                 :deposit-fn (fn [& _] (throw (ex-info "no deposit job" {})))
                 :review-fn (fn
                              ([cands] (reset! reviewed cands)
                               {:ok true :job "review-1"})
                              ([_ _] (throw (ex-info "not observed yet" {}))))
                 :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :awaiting-terminal (:status result)))
    (is (= "review-1" (:job-id result)))
    (is (= candidates @reviewed))
    (is (= :independent-review (:stage @saved)))
    (is (= "f27-guide" (get-in @saved [:deposit :depositor])))))

(deftest guide-mechanical-rejection-is-published-in-its-review-receipt
  (let [published (atom nil)
        mechanical [{:memory-id "guide-proof"
                     :reviewer "promotion-mechanical-guard"
                     :verdict :reject
                     :reason "mechanical rejection: proof-text-not-memory"
                     :residual "revise"
                     :finding-codes [:proof-text-not-memory]}]
        result (sut/drive!
                {:state {:state/type :promotion :stage :review-pending
                         :deposit {:depositor "f30-guide"}
                         :candidates [] :mechanical-reviews mechanical}
                 :review-fn (fn [& _]
                              (throw (ex-info "mechanical rejection needs no LLM" {})))
                 :publish-fn (fn [value]
                               (reset! published value)
                               {:ok true :receipt {:receipt/id "guide-rejected"}})
                 :persist-fn (fn [_] {:ok true})})]
    (is (= :certified (:status result)))
    (is (= mechanical (:reviews @published)))
    (is (= "promotion-mechanical-guard" (:reviewer @published)))
    (is (empty? (:candidates @published)))))

(deftest independent-review-validates-against-the-gated-candidates
  (let [candidates [{:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                     :source-attempts [1]}]
        review {:memory-id "m" :reviewer "f27-promotion-proctor" :verdict :approve
                :review-evidence-id "ev" :attachment-status :reviewed
                :pattern-ids ["p"] :reason "residual: L1. fact: x"
                :residual "Main.lean:1"}
        published (atom nil)
        result (sut/drive!
                {:state {:state/type :promotion :stage :independent-review
                         :deposit {:depositor "f27-guide"}
                         :candidates candidates :job "review-1"}
                 :review-fn (fn [_ _] {:ok true :reviewer "f27-promotion-proctor"
                                       :reviews [review]})
                 :publish-fn (fn [value] (reset! published value)
                               {:ok true :receipt {:receipt/id "gp"}})
                 :persist-fn (fn [_] {:ok true})})]
    (is (= :certified (:status result)))
    (is (= "f27-guide" (get-in @published [:candidates 0 :depositor])))
    (is (= "f27-promotion-proctor" (:reviewer @published)))))

(deftest independent-review-persists-returned-verdict-before-publication
  (let [candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1]}
        review {:memory-id "m" :reviewer "proctor" :verdict :reassign
                :review-evidence-id "review" :attachment-status :reviewed
                :pattern-ids ["canonical"] :reason "returned reason"
                :residual "returned residual"}
        calls (atom [])
        result
        (sut/drive!
         {:state {:state/type :promotion :stage :independent-review
                  :deposit {:depositor "scribe"} :candidates [candidate]
                  :job "review-job"}
          :review-fn (fn [_ _] {:ok true :reviewer "proctor"
                                :reviews [review]})
          :persist-reviews-fn
          (fn [value]
            (swap! calls conj [:persist-review value])
            {:ok true
             :reviews [(assoc review
                              :review-evidence-id "controller-review"
                              :depositor "scribe"
                              :reviewer "proctor")]})
          :publish-fn
          (fn [value]
            (swap! calls conj [:publish value])
            {:ok true :receipt {:receipt/id "done"}})
          :persist-fn (fn [_] {:ok true})})]
    (is (= :certified (:status result)))
    (is (= [:persist-review :publish] (mapv first @calls)))
    (is (= :reassign
           (get-in (second @calls) [1 :reviews 0 :verdict])))
    (is (= "controller-review"
           (get-in (second @calls)
                   [1 :candidates 0 :review-evidence-id])))))

(deftest generated-completed-pass-policy-holds-before-publication
  (let [materialization
        (fn [id digest]
          {:artifact-id id :content-digest digest
           :persisted-content-digest digest :read-back-content-digest digest
           :persistence-receipt-id id})
        candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1]
                   :materialization (materialization "m" "d")}
        review {:memory-id "m" :reviewer "proctor" :verdict :cannot-judge
                :reason "apparatus unavailable" :residual "repair apparatus"
                :pattern-ids ["p"]}
        saved (atom nil)
        published? (atom false)
        result
        (sut/drive!
         {:state {:state/type :promotion :stage :independent-review
                  :deposit {:depositor "scribe" :candidates [candidate]}
                  :candidates [candidate] :job "review-job"}
          :promotion-policy {:completed-pass-required true}
          :review-fn (fn [_ _] {:ok true :reviewer "proctor"
                                :reviews [review]})
          :persist-reviews-fn
          (fn [_]
            {:ok true
             :reviews [(assoc review
                              :review-evidence-id "review"
                              :attachment-status :proposed
                              :review-materialization
                              (materialization "review" "rd"))]})
          :publish-fn (fn [_] (reset! published? true))
          :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (:ok result))
    (is (= :awaiting-apparatus-repair (:status result)))
    (is (= :awaiting-apparatus-repair (:stage @saved)))
    (is (false? @published?))))

(deftest projection-failure-holds-with-persisted-judgement-and-never-publishes
  (let [materialization
        (fn [id digest]
          {:artifact-id id :content-digest digest
           :persisted-content-digest digest :read-back-content-digest digest
           :persistence-receipt-id id})
        candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1]
                   :materialization (materialization "m" "d")}
        returned {:memory-id "m" :reviewer "proctor" :verdict :approve
                  :review-evidence-id "reported-review"
                  :attachment-status :reviewed :pattern-ids ["p"]
                  :reason "appears coherent" :residual "projection failed"}
        persisted (assoc returned
                         :review-evidence-id "review"
                         :attachment-status :proposed
                         :projection/valid? false
                         :projection/finding {:failure :edge-write-failed}
                         :review-materialization
                         (materialization "review" "rd"))
        publication (atom nil)
        result
        (sut/drive!
         {:state {:state/type :promotion :stage :independent-review
                  :deposit {:depositor "scribe" :candidates [candidate]}
                  :candidates [candidate] :job "review-job"}
          :promotion-policy {:completed-pass-required true
                             :projection-repair-max-attempts 2}
          :contract-digest "contract-v1"
          :review-fn (fn [_ _] {:ok true :reviewer "proctor"
                                :reviews [returned]})
          :persist-reviews-fn
          (fn [_] {:ok false
                   :error/code :promotion-review-projection-failed
                   :review-job "review-job"
                   :reviews [persisted]
                   :persisted [{:review-evidence-id "review"}]
                   :findings [{:memory-id "m"
                               :failure :promotion-review-projection-failed}]})
          :publish-fn (fn [value]
                        (reset! publication value)
                        {:ok true :receipt {:receipt/id "done"}})
          :persist-fn (fn [_] {:ok true})})]
    (is (:ok result))
    (is (= :awaiting-apparatus-repair (:status result)))
    (is (nil? @publication))
    (is (= [persisted]
           (get-in result [:state :persisted-review-result :reviews])))
    (is (= [returned]
           (get-in result
                   [:state :persisted-review-result :returned-reviews])))
    (is (= :review-projection (get-in result [:state :repair/kind])))
    (is (= 2 (get-in result [:state :repair/max-attempts])))))

(deftest same-contract-projection-repair-reuses-terminal-review-job
  (let [candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1]
                   :materialization (materialization "m" "d")}
        review {:memory-id "m" :reviewer "proctor" :verdict :reject
                :review-evidence-id "review" :attachment-status :proposed
                :pattern-ids ["p"] :reason "merit rejection" :residual "none"
                :review-materialization (materialization "review" "rd")}
        last-valid {:state/type :promotion :stage :independent-review
                    :deposit {:depositor "scribe" :candidates [candidate]}
                    :candidates [candidate] :job "terminal-review"}
        calls (atom [])
        result
        (sut/drive!
         {:state {:state/type :promotion :stage :awaiting-apparatus-repair
                  :contract-digest "contract-v1" :last-valid-state last-valid
                  :repair/kind :review-projection :repair/attempts 0
                  :repair/max-attempts 1}
          :promotion-policy {:completed-pass-required true}
          :contract-digest "contract-v1"
          :review-fn (fn [job _]
                       (swap! calls conj job)
                       {:ok true :reviewer "proctor" :reviews [review]})
          :persist-reviews-fn (fn [_] {:ok true :reviews [review]})
          :publish-fn (fn [_] {:ok true :receipt {:receipt/id "done"}})
          :persist-fn (fn [_] {:ok true})})]
    (is (= :certified (:status result)))
    (is (= ["terminal-review"] @calls))))

(deftest exhausted-projection-repair-does-not-advance-promotion
  (let [state {:state/type :promotion :stage :awaiting-apparatus-repair
               :contract-digest "contract-v1"
               :last-valid-state {:stage :independent-review}
               :repair/kind :review-projection :repair/attempts 1
               :repair/max-attempts 1 :findings [{:failure :edge-write}]}
        result (sut/drive! {:state state :contract-digest "contract-v1"})]
    (is (false? (:ok result)))
    (is (= :promotion-apparatus-repair-exhausted (:error/code result)))
    (is (= state (:state result)))))

(deftest apparatus-hold-revalidates-persisted-review-after-contract-change
  (let [materialization
        (fn [id digest]
          {:artifact-id id :content-digest digest
           :persisted-content-digest digest :read-back-content-digest digest
           :persistence-receipt-id id})
        candidate {:memory-id "m" :content-digest "d" :pattern-ids ["p"]
                   :source-attempts [1]
                   :materialization (materialization "m" "d")}
        review {:memory-id "m" :reviewer "proctor" :verdict :reject
                :review-evidence-id "review" :attachment-status :proposed
                :pattern-ids ["p"] :reason "merit rejection" :residual "none"
                :review-materialization (materialization "review" "rd")}
        last-valid {:state/type :promotion :stage :independent-review
                    :deposit {:depositor "scribe" :candidates [candidate]}
                    :candidates [candidate] :job "completed-review"}
        result
        (sut/drive!
         {:state {:state/type :promotion :stage :awaiting-apparatus-repair
                  :contract-digest "contract-v1" :last-valid-state last-valid}
          :promotion-policy {:completed-pass-required true}
          :contract-digest "contract-v2"
          :review-fn (fn [job _]
                       (is (= "completed-review" job))
                       {:ok true :reviewer "proctor" :reviews [review]})
          :persist-reviews-fn (fn [_] {:ok true :reviews [review]})
          :publish-fn (fn [_] {:ok true :receipt {:receipt/id "done"}})
          :persist-fn (fn [_] {:ok true})})]
    (is (= :certified (:status result)))))

(deftest unresolved-review-contract-change-dispatches-append-only-successor
  (let [last-valid {:state/type :promotion :stage :independent-review
                    :deposit {:depositor "scribe"}
                    :candidates [{:memory-id "m"}]
                    :job "prior-terminal-review"}
        saved (atom nil)
        result
        (sut/drive!
         {:state {:state/type :promotion :stage :awaiting-apparatus-repair
                  :repair/kind :unresolved-review
                  :contract-digest "contract-v1"
                  :last-valid-state last-valid}
          :contract-digest "contract-v2"
          :review-fn
          (fn [candidates predecessor attempt]
            (is (= [{:memory-id "m"}] candidates))
            (is (= "prior-terminal-review" predecessor))
            (is (= 1 attempt))
            {:ok true :job "successor-review"})
          :persist-fn #(do (reset! saved %) {:ok true})})]
    (is (= :awaiting-terminal (:status result)))
    (is (= "successor-review" (:job-id result)))
    (is (= "prior-terminal-review" (:predecessor-job-id @saved)))
    (is (= 1 (:review-successor-attempt @saved)))))
