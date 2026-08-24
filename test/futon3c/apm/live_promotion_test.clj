(ns futon3c.apm.live-promotion-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-promotion :as sut]
            [futon3c.apm.promotion-pipeline :as pipeline]
            [futon3c.apm.typed-role-submission :as submission]))

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

(deftest f28-string-enum-reviews-cannot-silently-publish-empty
  (let [fixture (edn/read-string
                 (slurp "test/fixtures/apm/f28-solver-promotion-string-enums.edn"))
        candidates (:candidates fixture)
        raw-reviews (:reviews fixture)
        raw (pipeline/validate-review* candidates (:depositor fixture)
                                       (:reviewer fixture) raw-reviews)
        reviews (mapv #'sut/normalize-review-entry raw-reviews)
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
                            "digest" "blob")]
    (is (:ok accepted))
    (is (= "proctor" (:reviewer accepted)))
    (is (= reviews (:reviews accepted)))
    (is (= :promotion-review-candidate-digest-mismatch
           (:error/code
            (normalize {:candidate-set-digest "other"
                        :base-problem-blob "blob" :open-residuals []
                        :promotion-reviews reviews}
                       "digest" "blob"))))
    (is (= :promotion-review-attribution-ambiguous
           (:error/code
            (normalize {:candidate-set-digest "digest"
                        :base-problem-blob "blob" :open-residuals []
                        :promotion-reviews
                        (conj reviews {:memory-id "n" :reviewer "other"
                                       :verdict :reject :pattern-ids []})}
                       "digest" "blob"))))))

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
