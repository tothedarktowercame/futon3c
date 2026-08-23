(ns futon3c.apm.live-learning-phases-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-learning-phases :as sut]
            [futon3c.apm.live-preflight-runtime :as runtime]))

(deftest terminal-report-parser-accepts-one-prose-wrapped-edn-map-only
  (is (= {:command-own-exit 0 :frame-id "f21"}
         (runtime/parse-report
          "Bell sent. Final receipt:\n```clojure\n{:command-own-exit 0 :frame-id \"f21\"}\n```")))
  (is (nil? (runtime/parse-report
             "```edn\n{:a 1}\n``` and ```edn\n{:b 2}\n```"))))

(def contract (edn/read-string
               (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v1.edn")))
(def unit (second (:units (edn/read-string
                          (slurp "holes/labs/M-apm-demonstration/countdown-10-manifest-v2.edn")))))
(def preflight-receipt
  (let [body {:receipt/type :frame-preflight :receipt/frame-id "f19"
              :receipt/problem-id "a01J05" :receipt/result :preflight-passed}]
    (assoc body :receipt/id (machine/ledger-digest [body]))))
(def base
  {:contract contract :ledger {:digest (apply str (repeat 64 "a"))} :unit unit
   :role-card {:path "card.md" :blob "card-blob"}
   :workspace {:workspace/path "/tmp/student"}
   :receipts {:preflight preflight-receipt}})

(deftest student-request-is-fresh-and-bound-to-preflight
  (let [result (sut/build-request
                (merge base {:action {:kind :student-attempt
                                      :phase :student-attempt-1 :role :student
                                      :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
                             :seat {:agent-id "f19-student" :invoke-ready? true}}))
        request (:request result)]
    (is (:ok result))
    (is (:fresh-session? request))
    (is (string? (:fresh-session-nonce request)))
    (is (= #{(:receipt/id preflight-receipt)}
           (:input-receipt-ids request)))))

(deftest student-receipt-carries-agency-session-and-memory-use
  (let [action {:kind :student-attempt :phase :student-attempt-1 :role :student
                :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
        request (:request (sut/build-request
                           (merge base {:action action
                                        :seat {:agent-id "f19-student"
                                               :invoke-ready? true}})))
        job {:job-id "j1" :agent-id "f19-student" :session-id "fresh-s1"
             :state :done
             :report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                      :outcome :stuck :failure-account {:reason :gap}
                      :memory-use {:queries [] :surfaced-ids [] :used-ids []}}}
        ticket {:job-id "j1"}
        validated (sut/validate-terminal request ticket job)
        result (sut/receipt contract action (:receipts base) request ticket job validated)]
    (is (:ok validated))
    (is (:ok result))
    (is (= "fresh-s1" (get-in result [:certificate :receipt/fresh-session-id])))))

(deftest typed-terminal-repair-preserves-authority-and-carries-findings
  (let [repair (sut/terminal-repair-request
                {:dispatch/id "original" :frame-id "f25"
                 :problem-id "m94A02" :fresh-session? true
                 :memory-snapshot {:snapshot-digest "snapshot"}}
                {:ticket/id "ticket-1"}
                {:job-id "job-1"}
                {:findings [:command-own-exit-nonzero :frame-mismatch]})
        request (:request repair)]
    (is (:ok repair))
    (is (false? (:fresh-session? request)))
    (is (= "snapshot" (get-in request [:memory-snapshot :snapshot-digest])))
    (is (= [:command-own-exit-nonzero :frame-mismatch]
           (:repair/findings request)))
    (is (not= "original" (:dispatch/id request)))))

(deftest guide-must-prove-channel-isolation
  (let [request {:dispatch/type :guide-intervention :agent-id "f19-guide"
                 :frame-id "f19" :problem-id "a01J05"}
        job {:job-id "j" :agent-id "f19-guide" :state :done
             :report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                      :channel-audit {:direct-student-contact? true}}}]
    (is (some #{:guide-channel-isolation-unproved}
              (:findings (sut/validate-terminal request {:job-id "j"} job))))))

(deftest scribe-and-close-require-typed-reduction-evidence
  (doseq [[kind report finding]
          [[:scribe-reduce {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"}
            :scribe-reduction-evidence-missing]
           [:close-frame {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                          :result :closed}
            :close-evidence-invalid]]]
    (let [request {:dispatch/type kind :agent-id (if (= kind :scribe-reduce)
                                                  "f19-scribe" "f19-guide")
                   :frame-id "f19" :problem-id "a01J05"}
          job {:job-id "j" :agent-id (:agent-id request) :state :done :report report}]
      (is (some #{finding}
                (:findings (sut/validate-terminal request {:job-id "j"} job)))))))

(deftest promoted-solver-snapshot-is-bound-into-student-request-and-use
  (let [promotion {:receipt/id "promotion-receipt"
                   :receipt/snapshot-id "snapshot-1"
                   :receipt/snapshot-digest "snapshot-digest"}
        request (:request
                 (sut/build-request
                  (merge base
                         {:receipts {:preflight preflight-receipt
                                     :promote-solver promotion}
                          :snapshot-access
                          {:ok true
                           :snapshot {:snapshot/digest "snapshot-digest"}
                           :accessible-memory-ids #{"m1"}}
                          :action {:kind :student-attempt
                                   :phase :student-attempt-1 :role :student
                                   :frame-id "f19" :problem-id "a01J05"}
                          :seat {:agent-id "f19-student" :invoke-ready? true}})))
        job {:job-id "j" :agent-id "f19-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                      :outcome :stuck :failure-account {}
                      :memory-use {:receipt-id "wrong"
                                   :snapshot-id "snapshot-1"
                                   :snapshot-digest "snapshot-digest"
                                   :queries [] :surfaced-ids [] :used-ids []}}}]
    (is (= {:receipt-id "promotion-receipt"
            :snapshot-id "snapshot-1" :snapshot-digest "snapshot-digest"
            :accessible-memory-ids ["m1"]}
           (:memory-snapshot request)))
    (is (= 1 (:attempt-ordinal request)))
    (is (some #{:student-memory-snapshot-mismatch}
              (:findings (sut/validate-terminal request {:job-id "j"} job))))))

(deftest student-cannot-report-global-store-memory-outside-frozen-snapshot
  (let [request {:dispatch/type :student-attempt
                 :agent-id "f22-student" :frame-id "f22"
                 :problem-id "a98J02"
                 :memory-snapshot {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :accessible-memory-ids []}}
        job {:job-id "j" :agent-id "f22-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f22"
                      :problem-id "a98J02"
                      :memory-use {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :queries ["Banach fixed point"]
                                   :surfaced-ids ["e-global"]
                                   :used-ids ["e-global"]}}}
        result (sut/validate-terminal request {:job-id "j"} job)]
    (is (some #{:student-memory-surfaced-outside-snapshot}
              (:findings result)))))

(deftest student-prompt-names-frame-and-exact-memory-evidence-shape
  (let [text (sut/prompt {:dispatch/type :student-attempt
                          :phase :student-attempt-1
                          :frame-id "f22" :role-card-path "student.md"
                          :role-card-blob "blob"
                          :memory-snapshot {:accessible-memory-ids []}})]
    (is (.startsWith text "F22 student-attempt-1"))
    (is (re-find #":surfaced-ids and :used-ids" text))
    (is (re-find #"do not query, read, or use" text))))

(deftest student-report-must-record-exact-search-queries
  (let [request {:dispatch/type :student-attempt :agent-id "f22-student"
                 :frame-id "f22" :problem-id "p22"}
        job {:job-id "j" :agent-id "f22-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f22" :problem-id "p22"
                      :memory-use {:surfaced-ids [] :used-ids []}}}]
    (is (some #{:student-memory-use-ids-invalid}
              (:findings (sut/validate-terminal request {:job-id "j"} job))))))

(deftest promotion-deposit-is-bound-to-base-residual-and-solver-head
  (let [action {:kind :scribe-reduce :phase :promote-solver :role :scribe
                :frame-id "f19" :problem-id "a01J05"}
        solve {:receipt/id "solve" :receipt/final-head
               "1111111111111111111111111111111111111111"}
        verify {:receipt/id "verify"}
        v2-contract (edn/read-string
                     (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn"))
        result (sut/build-request
                (merge base {:contract v2-contract
                             :action action
                             :receipts {:solve solve :verify verify}
                             :seat {:agent-id "f19-scribe" :invoke-ready? true}}))
        request (:request result)]
    (is (:ok result))
    (is (= (get-in unit [:problem :blob]) (:base-problem-blob request)))
    (is (= (get-in unit [:problem :path]) (:problem-path request)))
    (is (= (:receipt/final-head solve) (:solver-final-head request)))))

(deftest student-dispatch-fails-closed-without-substrate-snapshot-proof
  (let [promotion {:receipt/id "promotion-receipt"
                   :receipt/snapshot-id "snapshot-1"
                   :receipt/snapshot-digest "snapshot-digest"}
        result (sut/build-request
                (merge base
                       {:receipts {:preflight preflight-receipt
                                   :promote-solver promotion}
                        :action {:kind :student-attempt
                                 :phase :student-attempt-1 :role :student
                                 :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
                        :seat {:agent-id "f19-student" :invoke-ready? true}}))]
    (is (= :live-learning-request-invalid (:error/code result)))
    (is (some #{:student-snapshot-access-unverified} (:findings result)))))

(deftest role-terminal-budgets-are-configurable-and-validated
  (let [action {:kind :guide-intervention :phase :guide-intervention-1
                :role :guide :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
        attempt {:receipt/id "attempt"}
        good (sut/build-request
              (merge base {:action action
                           :receipts {:student-attempt-1 attempt}
                           :seat {:agent-id "f19-guide" :invoke-ready? true}
                           :terminal-budgets {:guide {:collection-attempts 1
                                                      :repair-attempts 2}}}))
        bad (sut/build-request
             (merge base {:action action
                          :receipts {:student-attempt-1 attempt}
                          :seat {:agent-id "f19-guide" :invoke-ready? true}
                          :terminal-budgets {:guide {:collection-attempts 0
                                                     :repair-attempts 1}}}))]
    (is (= {:collection-attempts 1 :repair-attempts 2}
           (get-in good [:request :terminal-budget])))
    (is (some #{:terminal-budget-invalid} (:findings bad)))))

(deftest guide-request-derives-ordinal-from-registered-contract
  (let [action {:kind :guide-intervention :phase :guide-intervention-1
                :role :guide :frame-id "f19" :problem-id "a01J05"}
        attempt {:receipt/id "attempt"}
        result (sut/build-request
                (merge base {:action action
                             :receipts {:student-attempt-1 attempt}
                             :seat {:agent-id "f19-guide"
                                    :invoke-ready? true}}))]
    (is (:ok result))
    (is (= 1 (get-in result [:request :intervention-ordinal])))
    (is (= "attempt" (get-in result [:request :input-attempt-id])))))

(deftest promote-solver-requires-reviewed-candidates-for-controller-publication
  (let [request {:dispatch/type :scribe-reduce :phase :promote-solver
                 :agent-id "f19-scribe" :frame-id "f19" :problem-id "a01J05"}
        report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                :lanes [] :dispositions [] :promotion-reviews []
                :memory-candidates []}
        job {:job-id "j" :agent-id "f19-scribe" :state :done :report report}]
    (is (some #{:solver-promotion-candidates-invalid}
              (:findings (sut/validate-terminal request {:job-id "j"} job))))))

(deftest controller-missing-observation-is-content-addressed-and-not-student-authored
  (let [contract {:phase-order [:student-attempt-1]
                  :phases {:student-attempt-1
                           {:kind :student-attempt :role :student :ordinal 1
                            :receipt/type :student-attempt
                            :alternate-receipt/types #{:student-observation-missing}
                            :requires #{} :produces #{:attempt :memory-use}}}
                  :receipt/schemas
                  {:student-observation-missing
                   {:required #{:receipt/id :receipt/type :receipt/frame-id
                                :receipt/problem-id :receipt/attempt-ordinal
                                :receipt/job-id :receipt/author :receipt/reason
                                :receipt/repair-attempts :receipt/memory-snapshot
                                :receipt/harness-observed}}}}
        action {:kind :student-attempt :role :student :phase :student-attempt-1
                :frame-id "fixture-f25" :problem-id "m94A02"}
        result (sut/missing-observation-receipt
                contract action {}
                {:frame-id "fixture-f25" :problem-id "m94A02"
                 :attempt-ordinal 1 :workspace "/does/not/exist"
                 :memory-snapshot {:receipt-id "promotion-receipt"
                                   :snapshot-id "frozen-snapshot"
                                   :snapshot-digest "frozen-digest"
                                   :accessible-memory-ids ["memory-1"]}}
                {:job-id "f25-shaped-job"}
                {:job-id "f25-shaped-job" :agent-id "fixture-f25-student"
                 :state :done :terminal-code 0}
                1 {:collection/id "collection-evidence"})]
    (is (:ok result))
    (is (= :controller (get-in result [:certificate :receipt/author])))
    (is (= :typed-submission-missing
           (get-in result [:certificate :receipt/reason])))
    (is (= {:receipt-id "promotion-receipt"
            :snapshot-id "frozen-snapshot"
            :snapshot-digest "frozen-digest"}
           (get-in result [:certificate :receipt/memory-snapshot])))
    (is (nil? (get-in result [:certificate :receipt/fresh-session-id])))))
