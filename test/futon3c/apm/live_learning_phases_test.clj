(ns futon3c.apm.live-learning-phases-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.live-learning-phases :as sut]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.role-memory-search :as role-memory]))

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
        request (assoc request :memory-snapshot
                       {:receipt-id "promotion" :snapshot-id "snapshot"
                        :snapshot-digest "digest"
                        :accessible-memory-ids []})
        job {:job-id "j1" :agent-id "f19-student" :session-id "fresh-s1"
             :state :done
             :report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                      :outcome :stuck :failure-account {:reason :gap}
                      :memory-use {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :queries [] :surfaced-ids [] :used-ids []}}}
        ticket {:job-id "j1"}
        validated (sut/validate-terminal request ticket job)
        result (sut/receipt contract action (:receipts base) request ticket job validated)]
    (is (:ok validated))
    (is (:ok result))
    (is (= {:receipt-id "promotion" :snapshot-id "snapshot"
            :snapshot-digest "digest"}
           (get-in result [:certificate :receipt/memory-snapshot])))
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

(deftest close-result-is-canonicalized-at-the-json-wire-boundary
  (let [request {:dispatch/type :close-frame :agent-id "f25-guide"
                 :frame-id "f25" :problem-id "m94A02"}
        job (fn [result]
              {:job-id "j" :agent-id "f25-guide" :state :done
               :report {:command-own-exit 0 :frame-id "f25"
                        :problem-id "m94A02" :trace-id "trace"
                        :result result}})]
    (is (:ok (sut/validate-terminal request {:job-id "j"} (job "closed"))))
    (is (:ok (sut/validate-terminal request {:job-id "j"} (job :closed))))
    (is (:ok (sut/validate-terminal request {:job-id "j"} (job "partial"))))
    (is (some #{:close-evidence-invalid}
              (:findings (sut/validate-terminal request {:job-id "j"}
                                                       (job "void")))))))

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

(deftest student-may-use-memory-covered-by-validated-open-search-receipt
  (let [request {:dispatch/type :student-attempt
                 :agent-id "f30-student" :frame-id "f30"
                 :problem-id "a01J06"
                 :memory-snapshot {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :accessible-memory-ids []}}
        job {:job-id "repair" :agent-id "f30-student" :session-id "fresh"
             :state :done
             :typed-submission {:authority {:job-id "repair"}}
             :report {:command-own-exit 0 :frame-id "f30"
                      :problem-id "a01J06"
                      :memory-search-receipt-ids ["receipt"]
                      :memory-use {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :queries ["Jensen formula"]
                                   :surfaced-ids ["e-open"]
                                   :used-ids ["e-open"]}}}]
    (with-redefs [role-memory/validate-claims
                  (fn [_ receipt-ids]
                    (is (= ["receipt"] receipt-ids))
                    {:ok true :receipts [{:result-ids ["e-open"]}]})
                  role-memory/recorded-surfaced-ids-for-job (constantly #{})]
      (is (:ok (sut/validate-terminal request {:job-id "repair"} job))))))

(deftest terminal-repair-inherits-recorded-search-results-from-explicit-predecessor
  (let [request {:dispatch/type :student-attempt
                 :repair/of-job-id "original"
                 :agent-id "f30-student" :frame-id "f30"
                 :problem-id "a01J06"
                 :memory-snapshot {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :accessible-memory-ids []}}
        job {:job-id "repair" :agent-id "f30-student" :session-id "fresh"
             :state :done
             :typed-submission {:authority {:job-id "repair"}}
             :report {:command-own-exit 0 :frame-id "f30"
                      :problem-id "a01J06"
                      :memory-search-receipt-ids []
                      :memory-use {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :queries ["Jensen formula"]
                                   :surfaced-ids ["e-prior"]
                                   :used-ids ["e-prior"]}}}]
    (with-redefs [role-memory/validate-claims
                  (fn [_ _] {:ok true :receipts []})
                  role-memory/recorded-surfaced-ids-for-job
                  (fn [job-id]
                    (is (= "original" job-id))
                    #{"e-prior"})]
      (is (:ok (sut/validate-terminal request {:job-id "repair"} job))))))

(deftest f30-json-query-ledger-beside-memory-use-is-consumed-losslessly
  (let [request {:dispatch/type :student-attempt
                 :agent-id "f30-student" :frame-id "f30"
                 :problem-id "a01J06"
                 :memory-snapshot {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :accessible-memory-ids ["e-reviewed"]}}
        job {:job-id "repair" :agent-id "f30-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f30"
                      :problem-id "a01J06"
                      :queries ["dyadic shell summability"]
                      :memory-use {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :surfaced-ids ["e-reviewed"]
                                   :used-ids ["e-reviewed"]}}}]
    (with-redefs [role-memory/recorded-surfaced-ids-for-job (constantly #{})]
      (is (:ok (sut/validate-terminal request {:job-id "repair"} job))))))

(deftest student-prompt-names-frame-and-exact-memory-evidence-shape
  (let [text (sut/prompt {:dispatch/type :student-attempt
                          :phase :student-attempt-1
                          :frame-id "f22" :role-card-path "student.md"
                          :role-card-blob "blob"
                          :memory-snapshot {:accessible-memory-ids []}})]
    (is (.startsWith text "F22 student-attempt-1"))
    (is (re-find #":surfaced-ids and :used-ids" text))
    (is (re-find #"controller-owned open mathematics search" text))
    (is (re-find #":memory-search-receipt-ids" text))))

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

(deftest student-request-carries-the-workspace-base-for-reset-and-archive
  (let [base-revision (apply str (repeat 40 "b"))
        result (sut/build-request
                (merge base {:action {:kind :student-attempt
                                      :phase :student-attempt-2 :role :student
                                      :ordinal 2 :frame-id "f19" :problem-id "a01J05"}
                             :seat {:agent-id "f19-student" :invoke-ready? true}
                             :workspace {:workspace/path "/tmp/student"
                                         :base-revision base-revision
                                         :problem/path "problems/a01J05/lean/Main.lean"}
                             :receipts {:preflight preflight-receipt
                                        :guide-intervention-1
                                        (let [body {:receipt/type :guide-intervention
                                                    :receipt/frame-id "f19"
                                                    :receipt/problem-id "a01J05"}]
                                          (assoc body :receipt/id
                                                 (machine/ledger-digest [body])))}}))
        request (:request result)]
    (is (:ok result))
    (is (= base-revision (:base-revision request)))
    (is (= "problems/a01J05/lean/Main.lean" (:problem-path request)))))

(deftest fresh-student-attempt-resets-the-worktree-but-repairs-do-not
  ;; f27: attempts 2 and 3 opened attempt 1's finished proof and verified it.
  (let [calls (atom [])
        reset-fn (fn [lease] (swap! calls conj lease) {:ok true :head "h"})
        request {:dispatch/type :student-attempt :fresh-session? true
                 :workspace "/tmp/student" :base-revision "b"
                 :problem-path "problems/p/lean/Main.lean"}]
    (is (= :reset (:status (sut/prepare-student-workspace! request reset-fn))))
    (is (= [{:workspace/path "/tmp/student" :base-revision "b"
             :problem/path "problems/p/lean/Main.lean"}]
           @calls))
    (is (= :not-applicable
           (:status (sut/prepare-student-workspace!
                     (assoc request :repair/attempt 1) reset-fn))))
    (is (= :not-applicable
           (:status (sut/prepare-student-workspace!
                     (assoc request :dispatch/type :guide-intervention) reset-fn))))
    (is (= :student-workspace-base-unknown
           (:error/code (sut/prepare-student-workspace!
                         (dissoc request :base-revision) reset-fn))))
    (is (= :student-workspace-reset-failed
           (:error/code (sut/prepare-student-workspace!
                         request (fn [_] {:ok false :error/code :workspace-reset-git-failed})))))
    (is (= 1 (count @calls)) "only the original fresh dispatch touches the worktree")))

(deftest student-receipt-names-the-archived-source-blob
  (let [action {:kind :student-attempt :phase :student-attempt-1 :role :student
                :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
        request (assoc (:request (sut/build-request
                                  (merge base {:action action
                                               :seat {:agent-id "f19-student"
                                                      :invoke-ready? true}})))
                       :memory-snapshot {:receipt-id "promotion" :snapshot-id "snapshot"
                                         :snapshot-digest "digest"
                                         :accessible-memory-ids []}
                       :problem-path "problems/a01J05/lean/Main.lean")
        job {:job-id "j1" :agent-id "f19-student" :session-id "fresh-s1"
             :state :done
             :report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                      :outcome :success :failure-account []
                      :memory-use {:receipt-id "promotion" :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :queries [] :surfaced-ids [] :used-ids []}}}
        validated (sut/validate-terminal request {:job-id "j1"} job)
        archived (sut/archive-student-source!
                  request "/tmp/campaign/live/student-attempt-1.edn"
                  (fn [lease] {:ok true :source (assoc lease :blob "abc" :head "h")}))
        result (sut/receipt contract action (:receipts base) request {:job-id "j1"} job
                            (assoc validated :source (:source archived)))]
    (is (:ok validated))
    (is (= "/tmp/campaign/live/student-attempt-1-source"
           (get-in archived [:source :archive-directory])))
    (is (:ok result))
    (is (= "abc" (get-in result [:certificate :receipt/source :blob])))
    (is (= :student-source-unknown
           (:error/code (sut/archive-student-source!
                         (dissoc request :problem-path) "/tmp/x.edn" (fn [_] nil)))))))

(def guide-candidate
  {:memory-id "e-guide-1" :content-digest "d1" :pattern-ids ["math-informal/x"]
   :source-attempts [1]})

(deftest guide-candidates-must-be-gate-shaped-and-store-mode
  (let [request {:dispatch/type :guide-intervention :agent-id "f27-guide"
                 :frame-id "f27" :problem-id "m94A03"}
        report {:command-own-exit 0 :frame-id "f27" :problem-id "m94A03"
                :mode "store-mode" :channel-audit {:direct-student-contact? false}}
        findings (fn [r] (:findings (sut/validate-terminal
                                     request {:job-id "j"}
                                     {:job-id "j" :agent-id "f27-guide" :state :done
                                      :report r})))]
    (is (nil? (findings (assoc report :candidates [guide-candidate]))))
    (is (nil? (findings report)) "no candidates is a valid store-mode turn")
    (is (some #{:guide-candidates-invalid}
              (findings (assoc report :candidates
                               [(assoc guide-candidate :pattern-ids [])]))))
    (is (some #{:guide-candidates-outside-store-mode}
              (findings (assoc report :mode "harness-mode"
                               :candidates [guide-candidate]))))))

(deftest guide-receipt-carries-the-union-snapshot-when-one-was-published
  (let [action {:kind :guide-intervention :phase :guide-intervention-1 :role :guide
                :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
        request {:dispatch/type :guide-intervention :agent-id "f19-guide"
                 :frame-id "f19" :problem-id "a01J05"}
        report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                :mode "store-mode" :effect {:channel "memory-store"}
                :channel-audit {:direct-student-contact? false}}
        job {:job-id "j" :agent-id "f19-guide" :state :done :report report}
        attempt-1 (let [body {:receipt/type :student-attempt :receipt/frame-id "f19"
                              :receipt/problem-id "a01J05" :receipt/attempt-ordinal 1
                              :receipt/fresh-session-id "fresh-1"
                              :receipt/job-id "student-1" :receipt/outcome :stuck
                              :receipt/failure-account []
                              :receipt/memory-use {:surfaced-ids []}}]
                    (assoc body :receipt/id (machine/ledger-digest [body])))
        receipts (assoc (:receipts base) :student-attempt-1 attempt-1)
        bare (sut/receipt contract action receipts request {:job-id "j"} job
                          {:ok true :report report})
        union (sut/receipt contract action receipts request {:job-id "j"} job
                           {:ok true :report
                            (assoc report :memory-snapshot
                                   {:snapshot-id "s" :snapshot-digest "s"
                                    :snapshot-path "/tmp/s.edn"
                                    :reviewed-memory-ids ["e-guide-1"]
                                    :promotion-reviews [{:memory-id "e-guide-1"}]
                                    :independent-review? true})})]
    (is (:ok bare))
    (is (not (contains? (:certificate bare) :receipt/snapshot-digest)))
    (is (:ok union))
    (is (= "s" (get-in union [:certificate :receipt/snapshot-digest])))
    (is (= ["e-guide-1"] (get-in union [:certificate :receipt/reviewed-memory-ids])))))

(deftest guide-promotion-seeds-review-then-certifies-from-the-published-union
  (let [dir (java.nio.file.Files/createTempDirectory
             "guide-promotion-" (make-array java.nio.file.attribute.FileAttribute 0))
        state-path (.resolve dir "guide-intervention-1-review.edn")
        request {:dispatch/type :guide-intervention :agent-id "f27-guide"
                 :dispatch/id "d" :prior-snapshot {:snapshot-digest "solver"}}
        report {:candidates [guide-candidate]}
        runs (atom 0)
        run-fn (fn []
                 (swap! runs inc)
                 (let [state (runtime/read-state state-path)]
                   (case (:stage state)
                     :review-pending
                     (do (runtime/atomic-persist!
                          state-path (assoc state :stage :independent-review
                                            :job "review-1"))
                         {:ok true :status :awaiting-terminal :job-id "review-1"})
                     :independent-review
                     (do (runtime/atomic-persist!
                          state-path {:state/type :promotion-certified
                                      :receipt {:receipt/snapshot-id "u"
                                                :receipt/snapshot-digest "u"
                                                :receipt/snapshot-path "/tmp/u.edn"
                                                :receipt/reviewed-memory-ids ["e-guide-1"]
                                                :receipt/promotion-reviews []}})
                         {:ok true :status :certified}))))
        driver {:state-path state-path :run-fn run-fn}
        first-step (sut/guide-promotion-step! driver request report)
        seeded (runtime/read-state state-path)
        second-step (sut/guide-promotion-step! driver request report)]
    (is (= :awaiting-terminal (:status first-step)))
    (is (= "review-1" (:job-id first-step)))
    (is (= [guide-candidate] (:candidates seeded)))
    (is (= "f27-guide" (get-in seeded [:deposit :depositor])))
    (is (= :certified (:status second-step)))
    (is (= "u" (get-in second-step [:memory-snapshot :snapshot-digest])))
    (is (= 2 @runs))
    (is (= :certified (:status (sut/guide-promotion-step! driver request report)))
        "certified review is idempotent and runs nothing")
    (is (= 2 @runs))
    (is (= :guide-candidates-invalid
           (:error/code (sut/guide-promotion-step!
                         (assoc driver :state-path (.resolve dir "other.edn"))
                         request {:candidates [(assoc guide-candidate :pattern-ids [])]}))))))

(deftest student-attempt-two-binds-to-a-guide-union-snapshot
  (let [addressed (fn [body] (assoc body :receipt/id (machine/ledger-digest [body])))
        promotion (addressed {:receipt/type :solver-promotion :receipt/frame-id "f19"
                              :receipt/problem-id "a01J05"
                              :receipt/snapshot-id "solver" :receipt/snapshot-digest "solver"})
        guide-1 (addressed {:receipt/type :guide-intervention :receipt/frame-id "f19"
                            :receipt/problem-id "a01J05"
                            :receipt/snapshot-id "union" :receipt/snapshot-digest "union"
                            :receipt/snapshot-path "/tmp/union.edn"})
        result (sut/build-request
                (merge base {:action {:kind :student-attempt
                                      :phase :student-attempt-2 :role :student
                                      :ordinal 2 :frame-id "f19" :problem-id "a01J05"}
                             :seat {:agent-id "f19-student" :invoke-ready? true}
                             :receipts {:preflight preflight-receipt
                                        :promote-solver promotion
                                        :guide-intervention-1 guide-1}
                             :snapshot-access {:ok true
                                               :snapshot {:snapshot/digest "union"}
                                               :accessible-memory-ids ["e-guide-1"]}}))]
    (is (:ok result))
    (is (= {:receipt-id (:receipt/id guide-1) :snapshot-id "union"
            :snapshot-digest "union" :accessible-memory-ids ["e-guide-1"]}
           (:memory-snapshot (:request result))))
    (testing "the following Guide carries the union as its prior snapshot"
      (let [guide-2 (sut/build-request
                     (merge base {:action {:kind :guide-intervention
                                           :phase :guide-intervention-2 :role :guide
                                           :ordinal 2 :frame-id "f19" :problem-id "a01J05"}
                                  :seat {:agent-id "f19-guide" :invoke-ready? true}
                                  :receipts {:preflight preflight-receipt
                                             :promote-solver promotion
                                             :guide-intervention-1 guide-1
                                             :student-attempt-2
                                             (addressed {:receipt/type :student-attempt
                                                         :receipt/frame-id "f19"
                                                         :receipt/problem-id "a01J05"})}}))]
        (is (= "/tmp/union.edn"
               (get-in guide-2 [:request :prior-snapshot :snapshot-path])))))))
