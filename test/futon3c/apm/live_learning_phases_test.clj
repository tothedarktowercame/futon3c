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
                      :memory-use {:queries 2 :used []}}}
        ticket {:job-id "j1"}
        validated (sut/validate-terminal request ticket job)
        result (sut/receipt contract action (:receipts base) request ticket job validated)]
    (is (:ok validated))
    (is (:ok result))
    (is (= "fresh-s1" (get-in result [:certificate :receipt/fresh-session-id])))))

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
                                   :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
                          :seat {:agent-id "f19-student" :invoke-ready? true}})))
        job {:job-id "j" :agent-id "f19-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                      :outcome :stuck :failure-account {}
                      :memory-use {:receipt-id "wrong"
                                   :snapshot-id "snapshot-1"
                                   :snapshot-digest "snapshot-digest"}}}]
    (is (= {:receipt-id "promotion-receipt"
            :snapshot-id "snapshot-1" :snapshot-digest "snapshot-digest"
            :accessible-memory-ids ["m1"]}
           (:memory-snapshot request)))
    (is (some #{:student-memory-snapshot-mismatch}
              (:findings (sut/validate-terminal request {:job-id "j"} job))))))

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

(deftest promote-solver-requires-reviewed-candidates-for-controller-publication
  (let [request {:dispatch/type :scribe-reduce :phase :promote-solver
                 :agent-id "f19-scribe" :frame-id "f19" :problem-id "a01J05"}
        report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
                :lanes [] :dispositions [] :promotion-reviews []
                :memory-candidates []}
        job {:job-id "j" :agent-id "f19-scribe" :state :done :report report}]
    (is (some #{:solver-promotion-candidates-invalid}
              (:findings (sut/validate-terminal request {:job-id "j"} job))))))
