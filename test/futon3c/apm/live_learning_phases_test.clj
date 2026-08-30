(ns futon3c.apm.live-learning-phases-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.frame-cycle-handlers :as handlers]
            [futon3c.apm.job-port :as job-port]
            [futon3c.apm.live-learning-phases :as sut]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.role-memory-search :as role-memory]
            [futon3c.apm.typed-role-submission :as submission]))

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
   :receipts {:preflight preflight-receipt
              :promote-solver {:receipt/id "promotion-receipt"
                               :receipt/snapshot-id "snapshot"
                               :receipt/snapshot-digest "snapshot-digest"}}
   :snapshot-access {:ok true
                     :snapshot {:snapshot/digest "snapshot-digest"
                                :snapshot/memories []}
                     :accessible-memory-ids #{}}})

(deftest learning-receipts-preserve-the-terminal-job-lineage
  (let [job-id "agency-terminal-job"
        request {:frame-id "f19" :problem-id "a01J05"
                 :input-receipt-ids #{}
                 :dispatch/type :scribe-reduce}
        report {:lanes [] :dispositions [] :promotion-reviews []
                :memory-snapshot {:snapshot-id "snapshot"
                                  :snapshot-digest "digest"
                                  :snapshot-path "/tmp/snapshot.edn"
                                  :reviewed-memory-ids []}}
        cert (fn [phase]
               (with-redefs [handlers/validate-completion
                             (fn [_ _ receipt _]
                               {:ok true :receipt receipt})]
                 (sut/receipt
                  contract
                  {:kind :scribe-reduce :phase phase :role :scribe
                   :frame-id "f19" :problem-id "a01J05"}
                  {} (assoc request :phase phase) {:job-id job-id}
                  {:job-id job-id} {:ok true :report report})))]
    (is (= job-id (get-in (cert :promote-solver)
                           [:receipt :receipt/job-id])))
    (is (= job-id (get-in (cert :scribe-reduce)
                           [:receipt :receipt/job-id])))))

(deftest student-dispatch-refuses-without-a-promotion-bound-memory-snapshot
  (let [result (sut/build-request
                (-> base
                    (assoc :action {:kind :student-attempt
                                    :phase :student-attempt-1 :role :student
                                    :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
                           :seat {:agent-id "f19-student" :invoke-ready? true})
                    (update :receipts dissoc :promote-solver)
                    (dissoc :snapshot-access)))]
    (is (false? (:ok result)))
    (is (= :student-memory-snapshot-required (:error/code result)))
    (is (= [:promotion-receipt-missing] (:findings result)))
    (is (nil? (:request result)))))

(defn certified-candidate [frame-id problem-id ordinal]
  (let [head (apply str (repeat 40 (str ordinal)))
        body {:candidate/type :student-terminal
              :workspace/id (str frame-id "-student")
              :frame/id frame-id :problem/id problem-id
              :attempt/ordinal ordinal
              :candidate/head head
              :candidate/ref (str "refs/apm/student-candidates/" frame-id "/"
                                  problem-id "/attempt-" ordinal "/" head)
              :candidate/problem-blob (apply str (repeat 40 "b"))
              :candidate/lean-exit 0
              :candidate/worktree-clean? true
              :candidate/persisted-before-receipt? true}]
    (assoc body :candidate/id (machine/ledger-digest [body]))))

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
                                               :invoke-ready? true}
                                        :cascade-readers-fn
                                        (fn []
                                          (throw (ex-info
                                                  "arm-off reader was called"
                                                  {})))})))
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
        candidate (certified-candidate "f19" "a01J05" 1)
        result (sut/receipt contract action (:receipts base) request ticket job
                            (assoc validated :candidate candidate))]
    (is (:ok validated))
    (is (:ok result))
    (is (= {:receipt-id "promotion" :snapshot-id "snapshot"
            :snapshot-digest "digest"}
           (get-in result [:certificate :receipt/memory-snapshot])))
    (is (= candidate (get-in result [:certificate :receipt/candidate])))
    (is (= "fresh-s1" (get-in result [:certificate :receipt/fresh-session-id])))
    (is (not (contains? request :memory-cascade))
        "an absent arm config preserves the pre-cascade request shape")
    (is (not (contains? (:certificate result) :receipt/memory-cascade))
        "an absent arm config preserves the pre-cascade receipt shape")
    (is (= {:dispatch/type :student-attempt :phase :student-attempt-1
            :role :student :agent-id "f19-student" :frame-id "f19"
            :problem-id "a01J05" :ledger-digest (:digest (:ledger base))
            :role-card-path "card.md" :role-card-blob "card-blob"
            :input-receipt-ids #{(:receipt/id preflight-receipt)}
            :terminal-budget {:collection-attempts 1 :repair-attempts 1}
            :turn-timeout-ms 3600000 :attempt-ordinal 1
            :workspace "/tmp/student" :fresh-session? true
            :shelf/holdout :same-problem
            :shelf/withheld-ids [] :shelf/withheld-count 0
            :memory-snapshot
            {:receipt-id "promotion" :snapshot-id "snapshot"
             :snapshot-digest "digest" :accessible-memory-ids []}
            :fresh-session-nonce (:fresh-session-nonce request)
            :dispatch/id (:dispatch/id request)
            :submission/token (:submission/token request)}
           request)
        "the arm-off request is exactly the pre-change request")
    (let [certificate (:certificate result)]
      (is (= {:receipt/type :student-attempt :receipt/frame-id "f19"
              :receipt/problem-id "a01J05" :receipt/attempt-ordinal 1
              :receipt/fresh-session-id "fresh-s1" :receipt/job-id "j1"
              :receipt/outcome :stuck
              :receipt/failure-account {:reason :gap}
              :receipt/memory-use
              {:receipt-id "promotion" :snapshot-id "snapshot"
               :snapshot-digest "digest" :accessible-memory-ids []
               :surfaced-ids [] :used-ids [] :queries []}
              :receipt/memory-snapshot
              {:receipt-id "promotion" :snapshot-id "snapshot"
               :snapshot-digest "digest"}
              :shelf/holdout :same-problem
              :shelf/withheld-ids [] :shelf/withheld-count 0
              :receipt/candidate candidate :receipt/id (:receipt/id certificate)}
             certificate)
          "the arm-off receipt is exactly the pre-change receipt"))))

(def cascade-action
  {:kind :student-attempt :phase :student-attempt-1 :role :student
   :ordinal 1 :frame-id "f19" :problem-id "a01J05"})

(defn- cascade-request
  [cascade-fn]
  (let [result
        (sut/build-request
         (merge base
           {:unit (assoc unit :memory-cascade
                         {:enabled? true :routes [:sibling] :cap 10})
            :action cascade-action
            :snapshot-access {:ok true
                              :snapshot
                              {:snapshot/digest "snapshot-digest"
                               :snapshot/memories
                               [{:memory-id "shelf-memory"
                                 :depositor "f18-guide"
                                 :provenance {:campaign-id "c"
                                              :frame-id "f18"
                                              :problem-id "different"}}]}
                              :accessible-memory-ids #{"shelf-memory"}}
            :seat {:agent-id "f19-student" :invoke-ready? true}
            :cascade-fn cascade-fn
            :cascade-readers {:attachments-fn (constantly [])
                              :why-targets-fn (constantly [])
                              :pattern-fn (constantly nil)}}))]
    (when-not (:ok result)
      (throw (ex-info "cascade request fixture invalid" result)))
    (:request result)))

(defn- student-job
  [used-ids]
  {:job-id "cascade-job" :agent-id "f19-student" :session-id "fresh-cascade"
   :state :done
   :report {:command-own-exit 0 :frame-id "f19" :problem-id "a01J05"
            :outcome :stuck :failure-account {:reason :gap}
            :memory-use {:used-ids used-ids}}})

(deftest configured-cascade-offers-are-readable-citable-and-receipted
  (let [request (-> (cascade-request
                     (fn [seed-ids options]
                       (is (= ["shelf-memory"] seed-ids))
                       (is (= #{:sibling} (:routes options)))
                       {:routes [["shelf-memory" {:route :leaf :hops 0}]
                                 ["offered-memory"
                                  {:route :sibling :hops 1
                                   :pattern "math-strategy/p"}]]
                        :pattern-surfaces
                        {"math-strategy/p" {:offer/pattern-hook "Try the bridge"}}
                        :memory-metadata
                        {"offered-memory"
                         {:depositor "f18-guide"
                          :provenance {:campaign-id "c" :frame-id "f18"
                                       :problem-id "different"}}}
                        :expanded-available 1 :expanded-count 1 :cap 10
                        :truncated? false :routes-enabled #{:sibling}}))
                    (assoc :memory-snapshot
                           {:receipt-id "promotion" :snapshot-id "snapshot"
                            :snapshot-digest "digest"
                            :accessible-memory-ids ["shelf-memory"]}))
        ticket {:job-id "cascade-job"}
        job (student-job ["offered-memory"])]
    (is (= [{:memory-id "offered-memory" :route :sibling :hops 1
             :pattern "math-strategy/p" :pattern-hook "Try the bridge"}]
           (get-in request [:memory-cascade :offers])))
    (is (= {:sibling 1} (get-in request [:memory-cascade :histogram])))
    (is (re-find #"memory-cascade offers are also readable and citable"
                 (sut/prompt request)))
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (let [validated (sut/validate-terminal request ticket job)
            result (sut/receipt contract cascade-action (:receipts base)
                                request ticket job
                                (assoc validated :candidate
                                       (certified-candidate "f19" "a01J05" 1)))]
        (is (:ok validated))
        (is (:ok result) (pr-str result))
        (is (= [{:memory-id "offered-memory" :route :sibling
                 :pattern "math-strategy/p"}]
               (get-in result [:certificate :receipt/memory-cascade
                               :used-via-cascade])))
        (is (not (contains? (get-in result
                                    [:certificate :receipt/memory-cascade
                                     :offers 0])
                            :pattern-hook)))))
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (is (some #{:student-memory-used-without-surfacing}
                (:findings
                 (sut/validate-terminal request ticket
                                        (student-job ["not-offered"]))))))))

(deftest cascade-expansion-failure-does-not-block-student-dispatch
  (let [plain (:request
               (sut/build-request
                (merge base {:action cascade-action
                             :seat {:agent-id "f19-student"
                                    :invoke-ready? true}})))
        request (cascade-request
                 (fn [& _] (throw (ex-info "substrate unavailable" {}))))
        request (assoc request :memory-snapshot
                       {:receipt-id "promotion" :snapshot-id "snapshot"
                        :snapshot-digest "digest"
                        :accessible-memory-ids ["shelf-memory"]})]
    (is (map? request))
    (is (= "substrate unavailable" (get-in request [:memory-cascade :error])))
    (is (not (re-find #"memory-cascade offers are also readable"
                      (sut/prompt request))))
    (is (= (dissoc plain :dispatch/id :fresh-session-nonce :submission/token
                   :memory-snapshot)
           (dissoc request :dispatch/id :fresh-session-nonce :submission/token
                   :memory-cascade :memory-snapshot :memory-access-decisions)))
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (let [ticket {:job-id "cascade-job"}
            job (student-job [])
            validated (sut/validate-terminal request ticket job)
            result (sut/receipt contract cascade-action (:receipts base)
                                request ticket job
                                (assoc validated :candidate
                                       (certified-candidate "f19" "a01J05" 1)))]
        (is (:ok validated))
        (is (= {:error "substrate unavailable"
                :expansion-ms (get-in request [:memory-cascade :expansion-ms])}
               (get-in result [:certificate :receipt/memory-cascade])))))))

(deftest cascade-transport-failure-refuses-student-dispatch
  (let [result
        (sut/build-request
         (merge base
                {:unit (assoc unit :memory-cascade
                              {:enabled? true :routes [:sibling] :cap 10})
                 :action cascade-action
                 :seat {:agent-id "f19-student" :invoke-ready? true}
                 :snapshot-access {:ok true
                                   :snapshot {:snapshot/digest "snapshot-digest"
                                              :snapshot/memories []}
                                   :accessible-memory-ids #{}}
                 :cascade-fn
                 (fn [& _]
                   (throw
                    (ex-info "cascade timed out"
                             {:error/component :transport
                              :error/code :memory-cascade-unreachable
                              :timeout-ms 100})))
                 :cascade-readers {}}))]
    (is (false? (:ok result)))
    (is (= :transport (:error/component result)))
    (is (= :memory-cascade-unreachable (:error/code result)))
    (is (nil? (:request result)))))

(deftest typed-terminal-repair-preserves-authority-and-carries-findings
  (let [repair (sut/terminal-repair-request
                {:dispatch/id "original" :frame-id "f25"
                 :problem-id "m94A02" :fresh-session? true
                 :memory-snapshot {:snapshot-digest "snapshot"}}
                {:ticket/id "ticket-1"}
                {:job-id "job-1"
                 :report/error {:error/code :report-edn-lint-failed
                                :error/message "invalid EDN at 1:1"}}
                {:error/code :live-learning-terminal-invalid
                 :findings [:command-own-exit-nonzero :frame-mismatch]})
        request (:request repair)]
    (is (:ok repair))
    (is (false? (:fresh-session? request)))
    (is (= "snapshot" (get-in request [:memory-snapshot :snapshot-digest])))
    (is (= [:command-own-exit-nonzero :frame-mismatch]
           (:repair/findings request)))
    (is (= :report-edn-lint-failed
           (get-in request [:repair/validation-output :report/error
                            :error/code])))
    (is (not= "original" (:dispatch/id request)))))

(deftest repair-packet-leads-with-actionable-revision-instructions
  (let [repair (sut/terminal-repair-request
                {:dispatch/id "original" :dispatch/type :guide-intervention
                 :frame-id "f49" :phase :guide-intervention-1
                 :problem-id "a98A04" :agent-id "f49-guide"
                 :mode :store-mode
                 :role-card-path "guide.md" :role-card-blob "blob"}
                {:ticket/id "ticket-1"}
                {:job-id "job-1"
                 :report/error {:error/code :report-edn-lint-failed
                                :error/message "Invalid symbol: Gist:."}}
                {:error/code :live-job-submission-missing
                 :findings [:typed-submission-missing]})
        packet (sut/prompt (:request repair))]
    (is (:ok repair))
    (is (.startsWith packet
                     "REVISE AND RESUBMIT — THE PREVIOUS COMPLETION WAS REJECTED."))
    (is (re-find #"no typed submission was received" packet))
    (is (re-find #"Invalid symbol: Gist" packet))
    (is (re-find #"scripts/apm-submit-role.py --job-id" packet))
    (is (re-find #"repair budget is exhausted" packet))))

(deftest repair-refuses-a-finding-without-actionable-instructions
  (let [repair (sut/terminal-repair-request
                {:dispatch/id "original" :frame-id "f49"}
                {:ticket/id "ticket-1"} {:job-id "job-1"}
                {:findings [:new-unrendered-finding]})]
    (is (= :terminal-repair-instruction-missing (:error/code repair)))
    (is (= [:new-unrendered-finding] (:findings repair)))))

(deftest posthoc-guide-repair-rebuilds-current-authority-and-records-fault-origin
  (let [base-request {:dispatch/id "old" :dispatch/type :guide-intervention
                      :frame-id "f55" :problem-id "a99J06"
                      :phase :guide-intervention-1 :agent-id "f55-guide"
                      :role-card-path "guide.md" :role-card-blob "blob"}
        current (assoc base-request :dispatch/id "current" :mode :store-mode)
        apparatus (sut/posthoc-terminal-repair-request
                   current base-request {:ticket/id "ticket-1"}
                   {:job-id "job-1"}
                   {:error/code :frame-cycle-guide-mode-invalid})
        agent (sut/posthoc-terminal-repair-request
               current current {:ticket/id "ticket-2"} {:job-id "job-2"}
               {:error/code :live-learning-terminal-invalid
                :findings [:guide-channel-isolation-unproved]})]
    (is (:ok apparatus))
    (is (= :store-mode (get-in apparatus [:request :mode])))
    (is (= :apparatus (get-in apparatus [:request :repair/fault-origin])))
    (is (= [:guide-mode-authority-mismatch]
           (get-in apparatus [:request :repair/findings])))
    (is (re-find #"AUTHORIZED MODE: store-mode"
                 (sut/prompt (:request apparatus))))
    (is (:ok agent))
    (is (= :agent (get-in agent [:request :repair/fault-origin])))))

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
                 :frame-id "f25" :problem-id "m94A02"
                 :memory-use-audit []}
        job (fn [result]
              {:job-id "j" :agent-id "f25-guide" :state :done
               :report {:command-own-exit 0 :frame-id "f25"
                        :problem-id "m94A02" :trace-id "trace"
                        :result result :memory-use-audit []}})]
    (is (:ok (sut/validate-terminal request {:job-id "j"} (job "closed"))))
    (is (:ok (sut/validate-terminal request {:job-id "j"} (job :closed))))
    (is (:ok (sut/validate-terminal request {:job-id "j"} (job "partial"))))
    (is (some #{:close-evidence-invalid}
              (:findings (sut/validate-terminal request {:job-id "j"}
                                                       (job "void")))))))

(deftest close-repair-names-controller-memory-audit-for-both-failures
  (doseq [detail [:memory-use-audit-shape :memory-use-audit-mismatch]]
    (let [message (sut/repair-instructions
                   {:repair/findings [:close-evidence-invalid]
                    :repair/validation-output
                    {:finding/details {:close-evidence-invalid [detail]}}})]
      (is (re-find #":memory-use-audit" message))
      (is (re-find (re-pattern (name detail))
                   (str detail " " message))))))

(deftest close-prompt-explicitly-requires-verbatim-controller-audit
  (let [message (sut/prompt {:frame-id "f60" :phase :close-frame
                             :dispatch/type :close-frame
                             :role-card-path "guide.md"
                             :role-card-blob "blob"
                             :memory-use-audit []})]
    (is (re-find #"Copy :memory-use-audit verbatim from the request" message))))

(deftest close-frame-preserves-arbitrary-memory-ids-as-structured-evidence
  (let [memory-id "e-codexpilot-upgrade-diskwise-L1-convergence/v2"
        student (fn [ordinal]
                  {:receipt/type :student-attempt
                   :receipt/attempt-ordinal ordinal
                   :receipt/memory-use {:used-ids [memory-id]}})
        audit (sut/memory-use-audit
               {:student-attempt-1 (student 1)
                :student-attempt-2 (student 2)})]
    (is (= [{:memory-id memory-id :attempt-ordinals [1 2]}] audit))
    (let [request {:dispatch/type :close-frame :agent-id "f19-guide"
                   :frame-id "f19" :problem-id "a01J05"
                   :memory-use-audit audit}
          job {:job-id "close-job" :agent-id "f19-guide" :state :done
               :report {:command-own-exit 0 :frame-id "f19"
                        :problem-id "a01J05" :trace-id "trace"
                        :result :closed
                        :memory-use-audit audit}}]
      (is (:ok (sut/validate-terminal request {:job-id "close-job"} job)))
      (is (some #{:close-evidence-invalid}
                (:findings
                 (sut/validate-terminal
                  request {:job-id "close-job"}
                  (assoc-in job [:report :memory-use-audit] []))))))))

(deftest promoted-solver-snapshot-is-controller-bound-into-student-use
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
                                   :surfaced-ids ["agent-typo"]
                                   :queries ["agent replay"]
                                   :used-ids []}}}]
    (is (= {:receipt-id "promotion-receipt"
            :snapshot-id "snapshot-1" :snapshot-digest "snapshot-digest"
            :accessible-memory-ids ["m1"]}
           (:memory-snapshot request)))
    (is (= 1 (:attempt-ordinal request)))
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (let [result (sut/validate-terminal request {:job-id "j"} job)]
        (is (:ok result))
        (is (= {:receipt-id "promotion-receipt"
                :snapshot-id "snapshot-1"
                :snapshot-digest "snapshot-digest"
                :accessible-memory-ids ["m1"]
                :surfaced-ids ["m1"] :used-ids [] :queries []}
               (get-in result [:report :memory-use])))))))

(deftest attempt-one-requires-depositor-truthful-provenance
  (let [promotion {:receipt/id "promotion-receipt"
                   :receipt/snapshot-id "snapshot-1"
                   :receipt/snapshot-digest "snapshot-digest"}
        memories [{:memory-id "same" :depositor "f28-guide"
                   :provenance {:campaign-id "c1" :frame-id "f28"
                                :problem-id "a01J05"}}
                  {:memory-id "cross" :depositor "f29-guide"
                   :provenance {:campaign-id "c1" :frame-id "f29"
                                :problem-id "a98J02"}}]
        build (fn [ordinal memories]
                (sut/build-request
                 (merge base
                        {:contract {:phases {(keyword (str "student-attempt-" ordinal))
                                             {:ordinal ordinal :requires []}}}
                         :receipts {:promote-solver promotion}
                         :snapshot-access
                         {:ok true
                          ;; Verification remains against this complete, unfiltered
                          ;; snapshot and its promotion-bound digest.
                          :snapshot {:snapshot/digest "snapshot-digest"
                                     :snapshot/memories memories}
                          :accessible-memory-ids
                          (set (map :memory-id memories))}
                         :action {:kind :student-attempt
                                  :phase (keyword (str "student-attempt-" ordinal))
                                  :role :student :ordinal ordinal
                                  :frame-id "f19" :problem-id "a01J05"}
                         :seat {:agent-id "f19-student" :invoke-ready? true}})))
        first-result (build 1 memories)
        first-request (:request first-result)]
    (is (:ok first-result))
    (is (not-any? #{:student-snapshot-access-unverified} (:findings first-result))
        "the full snapshot digest is verified before filtering student access")
    (is (= ["cross"]
           (get-in first-request [:memory-snapshot :accessible-memory-ids])))
    (is (= :same-problem (:shelf/holdout first-request)))
    (is (= ["same"] (:shelf/withheld-ids first-request)))
    (is (= 1 (:shelf/withheld-count first-request)))
    (is (some #{:student-snapshot-provenance-invalid}
              (:findings (build 1 [(dissoc (first memories) :depositor)]))))
    (doseq [ordinal [2 3]
            :let [request (:request (build ordinal memories))]]
      (is (= ["cross" "same"]
             (get-in request [:memory-snapshot :accessible-memory-ids])))
      (is (not (contains? request :shelf/holdout)))
      (is (not (contains? request :shelf/withheld-ids)))
      (is (not (contains? request :shelf/withheld-count))))))

(deftest attempt-one-records-an-empty-same-problem-holdout
  (let [promotion {:receipt/id "promotion-receipt"
                   :receipt/snapshot-id "snapshot-1"
                   :receipt/snapshot-digest "snapshot-digest"}
        result (sut/build-request
                (merge base
                       {:receipts {:preflight preflight-receipt
                                   :promote-solver promotion}
                        :snapshot-access
                        {:ok true
                         :snapshot {:snapshot/digest "snapshot-digest"
                                    :snapshot/memories
                                    [{:memory-id "cross"
                                      :depositor "f29-guide"
                                      :provenance {:campaign-id "c1"
                                                   :frame-id "f29"
                                                   :problem-id "a98J02"}}]}
                         :accessible-memory-ids #{"cross"}}
                        :action {:kind :student-attempt
                                 :phase :student-attempt-1 :role :student
                                 :frame-id "f19" :problem-id "a01J05"}
                        :seat {:agent-id "f19-student" :invoke-ready? true}}))
        request (:request result)]
    (is (:ok result))
    (is (= :same-problem (:shelf/holdout request)))
    (is (= [] (:shelf/withheld-ids request)))
    (is (= 0 (:shelf/withheld-count request)))))

(deftest holdout-bounds-the-cascade-and-every-citation-channel
  ;; Amendment 7 (cascade arm) and amendment 8 (attempt-1 same-problem
  ;; holdout) go live on the same reload. The cascade reads candidates from
  ;; the store, so a withheld id would come back as a sibling of the shelf's
  ;; own patterns unless the expander is told to exclude it; and a withheld
  ;; id found by search or offered by a leaky cascade must not be citable.
  (let [promotion {:receipt/id "promotion-receipt"
                   :receipt/snapshot-id "snapshot-1"
                   :receipt/snapshot-digest "snapshot-digest"}
        memories [{:memory-id "same" :depositor "f28-guide"
                   :provenance {:campaign-id "c1" :frame-id "f28"
                                :problem-id "a01J05"}}
                  {:memory-id "cross" :depositor "f29-guide"
                   :provenance {:campaign-id "c1" :frame-id "f29"
                                :problem-id "a98J02"}}]
        seen-options (atom nil)
        request
        (:request
         (sut/build-request
          (merge base
                 {:unit (assoc unit :memory-cascade
                               {:enabled? true :routes [:sibling] :cap 10})
                  :contract {:phases {:student-attempt-1
                                      {:ordinal 1 :requires []}}}
                  :receipts {:promote-solver promotion}
                  :snapshot-access
                  {:ok true
                   :snapshot {:snapshot/digest "snapshot-digest"
                              :snapshot/memories memories}
                   :accessible-memory-ids #{"same" "cross"}}
                  :action cascade-action
                  :seat {:agent-id "f19-student" :invoke-ready? true}
                  :cascade-fn
                  (fn [seed-ids options]
                    (reset! seen-options options)
                    (is (= ["cross"] seed-ids) "seeds are the filtered shelf")
                    {:routes [["cross" {:route :leaf :hops 0}]
                              ["cross-sibling" {:route :sibling :hops 1
                                                :pattern "math-strategy/p"}]]
                     :pattern-surfaces {}
                     :memory-metadata
                     {"cross-sibling"
                      {:depositor "f29-guide"
                       :provenance {:campaign-id "c1" :frame-id "f29"
                                    :problem-id "a98J02"}}}
                     :expanded-available 1 :expanded-count 1 :cap 10
                     :truncated? false :routes-enabled #{:sibling}
                     :exclude-count 1 :excluded-offers 1})
                  :cascade-readers {:attachments-fn (constantly [])
                                    :why-targets-fn (constantly [])
                                    :pattern-fn (constantly nil)}})))
        ticket {:job-id "cascade-job"}]
    (is (= ["same"] (:shelf/withheld-ids request)))
    (is (= #{"same"} (:exclude @seen-options))
        "the withheld ids reach the expander as :exclude")
    (is (= ["cross-sibling"]
           (map :memory-id (get-in request [:memory-cascade :offers]))))
    (is (= 1 (get-in request [:memory-cascade :holdout-excluded]))
        "the cascade record says how many offers the holdout removed")
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (is (:ok (sut/validate-terminal request ticket
                                      (student-job ["cross-sibling"]))))
      (let [findings (:findings (sut/validate-terminal
                                 request ticket (student-job ["same"])))]
        (is (some #{:student-memory-used-despite-holdout} findings))
        (is (some #{:student-memory-used-without-surfacing} findings))))
    ;; A search hit on a withheld id is not a licence either.
    (with-redefs [role-memory/recorded-receipts-for-job
                  (fn [job-id]
                    (if (= "cascade-job" job-id)
                      [{:receipt/id "receipt" :query "same problem"
                        :result-ids ["same"]}]
                      []))]
      (is (some #{:student-memory-used-despite-holdout}
                (:findings (sut/validate-terminal
                            request ticket (student-job ["same"]))))))
    ;; And a leaky cascade record (offer of a withheld id) does not license it.
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (is (some #{:student-memory-used-despite-holdout}
                (:findings
                 (sut/validate-terminal
                  (assoc-in request [:memory-cascade :offers]
                            [{:memory-id "same" :route :sibling :hops 1
                              :pattern "math-strategy/p"}])
                  ticket (student-job ["same"]))))))))

(deftest student-cannot-use-global-store-memory-outside-controller-evidence
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
                      :memory-use {:used-ids ["e-global"]}}}]
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (let [result (sut/validate-terminal request {:job-id "j"} job)]
        (is (some #{:student-memory-used-without-surfacing}
                  (:findings result)))))))

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
                      :memory-use {:used-ids ["e-open"]}}}]
    (with-redefs [role-memory/recorded-receipts-for-job
                  (fn [job-id]
                    (if (= "repair" job-id)
                      [{:receipt/id "receipt" :query "Jensen formula"
                        :result-ids ["e-open"]}]
                      []))]
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
                      :memory-use {:used-ids ["e-prior"]}}}]
    (with-redefs [role-memory/recorded-receipts-for-job
                  (fn [job-id]
                    (if (= "original" job-id)
                      [{:receipt/id "prior" :query "Jensen formula"
                        :result-ids ["e-prior"]}]
                      []))]
      (is (:ok (sut/validate-terminal request {:job-id "repair"} job))))))

(deftest holdout-repair-fails-closed-on-unverifiable-inherited-result
  (let [request {:dispatch/type :student-attempt
                 :repair/of-job-id "original"
                 :agent-id "f30-student" :frame-id "f30"
                 :problem-id "a01J06" :shelf/holdout :same-problem
                 :memory-snapshot {:accessible-memory-ids []}}
        job {:job-id "repair" :agent-id "f30-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f30" :problem-id "a01J06"
                      :memory-use {:used-ids ["e-legacy"]}}}]
    (with-redefs [role-memory/recorded-receipts-for-job
                  (fn [job-id]
                    (if (= "original" job-id)
                      [{:receipt/id "legacy" :result-ids ["e-legacy"]
                        :content-matches [{:memory/id "e-legacy"}]}]
                      []))]
      (is (some #{:student-memory-used-without-surfacing}
                (:findings
                 (sut/validate-terminal request {:job-id "repair"} job)))))))

(deftest f32-freehand-surfaced-id-typo-is-ignored-in-favor-of-controller-data
  (let [request {:dispatch/type :student-attempt
                 :agent-id "f30-student" :frame-id "f30"
                 :problem-id "a01J06"
                 :memory-snapshot {:receipt-id "promotion"
                                   :snapshot-id "snapshot"
                                   :snapshot-digest "digest"
                                   :accessible-memory-ids
                                   ["e-1866fc8e-aa5a-426c-aa30-d8d57c224238"]}}
        job {:job-id "repair" :agent-id "f30-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f30"
                      :problem-id "a01J06"
                      :memory-use {:receipt-id "promotion"
                                   :queries ["agent replay"]
                                   :surfaced-ids
                                   ["e-1866fc8e-aa5e-426c-aa30-d8d57c224238"]
                                   :used-ids []}}}]
    (with-redefs [role-memory/recorded-receipts-for-job (constantly [])]
      (let [result (sut/validate-terminal request {:job-id "repair"} job)]
        (is (:ok result))
        (is (= ["e-1866fc8e-aa5a-426c-aa30-d8d57c224238"]
               (get-in result [:report :memory-use :surfaced-ids])))
        (is (= [] (get-in result [:report :memory-use :queries])))))))

(deftest student-prompt-names-frame-and-exact-memory-evidence-shape
  (let [text (sut/prompt {:dispatch/type :student-attempt
                          :phase :student-attempt-1
                          :frame-id "f22" :role-card-path "student.md"
                          :role-card-blob "blob"
                          :memory-snapshot {:accessible-memory-ids []}})]
    (is (.startsWith text "F22 student-attempt-1"))
    (is (re-find #"only vector-valued :used-ids" text))
    (is (re-find #"controller-owned open mathematics search" text))
    (is (re-find #"must not be copied" text))))

(deftest run-live-archives-the-exact-student-activation-packet
  (let [directory (java.nio.file.Files/createTempDirectory
                   "student-packet-"
                   (make-array java.nio.file.attribute.FileAttribute 0))
        state-path (.resolve directory "student-attempt-2.edn")
        request {:dispatch/type :student-attempt
                 :dispatch/id "dispatch-student-2"
                 :phase :student-attempt-2
                 :agent-id "f42-student"
                 :frame-id "f42"
                 :problem-id "a94A02"
                 :role-card-path "student.md"
                 :role-card-blob "student-blob"
                 :fresh-session? false}
        action {:kind :student-attempt :phase :student-attempt-2}
        activated (atom nil)
        job-id (submission/canonical-job-id request)
        expected (sut/prompt (submission/with-job-authority request))]
    (with-redefs [job-port/announce!
                  (fn [_ payload]
                    (is (= expected (:prompt payload)))
                    {:ok true :job-id job-id})
                  job-port/activate!
                  (fn [_ payload]
                    (reset! activated payload)
                    {:ok true})
                  submission/register!
                  (fn [_ _] {:ok true})]
      (let [result (sut/run-live!
                    {:contract contract :action action :receipts {}
                     :request request :state-path state-path
                     :preparation {}})
            packet-path (sut/packet-archive-path
                         state-path :student-attempt-2)]
        (is (:ok result))
        (is (= expected (:prompt @activated)))
        (is (java.nio.file.Files/isRegularFile
             packet-path (make-array java.nio.file.LinkOption 0)))
        (is (= expected
               (java.nio.file.Files/readString
                packet-path java.nio.charset.StandardCharsets/UTF_8)))))))

(deftest packet-archive-failure-is-reported-without-blocking-activation
  (let [directory (java.nio.file.Files/createTempDirectory
                   "student-packet-failure-"
                   (make-array java.nio.file.attribute.FileAttribute 0))
        state-path (.resolve directory "student-attempt-1.edn")
        request {:dispatch/type :student-attempt
                 :dispatch/id "dispatch-student-1"
                 :phase :student-attempt-1
                 :agent-id "f42-student"
                 :frame-id "f42"
                 :problem-id "a94A02"
                 :role-card-path "student.md"
                 :role-card-blob "student-blob"
                 :fresh-session? false}
        activated? (atom false)
        error-output (java.io.StringWriter.)]
    (binding [*err* error-output]
      (with-redefs [sut/archive-rendered-packet!
                    (fn [& _]
                      {:ok false :error/code :rendered-packet-archive-failed
                       :path "/unwritable/student-attempt-1-packet.txt"})
                    job-port/announce!
                    (fn [_ _]
                      {:ok true
                       :job-id (submission/canonical-job-id request)})
                    job-port/activate!
                    (fn [_ _]
                      (reset! activated? true)
                      {:ok true})
                    submission/register! (fn [_ _] {:ok true})]
        (let [result (sut/run-live!
                      {:contract contract
                       :action {:kind :student-attempt
                                :phase :student-attempt-1}
                       :receipts {} :request request :state-path state-path
                       :preparation {}})]
          (is (:ok result))
          (is @activated?))))
    (is (re-find #"rendered-packet-archive-failed" (str error-output)))))

(deftest student-report-must-record-used-ids-vector
  (let [request {:dispatch/type :student-attempt :agent-id "f22-student"
                 :frame-id "f22" :problem-id "p22"}
        job {:job-id "j" :agent-id "f22-student" :session-id "fresh"
             :state :done
             :report {:command-own-exit 0 :frame-id "f22" :problem-id "p22"
                      :memory-use {}}}]
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
                        :snapshot-access nil
                        :seat {:agent-id "f19-student" :invoke-ready? true}}))]
    (is (= :live-learning-request-invalid (:error/code result)))
    (is (some #{:student-snapshot-access-unverified} (:findings result)))))

(deftest role-terminal-budgets-are-configurable-and-validated
  (let [action {:kind :guide-intervention :phase :guide-intervention-1
                :role :guide :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
        attempt {:receipt/id "attempt"}
        good (sut/build-request
              (merge base {:action action
                           :authorized-mode :store-mode
                           :receipts {:student-attempt-1 attempt}
                           :seat {:agent-id "f19-guide" :invoke-ready? true}
                           :terminal-budgets {:guide {:collection-attempts 1
                                                      :repair-attempts 2}}}))
        bad (sut/build-request
             (merge base {:action action
                          :authorized-mode :store-mode
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
                             :authorized-mode :store-mode
                             :receipts {:student-attempt-1 attempt}
                             :seat {:agent-id "f19-guide"
                                    :invoke-ready? true}}))]
    (is (:ok result))
    (is (= :store-mode (get-in result [:request :mode])))
    (is (re-find #"AUTHORIZED MODE: store-mode"
                 (sut/prompt (:request result))))
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
                            :alternate-receipt/types #{:student-observation-missing
                                                       :student-observation-recovered}
                            :requires #{} :produces #{:attempt :memory-use}}}
                  :receipt/schemas
                  {:student-observation-missing
                   {:required #{:receipt/id :receipt/type :receipt/frame-id
                                :receipt/problem-id :receipt/attempt-ordinal
                                :receipt/job-id :receipt/author :receipt/reason
                                :receipt/repair-attempts :receipt/memory-snapshot
                                :receipt/harness-observed}}
                   :student-observation-recovered
                   {:required #{:receipt/id :receipt/type :receipt/frame-id
                                :receipt/problem-id :receipt/attempt-ordinal
                                :receipt/job-id :receipt/author :receipt/reason
                                :receipt/repair-attempts :receipt/memory-snapshot
                                :receipt/harness-observed :receipt/memory-use
                                :receipt/candidate-disposition}}}}
        action {:kind :student-attempt :role :student :phase :student-attempt-1
                :frame-id "fixture-f25" :problem-id "m94A02"}
        candidate (certified-candidate "fixture-f25" "m94A02" 1)
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
                1 {:collection/id "collection-evidence"}
                (fn [] {:ok true :source {:blob "source-blob"}})
                (fn [] {:ok true :candidate candidate}))]
    (is (:ok result))
    (is (= :controller (get-in result [:certificate :receipt/author])))
    (is (= [] (get-in result [:certificate :receipt/memory-use :used-ids])))
    (is (= ["memory-1"]
           (get-in result [:certificate :receipt/memory-use :surfaced-ids])))
    (is (= :student-observation-recovered
           (get-in result [:certificate :receipt/type])))
    (is (= :typed-submission-collection-failed-but-observation-recovered
           (get-in result [:certificate :receipt/reason])))
    (is (= {:receipt-id "promotion-receipt"
            :snapshot-id "frozen-snapshot"
            :snapshot-digest "frozen-digest"}
           (get-in result [:certificate :receipt/memory-snapshot])))
    (is (= candidate (get-in result [:certificate :receipt/candidate])))
    (is (nil? (get-in result [:certificate :receipt/fresh-session-id])))))

(deftest f30-shaped-missing-observation-certifies-the-preserved-candidate
  (let [contract {:phase-order [:student-attempt-3]
                  :phases {:student-attempt-3
                           {:kind :student-attempt :role :student :ordinal 3
                            :receipt/type :student-attempt
                            :alternate-receipt/types #{:student-observation-missing
                                                       :student-observation-recovered}
                            :requires #{} :produces #{:attempt :memory-use}}}
                  :receipt/schemas
                  {:student-observation-missing
                   {:required #{:receipt/id :receipt/type :receipt/frame-id
                                :receipt/problem-id :receipt/attempt-ordinal
                                :receipt/job-id :receipt/author :receipt/reason
                                :receipt/repair-attempts :receipt/memory-snapshot
                                :receipt/harness-observed}}
                   :student-observation-recovered
                   {:required #{:receipt/id :receipt/type :receipt/frame-id
                                :receipt/problem-id :receipt/attempt-ordinal
                                :receipt/job-id :receipt/author :receipt/reason
                                :receipt/repair-attempts :receipt/memory-snapshot
                                :receipt/harness-observed :receipt/memory-use
                                :receipt/candidate-disposition}}}}
        action {:kind :student-attempt :role :student :phase :student-attempt-3
                :frame-id "f30" :problem-id "a01J06"}
        candidate (certified-candidate "f30" "a01J06" 3)
        result (sut/missing-observation-receipt
                contract action {}
                {:frame-id "f30" :problem-id "a01J06" :attempt-ordinal 3
                 :workspace "/does/not/exist" :memory-snapshot {}}
                {:job-id "f30-student-job"}
                {:job-id "f30-student-job" :agent-id "f30-student"
                 :state :done :terminal-code 0}
                1 {:collection/id "collection"}
                (fn [] {:ok true :source {:blob "source-blob"}})
                (fn [] {:ok true :candidate candidate}))]
    (is (:ok result) (pr-str result))
    (is (= candidate
           (get-in result [:certificate :receipt/harness-observed
                           :workspace :candidate])))
    (is (= candidate (get-in result [:certificate :receipt/candidate])))
    (is (= :controller (get-in result [:certificate :receipt/author])))))

(deftest missing-observation-records-but-does-not-certify-invalid-candidate
  (let [contract {:phase-order [:student-attempt-3]
                  :phases {:student-attempt-3
                           {:kind :student-attempt :role :student :ordinal 3
                            :receipt/type :student-attempt
                            :alternate-receipt/types #{:student-observation-missing
                                                       :student-observation-recovered}
                            :requires #{} :produces #{:attempt :memory-use}}}
                  :receipt/schemas
                  {:student-observation-missing
                   {:required #{:receipt/id :receipt/type :receipt/frame-id
                                :receipt/problem-id :receipt/attempt-ordinal
                                :receipt/job-id :receipt/author :receipt/reason
                                :receipt/repair-attempts :receipt/memory-snapshot
                                :receipt/harness-observed}}
                   :student-observation-recovered
                   {:required #{:receipt/id :receipt/type :receipt/frame-id
                                :receipt/problem-id :receipt/attempt-ordinal
                                :receipt/job-id :receipt/author :receipt/reason
                                :receipt/repair-attempts :receipt/memory-snapshot
                                :receipt/harness-observed :receipt/memory-use
                                :receipt/candidate-disposition}}}}
        action {:kind :student-attempt :role :student :phase :student-attempt-3
                :frame-id "f34" :problem-id "a95J03"}
        rejected {:ok false :error/code :student-candidate-validation-failed
                  :head (apply str (repeat 40 "5"))
                  :ref "refs/apm/student-candidates/f34/a95J03/attempt-3/rejected"}
        resets (atom 0)
        result (sut/missing-observation-receipt
                contract action {}
                {:frame-id "f34" :problem-id "a95J03" :attempt-ordinal 3
                 :workspace "/does/not/exist" :memory-snapshot {}}
                {:job-id "f34-student-job"}
                {:job-id "f34-student-job" :agent-id "f34-student"
                 :state :done :terminal-code 0}
                1 {:collection/id "collection"}
                (fn [] {:ok true :source {:blob "source-blob"}})
                (fn [] rejected)
                (fn [] (swap! resets inc)
                  {:ok true :head (apply str (repeat 40 "b"))
                   :preservation-ref (:ref rejected)}))]
    (is (:ok result) (pr-str result))
    (is (= (select-keys rejected [:error/code :head :ref])
           (get-in result [:certificate :receipt/harness-observed
                           :workspace :candidate])))
    (is (nil? (get-in result [:certificate :receipt/candidate])))
    (is (= :student-observation-recovered
           (get-in result [:certificate :receipt/type])))
    (is (= :rejected-evidence
           (get-in result [:certificate :receipt/candidate-disposition])))
    (is (= 1 @resets))
    (is (true? (get-in result [:certificate :receipt/harness-observed
                               :workspace :reset-after-rejection :ok])))
    (is (= :controller (get-in result [:certificate :receipt/author])))))

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
                                        :promote-solver
                                        (get-in base [:receipts :promote-solver])
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
                            (assoc validated :source (:source archived)
                                   :candidate
                                   (certified-candidate "f19" "a01J05" 1)))]
    (is (:ok validated))
    (is (= "/tmp/campaign/live/student-attempt-1-source"
           (get-in archived [:source :archive-directory])))
    (is (:ok result))
    (is (= "abc" (get-in result [:certificate :receipt/source :blob])))
    (is (= :student-source-unknown
           (:error/code (sut/archive-student-source!
                         (dissoc request :problem-path) "/tmp/x.edn" (fn [_] nil)))))))

(def guide-candidate
  {:name "explicit witness"
   :hook "when an abstract existence route is too expensive"
   :body "Construct the reusable witness directly and prove its defining property."
   :pattern-ids ["math-informal/x"]})

(deftest f30-fixture-builds-exact-three-attempt-zai-scribe-request
  (let [root "data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f30/live"
        states (mapv #(edn/read-string
                       (slurp (str root "/student-attempt-" % ".edn")))
                     [1 2 3])
        attempt-inputs
        (mapv (fn [n state]
                (let [receipt (:receipt state)
                      job-id (:receipt/job-id receipt)]
                  {:phase (keyword (str "student-attempt-" n))
                   :job-id job-id
                   :job-trace-ref (str "http://localhost:7070/api/alpha/invoke/jobs/"
                                       job-id)
                   :repair-job-ids []
                   :memory-use (:receipt/memory-use receipt)
                   :failure-account (:receipt/failure-account receipt)}))
              [1 2 3] states)
        action {:kind :scribe-reduce :role :scribe :phase :scribe-reduce
                :frame-id "f30" :problem-id "a01J06"}
        local-contract (assoc-in contract [:phases :scribe-reduce :requires] #{})
        built (sut/build-request
               {:contract local-contract :action action
                :ledger {:digest (apply str (repeat 64 "d"))}
                :unit {:frame/id "f30" :problem/id "a01J06"
                       :problem {:blob (apply str (repeat 40 "b"))
                                 :path "problems/a01J06/lean/Main.lean"}}
                :role-card {:path "zai-scribe-v1.md"
                            :blob (apply str (repeat 40 "c"))}
                :seat-role :zai-scribe
                :seat {:agent-id "f30-zai-scribe" :invoke-ready? true}
                :receipts {:solve {:receipt/id "solve"
                                   :receipt/final-head
                                   (apply str (repeat 40 "e"))}}
                :student-attempt-inputs attempt-inputs})
        request (:request built)]
    (is (:ok built) (pr-str built))
    (is (= :zai-scribe (:role request)))
    (is (= (mapv #(get-in % [:receipt :receipt/job-id]) states)
           (mapv :job-id (:student-attempts request))))
    (is (= (mapv #(get-in % [:receipt :receipt/memory-use]) states)
           (mapv :memory-use (:student-attempts request))))))

(deftest guide-candidates-must-be-gate-shaped-and-store-mode
  (let [request {:dispatch/type :guide-intervention :agent-id "f27-guide"
                 :frame-id "f27" :problem-id "m94A03" :mode :store-mode}
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

(deftest guide-terminal-must-echo-authorized-mode
  (let [request {:dispatch/type :guide-intervention :agent-id "f27-guide"
                 :frame-id "f27" :problem-id "m94A03" :mode :store-mode}
        report {:command-own-exit 0 :frame-id "f27" :problem-id "m94A03"
                :channel-audit {:direct-student-contact? false}}
        findings (fn [mode]
                   (:findings
                    (sut/validate-terminal
                     request {:job-id "j"}
                     {:job-id "j" :agent-id "f27-guide" :state :done
                      :report (assoc report :mode mode)})))]
    (is (nil? (findings "store-mode")))
    (is (some #{:guide-mode-authority-mismatch} (findings nil)))
    (is (some #{:guide-mode-authority-mismatch}
              (findings "harness-mode")))))

(deftest guide-mode-normalizes-json-strings-without-weakening-authority
  (let [report {:command-own-exit 0 :frame-id "f56" :problem-id "a99J07"
                :channel-audit {:direct-student-contact? false}
                :candidates [guide-candidate]}
        findings (fn [authorized submitted]
                   (:findings
                    (sut/validate-terminal
                     {:dispatch/type :guide-intervention :agent-id "f56-guide"
                      :frame-id "f56" :problem-id "a99J07"
                      :mode authorized}
                     {:job-id "j"}
                     {:job-id "j" :agent-id "f56-guide" :state :done
                      :report (assoc report :mode submitted)})))]
    (is (nil? (findings :store-mode "store-mode")))
    (is (some #{:guide-candidates-outside-store-mode}
              (findings :harness-mode "harness-mode")))
    (is (some #{:guide-mode-authority-mismatch}
              (findings :store-mode nil)))))

(deftest guide-mode-declaration-conflict-is-distinctly-refused
  (let [result
        (sut/validate-terminal
         {:dispatch/type :guide-intervention :agent-id "f60-guide"
          :frame-id "f60" :problem-id "b00J02" :mode :store-mode}
         {:job-id "j"}
         {:job-id "j" :agent-id "f60-guide" :state :done
          :report {:command-own-exit 0 :frame-id "f60" :problem-id "b00J02"
                   :mode "store-mode"
                   :guide-mode-declaration-conflict
                   {:payload :store-mode :channel-audit :harness-mode}
                   :channel-audit {:direct-student-contact? false}}})]
    (is (some #{:guide-mode-declaration-conflict} (:findings result)))))

(deftest guide-channel-audit-normalizes-only-the-json-predicate-alias
  (let [validate (fn [audit]
                   (sut/validate-terminal
                    {:dispatch/type :guide-intervention :agent-id "f56-guide"
                     :frame-id "f56" :problem-id "a99J07"
                     :phase :guide-intervention-1 :mode :store-mode}
                    {:job-id "j"}
                    {:job-id "j" :agent-id "f56-guide" :state :done
                     :report {:command-own-exit 0 :frame-id "f56"
                              :problem-id "a99J07" :mode "store-mode"
                              :channel-audit audit}}))
        alias-only (validate {:direct-student-contact false})
        conflict (validate {:direct-student-contact? false
                            :direct-student-contact true})
        missing (validate {})]
    (is (:ok alias-only))
    (is (= false
           (get-in alias-only [:report :channel-audit
                               :direct-student-contact?])))
    (is (some #{:wire-predicate-key-conflict} (:findings conflict)))
    (is (some #{:guide-channel-isolation-unproved} (:findings missing)))))

(deftest guide-candidate-repair-renders-the-specific-validator-finding
  (let [base-request {:dispatch/id "d" :dispatch/type :guide-intervention
                      :agent-id "f54-guide" :frame-id "f54"
                      :problem-id "a99J05" :phase :guide-intervention-1
                      :mode :store-mode :role-card-path "guide.md"
                      :role-card-blob "blob"}
        report {:command-own-exit 0 :frame-id "f54" :problem-id "a99J05"
                :mode "store-mode"
                :channel-audit {:direct-student-contact? false}}
        validate (fn [request candidates]
                   (sut/validate-terminal
                    request {:job-id "j"}
                    {:job-id "j" :agent-id (:agent-id request) :state :done
                     :report (assoc report :candidates candidates)}))
        semantic-candidate {:name "explicit witness"
                            :hook "when an abstract route is too expensive"
                            :body "Construct the witness and verify it directly."
                            :pattern-ids ["math-informal/construct-an-explicit-witness"]}
        semantic-success (validate base-request [semantic-candidate])
        shape-failure (validate base-request
                                [{:memory-id "m" :content-digest "d"
                                  :pattern-ids ["p"] :source-attempts []}])
        depositor-failure (validate (assoc base-request :agent-id nil)
                                    [semantic-candidate])
        packet (fn [failure]
                 (-> (sut/terminal-repair-request
                      base-request {:ticket/id "t"} {:job-id "j"} failure)
                     :request sut/prompt))
        shape-packet (packet shape-failure)
        depositor-packet (packet depositor-failure)]
    (is (:ok semantic-success))
    (is (= [:candidate-shape-invalid]
           (get-in shape-failure
                   [:finding/details :guide-candidates-invalid])))
    (is (= [:depositor-missing]
           (get-in depositor-failure
                   [:finding/details :guide-candidates-invalid])))
    (is (re-find #":name" shape-packet))
    (is (re-find #"controller-derived" shape-packet))
    (is (re-find #":depositor" depositor-packet))
    (is (not= shape-packet depositor-packet))))

(deftest guide-receipt-carries-the-union-snapshot-when-one-was-published
  (let [action {:kind :guide-intervention :phase :guide-intervention-1 :role :guide
                :ordinal 1 :frame-id "f19" :problem-id "a01J05"}
        request {:dispatch/type :guide-intervention :agent-id "f19-guide"
                 :frame-id "f19" :problem-id "a01J05" :mode :store-mode}
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
    (is (= :store-mode (get-in bare [:certificate :receipt/mode])))
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
        materialized (assoc guide-candidate :memory-id "e-controller-guide-1"
                            :materialization {:artifact-id "e-controller-guide-1"})
        driver {:state-path state-path :run-fn run-fn
                :persist-candidates-fn
                (fn [deposit _request]
                  {:ok true :deposit (assoc deposit :candidates [materialized])
                   :candidates [materialized]})}
        first-step (sut/guide-promotion-step! driver request report)
        seeded (runtime/read-state state-path)
        second-step (sut/guide-promotion-step! driver request report)]
    (is (= :awaiting-terminal (:status first-step)))
    (is (= "review-1" (:job-id first-step)))
    (is (= [materialized] (:candidates seeded)))
    (is (= "f27-guide" (get-in seeded [:deposit :depositor])))
    (is (= :certified (:status second-step)))
    (is (= "u" (get-in second-step [:memory-snapshot :snapshot-digest])))
    (is (= 2 @runs))
    (is (= :certified (:status (sut/guide-promotion-step! driver request report)))
        "certified review is idempotent and runs nothing")
    (is (= 2 @runs))
    (let [invalid-path (.resolve dir "other.edn")
          invalid (sut/guide-promotion-step!
                   {:state-path invalid-path
                    :persist-candidates-fn
                    (fn [deposit _] {:ok true :deposit deposit
                                     :candidates (:candidates deposit)})
                    :run-fn (fn [] {:ok true :status :certified})}
                   request {:candidates [(assoc guide-candidate :pattern-ids [])]})]
      (is (= :guide-candidates-invalid (:error/code invalid)))
      (is (some #{:candidate-shape-invalid} (:findings invalid)))
      (is (nil? (runtime/read-state invalid-path))))))

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
                                  :authorized-mode :store-mode
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
